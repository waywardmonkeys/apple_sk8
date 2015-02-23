;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories

(in-package :sk8dev)

(provide "PAINTFIELD")

(require "RECTANGLE")
(require "SLOWSTRIPCOPY" "functions;SlowStripCopy")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export 'realcolor :sk8dev)
  (import 'realcolor :sk8))

;; declarations
(sk8-declare-syms :sk8
                  :public
                  sk8::PaintField 		
                  sk8::undoGWorld 		
                  sk8::undoRegion 		
                  sk8::undoEnabled
                  sk8::selectiongWorld 		
                  sk8::selectionRegion 		
                  sk8::flashingSelection 		
                  sk8::drawSelection
                  sk8::overprintingSelection 
                  sk8::paintgWorld 
                  sk8::paintDepth 
                  sk8::chromaKeyColor 
                  sk8::freeDataStructures 
                  sk8::allocateDataStructures
                  sk8::SK8ColorToRGB 
                  sk8::getRegionFromUser 
                  sk8::RGB= 
                  sk8::fillField
                  sk8::eraseField 
                  sk8::SaveForUndo 
                  sk8::mouseInSelection 
                  sk8::selectionEmpty
                  sk8::clearSelection 
                  sk8::setRectSelection 
                  sk8::getSelectionRect
                  sk8::setSelectionRect 
                  sk8::scaleRegion 
                  sk8::setRegionSelection
                  sk8::fillSelection 
                  sk8::grabSelection 
                  sk8::printSelection
                  sk8::dragSelection 
                  sk8::drawWithBrush 
                  sk8::drawWithIcon
                  sk8::seedFill 
                  sk8::shrinkSelection 
                  sk8::RealColor 
                  sk8::actorIntoSelection
                  sk8::getPixelColor 
                  sk8::setPixelColor 
                  sk8::drawWithSelection
                  sk8::selectionBoundsRect 
                  sk8::selectionSize 
                  sk8::selectionLocation
                  sk8::scaleRect 
                  sk8::resizeSelection 
                  sk8::ditheringTransfers
                  sk8::storeFieldPixmap 
                  sk8::retrieveFieldPixmap
                  sk8::insertRect 
                  sk8::insertOval 
                  sk8::insertLine)

;;_________________________________________________
;;_________________________________________________
;;
;; PAINTFIELD
;;
;; A general paint-style field for SK8 1.0
;;
;; (c) 1993 ICE for Apple Computer, Inc.
;;
;;_________________________________________________
;;_________________________________________________
;; VERSION HISTORY
;; 02-16-96 RFS added a forceRedraw to chromaKeyColor setter
;; 02-01-96 RFS added new handler: storeField (previously in a patch patch)
;; 01-31-96 RFS got rid of all the get-record-field calls to eliminate library dependency
;; 01-31-96 RFS made chromaKeyColor work (added code & a set chromaKeyColor handler)
;; 11-08-95 SIDNEY make sure data structures (paintgWorld) are allocated on initialize,
;;         restore and load from store, and add check for valid gWorld before trying to use it in lots of places
;; 11-04-94 SIDNEY Rewrote InitializeFromStore to use new SK8 storage facility
;; 02-20-94 ICE  Made ShrinkSelection default to use slowStripCopy for accuracy.
;; 02-20-94 ICE Added slowStripCopy function
;; 02-20-94 ICE Fixed bug in GetPixelColor
;; 02-20-94 ICE Added GetPixelColor for Stage
;; 02-16-94 ICE Made drawWithBrush truncate the mouseLoc rather than round.
;; 01-27-94 DCS Made frameSize of PaintField be (0 0)
;; 01-19-94 ICE Added preservingShape keyword to actorIntoSelection.
;; 01-19-94 ICE Changed draw-selection to drawSelection.
;;_________________________________________________
;;_________________________________________________
;; PAINTFIELD
;; NB: CAT Team Version
;;     Does not need Library folder
;; This is the actor that handles ALL of the paint field routines.
;; You can make instances of this actor all you want.
;; Each has its own undo history.
;; Note that each one also consumes mass amounts of RAM.

(new Rectangle :objectName "PaintField" :project sk8)

(addParent PaintField Renderer)

;; UNDO
(addProperty PaintField 'undogWorld :propagatedValue nil)
(addProperty PaintField 'undoRegion :propagatedValue nil)
(addProperty PaintField 'undoEnabled)
;; SELECTION
(addProperty PaintField 'selectiongWorld :propagatedValue nil)
(addProperty PaintField 'selectionRegion :propagatedValue nil)
(addProperty PaintField 'flashingSelection)
(addProperty PaintField 'drawSelection :propagatedValue nil)
(addProperty PaintField 'overprintingSelection)
;; PAINT
(addProperty PaintField 'paintGWorld :propagatedValue nil)
(addProperty PaintField 'paintDepth)
(addProperty PaintField 'chromaKeyColor)
(addProperty PaintField 'ditheringTransfers)

;; some defaults
(setf (paintDepth PaintField) 0
      (frameSize PaintField) '(0 0)
      (undoEnabled PaintField) t
      (wantsIdle PaintField) t)


;;_________________________________________________
;;_________________________________________________
;; BASICS
;; Here are the basic functions that the field needs to be a good
;; SK8 object.

;;_________________________________________________
;; FREEDATASTRUCTURES
;; This command frees all of the mac heap data structures
;; associated with the paint field.

(define-handler freeDataStructures (PaintField)
  ;; undoGWorld
  (when (pointerp (undoGWorld me))
    (t_deactivategcosptr (undogWorld me) :disposeNow t)
    )
  (setf (undoGWorld me) nil)
  ;; undoRegion
  (when (handlep (undoRegion me))
    (#_DisposeRgn (undoRegion me))
    )
  (setf (undoRegion me) nil)
  ;; selectionGWorld
  (when (pointerp (selectiongWorld me))
    (t_deactivategcosptr (selectiongWorld me) :disposeNow t)
    )
  (setf (selectiongWorld me) nil)
  ;; selectionRegion
  (when (handlep (selectionRegion me))
    (#_DisposeRgn (selectionRegion me))  ;;;****CHECK ME
    )
  (setf (selectionRegion me) nil)
  ;; paintgWorld
  (when (pointerp (paintGWorld me))
    (t_deactivategcosptr (paintGWorld me) :disposeNow t)
    )
  (setf (paintGWorld me) nil)
  )


;;_________________________________________________
;; ALLOCATEDATASTRUCTURES
;; This singles command creates all of the needed data structures
;; for a fully functioning paint window.

(define-handler allocateDataStructures (PaintField &key (erase t))
  (without-interrupts
   ;; dispose the old data
   (freeDataStructures me)
   ;; make the new ones
   (gs:let+ ((boundsRect (:rect))
             (pixelDepth (or (paintDepth me)
                             0))
             (undoEnabled (undoEnabled me))
             uWorld pWorld)
     ;; determine the rect size
     (#_SetRect boundsRect 0 0 
      (round (width me :physical t)) 
      (round (height me :physical t)))
     ;; paint gWorld
     (if (setq pWorld (T_NewGworldGC boundsRect :depth pixelDepth :noGC t))
       ;; we have the main world
       (progn
         ;; store it!
         (setf (paintgWorld me) pWorld)
         ;; erase it
         (if erase (eraseField me))
         ;; make the undo world
         (if (and undoEnabled
                  (setq uWorld (T_NewGworldGC boundsRect :depth pixelDepth :noGC t)))
           ;; undo world created
           (progn
             (setf (undoEnabled me) t)
             (setf (undogWorld me) uWorld)
             (setf (undoRegion me) (#_NewRgn))
             ;; erase the undo world
             (when erase
               (saveForUndo me)
               ))
           ;; undo NOT created
           (progn
             (setf (undoEnabled me) nil)
             (setf (undoGWorld me) nil)
             (setf (undoRegion me) nil)))
         ;; make the selection
         (setf (selectionRegion me) (#_NewRgn))
         (#_SetEmptyRgn (selectionRegion me))
         t)
       ;; can't make the main world!!
       (progn
         (setf (paintgWorld me) nil)
         (ed-beep)
         nil)))))

;; get paintgWorld, making sure that it exists, without disturbing the data structures if it already exists
(define-handler ensurePaintgWorld :private (Paintfield)
                (unless (or (eq me PaintField) (pointerp (paintgworld me)))
                  (allocateDataStructures me))
                (paintgWorld me))

;;_________________________________________________
;; INITIALIZE
;; Allocates the needed data structures for this paintField.

(define-handler initialize (PaintField original child args)
  (declare (ignore-if-unused original child args)) 
  ;; Clear all dangerous slots.
  ;; These slots should not have been copied from the parent, but just in case they were, here they are cleared
  ;; Their values must be thrown away so that they are not freed in the following call!!
  (setf (undoGWorld me) nil
        (selectionGWorld me) nil
        (paintGWorld me) nil
        (undoRegion me) nil
        (selectionRegion me) nil)
  ;; do the usual -- if this sets the boundsrect it will force a call to resize which will allocate the data structures
  (call-next-method)
  ;; allocate the data structures if they haven't been yet
  (ensurePaintgWorld me)
  (setf (fillColor me) me))


;;_________________________________________________
;; RESIZED
;; Re-allocates the paint buffer whenever the paintArea

(define-handler resized (paintField)
  ;; for now, just blow away all of the data therein...
  (allocateDataStructures me))


;;_________________________________________________
;; PRESERVE
;; Gets this thing ready for saving...

(define-handler preserve (PaintField)
  (freeDataStructures me)
  (call-next-method))

;;_________________________________________________
;; RESTORE
;; Brings this thing back from the dead..

(define-handler restore (PaintField)
  (call-next-method)
  (unless (eq me PaintField)
    (ensurePaintgWorld me))
  )


;;_________________________________________________
;; INITIALIZEFROMSTORE
;; Brings this thing back from the store

(define-handler initializeFromStore (PaintField)
  (call-next-method)
  (unless (eq me PaintField)
    (allocateDataStructures me :erase t)))

;;_________________________________________________
;; TRANSLUCENT
;; Returns T if the paintField has a translucent color.

(define-handler translucent (PaintField)
  (chromaKeyColor me))

;;_________________________________________________
;; CHROMAKEYCOLOR
;; Have to reset fillcolor to self if chromaKeyColor goes from nil to something
;; or from something to nil. But not in other cases.

(define-handler (setf chromaKeyColor) (newValue PaintField)
  (let ((oldValue (chromaKeyColor me)))
    (setValue 'chromaKeyColor me newValue)
    (if (xor oldValue newValue)
      (setf (fillColor me) me)
      (forceRedraw me))
    (chromaKeyColor me)))

;;_________________________________________________
;; RENDER
;; Draws the actor by blasting the pixmap right onto the screen.

(define-handler render (PaintField theActor theRegion thePaper)
  (if (pointerp (paintgWorld me))
    ;; we have a gWorld - blast it!
    (gs:let+ ((oldClip (:region))
              (newClip (:region))
              (destRect (:rect))
              (curgWorld (gs::get-gWorld))
              (paintgWorld (paintgWorld me))
              (paintPixmap (pref paintGWorld :cgrafport.portpixmap)) ;; (get-record-field paintgWorld :cgrafport :portpixmap)
              (curPixmap (pref curGWorld :cgrafport.portpixmap)) ;; (get-record-field curGWorld :cgrafport :portpixmap)
              (gRect (gs:hrref PaintPixmap :pixmap.bounds :storage :pointer)) ;; (get-record-field PaintPixmap :pixmap :bounds)
              (xMode (if (ditheringTransfers me) 64 8))
              (mColor (chromaKeyColor me))
              sPix)
      ;; get the destinate rect
      (gs::SK8Rect-to-rect (gs:node-physicalBoundsRect me) destRect)
      (#_GetClip oldClip)
      (#_SectRgn oldClip theRegion newClip)
      (#_ForeColor #$blackCOlor)
      (#_BackColor #$whiteColor)
      (#_LockPixels paintPixmap)
      (#_SetClip newClip)
      ;; blast those bits!
      (if (chromaKeyColor me)
        ;; blast it translucently
        (%stack-block ((tempColor 6))
          (rset tempColor :rgbColor.red (car mColor))
          (rset tempColor :rgbColor.green (cadr mColor))
          (rset tempColor :rgbColor.blue (caddr mColor))
          (#_RGBBackColor tempColor)
          (gs:copy-bits paintPixmap
                              curPixMap
                              gRect
                              destRect
                              #$transparent)
          (#_BackColor #$whiteColor))        
        ;; blast it normally
        (gs:copy-bits paintPixmap
                            curPixMap
                            gRect
                            destRect
                            xMode))
      ;; do selection stuff?
      (when (handlep (selectionRegion me))
        ;; clip the selection
        (#_CopyRgn (selectionRegion me) newClip)
        ;; scale it
        (#_MapRgn newClip gRect destRect)
        ; (#_OffsetRgn newClip (rref destRect :Rect.left) (rref destRect :rect.top))
        ; (#_SectRgn oldClip newCLip newClip)
        ;; are we overprinting the selection?
        (when (and (overprintingSelection me)
                   (pointerp (SelectiongWorld me)))
          ;; we are
          (setq sPix (pref (SelectiongWorld me) :cgrafport.portpixmap)) ;; (get-record-field (SelectiongWorld me) :cgrafport :portpixmap)
          (copy-record (gs:hrref newClip :region.rgnbbox :storage :pointer) :rect destRect) ;; (get-record-field newClip :region :rgnbbox)
          (#_LockPixels sPix)
          (gs:copy-bits sPix
                              curPixmap
                              (gs:hrref sPix :pixmap.bounds :storage :pointer) ;; (get-record-field sPix :pixmap :bounds)
                              destRect
                              xMode
                              newClip)
          (#_UnlockPixels sPix))
        ;; are we drawing the selection?
        (when (drawSelection me)
          (#_PenMode #$PatXor)
          (#_FrameRgn newClip)
          (#_PenMode #$PatCopy)))
      (#_UnlockPixels paintPixmap)
      (#_SetClip oldClip)
      )
    ;; no gWorld - paint it white
    (render white theActor theRegion thePaper)))


;;_________________________________________________
;;_________________________________________________
;; FUNCTIONS
;; Here are some handy functions that you may want to use.

;;_________________________________________________
;; SK8COLORTORGB
;; Given a SK8 color and an RGB color, fills in the RGB record
;; with the SK8 color's foreColors.

(define-sk8-function SK8ColorToRGB nil (SK8Color macColor)
  (rset macColor :rgbColor.red (foreRed SK8Color))
  (rset macColor :rgbColor.green (foreGreen SK8Color))
  (rset macColor :rgbColor.blue (foreBlue SK8Color)))


;;_________________________________________________
;; GETREGIONFROMUSER
;; Given a starting location and a region, gets a freehand shape
;; from the user and sets the region to that shape.  The region
;; drawing is clipped to the clipActor.

(define-sk8-function getRegionFromUser nil
  (theRegion &key (startLoc (list (eventH) (eventV))) (clipActor stage))
  (gs:let+ ((startH (round (car startLoc)))
            (startV (round (cadr startLoc)))
            (oldX startH)
            (oldY startV)
            (clipRect (:rect))
            (drawingPort (gs:get-wmgr-port))
            (portRect (boundsRect clipActor :physical t))
            newX newY mouseLoc minX minY maxX maXY)
    (without-interrupts
     (with-port drawingPort
       (#_PenSize 1 1)
       (#_PenPat cl-user::*black-pattern*)
       (#_PenMode #$PatXor)
       (#_OpenRgn)
       (#_ShowPen))
     ;; set the mins
     (setq minX (round (car portRect))
           minY (round (cadr portRect))
           maxX (round (caddr portRect))
           maxY (round (cadddr portRect))
           oldX (max minX (min maxX oldX))
           oldY (max minY (min maxY oldY)))
     ;; set the clip
     (#_SetRect clipRect minX minY maxX maxY)
     ;; save the start loc
     (setq startH oldX startV oldY)
     (with-port drawingPort
       (#_ClipRect clipRect)
       (#_MoveTo oldX oldY))
     ;; loop
     (loop
       ;; return if mouse is up
       (if (not (mouse-down-p))
         (return))
       ;; get the new mouseLoc
       (setq mouseLoc (mouseLoc stage)
             newX (max minX (min maxX (round (car mouseLoc))))
             newY (max minY (min maxY (round (cadr mouseLoc)))))
       ;; check it if has moved
       (if (or (/= newX oldX)
               (/= newY oldY))
         ;; it has moved
         (progn
           (setq oldX newX oldY newY)
           (with-port drawingPort
             (#_PenSize 1 1)
             (#_PenPat cl-user::*black-pattern*)
             (#_PenMode #$PatXor)             
             (#_LineTo newX newY))
           (sleep (/ 60))
           )))
     ;; ok, the mouse is up
     (with-port drawingPort
       (#_PenSize 1 1)
       (#_PenPat cl-user::*black-pattern*)
       (#_PenMode #$PatXor)
       (#_LineTo startH startV)
       (#_CloseRgn theRegion)
       (#_PenMode #$PatCopy))
     
     ))
  theRegion)

;;_________________________________________________
;; REALCOLOR
;; Given an RGB color record, fills in the record with the actual
;; color that SK8 would draw with in the specified gWorld.

(define-sk8-function RealColor nil
  (RGB1 gWorld)
  (let* ((depth (href (#_GetGWorldPixmap gWorld) :pixmap.pixelSize)) ;; (get-record-field (#_GetGWorldPixmap gWorld) :pixmap :pixelSize)
         index)
    (when (<= depth 8)
      (gs::with-gWorld 
       (gWorld)
       (setq index (#_color2Index RGB1))
       (#_Index2Color index RGB1))))
  RGB1)


;;_________________________________________________
;; RGB=
;; Given two RGB colors and a grafPort, returns T if these colors
;; are the same in that port.

(define-sk8-function RGB= nil
  (RGB1 RGB2 gWorld)
  (declare (ignore gWorld))
  (and (= (rref RGB1 :rgbColor.red) (rref RGB2 :rgbColor.red))
       (= (rref RGB1 :rgbColor.green) (rref RGB2 :rgbColor.green))
       (= (rref RGB1 :rgbColor.blue) (rref RGB2 :rgbColor.blue))))


;;_________________________________________________
;;_________________________________________________
;; GENERAL FIELD SUPPORT
;; These functions are simple field functionality.

;;_________________________________________________
;; FILLFIELD
;; Fills the paint field with an RGBcolor.

(define-handler fillField (PaintField fillColor)
  (gs:let+ ((paintgWorld (paintgWorld me))
            (fillRGB (:rgbColor)))
    (when (pointerp paintgworld)
      (SK8ColortoRGB fillColor fillRGB)
      ;; get the destinate rect
      ;; blast those bits!
      (without-interrupts
       (gs::with-gWorld 
        ((paintgWorld me))
        (#_RGBForeColor fillRGB)
        (#_PaintRect (gs:hrref (gs:gWorld-pixmap paintgWorld) :pixmap.bounds :storage :pointer))) ;; (get-record-field (cl-user::gWorld-pixmap paintgWorld) :pixmap :bounds)
       ;; redraw
       (forceRedraw me)))))


;;_________________________________________________
;; ERASEFIELD
;; Erases the paint field.  You can specify an RGBcolor to use.

(define-handler eraseField (PaintField &key (eraseColor white))
  (fillField me eraseColor))

;;_________________________________________________
;; STOREFIELDPIXMAP
;; Captures the PaintField's current image to a PixMap.

(define-handler storeFieldPixmap (PaintField thePixmap)
  (CapturePicture thePixmap me)
  thePixmap)

;;_________________________________________________
;; SAVEFORUNDO
;; Copies the pixmap into the undo buffer, and the current selection
;; into the undo region.
;; Call this right before you do something you want to be able to undo.

(define-handler saveForUndo (PaintField)
  ;; only do it if undo is enabled
  (if (undoEnabled me)
    ;; do it
    (without-interrupts 
     (let* ((paintgWorld (paintgWorld me))
            (paintPixmap (and (pointerp paintgWorld)
                              (gs:gWorld-pixmap paintgWorld)))
            (undoGWorld (undoGWorld me))
            (undoPixmap (and (pointerp undoGWorld)
                             (gs:gWorld-pixmap undoGWorld))))
       (if (and paintPixmap undoPixmap)
         (progn
           (#_LockPixels paintPixmap)
           (#_LockPixels undoPixmap)
           (with-port (gs:get-wmgr-port)
             (gs:copy-bits paintPixmap
                                 undoPixmap
                                 (gs:hrref paintPixmap :pixmap.bounds :storage :pointer)  ;; (get-record-field paintPixmap :pixmap :bounds)
                                 (gs:hrref undoPixmap :pixmap.bounds :storage :pointer))) ;; (get-record-field undoPixmap :pixmap :bounds)
           (#_UnlockPixels undoPixmap)
           (#_UnlockPixels paintPixmap)
           t)
         nil)))
    nil))


;;_________________________________________________
;; doUNDO
;; Undoes the last action.  Works by swapping the paintgWorld and
;; the undogWorld.  Therefore, it does a redo when called twice in
;; a row.

(define-handler doUndo (PaintField)
  ;; only do it if undo is enabled
  (if (undoEnabled me)
    ;; do it
    (let ((paintgWorld (paintgWorld me))
          (undoGWorld (undoGWorld me))
          (undoRegion (undoRegion me))
          (selectionRegion (selectionRegion me)))
      (if (and (pointerp paintgWorld)
               (pointerp undoGWorld))
        (progn
          (setf (undoGWorld me) paintgWorld
                (paintgWorld me) undogWorld
                (undoRegion me) selectionRegion
                (selectionRegion me) undoRegion)
          (forceRedraw me)
          t)
        nil))
    nil))


;;_________________________________________________
;; IDLE
;; Flashes the selection, if any

(define-handler idle (PaintField)
  (when (and (or (flashingSelection me)
                 ) ;; (drawSelection me))
             (not (#_EmptyRgn (selectionRegion me))))
    (setf (drawSelection me)
          (not (drawSelection me)))
    (forceRedraw me)
    ))


;;_________________________________________________
;;_________________________________________________
;; SELECTION HANDLING
;; These are functions to support the selection handling.

;;_________________________________________________
;; MOUSEINSELECTION
;; returns T if the mouse is currently in the selection.
;; Works whether or not the selection is blinking just now..

(define-handler mouseInSelection (PaintField)
  (gs:let+ ((pRect (boundsRect me :physical t))
            (gRect (and (pointerp (paintgWorld me))
                        (pref (paintgWorld me) :cgrafport.portRect))) ;; (get-record-field (paintgWorld me) :cgrafport :portRect)
            (aRect (:rect))
            (tempRgn (:region))
            (curLoc (mouseLoc stage)))
    (when gRect
      (without-interrupts
       (#_SetRect aRect (car pRect) (cadr pRect) (caddr pRect) (cadddr pRect))
       (#_CopyRgn (selectionRegion me) tempRgn)
       (#_MapRgn tempRgn gRect aRect)
       (#_PtInRgn (make-point (round (car curLoc)) (round (cadr curLoc))) tempRgn)))))


;;_________________________________________________
;; SELECTIONEMPTY
;; Returns T if the selection is empty.

(define-handler selectionEmpty (PaintField)
  (#_EmptyRgn (selectionRegion me)))


;;_________________________________________________
;; CLEARSELECTION
;; Sets the selection to an empty region.

(define-handler clearSelection (PaintField)
  (#_SetEmptyRgn (selectionRegion me))
  (when (pointerp (selectiongWorld me))
    (t_deactivategcosptr (selectiongWorld me) :disposeNow t)
    (setf (selectiongWorld me) nil)))


;;_________________________________________________
;; SETRECTSELECTION
;; Sets the paintArea's selection to a rectangle, which is specified
;; in the paintarea's logical space.

(define-handler setRectSelection (PaintField rectList &key physical relative)
  (gs:let+ ((tRegion (:region))
            (left (car rectList))
            (right (caddr rectList))
            (top (cadr rectList))
            (bottom (cadddr rectList)))
    ;; perform relative scaling...
    (if relative
      (let ((theSize (size me)))
        (setq left (* (car theSize) left)
              right (* (car theSize) right)
              top (* (cadr theSize) top)
              bottom (* (cadr theSize) bottom))))
    (#_SetRectRgn tRegion (round left) (round top) (round right) (round bottom))
    ;; scale it...
    (if physical
      (scaleRegion me tRegion))
    ;; install it
    (setRegionSelection me tRegion)
    ))


;;_________________________________________________
;; SELECTIONLOCATION
;; Sets or returns the location of the center of the selection's bounding Rect.

(define-handler selectionLocation (PaintField &key physical)
  (declare (ignore physical))
  (let ((sRgn (selectionRegion me))
        tRect)
    (if (handlep sRgn)
      (progn
        (setq tRect (gs:hrref sRgn :region.rgnbbox :storage :pointer)) ;; (get-record-field sRgn :region :rgnbbox)
        (sk8-multivals (gs:f/ (+ (rref tRect :rect.right)
                                 (rref tRect :rect.left)) 2)
                       (gs:f/ (+ (rref tRect :rect.bottom)
                                 (rref tRect :rect.top)) 2)))
      nil)))

(define-handler (setf selectionLocation) (newValue PaintField &key physical relative)
  (declare (ignore physical))
  (when (handlep (selectionRegion me))
    (let ((oldRgn (selectionRegion me)))
      (if relative
        (#_OffsetRgn oldRgn
         (round (car newValue))
         (round (cadr newValue)))
        (let ((oldLoc (selectionLocation me)))
          (#_OffsetRgn oldRgn
           (round (- (car newValue) (car oldLoc)))
           (round (- (cadr newValue) (cadr oldLoc)))))))))



;;_________________________________________________
;; SELECTIONSIZE
;; Sets or returns the size of the selection's bounding rect.

(define-handler selectionSize (PaintField &key physical)
  (declare (ignore physical))
  (let ((sRgn (selectionRegion me))
        tRect)
    (if (handlep sRgn)
      (progn
        (setq tRect (gs:hrref sRgn :region.rgnbbox :storage :pointer)) ;; (get-record-field sRgn :region :rgnbbox)
        (sk8-multivals (- (rref tRect :rect.right)
                          (rref tRect :rect.left))
                       (- (rref tRect :rect.bottom)
                          (rref tRect :rect.top))))
      nil)))

(define-handler (setf selectionSize) (newValue PaintField &key physical relative)
  (declare (ignore physical))
  (let ((oldRgn (selectionRegion me))
        oldSize)
    (when (handlep oldRgn)
      (if relative
        (#_InsetRgn oldRgn
         (round (- (car newValue)) 2)
         (round (- (cadr newValue)) 2))
        (progn
          (setq oldSize (selectionSize me))
          (#_InsetRgn oldRgn
           (round (- (car oldSize) (car newValue)) 2)
           (round (- (cadr oldSize) (cadr newValue)) 2)))))))


;;_________________________________________________
;; SELECTIONBOUNDSRECT
;; Returns the rectangle that encloses the selection.

(define-handler selectionBoundsRect (PaintField &key physical)
  (declare (ignore physical))
  (let ((sRgn (selectionRegion me))
        tRect)
    (if (handlep sRgn)
      (sk8-multivals (rref (setq tRect (gs:hrref sRgn :region.rgnbbox :storage :pointer)) ;; (get-record-field sRgn :region :rgnbbox)
                           :rect.left)
                     (rref tRect :rect.top)
                     (rref tRect :rect.right)
                     (rref tRect :rect.bottom))
      nil)))

(define-handler (setf selectionBoundsRect) (newValue PaintField &key physical relative)
  (declare (ignore physical))
  (when (handlep (selectionRegion me))
    (let ((left (car newValue))
          (top (cadr newValue))
          (right (caddr newValue))
          (bottom (cadddr newValue))
          oldRect theLoc theSize)
      ;; adjust for relative...
      (when relative
        (setq oldRect (selectionBoundsRect me)
              left (+ left (car oldRect))
              top (+ top (cadr oldRect))
              right (+ right (caddr oldRect))
              bottom (+ bottom (cadddr oldRect))))
      ;; set the vars
      (setq theLoc (list (/ (+ left right) 2) (/ (+ bottom top) 2))
            theSize (list (- right left) (- bottom top)))
      ;; set the size and width
      (setf (selectionLocation me) theLoc
            (selectionSize me) theSize))))


;;_________________________________________________
;; GETSELECTIONRECT
;; Returns the rectangle that encloses the selection.

(define-handler getSelectionRect (PaintField)
  (let ((sRgn (selectionRegion me))
        tRect)
    (if (handlep sRgn)
      (sk8-multivals (rref (setq tRect (gs:hrref sRgn :region.rgnbbox :storage :pointer)) ;; (get-record-field sRgn :region :rgnbbox)
                           :rect.left)
                     (rref tRect :rect.top)
                     (rref tRect :rect.right)
                     (rref tRect :rect.bottom))
      nil)))


;;_________________________________________________
;; SETSELECTIONRECT
;; Sets the paintArea's selection to a to a new size, while keeping
;; its same shape.  May go outside the bounds of the shape...

(define-handler setSelectionRect (PaintField left top right bottom)
  (when (handlep (selectionRegion me))
    (#_SetRect (gs:hrref (SelectionRegion me) :region.rgnbbox :storage :pointer) ;; (get-record-field (SelectionRegion me) :region :rgnbbox)
     left top right bottom)))


;;_________________________________________________
;; STOREFIELDPIXMAP
;; Given a child of PixelMap, stores the field into it.  Does it
;; the dumb way for now...

(define-handler storeFieldPixmap (PaintField thePixmap)
  (CapturePicture thePixmap me)
  thePixmap)


;;_________________________________________________
;; RETRIEVEFIELDPIXMAP
;; Given a pixmap, blasts it into the field.

(define-handler retrieveFieldPixmap (PaintField thePixmap)
  (let ((mygworld (ensurePaintgWorld me)))
    (when (pointerp mygworld)
      (transfer-snapshot-pixmap 
       thePixmap
       mygworld
       (pref mygworld :cgrafPort.portRect)) ;; (get-record-field mygworld :cgrafPort :portRect)
      (forceRedraw me))))


;;_________________________________________________
;; SCALEREGION
;; Given a region is the actor's space, converts it into the field's local space.

(define-handler scaleRegion (PaintField theRegion)
  (gs:let+ ((gWorld (ensurePaintgWorld me))
            (gRect (and (pointerp gWorld)
                        (pref gWorld :cgrafPort.portRect))) ;; (get-record-field gWorld :cgrafPort :portRect)
            (pRect (:rect))
            (aRect (boundsRect me :physical t)))
    (when gRect
      (#_SetRect pRect (car aRect) (cadr aRect) (caddr aRect) (cadddr aRect))
      (#_MapRgn theRegion pRect gRect)))
  theRegion)


;;_________________________________________________
;; SCALERECT
;; Given a region is the actor's space, converts it into the field's local space.

(define-handler scaleRect (PaintField theRectList)
  (gs:let+ ((gWorld (ensurePaintgWorld me))
            (gRect (and (pointerp gWorld)
                        (pref gWorld :cgrafPort.portRect))) ;; (get-record-field gWorld :cgrafPort :portRect)
            (pRect (:rect))
            (aRect (boundsRect me :physical t))
            (rRect (:rect)))
    (when gRect
      (#_SetRect rRect (car theRectList) (cadr theRectList) (caddr theRectList) (cadddr theRectList))
      (#_SetRect pRect (car aRect) (cadr aRect) (caddr aRect) (cadddr aRect))
      (#_MapRect rRect pRect gRect)
      (setq theRectList (list (rref rRect :rect.left)
                              (rref rRect :rect.top)
                              (rref rRect :rect.right)
                              (rref rRect :rect.bottom)))))
  theRectList)


;;_________________________________________________
;; SETREGIONSELECTION
;; Given a region, sets the selection to that region.  Trims the region
;; to the paint area, if need be.

(define-handler setRegionSelection (PaintField theRegion)
  (gs:let+ ((tRegion (:region)))
    (#_SetRectRgn tRegion 0 0 (round (width me :physical t))
     (round (height me :physical t)))
    (#_SectRgn theRegion tRegion tRegion)
    (#_CopyRgn tRegion (selectionRegion me))
    ))


;;_________________________________________________
;; FILLSELECTION
;; Fills in the selection with the specified color.

(define-handler fillSelection (PaintField theColor)
  (gs:let+ ((gWorld (paintgWorld me))
            (tColor (:rgbColor)))
    (when (pointerp gWorld)
      (SK8ColortoRGB theColor tColor)
      (without-interrupts
       (gs::with-gWorld 
         (gWorld)
         (#_ClipRect (pref gWorld :cgrafPort.portRect)) ;; (get-record-field gWorld :cgrafPort :portRect)
         (#_RGBForeColor tColor)
         (#_PaintRgn (selectionRegion me))))
      ;; redraw
      (forceRedraw me))))


;;_________________________________________________
;; GRABSELECTION
;; Copies the selectionRegion from the gWorld to the saved world.

(define-handler grabSelection (PaintField)
  (declare (optimize (safety 3) (space 0) (debug 0) (speed 0)))
  (when (and (handlep (selectionRegion me))
             (pointerp (paintGWorld me)))
    (gs:let+ ((tRect (:rect))
              (paintgWorld (paintgWorld me))
              (paintPix (#_GetgWorldPixmap paintgWorld)) ;; (get-record-field paintgWorld :cgrafport :portpixmap)) ;; (cl-user::gWorld-pixmap paintgWorld))
              (selectiongWorld (selectiongWorld me))
              (sRegion (selectionRegion me))
              sPix)
      (without-interrupts
       (copy-record (gs:hrref sRegion :region.rgnbbox :storage :pointer) :rect tRect) ;; (get-record-field sRegion :region :rgnbbox)
       ;; set the rect
       (#_offsetRect tRect (- (rref tRect :rect.left))
        (- (rref tRect :rect.top)))
       ;; dispose the old world, if any
       (if (pointerp selectiongWorld)
         (#_DisposegWorld selectiongWorld))
       (setf (selectiongWorld me) nil)
       ;; make the new one
       (When (not (#_EmptyRect tRect))
         (setq selectiongWorld (T_NewGworldGC tRect :depth 0 :noGC t))
         ;; did we get it?
         (if (pointerp selectiongWorld)
           ;; we did - do it!
           (progn
             ;; save the gWorld
             (setf (selectionGWorld me) selectiongWorld)
             ;; lock it down
             (setq sPix (#_GetgWorldPixmap selectiongWorld)) ;; (get-record-field selectiongWorld :cgrafport :portpixmap)) ;; (cl-user::gworld-pixmap selectiongWorld))
             (#_LockPixels sPix)
             (#_LockPixels paintPix)
             (with-port (gs:get-wmgr-port)
               (#_ClipRect (pref (gs:get-wmgr-port) :cgrafPort.portRect)) ;; (get-record-field (cl-user::get-wmgr-port) :cgrafPort :portRect)
               (gs:copy-bits paintPix
                                   sPix
                                   (gs:hrref sRegion :region.rgnbbox :storage :pointer) ;; (get-record-field sRegion :region :rgnbbox)
                                   tRect))
             (#_UnlockPixels paintPix)
             (#_UnlockPixels sPix)
             t)
           ;; didn't get it
           (progn
             ;; 
             nil))
         ;; clean up
         
         )))))


;;_________________________________________________
;; PRINTSELECTION
;; Prints the selection FROM the selectionGWorld into the current
;; selectionRegion, which may have been resized or moved...

(define-handler printSelection (PaintField)
  ;; are we OK for this?
  (when (and (handlep (selectionRegion me))
             (pointerp (selectionGWorld me))
             (pointerp (paintGWorld me)))
    ;; set up the vars
    (let* ((paintgWorld (paintgWorld me))
           (paintPix (pref paintgWorld :cgrafport.portpixmap)) ;; (get-record-field paintgWorld :cgrafport :portpixmap)
           (selectiongWorld (selectiongWorld me))
           (sPix (pref selectiongWorld :cgrafport.portpixmap));; (get-record-field selectiongWorld :cgrafport :portpixmap)
           (sRegion (selectionRegion me))
           (xMode (if (ditheringTransfers me) 64 8))
           )
      (without-interrupts
       ;; do it!
       (#_LockPixels sPix)
       (#_LockPixels paintPix)
       (cl-user::with-port
        (gs:get-wmgr-port)
        (#_ClipRect (pref (gs:get-wmgr-port) :cgrafPort.portRect)) ;; (get-record-field (cl-user::get-wmgr-port) :cgrafPort :portRect)
        (gs:copy-bits sPix
                            paintPix
                            (gs:hrref sPix :pixmap.bounds :storage :pointer) ;; (get-record-field sPix :pixmap :bounds)
                            (gs:hrref sRegion :region.rgnbbox :storage :pointer) ;; (get-record-field sRegion :region :rgnbbox)
                            xMode
                            sRegion))
       (#_UnlockPixels paintPix)
       (#_UnlockPixels sPix)
       (forceRedraw me)
       t))))


;;_________________________________________________
;; DRAGSELECTION
;; Drags the selection around the screen.  Does not do any writing into
;; the gWorlds at all.  Use PRINTGWORLD for that.
;; Note that in order to SEE the selection, the field's overprintingSelection
;; property must be true.

(define-handler dragSelection (PaintField &key (startH (car (mouseLoc me))) (startV (cadr (mouseLoc me))))
  ;; can we even do this?
  (when (and (handlep (selectionRegion me))
             (pointerp (paintGWorld me)))
    ;; we can - set up the vars
    (let ((sRegion (selectionRegion me))
          (oldX (round startH))
          (oldY (round startV))
          newX newY mouseLoc)
      (without-interrupts
       (setf (drawSelection me) (not (pointerp (selectiongWorld me))))
       ;; now, loop!
       (loop
         ;; return if mouse is up
         (if (not (mouse-down-p))
           (return))
         ;; get the new mouseLoc
         (setq mouseLoc (mouseLoc me)
               newX (round (car mouseLoc))
               newY (round (cadr mouseLoc)))
         ;; did we move it?
         (if (or (/= newx oldX) (/= newY oldY))
           ;; we did
           (progn
             ;; offset the region
             (#_OffsetRgn sRegion (- newX oldX) (- newY oldY))
             (setq oldX newX oldY newY)
             ;; redraw the actor
             (forceRedraw me))))
       ;; store the region
       (setf (selectionRegion me) sRegion)
       ))))


;;_________________________________________________
;; DRAGSELECTION
;; Resizes the selection on the screen from its bottomRight  Does not do any writing into
;; the gWorlds at all.  Use PRINTGWORLD for that.
;; Note that in order to SEE the selection, the field's overprintingSelection
;; property must be true.
;; This routine takes special care not to screw up the region 
;; during extremes of scaling.

(define-handler resizeSelection (PaintField &key (startLoc (mouseloc me)))
  ;; can we even do this?
  (when (and (handlep (selectionRegion me))
             (pointerp (paintGWorld me)))
    ;; we can - set up the vars
    (gs:let+ ((sRegion (selectionRegion me))
              (oldX (round (car startLoc)))
              (oldY (round (cadr startLoc)))
              (sX oldX)
              (sy oldY)
              (saveRgn (:region))
              newX newY mouseLoc dx dy)
      ;; init the region
      (#_CopyRgn sRegion saveRgn)
      (without-interrupts
       (setf (drawSelection me) (not (pointerp (selectiongWorld me))))
       ;; now, loop!
       (loop
         ;; return if mouse is up
         (if (not (mouse-down-p))
           (return))
         ;; get the new mouseLoc
         (setq mouseLoc (mouseLoc me)
               newX (round (car mouseLoc))
               newY (round (cadr mouseLoc)))
         ;; did we move it?
         (if (or (/= newx oldX) (/= newY oldY))
           ;; we did
           (progn
             ;; determine the offset
             (setq dx (- newX sx)
                   dy (- newY sy)
                   oldX newX
                   oldY newY)
             ;; restore
             (#_CopyRgn saveRgn sRegion)
             ;; offset
             (#_OffsetRgn sRegion (round dx 2) (round dy 2))
             (#_InsetRgn sRegion (- (round dx 2)) (- (round dy 2)))
             ;; redraw the actor
             (forceRedraw me))))
       ;; store the region
       (setf (selectionRegion me) sRegion)
       ))))


;;_________________________________________________
;;_________________________________________________
;; BRUSH STUFF

;;_________________________________________________
;; DRAWWITHBRUSH
;; Given a brush size and a paint color, draws with a square brush.

(define-handler drawWithBrush (PaintField &key (startLoc (mouseLoc me)) (brushSize '(4 4)) (paintColor black) ((pointlist thepointList) nil))
  (gs:let+ ((gWorld (paintgWorld me))
            (tColor (:rgbColor))
            (oldX (if thepointList 
                    (truncate (car thepointList))
                    (truncate (car startLoc))))
            (oldY (if thepointList 
                    (truncate (cadr thepointList))
                    (truncate (cadr startLoc))))
            (newX oldX)
            (newY oldY)
            (numPoints (/ (length thepointList) 2)))
    ;; have we got a gWorld?
    (when (pointerp gWorld)
      (without-interrupts
       (SK8ColortoRGB paintColor tColor)
       ;; set up the world
       (gs::with-gWorld 
        (gWorld)
        (#_RGBForeColor tColor)
        (#_ClipRect (pref gWorld :cGrafPort.portRect)) ;; (get-record-field gWorld :cGrafPort :portRect)
        (#_PenSize (round (car brushSize)) (round (cadr brushSize)))
        (#_MoveTo oldX oldY)
        (#_LineTO newX newY))
       (forceRedraw me)
       ;; do the loop
       (if thepointList
         (progn
           (gs::with-gWorld 
            (gWorld)
            (doTimes (i numPoints)
              (#_LineTo (truncate (nth (* i 2) thepointList))
               (truncate (nth (1+ (* i 2)) thepointList)))))
           (forceRedraw me))
         (loop
           ;; check if mouse is down
           (if (not (mouse-down-p))
             (return))
           ;; get the new mouseLoc
           (setq startLoc (mouseLoc me)
                 newX (truncate (car startLoc))
                 newY (truncate (cadr startLoc)))
           ;; did we move?
           (if (or (/= newX oldX)
                   (/= newY oldY))
             ;; we did!
             (progn
               ;; draw the line
               (gs::with-gWorld 
                (gWorld)
                ; (#_MoveTo oldX oldY)
                (#_LineTO newX newY))
               ;; update the vars
               (setq oldX newX oldY newY)
               ;; redraw
               (forceRedraw me)))))))))

;;_________________________________________________
;; DRAWWITHICON
;; Given an icon, uses it as a paint brush.

(define-handler drawWithIcon (PaintField &key (startLoc (mouseLoc me)) (paintIcon trashEmptyIcon))
  (gs:let+ ((gWorld (paintgWorld me))
            (oldX (round (car startLoc)))
            (oldY (round (cadr startLoc)))
            (paintPix (pref gWorld :cgrafPort.portPixmap)) ;; (get-record-field gWorld :cgrafPort :portPixmap)
            (iconRect (:rect))
            (masterRect (:rect))
            (newX oldX)
            (newY oldY)
            (macHandle (withResourceFile ((file paintIcon))
                         (#_GetCIcon (resourceID paintIcon)))))
    ;; have we got a gWorld?
    (when (and (pointerp gWorld)
               (handlep macHandle))
      (without-interrupts
       (#_DetachResource machandle)
       (#_HLock macHandle)
       ;; set the rect
       (#_SetRect IconRect 0 0 (round (car (size paintIcon)))
        (round (cadr (size paintIcon))))
       ;; get the rect
       ;; (get-record-field (get-record-field macHandle :cicon :iconpMap) :pixmap :bounds)
       (copy-record (pref (gs:hrref macHandle :cicon.iconpMap :storage :pointer) :pixmap.bounds) :rect masterRect)
       ;; offset to center at the mouse
       (#_OffsetRect IconRect 
        (- (round (car startLoc)) (round (car (size paintIcon)) 2))
        (- (round (cadr startLoc)) (round (cadr (size paintIcon)) 2)))
       ;; get things going!
       (with-port gWorld
         (#_LockPixels paintPix)
         (#_PlotCIcon IconRect macHandle)
         (#_UnLockPixels paintPix))
       (forceRedraw me)
       ;; do the loop
       (loop
         ;; check if mouse is down
         (if (not (mouse-down-p))
           (return))
         ;; get the new mouseLoc
         (setq startLoc (mouseLoc me)
               newX (round (car startLoc))
               newY (round (cadr startLoc)))
         ;; did we move?
         (if (or (/= newX oldX)
                 (/= newY oldY))
           ;; we did!
           (progn
             ;; offset the rect
             (#_OffsetRect IconRect (- newX oldX) (- newY oldY))
             ;; draw the Icon
             (with-port gWorld
               (#_LockPixels paintPix)
               (#_PlotCIcon IconRect macHandle)
               (#_UnLockPixels paintPix))
             ;; update the vars
             (setq oldX newX oldY newY)
             ;; redraw
             (forceRedraw me)
             )))
       ;; unlock
       (#_HUnLock macHandle)
       (#_Disposecicon macHandle)))))


;;_________________________________________________
;; DRAWWITHSELECTION
;; Uses the current selection as a paint brush.

(define-handler drawWithSelection (PaintField &key (startLoc (mouseLoc me)))
  (gs:let+ ((gWorld (paintgWorld me))
            (oldX (round (car startLoc)))
            (oldY (round (cadr startLoc)))
            (paintPix (pref gWorld :cgrafPort.portPixmap)) ;; (get-record-field gWorld :cgrafPort :portPixmap)
            (masterRect (:rect))
            (newX oldX)
            (newY oldY)
            (sgWorld (selectiongWorld me))
            (sPix (and (pointerp sgWorld)
                       (#_getgWorldPixmap sgWorld)))
            (sRegion (selectionRegion me))
            (tRegion (:region))
            (xMode (if (ditheringTransfers me) 64 8))
            )
    ;; have we got a gWorld?
    (when (and (pointerp gWorld)
               (pointerp sgWorld))
      (without-interrupts
       ;; set the rect
       (copy-record (gs:hrref sPix :pixmap.bounds :storage :pointer) :rect masterRect) ;; (get-record-field sPix :pixmap :bounds)
       (#_copyrgn sRegion tregion)
       ;; get the rect
       ;; offset to center at the mouse
       (#_OffsetRgn tregion 
        (- (rref (gs:hrref tregion :region.rgnbbox :storage :pointer) :rect.left)) ;; (get-record-field (get-record-field tregion :region :rgnbbox) :rect :left)
        (- (rref (gs:hrref tregion :region.rgnbbox :storage :pointer) :rect.top))) ;; (get-record-field (get-record-field tregion :region :rgnbbox) :rect :top)
       (#_OffsetRgn tregion 
        (- (round (car startLoc)) (round (rref masterRect :rect.right) 2))
        (- (round (cadr startLoc)) (round (rref masterRect :rect.bottom) 2)))
       ;; get things going!
       (with-port (gs:get-wmgr-port)
         (#_LockPixels paintPix)
         (#_LockPixels sPix)
         (gs:copy-bits sPix paintPix masterRect (gs:hrref tregion :region.rgnbbox :storage :pointer) xMode tRegion) ;; (get-record-field tregion :region :rgnbbox)
         (#_UnLockPixels sPix)
         (#_UnLockPixels paintPix))
       (forceRedraw me)
       ;; do the loop
       (loop
         ;; check if mouse is down
         (if (not (mouse-down-p))
           (return))
         ;; get the new mouseLoc
         (setq startLoc (mouseLoc me)
               newX (round (car startLoc))
               newY (round (cadr startLoc)))
         ;; did we move?
         (if (or (/= newX oldX)
                 (/= newY oldY))
           ;; we did!
           (progn
             ;; offset the rect
             (#_OffsetRgn tregion (- newX oldX) (- newY oldY))
             ;; draw the Icon
             (with-port (gs:get-wmgr-port)
               (#_LockPixels paintPix)
               (#_LockPixels sPix)
               (gs:copy-bits sPix paintPix masterRect (gs:hrref tregion :region.rgnbbox :storage :pointer) xMode tRegion) ;; (get-record-field tregion :region :rgnbbox)
               (#_UnLockPixels sPix)
               (#_UnLockPixels paintPix))
             ;; update the vars
             (setq oldX newX oldY newY)
             ;; redraw
             (forceRedraw me)
             )))
       ;; unlock
       ))))


;;_________________________________________________
;; SEEDFILL
;; Given a fillLoc and a color, will do a MacPaint-paintBucket type of
;; fill starting at the fillLoc.

(define-handler seedFill (PaintField &key (fillLoc '(0 0)) (fillColor red))
  (let ((gWorld (ensurePaintgWorld me)))
    (when (pointerp gWorld)
      (gs:let+ ((paintPix (pref gWorld :cgrafport.portpixmap)) ;; (get-record-field gWorld :cgrafport :portpixmap)
                (maskRect (:rect))
                (theColor (:rgbColor))
                maskMap
                )
        (without-interrupts
         ;; make the mask
         (copy-record (gs:hrref paintPix :pixmap.bounds :storage :pointer) :rect maskRect) ;; (get-record-field paintPix :pixmap :bounds)
         (setq maskMap (gs:make-bitmap maskRect))
         ;; get an RGB color
         (SK8ColorToRGB fillColor theColor)
         ;; do it!
         (gs::with-gWorld 
          (gWorld)
          (#_LockPixels paintPix)
          ;; compute the fill area...
          (with-pointers ((cMap paintPix))
            (#_SeedCFill cMap maskMap maskRect maskRect 
             (round (car fillLoc)) (round (cadr fillLoc)) 
             (cl-user::%int-to-ptr 0) 0))
          ;; copy it back..
          (#_RGBForeColor theColor)
          (with-pointers ((cMap paintPix))
            (#_CopyMask maskMap maskMap cMap  maskRect maskRect maskRect))
          (#_UnLockPixels paintPix))
         ;; clean house!
         (#_DisposePtr maskMap)
         ;; redraw
         (forceRedraw me))))))


;;_________________________________________________
;; SHRINKSELECTION
;; Shrinks the selection down so that it contains only pixels of the
;; specified color.

(define-handler shrinkSelection (PaintField &key (shrinkColor white) (emptyRegions nil) (fastMask nil))
  (declare (ignore emptyRegions))
  (when (pointerp (selectiongWorld me))
    (gs:let+ ((gWorld (selectiongWorld me))
              (paintPix (#_GetgWorldPixmap gWorld)) ;; (get-record-field gWorld :cgrafport :portpixmap))
              (maskRect (:rect))
              (theColor (:rgbColor))
              (theRegion (:region))
              (sRegion (selectionRegion me))
              maskMap paintRect
              )
      (without-interrupts
       ;; make the mask
       (setq paintRect (gs:hrref paintPix :pixmap.bounds :storage :pointer)) ;; (get-record-field paintPix :pixmap :bounds)
       (copy-record paintRect :rect maskRect) ;; (get-record-field paintPix :pixmap :bounds)
       (setq maskMap (gs:make-bitmap maskRect))
       ;; get an RGB color
       (SK8ColorToRGB shrinkColor theColor)
       ;; real RGB
       (RealColor theColor gWorld)
       ;; do it!
       (gs::with-gWorld 
        (gWorld)
        (#_LockPixels paintPix)
        ;; compute the fill area...
        (#_RGBForeColor *black-rgb*)
        (#_RGBBackColor *white-rgb*)
        (if fastMask
          ;; use the trap
          (with-pointers ((cMap paintPix))
            (#_CalcCMask cMap maskMap paintRect maskRect theColor
             (%null-ptr) 0))
          ;; do it the slow way
          (slowStripCopy gWorld maskMap maskRect theColor))
        (#_UnLockPixels paintPix))
       ;; copy it back..
       (#_BitmapToRegion theRegion maskMap)
       (#_OffsetRgn 
        theRegion 
        (rref (gs:hrref sRegion :region.rgnbbox :storage :pointer) :rect.left) ;; (get-record-field (get-record-field sRegion :region :rgnbbox) :rect :left)
        (rref (gs:hrref sRegion :region.rgnbbox :storage :pointer) :rect.top)) ;; (get-record-field (get-record-field sRegion :region :rgnbbox) :rect :top)
       (#_DiffRgn sRegion theregion sRegion)
       ;; clean house!
       (#_DisposePtr maskMap)))
    ;; regrab
    (grabSelection me)
    ;; redraw
    (forceRedraw me)))


;;_________________________________________________
;; GETPIXELCOLOR
;; Given a location, returns a list of the red, green, and blue components
;; of that pixel.  If the keyword RGBRrecord is specified, then SK8 will
;; automatically fill in the Rgbcolor record that is passed with the proper
;; values.

(define-handler getPixelColor (PaintField pixelLoc &key (RGBrecord nil))
  (let ((mygworld (ensurePaintgWorld me)))
    (when (pointerp mygworld)
      (%stack-block ((colorPtr 6))
        (cl-user::with-port 
         (paintgWorld me)
         (#_LockPixels (gs:gWorld-pixmap (paintgWorld me)))
         (#_Getcpixel (round (Car pixelLoc)) (round (cadr pixelLoc)) colorPtr)
         (#_UnLockPixels (gs:gWorld-pixmap (paintgWorld me))))
        (if RGBrecord
          (copy-record colorPtr :RGBcolor RGBrecord))
        (list (rref colorPtr :rgbcolor.red)
              (rref colorPtr :rgbcolor.green)
              (rref colorPtr :rgbcolor.blue))
        ))))

(define-handler getPixelColor (Stage pixelLoc &key (RGBrecord nil))
  (%stack-block ((colorPtr 6))
    (cl-user::with-port 
     (gs:get-wmgr-port)
     (#_Getcpixel (round (Car pixelLoc)) (round (cadr pixelLoc)) colorPtr))
    (if RGBrecord
      (copy-record colorPtr :RGBcolor RGBrecord))
    (list (rref colorPtr :rgbcolor.red)
          (rref colorPtr :rgbcolor.green)
          (rref colorPtr :rgbcolor.blue))
    ))


;;_________________________________________________
;; SETPIXELCOLOR
;; Given a location and an RGBColor, sets the color of the pixel at the
;; location to the specified color.  You can also pass in a pointer to an
;; RGBcolor record instead of a pixelColor.

(define-handler setPixelColor (PaintField pixelLoc &key (pixelColor black) (RGBrecord nil))
  (let ((mygworld (ensurePaintgWorld me)))
    (when (pointerp mygworld)
      (gs:let+ ((theColor (:rgbColor)))
        (if RGBrecord
          (copy-record RGBrecord :RGBcolor theColor)
          (SK8colorToRGB pixelColor theColor))
        (gs::with-gWorld 
         (mygworld)
         (#_setcpixel (round (Car pixelLoc)) (round (cadr pixelLoc)) theColor)))
      t)))


;;_________________________________________________
;; ACTORINTOSELECTION
;; Given an actor, fills in the selectiongWorld with a snapshot of that
;; actor, and sets the selectionRegion to the boundsRegion of the
;; actor.  The selection will always be translated so that its topleft
;; corner is at {0,0} in the PaintField's space.  If the keyWord size
;; is specified, then the actor will be captured at that size - otherwise,
;; it is captured at actual size.
;; NOTE:
;; Currently will only work if the actor's boundsRegion is the same size
;; as the actor's boundsRect (true in most cases, but not for MaskedActors.
;; Use the without preservingShape form for these.

(define-handler actorIntoSelection (PaintField theActor &key (size (size theActor)) (preservingShape t))
  (gs:let+ ((tColor (new ImageRenderer :project (project me)))
            (tRect (new rectangle :project (project me) :frameSize '(0 0) :boundsRect (list 0 0 (car size) (cadr size))))
            (tRegion (:region))
            (gRect (:rect))
            (tempRect (:rect))
            (sGWorld (selectiongWorld me))
            (thePaper nil)
            )
    ;; dispose of old selection
    (if (pointerp sGworld)
      (#_DisposeGworld sGworld))
    (setf (selectiongWorld me) nil)
    ;; make new gWorld
    (#_SetRect gRect 0 0 (round (car size)) (round (cadr size)))
    (setq sgWorld (T_NewGworldGC gRect :depth (or (paintDepth me) 0) :noGC t))
    ;; did we get it?
    (when (pointerp sgWorld)
      ;; we did
      (setf (selectiongWorld me) sgWorld)
      ; capture the actor
      (CapturePicture tColor theActor :size size)
      (setf (fillColor tRect) tColor)
      (#_RectRgn tRegion gRect)
      ;; draw it
      (gs::with-gWorld
       (sgWorld)
       (#_LockPixels (#_GetgWorldPixmap sgWorld))
       (#_ClipRect gRect)
       (setq *clip* tRegion)
       (render tColor tRect tRegion thePaper)
       (#_UnLockPixels (#_GetgWorldPixmap sgWorld)))
      ;; compute the region
      (if preservingShape
        ;; actually scale the actor's boundsRegion
        (progn
          (#_CopyRgn (boundsRegion theActor) tRegion)
          (gs::sk8Rect-To-Rect (gs::node-physicalBoundsRect theActor) tempRect)
          (#_MapRgn tRegion tempRect gRect))
        ;; simply use a rectangle
        (progn
          (#_RectRgn tRegion gRect)))
      (SetRegionSelection me tRegion))
    ;; dispose trect, tcolor
    ))


;;_________________________________________________
;; INSERTRECT
;; Inserts a rectangle into the pixmap.

(define-handler insertRect (PaintField rectList &key (frameSize '(1 1)) (frameColor black) (fillColor white))
  (gs:let+ ((gWorld (ensurePaintgWorld me))
            (realRect (:rect))
            (realColor (:rgbColor)))
    (when (pointerp gWorld)
      (with-port gWorld
        ;; set the pen
        (#_PenMode #$PatCopy)
        ;; set the rect
        (#_SetRect realRect (round (car rectList))
         (round (cadr rectList))
         (round (caddr rectList))
         (round (cadddr rectList)))
        ;; set the rect
        (when fillColor
          (SK8colorToRGB fillColor realColor)
          (#_RGBForeColor realColor)
          (#_PaintRect realRect))
        ;; frame the Rect
        (when (not (or (not frameSize) (not frameColor)))
          (SK8ColorToRGB frameColor realColor)
          (#_RGBForeColor realColor)
          (#_PenSize (round (car frameSize)) (round (cadr frameSize)))
          (#_FrameRect realRect))))
    (forceRedraw me)))


;;_________________________________________________
;; INSERTOVAL
;; Inserts an oval into the pixmap.

(define-handler insertOval (PaintField rectList &key (frameSize '(1 1)) (frameColor black) (fillColor white))
  (gs:let+ ((gWorld (ensurePaintgWorld me))
            (realRect (:rect))
            (realColor (:rgbColor)))
    (when (pointerp gWorld)
      (with-port gWorld
        ;; set the pen
        (#_PenMode #$PatCopy)
        ;; set the rect
        (#_SetRect realRect (round (car rectList))
         (round (cadr rectList))
         (round (caddr rectList))
         (round (cadddr rectList)))
        ;; set the rect
        (when fillColor
          (SK8colorToRGB fillColor realColor)
          (#_RGBForeColor realColor)
          (#_PaintOval realRect))
        ;; frame the Rect
        (when (not (or (not frameSize) (not frameColor)))
          (SK8ColorToRGB (or frameColor black) realColor)
          (#_RGBForeColor realColor)
          (#_PenSize (round (car frameSize)) (round (cadr frameSize)))
          (#_FrameOval realRect))))
    (forceRedraw me)))


;;_________________________________________________
;; INSERTLINE
;; Inserts a line into the pixmap.

(define-handler insertLine (PaintField startLoc endLoc &key (lineSize '(1 1)) (lineColor black))
  (gs:let+ ((gWorld (ensurePaintgWorld me))
            (realColor (:rgbColor)))
    (when (pointerp gWorld)
      (with-port gWorld
        ;; set the pen
        (#_PenMode #$PatCopy)
        (SK8ColorToRGB (or lineColor black) realColor)
        (#_RGBForeColor realColor)
        (#_PenSize (round (car lineSize)) (round (cadr lineSize)))
        (#_MoveTo  (round (car startLoc)) (round (cadr startLoc)))
        (#_LineTO (round (car endLoc)) (round (cadr endLoc)))))
    (forceRedraw me)))

#|
	Change History (most recent last):
	1  	11/ 9/95	sidney  	initial checkin
	2  	11/ 9/95	sidney  	do symbol export/import at compile time so the file can be compiled
        3       09/28/96        royston         make work with new modular sk8
	2  	 9/26/96	Hernan  	Requiring slowStripCopy.
	3  	10/ 9/96	Hernan  	Need to reset the xor pattern every time we redraw.
	4  	 2/27/97	Hernan  	
|# ;(do not edit past this line!!)
