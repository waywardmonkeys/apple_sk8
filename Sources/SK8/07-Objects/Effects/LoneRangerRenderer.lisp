;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories

(in-package :sk8dev)

(provide "LONERANGERRENDERER")

(require "SLOWSTRIPCOPY" "functions;SlowStripCopy")

(SK8-declare-syms :SK8 :PUBLIC
                  SK8::DITHERINGTRANSFERS
                  SK8::ALLOCATEDATASTRUCTURES
                  SK8::FREEDATASTRUCTURES
                  SK8::PAINTGWORLD
                  SK8::IMAGESIZE
                  SK8::MAINRENDERER
                  SK8::MASKRENDERER
                  SK8::RECOMPUTEMASK 
                  SK8::QUICKMASK
                  SK8::AUTOMASK
                  SK8::AUTOMASKCOLOR
                  SK8::AUTOMASKFEATHER)

(SK8-declare-syms :SK8 :PUBLIC ;; for some reason :SPECIAL makes the "new Renderer" below crash
                  SK8::LONERANGERRENDERER
                  )
;; ________________________________________________________
;; ________________________________________________________
;; LoneRangerRenderer
;;
;; This patch adds a new child of renderer to the system.  This renderer
;; Renders masked actors.  You can specify a renderer to use for the
;; main bits, and another for the mask.  
;;
;; ________________________________________________________
;; ________________________________________________________
;; MODIFICATION HISTORY
;; 08-28-96 RFS Changes for new modular SK8 (addition of scope resolvers to some function calls)
;; 11-22-95 RFS Made defaults for automask and quickmask true
;; 11-08-95 SIDNEY Allocate data structures when loading from saved image or from store.
;;               Remove erroneous call to restore in initializefromstore
;; 10-01-95 RFS Changed some calls to make Library folder unecessary
;; 05-01-95 RFS Added some Beta 3 mods
;; 04-20-95 RFS Fixed quickmask bug in render...
;; 12-15-94 RFS Added thePaper argument to render handler...
;; 11-14-94 ICE Added quickMask feature for black and white masking...
;; 11-14-94 ICE Updated for SK8 1.0b2 (I think....)
;; 04-26-94 ICE Added dispose to get rid of data structures
;; 02-20-94 ICE Added autoMaskFeather property, which tells the computer to feather the mask's edges.
;; 02-20-94 ICE Changed recomputeMask to use slowStripCopy by default.  Also, added support for feathering the autoMask
;; 02-20-94 ICE Added SLOWSTRIPCOPY function
;; 02-16-94 DCS Changed allocateDataStructures to not call (setf imageSize)
;; 02-16-94 ICE Changed initialize & restore to prevent allocating the data
;; 02-16-94 ICE Made getter of paintgWorld allocate, if needed.
;; 02-16-94 ICE Made setter of imageSize re-allocateDataStructures
;; 01-21-93 ICE Added an autoMask property

(new Renderer :objectName "LoneRangerRenderer" :Project sk8)

(addProperty LoneRangerRenderer 'imageSize :initialValue '(100 100))
(addProperty LoneRangerRenderer 'mainRenderer :initialValue TrashEmptyRenderer)
(addProperty LoneRangerRenderer 'maskRenderer :initialValue Gray)
(addProperty LoneRangerRenderer 'paintgWorld :propagatedValue nil :initialValue nil)
(addProperty LoneRangerRenderer 'ditheringTransfers :initialValue nil)
(addProperty LoneRangerRenderer 'autoMask :initialValue t)
(addProperty LoneRangerRenderer 'autoMaskColor :initialValue '(65535 65535 65535))
(addProperty LoneRangerRenderer 'version :initialValue 1.0 :private t)
(addProperty LoneRangerRenderer 'autoMaskFeather :initialValue nil)
(addProperty LoneRangerRenderer 'quickMask :initialValue t)



;;_________________________________________________
;; FREEDATASTRUCTURES
;; This command frees all of the mac heap data structures
;; associated with the renderer.

(define-handler freeDataStructures (LoneRangerRenderer)
  ;; gWorld
  (when (pointerp (paintgWorld me :autoRecompute nil))
    (t_deactivategcosptr (paintgWorld me :autoRecompute nil) :disposeNow t))
  (setf (paintgWorld me) nil))


;;_________________________________________________
;; ALLOCATEDATASTRUCTURES
;; This single command creates all of the needed data structures
;; for a fully functioning renderer.

(define-handler allocateDataStructures (LoneRangerRenderer)
  (without-interrupts
   ;; dispose the old data
   (freeDataStructures me)
   ;; make the new ones
   (gs:let+ ((boundsRect (:rect))
             (size (or (imageSize me) '(100 100)))
             pWorld)
     ;; determine the rect size
     (if (quickMask me)
       (#_SetRect boundsRect 0 0 (round (car size)) (round (cadr size)))
       (#_SetRect boundsRect 0 0 
        (round (car size)) (1+ (* 2 (round (cadr size))))))
     ;; can't call (setf (imageSize self)) here because it calls allocateDataStructures
     (SetValue 'imageSize me (list (round (car size)) (round (cadr size))))
     ;; paint gWorld
     (if (setq pWorld (T_NewGWorldGC boundsRect :depth 0 :noGC nil))
       ;; we have the main world
       (progn
         ;; store it!
         (setf (paintgWorld me) pWorld)
         ;; erase it
         (recomputeMask me)
         t)
       ;; can't make the main world!!
       (progn
         (setf (paintgWorld me) nil)
         nil)))))


;;_________________________________________________
;; INITIALIZE
;; Allocates the needed data structures for this renderer

(define-handler initialize (LoneRangerRenderer original child args)
  (declare (ignore-if-unused original child args))
  (setf (paintgWorld me) nil)  ;; this must not be inherited -- let it be allocated when it is next needed
  ;; do the usual
  (call-next-method)
  )



;;_________________________________________________
;; PRESERVE
;; Gets this thing ready for saving...

(define-handler preserve (LoneRangerRenderer)
  (unless (eq me LoneRangerRenderer)
    (freeDataStructures me))
  (call-next-method))


;;_________________________________________________
;; RESTORE
;; Brings this thing back from the dead..

(define-handler restore (LoneRangerRenderer)
  (call-next-method)
  (paintgWorld me)   ;; forces allocation of data structures if necessary
  )


;;_________________________________________________
;; INITIALIZEFROMSTORE
;; Brings this thing back from the store
;; The default just calls restore.

(define-handler initializeFromStore (LoneRangerRenderer)
  (call-next-method)
  (paintgWorld me)   ;; forces allocation of data structures if necessary
  )


;;_________________________________________________
;; TRANSLUCENT
;; Returns T if the renderer is translucent

(define-handler translucent (LoneRangerRenderer)
  t)

;;_________________________________________________
;; RENDER
;; Draws the actor by blasting the pixmap right onto the screen.

(define-handler render (LoneRangerRenderer theActor theRegion thePaper)
  (if (pointerp (paintgWorld me))
    ;; we have a gWorld - blast it!
    (gs:let+ ((oldClip (:region))
              (newClip (:region))
              (destRect (:rect))
              (aRect (:rect))
              (mRect (:rect))
              (curgWorld (gs::get-gWorld))
              (paintgWorld (paintgWorld me))
              (paintPixmap (pref paintgWorld cgrafport.portpixmap))
              (curPixmap (pref curGWorld cgrafport.portpixmap))
              (xMode (if (ditheringTransfers me) 64 8))
              (size (imageSize me))
              (mColor (autoMaskColor me)))
      ;; get the destinate rect
      (gs::SK8Rect-to-rect (gs:node-physicalBoundsRect theActor) destRect)
      (#_SetRect aRect 0 0 (car size) (cadr size))
      (#_SetRect mRect 0 (1+ (cadr size)) (car size) (1+ (* 2 (cadr size))))
      (#_GetClip oldClip)
      (#_SectRgn oldClip theRegion newClip)
      (#_ForeColor #$blackCOlor)
      (#_BackColor #$whiteColor)
      (#_LockPixels paintPixmap)
      (#_SetClip newClip)
      ;; blast those bits!
      (with-pointers ((cpix curPixmap)
                      (gpix paintPixmap))
        (if (quickMask me)
          (%stack-block ((tempColor 6))
            (rset tempColor :rgbColor.red (car mColor))
            (rset tempColor :rgbColor.green (cadr mColor))
            (rset tempColor :rgbColor.blue (caddr mColor)) ;;<-- was car mcolor!
            (#_RGBBackColor tempColor)
            (#_CopyBits gPix cPix aRect destRect 36 (%null-ptr))
            (#_BackColor #$whiteColor))
          (#_CopyDeepMask  gPix gPix cPix
           aRect mRect destRect xMode (%null-ptr))))
      ; (cl-user::copy-bits paintPixmap curPixmap (rref paintgWorld cgrafPort.portRect) destRect)
      (#_UnlockPixels paintPixmap)
      (#_SetClip oldClip)
      )
    ;; no gWorld - paint it white
    (render white theActor theRegion thePaper)))


;;_________________________________________________
;; RECOMPUTEMASK
;; Recomputes the entire gWorld.

(define-handler recomputeMask (LoneRangerRenderer &key (fastMask nil) (feather nil))
  (gs:let+ ((tempActor (new rectangle :project (project me) :frameSize '(0 0)))
            (tempRenderer (new imageRenderer :Project (project me)))
            (tempPixmap (new pixelMap :Project (project me)))
            (size (imageSize me))
            (aRect (:rect))
            (mRect (:rect))
            (gWorld (paintgWorld me))
            (aColor (mainRenderer me))
            (mColor (maskRenderer me))
            (feather (if feather
                       feather
                       (autoMaskFeather me))))
    
    ;; get everything ready
    (setf (media tempRenderer) tempPixmap)
    (setf (fillColor tempActor) aColor)
    (setf (size tempActor) size)
    ;; get the rects
    (#_SetRect aRect 0 0 (car size) (cadr size))
    (if (quickMask me)
      (#_SetRect mRect 0 0 (car size) (cadr size))
      (#_SetRect mRect 0 (1+ (cadr size)) (car size) (1+ (* 2 (cadr size)))))
    ;; got it! - copy it in!
    (when (pointerp gWorld)
      ;; capture it!
      (setf (fillColor tempActor) aColor)
      (CapturePicture tempRenderer tempActor)
      (with-port 
        gWorld
        (transfer-snapshot-pixmap tempPixmap gWorld aRect))
      ;; capture the mask
      (unless (quickMask me)
        (setf (fillColor tempActor) mColor)
        (CapturePicture tempRenderer tempActor)
        (with-port 
          gWorld
          (transfer-snapshot-pixmap tempPixmap gWorld mRect))
        ;; now, do the mask
        (when (autoMask me)
          ;; compute the mask the "neato" way
          (gs:let+ ((bmap (gs:make-bitmap aRect))
                    (backColor (:rgbColor))
                    (maskOutColor (autoMaskColor me))
                    (gPix (pref gWorld cgrafPort.portPixmap)))
            ;; install the colors
            (rset backColor :rgbColor.red (car maskOutColor))
            (rset backColor :rgbColor.green (cadr maskOutColor))
            (rset backColor :rgbColor.blue (caddr maskOutColor))
            ;; copy the main renderer to the mask
            (with-port 
              (gs:get-wmgr-port)
              ; gWorld
              
              (#_lockPixels gPix)
              (if fastMask
                (with-pointers ((gP gPix)
                                (bp bMap))
                  (#_CalcCMask gp bp aRect aRect backColor (%null-ptr) 0))
                ;; slow mask
                (slowStripCopy gworld bMap aRect backColor))
              ;  (cl-user::copy-bits gPix bMap aRect aRect)
              (#_unlockPixels gPix)
              ;; copy the mask to the mask renderer
              (#_lockPixels gPix)
              (#_RGBbackColor *white-rgb*)
              (#_RGBForeColor *black-rgb*)
              (gs:copy-bits bMap gPix aRect mRect #$Patbic)
              (#_unlockPixels gPix)
              ;; feather
              (when feather
                (unless (numberp feather)
                  (setq feather 1))
                ;; get the region
                (gs:let+ ((trgn (:region))
                          (trgn2 (:region))
                          (step (round 65535 (+ 1 feather))))
                  ;;(cl-user::copy-bits bMap bMap aRect aRect #$NotPatCopy)
                  (#_BitmapToRegion trgn bMap)
                  (with-dereferenced-handles ((trgn-ptr trgn))
                    (#_RectRgn trgn2 (pref trgn-ptr region.rgnbbox)))
                  (#_XorRgn trgn2 trgn trgn)
                  (#_PenSize 1 1)
                  (#_PenMode #$PatCopy)
                  (#_OffsetRGN trgn (pref mRect :rect.left) (pref mRect :rect.top))
                  ;; loop for the feather radius
                  (gs::with-gWorld
                    (gWorld)
                    (#_lockPixels gPix)
                    (doTimes (i feather)
                      ;; get the next gray in line
                      (rset backColor :rgbColor.red (* step (- feather i)))
                      (rset backColor :rgbColor.green (* step (- feather i)))
                      (rset backColor :rgbColor.blue (* step (- feather i)))
                      ;; set the color
                      (#_RGBForeColor backColor)
                      ;; frame the region
                      (#_FrameRgn trgn)
                      ;; inset
                      (#_InsetRgn trgn 1 1))
                    ;; clean up
                    (#_unlockPixels gPix))
                  ))
              )
            ;; dispose extra storage
            (#_DisposePtr bMap)))))
    ;; clean up
    ))


(define-handler localPixelColor (LoneRangerRenderer xLoc yLoc)
  (let* ((gWorld (paintgWorld me))
         (gPix (gs:gWorld-pixmap gWorld))
         redPart greenPart bluePart)
    (%stack-block ((theColor 6))
      (with-port gWorld
        (#_LockPixels gPix)
        (#_GetCPixel (round xLoc) (round yLoc) theColor)
        (#_UnlockPixels gPix))
      (setq redPart (%get-word theColor 0)
            greenPart (%get-word theColor 2) 
            bluePart (%get-word theColor 4)))
    (sk8-multivals redPart greenPart bluePart)))


(define-handler (setf autoMaskFeather) (newValue LoneRangerRenderer)
  (SetValue 'autoMaskFeather me newValue)
  (recomputeMask me))


(define-handler (setf quickMask) (newValue LoneRangerRenderer)
  (SetValue 'quickMask me newValue)
  (allocateDataStructures me))

(define-handler (setf imageSize) (newValue LoneRangerRenderer)
  (SetValue 'imageSize me newValue)
  (allocateDataStructures me))

(define-handler (setf mainRenderer) (newValue LoneRangerRenderer)
  (SetValue 'mainRenderer me newValue)
  (recomputeMask me))

(define-handler (setf maskRenderer) (newValue LoneRangerRenderer)
  (SetValue 'maskRenderer me newValue)
  (recomputeMask me))

(define-handler (setf autoMask) (newValue LoneRangerRenderer)
  (SetValue 'autoMask me newValue)
  (recomputeMask me))

(define-handler (setf autoMaskColor) (newValue LoneRangerRenderer)
  (let (colorRed colorGreen colorBlue)
    (if (inheritsFrom newValue RGBColor)
      (setq colorRed (foreRed newValue)
            colorGreen (foreGreen newValue)
            colorBlue (foreBlue newValue))
      (if (listp newValue)
        (setq colorRed (car newValue)
              colorGreen (cadr newValue)
              colorBlue (caddr newValue))))
    (if colorRed
      (progn
        (SetValue 'autoMaskColor me (list colorRed colorGreen colorBlue))
        (recomputeMask me)))))


(define-handler paintgWorld (LoneRangerRenderer &key (autoRecompute t))
  (let ((thegWorld (getValue 'paintgWorld me)))
    (when (and autoRecompute (null thegWorld))
      (allocateDataStructures me)
      (setq thegWorld (getValue 'paintgWorld me)))
    ;; return it
    thegWorld))

#|
	Change History (most recent last):
	1  	11/ 9/95	sidney  	initial checkin
	2  	 2/27/97	Hernan  	
|# ;(do not edit past this line!!)
