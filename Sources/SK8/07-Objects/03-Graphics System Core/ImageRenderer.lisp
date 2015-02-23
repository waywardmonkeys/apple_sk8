;;; SK8 Built-in Renderers

;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories



(in-package :SK8Development)


#| Modification History

01-22-93 ruben id -> objectName
07-29-92 ice   changed to use SK8color object.s
07-26-92 ruben changed to SK8 package
06-13-92 ruben removed :PICT option from all PICT renderers
05-12-92 ruben d29 conversion
09-13-91 adam  piglets/rlets --> LET+
08-21-91 ruben cleaned-up renderers, added piglets

|#

(provide "IMAGERENDERER")

;;; ____________
;;; PixelMap Media!
;;; ____________

(new media :objectName "PixelMap"
     :prototype t
     :project sk8)

;;; This property gets set to t when capturePicture puts something new onto the
;;; pixel map. The store should save it at the right time.

(addProperty PixelMap 'needsSaving :initialvalue nil)

;;; restore-pixmap-data -- loads a pixelmap object's internal data
;;;
;;; We get the pixmap from the resource as a pict.
;;; With that info we make the gWorld, draw the pict in it.

(defun restore-pixmap-data (obj)
  (let ((myfile (sk8::file obj))
        (myresid (resourceId obj))
        (resourcetype "PICT")
        theGworld)
    ;; Only get stuff back if there is a resource id.
    (when (and (fileExists myfile) myresid)
      (withResourceFile 
       (myfile)
       (let ((pictH (T_Get1Resource resourcetype myresid)))
         (when pictH
           ;; The pict has to be locked because otherwise it will move
           ;; in the middle of this routine and our rect will be pointing
           ;; at empty space.
           (#_hLock pictH)
           (let ((gworld-rect (gs:hrref pictH :picture.picFrame :storage :pointer)))
             (setf theGWorld (T_newGWorldGC gworld-rect))
             (when theGWorld
               (gs:with-offscreen-Gworld
                (theGWorld)
                (#_DrawPicture pictH gworld-rect))))
           (#_hUnlock pictH)
           (#_ReleaseResource pictH)))))
    theGWorld))

(define-handler loadMedia (pixelMap)
  (let ((theHandle (mediaData me)))
    (if (and (macptrp theHandle) (not (ccl::%null-ptr-p theHandle)))
      theHandle
      (if (file me)
        (setf (mediaData me) (restore-pixmap-data me))
        (report-file-missing-error me)))))

(define-handler resType (pixelMap)
  "PICT")

(define-handler unloadMedia (pixelMap)
  (let ((theGWorld (mediaData me)))
    (when (and (macptrp theGWorld) (not (ccl::%null-ptr-p theGWorld)))
      (t_deactivategcosptr theGWorld :disposeNow t))
    (setf (mediadata me) nil)))
      
(define-handler size (pixelMap)
  (let* ((theGWorld (loadMedia me))
         (theRect (when (macptrp theGWorld) (gs:gWorld-rectangle theGWorld))))
    (if (pointerp theRect)
      (sk8-multivals (- (rref theRect :rect.right) (rref theRect :rect.left))
                     (- (rref theRect :rect.bottom) (rref theRect :rect.top)))
      (sk8-multivals 0 0))))

;;; me is the pixelMap. New version just makes a gWorld and puts it in the slot.

(defun initialize-snapShot-pixmap (me otherActor &key width height)
  (unless (and width height)
    (sk8-multival-setf (width height) (size otherActor :physical t)))
  (unloadMedia me)
  (let ((theGWorld))
    (rlet ((r :rect :topleft 0 :right (max 1 (gs:f.round width)) :bottom (max 1 (gs:f.round height))))
      (setf theGWorld (T_newGWorldGC r)))
    (when theGWorld
      (setf (mediaData me) theGWorld)
      t)))

(defun revalidate-snapshot-pixmap-internal (snapShot otherActor backGroundRenderer &key size)
  (gs:let+ ((isAnActor? (inheritsFrom otherActor Actor))
             (qd-rect (:rect :sk8Rect (if isAnActor?
                                        (gs:recompute-physicalBoundsRect otherActor)
                                        (gs:make-rect :left 0 :top 0 :right (car size) :bottom (cadr size)))))
             (draw-region (:region))
             (theRect (:rect))
             flags
             (thePort))
    ;; Ok: draw the thing!
    (when (initialize-snapShot-pixmap snapShot otherActor :width (car size) :height (cadr size))
      (setf thePort (mediaData snapShot))
      (let ((thePixmap (#_GetGworldPixmap thePort)))
        (with-locked-pixels-force (thePixmap)
          (with-port thePort
            (#_setOrigin (rref qd-rect :rect.left) (rref qd-rect :rect.top))
            ;; This line makes it work!
            (#_SetRect theRect -3000 -3000 3000 3000)
            (#_ClipRect theRect)
            (#_rectRgn (rref thePort :cgrafport.visrgn) theRect)
            ;; Paint gray behind the actor!
            (#_rectRgn draw-region qd-rect)
            (when backgroundRenderer
              (render backgroundRenderer nil draw-region thePort))
            (if (inheritsFrom otherActor renderer)
              (gs:with-clipped-region draw-region
                (render otherActor nil draw-region thePort))
              (progn
                ;; Now draw the actor! Make all the regions dirty so that they
                ;; get recomputed!!!
                (setf flags (gs:node-flags otherActor))
                (gs:recompute-Bounds-Region otherActor flags)
                (gs:recompute-fill-Region otherActor flags)
                (gs:recompute-frame-Region otherActor flags)
                (#_copyRgn (gs:node-boundsRegion otherActor) draw-region)
                (gs:dirty-subnodes* otherActor)
                ;; It is important to switch off fast-draw before drawing because we want to draw
                ;; EVERYTHING since the pixmap is empty to start with.
                (let ((gs:*fast-draw* nil))
                  (declare (special gs:*fast-draw*))
                  (gs:with-clipped-region draw-region
                    (funcall (gs:node-drawFunction otherActor) otherActor thePort draw-region)))
                )))))
      t)))

;;; If the actor is on that stage or is contained by something else, we call the function above
;;; to do the work. Otherwise we have to make the actor be in the right state (the physical boundsRect's corners
;;; should be 0,0 or bad things happen). Thus we move the actor to (0,0), capture and then restore
;;; the original position. This is a pain. This problem is caused by our clever hack to save the window's location
;;; in the physical rect when removing the actor from the stage.

(defun revalidate-snapShot-pixmap (snapShot otherActor backgroundRenderer &key size)
  (if (and (inheritsFrom otherActor Actor)
           (not (gs:node-container otherActor))
           (not (gs:hasWindow? (gs:node-flags otherActor))))
    (sk8-multival-bind (ll tt rr bb) (boundsRect otherActor :physical t)
      (let ((h (- rr ll))
            (v (- bb tt)))
        (unwind-protect 
          (progn
            (setBoundsRect otherActor 0 0 h v :physical t)
            (revalidate-snapshot-pixmap-internal snapshot otherActor backGroundRenderer :size size))
          (setBoundsRect otherActor ll tt rr bb :physical t))))
    (revalidate-snapshot-pixmap-internal snapshot otherActor backGroundRenderer :size size)))

;;; Should never be called without a rect!!!

(defun transfer-snapShot-pixmap (snapShot theport rect)
  (let* ((gWorld (loadMedia snapShot))
         (source-qd-rect (gs:gworld-rectangle gWorld)))
    ;; Copying: pixmap to pixmap.
    (#_ForeColor #$blackColor)
    (#_BackColor #$whiteColor)
    (gs:lock-gWorld gWorld)
    (gs:copy-bits (#_getGWorldPixmap gWorld)
                        (#_GetGWorldPixMap theport) 
                        source-qd-rect rect)
    (gs:unlock-gWorld gWorld)))

;;; Copies the bits in sourceSnapShots's pixmap to destSnapShot scaling as required. Need to install the pixmaps in two 
;;; ports to make this work...

(defun pixmap-to-pixmap (sourceSnapShot destSnapShot)
  (gs:gWorld-to-gWorld (loadMedia sourceSnapShot) (loadMedia destSnapShot)))

;;;______________________________________________________________________
;;; ImageRenderer
;;;______________________________________________________________________

;;; This is the parent of all the renderers that have media! Each of these also can stretch,
;;; tile and offset its bits.

(new Renderer 
     :objectName "ImageRenderer"
     :undisposable t
     :prototype t
     :properties `((media :value nil)
                   (hoffset :value 0)
                   (vOffset :value 0)
                   (renderStyle :value renderStretched)
                   (backgroundRenderer :value ,white)
                   (translucentColor :value nil))
     :project sk8)

(define-handler (setf translucentColor) (newValue ImageRenderer)
  ;; Error checking.
  (when (and newValue (not (inheritsFrom newValue RGBColor)))
    (sk8-error PropertyTypeMismatchError
               :object        newValue
               :expectedType  RGBColor
               :ownerObject   ImageRenderer
               :propertyName 'translucentColor
               ))
  ;; Set the slot.
  (setValue 'translucentColor me newValue)
  newValue)

;;; An image renderer is translucent if it uses its translucent color to 
;;; produce blue screening effects.

(define-handler translucent (imageRenderer)
  (let ((backRenderer (backgroundRenderer me)))
    (if (or (translucentColor me)
            (and backRenderer (translucent backRenderer)))
      t
      nil)))

;;; Error checking: you can only set the renderStyle to one of our three values.

(define-handler (setf renderStyle) (newValue ImageRenderer)
  (unless (memq newValue '(renderStretched renderTiled renderUnstretched))
    (sk8-error PropertyTypeMismatchError
               :object        newValue
               :expectedType  '(renderStretched renderTiled renderUnstretched)
               :ownerObject   ImageRenderer
               :propertyName 'renderStyle
               ))
  (setf (slot-value me 'renderStyle) newValue))

;;; Note that the values of the stretched and tiled properties can be infered by the 
;;; draw function being currently used.

;;; This macro is used by all the unstretched guys.

(defmacro qdrect-offset-and-fix-size (theRect hOffset vOffset hSize vSize)
  `(progn
     (rset ,theRect :rect.left (+ (rref ,theRect :rect.left) (gs:f.round ,hOffset)))
     (rset ,theRect :rect.top (+ (rref ,theRect :rect.top) (gs:f.round ,vOffset)))
     (rset ,theRect :rect.right (+ (rref ,theRect :rect.left) (gs:f.round ,hSize)))
     (rset ,theRect :rect.bottom (+ (rref ,theRect :rect.top) (gs:f.round ,vSize)))))

;;; ______________
;;; [1] ColorPatterns
;;; ______________

(define-handler defaultRenderStyle (colorPattern)
  'renderTiled)

(define-handler renderTiled (colorPattern theRenderer theActor region thePaper)
  (declare (ignore theActor theRenderer thePaper))
  (let (theHandle)
    (setf theHandle (loadMedia me))
    (if theHandle
      (progn (#_penpixpat theHandle)
             (#_paintRgn region)
             (#_penNormal))
      (render-an-error region))))

(defun colorpattern-to-port-to-region (thePixMap thePatRes theRegion background)
  ;; 1. remake gWorld to media size.
  (sk8-multival-bind (h v) (size thePatRes)
    (if (initialize-snapshot-pixmap thePixmap nil :width h :height v)
      ;; 2. Render media into it.
      (gs:let+ ((theGWorld (mediaData thePixmap))
             (theRect (:rect))
             (thePort (:pointer))
             (otherRgn (:region)))
        (#_getPort thePort)
        (#_SetRect theRect -3000 -3000 3000 3000)
        (with-locked-pixels-force ((#_GetGworldPixmap theGWorld))
          (with-port theGWorld
            (#_ClipRect theRect) 
            ;; render the background.
            (when background (render background nil theRegion theGWorld))
            ;; and the pattern.
            (#_rectRgn otherRgn (gs:gWorld-rectangle theGWorld))
            (#_penpixpat (loadMedia thePatRes))
            (#_paintRgn otherRgn)
            (#_penNormal)))
        ;; 3. Copy bits.
        (rlet ((r :rect))
          (gs:region-into-rect theRegion r)
          (transfer-snapshot-pixmap thePixmap (%get-ptr theport) r)))
      (render-an-error theRegion))))

;;; We need an extra pixel map to use when calling the function. We cannot use the one
;;; from the swatchTempSnapShotMedia because if we are running this during a redraw to
;;; fill in a cached pixmap, we crash horribly. 

(new pixelMap :objectname "patStretchPixelMap" :project sk8)

(define-handler renderStretched (colorPattern theRenderer theActor region thePaper)
  (declare (ignore theActor thePaper))
  (let ((theHandle (loadMedia me)))
    (if theHandle
         ;; fix it into a port, paint the pattern and copy bit it to our region.
      (colorpattern-to-port-to-region patStretchPixelMap me region 
                                      (when (translucent therenderer) (backgroundRenderer theRenderer)))
          (render-an-error region))))

(define-handler renderUnstretched (colorPattern theRenderer theActor region thePaper)
  (let (theHandle)
    (setf theHandle (loadMedia me))
    (if theHandle
      (gs:let+ ((renderRgn (:region))
             (qd-rect (:rect))
             (background (backgroundRenderer theRenderer)))
        (sk8-multival-bind (h v) (size me)
          (rlet ((r :rect))
            (gs:region-into-rect region r)
            (copy-record r :rect qd-rect))
          (qdrect-offset-and-fix-size qd-rect (hoffset theRenderer) (vOffset theRenderer) h v)
          (#_rectRgn renderRgn qd-rect)
          ;; If there is a background, render it.
          (render (or background White) theActor region thePaper)
          ;; Render the pattern!
          (#_penpixpat theHandle)
          (#_paintRgn renderRgn)
          (#_penNormal)))
      (render-an-error region))))

;;; ______________
;;; [2] BWPatterns
;;; ______________

(define-handler defaultRenderStyle (BWPattern)
  'renderTiled)

(define-handler renderTiled (BWPattern theRenderer theActor region thePaper)
  (declare (ignore theActor theRenderer thePaper))
  (let (theHandle)
    (setf theHandle (loadMedia me))
    (if theHandle
      (progn (with-pointers ((pntr thehandle))
               (#_PenPat pntr))
             (#_ForeColor #$blackColor)
             (#_BackColor #$whiteColor)
             (#_paintRgn region)
             (#_penNormal))
      (render-an-error region))))

(defun pattern-to-port-to-region (thePixMap thePatRes theRegion background)
  ;; 1. remake gWorld to media size.
  (sk8-multival-bind (h v) (size thePatRes)
    (if (initialize-snapshot-pixmap thePixmap nil :width h :height v)
      ;; 2. Render media into it.
      (gs:let+ ((theGWorld (mediaData thePixmap))
             (theRect (:rect))
             (thePort (:pointer))
             (otherRgn (:region)))
        (#_getPort thePort)
        (#_SetRect theRect -3000 -3000 3000 3000)
        (with-locked-pixels-force ((#_GetGworldPixmap theGWorld))
          (with-port theGWorld
            (#_ClipRect theRect) 
            ;; render the background.
            (when background
              (render background nil theRegion theGWorld))
            ;; and the pattern.
            (#_rectRgn otherRgn (gs:gWorld-rectangle theGWorld))
            (with-pointers ((pntr (loadMedia thePatRes)))
              (#_PenPat pntr))
            (#_paintRgn otherRgn)
            (#_penNormal)))
        ;; 3. Copy bits.
        (rlet ((r :rect))
          (gs:region-into-rect theRegion r)
          (transfer-snapshot-pixmap thePixmap (%get-ptr theport) r)))
      (render-an-error theRegion))))

(define-handler renderStretched (BWPattern theRenderer theActor region thePaper)
  (declare (ignore theActor thePaper))
  (let ((theHandle (loadMedia me)))
    (if theHandle
      ;; fix it into a port, paint the pattern and copy bit it to our region.
      (pattern-to-port-to-region patStretchPixelMap me region 
                                 (when (translucent therenderer) (backgroundRenderer theRenderer)))
      (render-an-error region))))

;;; BWPatterns are always 8x8 pixels. Make the small region, move it to the right place and
;;; render the pattern there.

(define-handler renderUnstretched (BWPattern theRenderer theActor region thePaper)
  (let (theHandle)
    (setf theHandle (loadMedia me))
    (if theHandle
      (gs:let+ ((renderRgn (:region))
             (qd-rect (:rect))
             (background (backgroundRenderer theRenderer)))
        (rlet ((r :rect))
          (gs:region-into-rect region r)
          (copy-record r :rect qd-rect))
        (qdrect-offset-and-fix-size qd-rect (hoffset theRenderer) (vOffset theRenderer) 8 8)
        (#_rectRgn renderRgn qd-rect)
        ;; If there is a background, render it.
        (render (or background White) theActor region thePaper)
        ;; Render the pattern!
        (with-pointers ((pntr thehandle))
          (#_PenPat pntr))
        (#_ForeColor #$blackColor)
        (#_BackColor #$whiteColor)
        (#_paintRgn renderRgn)
        (#_penNormal))
      (render-an-error region))))

;;; ______________
;;; [3] Cicns
;;; ______________

(define-handler defaultRenderStyle (IconRSRC)
  'renderStretched)

(define-handler renderStretched (IconRSRC theRenderer theActor region thePaper)
  ;; Render the icon!
  (gs:let+ ((handle (loadMedia me))
         (back (backgroundRenderer theRenderer))
         (tempRect (:rect)))
    (rlet ((r :rect))
      (gs:region-into-rect region r)
      (copy-record r :rect tempRect))
    (if (handlep handle)
      (gs:with-composed-clip region
        (when back (render back theActor region thePaper))
        (#_PlotCIcon tempRect handle))
      (render-an-error region))))

(define-handler renderTiled (IconRSRC theRenderer theActor region thePaper)
  (gs:let+ ((pict-box (:rect))
         (handle (loadMedia me))
         (hPhase (hOffset theRenderer))
         (vPhase (vOffset theRenderer))
         pxSize pySize rgn-phase cx cy ix iy)
    ;; If there is a background renderer, render it.
    (when (backgroundRenderer theRenderer)
      (render (backgroundRenderer theRenderer) theActor region thePaper))
    (sk8-multival-setf (pxSize pySize) (size me))
    (cond ((handlep handle)
           (rlet ((rRect :rect))
             (gs:region-into-rect region rRect)
             (setq cx (gs:int-to-fixed (rref rRect :rect.left))
                   cy (gs:int-to-fixed (rref rRect :rect.top))
                   ix (gs:f.trunc (gs:f.ceiling (gs:f- (gs:int-to-fixed (rref rRect :rect.right)) cx) pxSize))
                   iy (gs:f.trunc (gs:f.ceiling (gs:f- (gs:int-to-fixed (rref rRect :rect.bottom)) cy) pySize))))
           ;; Fixing hOffset.
           (unless (zerop hPhase)
             (setq rgn-phase (gs:fmod cx pxSize))
             (unless (eq rgn-phase hPhase)
               (gs:fdecf rgn-phase hPhase)
               (gs:fdecf cx rgn-phase)
               (incf ix)))
           ;; Fixing vOffset.
           (unless (zerop vPhase)
             (setq rgn-phase (gs:fmod cy pySize))
             (unless (eq rgn-phase vPhase)
               (gs:fdecf rgn-phase vPhase)
               (gs:fdecf cy rgn-phase)
               (incf iy)))
           ;; Rounding off things.
           (setq pxSize (gs:f.round pxSize)
                 pySize (gs:f.round pySize)
                 cx (gs:f.round cx)
                 cy (gs:f.round cy))
           ;; Drawing!
           (gs:with-composed-clip region
             (dotimes (yinc iy)
               (dotimes (xinc ix)
                 (#_SetRect pict-box 
                  (+ cx (* pxSize xInc)) (+ cy (* pySize yInc))
                  (+ cx (* pxSize (1+ xInc))) (+ cy (* pySize (1+ yInc))))
                 (#_PlotCIcon pict-box handle)))))
          (t (render-an-error region)))))

(define-handler renderUnstretched (IconRSRC theRenderer theActor region thePaper)
  ;; Render the icon!
  (gs:let+ ((handle (loadMedia me))
         (back (backgroundRenderer theRenderer))
         (tempRect (:rect)))
    (if (handlep handle)
      (sk8-multival-bind (h v) (size me)
        (render (or back White) theActor region thePaper)
        (rlet ((r :rect))
          (gs:region-into-rect region r)
          (copy-record r :rect tempRect))
        (qdrect-offset-and-fix-size tempRect (hoffset theRenderer) (vOffset theRenderer) h v)
        (gs:with-composed-clip region
          (#_PlotCIcon tempRect handle)))
      (render-an-error region))))

;;; ______________
;;; [4] PixelMaps
;;; ______________

(define-handler defaultRenderStyle (PixelMap)
  'renderStretched)

(define-handler renderStretched (PixelMap theRenderer theActor region thePaper)
  (declare (ignore theRenderer thePaper))
  ;; Render the pixmap!
  (if (pointerp (loadMedia me))
    (gs:let+ ((thePort (:pointer)))
      (if (pointerp theActor)
        (error "You are using a pointer where you should be using an Actor.")
        ;; theRect must be holding an actor...
        (rlet ((r :rect))
          (gs:region-into-rect region r)
          (#_getPort thePort)
          (gs:with-composed-clip region
            (transfer-snapshot-pixmap me (%get-ptr thePort) r)))))
    (render-an-error region)))

(define-handler renderTiled (PixelMap theRenderer theActor region thePaper)
  (declare (ignore theActor thePaper))
  (gs:let+ ((pict-box (:rect))
         (thePort (:pointer))
         (hPhase (hOffset theRenderer))
         (vPhase (vOffset theRenderer))
         pxSize pySize rgn-phase cx cy ix iy)
    (cond ((pointerp (loadMedia me))
           (sk8-multival-setf (pxSize pySize) (size me))
           (rlet ((rRect :rect))
             (gs:region-into-rect region rRect)
             (setq cx (gs:int-to-fixed (rref rRect :rect.left))
                   cy (gs:int-to-fixed (rref rRect :rect.top))
                   ix (gs:f.trunc (gs:f.ceiling (gs:f- (gs:int-to-fixed (rref rRect :rect.right)) cx) pxSize))
                   iy (gs:f.trunc (gs:f.ceiling (gs:f- (gs:int-to-fixed (rref rRect :rect.bottom)) cy) pySize))))
           ;; Fixing hOffset.
           (unless (zerop hPhase)
             (setq rgn-phase (gs:fmod cx pxSize))
             (unless (eq rgn-phase hPhase)
               (gs:fdecf rgn-phase hPhase)
               (gs:fdecf cx rgn-phase)
               (incf ix)))
           ;; Fixing vOffset.
           (unless (zerop vPhase)
             (setq rgn-phase (gs:fmod cy pySize))
             (unless (eq rgn-phase vPhase)
               (gs:fdecf rgn-phase vPhase)
               (gs:fdecf cy rgn-phase)
               (incf iy)))
           ;; Rounding off things.
           (setq pxSize (gs:f.round pxSize)
                 pySize (gs:f.round pySize)
                 cx (gs:f.round cx)
                 cy (gs:f.round cy))
           (#_getPort thePort)
           ;; Drawing!
           (gs:with-composed-clip region
             (dotimes (yinc iy)
               (dotimes (xinc ix)
                 (#_SetRect pict-box 
                  (+ cx (* pxSize xInc)) (+ cy (* pySize yInc))
                  (+ cx (* pxSize (1+ xInc))) (+ cy (* pySize (1+ yInc))))
                 (transfer-snapshot-pixmap me (%get-ptr thePort) pict-box)))))
          (t (render-an-error region)))))

(define-handler renderUnstretched (PixelMap theRenderer theActor region thePaper)
  ;; Render the Pixmap.
  (gs:let+ ((back (backgroundRenderer theRenderer))
         (thePort (:pointer))
         (tempRect (:rect)))
    (if (pointerp (loadMedia me))
      (sk8-multival-bind (h v) (size me)
        (render (or back White) theActor region thePaper)
        (rlet ((r :rect))
          (gs:region-into-rect region r)
          (copy-record r :rect tempRect))
        (qdrect-offset-and-fix-size tempRect (hoffset theRenderer) (vOffset theRenderer) h v)
        (#_getPort thePort)
        (transfer-snapshot-pixmap me (%get-ptr thePort) tempRect))
      (render-an-error region))))

;;; width, height or h or v could be 0... Make sure this case is covered. This deals with the case
;;; when we have to cache the picture of a vertical or horizontal line.

(defun adjust-size-to-multiples (otherActor width height)
  (when (zerop width) (setf width 2))
  (when (zerop height) (setf height 2))
  (if (inheritsFrom otherActor actor)
    (sk8-multival-bind (h v) (size otherActor :physical t)
      (when (zerop h) (setf h 2))
      (when (zerop v) (setf v 2))
      (let ((hMultiple (ceiling h width))
            (vMultiple (ceiling v height)))
        (when (and (oddp hMultiple) (neq hMultiple 1)) (incf hMultiple))
        (when (and (oddp vMultiple) (neq vMultiple 1)) (incf vMultiple))
        (sk8-multivals (gs:f.round (/ h hMultiple)) (gs:f.round (/ v vMultiple)))))
    (sk8-multivals (gs:f.round width) (gs:f.round height))))

;;; CapturePicture -- draws the otherActor into the pixmap... The size optional arg. is the size at which the 
;;;               pixmap is to be cached. SwatchTempSnapShotColor is used to draw the original actor. Then it
;;;               is blitted onto the new color whose pixmap is set to the right size!

(define-handler capturePicture (pixelMap otherActor &key size (backgroundRenderer white))
  (if (and size (inheritsFrom otherActor Actor))
    (without-interrupts ; Vulnerable temp pixmap
     ;; [1] Draw the actor into the temp pixmap.
     (revalidate-snapshot-pixmap (media SwatchTempSnapShotColor) otherActor backgroundRenderer)
     ;; [2] Initialize the new pixmap to the right size.
     (sk8-multival-bind (goodWidth goodHeight) (adjust-size-to-multiples otherActor (car size) (cadr size))
       (initialize-snapshot-pixmap me nil :width goodWidth :height goodHeight))
     ;; [3] Blit into it.
     (pixmap-to-pixmap (media swatchTempSnapShotColor) me))
    (revalidate-snapshot-pixmap me otherActor backgroundRenderer :size size))
  ;; And save the picture into the project file. 
  ;; (setf (resourceId me) (save-pixmap-resource me))
  (setf (needsSaving me) t)
  t)

(define-handler capturePicture (imageRenderer otherActor &key size)
  ;; Make sure the renderer has the right media! Note that an media object might be
  ;; left dangling after this...
  (unless (inheritsFrom (media me) PixelMap)
    (setf (media me) (new pixelMap :project (project me))))
  ;; Call capturePicture of pixelMap to do the work.
  (capturePicture (media me) otherActor :size size :backgroundRenderer (backgroundRenderer me)))

;;; ______________
;;; [5] PICTs
;;; ______________

;;; And now the methods.

(define-handler defaultRenderStyle (QDPicture)
  'renderStretched)

(define-handler renderStretched (QDPicture theRenderer theActor region thePaper)
  (gs:let+ ((handle (loadMedia me))
         (hPhase (hOffset theRenderer))
         (vPhase (vOffset theRenderer))
         (background (backgroundRenderer theRenderer))
         (pict-box (:rect)))
    (cond ((handlep handle)
           (gs:with-composed-clip region
             (#_hLock handle)
             (when background
               (render background theActor region thePaper))
             (rlet ((r :rect))
               (gs:region-into-rect region r)
               (copy-record r :rect pict-box))
             (#_OffsetRect pict-box (gs:f.round hPhase) (gs:f.round vPhase))
             (#_DrawPicture handle pict-box)
             (#_hUnlock handle)))
          (t (render-an-error region)))))

(define-handler renderTiled (QDPicture theRenderer theActor region thePaper)
  (gs:let+ ((pict-box (:rect))
         (handle (loadMedia me))
         (hPhase (hOffset theRenderer))
         (vPhase (vOffset theRenderer))
         pxSize pySize pRect rgn-phase cx cy ix iy)
    ;; If there is a background renderer, render it.
    (when (backgroundRenderer theRenderer)
      (render (backgroundRenderer theRenderer) theActor region thePaper))
    (cond ((handlep handle)
           (setq pRect (gs:hrref handle :picture.picframe :storage :pointer)
                 pxSize (gs:f- (gs:int-to-fixed (rref pRect :rect.right)) (gs:int-to-fixed (rref pRect :rect.left)))
                 pySize (gs:f- (gs:int-to-fixed (rref pRect :rect.bottom)) (gs:int-to-fixed (rref pRect :rect.top))))
           (rlet ((rRect :rect))
             (gs:region-into-rect region rRect)
             (setf cx (gs:int-to-fixed (rref rRect :rect.left))
                   cy (gs:int-to-fixed (rref rRect :rect.top))
                   ix (gs:f.trunc (gs:f.ceiling (gs:f- (gs:int-to-fixed (rref rRect :rect.right)) cx) pxSize))
                   iy (gs:f.trunc (gs:f.ceiling (gs:f- (gs:int-to-fixed (rref rRect :rect.bottom)) cy) pySize))))
           ;; Fixing hOffset.
           (unless (zerop hPhase)
             (setq rgn-phase (gs:fmod cx pxSize))
             (unless (eq rgn-phase hPhase)
               (gs:fdecf rgn-phase hPhase)
               (gs:fdecf cx rgn-phase)
               (incf ix)))
           ;; Fixing vOffset.
           (unless (zerop vPhase)
             (setq rgn-phase (gs:fmod cy pySize))
             (unless (eq rgn-phase vPhase)
               (gs:fdecf rgn-phase vPhase)
               (gs:fdecf cy rgn-phase)
               (incf iy)))
           ;; Rounding off things.
           (setq pxSize (gs:f.round pxSize)
                 pySize (gs:f.round pySize)
                 cx (gs:f.round cx)
                 cy (gs:f.round cy))
           ;; Clip to the draw region.
           (gs:with-composed-clip region
             ;; Drawing!
             (dotimes (yinc iy)
               (dotimes (xinc ix)
                 (#_SetRect pict-box 
                  (+ cx (* pxSize xInc))
                  (+ cy (* pySize yInc))
                  (+ cx (* pxSize (1+ xInc)))
                  (+ cy (* pySize (1+ yInc))))
                 (#_DrawPicture handle pict-box)))))
          (t (render-an-error region)))))

(define-handler renderUnstretched (QDPicture theRenderer theActor region thePaper)
  (gs:let+ ((theHandle (loadMedia me))
         (back (backgroundRenderer theRenderer))
         (tempRect (:rect)))
    (if (handlep theHandle)
      (sk8-multival-bind (h v) (size me)
        (render (or back White) theActor region thePaper)
        (rlet ((r :rect))
          (gs:region-into-rect region r)
          (copy-record r :rect tempRect))
        (qdrect-offset-and-fix-size tempRect (hoffset theRenderer) (vOffset theRenderer) h v)
        (gs:with-clipped-region region
          (#_drawPicture theHandle tempRect)))
      (render-an-error region))))

;;; _______________________________ 
;;; TRANSLUCENCY!!!
;;; _______________________________ 

(new pixelMap :objectname "translucencyHelper" :project sk8)

;;; Returns false if everything is ok. A width and a height otherwise.

(defun gWorld-resize-required (gWorld region)
  (let* ((region-rect (gs:hrref region :region.rgnbbox :storage :pointer))
         (region-width (- (rref region-rect :rect.right) (rref region-rect :rect.left)))
         (region-height (- (rref region-rect :rect.bottom) (rref region-rect :rect.top))))
    (if gWorld
      (let* ((gWorld-rect (gs:gWorld-rectangle gWorld))
             (gWorld-width (- (rref gworld-rect :rect.right) (rref gworld-rect :rect.left)))
             (gWorld-height (- (rref gworld-rect :rect.bottom) (rref gworld-rect :rect.top))))
        (unless (and (<= region-width gWorld-width) (<= region-height gWorld-height))
          (values region-width region-height)))
      (values region-width region-height))))

(defmacro with-extra-gWorld-step ((theColor theRegion) &body body)
  (let* ((theGWorld (gensym))
         (region-rect (gensym))
         (thePortPtr (gensym))
         (thePort (gensym)))
    `(gs:let+ ((,theGWorld (mediaData translucencyHelper))
            (,region-rect (:rect))
            (,thePortPtr (:pointer))
            (,thePort (:pointer)))
       ;; 0. Safely get the region's rect
       (with-dereferenced-handles ((rgn ,theRegion))
         (copy-record (rref rgn :region.rgnbbox :storage :pointer) :rect ,region-rect))
       ;; 1. Size gWorld to size of region (only if gWorld is too small).
       (multiple-value-bind (w h) (gWorld-resize-required ,thegWorld ,theRegion)
         (when w
           (initialize-snapshot-pixmap translucencyHelper nil :width w :height h)
           (setq ,theGWorld (mediaData translucencyHelper))))
       ;; 2. Set the port to it.
       (gs:with-offScreen-gWorld (,theGWorld)
         (require-trap #_setOrigin (rref ,region-rect :rect.left) (rref ,region-rect :rect.top))
         ;; 3. Do the rendering.
         ,@body)
       ;; 4. Copy bits to the other port using transparent and the new color as the background.
       (require-trap #_getPort ,thePortPtr)
       (%setf-macptr ,thePort (%get-ptr ,thePortPtr))
       ;; Use current port to draw into it.
       (require-trap #_penNormal)
       (with-rgb (rgb (mcl-color ,theColor))
         (require-trap #_rgbBackColor rgb))
       (unwind-protect 
         (progn 
           (gs:lock-gWorld ,theGWorld)
           (gs:copy-bits (gs:gWorld-pixmap ,theGWorld)
                               (gs:gWorld-pixmap ,thePort)
                               ,region-rect
                               ,region-rect
                               #$transparent
                               ,theRegion))
         (gs:unlock-gWorld ,theGWorld))
       (require-trap #_backColor #$whiteColor)
       (require-trap #_penNormal))))

;;; _______________________________ 
;;; Now the handlers that make it all work...
;;; _______________________________ 

(define-handler (setf hOffset) (newOffset imageRenderer)
  (if (integerp newOffset)
    (setf (slot-value me 'hoffset) (gs:f.round newOffset))
    (sk8-error PropertyTypeMismatchError
               :object        newOffset
               :expectedType  Integer
               :ownerObject   ImageRenderer
               :propertyName 'hOffset
               )))

(define-handler (setf vOffset) (newOffset imageRenderer)
  (if (integerp newOffset)
    (setf (slot-value me 'voffset) (gs:f.round newOffset))
    (sk8-error PropertyTypeMismatchError
               :object        newOffset
               :expectedType  Integer
               :ownerObject   ImageRenderer
               :propertyName 'vOffset
               )))

(define-handler (setf media) (newMedia imageRenderer)
  (let ((oldMedia (media me)))
    ;; Check that the new media is of the right type.
    (when (and newMedia
               (not (or (inheritsFrom newMedia PixelMap)
                        (inheritsFrom newMedia IconRSRC)
                        (inheritsFrom newMedia QDPicture)
                        (inheritsFrom newMedia BWPattern)
                        (inheritsFrom newMedia ColorPattern))))
      (sk8-error PropertyTypeMismatchError
                 :object        newMedia
                 :expectedType  (list PixelMap IconRSRC QDPicture BWPattern ColorPattern)
                 :ownerObject   ImageRenderer
                 :propertyName 'media
                 ))
    ;; Change the render style if required.
    (when (and newMedia (or (null oldMedia) (neq (baseParent oldMedia) (baseParent newMedia))))
      (setf (renderStyle me) (defaultRenderStyle newMedia)))
    (setf (slot-value me 'media) newMedia)
    newMedia))

;;; Render of ImageRenderer clips! No one else needs to (renderStretched, etc...)

(define-handler render (imageRenderer theActor region thePaper)
  (let ((theMedia (media me)))
    (if theMedia
      (if (and (translucent me) (translucentColor me))
        ;; You need to check that the translucent color is set to cover for cases
        ;; where the user redefines translucent...
        ;; This is the slow but cool blue screen effect.
        (with-extra-gWorld-step ((translucentColor me) region)
          (funcall (renderStyle me) theMedia me theActor region thePaper))
        ;; Normal, faster stuff.
        (funcall (renderStyle me) theMedia me theActor region thePaper))
      (renderAnError me theActor region))))

;;; Our very onw ERROR COLOR!!!

(new iconRSRC :project sk8 :objectname "ErrorColorRSRC" :resourceID 15134 :undisposable t)
(new imageRenderer :objectname "ErrorColor" :media ErrorColorRSRC :project sk8 :undisposable t)

;;; Used in render an error.

(defun render-icon-media (themedia region)
  (gs:let+ ((handle (loadMedia theMedia))
         (tempRect (:rect)))
    (rlet ((r :rect))
      (gs:region-into-rect region r)
      (copy-record r :rect tempRect))
    (if (handlep handle)
      (gs:with-composed-clip region
        (#_PlotCIcon tempRect handle))
      (real-render-an-error region))))

;;;____________________________________________________________________
;;;____________________________________________________________________
;;; The HELPER COLORS FOR SNAP SHOTS!!!
;;;____________________________________________________________________
;;;____________________________________________________________________

(new pixelMap :objectName "SwatchTempSnapShotMedia" :project sk8)
(new imageRenderer :objectName "SwatchTempSnapShotColor" :project sk8
     :media SwatchTempSnapShotMedia)

;;;____________________________________________________________________
;;;____________________________________________________________________
;;;                           Rendering strings!
;;;____________________________________________________________________
;;;____________________________________________________________________

;; This is the standard case, where we make a region out of the text and call the usual
;; render function

(define-handler render-string :private (Renderer theActor str x y theFont size style thePaper)
  ;; we can ignore the other info because it is already set in the current port!
  (gs:let+ ((region (:region))
         (rect (:rect))
         (bRect (gs:recompute-physicalBoundsRect theActor)))
    (#_SetRect rect (gs:rect-left bRect) (gs:rect-top bRect) (gs:rect-right bRect) (gs:rect-bottom bRect))
    (if (gs:string-region rect region str x y (fontData theFont) size style)
      (render me theActor region thePaper)
      ;; we failed!  Do the normal
      (render-string black theActor str x y theFont size style thePaper))))

;;____________________________________________________________________
;; RGBColor
;; This is the most basic of all - just move and draw.

(define-handler render-string :private (RGBColor theActor str x y theFont size style thePaper)
  (declare (ignore theActor thefont size style thePaper))
  ;; we can ignore the other info because it is already set in the current port!
  (gs:let+ ((color (:rgbColor :red (foreRed me)
                           :green (foreGreen me)
                           :blue (foreBlue me))))
    (#_RGBForeColor color)
    (#_MoveTo x y)
    (with-pstrs ((s str))
      (#_DrawString s))))

#|
	Change History (most recent last):
	8	10/1/93	hernan	=====================================
				= Sad but true: the node is gone!!! =
				=====================================
	9	10/13/93	hernan	The renderer hierarchy has been revisited!
	10	10/13/93	hernan	Added the complexPatternRenderer.
	11	10/13/93	hernan	(setf media) of pixelRenderer moved below the
				definition of pixelMap.
	12	10/13/93	hernan	Fixing the patterns and the gradients.
	13	10/15/93	hernan	ImageRenderers consolidated!
	14	10/15/93	hernan	Adding all options to imagerenderers.
	15	10/15/93	hernan	Adding the error color.
	16	10/15/93	hernan	Adding render options of colorPattern.
	17	10/15/93	hernan	Fixing the colorPattern render methods.
	18	10/15/93	hernan	Making sure the render-function got copied.
	19	10/18/93	hernan	Fixing render-pict-unstretched.
	20	10/20/93	hernan	Making a new default render function for image'
				renderers that just renders an error.
	21	10/22/93	hernan	Making render-pict-XXX render the background
				renderer if any.
	22	10/29/93	hernan	Making picts rendered with natural size be clipped
				to the draw region.
	23	11/3/93	kleiman	capturePicture without-interrupts added
	24	11/15/93	kleiman	PixelMap new properties and made to use nice
				relocatable handle instead of non-relocatable 
				pointer
	25	11/24/93	hernan	Making restore of pixelMap recompute the
				pixmap. We have changed restore of project to
				make sure that all pixelmaps get restored after
				everything else in their projects. We also have to
				add the actorSnapped property to it since restore
				will need it for recapture of the guy.
	26	11/24/93	hernan	Fixing restore of pixelmap to use the size option
				when the size cached is not the size of the actor
				cached.
	27	11/29/93	hernan	Using let+ to get the ports.
	28	11/29/93	hernan	Clipping the unstretched pict to the right region.
	29	12/2/93	hernan	revalidate-snapshot-pixmap makes sure all actors
				to draw are dirty! And it makes the actor being
				captured dirty too. You know what this means?!!
				Finally you are able to take snapshots without
				putting those actors on the stage!!!!!
	30	12/3/93	hernan	Removing print statements that were left over.
	31	12/21/93	sidney	Changes so files can be compiled
	32	12/22/93	rod	backRenderer->BackgroundRenderer
				Obsoleted ComplexPatternRenderer
				backgroundColor->BackgroundRenderer
	33	1/10/94	hernan	Fonts just became objects!!!
	34	1/11/94	hernan	self -> me
	35	1/14/94	hernan	Labeling properties and handlers private.
	36	2/21/94	hernan	The Final Renaming for Alpha! (yeah, right...)
	37	2/25/94	hernan	Using symbols instead of keywords for options!!!
	38	2/28/94	hernan	Now using GCable mac things.
	39	3/1/94	kleiman	handle some pointers correctly
	40	3/3/94	kleiman	private properties declared via makeprivateproperty
	41	3/11/94	Hernan	Making pixelMaps use gWorlds instead of pixmaps.
				This should simplify all these methods a lot!
	42	3/11/94	Hernan	Size method of pixelMap now really makes sure
				it has a gWorld before trying to look in it.
	43	3/14/94	kleiman	makePrivateProperty -> (setf (private ...
	44	3/16/94	Hernan	Making sure the the newGWorld trap only gets
				integers.
	45	3/17/94	Hernan	Forgot argument in call to error.
	46	3/24/94	Hernan	Fixing leftover keywords in complexGradient's
				render method.
	47	3/25/94	Hernan	Making render of the gradients cause an error
				when the direction is not valid.
	48	3/28/94	Hernan	Fixing revalidate-snapshot-pixmap to correctly
				capture when the actor to be capture has no 
				container and is not on the stage.
	49	3/28/94	rod	Error checking in pixel map stuff to make sure the
				size is always greater than 0.
	50	4/1/94	rod	Forcing disposePtr when it is possible.
	51	4/20/94	Hernan	Making (setf media) to accept both a QDPicture
				and a PICTFileMedia for rendering.
	52	4/20/94	Hernan	Replacing calls to get-record-field with calls to
				region-into-rect (which does not cons).
	53	4/25/94	Hernan	Allowing you to set the media of an imageRenderer
				to false. The render will subsequently render an
				error.
	54	4/29/94	Hernan	Forcing the size of snapshots to be multiples of
				the size of the actors they capture. This makes
				copybits works in a nicer way and minimizes the
				change of missing edges (although that still 
				happens).
	55	5/2/94	Hernan	Making the render functions of different media
				be methods. The renderStyle handler is the real
				property and is all we need to implement this. The
				options are the names of the new render methods.
	56	5/2/94	Hernan	Using renderAMediaError where appropriate.
	57	5/2/94	Hernan	Making the default renderStyle of the imageRenderer
				be renderStretched.
	58	5/3/94	Hernan	Making (setf media) use a default render style
				for each media type. This way by setting the 
				media, users can get things to render in the right
				style without extra work.
	59	5/4/94	Hernan	Mmmm. Found a good one. Had to create a new 
				pixelMap to be used by the methods that stretch
				patterns in the rendering. We could not just use
				the swatchTempRect guy because if we happen
				to stretch the pattern while capturing a picture, 
				the swatch guy is already in use!!! (This caused
				some delightfull crashes...)
	60	5/4/94	Hernan	Exporting our new helper pixelmap.
	61	5/24/94	rod	Fixing bug in render of image renderer.  Does
				faulty check for Media.  This is the culprit for
				a number of crashes.
	62	6/7/94	Hernan	Adding defaultRenderStyle of PICTFileMedia.
	63	6/10/94	Hernan	1166524: Set renderStyle now checks that the
				style provided is one of the valid ones.
	64	6/10/94	Hernan	Adding the infrastructure to implement blue
				screening for imageRenderers. Adding the
				translucentColor property and redefining
				translucent based on it.
	65	6/14/94	Hernan	Adding render methods for PICTFileMedia (which I forgot).
	66	6/23/94	sidney	pixelmap gets a new restype, make restypes global constants instead of a property
	67	7/5/94	sidney	the pixelmap restype constants aren't being used after all
	68	7/6/94	Hernan	1172074: adjust-size-to-multiples cannot return
				0 for either the width and size. We'll make the min
				be 2 for both.
	69	7/11/94	Hernan	Flattening the media hierarchy (no more resource
				and FileMedia).
	70	7/14/94	Hernan	1174501: render of ImageRenderer has to clip!
	71	7/15/94	Hernan	Fixing awful bug with render methods of pixelMap
				which dereferenced a nonsense pointer.
	72	7/22/94	Hernan	Adding in the translucency stuff.
	73	7/27/94	Hernan	1177128: render of imageRenderer should only
				do the extra gWorld step when the translucentColor is set.
	74	8/5/94	Hernan	1179002: set media checks the type of the media
				and produces a meaningful error message if the
				type is not right.
	75 	 8/12/94	Hernan  	1179906: tiled things need to clip to the region
							they get since they may overflow it.
	76 	 8/17/94	Hernan  	1174587: need to clip to the effective region
							before drawing a stretched thing.
	77 	 8/31/94	Hernan  	Coercing things that should be integers to integers.
	78 	 9/12/94	Hernan  	1180502: capitalizing object names.
	79 	 9/16/94	Hernan  	1186923: changing definition of translucent for
							imageRenderers to cover all cases. This is necessary
							to ensure no tla is rendered with a translucent
							renderer.
	80 	 9/28/94	Hernan  	1184837: revalidate-snapshot-pixmap-internal
							no longer forces a recompute of the regions of the
							actor.
	81 	10/ 3/94	Hernan  	handle -> mediaData. Also added paper as an argument to render.
	82 	10/24/94	Hernan  	Fixing set direction to take 'rect instead of 
							'rectangle.
	83 	11/15/94	Hernan  	Making capturePicture work when capturing
							renderers.
	84 	11/16/94	Hernan  	Fixing capturePicture problem.
	85 	11/18/94	Hernan  	Making pixelMap only cache the size on the data
							slot when the pixel map was caching something.
	86 	12/ 7/94	Hernan  	Replacing error with sk8-error where possible.
	87 	12/ 7/94	Hernan  	1203974: renderTiled should render the background
							renderer if there is one. This was only a problem
							for cicns and PICTs that may not be rectangular or
							may be translucent.
	88 	12/13/94	rod     	Fixing division by zero error in render of gradient.
							This case wasn't checked for while calculating
							color increments for the shape, oval and rectangle
							gradients.
	89 	 1/20/95	Hernan  	1213005: defining set translucentColor to only
							let people set it to false or an RGBColor.
	90 	 2/ 3/95	Hernan  	get-record-field -> rref or hrref.
	91 	 2/ 3/95	Hernan  	with-extra-gworld-step has to use the clip region
							when doing the copy bits.
	92 	 2/ 3/95	Hernan  	renderUnstretched of QDPicture clips before drawing.
	93 	 2/ 3/95	Hernan  	renderUnstretched of IconRSRC clips before drawing.
	94 	 2/10/95	sidney  	changes for buildstandalone merging resources into application file
	95 	 2/16/95	sidney  	readable argument names for initialize handler
	96 	 3/13/95	Hernan  	1227246: renderUnstretched of iconRSRC needs
							to compose the clip before rendering.
	97 	 3/16/95	till    	wrappin' some traps, #_lockpixels to revalidate-snapshot-pixmap-internal,  pattern-to-port-to-region, colorpattern-to-port-to-region
	98 	 3/17/95	till    	trap wrapping, hlock in renderStretched
	99 	 3/17/95	till    	Better to use with-locked-pixels-force
	100	 3/17/95	till    	damn, I was completely wrong about the hlock stuff
	101	 3/20/95	Hernan  	Making pixelMap behave like a real child of Media.
	102	 3/20/95	Hernan  	resourceType should be resType.
	103	 3/23/95	rod     	fixing unloadmedia of pixelmap to clear the 
							mediadata after disposing it.
	104	 3/27/95	sidney  	fix loadmedia to test for valid macptr, not just non-nil
	105	 3/27/95	sidney  	a little cleanup of code where we saw some crashes. don't know if it will fix anything, but it's a try.
	106	 3/29/95	Hernan  	Ooops. Removing references to the data property
							of PixelMap.
	107	 4/11/95	Hernan  	1238083: loadMedia now produces a nice error message
							when the file of the media is false.
	108	 4/14/95	Hernan  	with-extra-gWorld-step does not attempt to loadMedia of
							translucencyHelper.
	2  	 6/23/95	Hernan  	1247539: if no background specified and renderUnstretched, use white.
	2  	 4/10/96	Hernan  	define-system-handler->define-handler.
	3  	 4/18/96	Hernan  	Making *fast-draw* be stack based.
	4  	 4/18/96	Hernan  	Fixing sourceserver lossage.
	6  	 4/18/96	Hernan  	More sourceserver trials...
	7  	 4/18/96	Hernan  	More sourceserver trials...
	8  	 2/27/97	Hernan  	
|# ;(do not edit past this line!!)



