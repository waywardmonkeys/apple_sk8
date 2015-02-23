;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories


;;;

(in-package :UIDevelopment)

(SK8-declare-syms :SK8 :public ; Updated  3-09-94   2:55 pm
                  SK8::CLEARREFERENCES SK8::SETUPFORPROJECT)

(SK8-declare-syms :UI :public ; Updated  8-30-94   1:52 pm
                  UI::CLOSEBOXRECT UI::GROWBOXRECT UI::MACSTYLEINTERFACE UI::SHADESIZE UI::UIBLUETEXTURES
                  UI::UIGRAYTEXTURES UI::UIGREENTEXTURES UI::UIPINKTEXTURES UI::UIWINDOWCLOSER
                  UI::UIWINDOWGROWER UI::UIWINDOWSUBRECT UI::UIYELLOWTEXTURES UI::ZOOMSIZE)
(in-package :UIDevelopment)

(defparameter *MacStyleInterface* nil)

(defparameter *WindowTop* 20)
(defparameter *WindowLeft* 6)
(defparameter *WindowRight* 6)
(defparameter *WindowBottom* 6)
(defparameter *BarLeft* 15)

(addproperty ui 'MacStyleInterface)

(define-handler (setf MacStyleInterface) (theval ui)
  (unless (eq theval *MacStyleInterface*)
    (sk8dev::withlockedcursor animatedClock
      (mapcar #'lock (contents stage))
      (setvalue 'MacStyleInterface me theval)
      (setf *MacStyleInterface* theval)
      (if theval
        (progn
          (setf *WindowTop* 0)
          (setf *WindowLeft* 0)
          (setf *WindowRight* 0)
          (setf *WindowBottom* 0)
          (setf *BarLeft* 0)
          )
        (progn
          (setf *WindowTop* 20)
          (setf *WindowLeft* 6)
          (setf *WindowRight* 6)
          (setf *WindowBottom* 6)
          (setf *BarLeft* 15)
          ))
      (dolist (i (knowndescendants UISimpleWindow))
        (setf (text i) (text i))
        (setf (textvoffset i) (if theval -100 (textvoffset UISimpleWindow)))
        (setf (windowstyle i) (if theval 'documentwithzoom 'blank))
        (setf (top i :resizing t) (+ (top i) (* (if theval 1 -1) 20)))
        (resized i)
        (setf (visible (CloseBoxRect i)) (not theval)) 
        (setf (visible (GrowBoxRect i)) (not theval)) 
        )
      (when (and (not theval) (eq (container pbmholder) stage))
        (setf (container pbmholder) nil)
        (setf (container pbmholder) stage)
        )
      (when (eq (container drawpalette) stage)
        (setf (tools drawpalette) (tools drawpalette))
        )
      (mapcar #'unlock (contents stage))
      )))

;;;(setf (MacStyleInterface ui) t)
;;;(setf (MacStyleInterface ui) nil)
;;;(setf *MacStyleInterface* t)
;; These are the windows that the interface keeps using over and over.

;;;;
(new Renderer :objectname "UIRenderer" :project ui)
(addproperty UIRenderer 'backgroundRenderer)

(new rectangle :objectname "UISimpleWindow" :project ui
     :properties '((minimumSize :value (20 20))))

(addproperty UISimpleWindow 'Resizer)
(addproperty UISimpleWindow 'ZoomBox)
(addproperty UISimpleWindow 'sk8::menubar)
(addproperty UISimpleWindow 'ZoomSize)
(addproperty UISimpleWindow 'ShadeSize)

(define-handler (setf resizer) :after (theval UISimpleWindow)
                (let ((c (container me)))
                  (when c
                    (setf (container me) nil)
                    (setf (container me) c))))
(define-handler (setf ZoomBox) :after (theval UISimpleWindow)
                (let ((c (container me)))
                  (when c
                    (setf (container me) nil)
                    (setf (container me) c))))
(define-handler (setf sk8::menubar) :after (theval UISimpleWindow)
                (setf (textVoffset me) (if theval 1 0))
                (forceredraw me))
(define-handler (setf text) :after (theval UISimpleWindow)
                (setf (windowtitle me) theval)
                )
(define-handler makeFillRegion (UISimpleWindow)
  (let ((theFlags (gs:node-flags me)))
    (gs:recompute-bounds-region me theFlags)
    (#_copyRgn (gs:node-boundsRegion me) (gs:node-fillRegion me))
    (gs:fillDirty! theFlags 0)))

(define-handler makeFrameRegion (UISimpleWindow)
  (gs:FrameDirty! (gs:node-flags me) 0))

(define-handler initialize (UISimpleWindow original isNew initArgs)
  (declare (ignore isNew initArgs))
  (call-next-method)
  (push me (knownchildren original))
  (#_setEmptyRgn (gs:node-fillRegion me)))


(defmacro set-qd-rect (rect ll tt rr bb)
  `(progn
     (rset ,rect :rect.left ,ll)
     (rset ,rect :rect.top ,tt)
     (rset ,rect :rect.right ,rr)
     (rset ,rect :rect.bottom ,bb)))

(defvar *UISimpleWindowTopLeft* (#_newRgn))

(defun init-UISimpleWindow-topleft ()
  (gs:let+ ((r (:rect))
            (tempRgn (:region))
            (totalDirt (:region)))
    (#_setEmptyRgn totalDirt)
    ;; 18 by 18 square.
    (set-qd-rect r 0 0 18 18)
    (#_rectRgn *UISimpleWindowTopleft* r)
    ;; Vertical 1.
    (set-qd-rect r 0 0 1 17)
    (#_rectRgn tempRgn r)
    (#_unionRgn totalDirt tempRgn totalDirt)
    ;; Vertical 2.
    (set-qd-rect r 1 0 2 16)
    (#_rectRgn tempRgn r)
    (#_unionRgn totalDirt tempRgn totalDirt)
    ;; Horizontal 1.
    (set-qd-rect r 0 0 17 1)
    (#_rectRgn tempRgn r)
    (#_unionRgn totalDirt tempRgn totalDirt)
    ;; Horizontal 2.
    (set-qd-rect r 0 1 16 2)
    (#_rectRgn tempRgn r)
    (#_unionRgn totalDirt tempRgn totalDirt)
    ;; Now the stuff in the middle.
    (set-qd-rect r 2 2 3 4)
    (#_rectRgn tempRgn r)
    (#_unionRgn totalDirt tempRgn totalDirt)
    (set-qd-rect r 2 2 4 3)
    (#_rectRgn tempRgn r)
    (#_unionRgn totalDirt tempRgn totalDirt)
    ;; Subtract dirt from *UISimpleWindowTopleft*.
    (#_diffRgn *UISimpleWindowTopleft* totalDirt *UISimpleWindowTopleft*)))

(defvar *UISimpleWindowTopRight* (#_newRgn))

(defun init-UISimpleWindow-topRight ()
  (gs:let+ ((r (:rect))
         (tempRgn (:region))
         (totalDirt (:region)))
    (#_setEmptyRgn totalDirt)
    ;; 18 by 18 square.
    (set-qd-rect r 0 0 18 18)
    (#_rectRgn *UISimpleWindowTopRight* r)
    ;; Vertical 1.
    (set-qd-rect r 17 0 18 17)
    (#_rectRgn tempRgn r)
    (#_unionRgn totalDirt tempRgn totalDirt)
    ;; Vertical 2.
    (set-qd-rect r 16 0 17 16)
    (#_rectRgn tempRgn r)
    (#_unionRgn totalDirt tempRgn totalDirt)
    ;; Horizontal 1.
    (set-qd-rect r 1 0 17 1)
    (#_rectRgn tempRgn r)
    (#_unionRgn totalDirt tempRgn totalDirt)
    ;; Horizontal 2.
    (set-qd-rect r 2 1 17 2)
    (#_rectRgn tempRgn r)
    (#_unionRgn totalDirt tempRgn totalDirt)
    ;; Now the stuff in the middle.
    (set-qd-rect r 14 2 16 3)
    (#_rectRgn tempRgn r)
    (#_unionRgn totalDirt tempRgn totalDirt)
    (set-qd-rect r 15 3 16 4)
    (#_rectRgn tempRgn r)
    (#_unionRgn totalDirt tempRgn totalDirt)
    ;; Subtract dirt from *UISimpleWindowTopleft*.
    (#_diffRgn *UISimpleWindowTopRight* totalDirt *UISimpleWindowTopRight*)))

(defvar *UISimpleWindowTopRight2* (#_newRgn))
;;(init-UISimpleWindow-topRight2)
(defun init-UISimpleWindow-topRight2 ()
  (gs:let+ ((r (:rect))
         (tempRgn (:region))
         (totalDirt (:region)))
    (#_setEmptyRgn totalDirt)
    ;; 18 by 18 square.
    (set-qd-rect r 0 0 18 18)
    (#_rectRgn *UISimpleWindowTopRight2* r)
    ;; Vertical 1.
    (set-qd-rect r 17 0 18 17)
    (#_rectRgn tempRgn r)
    (#_unionRgn totalDirt tempRgn totalDirt)
    ;; Vertical 2.
    (set-qd-rect r 16 0 17 16)
    (#_rectRgn tempRgn r)
    (#_unionRgn totalDirt tempRgn totalDirt)
    ;; Horizontal 1.
    (set-qd-rect r 1 0 17 1)
    (#_rectRgn tempRgn r)
    (#_unionRgn totalDirt tempRgn totalDirt)
    ;; Horizontal 2.
    (set-qd-rect r 2 1 17 2)
    (#_rectRgn tempRgn r)
    (#_unionRgn totalDirt tempRgn totalDirt)
    ;; Now the stuff in the middle.
    (set-qd-rect r 3 0 18 11)
    (#_rectRgn tempRgn r)
    (#_unionRgn totalDirt tempRgn totalDirt)
    (set-qd-rect r 4 0 18 13)
    (#_rectRgn tempRgn r)
    (#_unionRgn totalDirt tempRgn totalDirt)
    (set-qd-rect r 5 0 18 14)
    (#_rectRgn tempRgn r)
    (#_unionRgn totalDirt tempRgn totalDirt)
    (set-qd-rect r 7 0 18 15)
    (#_rectRgn tempRgn r)
    (#_unionRgn totalDirt tempRgn totalDirt)
    (set-qd-rect r 9 0 18 16)
    (#_rectRgn tempRgn r)
    (#_unionRgn totalDirt tempRgn totalDirt)
    ;; Subtract dirt from *UISimpleWindowTopleft*.
    (#_diffRgn *UISimpleWindowTopRight2* totalDirt *UISimpleWindowTopRight2*)))

(defvar *UISimpleWindowBottomRight* (#_newRgn))

(defun init-UISimpleWindow-bottomRight ()
  (gs:let+ ((r (:rect))
         (tempRgn (:region))
         (totalDirt (:region)))
    (#_setEmptyRgn totalDirt)
    ;; 18 by 18 square.
    (set-qd-rect r 0 0 18 18)
    (#_rectRgn *UISimpleWindowBottomRight* r)
    ;; Vertical 1.
    (set-qd-rect r 17 1 18 18)
    (#_rectRgn tempRgn r)
    (#_unionRgn totalDirt tempRgn totalDirt)
    ;; Vertical 2.
    (set-qd-rect r 16 2 17 18)
    (#_rectRgn tempRgn r)
    (#_unionRgn totalDirt tempRgn totalDirt)
    ;; Horizontal 1.
    (set-qd-rect r 1 17 18 18)
    (#_rectRgn tempRgn r)
    (#_unionRgn totalDirt tempRgn totalDirt)
    ;; Horizontal 2.
    (set-qd-rect r 2 16 18 17)
    (#_rectRgn tempRgn r)
    (#_unionRgn totalDirt tempRgn totalDirt)
    ;; Now the stuff in the middle.
    (set-qd-rect r 15 14 17 15)
    (#_rectRgn tempRgn r)
    (#_unionRgn totalDirt tempRgn totalDirt)
    (set-qd-rect r 14 15 16 16)
    (#_rectRgn tempRgn r)
    (#_unionRgn totalDirt tempRgn totalDirt)
    ;; Subtract dirt from *topleft*.
    (#_diffRgn *UISimpleWindowBottomRight* totalDirt *UISimpleWindowBottomRight*)))

(defvar *UISimpleWindowBottomRight2* (#_newRgn))

(defun init-UISimpleWindow-bottomRight2 ()
  (gs:let+ ((r (:rect))
         (tempRgn (:region))
         (totalDirt (:region)))
    (#_setEmptyRgn totalDirt)
    ;; 18 by 18 square.
    (set-qd-rect r 0 0 18 18)
    (#_rectRgn *UISimpleWindowBottomRight2* r)
    ;; Vertical 1.
    (set-qd-rect r 17 15 18 18)
    (#_rectRgn tempRgn r)
    (#_unionRgn totalDirt tempRgn totalDirt)
    ;; Vertical 2.
    (set-qd-rect r 16 16 17 18)
    (#_rectRgn tempRgn r)
    (#_unionRgn totalDirt tempRgn totalDirt)
    ;; Horizontal 1.
    (set-qd-rect r 12 17 16 18)
    (#_rectRgn tempRgn r)
    (#_unionRgn totalDirt tempRgn totalDirt)
    ;; Subtract dirt from *UISimpleWindowTopleft*.
    (#_diffRgn *UISimpleWindowBottomRight2* totalDirt *UISimpleWindowBottomRight2*)))



(defvar *UISimpleWindowBottomLeft* (#_newRgn))

(defun init-UISimpleWindow-bottomLeft ()
  (gs:let+ ((r (:rect))
         (tempRgn (:region))
         (totalDirt (:region)))
    (#_setEmptyRgn totalDirt)
    ;; 18 by 18 square.
    (set-qd-rect r 0 0 18 18)
    (#_rectRgn *UISimpleWindowBottomLeft* r)
    ;; Vertical 1.
    (set-qd-rect r 0 15 1 18)
    (#_rectRgn tempRgn r)
    (#_unionRgn totalDirt tempRgn totalDirt)
    ;; Vertical 2.
    (set-qd-rect r 1 16 2 18)
    (#_rectRgn tempRgn r)
    (#_unionRgn totalDirt tempRgn totalDirt)
    ;; Horizontal 1.
    (set-qd-rect r 0 17 4 18)
    (#_rectRgn tempRgn r)
    (#_unionRgn totalDirt tempRgn totalDirt)
    ;; Subtract dirt from *UISimpleWindowTopleft*.
    (#_diffRgn *UISimpleWindowBottomLeft* totalDirt *UISimpleWindowBottomLeft*)))

;;; The region has to be a region or else!!!

(defun align-region (region newLeft newTop)
  (rlet ((r :rect))
    (gs:region-into-rect region r)
    (#_offsetRgn 
     region 
     (- newLeft (rref r :rect.left))
     (- newTop (rref r :rect.top)))))

(define-handler makeBoundsRegion (UISimpleWindow)
  (if *MacStyleInterface*
    (call-next-method)
    (gs:let+ ((physRect (gs:recompute-physicalBoundsRect me))
              (ll (gs:rect-left physRect))
              (tt (gs:rect-top physRect))
              (rr (gs:rect-right physRect))
              (bb (gs:rect-bottom physRect))
              (r (:rect))
              (temp (:region))
              (bounds (gs:node-boundsRegion me))
              (flags (gs:node-flags me)))
      ;; Rect 1.
      (set-qd-rect r (+ ll 18) tt (- rr 18) bb)
      (#_rectRgn bounds r)
      ;; Rect 2.
      (set-qd-rect r ll (+ tt 18) rr (- bb 18))
      (#_rectRgn temp r)
      (#_unionRgn bounds temp bounds)
      ;; TopLeft Corner.
      (align-region *UISimpleWindowTopleft* ll tt)
      (#_unionRgn bounds *UISimpleWindowTopleft* bounds)
      ;; TopRight Corner.
      (if (zoombox me)
        (progn
          (align-region *UISimpleWindowTopRight* (- rr 18) tt)
          (#_unionRgn bounds *UISimpleWindowTopRight* bounds))
        (progn
          (align-region *UISimpleWindowTopRight2* (- rr 18) tt)
          (#_unionRgn bounds *UISimpleWindowTopRight2* bounds)))
      (if (ShadeSize me)
        (progn
          (set-qd-rect r ll (+ tt 18) rr bb)
          (#_rectRgn temp r)
          (#_unionRgn bounds temp bounds))
        (progn
          ;; BottomRight Corner.
          (if (resizer me)
            (progn
              (align-region *UISimpleWindowBottomRight* (- rr 18) (- bb 18))
              (#_unionRgn bounds *UISimpleWindowBottomRight* bounds))
            (progn
              (align-region *UISimpleWindowBottomRight2* (- rr 18) (- bb 18))
              (#_unionRgn bounds *UISimpleWindowBottomRight2* bounds)))
          ;; BottomLeft Corner.
          (align-region *UISimpleWindowBottomLeft* ll (- bb 18))
          (#_unionRgn bounds *UISimpleWindowBottomLeft* bounds))
        )
      ;; Done.
      (gs:boundsDirty! flags 0))))

(define-handler mousedown (UISimpleWindow)
  (if (eq me (eventActor))
    (cond
     ((sk8-multival-bind (hh vv) (mouseloc stage)
        (sk8-multival-bind (ll tt rr bb) (boundsrect me :physical t)
          (declare (ignore tt ll))
          (and (resizer me) (>= hh (- rr 18)) (>= vv (- bb 18)))))
      (resize me 'bottomright))
     (t 
      (sk8-multival-bind (ll tt rr bb) (boundsrect me :physical t)
        (drag me :live t)
        (sk8-multival-bind (hh vv) (mouseloc stage)
          (sk8-multival-bind (ml mt mr mb) (boundsrect (mainmonitor system))
            (if (and (<= hh mr) (>= hh ml) (<= vv (+ mt 20)) (>= vv mt))
              (setboundsrect me ll tt rr bb :physical t)))))
      ))
    (call-next-method)))


(define-handler doubleclick (UISimpleWindow)
  (let ((vv (eventv))
        (tt (top me)))
    (when (and (eq me (eventactor)) (<= vv (+ tt 19)))
      (let ((size (ShadeSize me)))
        (sk8::withcursor watchcursor
          (if size
            (progn
              (setf (ShadeSize me) nil)
              (setf (minimumsize me) (cadr size))
              (setf (bottom me :resizing t) (+ (top me) (car size)))
              (moveonstage me))
            (progn
              (setf (ShadeSize me) (list (height me) (minimumsize me)))
              (setf (minimumsize me) '(1 1))
              (setf (bottom me :resizing t) (+ (top me) 19)))
            ))
        )
      )))

(define-handler restore (UISimpleWindow)
  (call-next-method)
  (when (eq me UISimpleWindow)
    (setf *UISimpleWindowTopleft* (#_newrgn))
    (setf *UISimpleWindowTopRight* (#_newrgn))
    (setf *UISimpleWindowTopRight2* (#_newrgn))
    (setf *UISimpleWindowBottomRight* (#_newrgn))
    (setf *UISimpleWindowBottomRight2* (#_newrgn))
    (setf *UISimpleWindowBottomLeft* (#_newrgn))
    (init-UISimpleWindow-bottomLeft)
    (init-UISimpleWindow-bottomRight)
    (init-UISimpleWindow-bottomRight2)
    (init-UISimpleWindow-topRight)
    (init-UISimpleWindow-topRight2)
    (init-UISimpleWindow-topLeft)))

(define-handler preserve (UISimpleWindow)
  (call-next-method)

  (when (and *UISimpleWindowTopleft* (macptrp *UISimpleWindowTopleft*))
    (#_disposergn *UISimpleWindowTopleft*))
  (setf *UISimpleWindowTopleft* nil)

  (when (and *UISimpleWindowTopRight* (macptrp *UISimpleWindowTopRight*))
    (#_disposergn *UISimpleWindowTopRight*))
  (setf *UISimpleWindowTopRight* nil)

  (when (and *UISimpleWindowTopRight2* (macptrp *UISimpleWindowTopRight2*))
    (#_disposergn *UISimpleWindowTopRight2*))
  (setf *UISimpleWindowTopRight2* nil)

  (when (and *UISimpleWindowBottomRight* (macptrp *UISimpleWindowBottomRight*))
    (#_disposergn *UISimpleWindowBottomRight*))
  (setf *UISimpleWindowBottomRight* nil)

  (when (and *UISimpleWindowBottomRight2* (macptrp *UISimpleWindowBottomRight2*))
    (#_disposergn *UISimpleWindowBottomRight2*))
  (setf *UISimpleWindowBottomRight2* nil)

  (when (and *UISimpleWindowBottomLeft* (macptrp *UISimpleWindowBottomLeft*))
    (#_disposergn *UISimpleWindowBottomLeft*))
  (setf *UISimpleWindowBottomLeft* nil))

(init-UISimpleWindow-bottomLeft)
(init-UISimpleWindow-bottomRight)
(init-UISimpleWindow-bottomRight2)
(init-UISimpleWindow-topRight)
(init-UISimpleWindow-topRight2)
(init-UISimpleWindow-topLeft)


(new imageRenderer :objectName "UITopLeft" :project ui)
(setf (backgroundRenderer UITopLeft) nil)
(new imageRenderer :objectName "UITopMiddle" :project ui)
(setf (backgroundRenderer UITopMiddle) nil)
(new imageRenderer :objectName "UITopRight" :project ui)
(setf (backgroundRenderer UITopRight) nil)
(new imageRenderer :objectName "UITopLeft2" :project ui)
(setf (backgroundRenderer UITopLeft2) nil)
(new imageRenderer :objectName "UITopMiddle2" :project ui)
(setf (backgroundRenderer UITopMiddle2) nil)
(new imageRenderer :objectName "UITopRight2" :project ui)
(setf (backgroundRenderer UITopRight2) nil)
(new imageRenderer :objectName "UIMiddleLeft" :project ui)
(setf (backgroundRenderer UIMiddleLeft) nil)
(new imageRenderer :objectName "UIMiddle" :project ui)
(setf (backgroundRenderer UIMiddle) nil)

(define-handler render (UIMiddle theActor region thePaper)
  (if *MacStyleInterface*
    (render white theactor region thePaper)
    (call-next-method)))


(new imageRenderer :objectName "UIMiddleRight" :project ui)
(setf (backgroundRenderer UIMiddleRight) nil)
(new imageRenderer :objectName "UIBottomLeft" :project ui)
(setf (backgroundRenderer UIBottomLeft) nil)
(new imageRenderer :objectName "UIBottomMiddle" :project ui)
(setf (backgroundRenderer UIBottomMiddle) nil)
(new imageRenderer :objectName "UIBottomRight" :project ui)
(setf (backgroundRenderer UIBottomRight) nil)
(new imageRenderer :objectName "UIBottomRight2" :project ui)
(setf (backgroundRenderer UIBottomRight2) nil)
(new rgbcolor :objectName "UIlightcolor" :project ui)
(new rgbcolor :objectName "UIdarkcolor" :project ui)

(new object :objectname "UITextureData" :project ui)
(addproperty UITextureData 'media)
(addproperty UITextureData 'fillcolor)
(addproperty UITextureData 'lightcolor)
(addproperty UITextureData 'darkcolor)


(defun CreateTextureObject (objname color resID darklist lightlist)
  (let (Med
        (dataobj (new UITextureData :objectname objname :project ui)))
    (setf (Fillcolor dataobj) color)
    (setf (lightcolor dataobj) lightlist)
    (setf (darkcolor dataobj) darklist)
    (dotimes (i 13)
      (push (new ColorPattern :project ui :resourceId (+ resid i) :file (file ui))
            Med))
    (setf (media dataobj) (reverse Med))))

(define-handler loadTexture (UITextureData)
  (when (media me)
    (unless (eq (media UITopLeft) (car (media me)))
      (let ((x (copy-list (media me))))
        (dolist (i (list UITopLeft UITopMiddle UITopRight
                         UIMiddleLeft UIMiddle UIMiddleRight
                         UIBottomLeft UIBottomMiddle UIBottomRight
                         UITopLeft2 UITopMiddle2 UITopRight2
                         UIBottomRight2))
          (setf (media i) (pop x))))
      (setf (forered UILightColor) (car (lightcolor me)))
      (setf (foregreen UILightColor) (second (lightcolor me)))
      (setf (foreblue UILightColor) (third (lightcolor me)))
      (setf (forered UIdarkcolor) (car (darkcolor me)))
      (setf (foregreen UIdarkcolor) (second (darkcolor me)))
      (setf (foreblue UIdarkcolor) (third (darkcolor me)))
      
      (when (and (boundp 'ui::swatchhashtable)
                 (mf::object-class-p ui::swatchhashtable))
        (maphash #'(lambda (x y) 
                     (declare (ignore y))
                     (updateswatchofactor x)) swatchhashtable))
      (sk8dev::do-sk8-window-actors (i)
        (when (eq (project i) ui)
          (forceRedraw i)))
      )))



(CreateTextureObject "UIGrayTextures" gray 1001 '(39321 39321 39321) '( 39321 39321 52429)) ;;This is the color blind safe highlight
(CreateTextureObject "UIBlueTextures" blue 2001 '(39321 39321 52428) '(52428 52428 65535))
(CreateTextureObject "UIPinkTextures" pink 3001 '(52428 39321 39321) '(65535 52428 52428))
(CreateTextureObject "UIGreenTextures" green 4001 '(26214 39321 39321) '(39321 52428 39321))
(CreateTextureObject "UIYellowTextures" yellow 5001 '(39321 39321 26214) '(52428 52428 39321))

#|  UNCOMMENT THE FOLLOWING FOR ALBEN+FARIS COLOR SAFE CHOICES...
(setf (lightcolor UIGrayTextures) '( 39321 39321 52429))

(setf (lightcolor UIBlueTextures) '( 39321 39321 52429))
(setf (lightcolor UIPinkTextures) '( 52429 39321 39321))
(setf (lightcolor UIGreenTextures) '( 26214 39321 39321))
(setf (lightcolor UIYellowTextures) '( 39321 39321 26214))
|#


;(loadtexture UIGrayTextures)
;(loadtexture UIBlueTextures)
;(loadtexture UIPinkTextures)
(loadtexture UIGreenTextures)
;(loadtexture UIYellowTextures)

(setf (renderstyle UITopLeft) 'Renderunstretched)
(setf (renderstyle UITopRight) 'Renderunstretched)
(setf (renderstyle UITopLeft2) 'Renderunstretched)
(setf (renderstyle UITopRight2) 'Renderunstretched)
(setf (renderstyle UIBottomLeft) 'Renderunstretched)
(setf (renderstyle UIBottomRight) 'Renderunstretched)
(setf (renderstyle UIBottomRight2) 'Renderunstretched)

;;; Port must already be set!!!

(defmacro with-offseted-port-and-clip (clipRgn h v &body body)
  `(progn 
     (require-trap #_SetOrigin ,h ,v)
     (require-trap #_offsetRgn ,clipRgn ,h ,v)
     (gs:with-clipped-region ,clipRgn
       ,@body)
     (require-trap #_offsetRgn ,clipRgn (- ,h) (- ,v))))

(new UIRenderer :objectname "UIWindowColor" :project ui)

(define-handler render (UIWindowColor theActor region thePaper)
  (declare (ignore region))
  (gs:let+ ((ourRect (:rect))
         (ourRegion (:region))
         qd-rect
         left
         top
         right
         bottom
         (thePort (:pointer))
         (theClip (:region))
         squashed?
         topLeft
         fillRegion)
    (#_getClip theClip)
    (#_getPort thePort)
    (setf topLeft (rref (rref (%get-ptr thePort) :cGrafPort.portRect) :rect.topleft))
    
    (gs:recompute-fill-region theActor (gs:node-flags theactor))
    (setf fillRegion (gs:node-fillRegion theActor))
    (setf qd-rect (gs:hrref fillRegion :region.rgnbbox :storage :pointer))
    (setf left (rref qd-rect :rect.left))
    (setf top (rref qd-rect :rect.top))
    (setf right (rref qd-rect :rect.right))
    (setf bottom (rref qd-rect :rect.bottom))
    (setf squashed? (> 50 (- bottom top)))
    
    (render UIMiddle theActor fillRegion thePaper)
    (if *MacStyleInterface*
      (when (resizer theactor)
        (rset ourRect :rect.left (- right 16))
        (rset ourRect :rect.top (- bottom 16))
        (rset ourRect :rect.right right)
        (rset ourRect :rect.bottom bottom)
        (#_rectRgn ourRegion ourRect)
        (render ResizeBoxRenderer theActor ourRegion thePaper)
        )
      (progn
        (rset ourRect :rect.left (+ left 31))
        (rset ourRect :rect.top top)
        (rset ourRect :rect.right (- right 10))
        (rset ourRect :rect.bottom (+ top 64))
        (#_rectRgn ourRegion ourRect)
        (if (sk8::menubar theactor)
          (render UITopMiddle theActor ourRegion thePaper)
          (render UITopMiddle2 theActor ourRegion thePaper))
        (unless squashed?
          (rset ourRect :rect.left left)
          (rset ourRect :rect.top (+ top 10))
          (rset ourRect :rect.right (+ left 31))
          (rset ourRect :rect.bottom (- bottom 10))
          (#_rectRgn ourRegion ourRect)
          (render UIMiddleLeft theActor ourRegion thePaper)
          
          (rset ourRect :rect.left 0)
          (rset ourRect :rect.top (+ top 31))
          (rset ourRect :rect.right 32)
          (rset ourRect :rect.bottom (- bottom 31))
          (#_rectRgn ourRegion ourRect)
          (with-offseted-port-and-clip theClip (- (- right 32)) 0
            (render UIMiddleRight theActor ourRegion thePaper))
          
          (rset ourRect :rect.left (+ left 31))
          (rset ourRect :rect.top 0)
          (rset ourRect :rect.right (- right 31))
          (rset ourRect :rect.bottom 32)
          (#_rectRgn ourRegion ourRect)
          (with-offseted-port-and-clip theClip 0 (- (- bottom 32))
            (render UIBottomMiddle theActor ourRegion thePaper))
          )
        (rset ourRect :rect.left 0)
        (rset ourRect :rect.top 0)
        (rset ourRect :rect.right 32)
        (rset ourRect :rect.bottom 64)
        (setf (voffset UIBottomLeft) 0)
        (#_rectRgn ourRegion ourRect)
        (with-offseted-port-and-clip theClip 0 0
          (if (sk8::menubar theactor)
            (render UITopLeft theActor ourRegion thePaper)
            (render UITopLeft2 theActor ourRegion thePaper))
          )
        (with-offseted-port-and-clip theClip (- (- right 32)) 0
          (if (sk8::menubar theactor)
            (render UITopRight theActor ourRegion thePaper)
            (render UITopRight2 theActor ourRegion thePaper)))
        
        (unless squashed?
          (with-offseted-port-and-clip theClip 0 (- (- bottom 32))
            (render UIBottomLeft theActor ourRegion thePaper))
          (with-offseted-port-and-clip theClip (- (- right 32)) (- (- bottom 32))
            (if (resizer theactor)
              (render UIBottomRight theActor ourRegion thePaper)
              (render UIBottomRight2 theActor ourRegion thePaper))))
        
        (with-port (%get-ptr thePort)
          (#_setOrigin (point-h topleft) (point-v topleft)))

        ;;;Now we draw the text
        (let* ((properties (gs:node-properties theactor))
               (default-text-size (gs:f.round (gs:f* (textSize theactor) (gs:node-yScale theactor))))
               (default-text-font (getf properties :text-font ChicagoFont))
               (default-text-style (getf properties :text-style 0))
               (default-text-color (make-color 57016 57016 57016))
               (thestring (text theactor))
               (width (- right left 45))
               (len (length thestring))
               count
               (low 1)
               (high len))
          (#_TextFont (fontData default-text-font))
          (#_TextFace default-text-style)
          (#_TextSize default-text-size)
          (unless (< (with-cstrs ((cstr thestring)) (#_TextWidth cstr 0 len)) width)
            (decf width (with-cstrs ((cstr "É")) (#_TextWidth cstr 0 1)))
            (loop
              (setq count (+ low (truncate (- high low) 2)))
              (if (> (with-cstrs ((cstr (subseq thestring 0 count))) (#_TextWidth cstr 0 count)) width)
                (setq high count)
                (setq low count))
              (when (= high (1+ low))
                (setq thestring (concatenate 'string (subseq thestring 0 (1- high)) "É"))
                (setq len high)
                (return))
              ))
          (with-rgb (rgb default-text-color)
            (#_RGBForeColor rgb))
          (#_MoveTo (+ (point-h topleft) (texthoffset theactor) 4)
           (+ (point-v topleft) (textvoffset theactor) 14))
          (with-cstrs ((cstr thestring))
            (#_DrawText cstr 0 (length thestring)))
          (with-rgb (rgb 0)
            (#_RGBForeColor rgb))
          (#_MoveTo (+ (point-h topleft) (texthoffset theactor) 3)
           (+ (point-v topleft) (textvoffset theactor) 13))
          (with-cstrs ((cstr thestring))
            (#_DrawText cstr 0 (length thestring)))
          )
        
        ))))


#|

(dolist (me (knowndescendants UISimpleWindow))
  (case (gs:node-drawFunction me)
                 (gs:sk8-text-draw (setf (gs:node-drawFunction me) 'gs:sk8-simple-draw))
                 (gs:sk8-full-draw (setf (gs:node-drawFunction me) 'gs:sk8-subActors-draw))))


|#

;;WE DON'T USE THE SYSTEM'S TEXT STUFF!
(define-handler (setf text) (theText UISimpleWindow &key textLocation)
  ;; If no location was passed, use the one in the actor's properties.
  (gs:making-self-dirty (me t nil nil nil (gs:node-fillRegion me) :fill nil)
    (if (string= "" theText)
      ;; clearing text.
      (setf (getf (gs:node-properties me) :text) "")
      ;; setting text.
      (progn 
        (setf (getf (gs:node-properties me) :text) theText)
        (setf (getf (gs:node-properties me) :location) textlocation)
        ))
    )
  theText)

(setf (fillcolor UISimpleWindow) UIWindowColor)

(setf (textlocation UISimpleWindow) 'topleft)
(setf (dofirstclick UISimpleWindow) t)
(setf (windowstyle UISimpleWindow) 'blank)
(setf (textfont UISimpleWindow) sk8::EspySansBoldFont)
(setf (textSize UISimpleWindow) 10)
(setf (textstyle UISimpleWindow) '(Plain))
(setf (texthoffset UISimpleWindow) 20)
(setf (framesize UISimpleWindow) '(0 0))


(define-handler addedMeAsParent (UISimpleWindow Child OldParents)
  (declare (ignore oldparents))
  (let ((tt (text child)))
    (setf (framesize Child) '(0 0))
    (setf (fillcolor Child) (fillcolor UISimpleWindow))
    (copyTextProperties child UIButton)
    (call-next-method)
    (setf (text child) tt)
    (setf (windowstyle child) (windowstyle UISimpleWindow))
    (setf (CloseBoxRect child) (new (CloseBoxRect UISimpleWindow) :project ui :container child))
    (setf (GrowBoxRect child) (new (GrowBoxRect UISimpleWindow) :project ui :container child))
    ))

;;__________________________________________________________________
;; (DE)ACTIVATE

(define-handler deactivate (UISimpleWindow)
  (deactivatewidget (keytarget me))
  (call-next-method)
  (setf (currentcommandkey system) nil)
  t)

(define-handler activate (UISimpleWindow)
  (let (hasmenubar)
    (dolist (i (contents me))
      (when (is-a i sk8::menubar)
        (setf hasmenubar t)
        (setf (currentcommandkey system) i)))
    (unless hasmenubar
      (setf (currentcommandkey system) ProjectBuilderMenubar))
    ;; Deals with the case when a window is activating by clicking on an
    ;; actor that can be the keytarget. If the old keytarget of the window
    ;; was someone else, it would activateText and only THEN would the new
    ;; keytarget activate itself. This produces some nasty flashing that
    ;; we get rid of using this hack.
    ;; NOTE: should use the eventActor instead of the thing currently under
    ;; the mouse (which could be the window coming up).
    (let ((isMouseDown? (and (boundp '*current-event*)
                             (eql 1 (rref *current-event* :event.what))))
          (eventActor (eventActor)))
      (if (and isMouseDown? 
               eventActor
               (eq (sk8::window eventActor) me) 
               (or (is-a eventActor textlist)
                   (is-a eventActor edittext)
                   (is-a (container eventActor) textlist)
                   (is-a (container eventActor) standardinputfield)))
        ;; (setf (slot-value (gs:node-window me) 'gs:curKey) nil)
        (setf (getf (gs:node-properties me) :curKey) nil)
        (progn
          (call-next-method)
          (activatewidget (keytarget me)))))
    (maybeRemove selectionhalo)
    t))

(define-handler deactivate (ui)
  (call-next-method)
  (maybeRemove selectionhalo))

(defun deactivatewidget (widget)
  (when (and widget
             (or (is-a widget edittext) (is-a widget picker) (is-a widget tablepicker))
             (neq (fillcolor widget) shadowWhite))
    (setf (fillcolor widget) shadowWhite))
  )
(defun activatewidget (widget)
  (when (and widget
             (or (is-a widget edittext) (is-a widget picker) (is-a widget tablepicker))
             (neq (fillcolor widget) white))
    (setf (fillcolor widget) white)))


(define-handler (setf keytarget) (theval UISimpleWindow)
  (let ((xx (keytarget me)))
    (cond
     ((and theval (eq xx theval) (eq (fillcolor theval) white))
      nil)
     ((neq me (sk8dev::eventWindow))
      (call-next-method))
     (t
      (withactorlocked (me)
        (deactivatewidget xx)
        (when theval
          (activatewidget theval))
        (call-next-method)))
     )))

(define-handler (setf fillcolor) (theval UISimpleWindow)
  (if (eq theval uiwindowcolor)
    (call-next-method)
    ))



;;;_______________________________________________________________________


(new rectangle :objectname "UIWindowSubRect" :project ui)
(setframesize UIWindowSubRect 0 0)
(setf (autohighlight UIWindowSubRect) t)

(new UIWindowSubRect :objectname "UIWindowCloser" :project ui)
(setf (container UIWindowCloser) UISimpleWindow)
(setf (boundsrect UIWindowCloser) '(-20 -20 -10 -10))
(setf (fillcolor UIWindowCloser) uiCloseBoxRenderer)
(tagpart UISimpleWindow UIWindowCloser 'ui::CloseBoxRect)
(define-handler highlight (UIWindowCloser)
  (eq (fillcolor me) uiHiliteBoxRenderer)
  )
(define-handler (setf highlight) (theval UIWindowCloser)
  (if theval
    (setf (fillcolor me) uiHiliteBoxRenderer)
    (setf (fillcolor me) uiCloseBoxRenderer))
  )


(define-handler click (UIWindowCloser)
  (sk8::close (sk8::window me)))

(new UIWindowSubRect :objectname "UIWindowGrower" :project ui)
(setf (container UIWindowGrower) UISimpleWindow)
(setf (boundsrect UIWindowGrower) '(-20 -20 -10 -10))
(setf (fillcolor UIWindowGrower) uiZoomBoxRenderer)
(tagpart UISimpleWindow UIWindowGrower 'GrowBoxRect)

(define-handler click (UIWindowGrower)
  (if (or (not (resizer (sk8::window me))) (ShadeSize (sk8::window me)))
    (ed-beep)
    (let ((size (zoomSize (sk8::window me))))
      (sk8::withcursor watchcursor
        (if size
          (progn
            (setf (zoomsize (sk8::window me)) nil)
            (setf (boundsrect (sk8::window me)) size)
            (moveonstage (sk8::window me)))
          (progn
            (setf (zoomSize (sk8::window me)) (boundsrect (sk8::window me)))
            ;;;***** THE FOLLOWING SHOULDN'T USE THE MAIN MONITOR ALWAYS,
            ;;; BUT RATHER THE MONITOR THAT MOST OF THE ACTOR IS ON!
            (sk8-multival-bind (ll tt rr bb) (boundsrect (mainmonitor system))
              (setf (boundsrect (sk8::window me)) (list ll (+ tt 20) rr bb)))))
        ))))

(define-handler highlight (UIWindowGrower)
  (eq (fillcolor me) uiHiliteBoxRenderer)
  )
(define-handler (setf highlight) (theval UIWindowGrower)
  (if theval
    (setf (fillcolor me) uiHiliteBoxRenderer)
    (setf (fillcolor me) uiZoomBoxRenderer))
  )


;;;;______________________________________________________________


(define-handler clearReferences (UISimpleWindow &key ((:objects theobjects)))
  (declare (ignore theobjects))
  nil)

(define-handler SetUpForProject (UISimpleWindow &key ((:project theproject)))
  (declare (ignore theproject))
  nil)

(define-handler updateEditor (UISimpleWindow &key (propertyname nil) (items nil))
  (declare (ignore propertyname items))
  nil)

(define-handler resized (UISimpleWindow)
  (call-next-method)
  (setboundsrect (CloseBoxRect me) 5 5 12 12)
  (setboundsrect (GrowBoxRect me) (- (width me) 12) 4 (- (width me) 5) 11)
  )

;;__________________________________________________________________
;; ACTIVATE
;; Hilites the Frame

(define-handler enteringStage (UISimpleWindow)
  (call-next-method)
  (unless (keytarget me)
    (setf (keytarget me) me)))

;;MCL3.0
(define-handler leavingStage (UISimpleWindow)
  (when (ShadeSize me)
    (process-run-function
     '(:name "LeavingStage - Shadesize" :background-p t)
     #'(lambda (me)
         (setf (minimumsize me) (cadr (ShadeSize me)))
         (setf (bottom me :resizing t) (+ (top me) (car (ShadeSize me))))
         (setf (ShadeSize me) nil)
         (moveonstage me))
     me))
  (call-next-method)
  (process-run-function
   '(:name "LeavingStage - Clearing refs" :background-p t)
   #'(lambda (me)
       (when (project (project me))  ;; this test prevents the call to clearreferences if
         (clearreferences me)))  ;; the object was disposed before the queue is run
   me))


(define-handler bringup (UISimpleWindow)
  (call-next-method)
  (when (ShadeSize me)
    (setf (minimumsize me) (cadr (ShadeSize me)))
    (setf (bottom me :resizing t) (+ (top me) (car (ShadeSize me))))
    (setf (ShadeSize me) nil)
    (moveonstage me)))


#|
	Change History (most recent last):
	2	5/27/93	false	Made it so haloes don't float on activate...
	3	6/10/93	Brian Roddy	
	4	6/14/93	Brian Roddy	
	5	6/14/93	Brian Roddy	
	11	10/27/93	rod	activate now sets the currentcommandkey
	12	11/6/93	rod	
	13	11/29/93	rod	
	14	12/3/93	rod	
	15	12/5/93	rod	
	16	12/5/93	kleiman	Now we set makefloat of selection halo rather
				than the floating property. This way if we switch
				from ui window to ui window, the halo is never
				set to floating and thus doesn't flash. -BJR
	18	1/13/94	rod	
	19	1/14/94	rod	
	20	2/21/94	hernan	The Final Renaming for Alpha! (yeah, right...)
	21	2/22/94	rod	
	22	3/3/94	Hernan	The great handler argument name renaming of 94!
	23	3/8/94	rod	
	24	3/9/94	rod	
	25	3/9/94	rod	Doing Project Switching and Reference Clearing.
	26	3/9/94	rod	Doing Project Switching and Reference Clearing.
	27	3/15/94	rod	Redoing undo...
	28	3/21/94	rod	
	29	3/26/94	rod	Minimum size stuff.
	30	3/28/94	rod	Getting rid of selection stuff for now.  As a test.
	31	4/11/94	rod	Removing reference to simple window.  No longer
				needed in the redesign of the UI.
	32	4/11/94	rod	Removing reference to simple window.  No longer
				needed in the redesign of the UI.
	33	4/11/94	Brian	
	34	4/11/94	Brian	
	35	4/11/94	Brian	
	36	4/11/94	Brian	
	37	4/11/94	Brian	
	38	4/12/94	rod	
	39	4/13/94	rod	
	40	4/13/94	Hernan	Avoiding using contents when possible.
	41	4/13/94	rod	
	42	4/13/94	rod	
	43	4/15/94	rod	
	44	4/18/94	rod	
	45	4/20/94	rod	
	46	4/20/94	Hernan	Stopped align-region from consing by using our
				new region-into-rect instead of get-record-field.
	47	4/28/94	rod	
	48	5/2/94	Hernan	RenderStyle options -> RenderOption 
				(eg. tiled -> renderTiled).
	49	5/4/94	rod	
	51	5/22/94	rod	Here begins the experimentation with a Mac look 
				to the UI.
	52	6/1/94	rod	
	53	6/10/94	Hernan	1161012: activate of uiSimpleWindow caused the
				problem of the editor not being ready for typing.
				Problem is that subversive thing that is done 
				when the cursor is over a textlist or edittext, etc.
				Why is this done? Ignoring this case fixes the
				problem.
	54	6/10/94	Hernan	1160112: now fixing this for real (after having 
				talked to Brian about it). See note in the code of
				activate of uiSimpleWindow.
	55	6/13/94	Hernan	Fixed activate to only do the subversive thing 
				when a mousedown caused the activation.
	56	6/13/94	Hernan	Fixing typo.
	57	6/14/94	rod	Added drop title to ui window titles.
	58	6/25/94	sidney	some package thing on some symbol
	59	7/6/94	Hernan	1169269: replacing the eventX variables with
				functions.
	60	7/6/94	Hernan	1169269: replacing the eventX variables with
				functions.
	61	7/13/94	rod	
	62	7/15/94	Hernan	The resizer should only kick in if it is the eventActor.
	63	7/25/94	Hernan	1175911: making actors remember their keyTarget
				when they are not on the Stage. This gets rid of
				the gs:curKey slot of windows.
	64	8/3/94	rod	Speeding up text stuff. 
				Adding "..." stuff to titles.
	65 	 8/23/94	rod     	Speeding up restore.  Only doing region 
							computations once.
	66 	 8/30/94	rod     	
	67 	 8/30/94	rod     	
	68 	 8/30/94	rod     	
	69 	 8/30/94	Brian   	
	70 	 8/30/94	Brian   	
	71 	 9/ 1/94	rod     	
	72 	 9/ 1/94	Hernan  	Qualifying some fonts.
	73 	 9/22/94	rod     	Attempting to fix problem with wierd drawing 
							behavior on the window
	74 	10/ 3/94	Hernan  	Adding the paper argument to render. (The paper
							is the Port at this point).
	75 	10/19/94	rod     	Graphic fixes.
	76 	10/19/94	rod     	Graphic fixes.
	77 	10/21/94	rod     	Setting highlight up so that the gray window style
							has the high contrast selection color for users
							who are color blind.
	78 	11/ 3/94	rod     	
	79 	11/ 3/94	rod     	
	80 	12/ 5/94	rod     	
	81 	 1/23/95	rod     	making bringup unwindowshade.
	82 	 2/ 3/95	Hernan  	WithLockedCursor is now correct. I check this file
							out and back in to force it to be recompiled.
	83 	 2/ 3/95	Hernan  	get-record-field -> rref or hrref.
	84 	 2/16/95	sidney  	readable argument names for initialize handler
	85 	 4/12/95	rod     	Making sure you can't move the window
							in the menubar.
	86 	 4/12/95	rod     	
	87 	 4/19/95	rod     	Fixing MacStyleInterface.
	88 	 4/19/95	Brian   	Fixing symbol declaration order problem.
	89 	 4/20/95	sidney  	fix crash in buildstandalone with no ui
	90 	 4/25/95	rod     	Dealing with deactivating halo in new two
							layer window model.
	3  	 8/ 9/95	Brian   	
	4  	 8/17/95	sidney  	boundsp is not enough anymore
	5  	 2/14/96	Brian   	removing gs:docontents
	2  	 4/10/96	Hernan  	define-system-handler->define-handler.
	3  	 7/25/96	sidney  	*current-event* not gs:*current-event*
	4  	11/12/96	sidney  	name some anonymous threads
	5  	12/12/96	Hernan  	Need to recompile do-sk8-window-actors.
	6  	 2/27/97	Hernan  	
|# ;(do not edit past this line!!)
