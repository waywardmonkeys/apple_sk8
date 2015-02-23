;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories


;;;
(in-package :SK8Development)

(provide "GETSHAPEFROMUSER")

(require "OVAL" "objects;Shapes:Oval")
(require "ROUNDRECT" "objects;Shapes:RoundRect")
(require "LINESEGMENT" "objects;Shapes:LineSegment")
(require "POLYGON" "objects;Shapes:Polygon")

;;______________________________________________________________________
;; cartesionToPolar
;; Given a point and an origin, returns an R and theta.

(defun cartesionToPolar (x y ox oy)
  (SK8-multivals (sqrt (+ (expt (- y oy) 2) (expt (- x ox) 2)))
                 (atan (- y oy) (- x ox))))


;;____________________________________________________________________________________________
;;The Basic Draw functions for drawing rects ovals and roundrects with the option to draw multiple
;;____________________________________________________________________________________________


(defun DrawScaledShapes (ll tt rr bb TheRect Shape Rows Cols Hspacing Vspacing)
  (let (wid 
        hei 
        (curhh ll)  
        (curvv tt)
        (totalwidth (- rr ll))
        (totalheight (- bb tt)))
    (without-interrupts
     (with-port (gs:get-wmgr-port)
       (#_PenMode #$PatXor)
       (#_PenPat *gray-pattern*)
       (setf wid (- (round (+ totalwidth hspacing) cols) hspacing))
       (setf hei (- (round (+ totalheight Vspacing) Rows) Vspacing))
       (dotimes (r rows)
         (dotimes (c cols)
           (#_setRect TheRect curhh curvv (+ wid curhh) (+ hei curvv))
           (case shape
             (:Rect (#_FrameRect TheRect))
             (:oval (#_FrameOval TheRect))
             (:rrect (#_FrameRoundRect TheRect 16 16)))
           (incf curhh (+ wid hspacing)))
         (setf curhh ll)
         (incf curvv (+ hei Vspacing)))))
    (sk8-multivals hei wid )))

(defun DrawRevealedShapes (ll tt rr bb TheRect Shape itemHeight itemWidth Hspacing Vspacing startH startV)
  (let* ((HDirection (= startH ll))
         (VDirection (= startV tt))
         rows
         cols
         tempH tempV
         (curhh startH)
         (curvv startV)
         (totalwidth (- rr ll))
         (totalheight (- bb tt)))
    (without-interrupts
     (with-port (gs:get-wmgr-port)
       (#_PenMode #$PatXor)
       (#_PenPat *gray-pattern*)
       (setf cols (max 1 (round (+ totalwidth hspacing) (+ itemwidth hspacing))))
       (setf rows (max 1 (round (+ totalheight Vspacing) (+ itemHeight Vspacing))))
       (dotimes (r rows)
         (dotimes (c cols)
           (setf tempH (if Hdirection (+ curhh itemWidth) (- curhh itemWidth)))
           (setf tempV (if Vdirection (+ curvv itemHeight) (- curvv itemHeight)))
           (#_setRect TheRect (min tempH curhh) (min tempV curvv) (max tempH curhh) (max tempV curvv) )
           (case shape
             (:Rect (#_FrameRect TheRect))
             (:oval (#_FrameOval TheRect))
             (:rrect (#_FrameRoundRect TheRect 16 16)))
           (if HDirection 
             (incf curhh (+ itemWidth hspacing))
             (decf curhh (+ itemWidth hspacing))))
         (setf curhh startH)
         (if Vdirection
           (incf curvv (+ itemHeight Vspacing))
           (decf curvv (+ itemHeight Vspacing))))))
    (sk8-multivals rows cols)))

(define-sk8-function getRectShapeFromUser nil (startH startV &key 
                                                          (shape :rect)
                                                          (centerOut nil)
                                                          (shiftConstraint t)
                                                          (MultipleDraw nil)
                                                          (hspacing 4)
                                                          (vspacing 4))
  (if centerout (setf multipledraw nil))
  (setq startH (round startH)
        startV (round startV))
  (let ((endH startH)
        (endV startV)
        ll tt rr bb
        oldll oldtt oldrr oldbb
        (oldH startH)
        (OldV startV)
        itemHeight itemWidth
        (lastdrawwasoption nil)
        (rows 1) (cols 1)
        dx dy)
    (without-interrupts
     (rlet ((tRect :Rect))
       ;; draw it
       (with-port (gs:get-wmgr-port)
         (#_SetClip (rref (gs:get-wmgr-port) :cGrafPort.visRgn)))
       (sk8-multival-setf (itemHeight itemWidth)
                          (DrawScaledShapes startH startV endH endV trect shape rows cols hspacing vspacing))
       (setf oldll startH oldtt startV oldrr endH oldbb endV)
       ;; loop to drag it out
       (loop
         (unless (down mouse) (return))
         (sleep (/ 60))
         (SK8-multival-setf (endH endV) (mouseLoc stage))
         ;; changed?
         (when (or (/= endH oldH) (/= endV oldV))
           ;; compute the new shape
           (setf oldh endh oldv endv)
           (Setq dy (abs (- endV startV))
                 dx (abs (- endH startH)))
           ;; constrain to square?
           (when (and (shiftKeyDown) shiftConstraint)
             (if (< dx dy)
               (setq endH (if (< endH startH) (- startH dx) (+ startH dx))
                     endV (if (< endV startV) (- startV dx) (+ startV dx))
                     dy dx)
               (setq endH (if (< endH startH) (- startH dy) (+ startH dy))
                     endV (if (< endV startV) (- startV dy) (+ startV dy))
                     dx dy)))
           ;; Calculate the boundsrect to draw within
           (if centerOut
             (setf ll (- startH dx)
                   tt (- startV dy)
                   rr (+ startH dx)
                   bb (+ startV dy))
             (setf ll (min startH endH) 
                   tt (min startV endV) 
                   rr (max startH endH)
                   bb (max startV endV)))
           (setq oldH endH oldV endV)
           ;; draw the new Rect, erase the old
           (if lastdrawwasoption
             (DrawRevealedShapes oldll oldtt oldrr oldbb trect shape itemheight itemwidth hspacing vspacing startH startV)
             (DrawScaledShapes oldll oldtt oldrr oldbb trect shape rows cols hspacing vspacing))
           (if (and MultipleDraw (optionkeydown) (> itemheight vspacing) (> itemwidth hspacing))
             (progn
               (setf lastdrawwasoption t)
               (sk8-multival-setf (rows cols)
                                  (DrawRevealedShapes ll tt rr bb trect shape itemheight itemwidth hspacing vspacing startH startV)))
             (progn
               (setf lastdrawwasoption nil)
               (sk8-multival-setf (itemHeight itemWidth)
                                  (DrawScaledShapes ll tt rr bb trect shape rows cols hspacing vspacing))))
           (setf oldll ll oldtt tt oldrr rr oldbb bb)))
       ;; erase it
       (if lastdrawwasoption
         (DrawRevealedShapes oldll oldtt oldrr oldbb trect shape itemHeight itemWidth hspacing vspacing startH startV)
         (DrawScaledShapes oldll oldtt oldrr oldbb trect shape rows cols hspacing vspacing))
       (#_FlushEvents #$everyEvent 0)))
    ;; return it
    (if MultipleDraw
      (let ((HDirection (= startH oldll))
            (VDirection (= startV oldtt))
            vallist (curhh startH) (curvv startV)
            tempH tempV
            (totalwidth (- oldrr oldll))
            (totalheight (- oldbb oldtt)))
        (unless lastdrawwasoption
          (setf itemWidth (- (round (+ totalwidth hspacing) cols) hspacing))
          (setf itemHeight (- (round (+ totalheight Vspacing) Rows) Vspacing)))
        (dotimes (r rows)
          (dotimes (c cols)
            (setf tempH (if Hdirection (+ curhh itemWidth) (- curhh itemWidth)))
            (setf tempV (if Vdirection (+ curvv itemHeight) (- curvv itemHeight)))
            (push (list (min tempH curhh) (min tempV curvv) (max tempH curhh) (max tempV curvv)) vallist)
            (if HDirection 
              (incf curhh (+ itemWidth hspacing))
              (decf curhh (+ itemWidth hspacing))))
          (setf curhh startH)
          (if Vdirection
            (incf curvv (+ itemHeight Vspacing))
            (decf curvv (+ itemHeight Vspacing))))
        (if (eql (length vallist) 1) (setf vallist (car vallist)))
        vallist)
      (SK8-multivals oldll oldtt oldrr oldbb))
    ))

;;____________________________________________________________________________________________
;;____________________________________________________________________________________________
;;Here's the User's API
;;____________________________________________________________________________________________
;;____________________________________________________________________________________________


(define-handler GetShapeFromUser (Actor &key 
                                                  (startH nil)
                                                  (startV nil)
                                                  (style 'CornerToCorner)
                                                  (ShiftConstraint t)
                                                  (MultipleDraw nil)
                                                  (BoundsRectSet nil))
  (if (or (not startH) (not startV))
    (sk8-multival-bind (xx yy) (mouseloc stage)
      (setf startH xx)
      (setf startV yy)))
  (let ((bounds (GetRectShapeFromUser startH startV :shape :rect :centerOut (eq Style 'CenterOut) 
                                      :ShiftConstraint ShiftConstraint
                                      :MultipleDraw MultipleDraw)))
    (if (and (not multipleDraw) BoundsRectSet)
      (setf (boundsrect me :physical t) bounds)
      bounds)))

(define-handler GetShapeFromUser (Rectangle &key
                                                      (startH nil)
                                                      (startV nil)
                                                      (style 'CornerToCorner)
                                                      (shiftConstraint t)
                                                      (MultipleDraw nil)
                                                      (BoundsRectSet nil))
  (if (or (not startH) (not startV))
    (sk8-multival-bind (xx yy) (mouseloc stage)
      (setf startH xx)
      (setf startV yy)))
  (let ((bounds (GetRectShapeFromUser startH startV :shape :rect :centerOut (eq Style 'CenterOut) :shiftConstraint shiftConstraint
                                      :MultipleDraw MultipleDraw)))
    (if (and (not multipleDraw) BoundsRectSet)
      (setf (boundsrect me :physical t) bounds)
      bounds)))

(define-handler GetShapeFromUser (oval &key
                                                 (startH nil)
                                                 (startV nil)
                                                 (style 'CornerToCorner)
                                                 (MultipleDraw nil)
                                                 (shiftConstraint t)
                                                 (BoundsRectSet nil))
  (if (or (not startH) (not startV))
    (sk8-multival-bind (xx yy) (mouseloc stage)
      (setf startH xx)
      (setf startV yy)))
  (let ((bounds (GetRectShapeFromUser startH startV :shape :oval :centerOut (eq Style 'CenterOut) :shiftConstraint shiftConstraint
                                      :MultipleDraw MultipleDraw)))
    (if (and (not multipleDraw) BoundsRectSet)
      (setf (boundsrect me :physical t) bounds)
      bounds)))
    
(define-handler GetShapeFromUser (RoundRect &key
                                                      (startH nil)
                                                      (startV nil)
                                                      (style 'CornerToCorner)
                                                      (MultipleDraw nil)
                                                      (shiftConstraint t)
                                                      (BoundsRectSet nil))
  (if (or (not startH) (not startV))
    (sk8-multival-bind (xx yy) (mouseloc stage)
      (setf startH xx)
      (setf startV yy)))
  (let ((bounds  (GetRectShapeFromUser startH startV :shape :rrect :centerOut (eq Style 'CenterOut) :shiftConstraint shiftConstraint
                                       :MultipleDraw MultipleDraw)))
    (if (and (not multipleDraw) BoundsRectSet)
      (setf (boundsrect me :physical t) bounds)
      bounds)))


(defun getLineFromUser (startX startY)
  (without-interrupts
   (Setq startX (round startX)
         startY (round startY))
   (let* ((endX startX)
          (endY startY)
          (oldX startX)
          (oldY startY))
     ;; draw it
     (with-port (gs:get-wmgr-port)
       (#_SetClip (rref (gs:get-wmgr-port) :cGrafPort.visRgn))
       (#_PenMode #$PatXor)
       (#_PenPat *gray-pattern*)
       (#_MoveTo startX startY)
       (#_LineTo endX endY))
     ;; loop to drag it out
     (loop
       ;; return if the mouse is up
       (when (not (mouse-down-p))
         (return))
       (sleep (/ 60))
       ;; get the new mouseLoc
       (SK8-multival-setf (endX endY) (mouseLoc stage))
       ;; constrain to straight line
       ;; changed?
       (when (or (/= endX oldX)
                 (/= endY oldY))
         (with-port (gs:get-wmgr-port)
           (#_PenMode #$PatXor)
           (#_PenPat *gray-pattern*)
           (#_MoveTo startX startY)
           (#_LineTo oldX oldY)
           (#_MoveTo startX startY)
           (#_LineTo endX endY))
         (setq oldX endX
               oldY endY)))
     ;; erase it
     (with-port (gs:get-wmgr-port)
       (#_PenMode #$PatXor)
       (#_PenPat *gray-pattern*)
       (#_MoveTo startX startY)
       (#_LineTo endX endY))
     ;; return it
     (#_FlushEvents #$everyEvent 0)
     (sk8-multiVals startX startY endX endY))))


(define-handler GetShapeFromUser (LineSegment &key
                                                        (startH nil)
                                                        (startV nil)
                                                        (MultipleDraw nil)
                                                        (BoundsRectSet nil))
  (declare (ignore MultipleDraw))
  (if (or (not startH) (not startV))
    (sk8-multival-bind (xx yy) (mouseloc stage)
      (setf startH xx)
      (setf startV yy)))
  (sk8-multival-bind (startX startY endX endY)
                     (getLineFromUser startH startV)
    (when BoundsRectSet
      (setEndPoints me startX startY endX endY :physical t))
    (sk8-multivals startX startY endX endY)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;  THE BIG ONE: POLYGONS!!!!
;;;;;;;;;;;;;;;;  
;;;;;;;;;;;;;;;;  PointToPoint, Regular, SymmetricalRegular, FreeHand
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun getFreehandFromUser (startX startY)
  (without-interrupts 
   (setq startX (round startX)
         startY (round startY))
   (let ((endX startX)
         (endY startY)
         (oldX startX)
         (oldY startY)
         (pList (list startX startY))
         r1 r2 a1 a22 oldPList)
     ;; draw it
     (with-port (gs:get-wmgr-port)
       (#_SetClip (rref (gs:get-wmgr-port) :cGrafPort.visRgn))
       (#_PenMode #$PatXor)
       (#_PenPat *black-pattern*)
       (#_MoveTo startX startY))
     ;; loop to drag it out
     (loop
       ;; return if the mouse is up
       (when (not (mouse-down-p))
         (return))
       (sleep (/ 60))
       ;; get the new mouseLoc
       (SK8-multival-setf (endX endY) (mouseLoc stage))
       ;; changed?
       (when (or (/= endX oldX)
                 (/= endY oldY))
         (if (and (optionKeyDown) (> (length plist) 2))
           ;; rotate
           (progn
             (sk8-multival-setf (r1 a1) 
                                (cartesionToPolar oldX oldY startX startY))
             (sk8-multival-setf (r2 a22) 
                                (cartesionToPolar endX endY startX startY))
             (when (and (not (zerop r1)) (not (zerop r2)))
               (setq oldPList plist
                     pList (rotatePointList (list oldX oldY) startX startY
                                            (- a22 a1) :Scale (/ r2 r1))
                     endX (car pList)
                     endY (cadr pList)
                     pList (rotatePointList oldPList startX startY
                                            (- a22 a1) :Scale (/ r2 r1)))
               (drawPointList (append pList nil) nil)
               (drawPointList (append oldpList nil) nil)
               (Setq oldX endX
                     oldY endY)))
           ;; normal  *This could be sped up by only drawing the last point somehow...
           (progn
             (setq oldX endX 
                   oldY endY)
             (drawPointList plist nil)
             (setq pList (nCOnc plist (cons endX (cons endY nil))))
             (drawPointList plist nil)
             ))))
     ;; erase it
     (drawPointList pList nil)
     ;; return it
     (#_FlushEvents #$everyEvent 0)
     pList)))

(defun makePolyPointsList (sx sy ex ey numSides)
  (sk8-multival-bind (radius theoffset) (cartesionToPolar sx sy ex ey)
    (let* ((dTheta (/ gs:2pi numSides))
           (theta (+ dTheta pi theoffset))
           (points nil))
      
      ;; Compute the new points
      (dotimes (j numSides)
        (push (round (gs:f+ sy (round (* radius (sin theta))))) points)
        (push (round (gs:f+ sx (round (* radius (cos theta))))) points)
        (setq theta (+ theta dtheta)))
      ;; return them
      points)))

(defun drawPointList (plist &optional (closed t))
  (with-port (gs:get-wmgr-port)
    (#_SetClip (rref (gs:get-wmgr-port) :cGrafPort.visRgn))
    (#_PenMode #$PatXor)
    (#_PenPat *black-pattern*)
    (#_MoveTo (round (car pList)) (round (cadr pList)))
    (doTimes (i (1- (/ (length pList) 2)))
      (#_LineTo (round (nth (* 2 (1+ i)) plist))
       (round (nth (1+ (* 2 (1+ i))) plist))))
    (if closed
      (#_LineTo (round (car pList)) (round (cadr pList))))
    (#_PenNormal))
    )

(defun sidesFromKeys (oldSides &optional (allow12 nil))
  (sk8::getkeys)
  
  (cond ((sk8::%getbit mf::*keyMap* 18)
         (if allow12 1 11))
        ((sk8::%getbit mf::*keyMap* 19)
         (if allow12 2 12))
        ((sk8::%getbit mf::*keyMap* 20)
         3)
        ((sk8::%getbit mf::*keyMap* 21)
         4)
        ((sk8::%getbit mf::*keyMap* 23)
         5)
        ((sk8::%getbit mf::*keyMap* 22)
         6)
        ((sk8::%getbit mf::*keyMap* 26)
         7)
        ((sk8::%getbit mf::*keyMap* 28)
         8)
        ((sk8::%getbit mf::*keyMap* 25)
         9)
        ((sk8::%getbit mf::*keyMap* 29)
         10)
        ((or (sk8::%getbit mf::*keyMap* 27)
             (sk8::%getbit mf::*keyMap* 78))
         (sleep (/ 10))
         (max 3 (1- oldSides)))
        ((or (sk8::%getbit mf::*keyMap* 24)
             (sk8::%getbit mf::*keyMap* 69))
         (sleep (/ 10))
         (1+ oldSides))
        (t oldSides)))

(defun getRegularPolygonFromUser (startX startY &optional (numSides 5))
  (without-interrupts
   (setq startX (round startX)
         startY (round startY))
   (let ((endX startX)
         (endY startY)
         (oldX startX)
         (oldY startY)
         (oldSides numSides)
         oldPList pList)
     ;; compute the pList
     (setq pList (makePolyPointsList startX startY endX endY numSides))
     (setq oldPList pList)
     ;; draw it
     (drawPointList pList)
     ;; loop to drag it out
     (loop
       ;; return if the mouse is up
       (when (not (mouse-down-p))
         (return))
       (sleep (/ 60))
       ;; get the new mouseLoc
       (SK8-multival-setf (endX endY) (mouseLoc stage))
       (setq numSides (sidesFromKeys numSides))
       ;; changed?
       (when (or (/= endX oldX)
                 (/= endY oldY)
                 (/= numSides oldSides))
         (setq oldX endX 
               oldY endY
               oldSides numSides
               oldPList pList
               pList (makePolyPointsList startX startY endX endY numSides))
           (drawPointList pList)
           (drawPointList oldpList)))
     ;; erase it
     (drawPointList pList)
     ;; return it
     (#_FlushEvents #$everyEvent 0)
     (if (and (= startx endx) (= starty endy)) 
       (list startx starty)
       pList))))

(defun makeSimiPointsList (cx cy thePointList numSides)
  (let ((dTheta (/ gs:2pi numSides))
        theta points theOffset Radius) 
    ;; Compute the new points
    (dotimes (j numSides)
      (doTimes (i (/ (length thePointList) 2))
        (sk8-multival-setf (radius theOffset) 
                           (cartesionToPolar cx cy (nth (* 2 i) thePointList)
                                                  (nth (1+ (* 2 i)) thePointList)))
        (setq theta (+ (* (1+ j) dTheta) pi theoffset))
        (push (round (gs:f+ cy (round (* radius (sin theta))))) points)
        (push (round (gs:f+ cx (round (* radius (cos theta))))) points)))
    ;; return them
    points))


(defun rotatePointList (thePointList cx cy theAngle &key (scale 1))
  (let (theta points Radius) 
    ;; Compute the new points
    (doTimes (i (/ (length thePointList) 2))
      (sk8-multival-setf (radius theta) 
                         (cartesionToPolar (nth (* 2 i) thePointList)
                                                (nth (1+ (* 2 i)) thePointList) cx cy))
      (setq radius (* scale radius))
      (setq points (nConc points
                          (list 
                           (gs:f+ cx (* (cos (+ theAngle theta)) radius))
                           (gs:f+ cy (* (sin (+ theAngle theta)) radius))))))
    ;; return them
    points))

(defun getSymegonFromUser (startX startY &optional (numSides 5))
  (without-interrupts
   (setq startX (round startX)
         startY (round startY))
   (let* ((endX startX)
          (endY startY)
          (oldX startX)
          (oldY startY)
          (oldSides numSides)
          (thePointList (list endX endY))
          (numPoints 0)
          (waitingForMouseDown nil)
          (lastTime (#_TickCount))
          (lastX startX)
          (lastY startY)
          nextPoint oldPList pList r1 r2 a1 a22)
     ;; compute the pList
     (setq pList (makeSimiPointsList startX startY thePointList numSides))
     (setq oldPList pList)
     ;; draw it
     (drawPointList pList)
     ;; loop to drag it out
     (loop
       (setq nextPoint
             (catch :nextPoint
               ;; loop to track the mouse
               (loop
                 ;; are we waiting for mouseDown? (ie, was the mouse up?)
                 (if waitingForMouseDown
                   ;; is the mouse up?
                   (when (mouse-down-p)
                     ;; is it a double click?
                     (if (and (double-click-spacing-p (make-point (round lastX) (round lastY))
                                                      (make-point (round endX) (round endY)))
                              (< (- (#_TickCount) lastTime) (#_GetDblTime)))
                       ;; it is - return from the loop
                       (throw :nextPoint nil)
                       ;; it is not - on to the next point
                       (throw :nextPoint t)))
                   ;; we are waiting for mouseUp (ie, the mouse is already down)
                   (when (not (mouse-down-p))
                     ;; the mouse is up
                     (setq waitingForMouseDown t)))
                 ;; track the mouse
                 (sleep (/ 60))
                 (setq lastTime (#_TickCount))
                 ;; get the new mouseLoc
                 (SK8-multival-setf (endX endY) (mouseLoc stage))
                 (setq numSides (sidesFromKeys numSides t))
                 ;; changed?
                 (when (or (/= endX oldX)
                           (/= endY oldY)
                           (/= numSides oldSides))
                   (if (and (optionKeyDown) (> numPoints 0))
                     (progn
                       (sk8-multival-setf (r1 a1) 
                                          (cartesionToPolar oldX oldY startX startY))
                       (sk8-multival-setf (r2 a22) 
                                          (cartesionToPolar endX endY startX startY))
                       (setf oldplist plist)
                       (when (and (not (zerop r1)) (not (zerop r2)))
                         (setq thePointList (rotatePointList thePointList startX startY 
                                                          (- a22 a1) :scale (/ r2 r1))
                               oldX endX
                               oldY endY
                               pList (makeSimiPointsList startX startY thePointList numSides))))
                     (progn
                       (setf oldX endX 
                             oldY endY
                             oldSides numSides
                             oldPList pList
                             (nth (* 2 numPoints) thePointList) endX
                             (nth (1+ (* 2 numPoints)) thePointList) endY
                             pList (makeSimiPointsList startX startY thePointList numSides))))
                   (drawPointList pList)
                   (drawPointList oldpList)))))
       (if nextPoint
         (setq thePointList (nCOnc thePointList (cons endX (cons endY nil)))
               numPoints (1+ numPoints)
               lastX endX
               lastY endY
               waitingForMouseDown nil)
         (return)))
     ;; erase it
     (drawPointList pList)
     ;; return it
     (#_FlushEvents #$everyEvent 0)
     (if (and (= startx endx) (= starty endy)) 
       (list startx starty)
       pList)
     )))

(defun getPolygonFromUser (startX startY)
  (without-interrupts   ;;;; so GC doesn't interrupt us and cause a hiccup leaving xor lines behind...
   (setq startX (round startX)
         startY (round startY))
   (let ((endX startX)
         (endY startY)
         (oldX startX)
         (oldY startY)
         (lastX startX)
         (lastY startY)
         (pList (list startX startY))
         (waitingForMouseDown nil)
         (lastTime (#_TickCount))
         nextPoint oldPList r1 r2 a1 a22)
     ;; draw it
     (with-port (gs:get-wmgr-port)
       (#_SetClip (rref (gs:get-wmgr-port) :cGrafPort.visRgn))
       (#_PenMode #$PatXor)
       (#_PenPat *black-pattern*)
       (#_MoveTo startX startY)
       (#_LineTo startX startY))
     ;; loop to drag it out
     (loop
       (setq nextPoint
             (catch :nextPoint
               ;; loop to track the mouse
               (loop
                 ;; are we waiting for mouseDown? (ie, was the mouse up?)
                 (if waitingForMouseDown
                   ;; is the mouse up?
                   (when (mouse-down-p)
                     ;; is it a double click?
                     (if (and (double-click-spacing-p (make-point (round lastX) (round lastY))
                                                      (make-point (round endX) (round endY)))
                              (< (- (#_TickCount) lastTime) (#_GetDblTime)))
                       ;; it is - return from the loop
                       (throw :nextPoint nil)
                       ;; it is not - on to the next point
                       (throw :nextPoint t)))
                   ;; we are waiting for mouseUp (ie, the mouse is already down)
                   (when (not (mouse-down-p))
                     ;; the mouse is up
                     (setq waitingForMouseDown t)))
                 ;; track the mouse
                 (sleep (/ 60))
                 (setq lastTime (#_TickCount))
                 ;; get the new mouseLoc
                 (SK8-multival-setf (endX endY) (mouseLoc stage))
                 (when (or (/= endX oldX)
                           (/= endY oldY))
                   ;; we've moved - update the line
                   (if (and (optionKeyDown) (> (length plist) 2))
                     (progn
                       (sk8-multival-setf (r1 a1) 
                                          (cartesionToPolar oldX oldY startX startY))
                       (sk8-multival-setf (r2 a22) 
                                          (cartesionToPolar endX endY startX startY))
                       (when (and (not (zerop r1)) (not (zerop r2)))
                         (setq oldPList pList
                               pList (rotatePointList (list oldX oldY) startX startY 
                                                      (- a22 a1) :scale (/ r2 r1))
                               endX (car pList)
                               endY (cadr pList)
                               pList (rotatePointList oldPList startX startY 
                                                      (- a22 a1) :scale (/ r2 r1))
                               lastX (nth (* 2 (1- (/ (length pList) 2))) pList)
                               lasty (nth (1+ (* 2 (1- (/ (length pList) 2)))) pList))
                         (drawPointList (append oldpList (list (round oldX) (round oldY))) nil)
                         (drawPointList (append pList (list (round endX) (round endY))) nil)
                         (Setq oldX endX
                               oldY endY)))
                     (progn
                       (with-port (gs:get-wmgr-port)
                         (#_PenMode #$PatXor)
                         (#_PenPat *black-pattern*)
                         (#_moveTo (round lastx) (round lasty))
                         (#_LineTo (round oldX) (round oldY))
                         (#_moveTo (round lastx) (round lasty))
                         (#_LineTo (round endX) (round endY))
                         )
                       (setq oldX endX 
                             oldY endY)))))))
       (if nextPoint
         (progn
           (drawPointList (append plist (list (round oldX) (round oldY))) nil)
           (setq pList (nConc plist (cons endX (cons endY nil)))
                 lastX endX
                 lastY endY
                 waitingForMouseDown nil)
           (drawPointList (append pList (list (round endX) (round endY))) nil))
         (return)))
     ;; OK, we have all of the points
     ;; erase it
     (drawPointList pList nil)
     (drawPointList (append (last plist 2) (last plist 2)) nil)  ;;; This gets rid of that last tiny dot that is sometimes there...
     ;; get rid of extra events
     (#_FlushEvents #$everyEvent 0)
     ;; return it
     pList)))


(define-handler GetShapeFromUser (polygon &key
                                                    (startH nil)
                                                    (startV nil)
                                                    (style 'Points)
                                                    (MultipleDraw nil)
                                                    (BoundsRectSet nil))
  (declare (ignore MultipleDraw))
  (let (plist)
    (if (or (not startH) (not startV))
      (sk8-multival-bind (xx yy) (mouseloc stage)
        (setf startH xx)
        (setf startV yy)))
    (setf plist (cond
                 ((eq style 'Points) (getPolygonFromUser startH startV))
                 ((eq style 'Regular) (getRegularPolygonFromUser startH startV 3))
                 ((eq style 'FreeHand) (getFreeHandFromUser startH startV))
                 ((eq style 'symmetrical) (getSymegonFromUser startH startV 3))
                 ((eq style 'Symegon) (getSymegonFromUser startH startV 3))
                 (t (sk8-error GeneralProgrammaticError
                               :strings '("Style must be one of: 'Points', 'Regular', 'FreeHand', 'symmetrical', or 'Symegon'.")
                               ))))
    (if BoundsRectSet
      (setf (points me :physical t) pList))
    plist))

#|
	Change History (most recent last):
	1	6/1/93	Brian Roddy	This is the generic getshapefromuser handlers.  Have to redo the draw tools to make use of these great guys!!!
	2	6/3/93	Brian Roddy	
	3	6/4/93	Brian Roddy	Added get circle from user to getshape of oval...
	4	6/10/93	Brian Roddy	
	10	10/8/93	rod	
	11	10/15/93	rod	
	12	10/19/93	rod	Flushed events and wrapped all in without interrupts
	14	3/3/94	Hernan	The great handler argument name renaming of 94!
	15	3/10/94	rod	
	16	3/10/94	rod	
	17	3/18/94	rod	Finally fixed the standard polygon draw function.
	18	3/18/94	rod	OK, this should really fix all those stupid 
				bugs.  I simplified the polygon code and made
				sure all the stray dots went away.
	19	3/18/94	rod	whoops, now it is really fixed.  now i make sure 
				that the polygon option modifier only works
				when there is more than 1 set of points.
	20	6/27/94	Hernan	1170049: Keywords turning into symbols for
				options to the style argument of getShapeFromUser.
	21	7/15/94	rod	Slowing down the + and - keys while drawing.
	22	7/17/94	rod	
	23	8/2/94	rod	1178335:  making extended plus or minus keys
				to work.
	24 	12/ 3/94	rod     	error message fixes.
	25 	12/ 7/94	dy      	pointList arg -> thePointList
	26 	12/ 8/94	dy      	more of same
	2  	 2/14/96	Brian   	removing dependencies on other files
	2  	 4/10/96	Hernan  	define-system-handler->define-handler.
	3  	 4/19/96	Hernan  	Putting define-sk8-function back in the code.
	4  	 2/20/97	Hernan  	Sanitizing comments...
	5  	 2/27/97	Hernan  	
|# ;(do not edit past this line!!)
