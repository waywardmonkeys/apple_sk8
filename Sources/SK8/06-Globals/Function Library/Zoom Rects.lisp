;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories


;;;

(in-package :SK8Development)

(provide "ZOOMRECTS")

;; ZOOM RECT SUPPORT

;;; Adding a duration keyword. The duration is specified in seconds. Note that the
;;; animation will take the specified number of frames and will be guaranteed to take
;;; no fewer seconds than what is specified in the duration arg.

;;; We use the internal-real-time.

(define-sk8-function zoomRect nil (fromLeft fromTop fromRight fromBottom
                                              toLeft toTop toRight toBottom
                                              &key (NumberOfFrames 30) duration)
  (let ((leftDelta (/ (- toLeft fromLeft) NumberOfFrames))
        (rightDelta (/ (- toRight fromRight) NumberOfFrames))
        (topDelta (/ (- toTop fromTop) NumberOfFrames))
        (bottomDelta (/ (- toBottom fromBottom) NumberOfFrames))
        (wmgr-port (gs:get-wmgr-port))
        currentTime newTime elapsedMilSecs milSecsPerFrame)
    (rlet ((rect1 :rect :top (round fromTop) :left (round fromLeft)
                  :right (round fromRight) :bottom (round fromBottom))
           (rect2 :rect :topLeft #@(-3000 -3000) :bottomRight #@(3000 3000))
           (rect3 :rect :topLeft #@(-3000 -3000) :bottomRight #@(3000 3000)))
      (without-interrupts
       (unwind-protect
         (progn
           (with-port wmgr-port
             (#_ClipRect rect3)
             (#_PenPat *gray-pattern*)
             (#_PenMode #$PatXor)
             (#_FrameRect rect1))
           (copy-record rect1 :rect rect2)
           ;; Set up time vars.
           (when duration
             (setf milSecsPerFrame (/ (* duration 1000) numberOfFrames))
             (setf currentTime (get-internal-real-time)))
           ;; cycle over
           (doTimes (i NumberOfFrames)
             ;; inc the bounds
             (setq fromLeft (+ fromLeft leftDelta)
                   fromTop (+ fromTop topDelta)
                   fromRight (+ fromRight rightDelta)
                   fromBottom (+ fromBottom bottomDelta))
             ;; set the rect
             (rset rect1 :rect.top (round fromTop))
             (rset rect1 :rect.left (round fromLeft))
             (rset rect1 :rect.bottom (round fromBottom))
             (rset rect1 :rect.right (round fromRight))
             ;; redraw
             (with-port wmgr-port
               (#_ClipRect rect3)
               (#_PenPat *gray-pattern*)
               (#_PenMode #$PatXor)
               (#_FrameRect rect1)
               (#_FrameRect rect2))
             ;; copy the rect so we can erase
             (copy-record rect1 :rect rect2)
             ;; If this step took less than the time we have per frame, wait
             ;; until time is up.
             (when duration
               (setf newTime (get-internal-real-time))
               (setf elapsedMilSecs (- newTime currenttime))
               (when (< elapsedMilSecs milSecsPerFrame)
                 (sleep (/ (- milSecsPerFrame elapsedMilSecs) 1000)))
               (setf currentTime newTime))))
         ;; erase the last one
         (with-port wmgr-port
           (#_ClipRect rect3)
           (#_PenPat *gray-pattern*)
           (#_PenMode #$PatXor)
           (#_FrameRect rect1)
           (#_PenPat *black-pattern*)
           (#_PenMode #$PatCopy)))))))

(defmacro withZoomingBounds (theActor &body body)
  (let ((fll (gensym))
        (ftt (gensym))
        (frr (gensym))
        (fbb (gensym))
        (tll (gensym))
        (ttt (gensym))
        (trr (gensym))
        (tbb (gensym)))
  `(withActorLocked (,theActor)
     (SK8-multival-bind (,fll ,ftt ,frr ,fbb)
                          (boundsRect ,theActor :physical t)
       ,@body
       (SK8-multival-bind (,tll ,ttt ,trr ,tbb)
                            (boundsRect ,theActor :physical t)
         (zoomRect ,fll ,ftt ,frr ,fbb
                   ,tll ,ttt ,trr ,tbb))))))


(define-sk8-function zoomBoundsRect nil (&key from to (numberOfFrames 30) duration)
  (sk8-multival-bind (ll tt rr bb) (boundsRect from :physical t)
    (SK8-multival-bind (lll ttt rrr bbb) (boundsRect to :physical t)
      (zoomRect ll tt rr bb lll ttt rrr bbb :NumberOfFrames numberofframes
                :duration duration))))


;;;______________________________________________________________________________________
(define-sk8-function DrawXORRectangle nil (ll tt rr bb &key (grayline nil))
  (setq ll (gs:f.round ll)
        tt (gs:f.round tt)
        rr (gs:f.round rr)
        bb (gs:f.round bb)
        )
  (gs:let+ ((qd-rect (:rect)))
    (with-port (gs:get-wmgr-port)
      (#_SetClip (rref (gs:get-wmgr-port) :cGrafPort.visRgn))
      (#_PenMode #$PatXor)
      (#_PenPat (if grayline
                  *gray-pattern*
                  *black-pattern*))
      (rset qd-rect :rect.right rr)
      (rset qd-rect :rect.bottom bb)
      (rset qd-rect :rect.left ll)
      (rset qd-rect :rect.top tt)
      (#_PaintRect qd-rect)
      )))

(define-sk8-function DrawXORLine nil (startX startY endx endy &key (size '(3 3)) (grayline nil))
  (setq startX (gs:f.round startX)
        startY (gs:f.round startY)
        endx (gs:f.round endx)
        endy (gs:f.round endy)
        )
  (with-port (gs:get-wmgr-port)
    (#_SetClip (rref (gs:get-wmgr-port) :cGrafPort.visRgn))
    (#_PenMode #$PatXor)
    (#_PenSize (car size) (cadr size))
    (#_PenPat (if grayline
                *gray-pattern*
                *black-pattern*))
    (#_MoveTo startX startY)
    (#_LineTo endX endY)))

(define-sk8-function DrawXORFrame nil (ll tt rr bb &key (pensize '(2 2)) (grayline nil))
  (setq ll (gs:f.round ll)
        tt (gs:f.round tt)
        rr (gs:f.round rr)
        bb (gs:f.round bb)
        )
  (gs:let+ ((qd-rect (:rect)))
    (with-port (gs:get-wmgr-port)
      (#_SetClip (rref (gs:get-wmgr-port) :cGrafPort.visRgn))
      (#_PenMode #$PatXor)
      (#_pensize (car pensize) (cadr pensize))
      (#_PenPat (if grayline
                  *gray-pattern*
                  *black-pattern*))
      (rset qd-rect :rect.right rr)
      (rset qd-rect :rect.bottom bb)
      (rset qd-rect :rect.left ll)
      (rset qd-rect :rect.top tt)
      (#_FrameRect qd-rect)
      (#_PenNormal)
      )))


#|
	Change History (most recent last):
	7	3/28/94	Hernan	Added a duration option to zoomBoundsRect and
				zoomRects which constraints the length of the
				animation.
	8	3/28/94	Hernan	Doing the work in zoomRect without interrupts. 
				This should prevent the port from being changed
				under us leaving garbage on the screen.
	2  	 2/15/96	Brian   	Adding DrawXOR functions.
	2  	 4/10/96	Hernan  	Removing case preserving reader macro.
	5  	 2/27/97	Hernan  	Putting define-sk8-function back in the code.
						4  10/ 9/96Hernan  Need to reset the xor pattern every time we redraw.
|# ;(do not edit past this line!!)
