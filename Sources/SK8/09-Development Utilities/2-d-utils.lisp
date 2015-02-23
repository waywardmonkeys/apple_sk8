;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories


;;;


(in-package :cl-user)

#| Modification History

03-30-92 ruben began d29.

|#




(defun trace-sk8-events (&optional (author-p t))
  (when author-p
    (trace authormouseDown
           authormouseUp
           authormouseEnter
           authormouseLeave
           authormouseWithin
           authorclick
           authordoubleClick))
  (trace idle
         mouseDown
         mouseUp
         mouseEnter
         mouseLeave
         mouseWithin
         click
         doubleClick
         activateText
         activateCard
         activateLayer
         activateWindow
         activateProject
         deactivateText
         deactivateCard
         deactivateLayer
         deactivateWindow
         deactivateProject
         drop
         connected
         disconnected
         dragged
         affixed
         unaffixed))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; DRAWING UTILITIES
;;;

(defun flash-region-in-port (region port &optional (num-flashes 6) (flash-time 10000))
  (when (and (handlep region) (pointerp port))
    (let+ ((oldPort (:pointer))
           (oldGdev (:pointer))
           (save-clipRgn (:region)))
      (without-interrupts
       (#_GetGWorld oldPort oldGdev)
       (#_SetGWorld port (%null-ptr))
       (#_GetClip save-clipRgn)
       
       (#_SetClip region)
       (dotimes (i (* 2 num-flashes))
         (#_InvertRgn region)
         (dotimes (j flash-time)))
       
       (#_SetClip save-clipRgn)
       (#_SetGWorld (%get-ptr oldPort) (%get-ptr oldGdev))))))


(defun flash-sk8rect-in-port (SK8Rect port &optional (num-flashes 6) (flash-time 10000))
  (let+ ((region (:region :init-SK8Rect SK8Rect)))
    (flash-region-in-port region port num-flashes flash-time)))


(defun flash-region (self &optional (num-flashes 6) (flash-time 10000))
  (let+ ((node (mf::get-struct self))
         (window (window self))
         (ownedRegion (:region)))
    (when window
      (make-region self ownedRegion (mf::node-rect node) node window)
      (flash-region-in-port ownedRegion
                            (wptr (mf::get-struct window))
                            num-flashes flash-time))))


(defun flash-rect (self &optional (num-flashes 6) (flash-time 10000))
  (let ((window (window self)))
    (flash-sk8rect-in-port (mf::node-rect (mf::get-struct self))
                           (if window (wptr (mf::get-struct window)) (get-wmgr-port))
                           num-flashes flash-time)))


(defun flash-logrect (self &optional (num-flashes 6) (flash-time 10000))
  (let+ ((node (mf::get-struct self))
         (window (window self))
         (ownedRegion (:region :init-SK8Rect (mf::node-logical-rect node))))
    (flash-region-in-port ownedRegion
                          (if window (wptr (mf::get-struct window)) (get-wmgr-port))
                          num-flashes flash-time)))


(defun flash-dirty-region (self &optional (num-flashes 6) (flash-time 10000))
  (let ((clos-window (mf::get-struct self)))
    (flash-region-in-port (slot-value clos-window 'mf::dirty-region)
                          (wptr clos-window)
                          num-flashes flash-time)))



(defun flash-dirty-nodes (self &optional (num-flashes 6) (flash-time 10000))
  (let+ ((clos-window (mf::get-struct self))
         (rgn (:region))
         (totalrgn (:region)))
    (#_SetEmptyRgn totalrgn)
    (mapc #'(lambda (node)
              (make-region (mf::node-frame node) rgn (mf::node-rect node) node self)
              (#_UnionRgn totalrgn rgn totalrgn))
          (slot-value clos-window 'mf::dirty-nodes))
    (flash-region-in-port totalrgn
                          (wptr clos-window)
                          num-flashes flash-time)))


#|

(setq mf::*graphics-debug* t)
(setq mf::*graphics-debug* nil)

(flash-dirty-nodes 'window-196)


|#



#|

;; To display, step by step, the edges of the polygon in the given point-array
(without-interrupts
 (with-port (wptr (mf::get-struct {window-201}))
   (#_MoveTo (point-h (aref temp-line-points 0)) (point-v (aref temp-line-points 0)))
   (dotimes (i 9)
     (#_LineTo (point-h (aref temp-line-points (1+ i))) (point-v (aref temp-line-points (1+ i)))))
   (#_LineTo (point-h (aref temp-line-points 0)) (point-v (aref temp-line-points 0)))))
   
(without-interrupts
 (with-port (wptr (mf::get-struct {window-201}))
   (dotimes (i 9)
     (ed-beep)
     (dotimes (j 10)
       (#_ForeColor #$whiteColor)
       (#_MoveTo (point-h (aref temp-line-points i)) (point-v (aref temp-line-points i)))
       (#_LineTo (point-h (aref temp-line-points (1+ i))) (point-v (aref temp-line-points (1+ i))))
       (dotimes (k 10000))
       (#_ForeColor #$blackColor)
       (#_MoveTo (point-h (aref temp-line-points i)) (point-v (aref temp-line-points i)))
       (#_LineTo (point-h (aref temp-line-points (1+ i))) (point-v (aref temp-line-points (1+ i))))
       (dotimes (k 10000))))))

|#
#|
	Change History (most recent last):
	2	2/12/94	kleiman	renaming
	2  	 2/27/97	Hernan  	
|# ;(do not edit past this line!!)
