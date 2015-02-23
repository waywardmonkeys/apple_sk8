;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories

; FILE          "Simple-Queue.lisp"
; IMPLEMENTS     A Queue. 
; AUTHOR         Ken Dickey
; DATE           1994 August 3
; LAST UPDATE    1994 August 3
; COPYRIGHT (c)  1994 Apple Computer, Inc. ; All rights reserved
; NOTES:   I can't seem to find the queue or deque code anywhere ... ??? @@

(in-package :SK8Dev)
;;(in-package :cl-user)

(defclass queue ()
  ((head :initform 'nil :accessor head)
   (tail :initform 'nil :accessor tail))
)

(defmethod push-last (obj (q queue))
  ;;(when debug (format t "~%push-last ~s" obj))
  (if (null (head q))
    (let ( (head-and-tail (cons obj nil)) )
      (setf (head q) head-and-tail)
      (setf (tail q) head-and-tail)
    )
    (let ( (old-tail (tail q))
           (new-tail (cons obj nil)) 
         )
      (setf (cdr old-tail) new-tail)
      (setf (tail q) new-tail)
    )
) )

(defmethod pop-first ((q queue))
  (let ( (head (head q)) ) ; unchecked
    (setf (head q) (cdr head))
    (when (eq head (tail q))
      (setf (tail q) nil))
    (car head))
)

(defmethod empty? ((q queue))
  (null (head q)))

(defmethod size ((q queue) &key)
  (length (head q)))

;; (provide 'simple-queue)

;;                             --- E O F ---

#|
	Change History (most recent last):
	1  	 9/26/94	kend    	New {I needed a queue!} - KenD
	3  	 2/27/97	Hernan  	Fixing method congruency problems.
|# ;(do not edit past this line!!)
