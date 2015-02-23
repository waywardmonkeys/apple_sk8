;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories
(in-package :ccl)

;;; Someday, when the listener output stream code will take care of this automatically,
;;; and this won't be needed.
(defmacro with-undisturbed-graphics (&body body)
  `(with-port ccl::%temp-port%
     (with-focused-view nil
       ,@body)))

(setf *trace-bar-frequency* 4)
  
;;; pr and prtrace make use of this information to enable debug
;;; printouts to use appropriate indentation
(defvar *trace-indent-active* nil)

(let ((ccl::*warn-if-redefine-kernel* nil)
      (ccl::*warn-if-redefine* nil))
  
  (defun push-trace-level ()
    (incf *trace-level*))
  
  (defun pop-trace-level ()
    (decf *trace-level*))
  
  (defun trace-tab (&optional suppress-extra-indent &aux (n (min *trace-level* *trace-max-indent*)))
    (format *trace-output*
            (concatenate
             'string
             (if (> n 0)
               "~& "
               "~&")
             (with-output-to-string (str)
               (dotimes (i (- n (if suppress-extra-indent 1 0)))
                 (declare (fixnum i))
                 (declare (fixnum i))
                 (write-char (if *trace-bar-frequency*
                               (case (mod i (min 6 *trace-bar-frequency*))
                                 (0 #\|)
                                 (1 #\:)
                                 (2 #\!)
                                 (3 #\:)
                                 (4 #\I)
                                 (5 #\:))
                               #\space)
                             str))
               str)
             (if suppress-extra-indent "" " "))))
  
  (defun trace-before  (&rest args)
    (declare (dynamic-extent args))
    (with-undisturbed-graphics
      (trace-tab t)
      (let* ((*print-level* *trace-print-level*)
             (*print-length* *trace-print-length*)
             (*print-readably* nil))
        (declare (special *print-level* *print-length* *print-readably*))
        (format *trace-output* "/~S ~%" args))
      (setf *trace-indent-active* t)))
  
  (defun trace-after (sym &rest args &aux (n (length args)))
    (declare (dynamic-extent args))
    (let* ((*print-level* *trace-print-level*)
           (*print-length* *trace-print-length*)
           (*print-readably* nil))
      (declare (special *print-level* *print-length* *print-readably*))
      (with-undisturbed-graphics
        (trace-tab t)
        (format *trace-output* "\\ ~S => " sym)
        (if (eq n 1)
          (progn
            (format *trace-output* "~S~%" (%car args)))
          (progn
            (format *trace-output* " values :" n)
            (dolist (val args)
              (trace-tab t)
              (format *trace-output* "     ~S" val))))
        (when (= *trace-level* 1)
          (setf *trace-indent-active* nil)))))
  )

#| TESTS

(defun x1 (arg)
  arg)
(defun x2 (arg) (x1 arg))
(defun x3 (arg) (x2 arg))
(defun x4 (arg) (x3 arg))
(defun x5 (arg) (x4 arg))

(trace x1 x2 x3 x4 x5)

(x1 1)
(x5 1)
|#

#|
	Change History (most recent last):
	1  	10/13/94	dy      	Make traces more readable
	2  	10/13/94	dy      	|:|:|:| instead of | | | |
	3  	10/17/94	dy      	vary the column characters for readability
	4  	10/19/94	dy      	change *trace-bar-frequency* to 4
	5  	10/25/94	dy      	trace-tab use ! instead of i
	6  	 2/15/95	dy      	changes to trace-tab, trace-before, and trace-after to work with the trace printout functions
	7  	 2/20/95	dy      	with-undisturbed-graphics wasn't working when compiled
	8  	 2/20/95	dy      	put back the setf *trace-bar-frequency* which was inadvertently removed
	9  	 2/20/95	dy      	cleanup
	10 	 2/20/95	dy      	move *trace-indent-active* outside the let
	2  	 2/27/97	Hernan  	
|# ;(do not edit past this line!!)
