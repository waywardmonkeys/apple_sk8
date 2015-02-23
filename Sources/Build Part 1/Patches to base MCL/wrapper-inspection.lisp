;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories

;;;

(in-package :inspector)


;;;  Patches existing function-inspection code so that inspecting a SK8-debug-wrapped lfun doesn't
;;;  break with a lisp error (or worse, a trip to Macsbug)
;;;


(unless (fboundp 'SS::!debugWrapper?)
  (let ((*record-source-file* nil))
    (defun SS::!debugWrapper? (thing) (declare (ignore thing)) nil)))


(defun compute-disassembly-lines (f &optional (function (inspector-object f)))
  (if (functionp function)
    (let* ((info (when (disasm-p f)
                   (list-to-vector (if (SS::!debugWrapper? function)
                                     (let ((old-lfun-bits (CCL::lfun-bits function)))
                                       (CCL::lfun-bits function 0)
                                       (unwind-protect (CCL::disassemble-list function)
                                         (CCL::lfun-bits function old-lfun-bits)))
                                     (CCL::disassemble-list function)))))
           (length (length info))
           (last-pc (if info (car (svref info (1- length))) 0)))
      (if (listp last-pc) (setq last-pc (cadr last-pc)))
      (setf (pc-width f) (length (format nil "~d" last-pc)))
      (setf (disasm-info f) info)
      length)
    0))

(defmethod line-n ((f function-inspector) n)
  (let ((o (inspector-object f)))
    (case n
      (0 (values o ""))
      (1 (values (function-name o) "Name" :colon))
      (2 (multiple-value-bind (arglist type) (unless (SS::!debugWrapper? o) (arglist o))
           (let ((label (if type (format nil "Arglist (~(~a~))" type) "Arglist unknown")))
             (values arglist label (if type :colon '(:comment (:plain)))))))
      (t (disassembly-line-n f (- n 3))))))



#|
	Change History (most recent last):
	1	3/25/94	chip	new file
	2  	 2/27/97	Hernan  	
|# ;(do not edit past this line!!)
