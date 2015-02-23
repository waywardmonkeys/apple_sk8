;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories



;;;________________________________________________________________________________________________
;;;________________________________________________________________________________________________
;;;________________________________________________________________________________________________
;;;JAVA INDENTATION
;;;________________________________________________________________________________________________
;;;________________________________________________________________________________________________
;;;________________________________________________________________________________________________
(in-package :sk8dev)

(defparameter java-indent-level 0)

(defmacro find-forward (theBuffer theString &optional startPos)
  `(ccl::buffer-forward-search ,theBuffer ,theString ,startPos))

(defun count-chars-on-line (w theString)
  (let* ((theBuffer (fred-buffer w))
         (start-line-pos (buffer-line-start theBuffer))
         (eol (buffer-line-end theBuffer start-line-pos))
         (caseMarkerPos start-line-pos)
         (cnt 0))
    (ccl::while (and caseMarkerPos (<= caseMarkerPos eol))
      (setf caseMarkerPos (find-forward theBuffer theString caseMarkerPos))
      (if (and caseMarkerPos (<= caseMarkerPos eol) 
               (not (string= (buffer-substring theBuffer (- caseMarkerPos 2) (1- caseMarkerPos)) "\"")))  ;; a hack for now...
        (incf cnt)))
    cnt))

(defun indent-this-java-line (w)
  (let* ((buf (fred-buffer w))
         (numOpen (count-chars-on-line w "{"))
         (numClosed (count-chars-on-line w "}"))
         )
    (if (> numclosed 0) (decf java-indent-level))
    (if (< java-indent-level 0) (setf java-indent-level 0))
    (set-indent buf java-indent-level)
    (if (> numclosed 1) (decf java-indent-level (1- numclosed)))
    (incf java-indent-level numOpen)
    (if (< java-indent-level 0) (setf java-indent-level 0))
    
    ))


(defun ed-indent-java-region (w)
  (let* ((buf (fred-buffer w))
         (java-indent-level 0)
         (incomment nil))
    (multiple-value-bind (start end) (selection-range w)
      (do ((line-point start (1+ (buffer-line-end buf))))
          ((or (> line-point end)
               (> line-point (buffer-size buf))))
        (set-mark buf line-point)
        (if (and (not incomment) (> (count-chars-on-line w "/*") 0)) (setf incomment t))
        (if (eq incomment nil) 
          (indent-this-java-line w)
          (set-indent buf java-indent-level))
        (if (and incomment (> (count-chars-on-line w "*/") 0)) (setf incomment nil))
        ))
    nil))


(defun java-indent-and-save (theFile)
  (if (probe-file theFile)
    (let* ((theWindow (make-instance *default-editor-class* :filename theFile :window-show nil))
           (buf (fred-buffer theWindow)))
      (set-selection-range theWindow 0 (buffer-size buf))
      (ed-indent-java-region theWindow)
      (window-save theWindow)
      (window-close theWindow))
    (format t "~%WARNING: file ~a was not found." theFile)))


(defun indent-java-directory (outPath)
  (setf outpath (namestring (truename (process-potential-file outpath))))
  (unless (directory-pathname-p outpath) (error "Pathname must be a directory."))
  (dolist (outfile (directory (concatenate 'string outPath "*.java")))
    (sk8::sendtolog (format nil "indenting ~A~%" outfile))
    (java-indent-and-save outfile)))




(comtab-set-key *comtab* '(:control :meta #\j)
                #'ed-indent-java-region
                "indent the java code in the region")


;;;**** MOVE ME TO THE RIGHT PLACE!!
;;(indent-java-directory "ccl;tempdir:")

(define-handler mapAncestors (Object &key ((:function fun) nil))
  (when fun
    (let ((mf::*ancestors* nil))
      (declare (special mf::*ancestors*))
      (mf::mapancestors1 (parents me) fun))))

(define-handler ancestors (Object &key)
  (let (result)
    (mapancestors me :function #'(lambda (obj) (setq result (nconc result (cons obj nil)))))
    result))

(define-handler ancestors (symbol &key)
  (ancestors symbol))



#|
	Change History (most recent last):
	2  	 9/23/96	Brian   	
	3  	10/22/96	Hernan  	Error checking in java indentation.
	4  	10/22/96	Brian   	sending things to the log rather than the listener
	5  	 2/27/97	Hernan  	
|# ;(do not edit past this line!!)
