;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories

(in-package :ps)

;;; make-temporary-file -- returns a temporary filename and optionally makes a new pheap file with that name
;;;  object - an object whose objectname should be used as an infix for the filename
;;;  name - boolean: if true, then return the name only (don't create a file)

(defparameter *temporary-file-counter* 0)

(defconstant *temporary-file-folder* "SK8:SK8 Temporary Files;")

(defun make-temporary-file (&optional object name)
  (let* ((obname (when object (sk8::sk8_id object)))
         (filename (concatenate 'string "$"
                                (write-to-string (incf *temporary-file-counter*))
                                "$"
                                (when (and obname (symbolp obname))
                                  (string obname))
                                )))
    (setf filename (concatenate 'string *temporary-file-folder*
                                (if (> (length filename) 31)   ;; truncate to legal filename if it is too long
                                  (subseq filename 0 31)  ;; *** KLUDGE ALERT: HARDCODED CONSTANT 31 = MAX LEGAL FILE NAME ON MAC OS
                                  filename)))
    (when (probe-file filename) (return-from make-temporary-file (make-temporary-file object name)))
    (unless name (create-file filename :if-exists :supersede :mac-file-type :PROJ))
    filename))

(defun create-swapfile-if-needed (proj)
  (or (sk8::swapfile proj)
      (let ((thefile (sk8::file proj))
            (swapfile (sk8::new sk8::file :project proj :logicalname (make-temporary-file proj t))))
        ;; copy the original project file to the temporary working file space
        (if (sk8::fileExists thefile)
          (progn
            (sk8dev::copyfile (sk8::file proj) swapfile :if-exists :supersede)
            (ccl::with-open-res-file (nil (sk8::physicalname swapfile) :if-does-not-exist :create)
              )  ;; force the swapfile to have a resource fork in case it doesn't
            )
          ;; no project file to copy: make a new swapfile for this project
          (let ((file-pathname (namestring (sk8::physicalname swapfile))))
            (mf::create-project-file proj file-pathname)
            (with-pstrs ((filename (mac-namestring file-pathname)))
              (require-trap #_createresfile filename))))
        (setf (sk8::swapfile proj) swapfile))))

;; get rid of all files in the temporary folder. used for cleanup
(defun delete-all-temporary-files ()
  (dolist (fl (directory (make-pathname :host (host-namestring *temporary-file-folder*)
                                        :directory (directory-namestring *temporary-file-folder*)
                                        :name :wild)
                         :directories t))
    (ignore-errors
     (with-pstrs ((pathname (mac-namestring fl)))
       (#_closeresfile (#_openresfile pathname)))
     (delete-file fl))))

#|
	Change History (most recent last):
	2  	10/21/96	sidney  	delete-temporary-files
	3  	 2/27/97	Hernan  	
|# ;(do not edit past this line!!)
