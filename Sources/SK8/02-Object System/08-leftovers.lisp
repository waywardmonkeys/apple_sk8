;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories

;;;-*- Mode: Lisp; Package: MACFRAMES -*-

(in-package :macframes)

;;; Autoload patches from Patches directory. Developer release can load Lisp source files, otherwise only loads fasls
;;;

(defun do-load-patches (directory developer-p)
  (when (probe-file "ccl;Patches:")
    (let ((files (if developer-p
                   (directory (concatenate 'string directory "**:*.*"))
                   (directory (concatenate 'string directory "**:*.*")
                              :test #'(lambda(f) (eq (mac-file-type f)
                                                     #.(ccl::make-keyword (string-upcase (pathname-type *.fasl-pathname*))))))))
          (*record-source-file* nil))
      (when files
        (with-cursor *watch-cursor*
          (dolist (f files)
            (multiple-value-bind (val err)
                                 (ignore-errors 
                                  (if developer-p
                                    (load f)
                                    (ccl::%fasload (mac-namestring f))))
              (declare (ignore val))
              (when err
                (flet ((errstr (cndt)
                         (let ((*print-circle* *error-print-circle*)
                               (*print-array* nil)
                               (*print-escape* t)
                               (*print-gensym* t)
                               (*print-length* nil)  ; ?
                               (*print-level* nil)   ; ?
                               (*print-lines* nil)
                               (*print-miser-width* nil)
                               (*print-readably* nil)
                               (*print-right-margin* nil)
                               (*signal-printing-errors* nil))
                           (with-output-to-string (s) (ccl::report-condition cndt s)))))
                  (let* ((width 500)
                         (height 400)
                         (left 40)
                         (top 40))
                    (message-dialog (format nil "Error loading patch file~%~a~%~a"
                                            (mac-namestring f)
                                            (errstr err))
                                    :title "Fatal Error"
                                    :ok-text "Quit" :size (make-point width height) :position (make-point left top))
                              (setf (symbol-function 'cl-user::startup-sk8-2) #'(lambda())) ;; suppress rest of startup
                    (quit)))))))))))

(let ((*warn-if-redefine-kernel* nil)
      (*warn-if-redefine* nil))
  (if cl-user::*development-p*
    (defun load-patches (&key (directory "ccl;patches:"))
      (do-load-patches directory t))
    (defun load-patches (&key (directory "ccl;patches:"))
      (do-load-patches directory nil))
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Lisp development mode: Only leave in Developer build, not for release to SK8 users who do not own MCL since it provides a Lisp listener
;; This has to be defined during the SK8 build process, but the Release build undefines it before the application image is saved
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar SS::!*lisp-dev-mode* t) ; must be defined right away so listener will work during build!

(defvar sk8dev::*user-menubar-obj*)
(defvar sk8dev::*user-menubar-hidden?* nil)

(queue-build-fixup
  (setq sk8dev::*user-menubar-obj* (SK8:menubar SK8:Stage)))

(defun SK8:dev-mode (&key force?)
  (cond
   (SS::!*lisp-dev-mode*
    ; *** maybe (untrace)?
    (setq SS::!*lisp-dev-mode* nil)
    (let ((w (front-window :class 'listener)))
      (when w
        (window-close w)))
    (when (and sk8dev::*user-menubar-obj*  ;; if it is nil, let it stay nil, in case that's what someone really wants
               (or (null (SK8:inheritsfrom sk8dev::*user-menubar-obj* SK8:actor))  ;; in case this is really a disposed object
                   (null (SK8:project sk8dev::*user-menubar-obj*))))
      (setq sk8dev::*user-menubar-obj* SK8:SimpleMenuBar))  ;; a nicer default than the nil one we used to use
    (let ((theMenuBar (or (SK8:menubar SK8:Stage) SK8dev::*user-menubar-obj*)))
      (setf (SK8:menubar SK8:Stage :visible (when theMenuBar (SK8:visible theMenuBar)))
            theMenuBar))
    (when sk8dev::*user-menubar-hidden?* (gs::hideMenubar))
    t)
   (t
    (when (or force? (and (shift-key-p) (option-key-p)))
      (setq SS::!*lisp-dev-mode* t
            sk8dev::*user-menubar-hidden?* (not *mBarVisible*)
            sk8dev::*user-menubar-obj* (SK8:menubar sk8:Stage))
      (setf (SK8:menubar sk8:Stage) nil)
      (set-menubar *default-menubar*)
      (gs::showMenuBar :force t)
      (window-select (or (front-window :class 'listener :include-invisibles t)
                         (ccl::make-new-listener)))
      t))))

;; this is the definition used by a runtime build. It is redefined later if a full SK8 is being built
(defmethod ccl::application-error ((a ccl::sk8-application-class) cndition error-pointer)
  (let ((SK8cndition (sk8dev::condition-to-sk8-condition cndition)))
    (sk8dev::SK8-signal SK8cndition))
  (flet ((errstr (cndt)
           (let ((*print-circle* *error-print-circle*)
                 (*print-array* nil)
                 (*print-escape* t)
                 (*print-gensym* t)
                 (*print-length* nil)  ; ?
                 (*print-level* nil)   ; ?
                 (*print-lines* nil)
                 (*print-miser-width* nil)
                 (*print-readably* nil)
                 (*print-right-margin* nil)
                 (*signal-printing-errors* nil))
             (with-output-to-string (s) (ccl::report-condition cndt s)))))
    (if sk8script::!*lisp-dev-mode*
      (ccl::break-loop-handle-error cndition error-pointer)
      (let* ((width 300)
             (height 100)
             (ctr (sk8:MainMonitorCenter))
             (left (- (first ctr) (round width 2)))
             (top (- (second ctr) (round height 2))))
        (message-dialog (errstr cndition) :title "Fatal Error" :ok-text "Quit" :size (make-point width height) :position (make-point left top))
        (quit)))))

#|
	Change History (most recent last):
	1  	 5/ 9/96	sidney  	New object system
	2  	 9/ 3/96	Hernan  	misc. changes
	3  	 2/27/97	sidney  	remove unused code, add some comments
	4  	 2/27/97	Hernan  	
	5  	 3/ 7/97	sidney  	load-patches in non-developer version should not load lisp source files
						define simple error handling for use in runtime-only build, to be redefined later in full build
|# ;(do not edit past this line!!)
