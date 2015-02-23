;;;; Those parts of the source code control system that don't care
;;;; about the interface to the underlying database (i.e., whether
;;;; code is stored by Projector or Pogo).  The Projector and Pogo
;;;; databases are subclassed off of this.
;;;;
;;;; Many of these non-generic functions ought to be generics that
;;;; specialize on project, but I haven't bothered to make them generic
;;;; since there's no need yet.
;;;;

(in-package :SourceServer)

;;; *** ought to change these to class variables
(defvar *projects* nil)           ;; interned projects
(defvar *mounted-projects* nil)
(defvar *pathname->project*)

;;;
;;; Project-related conditions
;;; *** probably there should be a subclass project-file-error, that takes a file slot too
;;; *** the project and file ought to be the first two arguments passed to the format string

(define-condition project-error (error)
                  ((project :reader project-error-project :initarg :project)))
(define-condition simple-project-error (project-error simple-condition))
(define-condition project-remote-is-newer (simple-project-error))
(define-condition project-file-already-checked-out (simple-project-error))
(define-condition project-does-not-contain-file (simple-project-error))
(define-condition project-volume-not-available (simple-project-error))
(define-condition project-file-checked-out (simple-project-error)
                  ((remote-user :reader project-error-remote-user :initarg :remote-user)))
(define-condition project-not-accessible (project-error)
                  ()
  (:report (lambda (c stream)
             (format stream "Project ~A is not accessible."
                     (project-error-project c)))))
(define-condition file-not-in-a-project (project-error)
                  ((filename :accessor project-error-filename :initarg :filename))
  (:report (lambda (c stream)
             (format stream "~/pp-path/ does not belong to a project."
                     (project-error-filename c)))))

(defun signal-project-error (error-type project format-string &rest arguments)
  (error (make-condition error-type :project project :format-string format-string :format-arguments arguments)))

(defun project-file-checked-out (remote-user format-string &rest arguments)
  (error (make-condition 'project-file-checked-out :remote-user remote-user
                         :format-string format-string :format-arguments arguments)))

(defun project-not-accessible (project)
  (error (make-condition 'project-not-accessible :project project)))

(defun file-not-in-a-project (path)
  (error (make-condition 'file-not-in-a-project :filename path)))

(defun cl-user::pp-path (stream path &optional colon atsign)
  (declare (ignore colon atsign))
  (prin1 (namestring path) stream))

;;;
;;; Project class
;;;

;; *** change the local-dir from the pathname to the pathname-directory list,
;; since that's always what's needed and right now everything has to convert
;; (change its name to local-directory as well)
(defclass projector ()
  ((local-dir :reader project-local-dir :initarg :local-dir)
   (parent :reader project-parent :initarg :parent :initform nil)
   (children :accessor project-children :initform nil)
   (mount-alias :reader project-mount-alias :initarg :mount-alias)))

(defmethod initialize-instance ((project projector) &rest initargs
                                &key parent &allow-other-keys)
  (declare (dynamic-extent initargs))
  (apply #'call-next-method project initargs)
  (when (slot-boundp project 'remote-pathname)  ; << kludge
    (setq *projects* (sort-project-list (cons project *projects*))))
  (when parent
    (pushnew project (project-children parent))))

(defmethod print-object ((project projector) stream)
  (flet ((write-project-name ()
           (write (project-name project) :stream stream :escape t)))
  (if *print-escape*
    (print-unreadable-object (project stream :type t)
      (write-project-name))
    (write-project-name))))

(defun sort-project-list (projects)
  (sort projects #'(lambda (&rest args)
                     (declare (dynamic-extent args))
                     (apply #'string< (mapcar #'project-name args)))))

(defun project-mounted-p (project)
  (find project *mounted-projects*))

;; *** currently assumes all projects are mpw-projects
(defun mount-projects (projects)
  (when projects
    (mpw-projects-mount projects)))

(defun project-mounted (project)
  (unless (memq project *mounted-projects*)
    (setq *mounted-projects* (sort-project-list (cons project *mounted-projects*)))
    (incremental-scan-project project)))

(defun project-unmounted (project)
  (setq *mounted-projects* (delete project *mounted-projects*)))

(defmethod project-mount ((project projector))
  (project-mounted project))

(defmethod project-unmount ((project projector))
  (project-unmounted project))

(defun project-root (project &aux (parent (project-parent project)))
  (if parent
    (project-root parent)
    project))

(defun project-root-p (project)
  (null (project-parent project)))

;;;
;;; File utilities
;;;

;; *** Assumes everything is a projector file.  We'll have to do something about that.
;; Perhaps pathname-project ought to always make the guess, and interrogate the guessed
;; project about whether it does indeed contain the file.  Then pathname-project-p can
;; be a with-simple-restart wrapper around pathname-project, and the error can be moved
;; from here to project-file-verify or some such.  Or maybe there should be a project-file-p.
;;
;; Since many things die with no remote-pathname, we kludge to ignore such projects
;; I believe this means that only those projects created by setup-initial-projects
;; will be heeded. 
(defun pathname-project-p (path)
  (or (gethash path *pathname->project*)
      (let* ((projector-name (projector-file-project-name path))
             (project
              (when projector-name
                (intern-projector-name (make-pathname :directory (pathname-directory path))
                                       projector-name))))
        (when (and project (slot-boundp project 'remote-pathname))  ;<< Kludge
          (setf (gethash path *pathname->project*) project)
          project))))

(defun pathname-project (path &aux (project (pathname-project-p path)))
  (or project
      (restart-case (file-not-in-a-project path)
        (add-file-to-project (&optional (project (guess-pathname-project path)))
         project))))

; was broken
(defun guess-pathname-project (path &aux (directory  (mac-directory-namestring path)))
  (dolist (project *mounted-projects* nil)
    (when (equalp directory  (mac-directory-namestring  (project-local-dir project)))
      (return project))))

(defun remove-pathname-from-cache (path)
  (remhash path *pathname->project*))

(defun reset-pathname-cache ()
  (setq *pathname->project* (make-hash-table :test #'equal)))

(def-load-pointers reset-pathname-cache-on-restart ()
  (reset-pathname-cache))

(eval-when (load eval)
  (reset-pathname-cache))

;;;
;;; logical hosts
;;; *** move to the ui area

(defun project-host-name (project)
  (first (last (pathname-directory (project-local-dir project)))))

(defun define-logical-host-for-project (project)
  (when (project-root-p project)
    (let ((host-name (string-trim '(#\º) (project-name project))))
      (unless (ccl::logical-host-p host-name)
        (setf (logical-pathname-translations host-name)
              `(("**;*.*" ,(mac-namestring (project-local-dir project)))))))))

(defun remove-logical-host-for-project (project)
  (when (project-root-p project)
    (let ((host-name (string-trim '(#\º) (project-name project))))
      (remove-logical-pathname-translations host-name))))

(defun remove-logical-pathname-translations (host)
  (declare (special ccl::%logical-host-translations%))
  (let ((ass (ccl::%str-assoc host ccl::%logical-host-translations%)))
    (when ass
      (setf ccl::%logical-host-translations%
            (delete ass ccl::%logical-host-translations% )))))

;;;
;;; unexamined Project stuff
;;;

(defun root-project-check-out-dir (root-project)
  (let* ((bare-name (string-trim '(#\º) root-project))
         (ass (assoc bare-name (my-projects)
                     :test #'(lambda (subdir dir)
                               (let* ((pos (search subdir dir :from-end t))
                                      (place (- (length dir) (length subdir))))
                                 (and pos
                                      (or (= pos place)
                                          (= pos (1- place)))))))))
    (if ass
      (cadr ass)
      (error "Project: ~s is not in: ~s.." root-project '*my-projects*))))

;;;
;;; Utilities
;;;

(defun do-project-files (project fn)
  (dolist (file (directory (merge-pathnames "*.*" (project-local-dir project))))
    (when (project-file-in-project project file)
      (funcall fn file))))

;;categorize-project-modified-files returns five lists:
;;  files that are checked out
;;  files that are modify-read-only and can be checked in with further ado
;;  files that are modify-read-only and can be checked in after merging
;;  files that are modify-read-only and cannot be checked in because they
;;    have been checked out by someone else and they have the same versions as local files
;;  files that are modify-read-only and cannot be checked in because they
;;    have been checked out by someone else and they have the different versions from local files
;;
;;  each element of each list is a cons of path and project

(defun categorize-project-modified-files (projects)
  (format t "~&Scanning project directories for conflicts...")
  (let ((check-in-files ())
        (check-out-and-in-files ())
        (merge-in-files ())
        (wedge-files ())
        (changed-wedge-files ()))
    (dolist (project projects)
      (multiple-value-bind (check-in-1 check-out-1 merge-in-1 wedge-1 changed-wedge-1)
                           (project-categorize-modified-files project)
        ;; All these nconcs walk to the tail of the lists each time, and the lists get
        ;; longer and longer.  Not, in general, a good idea.  However, if this actually
        ;; adds appreciably to the time it takes projector to do anything, I'll eat the
        ;; code.
        (setq check-in-files (nconc check-in-files check-in-1)
              check-out-and-in-files (nconc check-out-and-in-files check-out-1)
              merge-in-files (nconc merge-in-files merge-in-1)
              wedge-files (nconc wedge-files wedge-1)
              changed-wedge-files (nconc changed-wedge-files changed-wedge-1))))
    (values check-in-files check-out-and-in-files merge-in-files wedge-files changed-wedge-files)))

#|
	Change History (most recent last):
	30	4/3/92	ows	mostly new:  the 'projector class
	3	4/4/93	Sidney	Use SourceServer package instead of CCL and use
				MCL 2.0 low-level system calls instead of ones
				from verison 1.3 that required loading a verison of
				sysequ.lisp that could lead to name collisions in
				the ccl package.
	4  	10/19/94	dy      	scan-on-mount now incremental part of mounting
	5  	10/19/94	dy      	introduce *scan-on-mount-is-incremental*
	6  	10/19/94	dy      	call incremental-scan-project instead of checking global *scan-on-mount*
|# ;(do not edit past this line!!)
