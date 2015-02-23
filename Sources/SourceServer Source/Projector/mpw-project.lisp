;;;; Most of this used to be projector.lisp.  The change history from that life is at
;;;; the end of the file.

(in-package :SourceServer)

;;;
;;; Projector class and conditions
;;;

(defclass mpw-project (projector)
  ((remote-pathname :reader project-remote-pathname :initarg :remote-pathname)
   (projector-name :reader projector-name :initarg :projector-name)))

(defun project-volume (project)
  (concatenate 'string (second (pathname-directory (project-remote-pathname project)))
               ":"))

;;;
;;; old Projector primitives
;;;

(defun break-string-into-list (str &optional (ch #\return))
  (let ((list nil))
    (do* ((start 0 (1+ pos))
          (pos (position ch str :start start) (position ch str :start start)))
         ((null pos) (when (neq start (length str)) (push (subseq str start) list)))
      (push (subseq str start pos) list))
    (nreverse list)))

(defun mpw-mounted-projects ()
  (let ((str (mpw-mountproject nil :short t :recursive t :project-paths t)))
    (when str
      (break-string-into-list str #\return))))

;;;
;;; file info utilities
;;;

;; KLUDGE ALERT!!!
;; There is no good documented way to check if a resource file is already open, but
;; we have to close a file after opening it if and only if it was not already open.
;; This routine uses the documented low-level structure of the list of open resource
;; files as described in Inside Mac, Vol I. The with-open-res-file macro uses this
;; function to make sure it only closes a file it has opened if it wasn't already open.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require "RESOURCES"))

;;; fix a bug in the resources library
(let ((ccl::*warn-if-redefine-kernel* nil)
      (ccl::*warn-if-redefine* nil))
  (defun ccl::open-resource-file (file &key (if-does-not-exist :error) (errorp t))
    (let ((real-file (probe-file file)))          ; resolve alias
      (setq real-file (mac-namestring (or real-file file)))
      (ccl::with-pstr (pf real-file)
        (let ((res (#_OpenResFile pf)))
          (declare (fixnum res))
          (when (< res 0)
            (flet ((err (code)
                     (if errorp
                       (if (eq code #$eofErr)
                         (error "Trying to read resources, but there is no resource fork in file ~s" file)
                         (ccl::signal-file-error code file))
                       (return-from ccl::open-resource-file (values nil code)))))
              (declare (dynamic-extent #'err))
              (let ((code (#_ResError)))
                (unless (or (eq code #$fnfErr)
                            (eq code #$eofErr)
                            (eq code #$resFNotFound))
                  (err code))
                (case if-does-not-exist
                  (:create
                   (#_CreateResFile pf)
                   (setq res (#_OpenResFile pf))
                   (when (< res 0) (err (#_ResError))))
                  (:error
                   (err code))
                  ((nil) (return-from ccl::open-resource-file nil))
                  (t (error (ccl::%badarg if-does-not-exist '(member nil :create :error))))))))
          res)))))

#| This is now defined in the MCL for SK8 build. It does need to be here if the SourceServer interface
    is being built on top of a plain MCL

(eval-when (:compile-toplevel :execute)
  (defconstant $nextResMapHandle 16)
  (defconstant $resFileNumber 20))

;; return list of resFile numbers of all open res files
(defun OpenResFileNums ()
  (ccl:without-interrupts
   (let ((resFileNums nil))
     (ccl:with-macptrs ((resMapHandle (ccl:%get-ptr (ccl:%int-to-ptr #$TopMapHndl)))
                    (resMapPtr (ccl:%null-ptr)))
       (loop
         (when (ccl:%null-ptr-p resMapHandle) (return resFileNums))
         (ccl:%setf-macptr resMapPtr (ccl:%get-ptr resMapHandle))
         (push (ccl:%get-word resMapPtr $resFileNumber) resFileNums)
         (ccl:%setf-macptr resMapHandle (ccl:%get-ptr resMapPtr $nextResMapHandle)))))))
|#

(defun projector-local-file-info (project path &key project-name-request local-comment-request
                                          local-user-request local-version-request
                                          &aux projector-file-p modrop projector-name
                                          local-comment local-checked-out-p
                                          local-user local-version)
  (with-open-res-file
    (nil path :errorp nil)
    (with-resource (ckid "ckid" 128)
      (setq projector-file-p t)
      (unless (= 4 (rref ckid ckid.version))
        (signal-project-error 'simple-project-error project
                              "Unknown 'ckid' version in ~A."
                              path))
      (with-dereferenced-handles ((p ckid))
        (setq local-checked-out-p (not (zerop (rref p ckid.readonly :storage :pointer))))
        (setq modrop (rref p ckid.modifyreadonly :storage :pointer))
        (let ((str-ptr (%inc-ptr p 40)))
          (labels ((get-str ()
                     (let* ((len (%get-unsigned-byte str-ptr))
                            (str (ccl::%str-from-ptr (%inc-ptr str-ptr 1) len)))
                       (setq str-ptr (%inc-ptr str-ptr (+ 2 len)))
                       str))
                   (skip-str ()
                     (setq str-ptr (%inc-ptr str-ptr (+ 2 (%get-unsigned-byte str-ptr))))
                     nil)
                   (maybe-get-str (flag)
                     (if flag (get-str) (skip-str))))
            (setq projector-name (maybe-get-str project-name-request)
                  local-user (maybe-get-str local-user-request)
                  local-version (maybe-get-str local-version-request))
            (skip-str)
            (skip-str)
            (setq local-comment (maybe-get-str local-comment-request)))))))
  (values projector-file-p projector-name modrop local-checked-out-p local-comment local-user local-version))

(defun name-and-version (string)
  (let* ((comma-pos (position #\, string))
         (length (length string))
         (last-char (char string (1- length))))
    (labels ((return-state (state)
               (values (subseq string 0 comma-pos)
                       (subseq string (1+ comma-pos) (and state (1- length)))
                       (or state :checked-in))))
      (case last-char
        (#\+ (return-state :checked-out))
        (#\* (return-state :checked-in))
        (t (return-state nil))))))

(defun parse-projector-file-info (info-lines
                                  &key (remote-comment-request t)
                                       (remote-user-request t) (remote-version-request t)
                                  &aux remote-checked-out-p remote-comment
                                       remote-user remote-version)
  (declare (ignore remote-version-request))
  (labels ((lookup-field (field-name &aux (field-length (+ (length field-name) 4)))
             (let ((line
                    (find-if #'(lambda (line)
                                 (and (> (length line) field-length)
                                      (string-equal line field-name
                                                    :start1 4
                                                    :end1 field-length)))
                             info-lines)))
               (when line
                 (subseq line (1+ field-length))))))
    (multiple-value-bind (name version state)
                         (name-and-version (first info-lines))
      (declare (ignore name))
      (setq remote-version version)
      (setq remote-checked-out-p (eq state :checked-out)))
    (when remote-user-request
      (setq remote-user (or (lookup-field "Owner:") (lookup-field "Author:"))))
    (when remote-comment-request
      (setq remote-comment
            (with-output-to-string (stream)
              (let (separator)
                (dolist (line info-lines)
                  (when (>= (or (position-if-not #'(lambda (xchar) (char= xchar #\Space)) line) 0)
                            8)
                    (write-string (subseq line 8) stream)
                    (when separator
                      (princ #\Newline stream))
                    (setq separator t))))))))
  (values remote-checked-out-p remote-comment remote-user remote-version))

(defun projector-remote-file-info (projector-name path &rest requests
                                                       &key remote-comment-request
                                                       &allow-other-keys)
  (declare (dynamic-extent requests))
  (apply #'parse-projector-file-info
         (break-string-into-list
          (mpw-projectinfo :file (file-namestring path)
                           :project projector-name
                           :latest t :comments remote-comment-request))
         requests))

;;requests is a list of keywords of the specifies requested information
;;information is returned as multiple-values in the same order as requests
;;valid keywords: projector-file-p projector-name modrop 
;;                local-checked-out-p  local-user local-version local-comment
;;                remote-checked-out-p remote-user remote-version remote-comment
;;if any remote info is available, that implies that the project is accessible
(defun projector-file-info (project path requests)
  (let (read-local-info read-remote-info need-not-exist
        (projector-name (and project (projector-name project)))
        projector-file-p modrop (exists-locally-p t)
        local-checked-out-p  local-user  local-version  local-comment
        remote-checked-out-p remote-user remote-version remote-comment)
    (dolist (request requests)
      (ecase request
        ((:projector-file-p :projector-name :modrop :local-checked-out-p :local-user :local-version :local-comment)
         (setq read-local-info t))
        ((:remote-checked-out-p :remote-user :remote-version :remote-comment)
         (setq #| read-local-info t |# read-remote-info t))
        (:exists-locally-p
         (setq need-not-exist t))))
    (unless (probe-file path)
      (if need-not-exist
        (setq exists-locally-p nil
              read-local-info nil)
        (error "Non-existent ~A." path)))
    ;; @@@ this can go away when nil projects are disallowed
    (unless project
      (when read-remote-info
        (setq read-local-info exists-locally-p)))
    (when read-local-info
      (multiple-value-setq (projector-file-p projector-name modrop local-checked-out-p local-comment local-user local-version)
        (projector-local-file-info project path
                                   :project-name-request t
                                   :local-comment-request (memq :local-comment requests)
                                   :local-user-request (memq :local-user requests)
                                   :local-version-request (memq :local-version requests)))
      (unless projector-file-p
        (restart-case (file-not-in-a-project path)
          (add-file-to-project (&optional (proj (or project (guess-pathname-project path))))
                               (multiple-value-setq (projector-file-p projector-name modrop local-checked-out-p local-comment
                                                                      local-user local-version)
                                 (projector-local-file-info proj path
                                                            :project-name-request t
                                                            :local-comment-request (memq :local-comment requests)
                                                            :local-user-request (memq :local-user requests)
                                                            :local-version-request (memq :local-version requests)))
                               proj)))
      ;; @@@ passing nil for the project is an ugly hack.  Something is wrong with the protocol.
      (assert (or (null project) (string-equal projector-name (projector-name project))) nil
              "File ~S thinks it's in project ~S, but the system thinks it's in project ~S."
              path projector-name (projector-name project)))
    (when read-remote-info
      (unless (project-mounted-p project)
        (restart-case (project-not-accessible project)
          (project-has-been-mounted ())))
      (multiple-value-setq (remote-checked-out-p remote-comment remote-user remote-version)
        (projector-remote-file-info projector-name path
                                    :remote-comment-request (memq :remote-comment requests)
                                    :remote-user-request (memq :remote-user requests)
                                    :remote-version-request (memq :remote-version requests))))
    (let ((return-values nil))
      (dolist (request requests)
        (push (case request
                    (:projector-file-p projector-file-p) (:projector-name projector-name) 
                    (:modrop modrop)
                    (:local-checked-out-p local-checked-out-p) (:local-user local-user)
                    (:local-version local-version) (:local-comment local-comment)
                    (:remote-checked-out-p remote-checked-out-p) (:remote-user remote-user)
                    (:remote-version remote-version) (:remote-comment remote-comment)
                    (:exists-locally-p exists-locally-p))
              return-values))
      (values-list (nreverse return-values)))))

;;;
;;; File operations
;;;

(defun temporary-file-name (path)
  (pathname (concatenate 'string (namestring path) "~")))

(defun transfer-ckid-from-project-file (projector-name path &key modifiable)
  (let ((temp-name (temporary-file-name path)))
    (rename-file path temp-name)
    (unwind-protect
      (progn
        (mpw-checkout (mac-namestring path)
                      :yes-to-dialogs t :modifiable modifiable :notouch t :project projector-name)
        (transfer-ckid path temp-name)
        (delete-file path))
      (rename-file temp-name path))))

(defun project-file-checkout (project path &key comment)
  (bind-file-info (local-checked-out-p remote-checked-out-p remote-user modrop
                   local-version remote-version projector-name exists-locally-p)
                  project path
    (declare (ignore exists-locally-p))
    (cond
     (local-checked-out-p
      (signal-project-error 'project-file-already-checked-out project
                           "You already have ~/pp-path/ checked out."
                           path))
     (remote-checked-out-p
      (project-file-checked-out remote-user
                                "~/pp-path/ can't be checked out because ~s has it checked out."
                                path remote-user))
     (modrop
      (let ((remote-is-newer (not (string-equal local-version remote-version))))
        (when remote-is-newer
          (restart-case
            (signal-project-error 'project-remote-is-newer project
                                 "~/pp-path/ has been modified and a newer version has been checked in."
                                 path)
            (checkout-newer-file ())))
        (unless remote-is-newer
          ;;checkout modro
          ;; @@@ consolidate this with the code in project-cancel-checkout
          (transfer-ckid-from-project-file projector-name path)))))
    (mpw-checkout (mac-namestring path) :modifiable t :notouch t :project projector-name :comment comment)))

(defun project-file-checkin (project path &key comment)
  (bind-file-info (local-checked-out-p modrop local-version projector-name)
                  project path
    (unless local-checked-out-p
      (unless modrop
        (signal-project-error 'simple-project-error project
                              "~/pp-path/ is not checked out for modification."
                              path))
      (bind-file-info (remote-checked-out-p remote-version remote-user)
                      project path
        (when remote-checked-out-p
          (project-file-checked-out remote-user
                                    "~/pp-path/ can't be checked in because it is checked out by ~s."
                                    path remote-user))
        (unless (string-equal local-version remote-version)
          (signal-project-error 'project-remote-is-newer project
                                "~/pp-path/ can't be checked in until it is merged because there is a newer version in the database."
                                path))))
    ;;check it in
    (loop
      (mpw-checkin (mac-namestring path) :touch t :project projector-name :comment comment
                   :yes-to-dialogs t)
      (if (eq (project-file-local-state project path) :checked-in)
        (return)
        (cerror "Try to check ~/pp-path/ into ~A again."
                (format nil "~/pp-path/ in project ~A wasn't actually checked in, even though MPW said it was. ~
                             Please send mail about this to Internet address dylan-implementors@cambridge.apple.com."
                        path project)
                path project)))))

(defun project-file-add (project path &key comment)
  (remove-pathname-from-cache path)
  (mpw-checkin (mac-namestring path) :new t :touch t :project (projector-name project) :comment comment))

(defun project-file-modro (project path)
  (bind-file-info (local-checked-out-p modrop) project path
    (cond
     (local-checked-out-p
      (signal-project-error 'project-file-already-checked-out project
                           "~/pp-path/ can't be made ModifyReadOnly because it is already checked out."
                           path))
     (modrop
      (signal-project-error 'simple-project-error project
                           "~/pp-path/ can't be made ModifyReadOnly because it is already ModifyReadOnly."
                           path))
     (t (set-file-modify-read-only path)))))

(defun project-file-orphan (project path)
  (declare (ignore project))
  (remove-pathname-from-cache path)
  (remove-ckid-resource path))

(defun project-file-obsolete (project path)
  (bind-file-info (local-checked-out-p remote-checked-out-p remote-user modrop
                   local-version remote-version projector-name exists-locally-p)
                  project path
    (declare (ignore local-checked-out-p modrop local-version remote-version
                     projector-name exists-locally-p))
    (when
      remote-checked-out-p
      (project-file-checked-out remote-user
                                "~/pp-path/ can't be obsoleted because ~s has it checked out."
                                path remote-user)))
  (mpw-obsolete (mac-file-namestring path) :project (projector-name project))
  (remove-pathname-from-cache path)
  (remove-ckid-resource path))

(defun project-file-unobsolete (project path)
  (mpw-unobsolete (mac-file-namestring path) :project (projector-name project)))

(defun project-file-remove (project path)
  (bind-file-info (remote-checked-out-p remote-user)
                  project path
    (when
      remote-checked-out-p
      (project-file-checked-out remote-user
                                "~/pp-path/ can't be removed because ~s has it checked out."
                                path remote-user))
    (mpw-remove (mac-file-namestring path) :project (projector-name project))
    (remove-pathname-from-cache path)
    (remove-ckid-resource path)))

(defun project-file-cancel-checkout (project path &key convert-to-modro)
  (bind-file-info (local-checked-out-p projector-name remote-checked-out-p remote-user)
                  project path
    (declare (ignore remote-user remote-checked-out-p))
    (unless local-checked-out-p
      (okay-or-cancel-dialog 
                            (format nil "~/pp-path/~%local file is not marked as checked out.~%~
                             Try to cancel checkout anyway?"
                            path) :size #@(350 250)))
#|
    (when remote-checked-out-p
      (restart-case
        (project-file-checked-out remote-user
                                  "Can't cancel checkout because ~/pp-path/ is checked out to: ~s."
                                  path remote-user)
        (cancel-remote-checkout ())))
|#
    (if (not convert-to-modro)
      
      ;;throw away changes and retrieve latest version
      (mpw-checkout path :cancel t :notouch t :yes-to-dialogs t :project projector-name)
      
      ;;keep changes and convert to modro
      ;; @@@ consolidate this with the code in project-cancel-checkout
      (let ((temp-name (temporary-file-name path)))
        ;;kludge around projector limitations -- can't cancel checkout without getting new version
        (copy-file path temp-name)
        (mpw-checkout (mac-namestring path) :cancel t :notouch t :yes-to-dialogs t :project projector-name)
        (delete-file path)
        (rename-file temp-name path)
        (set-file-local-checked-out-p path nil)
        (set-file-modify-read-only path)))))

(defun project-file-cancel-modro (project path)
  (bind-file-info (modrop projector-name local-version)
                  project path
    (unless modrop
      (signal-project-error 'simple-project-error project
                           "Can't cancel ModifyReadOnly because ~/pp-path/ is not ModifyReadOnly."
                           path))
    (delete-file path)
    (mpw-checkout (mac-namestring path) :notouch t :project projector-name :version local-version)))

#|
(defun project-file-versions (project path)
  (mapcan #'(lambda (line)
              (unless (string= line "")
                (multiple-value-bind (name version state)
                                     (name-and-version line)
                  (declare (ignore name))
                  (when (eq state :checked-in)
                    (list version)))))
          (break-string-into-list
           (mpw-projectinfo :file (file-namestring path) :project (projector-name project)
                            :short t))))
|#


(defun project-file-get (project path &key version directory)
  (mpw-checkout (file-namestring path) :project (projector-name project)
                :notouch t :version version :directory (and directory (mac-namestring directory))))

;;;
;;; File information
;;;

(defun projector-file-project-name (path)
  (nth-value 1
             (projector-local-file-info nil path :project-name-request t)))

(defun project-file-local-info (project path)
  (bind-file-info (projector-file-p projector-name modrop
                   local-checked-out-p local-comment local-user local-version)
                  project path
    (when projector-file-p
      (list :projector-name projector-name
            :state (cond (modrop :modro)
                               (local-checked-out-p :checked-out)
                               (t :checked-in))
            :comment local-comment
            :user local-user
            :version local-version
            :allow-other-keys t))))

(defun project-file-remote-info (project path)
  (bind-file-info (remote-checked-out-p remote-comment remote-user remote-version)
                  project path
    (list :state (if remote-checked-out-p :checked-out :checked-in)
          :comment remote-comment
          :user remote-user
          :version remote-version
          :allow-other-keys t)))

(defun project-file-local-state (project path)
  (bind-file-info (projector-file-p modrop local-checked-out-p)
                  project path
    (when projector-file-p
      (cond (modrop :modro)
            (local-checked-out-p :checked-out)
            (t :checked-in)))))

(defun project-file-local-comment (project path)
  (bind-file-info (local-comment) project path
    local-comment))

(defun project-file-local-version (project path)
  (bind-file-info (local-version) project path
    local-version))

(defun project-file-in-project (project path)
  (projector-local-file-info project path))

(defun project-file-versions (project path)
  (let (versions lines)
    (dolist (line (break-string-into-list
                   (mpw-projectinfo :file (file-namestring path)
                                    :project (projector-name project)
                                    :comments t))
                  (nreverse versions))
      (cond ((and (> (length line) 0)
                  (char= (char line 0) #\Space))
             (push line lines))
            (t
             (when lines
               (multiple-value-bind (checked-out-p comment user version)
                                    (parse-projector-file-info (nreverse lines))
                 (push `(:version ,version
                                  :state ,(if checked-out-p :checked-out :checked-in)
                                  :user ,user :comment ,comment) versions)))
             (setq lines (list line)))))))
;;;
;;; Projector name operations
;;;

(defun find-projector-name (projector-name)
  (dolist (project *projects* nil)
    (and (typep project 'mpw-project)
         (string-equal projector-name (projector-name project))
         (return project))))

(defun projector-name-parent-name (projector-name)
  (let ((position (position #\บ projector-name :from-end t
                            :end (- (length projector-name) 2))))
    (when position
      (subseq projector-name 0 (1+ position)))))

(defun projector-name-leaf-name (projector-name)
  (let* ((length (length projector-name))
         (position (position #\บ projector-name :from-end t :end (- length 2))))
    (subseq projector-name (1+ (or position -1)) (1- length))))

;; @@@ this ought to be split into some find-or-make thingy, and
;; an initialize-instance method on mpw-project
(defun intern-projector-name (local-dir projector-name &optional parent)
    (or (find-projector-name projector-name)
        (let ((par (or parent
                       (let ((parent-name (projector-name-parent-name projector-name)))
                         (when parent-name
                           (intern-projector-name
                            (make-pathname :directory (butlast (pathname-directory local-dir)))
                            parent-name))))))
          (if (and par (slot-boundp par 'remote-pathname))
            (make-instance 'mpw-project
              :local-dir local-dir
              :projector-name projector-name
              :parent par
              :remote-pathname (concatenate 'string
                                            (project-remote-pathname par)
                                            (projector-name-leaf-name projector-name)
                                            ":"))
            (make-instance 'mpw-project
              :local-dir local-dir
              :projector-name projector-name
              :parent par)))))
;;;
;;; Project operations
;;;

(defun project-getnewer (project &key verbose)
  (let ((result (mpw-checkout nil :progress t :newer t :notouch t :deleteobsolete t :project (projector-name project))))
    (and verbose result
         (format t "~%~A" result))))

(defun project-info-text (project &rest options &key file &allow-other-keys)
  (apply #'mpw-projectinfo :project (projector-name project)
         :file (and file (file-namestring file))
         options))

(defun mpw-project-mount (project)
  (when (project-root-p project)
    (let* ((remote-dir (project-remote-pathname project))
           (local-dir (namestring (project-local-dir project)))
           (local-dir-len (length local-dir))
           (cur-app-path (mac-namestring (current-application-pathname))))
      ;; For some unknown reason, Projector crashes when trying to
      ;; checkout all into a directory that contains a running application.
      ;;
      ;; Actually, I bet the reason is that Leibniz at some point does a
      ;; projector-local-file-info, on the files in the directory, which involves
      ;; opening its resource fork in order to get the ckid, and then closing it
      ;; again regardless of whether it had been open before the OpenResource
      ;; call was made.
      ;; @@@ this should be fixed.
      (assert (or (< (length cur-app-path) local-dir-len)
                  (not (string-equal local-dir cur-app-path :end1 local-dir-len)))
              nil
              "The running application: ~s cannot be in a directory or subdirectory of a ~
               project directory: ~s." cur-app-path local-dir)
      (do ()
        ((probe-file remote-dir))
        (restart-case
          (signal-project-error 'project-volume-not-available project
                                "Project ~A requires volume ~S to be mounted."
                                project (project-volume project))
          ;; @@@ add a :report for this condition
          (volume-mounted ())))
      (mpw-mountproject (truename remote-dir))
      (mpw-command "CheckoutDir" "-r" local-dir))))

(defun refresh-mpw-projects (mpw-projects)
  (dolist (project *mounted-projects*)
    (when (and (typep project 'mpw-project)
               (project-parent project))
      (project-unmounted project)))
  (dolist (projector-name mpw-projects)
    (let ((project
           (or (find-projector-name projector-name)
               (let* ((parent-name (projector-name-parent-name projector-name))
                      (parent (and parent-name (find-projector-name parent-name))))
                 (when parent
                   (intern-projector-name
                    (make-pathname :directory
                                   (append (pathname-directory (project-local-dir parent))
                                           (list (projector-name-leaf-name projector-name))))
                    projector-name parent))))))
      (when project
        (project-mounted project)))))

(defun mpw-projects-mount (projects &aux (mpw-projects (mpw-mounted-projects)) refresh)
  (dolist (project projects)
    (if (member (projector-name project) mpw-projects :test #'string-equal)
      (project-mounted project)
      (progn
        (project-unmounted project)
        (mpw-project-mount project)
        (setq refresh t))))
  (refresh-mpw-projects (if refresh (mpw-mounted-projects) mpw-projects)))

(defmethod project-mount ((project mpw-project))
  (when (project-root-p project)
    (mpw-project-mount project))
  (apply #'refresh-mpw-projects project)) ; was (refresh-mpw-projects))

(defmethod project-name ((project mpw-project))
  (projector-name project))

(defmethod project-unmount ((project mpw-project))
  (when (project-root-p project)
    (mpw-command "UnmountProject" (projector-name project)))
  (call-next-method)
  (mapc #'project-unmounted (project-children project)))

;;;
;;; Utility functions
;;;

(defun project-newer-files (project)
  (do* ((file-list nil)
        (ckoutdir (namestring (project-local-dir project)))
        (newer-str (project-info-text project :newer t :short t))
        (cur-pos (position #\return newer-str)))
       ((null cur-pos) file-list)
    (let* ((start-pos (position-if-not #'(lambda (xchar) (memq xchar '(#\space #\'))) newer-str :start (1+ cur-pos)))
           (end-pos (position #\, newer-str :start cur-pos)))
      (when (and start-pos end-pos)
        ;; @@@ merge-pathnames
        (let ((path (concatenate 'string ckoutdir (subseq newer-str start-pos end-pos))))
          (when (probe-file path)
            (bind-file-info (modrop) project path
              (unless modrop (push path file-list))))))
      (setq cur-pos (position #\return newer-str :start (1+ cur-pos))))))

(defun project-categorize-modified-files (project)
  (let ((check-in-files ())
        (check-out-and-in-files ())
        (merge-in-files ())
        (wedge-files ())
        (changed-wedge-files ()))
    (do-project-files
     project
     #'(lambda (path)
         (bind-file-info (local-checked-out-p modrop local-version)
                         project path
           (if local-checked-out-p
             (push path check-in-files)
             (if modrop
               (bind-file-info (remote-checked-out-p remote-version) project path
                 (let ((same-version (string-equal local-version remote-version)))
                   (if remote-checked-out-p
                     (if same-version
                       (push path wedge-files)
                       (push path changed-wedge-files))
                     (if same-version
                       (push path check-out-and-in-files)
                       (push path merge-in-files))))))))))
    (values check-in-files check-out-and-in-files merge-in-files wedge-files changed-wedge-files)))

;;; Old change history from projector.lisp:
;;; 03/24/92 jaj  if MPW fails to check in file, warn and keep trying.
;;;               fixed bug in check-in-projects
;;; 02/24/92 jaj  find-latest-leibniz-version works, search for ~~ from end,
;;;               cancel-checkout-dialog is centered, checkout modro passes -y
;;; 02/12/92 alms use y-or-n-dialog function, rather than class.
;;; 1/21/92  jaj  got rid of "foo", use chooser name as default in user-mount-volume,
;;;               error in mount-projects if current app is in project (sub)dir,
;;;               bigger "add to current project" dialog, don't select listener in project-info,
;;;               add-change saves selection and scroll-position, cancel-checkout puts up a
;;;               dialog.  add ccl::*save-all-old-projector-versions*
;;; 01/07/92  gz  correct args to message-dialog.  'fred-window -> *default-editor-class*
;;; 12/6/91  jaj  add missing parameter to warn
;;; 12/5/91  jaj  don't overwrite old comments that have ~~
;;; 11/7/91  jaj  minor cleanup for adding comments
;;; 11/06/91 gz  Don't mess around with *restore-lisp-functions*, it's going away.
;;; 11/05/91 gz  Convert to new traps.
;;;6-nov-91  alms remove center-size-in-screen, don't move selection before saving
;;;6-nov-91  jaj  fix last fix
;;;5-nov-91  jaj  fix stupidity in check-out-file (not getting newer versions)
;;;1-nov-91  jaj  added projector icons to mini-buffer
;;;23-oct-91 jaj  fixed find-project-check-out-dir, simplified logical hosts,
;;;               don't delete buffer in check-out-file
;;;18-oct-91 jaj  almost a complete rewrite
;;;23-sep-01 jaj  incorporated Tom Vrhel's patches
;;;21-aug-91 jaj  many bug fixes and enhancements
;;;14-aug-91 alms don't redefine view-key-event-handler for window
;;;               comment-dialog allows returns
;;;06-aug-91 jaj  fix bugs in cancel-checkout and aux functions

#|
;;; Old change history from projector.lisp:
	Change History (most recent last):
	1	4/29/91	JRM	'add entry to change log in check out active'
	2	4/29/91	lak	'Add in ProjectInfo menu item.'
	3	4/30/91	grf	changed mount-all-projects to accept \"home;\"
				forms... now *my-projects* can include pathnames
				like: \"home;newton:\".
        4	4/30/91	pjp	working on facility that auotmatically appends
        			comments to bottom of file.  Also fixed various
        			bugs
	5	4/30/91	pjp	Fixing the numbering
        			of thse comments at the bottom of the file.
	6	5/6/91	jcg	0.2.5 changes: new pathname form
	7	5/9/91	jcg	+ changing load of ff to use requires
				+ save file after checkout
	8	5/10/91	jcg	ff, ProjectorLib.o loads commented out;
				now part of bootstrapping phase to make
				image
	9	5/10/91	jcg	more fixes
	10	5/14/91	pjp	add new items to the projectinfo menu item:
				"Modifiable on my disk"
				"History of Active window"
				Also added new hierarchical menu "Rarely used stuff":
				"Cancel Checkout Active"
	4	5/30/91	ads	Adding comments to "Currently Checked Out"
                                Modifying ProjectInfo-Active Window
                                Fixing bug in Cancel Active Checkout
	5	5/31/91	tv 	Added user protections before asking for comments
				
	6	5/31/91	tv 	fixing checkinActive free variable
				
	7	6/3/91	tv 	Changed MakeCurProjectMenu to protect from double entries
				
	8	6/3/91	jcg	moving over to Leibniz 1.0:
				  + file goes in ralphdebug package (adbg)
	11	6/6/91	   	more bug fixes
	12	6/6/91	   	changing menu updates, layouts
	13	6/6/91	   	testing cancel active
	14	6/10/91	tv 	fixing word wrap in static-text-dialog-item
	15	6/10/91	tv 	testing checkin/checkout for 1.1d06
	17	6/17/91	tv 	menu deletion hack
	18	6/17/91	tv 	testing menu hack
	19	6/17/91	tv 	testing menu hack
	20	6/17/91	tv 	testing before release
	21	6/17/91	tv 	Fix for updating with checkin all and checkin current project
	22	6/25/91	tv 	fixing for ralph opens
	23	6/25/91	tv 	Fixed current-project switch
	24	6/25/91	tv 	changing to be compatible with new menubars
	25	6/25/91	tv 	fixing menubars
	26	6/25/91	tv 	sanity check - 1.1d15 release
	27	7/17/91	jaj 	*user-initials* and *my-projects* are retrieved by functions
        0       8/05/91 gz      Use symbolic names for read-only states.
	2	9/12/91	alms	Fix bug in unquoted project names
	4	10/18/91	jaj	test
	4	10/18/91	jaj	test
	1	10/18/91	jaj	many changes
	2	10/18/91	jaj	test2, in
	3	10/18/91	jaj	test2, in
	4	10/18/91	jaj	fix to update-leibniz-version
	6	10/23/91	jaj	minor fix in update-leibniz-version
	8	10/31/91	gsb	Don't produce ratios when computing screen positions
	8	10/31/91	gsb	yes
	10	11/5/91	jaj	fix stupidity in check-out-file (not getting newer
				versions)
	11	11/6/91	jaj	fix to last fix
	14	11/8/91	gsb	Put correct newlines in auto-inserted comments.
	14	11/8/91	gsb	Don't insert comment lines without a newline.
	16	1/6/92	jh	Fix dependency on logical pathname
	19	2/19/92	alms	fix y-or-n-dialog.
	20	2/24/92	jaj	bug fixes
	22	3/12/92	ows	only ignore projects that aren't in *my-projects*
	23	3/13/92	ows	compare directory names with string-equal, for case-independence
	24	3/24/92	jaj	fix bugs
	25	3/24/92	jaj	bug fix
	26	3/24/92	jaj	test
	27	3/24/92	jaj	test
	28	3/25/92	ows	switched use of fprojector to new-style mpw-command
				made cancel dialog bigger
	29	3/27/92	ows	(my-projects) updates the "Update Leibniz Version" menu item now.
				Fixed the ธีs menu item.
|#

#|
;;; Old change history from Cambridge:
	Change History (most recent last):
	1	4/3/92	ows	split from projector.lisp
	2	4/3/92	ows	split from projector.lisp
	4	4/3/92	ows	fix some problems with checkout and cancel-checkout
	6	4/7/92	ows	changed the bug address
	10	4/24/92	ows	allow file-checkout of files that dont exist locally
	11	4/29/92	ows	with-open-resource-file -> with-open-res-file (see note)
|#

#|
	Change History (most recent last):
        1	3/26/93 sidney  remove mpw-quote, which isn't correct for sourceserver interface
	3	3/30/93	sidney	Make sure a new subproject has a valid remote
				pathname derived from its parent before the
				initialize method is called when it is created.
				Fixes a problem when there are spaces in the
				remote pathname of the base project.
				Touch files when checking in and not on checkout
	4	3/30/93	sidney	change name of variable from char to xchar to
				avoid possible name collisions
	5	3/31/93	Sidney	Add Obsolete File command to menu
	6	3/31/93	Sidney	Pass correct file name to Obsolete File
        7       4/4/93  Sidney  Use SourceServer package instead of CCL to
                                minimize possibility of name collision by
                                users.
                                Switch to MCL 2.0 trap interface calls instead
                                of now undocumented 1.3 calls
	8	4/4/93	Sidney	Get comments in file and version control in synch
	9	4/26/93	Sidney	Don't close a resource file that was already open when
                                we opened it
				Use Get1Resource instead of GetResource to look up the
                                CKID resource in the correct file
	10	4/26/93	Sidney	Fix a typo
	11	9/1/93	Sidney	Add menu items for unobsolete and remove files, and allow for checkin of files without CKID resource
	12	9/1/93	Sidney	Allow cancel of checkout when local file is not marked as checked out
	13	2/21/94	sidney	Use the functional interface to resource traps that is already defined
				in MCL instead of hacking up calls to the traps.
	14	2/21/94	sidney	Patch a bug in the Resources library
	15	3/2/94	sidney	Fix problem with files that don't have a resource fork
	16	5/1/94	sidney	Radar bug #1157940, made uninformative error message more informative
	17	5/2/94	sidney	whoops. screwed up check for errorp in last fix.
	18	7/7/94	sidney	Use string for resource id instead of symbol
	19	7/15/94	sidney	move some macro definitions to earlier in the build
	20 	 9/ 7/94	sidney  	delete obsolete files when updating a project
	21 	 9/ 8/94	sidney  	fix Other File | Remove to work with orphaned, obsoleted files
	22 	 2/14/95	sidney  	openresfilenums is now defined as part of the patches to the base MCL
	3  	 6/ 8/95	sidney  	Fix a few places where mac-file-namestring should have been called to prevent breaking on file names with "." in them
	4  	 6/18/95	sidney  	add restart to add a file to a project when there is such a file in the project, but the copy being checked in didn't come from there
|# ;(do not edit past this line!!)
