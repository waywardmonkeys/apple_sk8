;;; 03/24/92 jaj mpw-command now checks MPW status
;;; 01/21/92 jaj added restart around sending appleevent
;;; 01/15/92 gz  In case of multiple MPW processes, prefer the *mpw-path* one.
; 1/3/92   jaj   added current-application-pathname, chooser-name
;;; 11/05/91 gz  Convert to new traps.
;16-sep-91 jaj  handle aborts during mpw error
;12-sep-91 alms/jaj  fix to speed up processing of MPW commands
;15-aug-91 jaj  add *mpw-path*, use or set *user-initials*.  turn #\return into ¶n
; 6-aug-91 jaj  put up watch cursor and disable event processing while doing mpw-command


(in-package :SourceServer)

(defconstant $mpw-signature "MPS ")
(defconstant $mpw-msg-class :|MPS |)
(defparameter *mpw-target* nil)
(defparameter *mpw-name* nil)
(defparameter *mpw-comm-file* nil)
(defparameter *mpw-error-file* nil)
(defparameter *mpw-status-file* nil)
(defparameter *mpw-done-file* nil)
(defvar *mpw-path* nil)
;(defvar *user-initials*)

(defun mpw-comm-file ()
  (or *mpw-comm-file*
      (setq *mpw-comm-file*
            (mac-namestring (merge-pathnames "mpw-to-mcl" (or (find-folder "temp") "ccl:"))))))

(defun mpw-error-file ()
  (or *mpw-error-file*
      (setq *mpw-error-file*
            (mac-namestring (merge-pathnames "mpw-errors" (or (find-folder "temp") "ccl:"))))))

(defun mpw-status-file ()
  (or *mpw-status-file*
      (setq *mpw-status-file*
            (mac-namestring (merge-pathnames "mpw-status" (or (find-folder "temp") "ccl:"))))))

(defun mpw-done-file ()
  (or *mpw-done-file*
      (setq *mpw-done-file*
            (mac-namestring (merge-pathnames "mpw-done" (or (find-folder "temp") "ccl:"))))))

(def-load-pointers clear-mpw-comm-vars ()
  (setq *mpw-comm-file* nil
        *mpw-error-file* nil
        *mpw-status-file* nil
        *mpw-done-file* nil
        *mpw-target* nil
        *mpw-name* nil))

(defun choose-mpw-target ()
  (loop
    (with-simple-restart
      (nil "Try connecting to ~S again." *mpw-path*)
      (labels ((try-to-connect ()
                 (multiple-value-bind (psnhigh psnlow path) (find-mpw-process)
                   (when psnhigh
                     (verify-mpw-version path)
                     (return-from choose-mpw-target (set-mpw-psn-target psnhigh psnlow))))))
        (try-to-connect)
        (let ((path *mpw-path*))
          (unless (and path (probe-file path))
            (message-dialog "Please locate MPW Shell (3.3 or later)")
            (setq path (choose-file-dialog :button-string "Launch"
                                           :mac-file-type "APPL"
                                           :mac-file-creator $mpw-signature)
                  *mpw-path* path))
          (verify-mpw-version path)
          (loop
            (handler-case (return (sublaunch path nil t))
              (simple-error (c)
               (if (string= (simple-condition-format-string c)
                            "Not enough room in heap zone.")
                 (cerror (format nil "Try launching ~S again." path)
                         "Not enough system memory to launch ~S." path)
                 (signal c)))))
          (loop 
            (event-dispatch)
            (if (mpw-idle-p) (return)))
        (if (and (boundp '*user-initials*) *user-initials*)
          (old-mpw-command (concatenate 'string "set user '" *user-initials* "'")))))))
  (unless (and (boundp '*user-initials*) *user-initials*)
    (let ((str (old-mpw-command "echo {user}")))
      (setq *user-initials* (subseq str 0 (1- (length str)))))))

(defun verify-mpw-target ()
  (unless *mpw-target*
    (choose-mpw-target)))

(defun verify-mpw-version (path)
  (unless (and (eq $mpw-signature (mac-file-creator path))
               (eq "APPL" (mac-file-type path)))
    (error "~s is not an MPW Shell version 3.3 or later" path))
  (setq *mpw-name* (mac-file-namestring path)))

(defun forget-mpw-target ()
  (when *mpw-target*
    (dispose-record *mpw-target* :aeaddressdesc)
    (setq *mpw-target* nil)))

(defun set-mpw-psn-target (psnhigh psnlow)
  (let ((success nil))
    (unless *mpw-target*
      (setf *mpw-target* (make-record (:aeaddressdesc :clear t))))
    (unwind-protect
      (progn
        (AppleEvents:create-psn-target *mpw-target* psnhigh psnlow)
        (setq success t))
      (unless success
        (forget-mpw-target)))))

(defun delete-file-until-not-busy (filename)
   ;; Delete a file.  It's okay if the file doesn't exist; if it does, and it's still
   ;; busy, try forever until it's released.  Used by mpw-command for synchronization.
   (loop
      (handler-case (delete-file filename)
         (file-error (condition)
           ;; The string= is a terrible, terrible kludge.  It wants a condition system that
           ;; includes different file errors or at least has a slot for the error code in
           ;; order to do better
           (if (string= (slot-value condition 'ccl::error-type)
                             "File ~^~S is busy or locked.")
               (event-dispatch t)
               (error condition)))
         (:no-error (value)
           (declare (ignore value))
           (return)))))

(defun mpw-basic-send (str)
   (with-aedescs (ae reply)
       (AppleEvents:create-appleevent ae $mpw-msg-class :|scpt| *mpw-target*)
       (ccl::ae-put-parameter-char ae #$keyDirectObject str)
       (with-simple-restart
           (retry-send-appleevent "Send AppleEvent again.")
           (AppleEvents:send-appleevent ae reply))))

;;; MCL3.0
(defun old-mpw-command (command-line)
  (verify-mpw-target)
  (let* ((temp-file (mpw-comm-file))
         (err-file (mpw-error-file))
         (status-file (mpw-status-file))
         (done-file (mpw-done-file)))
    (when (probe-file done-file)
      (%stack-block ((p 18))
        (get-next-event p t 0 60))
      (delete-file-until-not-busy done-file))
    (ccl::without-event-processing
     (with-cursor *watch-cursor*
       (mpw-basic-send (format nil "~a > ~A ³ ~A"
                               command-line temp-file err-file))
       (mpw-basic-send (format nil "echo {status} > ~A; echo > ~A"
                               status-file done-file))
       (loop
         (%stack-block ((p 18))
           (get-next-event p t 0 60))
         (if (probe-file done-file) (return)))))
    (let ((status (read-from-string (file-to-string status-file))))
      (if (eq status 0)
        (if (neq 0 (file-size temp-file))
          (file-to-string temp-file)
          nil)
        (let ((message (if (eq 0 (file-size err-file)) "No Message" (file-to-string err-file))))
          (error "MPW Error status: ~a message:~a" status message))))))

(defun file-size (path)
  (ccl::%stack-iopb (pb np)
    (ccl::%path-to-iopb path pb :errchk)
    (#_PBHGetFInfoSync pb)
    ;;(%get-long pb $ioFlLgLen)
    (pref pb :hParamBlockRec.ioFlLgLen)
    ))

(defun file-to-string (path)
  (let ((size (file-size path)))
    (ccl::%stack-iopb (pb np)
      (ccl::%path-to-iopb path pb :errchk)
      (%vstack-block (buff size)
        (#_PBHOpenSync :errchk pb)
;;        (%put-ptr pb buff $ioBuffer)
;;        (%put-long pb size $ioReqCount)
;;        (%put-word pb #$fsAtMark $ioPosMode)
        (rset pb :ParamBlockRec.ioBuffer buff)
        (rset pb :ParamBlockRec.ioReqCount size)
        (rset pb :ParamBlockRec.ioPosMode #$fsAtMark)
        (#_PBReadSync pb)
        (#_PBCloseSync pb)
        (ccl::%str-from-ptr buff size)))))

(defun find-mpw-process ()
  (let ((mpw-path (full-pathname *mpw-path*))
        last-high last-low last-path)
    (rlet ((pinfo ProCessInfoRec)
           (fss fsspec)
           (psn ProcessSerialNumber)
           (procname (:string 32)))
      (rset pinfo :pRocessInfoRec.proCessNamE procname)
      (rset pinfo :ProcessInfoRec.processAppSpec fss)
      (rset psn :ProceSsSerialNumber.highLongOfPSN #$kNoProcess)
      (rset psn :ProceSsSerialNumber.lowLongOfPSN #$kNoProcess)
      (loop
        (unless (eql #$NoErr (#_GetNextProcess psn)) (return))
        (rset pinfo :processInfoRec.ProcessInfoLength #x3c)
        (unless (eql #$NoErr (#_GetProcessInformation psn pinfo)) (return))
        (when (eq  $mpw-msg-class (rref pinfo :ProcessInfoRec.processSignature))
          (setq last-high (rref psn :ProceSsSerialNumber.highLongOfPSN)
                last-low (rref psn :ProceSsSerialNumber.lowLongOfPSN)
                last-path (%path-from-fsspec (rref pinfo :ProcessInfoRec.processAppSpec)))
          (when (equalp last-path mpw-path)
            (return)))))
    (values last-high last-low last-path)))

(defun current-application-pathname ()
  (rlet ((pinfo ProcessInfoRec)
         (fss fsspec)
         (psn ProcessSerialNumber))
    (%stack-block ((procname 33))
      (#_GetCurrentProcess psn)
      (rset pinfo :processInfoRec.processName procname)
      (rset pinfo :processInfoRec.processAppSpec fss)
      (rset pinfo :processInfoRec.processInfoLength #x3c)
      (#_GetProcessInformation psn pinfo)
      (%path-from-fsspec (rref pinfo :ProcessInfoRec.processAppSpec)))))

(defun chooser-name ()
  "returns macintosh name, else chooser name, else \"unspecified\""
  (let ((h (#_GetString -16096)))
    (unless (or (%null-ptr-p h) (%null-ptr-p (%get-ptr h)) (eql 0 (%get-byte (%get-ptr h))))
      (%get-string h))))

(defun mpw-idle-p (&optional (transaction-id #$kAnyTransactionID))
  (verify-mpw-target)
  (with-aedescs (ae reply)
    (AppleEvents:create-appleevent ae $mpw-msg-class :|stat| *mpw-target* :transaction-id transaction-id)
    (AppleEvents:send-appleevent ae reply :reply-mode :wait-reply)
    (ae-error (get-error-number reply))
    (equalp *mpw-name* (ccl::ae-get-parameter-char reply :|who |))))

;;; Functions beneath here are from my own Projector system.
;;; I haven't tried to integrate them with the functions above;
;;; I've only added them where the functions above weren't doing
;;; the right thing.  I'll probably migrate towards something like this
;;; that splits out talking-to-mpw functionality and knowledge of MPW
;;; command strings from Projector functionality, to make it easier
;;; to replace the latter with POGO as a first step towards database
;;; migration. -- ows 3/5/92

(export '(mpw-command))

(defvar *mpw-trace* nil)
(defvar *mpw-magic-characters* "\"'#\\/{}[]`?Å*+ÇÈ<>³·É¶")
(defvar *mpw-quotable-characters*
   #.(concatenate 'simple-string '(#\¶ #\' #\Newline)))

(defun mpw-unquote-stream (stream)
   "Unquote a stream that's returned by MPW"
   (with-output-to-string (output)
     (do (char)
         ((null (setq char (read-char stream nil))))
       (case char
         (#\¶ (if (setq char (read-char stream nil))
                (write-char char output)
                (return nil)))
         (#\' (do (char)
                  ((or (null (setq char (read-char stream nil)))
                       (char= char #\')
                       (and (char= char #\¶) (null (setq char (read-char stream nil))))))
              (write-char char output)))
         (t (write-char char output))))))

(defun mpw-unquote (string)
   "Unquote a string that's returned by MPW"
   (when string
     (with-input-from-string (stream string)
       (mpw-unquote-stream stream))))

(defun append-mpw-args (&rest args)
   "Paste the arguments together with spaces in between.  Each
argument can either be nil, in which case it's ignored, a list, in
which case each of its elements is recursively pasted into the stream,
or any other item, which is simply printed."
   (with-output-to-string (stream)
       (labels ((print-args (args)
                       (when args
                           (let ((arg (first args)))
                              (typecase arg
                                 (null)
                                 (list (print-args arg))
                                 (t (princ #\Space stream)
                                     (princ (first args) stream))))
                           (print-args (rest args)))))
          (print-args args))))

(defun mpw-command (&rest args)
   "Send a list of arguments to MPW, and wait for a reply.  Return the reply string,
or nil if it's empty.  If an error occurred, signal that instead.  If *mpw-trace* is
non-nil, echo the command."
   (let ((send-text (append-mpw-args args)))
      (when *mpw-trace* (print send-text))
      (loop
         (restart-case
            (handler-case (return (old-mpw-command send-text))
              (appleevent-error (c)
               (when (eq (slot-value c 'ccl::oserr) #$procNotFound)
                 (progn
                   (cerror "Reconnect to MPW." "The MPW process was not found.")
                   (invoke-restart 'reconnect-to-mpw))
                 (signal c))))
           (resend-mpw-command ()
           :report (lambda (stream) (princ "Send MPW command again." stream)))
           (reconnect-to-mpw ()
            :report (lambda (stream) (princ "Reconnect to MPW." stream))
            (choose-mpw-target))))))

;;;
;;; Projector primitives
;;;

(export '(mpw-mount-volume mpw-projectinfo mpw-checkout mpw-checkin mpw-mountproject))

(defun mpw-argument (switch setting)
  "If setting is non-nil, return it as an MPW argument of type switch."
  (and setting (list switch setting)))

(defun make-file-and-version-name (file version)
  "Construct a Projector-style name for the specified version, if any, of the file."
  (let ((name (mac-namestring file)))
    (if version
      (format nil "~A,~A" name version)
      name)))

(defun mpw-mount-volume (path &key (user *user*) password)
  "Mount a volume with MPW."
  (assert (or password (not user)) nil "Password required")
  (mpw-command "Choose"
               (mpw-argument "-pw" password)
               (mpw-argument "-u" user)
               (and (not user) (not password) "-guest")
               path))

(defun mpw-projectinfo (&key file version checked-out comments latest log newer
                             project recursive short file-info)
  "Low-level access to the ProjectInfo command."
  (mpw-command "ProjectInfo"
               (mpw-argument "-project" project)
               (and comments "-comments")
               (and latest "-latest")
               (and newer "-newer")
               (and file-info "-f")
               (and recursive "-r")
               (and short "-s")
               ; (and only "-only")
               (and checked-out "-m")
               (and log "-log")
               ; (mpw-argument "-a" revised-by)
               ; (mpw-argument "-af" created-by)
               ; (mpw-argument "-d" revision-dates)
               ; (mpw-argument "-df" file-dates)
               ; (mpw-argument "-c" revision-pattern)
               ; (mpw-argument "-cf" file-pattern)
               ; (mpw-argument "-t" task-pattern)
               ; (mpw-argument "-n" name)
               ; (and update "-update")
               (and file
                    (make-file-and-version-name file version))))

(defun mpw-checkout (file &key comment directory modifiable version project newer deleteobsolete
                          update notouch progress cancel yes-to-dialogs (user *user*))
  "Medium-level access to the Checkout command."
  (mpw-command "CheckOut"
               ; (and all "-a")
               ; (and branch "-b")
               ; (and cancel-conflict-dialog "-c")
               (and cancel "-cancel")
               ; (and comment-file "-cf")
               ; (and close-window "-close")
               (mpw-argument "-cs" comment)
               (mpw-argument "-d" directory)
               (and modifiable "-m")
               ; (and no-to-dialogs "-n")
               (and newer "-newer")
               (and deleteobsolete "-deleteobsolete")
               (and update "-update")
               (and notouch "-noTouch")
               ; (and open "-open")
               (and progress "-p")
               (mpw-argument "-project" project)
               ; (and recursive "-r")
               ; (mpw-argument "-t" task)
               (mpw-argument "-u" user)
               ; (and update "-update")
               ; (and open-window "-w")
               (and yes-to-dialogs "-y")               
               (if (listp file)
                 file
                 (make-file-and-version-name file version))))

(defun mpw-checkin (file &key project comment touch new yes-to-dialogs (user *user*))
  (mpw-command "CheckIn"
               (and new "-new")
               (and yes-to-dialogs "-y")
               (and touch "-touch")
               (mpw-argument "-cs" comment)
               (mpw-argument "-project" project)
               (mpw-argument "-u" user)
               file))

(defun mpw-obsolete (file &key project (user *user*))
  (mpw-command "ObsoleteProjectorFile"
               (mpw-argument "-project" project)
               (mpw-argument "-u" user)
               file))

(defun mpw-unobsolete (file &key project (user *user*))
  (mpw-command "UnObsoleteProjectorFile"
               (mpw-argument "-project" project)
               (mpw-argument "-u" user)
               file))

(defun mpw-remove (file &key project (user *user*))
  (mpw-command "DeleteRevisions"
               file
               (mpw-argument "-project" project)
               (mpw-argument "-u" user)
               "-file"
               "-y"))

(defun mpw-mountproject (path &key project-paths recursive short)
  (mpw-command "MountProject"
               (and project-paths "-pp")
               (and recursive "-r")
               (and short "-s")
               (when path
                 (namestring path))))

#|
	Change History (most recent last):
	2	9/12/91	alms	allow better background processing during mpw-command
	7	3/12/92	ows	bring over some functions from my own system
	8	3/24/92	jaj	fix bugs
	9	3/25/92	ows	Fixed a bug where mpw-command errored because it was trying to delete the "done" file before MPW had closed it.
				Moved the code to add the Leibniz-version menu item to projector-menus.
				Switch over to the new mpw-command format.
	10	4/3/92	ows	add restarts
				look for the "done" file in a separate command
	11	4/3/92	ows	move the mpw-xxx commands here
	12	4/7/92	ows	mpw-quote quotes empty strings too
	4	3/30/93	sidney	allow continuing from the case that there wasn't enough memory to launch MPW
				add yes-to-dialogs as an option to mpw-checkin
				33/30/93sidneyTouch files when checking in and not on checkout
				
				Remove apostrophes from SourceServer reply
				strings that have embedded spaces
	5	3/31/93	Sidney	Add Obsolete File command to menu
	6	4/2/93	Sidney	Fix unquote bug when a string contains both
				spaces and the MPW quote character
	7	4/4/93	Sidney	Use SourceServer package instead of CCL to
				minimize possibility of name collision by users.
				
				Switch to MCL 2.0 trap interface calls instead of
				now undocumented 1.3 calls
	8	4/4/93	Sidney	Fix some undefined references that needed an
				explicit package modifier
	9	4/12/93	SIdney	Put AppleEvent code in its own package.
	10	9/1/93	Sidney	Add menu items for unobsolete and remove files, and allow for checkin of files without CKID resource
	11	7/7/94	sidney	Use strings for file creator and type
	12 	 9/ 7/94	sidney  	use user argument everywhere it should be
	13 	 9/ 7/94	sidney  	delete obsolete files when updating a project
	14 	 4/11/95	Hernan  	fix checkout and get info of files with embedded dots in name
	3  	 7/ 1/96	sidney  	modernize to MCL 3.9
|# ;(do not edit past this line!!)
