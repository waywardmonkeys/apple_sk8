;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#|
	sourceserver-command.lisp

	copyright © 1992, Apple Computer, Inc.

	Call sourceserver-command with a bunch of strings which are "tokens" that 
	SourceServer understands. It will retunr the reply, or error.

	Example:

	(sourceserver-command "NewProject" "-u" "derek" "-cs" "testing" "testProject")

	Impl. Notes:
	
	Note that the SourceServer documentation is wrong or misleading in many instances,
	and the interface itself is screwy (mostly for historical reasons). The part about
	the AppleEvent Error# being 2 bytes instead of 4 and strings needing nulls will
	probaly be fixed in future versions of SourceServer.
|#
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(in-package :SourceServer)

(defconstant $projector-signature "MPSP")
(defconstant $projector-canonical-name "SourceServer")

(defconstant $projector-msg-class :|MPSP|)
(defconstant $projector-msg-id :|cmnd|)
(defconstant $projector-status :|stat|)
(defconstant $projector-diag :|diag|)

(defparameter *projector-target* nil)
(defvar *projector-path* nil)

(def-load-pointers clear-projector-comm-vars ()
  (setq *projector-target* nil))

(defun find-appl (appl-name)
  (message-dialog (concatenate 'string "Please locate " appl-name "."))
  (choose-file-dialog :button-string "Launch" :mac-file-type "APPL"))

(defun find-and-launch-appl (appl-name)
  (let ((path *projector-path*))
    (unless path 
      (setq path (or (find-application-for-creator $projector-signature)
                     (find-appl appl-name))))
    (handler-case (sublaunch path nil t)
      (error () (progn
                   (setq path (find-appl appl-name))
                   (sublaunch path nil t))))))

(let ((*warn-if-redefine* nil))
  (defun chooser-name ()
    "returns macintosh name, else chooser name, else \"unspecified\""
    (let ((h (#_GetString -16096)))
      (unless (or (%null-ptr-p h) (%null-ptr-p (%get-ptr h)) (eql 0 (%get-byte (%get-ptr h))))
        (%get-string h))))
  
  (defun user-initials ()
    (unless (and (boundp '*user-initials*) *user-initials*)
      (setq *user-initials* (get-string-from-user "What is your name?" :initial-string (chooser-name))))
    (unless (and (boundp '*user*) *user*)
      (setf *user* *user-initials*))
    *user-initials*)
  )

(defun forget-projector-target ()
  (when *projector-target*
    (unless (%null-ptr-p (rref *projector-target* aedesc.datahandle))
      (require-trap #_aedisposedesc *projector-target*))
    (dispose-record *projector-target* :aeaddressdesc)
    (setq *projector-target* nil)))

(defun set-projector-psn-target (psnhigh psnlow)
  (forget-projector-target)
  (setf *projector-target* (make-record (:aeaddressdesc :clear t)))
  (AppleEvents:create-psn-target *projector-target* psnhigh psnlow))

(defun projector-target (&optional dont-start-it)
  (or *projector-target*
      (progn
        (unless dont-start-it  ;; this is a hack to ensure that we have user-intials defined anytime we really want to have sourceserver running
          (user-initials))   ;; but when we don't care if sourceserver is running (like when we are quitting it) we don't care about initials
        (loop
          (multiple-value-bind (psnhigh psnlow) (AppleEvents:find-process-with-signature $projector-msg-class)
            (if psnhigh
              (return (set-projector-psn-target psnhigh psnlow))
              (if dont-start-it
                (return nil)
                (find-and-launch-appl $projector-canonical-name)))))
        *projector-target*)))

(defun parse-projector-reply (reply)
  "Returns output (text), status (number), errn (number), and diagnaostic (text)."
  (let ((output (mpw-unquote (ccl::ae-get-parameter-char reply #$keyDirectObject nil)))
        (diag (ccl::ae-get-parameter-char reply $projector-diag nil)) 
        (errn (ccl::ae-get-parameter-longinteger reply #$keyErrorNumber nil)) 
        (status (ccl::ae-get-parameter-longinteger reply $projector-status nil)))
    (check-required-params "" reply)
    (values output status errn diag)))

(defun ae-replace-target (the-event the-target)
  (ae-error (#_AEPutAttributeDesc the-event #$keyAddressAttr the-target)))

(defun create-string-list (the-desc list-strings)
  (labels ((foo (more-strings)
             (dolist (str more-strings)
               (when str
                 (if (consp str)
                   (foo str)
                   (with-cstrs ((c-string str))
                     (#_AEPutPtr the-desc 0 #$typeChar c-string
                      (1+ (length str)))))))))                            
    (ae-error (#_AECreateList (%null-ptr) 0 nil the-desc))
    (dolist (a-str list-strings)
      (when a-str
        (if (consp a-str)
          (foo a-str)
          (with-cstrs ((c-string a-str))	; the nulls are gratuitous except that SourceServer from ETO#7 needs them.
            (#_AEPutPtr the-desc 0 #$typeChar c-string
             (1+ (length a-str))))))
      the-desc)))

; Hacked version of get-error-number that won't barf 
; because SourceServer sent a two-byte error code!

(defun ae-get-parameter-some-integer (the-desc keyword &optional (errorp t))
  (rlet ((buffer :signed-long)
         (typecode :desctype)
         (actualsize :size))
    (ccl::ae-errorp-handler
      errorp
      (ae-error (#_aegetparamptr the-desc keyword
                 #$typeLongInteger typecode buffer
                 (record-length :signed-long) actualsize)))
    (ecase (%get-signed-long actualsize)
      (1 (%get-signed-byte buffer))
      (2 (%get-signed-word buffer))
      (4 (%get-signed-long buffer)))))

(let ((*warn-if-redefine* nil))
  (defun get-error-number (the-desc &optional (errorp t))
    (ae-get-parameter-some-integer the-desc #$keyErrorNumber errorp))
  )

;;; MCL3.0
(defun sourceserver-command (&rest arg-strings)
  (ccl::without-event-processing ; why?
    (without-interrupts 
     (with-cursor *watch-cursor*
       (with-aedescs (ae reply ae-args)
         (AppleEvents:create-appleevent ae $projector-msg-class $projector-msg-id (projector-target))
         
         (create-string-list ae-args arg-strings)
         (#_AEPutParamDesc ae #$keyDirectObject ae-args)
         (setq *idle* t)
         (handler-case
           (AppleEvents:send-appleevent ae reply :reply-mode :wait-reply :timeout #$kNoTimeOut)
           (appleevent-error (the-error)
                             (case (ccl::oserr the-error) 
                               ((-600 -906) 			; -600 = procNotFound
                                (progn
                                  (forget-projector-target)
                                  (ae-replace-target ae (projector-target))
                                  (AppleEvents:send-appleevent ae reply :reply-mode :wait-reply :timeout #$kNoTimeOut)))
                               (t (error "An Error ~D, ~A. " (ccl::oserr the-error) (ccl::error-string the-error))))))
         (setq *idle* nil)
         (multiple-value-bind (output status errno diagnostic) 
                              (parse-projector-reply reply)
           (unless (and (zerop status) #| (null diagnostic) |# (zerop errno))
             (if diagnostic
               (progn (cerror "Ignore error" "~A (Status = ~D, Error = ~D)." (string-right-trim '(#\newline) diagnostic) status errno)
                      (return-from sourceserver-command))
               (cerror "Ignore error" "No diagnostic message (Status = ~D, Error = ~D)" status errno)))
           (or output diagnostic)))))))

(defun quit-source-server ()
  (let ((target (projector-target t))) ;; don't start projector app if it isn't already running
    (when target
      (with-aedescs (event reply)
        (AppleEvents:create-quit event target)
        (AppleEvents:send-appleevent event reply :reply-mode :no-reply))
      (forget-projector-target))))

(defvar *server-menu* (make-instance 'menu :menu-title "Projects"))
(defvar *my-projects* nil)
(defvar *source-directory* nil)

#|
; here is a much simpler interface (than projector interface) which has a different notion
; of the contents of *my-projects*
(defun server-setup ()
  (let ((menu *server-menu*))
    (add-new-item menu "Mount Project…"
                  'server-mount)
    (add-new-item menu "CheckoutDir…"
                  'server-checkoutdir)
    (add-new-item menu "Checkout Modify…"
                  'server-checkout-m)
    (add-new-item menu  "Checkout RO…"
                  'server-checkout-ro)
    (add-new-item menu "CheckIn…"
                  'server-checkin)
    (menu-install menu)))


(defun server-mount ()
  (catch-cancel
    (let ((dir  (choose-directory-dialog)))
         (when (and dir (typep dir '(or pathname string)))
           (setq dir (mac-namestring dir))
           (push dir *projects*)
           (sourceserver-command "mountproject" dir)))))

(defun server-checkoutdir ()
  (catch-cancel
    (let ((dir  (choose-directory-dialog)))
      (when (and dir (typep dir '(or string pathname)))
        (setq *source-directory* (mac-namestring dir))
        (sourceserver-command "checkoutdir" *source-directory*)))))

(defun server-checkout-m ()
  (server-checkout "-m"))
(defun server-checkout-ro ()
  (server-checkout ""))

(defun server-checkout (option)
  (catch-cancel
    (let ((file  (choose-file-dialog :directory (car *my-projects*))))
      (when (and file (typep file '(or string pathname)))
        (setq file (mac-file-namestring file))
        (sourceserver-command "checkout" "-u" *user* "-project" (car *my-projects*) option file)))))

(defun server-checkin ()
  (catch-cancel 
    (let ((file (choose-file-dialog :directory (car *projects*))))
      (when (and file (typep file '(or string pathname)))
        (setq file (mac-file-namestring file))
        (sourceserver-command "checkin" "-u" *user* "-project" (car *my-projects*)  file)))))
|#

(let ((*warn-if-redefine* nil))
  (defun mpw-command (&rest args)
    (apply #'sourceserver-command args))
  )


;(unless (find-menu "Projects") (server-setup))

#|

(sourceserver-command "directory")
; sets the directory to receive the checked out files
(sourceserver-command "checkoutdir")
(sourceserver-command "MountProject")

(sourceserver-command "Project")
(sourceserver-command "ProjectInfo")


(sourceserver-command "NewProject" "-u" "derek" "-cs" "testing" "testProject")




(sourceserver-command "mountproject" "27B/6:Derek:Dyanmo:")
|#

(defun simple-mpw-project-mount (project-path dst-path)
  (sourceserver-command "MountProject" (mac-namestring project-path))
  (sourceserver-command "CheckoutDir" "-r" (mac-namestring dst-path)))

#|
	Change History (most recent last):
	3	3/30/93	sidney	Remove apostrophes from SourceServer reply
				strings that have embedded spaces
	4	4/4/93	Sidney	Use SourceServer package instead of CCL and use
				MCL 2.0 low-level system calls instead of ones
				from verison 1.3 that required loading a verison of
				sysequ.lisp that could lead to name collisions in
				the ccl package.
	5	4/12/93	SIdney	AppleEvent code put in its own package
	6	4/26/93	Sidney	Disable interrupts in critical section of processing Apple events
	7	9/1/93	Sidney	Don't signal error because of diagnostic output from SourceServer when error and status codes are 0
	8	3/1/94	kleiman	SourceServer will now ask for your name when
				coming up if no name is specified.
	9	3/1/94	kleiman	Moved defvar of *user* to the top of the file since it is now used by user-initials.
	10	7/7/94	sidney	file creator is now a string instead of a keyword symbol
	11 	 9/ 7/94	sidney  	get user name before launching sourceserver app
	12 	 9/ 7/94	sidney  	move defvar *user* to earlier in load, to projector-utilities
	13 	 3/19/95	sidney  	quit sourceserver on reset so it isn't hanging around all the time
	14 	 3/19/95	sidney  	fix some build problems regarding initializing *user-initials*
|# ;(do not edit past this line!!)
