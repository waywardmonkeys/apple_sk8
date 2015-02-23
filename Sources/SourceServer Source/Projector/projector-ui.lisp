;;;; The more user-interface oriented parts of the source code control system:
;;;; dialogs, menus, window manipulation functions, and the high-level
;;;; source code control functions that tie all these together with the
;;;; low-level functions (in project and projector).
;;;;
;;;; Parts of this used to live in projector.lisp
;;;;
;;;; @@@ perhaps there ought to be a project-ui mixin to the project class;
;;;; then there wouldn't need to be separate functions with separate names
;;;; for the versions of these that manipulate the user interface

(in-package :SourceServer)

;;;
;;; Global variables
;;;

(defvar *save-all-old-projector-versions* nil)
(defvar *scan-on-mount* t)
(defvar *scan-on-mount-is-incremental* nil) ; only meaningful when *scan-on-mount*
(defvar *scan-on-update* nil)

(defvar *current-project* nil)
(defvar *break-on-projector-errors* nil)
(declaim (special *projects-menu* *projector-menu* *scan-project-files-menu-item*
                  *my-projects*))

#|
;; the following parameter should be defined to the appropriate value in init.lisp
;;; e.g., a reasonable entry in your init.lisp might be:

(setf ccl::*my-projects*
  '(("Sources:Ralph:Projects:" "Newton:")
    ("Sources:Ralph:Environment:" "Environment:")))

|#

;; @@@ this is a terrible, terrible kludge
(defun my-projects ()
  (declare (special *my-projects*))
  (loop
    (when (boundp '*my-projects*)      
      (return *my-projects*))
    (cerror "Use new value for ~s."
            "The variable ~s must be initialized."
            'ccl::*my-projects*)))

(defconstant *lockfile-name* "_Release Notes")

(defvar *last-checkout-comment* "")


;;;
;;; Window utility functions
;;;

(defun map-pathname-windows (fn pathname)
  (setq pathname (full-pathname pathname))
  (map-windows #'(lambda (window &aux (window-filename (window-filename window)))
                   (and window-filename
                        (equalp (full-pathname window-filename) pathname)
                        (funcall fn window)))
               :class 'fred-window :include-invisibles t))



(defun pathname-window (path)
  (do-pathname-windows (window path) (return-from pathname-window window))
  nil)

(defun file-checkout-or-modro (path &optional
                                        (text (format nil "~/pp-path/ is not modifiable." path)))
  (if (y-or-n-dialog text :yes-text "CheckOut" :no-text "ModRO")
    (file-checkout path)
    (file-modro path)))

;;let people know when they try to modify a read-only file
(defun help-on-attempt-to-modify-readonly-buffer (window)
  (let ((path (window-filename window)))
    (if (pathname-project-p path)
      (file-checkout-or-modro path "You are attempting to modify a read-only buffer controlled by Projector. The file should be checked out or set to ModifyReadOnly mode.")
      (ed-beep))))

(defun set-path-windows-readonly-state (path state message &key revert)
  (do-pathname-windows (window path)
    (when revert
      (setf (read-only-state window) #.$ckid-readwrite)
      (window-revert window t))
    (setf (read-only-state window)
          (getf '(:read-only #.$ckid-readonly
                  :modify-read-only #.$ckid-modifyreadonly
                  :read-write #.$ckid-readwrite) state))
    (when message
      (set-mini-buffer window message))))

#| ; let fred do this for you
(defmacro with-editor-excursion (window &body body)
  (let ((scroll-pos (gensym "SCROLL-POS"))
        (selection-start (gensym "SELECTION-START"))
        (selection-end (gensym "SELECTION-END"))
        (bufSize (gensym "BUFFER-SIZE")))
    `(when ,window
       (let ((,scroll-pos (buffer-position (fred-display-start-mark ,window)))
             ,bufSize)
         (multiple-value-bind (,selection-start ,selection-end)
                              (selection-range ,window)
           ,@body
           ;; check that these values are still in range
           (setf ,bufSize (buffer-size (fred-buffer ,window)))
           (when (> ,scroll-pos ,bufSize) (setf ,scroll-pos 0))
           (when (> ,selection-end ,bufSize)
             (setf ,selection-start 0)
             (setf ,selection-end 0))
           ;; Now set the selection and the scroll.
           (set-selection-range ,window ,selection-start ,selection-end) ; this should return to current pos
           (set-mark (fred-display-start-mark ,window) ,scroll-pos))))))
|#
;;;
;;; Projects menu
;;;

;;; put check-mark next to the current project
(defun projector-menu-update ()
  (let ((current *current-project*))
    (dolist (item (menu-items *projects-menu*))
      (set-menu-item-check-mark item nil))
    (when current
      (set-menu-item-check-mark (find-menu-item *projects-menu* (project-name current)) t))))

;;; add items to the Projector menu that list each mounted project and selecting that item
;;; will set that as the current project.
(defun make-projects-menu ()
  (menu-install *projects-menu*)
  (apply #'remove-menu-items *projects-menu* (menu-items *projects-menu*))
  ;; @@@ ought to be able to use *projects*
  (dolist (project *mounted-projects*)
    (let ((name (project-name project)))
      (unless (find-menu-item *projects-menu* name)
        (add-new-item *projects-menu* name
                      (let ((project project))
                        #'(lambda ()
                            (set-current-project project))))))))

;;;
;;; Filenames in Projector menu
;;;

(defun find-or-open-pathname (path)
  (let((window (pathname-window path)))
    (if window
      (window-select window)
      (if (eq :TEXT (mac-file-type path))
        (fred path)
        (message-dialog (format nil "~/pp-path/ is not a text file" path))))))

(defclass project-file-menu-item (menu-item)
  ((pathname :accessor menu-item-pathname :initarg :pathname)
   (checkout-state :accessor menu-item-checkout-state :initarg :checkout-state)))

(defmethod menu-item-title ((item project-file-menu-item))
  (concatenate 'string
               (case (menu-item-checkout-state item)
                 (:modro "* ")
                 (:checked-out "+ ")
                 (t "  "))
               (file-namestring (menu-item-pathname item))))

(defmethod initialize-instance ((item project-file-menu-item) &rest initargs)
  (apply #'call-next-method item initargs)
  (setf (slot-value item 'ccl::title) (menu-item-title item)))

(defmethod menu-item-action ((item project-file-menu-item))
  (find-or-open-pathname (menu-item-pathname item)))

(defun find-project-file-menu-item (path)
  (dolist (item (menu-items *projector-menu*) nil)
    (when (and (typep item 'project-file-menu-item)
               (equalp (menu-item-pathname item) path))
      (return item))))

(defun insert-menu-item (menu item position)
  (let* ((all-items (menu-items menu))
         (after-items (nthcdr position all-items)))
    (apply #'remove-menu-items menu after-items)
    (apply #'add-menu-items menu item after-items)))

(defun add-filename-to-projector-menu (path checkout-state)
  (unless (find-project-file-menu-item path)
    (let ((namestring (file-namestring path)))
      (insert-menu-item
       *projector-menu*
       (make-instance 'project-file-menu-item :pathname path :checkout-state checkout-state) 
       (1+ (position-if #'(lambda (item)
                            (let ((title (menu-item-title item)))
                              (or (string-equal "-" title)
                                  (string-greaterp namestring (subseq title 2)))))
                        (menu-items *projector-menu*) :from-end t))))))

(defun maybe-add-filename-menu (file)
  (let ((project (pathname-project-p file)))
    (when project
      (let ((state (project-file-local-state project file)))
        (when (member state '(:modro :checked-out))
          (add-filename-to-projector-menu file state))))))

(defun remove-filename-from-projector-menu (path)
  (remove-menu-items *projector-menu* (find-project-file-menu-item path)))

(defun update-projector-file-state (path &optional (remote-changed t))
  (remove-filename-from-projector-menu path)
  (maybe-add-filename-menu path)
  (close-checkin-categories-dialog)
  (let ((window (find-file-info-window path)))
    (when window
      (invalidate-view window)
      (update-file-info-window window remote-changed))))

(defun remove-all-filename-menus (&optional project)
  (apply #'remove-menu-items *projector-menu*
         (remove-if-not #'(lambda (item)
                            (and (typep item 'project-file-menu-item)
                                 (or (null project)
                                     (eq (pathname-project (menu-item-pathname item)) project))))
                        (menu-items *projector-menu*))))

;;;
;;; Dialogs
;;;

;; Same as y-or-n-dialog, except only two buttons and one of them cancels.
(defun okay-or-cancel-dialog (message &key (size #@(250 200))
                                          (position (list :top (+ 2 *menubar-bottom*)))
                                          (okay-text "Okay")
                                          (cancel-text "Cancel")
                                          help-spec)
  (modal-dialog
   (make-instance 'ccl::keystroke-action-dialog
     :window-type :double-edge-box
     :view-size size
     :view-position position
     :window-show nil
     :help-spec (getf help-spec :dialog)
     :view-subviews
     `(
       ,(make-dialog-item 'static-text-dialog-item
                          #@(20 12) (subtract-points size #@(38 72))
                          message nil :help-spec (getf help-spec :dialog))
       ,(make-dialog-item 'default-button-dialog-item               
                          (make-point 20 (- (point-v size) 30))
                          #@(74 18) okay-text
                          #'(lambda (item) 
                              (declare (ignore item))
                              (return-from-modal-dialog t))
                          :help-spec (getf help-spec :okay-text))
       ,(make-dialog-item 'button-dialog-item
                          (subtract-points size #@(#.(+ 20 74) 30))
                          #@(74 18) cancel-text
                          #'(lambda (item)
                              (declare (ignore item))
                              (return-from-modal-dialog :cancel))
                          :help-spec (getf help-spec :cancel-text))))))

(defun cancel-checkout-dialog (path)
  (modal-dialog
   (make-instance 'dialog
     :window-type :double-edge-box
     :window-show nil
     :view-position '(:top 100)
     :view-size #@(428 150)
     :view-subviews
     (list (make-dialog-item 'static-text-dialog-item #@(16 7) #@(402 100)
                             (format nil "Cancel checkout for ~/pp-path/ and:~
                                          ~%¥ Keep changes and convert to ModifyReadOnly.~
                                          ~%¥ Throw away changes and retrieve latest version."
                                     path))
           (make-dialog-item 'button-dialog-item #@(16 120) #@(62 16)
                             "Cancel"
                             #'(lambda (item) (declare (ignore item))
                                (return-from-modal-dialog :cancel)))
           (make-dialog-item 'button-dialog-item #@(140 120) #@(110 16)
                             "Keep Changes" 
                             #'(lambda (item)
                                 (declare (ignore item))
                                 (return-from-modal-dialog t))
                             :default-button t)
           (make-dialog-item 'button-dialog-item #@(290 120) #@(120 16)
                             "Throw Away" 
                             #'(lambda (item) 
                                 (declare (ignore item))
                                 (return-from-modal-dialog nil)))))))

;(cancel-checkout-dialog (pathname (front-window)))
   
#|
(defun center-size-in-screen (size-point)
  (let* ((x (truncate (- *screen-width* (point-h size-point)) 2))
         (y (max 44 (- (truncate *screen-height* 3) (point-v size-point)))))
    (make-point x y)))
|#

;;; Following code opens a dialog to ask user for a comment
(defun comment-dialog (file &optional (initial-comment ""))
  (let* ((view-size #@(362 172))
         (c-dialog
          (make-instance 'color-dialog
            :window-type :double-edge-box
            :window-title "Projector-comment-dialog"
            :view-position '(:top 44)
            :view-size view-size
            :window-show nil
            :view-subviews
            (list
             (make-dialog-item 'static-text-dialog-item
                               #@(10 10) #@(300 16)
                               "What are your changes to:")
             (make-dialog-item 'static-text-dialog-item
                               #@(10 26) nil (namestring file)
                               )
             (make-dialog-item 'editable-text-dialog-item
                               #@(10 50) #@(340 80) 
                               (format nil "~a" initial-comment) ;; use a copy or you'll be sorry!
                               nil
                               :view-nick-name 'replace-text-item
                               :allow-returns t :WRAP-P T)
             (make-dialog-item 'button-dialog-item
                               #@(90 145) #@(70 20)  "OK"
                               #'(lambda (item)
                                   (let ((my-dialog (view-container item)))
                                     (return-from-modal-dialog
                                      (dialog-item-text (view-named 'replace-text-item my-dialog)))))
                               ;; should not default if <CR> doesn't actuate
                               :default-button t  
                               )
             (make-dialog-item 'button-dialog-item
                               #@(180 145) #@(70 20)  "Cancel"
                               #'(lambda (item)
                                   (declare (ignore item))
                                   (return-from-modal-dialog :cancel)))))))
    (modal-dialog c-dialog)))

(defclass secret-editable-text (editable-text-dialog-item)
  ((secret-string :accessor secret-string
                  :initform (make-array '(0) :element-type 'character :fill-pointer t :adjustable t))))

(defmethod view-key-event-handler ((item secret-editable-text) xchar)
  (call-next-method item #\¥)
  (let ((string (secret-string item)))
    (if (eq xchar #\backspace)
      (setf (fill-pointer string) (max 0 (1- (fill-pointer string))))
      (vector-push-extend xchar string))))

(defun volume-server-alist ()
  (declare (special ccl::*volume-server-alist*))
  (and (boundp 'ccl::*volume-server-alist*)
       ccl::*volume-server-alist*))

#|
;;; Useful code from Guillame CartiŽr
(defconstant $zoneNameOffset      24)
(defconstant $serverNameOffset    57)
(defconstant $volNameOffset       89)
(defconstant $userNameOffset     117)
(defconstant $userPassWordOffset 147)
(defconstant $volPassWordOffset  158)

(defun remote-mount (zone server user password volume
                          &optional (volpass ""))
  (rlet ((afp :AFPVolMountInfo
              :length 167
              :media "afpm"
              :flags 0
              :nbpInterval 5
              :nbpCount 10
              :uamType 6
              :zoneNameOffset     $zoneNameOffset
              :serverNameOffset   $serverNameOffset
              :volNameOffset      $volNameOffset
              :userNameOffset     $userNameOffset
              :userPassWordOffset $userPassWordOffset
              :volPassWordOffset  $volPassWordOffset))
    (%put-string afp zone     $zoneNameOffset)
    (%put-string afp server   $serverNameOffset)
    (%put-string afp volume   $volNameOffset)
    (%put-string afp user     $userNameOffset)
    (%put-string afp password $userPassWordOffset)
    (%put-string afp volpass  $volPassWordOffset)
    (rlet ((pb :ParamBlockRec
               :ioCompletion (%null-ptr)
               :ioBuffer afp))
      (#_PBVolumeMount pb))))
|#


(defun user-mount-volume (volume &key zone server)
  (declare (special *user-initials*))
  (let* ((guestp nil)
         (user-name (or (chooser-name)
                        (and (boundp '*user-initials*) *user-initials*)
                        ""))
         (password "")
         (password-item
          (make-dialog-item 'secret-editable-text #@(104 205) #@(65 17) ""
                            #'(lambda (item) (setq password (secret-string item)))))
         (disappearing-items
          (list           
           (make-dialog-item 'static-text-dialog-item #@(26 179) #@(71 16) "Name:")
           (make-dialog-item 'static-text-dialog-item #@(26 206) #@(73 16) "Password:" nil)
           (make-dialog-item 'editable-text-dialog-item #@(104 178) #@(263 17) user-name
                             #'(lambda (item) (setq user-name (dialog-item-text item))))
           password-item)))
    (let ((ass (assoc volume (volume-server-alist) :test #'string-equal)))
      (setq zone (or zone (cadr ass) "")
            server (or server (caddr ass) "")))
    (let ((dialog
           (make-instance 'dialog
             :window-type :double-edge-box
             :view-position '(:top 150)
             :view-size #@(377 283)
             :close-box-p nil
             :view-font '("Chicago" 12 :srcor :plain)
             :window-show nil
             :view-subviews
             (append
              (list (make-dialog-item
                     'static-text-dialog-item #@(5 5) #@(255 16) "Connect to shared disk in:")
                    (make-dialog-item 'static-text-dialog-item #@(23 33) #@(48 16) "Zone:")
                    (make-dialog-item 'editable-text-dialog-item #@(81 33) #@(186 17) zone
                                      #'(lambda (item) (setq zone (dialog-item-text item))))
                    (make-dialog-item 'static-text-dialog-item #@(23 61) #@(51 16) "Server:")
                    (make-dialog-item 'editable-text-dialog-item #@(81 61) #@(186 17) server
                                      #'(lambda (item) (setq server (dialog-item-text item))))
                    (make-dialog-item 'static-text-dialog-item #@(23 88) #@(56 16) "Volume:")
                    (make-dialog-item 'editable-text-dialog-item #@(81 87) #@(186 17) volume
                                      #'(lambda (item) (setq volume (dialog-item-text item))))
                    (make-dialog-item 'radio-button-dialog-item #@(23 125) #@(72 16) "Guest"
                                      #'(lambda (item)
                                          (setq guestp t)
                                          (apply #'remove-subviews (view-container item) disappearing-items)))
                    (make-dialog-item 'radio-button-dialog-item #@(23 143) #@(167 16) "Registered User"
                                #'(lambda (item)
                                    (setq guestp nil)
                                    (apply #'add-subviews (view-container item) disappearing-items))
                                :radio-button-pushed-p t)
                    (make-dialog-item 'button-dialog-item #@(295 253) #@(62 16) "OK"
                                      #'(lambda (item) (declare (ignore item)) (return-from-modal-dialog t)) :default-button T)
              (make-dialog-item 'button-dialog-item #@(23 253) #@(62 16) "Cancel"
                                #'(lambda (item) (declare (ignore item)) (return-from-modal-dialog :cancel))))
              disappearing-items))))
      (set-current-key-handler dialog password-item)
      (modal-dialog dialog))
    (let ((place (concatenate 'string zone (if (find #\: zone) "" ":") server (if (find #\: zone) "" ":") volume)))
      (if guestp
        (mpw-mount-volume place)
        (mpw-mount-volume place :user user-name :password password))))
  (let ((time (get-universal-time)))
    (loop
      (if (probe-file volume) (return))
      (event-dispatch t)
      (if (> 10 (- (get-universal-time) time))
        (error "Timeout mounting volume: ~s" volume)))))

;;;
;;; Project commands
;;;

(defun report-project-error (error)
  (when *break-on-projector-errors*
    (error error))
  (message-dialog (princ-to-string error) :size #@(350 250))
  (cancel))

;; The syntax of handler-bind with the syntax of handler-case.
;; Mainly so that I can have a nice syntax for handlers that
;; have access to restarts, which handler-case doesn't do.
;; @@@ ought to declare the clauses dynamic-extent, I suppose
(defmacro with-handlers (form &body clauses)
  `(handler-bind
     ,(mapcar #'(lambda (clause)
                  (destructuring-bind (case arglist &rest body) clause
                    `(,case #'(lambda ,arglist ,@body))))
              clauses)
     ,form))

;; @@@ should add a clause here to automount the project
(defmacro project-handler-case (form &body clauses)
  `(handler-bind
     ((project-error #'report-project-error))
     (with-handlers ,form ,@clauses)))

#|
(defun user-mount-project (&optional project)
  (unless (and project (project-mounted-p project))
    (if *mounted-projects*
      (if (y-or-n-dialog (format nil "Project ~a not mounted - mount it now?" project)
                         :yes-text "Mount"
                         :no-text "Cancel"
                         :cancel-text nil)
        (mount-some-projects)
        (cancel))
      (if (y-or-n-dialog "No projects are mounted - mount all now?"
                         :yes-text "All"
                         :no-text "Select"
                         :cancel-text "Cancel")
        (Mount-All-Projects)
        (mount-some-projects))))
  (if project (project-mounted-p project)))
|#


;;;
;;; Checking out and in all
;;;

(defvar *close-checkin-categories-dialog* t)

(defclass file-list-dialog-item (sequence-dialog-item)
  ())

(defmethod cell-contents-string ((item file-list-dialog-item) cell)
  (ccl::pathname-to-window-title (cell-contents item cell)))

(defclass filelist-pane (view) ())

(defmethod set-view-size ((view filelist-pane) h &optional v)
  (call-next-method view h v)
  (let* ((size (view-size view))
         (margin 2)
         (label-height 20)
         (width (- (point-h size) (* 2 margin)))
         (height (point-v size))
         (views (view-subviews view))
         (label (elt views 0))
         (list (elt views 1)))
    (set-view-size label width label-height)
    (set-view-size list width (- height (* 3 margin) label-height))))

(defclass checkin-categories-dialog (dialog)
  ()
  (:default-initargs :grow-icon-p t :view-size #@(400 270)))

(defmethod initialize-instance ((dialog checkin-categories-dialog) &rest rest
                                    &key (window-show t))
  (declare (dynamic-extent rest))
  (apply #'call-next-method dialog :window-show nil rest)
  (resize-subviews dialog)
  (when window-show
    (window-show dialog)))

(defmethod set-view-size ((dialog checkin-categories-dialog) h &optional v)
  (call-next-method dialog h v)
  (with-focused-view dialog
    (rlet ((rect :rect :topLeft 0 :bottomRight 0))
      (with-clip-rect rect
        (resize-subviews dialog)))
    (invalidate-view dialog t)))

(defun resize-subviews (dialog)
  (let* ((size (view-size dialog))
         (width (point-h size))
         (height (point-v size))
         (label-height 40)
         (button-height 20)
         (margin 4)
         (lower-pane-height (+ margin label-height margin button-height margin)))
    (labels ((empty-filelist (view)
               (zerop (length (table-sequence (elt (view-subviews view) 1))))))
      (let* ((views (remove-if-not #'(lambda (type) (eq type 'filelist-pane)) (view-subviews dialog)
                                   :key #'type-of))
             (filled (remove-if #'empty-filelist views))
             (count (length filled)))
        (map nil #'(lambda (view)
                     (when (empty-filelist view)
                       (set-view-position view -10000 0)))
             views)
        (unless (zerop count)
          (let ((subwidth (truncate (- (point-h size) (* margin (1+ count))) count))
                (left 0))
            (map nil #'(lambda (view &aux (top (point-v (view-position view))))
                         (set-view-position view (incf left margin) top)
                         (set-view-size view subwidth (- height top lower-pane-height))
                         (incf left subwidth))
                 filled)))))
    (let* ((message (view-named 'message dialog))
           (button1 (view-named 'button1 dialog))
           (button2 (view-named 'button2 dialog))
           (button-width 60)
           (button-top (- height margin button-height)))
      (set-view-position message 2 (- height lower-pane-height (- margin)))
      (set-view-size message (- width margin margin) label-height)
      (set-view-position button1 0 button-top)
      (set-view-position button2 0 button-top)
      (set-view-size button1 button-width button-height)
      (set-view-size button2 button-width button-height)))
  (set-button-positions dialog))

(defun set-button-positions (dialog)
  (let* ((button1 (view-named 'button1 dialog))
         (button2 (view-named 'button2 dialog))
         (button-top (point-v (view-position button1)))
         (button-width (point-h (view-size button1)))
         (view-width (point-h (view-size dialog))))
    (cond ((string= (dialog-item-text button1) "")
           (set-view-position button1 -1000 button-top)
           (set-view-position button2 -1000 0))
          ((string= (dialog-item-text button2) "")
           (set-view-position button1 (truncate (- view-width button-width) 2) button-top)
           (set-view-position button2 -1000 0))
          (t
           (let ((tween (truncate (- view-width button-width button-width) 3)))
             (set-view-position button1 tween button-top)
             (set-view-position button2 (+ tween button-width tween) button-top))))))

(defun remove-buttons (dialog)
  (let ((button1 (view-named 'button1 dialog))
        (button2 (view-named 'button2 dialog)))
    (set-view-position button1 -1000 0)
    (set-view-position button2 -1000 0)))

#|
(defun add-pathname-to-category (pathname category)
  (let ((dialog (front-window :class 'checkin-categories-dialog)))
    (when dialog
      (let* ((item (view-named category dialog))
             (sequence (table-sequence item)))
        (unless (member pathname sequence :test #'equal)
          (remove-buttons dialog)
          (set-table-sequence item (append sequence (list pathname)))
          (unless sequence
            (set-view-size dialog (view-size dialog))))))))

(defun remove-pathname-from-category (pathname category)
  (let ((dialog (front-window :class 'checkin-categories-dialog)))
    (when dialog
      (let* ((item (view-named category dialog))
             (sequence (table-sequence item)))
        (when (member pathname sequence :test #'equal)
          (remove-buttons dialog)
          (let ((sequence (remove pathname sequence)))
            (set-table-sequence item sequence)
            (unless sequence
              (set-view-size dialog (view-size dialog)))))))))
|#

(defun make-filelist-pane (label help-spec name contents)
  (make-instance 'filelist-pane
    :view-size #@(100 250)
    :view-position #@(2 2)
    :view-nick-name name
    :help-spec help-spec
    :view-subviews
    `(
      ,(make-dialog-item 'static-text-dialog-item
                         #@(2 2) #@(100 20) label nil
                         :help-spec help-spec)
      ,(make-instance 'sequence-dialog-item
         :view-size #@(96 200)
         :view-position #@(2 25)
         :view-font '("Geneva" 9)
         :help-spec help-spec
         :selection-type :disjoint
         :table-hscrollp nil
         :table-sequence contents
         :table-print-function #'(lambda (path stream)
                                   (princ (ccl::pathname-to-window-title path) stream))
         :dialog-item-action
         #'(lambda (item)
             (when (double-click-p)
               (dolist (cell (selected-cells item))
                 (let* ((path (cell-contents item cell))
                        (window (pathname-window path)))
                   (if window
                     (window-select window)
                     (fred path))))))))))

(defun close-checkin-categories-dialog ()
  (when *close-checkin-categories-dialog*
    (map-windows #'window-close :class 'checkin-categories-dialog)))

(defun make-checkin-categories-dialog (modified checkers mergers wedgers changed-wedgers)
  (make-instance 'checkin-categories-dialog
    :window-title "Changed File Categories"
    :view-position `(:top ,(+ 20 *menubar-bottom*))
    :view-subviews
    `(
      ,(make-filelist-pane "Out:" "Files that are checked out."
                           'checked-out modified)
      ,(make-filelist-pane "Modified:" "Files that are ModifyReadOnly and can be checked in without further ado."
                           'modify-read-only checkers)
      ,(make-filelist-pane "Merge:" "Files that are ModifyReadOnly and can be checked in after merging."
                           'merge mergers)
      ,(make-filelist-pane "Wedged:" "Files that are ModifyReadOnly and cannot be checked in because they have been checked out by someone else and they have the same versions as local files."
                           'wedge wedgers)
      ,(make-filelist-pane "Merge/Wedged:" "Files that are ModifyReadOnly and cannot be checked in because they have been checked out by someone else and they have the different versions from local files."
                           'changed-wedged changed-wedgers)
      ,(make-dialog-item 'static-text-dialog-item
                         #@(0 0) #@(10 10) "" nil
                         :view-nick-name 'message
                         :view-font '("Geneva" 9 :bold))
      ,(make-dialog-item 'button-dialog-item
                         #@(0 0) #@(10 10) "Okay" nil
                         :view-nick-name 'button1)
      ,(make-dialog-item 'button-dialog-item
                         #@(0 0) #@(10 10) "" nil
                         :view-nick-name 'button2)
      )))

(defun set-checkin-categories-action (dialog message-text &optional
                                                  (button1-text "") button1-action
                                                  (button2-text "") button2-action)
  (let ((message (view-named 'message dialog))
        (button1 (view-named 'button1 dialog))
        (button2 (view-named 'button2 dialog)))
    (set-dialog-item-text message message-text)
    (set-dialog-item-text button1 button1-text)
    (set-dialog-item-text button2 button2-text)
    (flet ((close-action (action)
             (when action
               #'(lambda (item)
                   (declare (ignore item))
                   (dialog-item-disable button1)
                   (dialog-item-disable button1)
                   (funcall (enqueued-action (funcall action)))))))
      (set-dialog-item-action-function button1 (close-action button1-action))
      (set-dialog-item-action-function button2 (close-action button2-action)))
    (dialog-item-enable button1)
    (dialog-item-enable button2))
  (set-button-positions dialog))

(defmacro if-checkin-category-query (dialog message-text &rest clauses)
  `(set-checkin-categories-action
    ,dialog
    ,message-text
    ,@(mapcan #'(lambda (clause)
                  (destructuring-bind (button-text &rest body) clause
                    `(,button-text #'(lambda () ,@body))))
              clauses)))

(defun print-a-and-b (a aname b bname &key (count t) first-word)
  (with-output-to-string (stream)
    (labels ((show (list name)
               (when list
                 (when count
                   (let* ((length (length list))
                          (text (with-output-to-string (stream)
                                  (format stream (if (<= length 10) "~R " "~D ") length))))
                     (format stream (if first-word "~@(~A~)" "~A") text)
                     (setq first-word nil)))
                 (princ name stream))))
      (show a aname)
      (and a b (princ " and " stream))
      (show b bname)
      (format stream " file~[~;~:;s~]"
              (length (or b a))))))

#|
(print-a-and-b '() "A" '(1) "B" :first-word t)
(print-a-and-b '() "A" '(1 2) "B")
(format nil "~:[~;~~(~]" t)

(multiple-value-setq (*mod* *check* *merge* *wedge* *change-wedge*)
  (categorize-project-modified-files *projects*))


(setq dialog (make-checkin-categories-dialog *mod* *check* *merge* *wedge* *change-wedge*))

(setq dialog (front-window :class 'checkin-categories-dialog))

(set-checkin-categories-action
 dialog
 (format nil "~:[~;Wedged~]~:[~; and ~]~:[~;Changed~] files cannot be uploaded, because they are checked out ~
              to other users.  Proceed with uploading the files that can be uploaded?"
         t (and t nil) nil)
 "Okay" #'(lambda () (print "Okay")))

(set-checkin-categories-action
 dialog
 (format nil "~:[~;Merge~]~:[~; and ~]~:[~;Changed~] files need merging. ~
              Move them to a new merge directory and check out the latest versions, ~
              or upload the Modified and Ready files?"
         t (and t nil) nil)
 "Merge" nil "Checkin" nil)

(make-checkin-categories-dialog '("Modified") '("Checkers") '("mergers")
                                '("wedgers") '("changed-wedgers"))
|#

;; @@@ fix these to use the high-level interface

;; @@@ race condition with the project.  Ought to find out everything that would
;; be checked out, deal with it, and then check out exactly those versions of those
;; files.  Better yet, check out everything with a certain stamp, unless told otherwise.
(defun check-out-projects (&rest projects)
  (when *save-all-old-projector-versions*
    (report-progress "Saving unmodified files with newer versions...~%")
    (move-files-to-merge-directory (mapcan #'project-newer-files projects) " Save"))
  (multiple-value-bind (modified checkers mergers wedgers changed-wedgers)
                       (categorize-project-modified-files projects)
    (declare (ignore modified checkers wedgers))
    (close-checkin-categories-dialog)
    (flet ((get-newer-files ()
             (map-windows #'window-close :class 'projector-file-info-window)
             (dolist (project projects)
               (report-progress "Checking out newer files in project: ~A..." project)
               (project-getnewer project :verbose t)
               (when *scan-on-update*
                 (scan-project project)))))
      (if (or mergers changed-wedgers)
        (let ((dialog (make-checkin-categories-dialog nil nil mergers nil changed-wedgers)))
          (if-checkin-category-query
           dialog
           (format nil "~A ~[~;has a newer version~:;have newer versions~] in the project database. ~
                        Move ~:*~[~;it~:;them~] to a new merge directory, ~
                        or continue with the update but don't overwrite local modified files?"
                   (print-a-and-b mergers "Merge" changed-wedgers "Merge/Wedged" :first-word t)
                   (+ (length mergers) (length changed-wedgers)))
           ("Move"
            (let ((move-files (append mergers changed-wedgers)))
              (dolist (file move-files)
                (map-pathname-windows #'window-close file))
              (move-files-to-merge-directory move-files)
              (window-close dialog))
            (get-newer-files))
           ("Continue"
            (window-close dialog)
            (get-newer-files))))
        (get-newer-files)))))

(defun check-in-projects (&rest projects)
  (multiple-value-bind (modified checkers mergers wedgers changed-wedgers)
                       (categorize-project-modified-files projects)
    (close-checkin-categories-dialog)
    (let ((dialog (when (or wedgers changed-wedgers mergers changed-wedgers)
                    (make-checkin-categories-dialog modified checkers mergers wedgers changed-wedgers))))
      (labels
        ((look-for-wedgers ()
           (if (or wedgers changed-wedgers)
             (if-checkin-category-query
              dialog
              (format nil "~A cannot be uploaded, because ~[~;it is~:;they are~] checked out ~
                           to ~:*~[~;another user~:;other users~].  Proceed with uploading the ~A?"
                      (print-a-and-b wedgers "Wedged" changed-wedgers "Merge/Wedged" :first-word t)
                      (+ (length wedgers) (length changed-wedgers))
                      (print-a-and-b modified "Out" checkers "Modified" :count nil))
              ("Proceed"
               (look-for-mergers)))
             (look-for-mergers)))
         (look-for-mergers ()
           (if (or mergers changed-wedgers)
             (if-checkin-category-query
              dialog
              (format nil "~A need~[~;s~:;~] merging. ~
                           Move ~:*~[~;it~:;them~] to a new merge directory and check out the latest version~:*~[~;~:;s~], ~
                           or upload the ~A?"
                      (print-a-and-b mergers "Merge" changed-wedgers "Merge/Wedged" :first-word t)
                      (+ (length mergers) (length changed-wedgers))
                      (print-a-and-b modified "Out" checkers "Modified" :count nil))
              ("Merge"
               (let ((merge-files (append mergers changed-wedgers)))
                 (dolist (file merge-files)
                   (map-pathname-windows #'window-close file))
                 (let ((merge-dirs (move-files-to-merge-directory merge-files)))
                   ;;check out mergers for modification
                   (dolist (path mergers)
                     (report-progress "Checking out ~/pp-path/" path)
                     (let ((*close-checkin-categories-dialog* nil))
                       (file-checkout path)))
                   ;;check out changed-wedgers read-only
                   (dolist (path changed-wedgers)
                     (report-progress "Checking out ~/pp-path/" path)
                     ;; @@@ change this to use the higher level interface, as soon as we
                     ;; @@@ add one to get the latest version of something that isn't
                     ;; @@@ already there
                     (mpw-checkout path :project (projector-name (pathname-project path)))
                     (update-projector-file-state path))
                   (okay-or-cancel-dialog "Merge directories?")
                   (dolist (pair merge-dirs)
                     (destructuring-bind (local-dir . merge-dir) pair
                       (merge-directories merge-dir local-dir)))
                   (checkin-files))))
              ("Checkin"
               (checkin-files)))
             (checkin-files)))
         (checkin-files ()
           (let* ((checkin (append modified checkers))
                  (lock-file (find *lockfile-name* checkin :key #'file-namestring :test #'string-equal)))
             (when lock-file
               (setq checkin (nconc checkin (list lock-file))))
             (dolist (file (delete lock-file checkin))
               (report-progress "Checking in ~/pp-path/" file)
               (let ((*close-checkin-categories-dialog* nil))
                 (file-checkin file :no-comment (memq file modified)))))))
        (look-for-wedgers)))))

;;;
;;; File info
;;;

(defclass projector-file-info-window (dialog)
  ((pathname :accessor gf-pathname :initarg :pathname))
  (:default-initargs :color-p t))

(defmethod initialize-instance ((dialog projector-file-info-window) &key &allow-other-keys)
  (call-next-method)
  (update-file-info-window dialog))

(defmethod view-draw-contents ((dialog projector-file-info-window))
  (rlet ((rect :rect :topLeft #@(0 0) :bottomRight #@(32 32)))
    (#_OffsetRect rect 18 4)
    (#_PlotIconID rect 0 0 129))
  (labels ((frame-view (name &aux (view (view-named name dialog)))
             (multiple-value-bind (tl br) (view-corners view)
               (rlet ((rect :rect :topLeft tl :bottomRight (subtract-points br #@(1 1))))
                 (#_FrameRect rect)
                 #+never
                 (rlet ((rgb :rgbColor :red 32767 :green 32767 :blue 32767))
                   (#_RGBForeColor rgb)
                   (#_MoveTo (1- (point-h br)) (1+ (point-v tl)))
                   (#_LineTo (1- (point-h br)) (1- (point-v br)))
                   (#_LineTo (1+ (point-h tl)) (1- (point-v br))))
                 #+never
                 (rlet ((rgb :rgbColor :red 0 :green 0 :blue 0))
                   (#_RGBForeColor rgb))))))
    (call-next-method)
    (frame-view 'local)
    (frame-view 'remote)))

(defmethod view-click-event-handler ((dialog projector-file-info-window) where)
  (labels ((point-in-icon (&optional (where (view-mouse-position dialog)))
             (and (< 0 (- (point-h where) 18) 32)
                  (< 0 (- (point-v where) 4) 32)))
           (plot (state)
             (rlet ((rect :rect :topLeft #@(0 0) :bottomRight #@(32 32)))
               (#_OffsetRect rect 18 4)
               (#_PlotIconID rect 0 state 129)))
           (off ()
             (cond ((not (mouse-down-p)))
                   ((point-in-icon) (plot #$ttSelected) (on))
                   (t (off))))
           (on ()
             (cond ((not (mouse-down-p))
                    (plot #$ttNone)
                    (find-or-open-pathname (gf-pathname dialog)))
                   ((point-in-icon) (on))
                   (t (plot #$ttNone) (off)))))
    (when (point-in-icon where)
      (plot #$ttSelected)
      (on))))

(defun update-file-info-window (dialog &optional (remote-changed t)
                                           &aux (path (gf-pathname dialog))
                                           (project (pathname-project path)))
  (labels ((set-item (view name contents)
             (set-dialog-item-text (view-named name view) contents))
           (state-string (state)
             (getf '(:modro "ModifyReadOnly"
                     :checked-in "Checked In"
                     :checked-out "Checked Out")
                   state))
           (update-pane (name info-fn &aux (view (view-named name dialog)))
             (destructuring-bind (&key state version comment user)
                                 (funcall info-fn project path)
               (set-item view 'state (state-string state))
               (set-item view 'version version)
               (set-item view 'comment (if (string= comment "") "Ñ" comment))
               (set-item view 'user user)))
           (trim-to-pane (name)
             (set-view-size dialog (add-points (nth-value 1
                                                          (view-corners (view-named name dialog)))
                                               #@(1 1)))))
    (set-item dialog 'project-name (project-name project))
    (update-pane 'local #'project-file-local-info)
    (when remote-changed
      (if (project-mounted-p project)
        (with-cursor *watch-cursor*
          (window-update-event-handler dialog)
          (update-pane 'remote #'project-file-remote-info)
          (trim-to-pane 'remote))
        (trim-to-pane 'local)))))

(defun find-file-info-window (path)
  (map-windows #'(lambda (window)
                   (when (equalp (gf-pathname window) path)
                     (return-from find-file-info-window window)))
               :class 'projector-file-info-window))

(defclass static-justified-text-dialog-item (static-text-dialog-item)
  ((ccl::text-justification :allocation :instance)))

(defun show-file-info (path)
  (window-select
   (or (find-file-info-window path)
       (let ((current-y (+ 32 6))
             (plain-font '("Geneva" 9))
             (label-font '("Geneva" 9 :bold)))
         (make-instance 'projector-file-info-window
           :window-title (format nil "~A Info" (file-namestring path))
           :pathname path
           :view-size #@(200 282)
           :view-subviews
           (labels ((make-item (item-data)
                      (destructuring-bind (label name &optional (dy 12)) item-data
                        (prog1
                          (list
                           (make-dialog-item 'static-justified-text-dialog-item (make-point 2 current-y) #@(55 12)
                                             label nil
                                             :text-justification :right
                                             :view-font label-font)
                           (make-dialog-item 'static-text-dialog-item (make-point 62 current-y)
                                             (make-point 120 dy) "É" nil
                                             :text-justification :left
                                             :view-nick-name name
                                             :view-font plain-font))
                          (incf current-y dy))))
                    (make-items (list)
                      (mapcan #'make-item list))
                    (make-pane (label name top)
                      (setq current-y 14)
                      (make-instance 'view
                        :view-position (make-point 2 (+ top 2))
                        :view-size #@(197 101)
                        :view-nick-name name
                        :view-subviews
                        `(,(make-dialog-item 'static-text-dialog-item #@(2 2) #@(192 96)
                                             label nil
                                             :view-font '("Geneva" 9 :italic))
                          ,@(make-items '(("User:" user)
                                          ("State:" state)
                                          ("Version:" version)
                                          ("Comment:" comment 48)))))))
             (append
              (make-item '("Project:" project-name 36))
              (list
               (make-dialog-item 'static-text-dialog-item #@(60 12) #@(156 12)
                                 (file-namestring path) nil
                                 :view-font plain-font)
               (make-pane "Local" 'local 76)
               (make-pane "Remote" 'remote 178)))))))))


;;;
;;; File commands
;;;

(defun file-path-and-project (file &aux (path (pathname file)))
  ;; @@@ maybe ought to mount the project too
  (values path (pathname-project path)))

(defmacro with-file-path-and-project ((path project) file &body body)
  `(project-handler-case
     (multiple-value-bind (,path ,project)
                          (file-path-and-project ,file)
       ,@body)))


(defmethod suppress-sourceserver-comments-p (w)
  (declare (ignore w))
  nil)


(defun file-checkout (file &key no-comment)
  (with-file-path-and-project (path project) file
    (map-pathname-windows #'(lambda (w)
                              (window-save w)
                              (unless no-comment
                                (setq no-comment (suppress-sourceserver-comments-p w))))
                          path)
    (let ((comment (unless no-comment
                     (setq *last-checkout-comment*
                           (comment-dialog path *last-checkout-comment*)))))
      (with-handlers (project-file-checkout project path :comment comment)
        (project-remote-is-newer (error)
                                 (okay-or-cancel-dialog (format nil "~A  Move it to merge-directory and check out newer version?"
                                                                error))
                                 (move-files-to-merge-directory (list path))
                                 (report-progress "Getting newer version of ~s.~%" path)
                                 (invoke-restart 'checkout-newer-file)))
      (update-projector-file-state path)
      ;(remove-pathname-from-category path 'modify-read-only)
      ;;do window hacking
      (multiple-value-bind (window existing-window)
                           (open-pathname-comment-log-window-p path no-comment)
        (when window
          (let ((new-version (inc-projector-version (project-file-local-version project path))))
            (progn ;with-editor-excursion window
              (setf (read-only-state window) $ckid-checkedout)    ; set window to be writeable
              ;; always revert, because project-file-checkout only warns if the file
              ;; was newer on the server if it was also modreadonly locally
              (window-revert window t)               
              (unless no-comment
                (add-change window new-version comment nil)))  ; insert the comment line
            (window-save window)                 ; save the contents of the window
            ;; the following expression was (fred-update window)
                  ;; then it was
            ;;(with-focused-view window
            ;;  (ccl::frec-draw-contents (frec window) t))
            ;; if the following flickers too much remove the fred-update from it and see if file-modro still displays ok
            (update-fred-window-from-changed-file window)
            (set-mini-buffer window "checked out")
            (unless existing-window (window-close window))))))))

;;; This function makes sure the state of the comment in the file is ok. If the file is
;;; to be checked in, makes sure the double squiggle is not there anymore. 
;;; Returns the comment for the file. If interactive is t, the comment dialog comes up. 
;;; If interactive is nil, the user should not be bothered at all. 

(defun fix-comment-in-file (project path add &optional (interactive? t))
  (let ((comment "")
        (new-version (inc-projector-version 
                      (if add
                        "0"
                        (project-file-local-version project path)))))
    (multiple-value-bind (window existing-window)
                         (open-pathname-comment-log-window-p path (not interactive?))
      (if window
        (progn
          (progn ;with-editor-excursion window
            (setf comment (or (find-comment-from-window window)
                              (unless add
                                (project-file-local-comment project path))
                              ""))
            (when interactive?
              (setq comment (comment-dialog path comment)))
            (add-change window new-version comment t))
          (window-save window)                 ; save the contents of the window first
          ;; the following expression was (fred-update window)
          ;; then it was
          ;;(with-focused-view window
          ;;  (ccl::frec-draw-contents (frec window) t))
          ;; if the following flickers too much remove the fred-update from it and see if file-modro still displays ok
          (update-fred-window-from-changed-file window)
          (unless existing-window (window-close window))
          comment)
        (progn (setq comment (or (unless add
                                   (project-file-local-comment project path))
                                 ""))
               (when interactive? 
                 (comment-dialog path comment)))))))

;;; Decomposition... what a thing!

(defun file-checkin (file &key no-comment &aux add comment really-in-project)
  (project-handler-case
    (with-handlers
      (multiple-value-bind (path project)
                           (file-path-and-project file)
        (when (and (not add) (probe-file path) really-in-project)
          (transfer-ckid-from-project-file (projector-name project) path :modifiable t))
        (map-pathname-windows #'(lambda (w)
                                  (window-save w)
                                  (unless no-comment
                                    (setq no-comment (suppress-sourceserver-comments-p w))))
                              path)
        (if add
          (project-file-add project path :comment comment)
          ;; Check it in. If there is already a comment in the file (with the double squiggles)
          ;; remove it!
          (progn
            (setf comment (fix-comment-in-file project path add (not no-comment)))
            (project-file-checkin project path :comment comment)))
        (set-path-windows-readonly-state path :read-only "checked in")
        (update-projector-file-state path))
      ;; this is the with-handlers clause
      (file-not-in-a-project (error)
                             (let* ((path (project-error-filename error))
                                    (pname (file-namestring path))
                                    (project (guess-pathname-project path)))
                               (cond
                                ((search (format nil "    ~a~%" pname)
                                         (project-info-text project :file-info t :short t)
                                         :test #'char-equal)
                                 (okay-or-cancel-dialog (format nil "Local copy of ~A is not marked as checked out of project ~A~%~
                                                                     Mark it and use it anyway?~%" pname project)
                                                        :size #@(350 150))
                                 (setq really-in-project t))
                                (t
                                 (okay-or-cancel-dialog (format nil "~A  Add it to the project ~A?"
                                                                error project)
                                                        :size #@(350 150))
                                 (setq add t)))
                               (invoke-restart 'add-file-to-project project))))))

#|

(defun file-checkin (file &key no-comment &aux add comment really-in-project)
  (project-handler-case
    (with-handlers
      (multiple-value-bind (path project)
                           (file-path-and-project file)
        (when (and (not add) (probe-file path) really-in-project)
          (transfer-ckid-from-project-file (projector-name project) path :modifiable t))
        (map-pathname-windows #'(lambda (w)
                                  (window-save w)
                                  (unless no-comment
                                    (setq no-comment (suppress-sourceserver-comments-p w))))
                              path)
        (unless no-comment
          (let ((new-version (inc-projector-version (if add
                                                      "0"
                                                      (project-file-local-version project path)))))
            (multiple-value-bind (window existing-window)
                                 (open-pathname-comment-log-window-p path)
              (if window
                (progn
                  (progn ;with-editor-excursion window
                    (setq comment (comment-dialog path (or (find-comment-from-window window)
                                                           (unless add
                                                             (project-file-local-comment project path))
                                                           "")))
                    (add-change window new-version comment t))
                  (window-save window)                 ; save the contents of the window first
                  ;; the following expression was (fred-update window)
                  ;; then it was
                  ;;(with-focused-view window
                  ;;  (ccl::frec-draw-contents (frec window) t))
                  ;; if the following flickers too much remove the fred-update from it and see if file-modro still displays ok
                  (update-fred-window-from-changed-file window)
                  (unless existing-window (window-close window)))
                (setq comment (comment-dialog path (or (unless add
                                                         (project-file-local-comment project path))
                                                       "")))))))
        (if add
          (project-file-add project path :comment comment)
          (project-file-checkin project path :comment comment))
        (set-path-windows-readonly-state path :read-only "checked in")
        (update-projector-file-state path))
      ;; this is the with-handlers clause
      (file-not-in-a-project (error)
                             (let* ((path (project-error-filename error))
                                    (pname (file-namestring path))
                                    (project (guess-pathname-project path)))
                               (cond
                                ((search (format nil "    ~a~%" pname)
                                         (project-info-text project :file-info t :short t)
                                         :test #'char-equal)
                                 (okay-or-cancel-dialog (format nil "Local copy of ~A is not marked as checked out of project ~A~%~
                                                                     Mark it and use it anyway?~%" pname project)
                                                        :size #@(350 150))
                                 (setq really-in-project t))
                                (t
                                 (okay-or-cancel-dialog (format nil "~A  Add it to the project ~A?"
                                                                error project)
                                                        :size #@(350 150))
                                 (setq add t)))
                               (invoke-restart 'add-file-to-project project))))))

|#

(defun file-modro (file)
  (project-file-modro (pathname-project-p file) file)
  (set-path-windows-readonly-state file :modify-read-only "buffer is ModifyReadOnly")
  (update-projector-file-state file nil))

(defun file-orphan (file)
  (with-file-path-and-project (path project) file 
    (when (eq (project-file-local-state project path) :checked-out)
      (okay-or-cancel-dialog (format nil "~/pp-path/ is checked out to you.  Are you sure you want to orphan it?" path)))
    (project-file-orphan project path)
    (set-path-windows-readonly-state path :read-write "file is orphaned")
    (update-projector-file-state path nil)))

(defun file-obsolete (file)
  (with-file-path-and-project (path project) file 
    (okay-or-cancel-dialog
     (format nil "~/pp-path/~% will be removed from subsequent versions of the project.~%~%~
                  Are you sure you want to remove it?" path))
    (project-file-obsolete project path)
    (set-path-windows-readonly-state path :read-write "file is orphaned")
    (update-projector-file-state path nil)))

(defun file-unobsolete (file)
  (let* ((path (namestring file))
         (project (guess-pathname-project path)))
    (project-file-unobsolete project path)
    (message-dialog
     (format nil "The file has not been synched with the version in the project.~%~
                  Be sure to update appropriately."))
    (update-projector-file-state path)))

(defun file-remove (file)
  (let* ((path (pathname file))
         (project (guess-pathname-project path)))
    (okay-or-cancel-dialog
     (format nil "~/pp-path/~% will be permanently removed from subsequent versions of the project.~%~%~
                  Are you sure you want to remove it?" path))
    (project-file-remove project path)
    (set-path-windows-readonly-state path :read-write "file is orphaned")
    (update-projector-file-state path nil)))

(defun file-cancel-checkout (file)
  (with-file-path-and-project (path project) file
    (let ((convert-to-modro (cancel-checkout-dialog path)))
      (with-handlers (project-file-cancel-checkout project path
                                                   :convert-to-modro convert-to-modro)
        (project-file-checked-out (error)
                                  (unless (string-equal *user-initials* (project-error-remote-user error))
                                    (signal error))
                                  (invoke-restart 'cancel-remote-checkout)))
      (if convert-to-modro
        (set-path-windows-readonly-state path :modify-read-only "buffer is now ModifyReadOnly")
        (set-path-windows-readonly-state path :read-only "checkout cancelled" :revert t))
      (update-projector-file-state path))))

(defun file-cancel-modro (file)
  (with-file-path-and-project (path project) file
    (let* (#+never (local-version (project-file-local-version project path))
           #+never (remote-version (second (memq :version (project-file-remote-info project path))))
           #+never (version
                    (if (string= local-version remote-version)
                      (progn (okay-or-cancel-dialog (format nil "Cancel ModifyReadOnly to ~/pp-path/ by forgetting any modifications and retrieving the current version?"
                                                            path)
                                                    :size #@(318 165))
                             local-version)
                      (if (y-or-n-dialog (format nil "Cancel ModifyReadOnly to ~/pp-path/ by forgetting any modifications and:~%~%~
                                                      ¥ retrieving the latest version (~A)?~%~
                                                      ¥ retrieving the local version (~A)?"
                                                 path remote-version local-version)
                                         :yes-text "Local" :no-text "Latest"
                                         :size #@(350 200))
                        local-version remote-version))))
      (okay-or-cancel-dialog (format nil "Cancel ModifyReadOnly to ~/pp-path/ by forgetting any modifications and retrieving the latest version?"
                                     path)
                             :size #@(318 165))
      (project-file-cancel-modro project path)
      #+never (unless (string= version local-version)
                (project-file-get project path :version version))
      (project-file-get project path)
      (set-path-windows-readonly-state path :read-only "ModifyReadOnly cancelled" :revert t)
      (update-projector-file-state path nil))))

(defun file-get-latest (file)
  (with-file-path-and-project (path project) file
    (unless (eq (project-file-local-state project path) :checked-in)
      (signal-project-error 'simple-project-error project
                            "Can't get a newer version of ~/pp-path/, because it is locally modifiable."
                            path))
    (project-file-get project path)
    (set-path-windows-readonly-state path :read-only "Latest version" :revert t)
    (update-projector-file-state path nil)))

(defun file-get-version (file)
  (with-file-path-and-project (path project) file
    (unless (eq (project-file-local-state project path) :checked-in)
      (signal-project-error 'simple-project-error project
                            "Can't get a different version of ~/pp-path/, because it is locally modifiable."
                            path))
    (let* ((local-version (project-file-local-version project path))
           (versions (remove local-version (project-file-versions project path)
                             :test #'string= :key #'(lambda (info) (getf info :version)))))
      (if versions
        (let ((version (first (select-item-from-list
                               versions
                               :window-title
                               (format nil "You have version ~S of ~S.  RetrieveÉ"
                                       local-version (file-namestring path))
                               :table-print-function
                               #'(lambda (info stream)
                                   (format stream "~3@A ~8A ~A"
                                           (getf info :version)
                                           (getf info :user)
                                           (getf info :comment)))))))
          (when version
            (let ((version-name (getf version :version)))
              (project-file-get project path :version version-name)
              (set-path-windows-readonly-state path :read-only (format nil "Version ~A" version-name) :revert t)
              (update-projector-file-state path nil))))
        (message-dialog (format nil "~S is the only version of ~/pp-path/" local-version path))))))

#|
(defun file-compare (file)
  (with-file-path-and-project (path project) file
    (let* ((local-version (project-file-local-version project path))
           (versions (remove (not local-version) (project-file-versions project path)
                             :test #'string= :key #'(lambda (info) (getf info :version)))))
      (when versions
        (let ((version (first (select-item-from-list
                               versions
                               :window-title
                               (format nil "Compare to versionÉ"
                                       local-version (file-namestring path))
                               :table-print-function
                               #'(lambda (info stream)
                                   (format stream "~3@A ~8A ~A"
                                           (getf info :version)
                                           (getf info :user)
                                           (getf info :comment)))))))
          (when version
            (let* ((version-name (getf version :version))
                   (directory (or (find-folder "temp") "ccl:"))
                   (other-file (merge-pathnames directory (file-namestring path))))
              (project-file-get project path :version version-name :directory directory)
              (compare-files-to-buffer other-file path))))))))

(defun compare-target ()
  (file-compare (window-filename (target))))

(file-compare #P"Tradecraft:Leibniz:Leibniz:Projector:mpw-command.lisp")
|#

;;;
;;; File information
;;;

(defun show-local-file-info (file)
  (with-file-path-and-project (path project) file
    (destructuring-bind (&key projector-name state version comment user)
                        (project-file-local-info project path)
      (format t "File: ~a,~a" (file-namestring path) version)
      (ecase state
        (:checked-out (format t "+~%    Checked out to: ~s~%" user))
        (:modro (format t "*~%    ModifyReadOnly~%"))
        (:checked-in (format t "~%    Read only~%")))
      (format t "    Project: ~a~%" projector-name)
      (unless (zerop (length comment))
        (format t "    Comment: ~s~%" comment)))))

(defun show-remote-file-info (file)
  (with-file-path-and-project (path project) file
    (print (project-info-text project :file path :comments t :latest t))))

;;this just outputs to the listener
(defun describe-modifiable-files ()
  (dolist (project *projects*)
    (unless *report-progress-to-listener* (report-progress "~A" project))
    (do-project-files 
     project
     #'(lambda (path)
         (destructuring-bind (&key state comment version)
                             (project-file-local-info project path)
           (unless (eq state :checked-in)
             (format T "~%FILE: ~a version: ~a ~a~%" 
                     path version (if (eq state :checked-out) "Checked Out" "ModifyReadOnly"))
             (unless (zerop (length comment))
               (format t "   Comments: ~a" comment))))))))

;;;
;;; Projector menu items
;;;

(defun mount-all-projects ()
  (unless (boundp '*my-projects*)
    (my-projects))
  (reset-projects)
  (setup-initial-projects)
  (unless *projects*
    (message-dialog "There are no projects defined in ccl::*my-projects* to mount.")
    (cancel))
  (menu-projects-mount *projects*)
  (set-current-project))

(defun new-project ()
  (catch-cancel
    (let ((pdir (choose-new-file-dialog :prompt "Project directory"
                                        :button-string "Create"))
          fdir)
      (setq pdir (make-pathname :directory (namestring pdir) :defaults nil))
      (mpw-command "NewProject" (directory-namestring pdir)
                             "-u" (or *user* "Unknown"))
      (setq fdir (choose-new-file-dialog :prompt "Local files directory"
                                        :button-string "Create"))

      (setq fdir (make-pathname :directory (namestring fdir) :defaults nil))
      (when (not (probe-file fdir))
        ; perhaps we should complain if already exists, but perhaps not.
        ; ie we are about to turn a directory of files into a project
        (create-directory fdir))
      (when (y-or-n-dialog "Add to ccl::*my-projects*?" :cancel-text nil)
        (push (list pdir fdir) *my-projects*)))))
    
(defun scan-project (project)
  (define-logical-host-for-project project)
  (do-project-files project
                    #'(lambda (file)
                        (update-projector-file-state file))))

(defun incremental-scan-project (project)
  (when (and *scan-on-mount*
             *scan-on-mount-is-incremental*)
    (scan-project project)))

(defun scan-project-files (&aux (item *scan-project-files-menu-item*)
                                   (saved-text (menu-item-title item)))
  (remove-all-filename-menus)
  (menu-item-disable item)
  (unwind-protect
    (dolist (project *mounted-projects*)
      (set-menu-item-title item (format nil "Scanning ~AÉ" project))
      (scan-project project))
    (set-menu-item-title item saved-text)
    (menu-item-enable item)))

#|
(defun mount-some-projects ()
  (menu-projects-mount
   (select-item-from-list *projects*
                          :window-title "Project(s) to Mount"
                          :table-print-function
                          #'princ
                          :selection-type :disjoint)))
|#

(defun print-project-info (&rest options &key recursive log &allow-other-keys)
  (flet ((print-it (project)
           (let ((result (apply #'project-info-text project :recursive nil options)))
             (when result
               (if (< (position #\Return result) (1- (length result)))
                 (format t "~%~A" result)
                 (report-progress "~A" (project-name project)))))))
    (if (and recursive (not log))
        (mapc #'print-it *mounted-projects*)
        (when (set-current-project)
          (print-it *current-project*)))))

(defun unmount-current-project ()
  (when *current-project*
    (menu-project-unmount *current-project*)
    (set-current-project)))

(defun unmount-all-projects ()
  (mapc #'menu-project-unmount *mounted-projects*)
  (setq *mpw-target* nil)
  (setq *current-project* nil))

(defun CheckoutNewerCur ()
  (check-out-projects *current-project*))

(defun checkoutNewerAll ()
  (apply #'check-out-projects *mounted-projects*))

(defun check-in-current-project ()
  (check-in-projects *current-project*))

(defun check-in-all ()
  (apply #'check-in-projects *mounted-projects*))

(defun toggle-scan-on-update ()
  (set-menu-item-check-mark (find-menu-item *projector-menu* "Scan on update")
                            (setf *scan-on-update* (not *scan-on-update*))))

(defun toggle-scan-on-mount ()
  (set-menu-item-check-mark (find-menu-item *projector-menu* "Scan on mount")
                            (setf *scan-on-mount* (not *scan-on-mount*))))

(defun toggle-scan-on-mount-is-incremental ()  
  (set-menu-item-check-mark (find-menu-item *projector-menu* "Scan on mount is incremental")
                            (setf *scan-on-mount-is-incremental* (not *scan-on-mount-is-incremental*))))

#|
projector menu has these:
print-project-info :newer :comments :latest :checked-out :recursive :project :file
show-local-file-info filename
describe-modifiable-files
cancel-checkout-file
cancel-modro-file
orphan-file
check-in-file
check-out-file
modify-read-only-file
mount-all-projects
CheckoutNewerCur
CheckoutNewerAll
check-in-current-project
check-in-all
unmount-current-project
unmount-all-projects
merge-active-lisp-window-and-file
merge-directory-dialog
update-leibniz-version
|#

;;;
;;; Miscellaneous
;;;

(defun set-current-project (&optional (project *current-project*))
  ;; @@@ ought to be able to change to *projects*
  (unless (member project *mounted-projects*)
    (setq project (first *mounted-projects*)))
  (setq *current-project* project)
  (projector-menu-update))

;; @@@ this is specific to projector-projects; maybe it (and the relevant
;; slots) shouldn't be
(defun mount-project-volume (project &aux
                                         (remote (project-remote-pathname project))
                                         (alias (project-mount-alias project)))
  (when alias
    (probe-file alias))
  (loop (let ((path (probe-file remote)))
          (if path
            (return path)
            (cerror "Will retry accessing volume ~s" "Volume ~s not mounted. Mount it, then continue." (project-volume project))))))

(defun menu-projects-mount (projects)
  (project-handler-case (mount-projects projects)
    (project-volume-not-available (error)
                                  (if (mount-project-volume (project-error-project error))
                                    (invoke-restart 'volume-mounted)
                                    (error error))))
  (unless (menu-installed-p *projects-menu*)
    (menu-install *projects-menu*)
    (let ((pkg (find-package "ADBG")))
      (when pkg
        (let ((sym (intern "*BOTH-AFTER-MENUS*" pkg)))
          (when (boundp sym)
            (set sym (append (symbol-value sym) (list *projects-menu*))))))))
  (remove-menu-items *projects-menu* (menu-items *projects-menu*))
  (make-projects-menu)
  (projector-menu-update)
  (when *scan-on-mount*
    (unless *scan-on-mount-is-incremental*
      (scan-project-files))))

(defun menu-project-unmount (project)
  (project-unmount project)
  (remove-menu-items *projects-menu* (find-menu-item *projects-menu* (project-name project)))
  (remove-all-filename-menus project) ;;@@@ should no longer be necessary
  (remove-logical-host-for-project project)
  (when (eq *current-project* project)
    (set-current-project)))

(defun setup-initial-projects ()
  (when (boundp '*my-projects*)
    (dolist (project-data *my-projects*)
      (destructuring-bind (remote-pathname local-dir &key alias)
                          project-data
        (flet ((add-colon (pathname &aux (name (namestring (translate-logical-pathname pathname))))
                 (and pathname
                      (if (find (elt name (1- (length name))) ":'")
                        name
                        (concatenate 'string name ":")))))
          (let* ((remote-pathname (add-colon remote-pathname))
                 (projector-name (concatenate 'string
                                              (first (last (pathname-directory remote-pathname)))
                                              "º"))
                 (localdirname (add-colon local-dir)))
            (unless (probe-file localdirname)
              (error "Source directory ~a specified in ccl::*my-projects* was not found. Please check your directory and try again."
                     local-dir))
            (let ((project (or (find-projector-name projector-name)
                               (make-instance 'mpw-project
                                 :remote-pathname remote-pathname
                                 :local-dir (add-colon local-dir)
                                 :projector-name  projector-name
                                 :mount-alias alias))))
              (when project
                (setf (slot-value project 'remote-pathname) remote-pathname)
                (setf (slot-value project 'mount-alias) alias)))))))))

(defun reset-projects (&optional all-the-way?)
  ;; remove menu items from the ¸'s menu
  ;; make sure that project menu is gone since projects must be remounted
  (when all-the-way?
    (unmount-all-projects)
    (quit-source-server))
  (menu-deinstall *projects-menu*)
  (apply #'remove-menu-items *projects-menu* (menu-items *projects-menu*))
  (remove-all-filename-menus)
  (setq *projects* nil *mounted-projects* nil)
  (reset-pathname-cache)
  (when all-the-way? 
    (load-compare-preferences) ;; can you think of a better time to do this?
    (load-merge-preferences))
  )

(def-load-pointers reset-projects-on-restart ()
  (reset-projects t)
  )

;; one call to mount projects and update all files, not returning until it is done
(defun ccl::update-all-projects ()      
  ;; this is similarly true
  (when CCL::*my-projects*  ;; only do anything if there are projects to mount
    (unless *mounted-projects* ;; only mount projects if they are not already mounted
      (setq *scan-on-mount* NIL)
      (format t "~%~%  Mounting SourceServer projects...~%")
      (mount-all-projects))
    (checkoutNewerAll)))

#|
	Change History (most recent last):
	1	4/3/92	ows	split from projector.lisp
	2	4/3/92	ows	split from projector.lisp
	4	4/3/92	ows	fix a problem with file-cancel-checkout
	6	4/7/92	ows	cleaned up directory scanning
				added back the icon
				download closes windows, not files
	11	4/24/92	ows	fix some problems with check-in-all
	12	4/26/92	ows	check-in-all no longer closes its window too early
	13	4/29/92	ows	move fns to ui
				move help-desc to individual items
				enlarge the modreadonly cancel dialog
	3	3/30/93	sidney	change name of variable from char to xchar to
				avoid possible name collisions
	4	3/31/93	Sidney	Add Obsolete File command to menu
	5	4/4/93	Sidney	Use SourceServer package instead of CCL and use
				MCL 2.0 low-level system calls instead of ones
				from verison 1.3 that required loading a verison of
				sysequ.lisp that could lead to name collisions in
				the ccl package.
	6	4/5/93	sidney	Allow setting a read only file to modifiable when
				there is no mounted proiject
	7	4/5/93	sidney	Allow modify readonly when no project is open
	8	5/13/93	Sidney	Fix checkin of new file that is not a source file
	9	9/1/93	Sidney	Add menu items for unobsolete and remove files, and allow for checkin of files without CKID resource
	10	9/1/93	Sidney	Create project objects when mounting, not unmounting projects, so as not to preserve unwanted state
	11	9/1/93	Sidney	Increase size of error dialog
	12	2/11/94	sidney	Fix bug when filename is substring of file in the project
	13	2/25/94	sidney	Check if source directories exist when mounting a project
	14	3/3/94	sidney	Only try some editor stuff if there is an editor window
	15	3/3/94	sidney	More: Only try some editor stuff if there is an editor window
	16	3/8/94	sidney	Allow not asking for comments when checking out a file
	17	5/13/94	sidney	redisplay window after checkin or checkout
	18	6/7/94	sidney	RADAR #1164044: don't try to access source directory when first starting up lisp
	19	7/7/94	sidney	use string for resource id instead of keyword symbol
	20	7/14/94	sidney	get latest version of file when cancelling a modified-read-only, instead of getting local version
	21	8/3/94	chip	file-checkout now includes a protocol for the file deciding whether or not to include a comment
	22	8/22/94	chip	file-checkin now also includes the same protocol
	23 	 9/ 2/94	chip    	find-or-open-pathname uses the 'fred' function instead of calling make-instance of *default-editor-class*; this lets SourceServer take advantage of any improvements to 'fred'
	24 	 9/ 8/94	sidney  	fix Other File | Remove to work with orphaned, obsoleted files
	25 	10/19/94	dy      	scan-on-mount now incremental part of mounting; new *scan-on-update* global, default nil
	26 	10/19/94	dy      	introduce *scan-on-mount-is-incremental*
	27 	10/19/94	dy      	defun incremental-scan-project
	28 	10/21/94	dy      	scan on mount in Sourceserver menu
	29 	 3/18/95	sidney  	load merge preferences on reset
	30 	 3/19/95	sidney  	also load compare preferences on reset
	31 	 3/19/95	sidney  	quit sourceserver on reset so it isn't hanging around all the time
	3  	 8/11/95	sidney  	Better recovery when project volume is not mounted
	4  	11/28/95	Hernan  	comment-dialog should use a copy of the string that it is
						given for the comment. Otherwise some Fred demon might
						be messing with a string you care about.
	5  	 4/16/96	Hernan  	with-editor-excursion should check that the selection and
						scroll values are within range before trying to set them 
						anyway (just like the comment in the file says it should).
	6  	 4/22/96	Hernan  	Fixing file-checkin to make sure that the double squiggles 
						are not left in the file after the file is checked in (this 
						happened when a checkin all projects was done).
	7  	 7/ 7/96	sidney  	incorporating some changes that seem to be fixes in the MCL3.9 example version
	8  	 8/20/96	Hernan  	Added an optional argument to reset-projects. It is not safe
						to call this function with the argument set to t at start up time.
	9  	10/21/96	sidney  	fixed the problem that broke reset-projects at startup, so enable full reset again
	10 	 3/ 4/97	sidney  	define update-all-projects function that works synchronously
|# ;(do not edit past this line!!)
