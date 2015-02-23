;projector-menus

;; 01/23/92 gz   Don't bother eval-enqueing modify-read-only-file.
;;               Do bother eval-enqueing project unmounting.
;; 10/18/91 jaj  reorged and added a few menu items.  Dim menus when inappropriate.
;; 9/23/91 jaj  added unmount project menu-items
;; 8/23/91 jaj  minor changes to some names, call correct fns for checking in/out files

(in-package :SourceServer)

;; use a defvar so the user can set it in her preferences file

(defparameter *make-projects-menu* T)
(defvar *projector-menu*)
(defvar *projects-menu*)

(ignore-errors (menu-deinstall *projects-menu*))
(setq *projects-menu* (make-instance 'menu :menu-title "∏’s"))

(declaim (special *my-projects*
                  *scan-on-mount*
                  *scan-on-mount-is-incremental*
                  *scan-on-update*))



(defun front-window-projector-file (&aux (window (front-window)))
  (typecase window
    (listener)
    (fred-window (window-filename window))
    (projector-file-info-window (gf-pathname window))))

(defclass projector-menu-item (menu-item)
  ((need-project          :initarg :need-project          :initform nil)
   (need-active           :initarg :need-active           :initform nil)
   (need-local-state      :initarg :need-local-state      :initform nil)
   (check-on-local-state  :initarg :check-on-local-state  :initform nil)
   (check-on-global-state :initarg :check-on-global-state :initform nil)
   (need-scan-on-mount    :initarg :need-scan-on-mount    :initform nil)))

(defun make-projector-menu-item (title action &rest rest &key need-local-state &allow-other-keys)
  (declare (dynamic-extent rest))
  (apply #'make-instance 'projector-menu-item
         :menu-item-title title :menu-item-action action
         :need-local-state (if
                             (keywordp need-local-state)
                             (list need-local-state)
                             need-local-state)
         rest))

(defun update-projector-menu (theMenu)
  (declare (special *mounted-projects*))
  (let* ((projects-mounted-p *mounted-projects*)
         (pathname (front-window-projector-file))
         (project (and pathname (pathname-project-p pathname)))
         (potential (and pathname (or project (guess-pathname-project pathname))))
         (state (and project (handler-case
                               (project-file-local-state project pathname)
                               (file-not-in-a-project ()
                                (setq project nil)
                                nil)))))
    (labels ((enable-menu-items (items)
               (dolist (item items)
                 (typecase item
                   (menu (enable-menu-items (menu-items item)))
                   (projector-menu-item
                    (with-slots (need-project need-active
                                              need-local-state
                                              need-scan-on-mount
                                              check-on-local-state
                                              check-on-global-state)
                                item
                      (if (macrolet ((implies (a b) `(or (not ,a) ,b)))
                            (and (implies need-project projects-mounted-p)
                                 (implies need-active project)
                                 (implies need-local-state potential)
                                 (implies need-local-state
                                          (memq state need-local-state))
                                 (implies need-scan-on-mount *scan-on-mount*)))
                        (menu-item-enable item)
                        (menu-item-disable item))
                      #+never
                      (when check-on-local-state
                        (set-menu-item-check-mark item (eq state check-on-local-state)))))))))
      (enable-menu-items (menu-items theMenu))))
    
  (labels ((check-menu-items (items)
             (dolist (item items)
               (typecase item
                 (projector-menu-item
                  (with-slots (check-on-global-state)
                              item
                    (when check-on-global-state
                      (set-menu-item-check-mark
                       item
                       (case check-on-global-state
                         (:scan-on-update               *scan-on-update*)
                         (:scan-on-mount                *scan-on-mount*)
                         (:scan-on-mount-is-incremental *scan-on-mount-is-incremental*))))))))))
    (check-menu-items (menu-items theMenu))))


(defun enqueued-active-window-action (fn)
  (enqueued-action ((pathname (front-window-projector-file)))
    (funcall fn pathname)))

(defun enqueued-choose-file-action (fn string)
  (enqueued-action ((file (choose-file-dialog :button-string string)))
    (funcall fn file)))

(defparameter *other-active-menu*
  (make-instance
    'menu
    :menu-title "Other Active"
    :menu-items
    (list
     (make-projector-menu-item "File Info"
                               (enqueued-active-window-action 'show-file-info)
                               :command-key #\I
                               :need-active t)
     (make-projector-menu-item "Local Info"
                               (enqueued-active-window-action 'show-local-file-info)
                               :need-active t)
     (make-projector-menu-item "Checkout Info"
                               (enqueued-active-window-action 'show-remote-file-info)
                               :need-project t :need-active t)
     (make-instance 'menu-item :menu-item-title "-")
     (make-projector-menu-item "Cancel Checkout…"
                               (enqueued-active-window-action 'file-cancel-checkout)
                               :need-local-state :checked-out :need-project t)
     (make-projector-menu-item "Cancel ModifyReadOnly…"
                               (enqueued-active-window-action 'file-cancel-modro)
                               :need-local-state :modro :need-project t)
     (make-projector-menu-item "Orphan…"
                               (enqueued-active-window-action 'file-orphan)
                               :need-active t)
     (make-instance 'menu-item :menu-item-title "-")
     (make-projector-menu-item "Make File Obsolete…"
                               (enqueued-active-window-action 'file-obsolete)
                               :need-active t :need-project t)
     (make-instance 'menu-item :menu-item-title "-")
     (make-projector-menu-item "Get Latest Version"
                               (enqueued-active-window-action 'file-get-latest)
                               :need-local-state :checked-in :need-project t)
     (make-projector-menu-item "Get Other Version…"
                               (enqueued-active-window-action 'file-get-version)
                               :need-local-state :checked-in :need-project t)
     (make-instance 'menu-item
       :menu-item-title "Compare/Merge to File…"
       :menu-item-action (enqueued-action (merge-active-lisp-window-and-file))))))

(defparameter *project-info-menu*
  (make-instance 
    'menu
    :menu-title "ProjectInfo"
    :menu-items
    (list
     (make-projector-menu-item "Newer"
                               (enqueued-action (print-project-info :newer t :comments t))
                               :need-project t)
     (make-projector-menu-item "Latest with Comments"
                               (enqueued-action (print-project-info :latest t :comments t))
                               :need-project t)
     (make-projector-menu-item "Currently Checked Out"
                               (enqueued-action (print-project-info :checked-out t :recursive t :comments t))
                               :need-project t)
     (make-projector-menu-item "Short Latest"
                               (enqueued-action (print-project-info :short t :latest t))
                               :need-project t)
     (make-projector-menu-item "Project Log"
                               (enqueued-action (print-project-info :log t))
                               :need-project t)
     ;;; get info on all modifiable files in user checkout out directories
     (make-projector-menu-item "Modifiable on my disk"
                               (enqueued-action (describe-modifiable-files))))))

(defparameter *other-file-menu*
  (make-instance 
    'menu 
    :menu-title "Other File"
    :menu-items
    (list
     (make-projector-menu-item "File Info…"
                               (enqueued-choose-file-action 'show-file-info "File Info")
                               :need-project t)
     (make-projector-menu-item "Local Info…"
                               (enqueued-choose-file-action
                                'show-local-file-info "Local Info"))
     (make-projector-menu-item "Checkout Info…"
                               (enqueued-choose-file-action
                                'show-remote-file-info "Checkout Info")
                               :need-project t)
     (make-instance 'menu-item :menu-item-title "-")
     (make-projector-menu-item "Checkout…"
                               (enqueued-choose-file-action 'file-checkout "Check Out")
                               :need-project t)
     (make-projector-menu-item "Checkin…"
                               (enqueued-choose-file-action 'file-checkin "Check In")
                               :need-project t)
     (make-projector-menu-item "ModifyReadOnly…"
                               (enqueued-choose-file-action 'file-modro "ModifyReadOnly"))
     (make-instance 'menu-item :menu-item-title "-")
     (make-projector-menu-item "Cancel Checkout…"
                               (enqueued-choose-file-action 'file-cancel-checkout "UnCheckout")
                               :need-project t)
     (make-projector-menu-item "Cancel ModifyReadOnly…"
                               (enqueued-choose-file-action 'file-cancel-modro "UnModRO")
                               :need-project t)
     (make-projector-menu-item "Orphan…"
                               (enqueued-choose-file-action 'file-orphan "Orphan"))
     (make-instance 'menu-item :menu-item-title "-")
     (make-projector-menu-item "Make File Obsolete"
                               (enqueued-choose-file-action 'file-obsolete "Obsolete")
                               :need-project t)
     (make-projector-menu-item "Cancel File Obsolete"
                               (enqueued-choose-file-action 'file-unobsolete "Unobsolete")
                               :need-project t)
     (make-projector-menu-item "Remove File From Project"
                               (enqueued-choose-file-action 'file-remove "Remove")
                               :need-project t)
     (make-instance 'menu-item :menu-item-title "-")
     (make-projector-menu-item "Get Latest Version…"
                               (enqueued-choose-file-action 'file-get-latest "Get Latest")
                               :need-project t)
     (make-projector-menu-item "Get Other Version…"
                               (enqueued-choose-file-action 'file-get-version "Version…")
                               :need-local-state :checked-in :need-project t))))

(defparameter *scan-project-files-menu-item*
  (make-instance 'menu-item :menu-item-title "Scan Project Files"
                 :menu-item-action (enqueued-action (scan-project-files))))

(ignore-errors (menu-deinstall *projector-menu*))
(setq *projector-menu*
  (make-instance 'menu
    :update-function 'update-projector-menu
    :menu-title "Sourceserver"
    :menu-items
    (list
     (make-projector-menu-item "Checkout Active…"
                               (enqueued-active-window-action 'file-checkout)
                               :need-local-state '(:checked-in :modro) :need-project t
                               :check-on-local-state :checked-out)
     (make-projector-menu-item "Checkin Active…"
                               (enqueued-active-window-action 'file-checkin)
                               :need-local-state '(:checked-out :modro nil) :need-project t
                               :check-on-local-state :checked-in)
     (make-projector-menu-item "ModifyReadOnly Active"
                               #'(lambda () (let ((path (window-filename (front-window))))
                                              (file-modro path)))
                               ;;:need-local-state :checked-in
                               :check-on-local-state :modro)
     *other-active-menu*
     (make-instance 'menu-item :menu-item-title "-")
     (make-projector-menu-item "Mount my projects!"
                               (enqueued-action (mount-all-projects)))
     (make-projector-menu-item "New Project…"
                               (enqueued-action (new-project)))
     (make-projector-menu-item "Update All Projects"
                               (enqueued-action (checkoutNewerAll))
                               :need-project t)
     (make-projector-menu-item "Checkin All Projects"
                               (enqueued-action (check-in-all))
                               :need-project t)
     (make-projector-menu-item "Unmount All Projects"
                               (enqueued-action (reset-projects t))
                               :need-project t)
     (make-instance 'menu-item :menu-item-title "-")
     (make-projector-menu-item "Update Current Project"
                               (enqueued-action (CheckoutNewerCur))
                               :need-project t)
     (make-projector-menu-item "Checkin Current Project"
                               (enqueued-action (check-in-current-project))
                               :need-project t)
     (make-projector-menu-item "Unmount Current Project"
                               (enqueued-action (unmount-current-project))
                               :need-project t)
     (make-instance 'menu-item :menu-item-title "-")
     *project-info-menu*
     *other-file-menu*
     (make-instance 'menu-item :menu-item-title "-")
     (make-projector-menu-item "Scan on update"
                               (enqueued-action (toggle-scan-on-update))
                               :check-on-global-state :scan-on-update)
     (make-projector-menu-item "Scan on mount"
                               (enqueued-action (toggle-scan-on-mount))
                               :check-on-global-state :scan-on-mount)
     #+DYost
     (make-projector-menu-item "Scan on mount is incremental"
                               (enqueued-action (toggle-scan-on-mount-is-incremental))
                               :check-on-global-state :scan-on-mount-is-incremental
                               :need-scan-on-mount t)
     (make-instance 'menu-item :menu-item-title "-")
     (make-instance 'menu-item
       :menu-item-title "Merge Directories…"
       ::menu-item-action 'choose-and-merge-directories)
     *scan-project-files-menu-item*
     (make-instance 'menu-item :menu-item-title "-"))))


(menu-install *projector-menu*)

#|
	Change History (most recent last):
	4	1/23/92	gz	non-blocking Modro Active
	5	3/12/92	ows	use enqueued-command to set up dialogs immediately; factor out code
	6	3/25/92	ows	Adds the Leibniz-version menu item here, not in mpw-command
				Decide whether you're a Leibniz implementor according to *my-projects*, not the current zone.
				Change the scheme for updating menu items
	7	3/27/92	ows	no longer require ccl::*my-projects* to be bound when this file is loaded
	8	4/3/92	ows	call the new checkin/checkout commands
				remove the menus before adding them again (for loading the file twice)
	9	4/3/92	ows	make the menu items smarter
	10	4/3/92	ows	fix menu update routine for unlabeled front-windows
	10	4/4/92	ows	new windows confused the menu update fn
	12	4/7/92	ows	cleaned up directory scanning
				removed confusing check marks
	14	4/16/92	ows	fix update for non-project windows
	3	3/31/93	Sidney	Add Obsolete File command to menu
	4	4/1/93	Sidney	Add menu item to obsolete a file that isn't in an
				active window
	5	4/4/93	Sidney	Use SourceServer package instead of CCL and use
				MCL 2.0 low-level system calls instead of ones
				from verison 1.3 that required loading a verison of
				sysequ.lisp that could lead to name collisions in
				the ccl package.
	6	4/5/93	sidney	Allow modify readonly when no project is open
	7	9/1/93	Sidney	Add menu items for unobsolete and remove files, and allow for checkin of files without CKID resource
	8  	10/21/94	dy      	scan on mount in Sourceserver menu
	9  	10/21/94	dy      	some declamation
	10 	10/21/94	dy      	more "…" where appropriate
	2  	10/18/96	sidney  	quit sourceserver when unmounting all projects from menu
|# ;(do not edit past this line!!)
