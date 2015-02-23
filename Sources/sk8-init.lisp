;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MacFrames INIT
;;;

#| Modification History

10-03-92 ruben sk8 version is a floanum
08-21-92 ruben development, release, runtime, project store and sk8 ui options
05-12-92 ruben d29 conversion
03-25-92 ruben arglist-on-space now MCL default
02-12-92 ruben simplified with initial boot dialog
01-03-92 ruben changed it for MacFrames II (d28)
08-09-91 ruben added kernel-only-p option for Matthew
08-07-91 ruben always save local symbols (for releases as well as development versions)
04-18-91 ruben

|#

;;;
;;; INSTRUCTIONS:
;;;
;;;        This file contains all the information necessary to boot MacFrames.
;;;        Insert this code into your init to load MacFrames.  The MacFrames
;;;        Kernel and User Interface system will be loaded.  Comment out
;;;        below the features that you don't want.
;;;

(in-package :cl-user)

;; make sure that the init file is loaded, even if this file was started by launching its fasl
;; Note: since launching MCL from the app or a source file will load init, you have to make sure that
;;  whatever is in there can be loaded twice without ill effects.
;; This is to allow us developers to customize our build environments. There should
;;  not be an init.lisp checked in under version control, and a normal build will not load one.

(load "ccl;init" :if-does-not-exist nil)
    
;;; Get rid of some bothersome defaults used by MCL 2.0

(setq *paste-with-styles* nil)

;;; This variable is used by the save application code to know whether it is
;;; saving SK8, a runtime or an application. The possible values are :sk8, :runtime and :app.

(defvar *application-type* :sk8)

(defvar *development-preferences*
  '(:objectSystem :graphicsSystem :Sk8Script :Store 
    :functions :objects :ui :developmentUtilities))

(defvar *full-release-preferences* 
  '(:objectSystem :graphicsSystem :Sk8Script :Store 
    :functions :objects :ui))

(defvar *runtime-release-preferences*
  '(:objectSystem :graphicsSystem :SK8ScriptRuntime))

(defvar *features-wanted* (if *development-p* *development-preferences* *full-release-preferences*))

(defparameter *full-recompile-wanted* nil)
(defvar *update-all-projects-wanted* nil)
(defvar *save-when-done-wanted* t)
(defvar *log-file-wanted* t)
(defvar *sk8-version-to-be-built* "")

(defvar *boot-dialog* nil)
(defvar *VersionText* nil)

(defun sk8-boot-dialog ()
  (let* ((RuntimeRB (MAKE-DIALOG-ITEM
                     'RADIO-BUTTON-DIALOG-ITEM
                     #@(235 46)
                     #@(113 16)
                     "Runtime"
                     #'(LAMBDA (item$) 
                         (declare (ignore item$))
                         (setf *application-type* :runtime)
                         (setf (getf *sk8-features* :development) nil)
                         (setf *features-wanted* *runtime-release-preferences*))
                     :RADIO-BUTTON-PUSHED-P (eq *features-wanted* *runtime-release-preferences*)))
         (DevelopmentRB (MAKE-DIALOG-ITEM
                         'RADIO-BUTTON-DIALOG-ITEM
                         #@(234 94)
                         #@(116 16)
                         "Development"
                         #'(LAMBDA (item$) 
                             (declare (ignore item$))
                             (setf *application-type* :sk8)
                             (setf (getf *sk8-features* :development) t)
                             (setf *features-wanted* *development-preferences*))
                         :RADIO-BUTTON-PUSHED-P (eq *features-wanted* *development-preferences*)))
         (ReleaseRB (MAKE-DIALOG-ITEM
                     'RADIO-BUTTON-DIALOG-ITEM
                     #@(235 73)
                     #@(72 17)
                     "Release"
                     #'(LAMBDA (item$) 
                         (declare (ignore item$))
                         (setf *application-type* :sk8)
                         (setf (getf *sk8-features* :development) nil)
                         (setf *features-wanted* *full-release-preferences*))
                     :radio-button-pushed-p (eq *features-wanted* *full-release-preferences*)))
         (UpdateCB (MAKE-DIALOG-ITEM
                    'CHECK-BOX-DIALOG-ITEM
                    #@(17 46)
                    #@(149 16)
                    "Update all projects"
                    #'(LAMBDA (item$)
                        (setf *update-all-projects-wanted* (check-box-checked-p item$)))
                    :CHECK-BOX-CHECKED-P *update-all-projects-wanted*)))
    (unless *development-p*
      (dialog-item-disable DevelopmentRB)
      (dialog-item-disable UpdateCB))
    (setq *boot-dialog*
          (MAKE-INSTANCE 'COLOR-DIALOG
            :WINDOW-TYPE
            :TOOL
            :WINDOW-TITLE
            "SK8 Boot Preferences"
            :VIEW-POSITION
            #@(40 40)
            :VIEW-SIZE
            #@(372 171)
            :VIEW-FONT
            '("Chicago" 12 :SRCOR :PLAIN)
            :VIEW-SUBVIEWS
            (LIST ReleaseRB
                  DevelopmentRB
                  (MAKE-DIALOG-ITEM
                   'STATIC-TEXT-DIALOG-ITEM
                   #@(18 14)
                   #@(75 16)
                   "VERSION:"
                   NIL
                   :VIEW-FONT
                   '("Chicago" 14 :SRCOR :PLAIN))
                  (MAKE-DIALOG-ITEM
                   'CHECK-BOX-DIALOG-ITEM
                   #@(17 70)
                   #@(163 16)
                   "Recompile Everything"
                   #'(LAMBDA (item$)
                       (setf *full-recompile-wanted* (check-box-checked-p item$)))
                   :CHECK-BOX-CHECKED-P *full-recompile-wanted*)
                  UpdateCB
                  (MAKE-DIALOG-ITEM
                   'CHECK-BOX-DIALOG-ITEM
                   #@(17 94)
                   #@(131 16)
                   "Save when done"
                   #'(LAMBDA (item$)
                       (setf *save-when-done-wanted* (check-box-checked-p item$)))
                   :CHECK-BOX-CHECKED-P *save-when-done-wanted*)
                  (MAKE-DIALOG-ITEM
                   'CHECK-BOX-DIALOG-ITEM
                   #@(17 118)
                   #@(131 16)
                   "Produce log file"
                   #'(LAMBDA (item$)
                       (setf *log-file-wanted* (check-box-checked-p item$)))
                   :CHECK-BOX-CHECKED-P *log-file-wanted*)
                  RuntimeRB
                  (MAKE-DIALOG-ITEM
                   'BUTTON-DIALOG-ITEM
                   #@(291 131)
                   #@(62 16)
                   "OK"
                   #'(LAMBDA (item$)
                       (let ((*boot* t))
                         (declare (special *boot*))
                         (setf *sk8-version-to-be-built*
                               (read-from-string
                                (dialog-item-text (car (subviews (view-window item$) 'fred-dialog-item)))))
                         (setf *VersionText*
                               (dialog-item-text (car (subviews (view-window item$) 'fred-dialog-item))))
                         (window-close (view-window item$))))
                   :DEFAULT-BUTTON T)
                  (MAKE-DIALOG-ITEM
                   'BUTTON-DIALOG-ITEM
                   #@(210 132)
                   #@(62 16)
                   "Cancel"
                   #'(LAMBDA (item$)
                       (let ((*boot* nil))
                         (declare (special *boot*))
                         (window-close (view-window item$))
                         (throw :cancel nil)))
                   :DEFAULT-BUTTON NIL)
                  (MAKE-DIALOG-ITEM
                   'EDITABLE-TEXT-DIALOG-ITEM
                   #@(99 14)
                   #@(209 17)
                   #+ppc-target "1.1 PPC" #-ppc-target "1.1 68K"
                   'NIL
                   :ALLOW-RETURNS NIL))))
    (defmethod window-close ((window (eql *boot-dialog*)))
      (declare (special *boot*))
      (call-next-method)
      (if (and (boundp '*boot*)
               *boot*)
        (eval-enqueue  `(boot-sk8)) (ed-beep)))
    t))

(defun boot-sk8 (&aux (starttime (get-universal-time)))
  (when *log-file-wanted*
    (let ((logfile (format nil "ccl:SK8 ~a~a log" (if (eq *application-type* :runtime) "runtime" "") *VersionText*)))
      (delete-file logfile :if-does-not-exist nil)
      (dribble logfile)))
  (when (and (boundp 'CCL::*my-projects*) *update-all-projects-wanted*)
    (ccl::update-all-projects))
  (get-global-recompile-date)
  (reset-sk8-globals)
  (catch :cancel
    (catch :abort
      (process-run-function
       '(:background-p t)
       #'(lambda (starttime)
           (declare (ftype t sk8::buildStandalone))  ;; avoid a warning message here
           (require :sk8-load)
           (let ((elapsed-time (- (get-universal-time) starttime)))
             (multiple-value-bind (hours nonhours)
                                  (floor elapsed-time 3600)
               (multiple-value-bind (minutes seconds)
                                    (floor nonhours 60)
                 (format t "~%~%Load of version ~a completed in ~d hours, ~d minutes, ~d seconds.~%~%"
                         *VersionText* hours minutes seconds )
                 (dribble)
                 ;; It's a shame the timing statistics go away.  We use AppleEvents to send them to some other app, which will display them -DY
                 (if *save-when-done-wanted*
                   (progn
                     (when (boundp 'CCL::*my-projects*)
                       (ccl::reset-projects)
                       (setf ccl::*user* nil ccl::*user-initials* nil))
                     (sk8::buildStandalone (format nil "sk8;SK8 ~a~a" (if (eq *application-type* :runtime) "runtime" "") *VersionText*)
                                           :creator ccl::*default-file-creator*))
                   (format t "(progn ~%  (ccl::reset-projects)~%  (setf ccl::*user* nil ccl::*user-initials* nil)~%  (sk8::buildStandalone \"sk8;SK8 ~a~a\" :creator ~s) )~%~%"
                           (if (eq *application-type* :runtime) "runtime" "") *VersionText* (string ccl::*default-file-creator*)))))))
       starttime))))

;;;;;;;;;;;;;;;;;;;;;;
;;; Put up the dialog:
;;;

(sk8-boot-dialog)

#|
	Change History (most recent last):
	3	10/25/93	kleiman	simplified build dialog
	4	12/6/93	kleiman	*trace-print-length* permanently NIL
	5	12/13/93	kleiman	Introduced reset-sk8-globals, now *development-p* et al will work as intended.
	6	12/16/93	till	add femto-defsystem (sk8-build-files).
	7	12/16/93	till	forgot something
	8	12/17/93	till	forgot something
	9	12/21/93	sidney	Changes so files can be compiled
	10	1/10/94	sidney	Show build time and version info
	11	2/4/94	sidney	Rebuild project files when recompiling everything
	11	2/11/94	sidney	Unnamed objects that can be gc'd
	12	2/15/94	kleiman	
	13	2/18/94	sidney	Stamp fasl with source file info and use that to decide when need to recompile
	14	2/21/94	sidney	Move much of earliest build code into dumped MCL image used for building this
	15	4/1/94	sidney	reset sourceserver userid before dumping sk8
	16	4/4/94	kleiman	save-sk8 -> buildStandalone
				sk8; prefix passed to buildStandalone
	17	6/12/94	dy	Just do the save, for godssake.  Or: I thought I told you to shut up!
	18	6/19/94	dy	2 new checkboxes in the dialog: mount projects and update all
	19	6/20/94	dy	tweak dialog
	20	8/2/94	Hernan	Making the code that mounts all projects be a
				separate function so that I can call it from the doc
				building dialog.
	21 	10/28/94	dy      	save listener buffer as log file
	22 	10/28/94	dy      	fix previous comment
	23 	11/15/94	dy      	improved "compile all"
	24 	12/ 4/94	sidney  	1200474: queued build stuff not being run during the build.
	25 	 2/10/95	sidney  	changes for buildstandalone merging resources into application file
	26 	 2/22/95	dy      	load init.lisp first thing
	27 	 2/22/95	sidney  	don't crash if there is no init file in the directory
	28 	 3/ 2/95	sidney  	do first thing first, not second thing first in build
	29 	 3/19/95	sidney  	use new :creator argument in buildstandalone to leave SK8's creator alone
	3  	 6/12/95	sidney  	add declaration to avoid a compiler warning
	4  	 7/10/95	sidney  	change default version string for now
	5  	 8/ 5/95	dy      	save log file when done.
	6  	 8/ 5/95	dy      	Log file didn't work.  Was empty.
	7  	 1/22/96	Hernan  	Adding a checkbox to determine whether a log file is produced.
	8  	 1/22/96	sidney  	Implement production of build log file
	9  	 2/ 6/96	sidney  	include log of file updates in the build log
	2  	 4/ 8/96	Hernan  	Moving the require :sk8-load into the process-run-function.
	3  	 4/ 8/96	Build   	Made sk8 version be 1.1
	4  	 4/15/96	Hernan  	Defining full-recompile-wanted as a defparameter (since
						this is a redefinition of the original in Build Part 1).
	5  	 4/25/96	Hernan  	Added *application-type*, a variable to let SK8 remember
						if it is a full SK8 app, a runtime or a derived user app.
	6  	 7/ 7/96	sidney  	remove redundant unmount-all-projects since reset-projects does that already
						make sure that buildstandalone happens in a listener process
	7  	 7/18/96	sidney  	move change to buildstandalone in build to buildstandalone function
	8  	10/24/96	sidney  	change default version name for built image to production name "1.1"
	9  	10/24/96	sidney  	Add PPC or 68K to default version name
						make default setting to not update all projects in deference to users who will not have a SourceServer project
						runtime/full release/development radio buttons weren't reseting properly if user changed mind after selecting runtime
						add the word runtime to the image file name when building a runtime
	10 	 3/ 4/97	sidney  	remove reference to sourceserver package that broke attempts to build a non-developer version
	11 	 3/ 7/97	sidney  	add/remove :development from *sk8-features* to reflect state of radio button selection
|# ;(do not edit past this line!!)
