;;;This file is used to load a stand alone sourceserver interface into MCL
(defpackage SourceServer (:use ccl common-lisp))

(in-package :SourceServer)
(import '(ccl::*user-initials* ccl::*user* ccl::*my-projects* ccl::reset-projects))

;; a few miscellanies needed by projector files

(defconstant wsp&cr
  (coerce '(#\space #\^M #\^I #\^L #\^@ #\^J #\312) 'string))

(defconstant $AppParmHandle #xAEC)	 ; handle to hold application parameters

(eval-when (:load-toplevel :execute)
  (let* ((*load-verbose* t)
         (ccl:*autoload-traps* t)
         (thisdir (pathname-directory *loading-file-source-file*))
         (thishost (pathname-host *loading-file-source-file*))
         (thatdir (append thisdir '("Projector")))
         (top-files-1 '("ui-utilities"
                        ;; "plot-icon"   ;; MCL3.9 has these definitions built in. Maybe need in 3.0?
                        ;; "appleevent-toolkit"
                        ))
         (projector-files-1 '(
                              "find-folder"
                              "sublaunch"
                              "projector-utilities"
                              "mpw-command"
                              ))
         (top-files-2       '(
                              "sourceserver-command"
                              ))
         (projector-files-2 '(
                              "compare" 
                              "merge"    
                              "read-only"
                              "projector"
                              "mpw-project"
                              "projector-menus"
                              "projector-ui"    
                              ))
         )
    (require :lispequ)
    (mapc #'(lambda(f) (ccl:compile-load (make-pathname :host thishost :directory thisdir :name f))) top-files-1)
    (mapc #'(lambda(f) (ccl:compile-load (make-pathname :host thishost :directory thatdir :name f))) projector-files-1)
    (mapc #'(lambda(f) (ccl:compile-load (make-pathname :host thishost :directory thisdir :name f))) top-files-2)
    (mapc #'(lambda(f) (ccl:compile-load (make-pathname :host thishost :directory thatdir :name f))) projector-files-2)
    ))
;;;
;; set these to whatever is appropriate

; *my-projects* is a list of 2 element lists. In each sublist
; first is the directory containing the project database, 
; second is directory containing the local files.
; If you do not have any projects, set *my-projects* to nil, and
; use New Project on the source server menu to create a new project.

(setq *my-projects* nil)

;; Here's an example of how to set SourceServer::*my-projects*
;; Note that you don't have to specify the subprojects that are in subdirectories of a root project

;;(setq ccl::*my-projects* '(("SweetPea:MCL work in progress:SourceServerProject:"
;;                             "SweetPea:MCL work in progress:SourceServer:")
;;                              ))

(setq *user-initials* nil) ;; will be set to chooser name if nobody initializes it to something else
(setq *user* "Firstname Lastname")  ;; This does have to be set in user's init.lisp


(menu-install *projector-menu*)



#|
	Change History (most recent last):
	2	6/4/91	tv 	adding compare / merge functionality
	3	6/7/91	   	testing checkout/in before release
	4	6/25/91	tv 	testing
        5               alms    push onto dependendencies list
	6	7/17/91	jaj 	moved a lot of stuff to Leibniz.lisp, projector-package and projector-ff

	3	3/28/93	sidney	Changed hardcoded disk name in location of
				project files. I don't think that should be
				necessary.
	4	3/28/93	sidney	Set logical hosts for this project to assume that the files are kept under the ccl directory, and added some comments.
	5	3/30/93	sidney	Load project files from the directory containing
				this file instead of using a hardcoded pathname.
				
				Use compile-load function to conditionally compile
				files to make it easy to keep files as fasls.
	6	4/4/93	Sidney	Use SourceServer package instead of CCL and use
				MCL 2.0 low-level system calls instead of ones
				from verison 1.3 that required loading a verison of
				sysequ.lisp that could lead to name collisions in
				the ccl package.
	7	4/8/93	sidney	Fix case in which pathname of load file is specified
				using a logical host.
	8	4/12/93	SIdney	Move AppleEvent toolkit file from examples folder
				into the same directory as the rest of
				SourceServer and put it under version control
	9	3/29/94	sidney	Move appleEvent support to base MCL
	2  	 6/13/96	sidney  	Move require lapmacros to where it might be needed
	3  	 7/ 1/96	sidney  	some changes for MCL3.9
	4  	 3/ 4/97	sidney  	get reset-projects in ccl package so it can be conditionally called by code that doesn't require sourceserver package
|# ;(do not edit past this line!!)
