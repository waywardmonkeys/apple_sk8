;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories

;;;

;; patches to MCL to make the default file creator a parameter and to set that to the app's creator type

(in-package :ccl)

(export '(*default-file-creator* sk8-application-class) "CCL")

(defparameter *default-file-creator* :|SK8 |)

;;; here's an expression that returns the filename of the application

(defun startup-pathname ()
  (merge-pathnames (ccl::startup-directory)
                   (%get-string (%int-to-ptr (+ 62 (%get-long (%int-to-ptr #x34e))
                                                (%get-word (%int-to-ptr #$CurApRefNum)))))))

;; cause the default creator type automatically to be that of a saved application
(defun set-default-creator-to-that-of-the-application ()
  (setf *default-file-creator* (mac-file-creator (startup-pathname))))

(pushnew 'set-default-creator-to-that-of-the-application *lisp-startup-functions*)

;; define an application class for SK8
;;The Application class did not exist in MCL 2.0. Using this would make some things simpler.
;; Here are included all the methods that MCL defines on the Lisp development application
;;   to make it easy to decide what we want to specialize when we do things more correctly.
;; At first, only the file creator is specialized, and the rest is commented out.

(defclass sk8-application-class (lisp-development-system)
  ())

(defmethod application-file-creator ((app sk8-application-class))
  *default-file-creator*)

#|
(defmethod toplevel-function ((a sk8-application-class) init-file)
  (call-next-method)
  )


(defmethod application-about-dialog ((app sk8-application-class))
  (call-next-method))

(defmethod open-application-document ((a sk8-application-class) path &optional startup)
  (call-next-method)
  )

(defmethod print-application-document ((a sk8-application-class) path &optional startup)
  (call-next-method)
  )

(defmethod Application-eval-enqueue ((app sk8-application-class) form)
  (call-next-method)
  )

;; This one is defined elsewhere
(defmethod application-error ((a sk8-application-class) condition error-pointer)
  (call-next-method)
  )
|#

;; None of these next ones are used even by MCL
#|
(defmethod application-name          ((app sk8-application-class)) nil)
(defmethod application-resource-file ((app sk8-application-class)) nil)
(defmethod application-sizes         ((app sk8-application-class)) nil)
(defmethod application-about-view ((app sk8-application-class))
  (call-next-method)
  )
|#

;; Here's where we tell MCL that we are a different application from now on.
;; If more of the above methods are redefined in a more drastic way, this might have to
;;   be turned on later in the build process.

(defparameter *application*
  (make-instance 'sk8-application-class))

#|
	Change History (most recent last):
	1	1/4/94	sidney	Make creator of output files a defparameter and set it to "SK8 "
	2	1/4/94	sidney	Get defconstants right
	3	1/4/94	kleiman	Default file extension should be lisp, not sk8
	1	2/21/94	sidney	move this file to a new subproject
	2	3/16/94	sidney	comment out redefinition of window-save-as, now defined in new Fred code
	3	6/29/94	chip	changed restypes to strings (from keywords) since symbols now have arbitrary case
	4  	 3/19/95	sidney  	default creator type now set to app's creator at lisp startup
	2  	 6/12/95	sidney  	use new MCL application class facilities to do this
	3  	10/21/96	sidney  	open project when project file is dropped on application
	4  	10/21/96	sidney  	whoops - can't define sk8 stuff this early in the build - move open document code to where openproject is defined
	5  	 2/27/97	Hernan  	
|# ;(do not edit past this line!!)
