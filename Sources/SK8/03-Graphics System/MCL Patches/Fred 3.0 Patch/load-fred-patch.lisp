;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories


;;;

;;; load-fred-patch.lisp
;;;
;;; Install the new buffer code
;;;

(in-package :ccl)

(defparameter *bit-bucket* (or *dribble-stream* (make-instance 'broadcast-stream :streams nil)))

(defun do-backtrace ()
  (let* ((dialog (new-backtrace-info
                  ()  
                  (%get-frame-ptr)
                  (if *backtrace-dialogs*
                    (child-frame (bt.youngest (car *backtrace-dialogs*)) *current-stack-group*)
                    (last-frame-ptr))
                  *current-stack-group*))
         (*backtrace-dialogs* (cons dialog *backtrace-dialogs*))
         (w (select-backtrace)))
    (while (wptr w)
      (ignore-errors (event-dispatch)))))

(defun load-new-fred ()
  (without-interrupts
   (let ((*warn-if-redefine* nil)
         (*warn-if-redefine-kernel* nil)
         (*terminal-io* *bit-bucket*)
         (*debug-io* *bit-bucket*)
         (*standard-output* *bit-bucket*)
         (*error-output* *bit-bucket*))
     (handler-bind ((serious-condition
                     #'(lambda (c)
                         (message-dialog 
                          (with-output-to-string (stream)
                            (let ((*error-output* stream))
                              (%break-message " Error" c (%get-frame-ptr))))
                          :ok-text "Backtrace")
                         (do-backtrace)
                         (quit))))
       ;; Kill windows, frecs and make sure they go away.
       (map-windows #'window-close :include-windoids t :include-invisibles t)
       ;; Load the patch to frec.
       (cl-user::sk8-build-files "graphics-patches;Fred 3.0 Patch:Frec does GWorlds")
       ;; Beep in joy!
       (ed-beep)
       ;; When in doubt, uncomment this!
       ;;(let ((theWindow (ed)))
       ;;  (format theWindow "Hello there, lucky hacker!!!"))
       ))))

(load-new-fred)

#|
	Change History (most recent last):
	1	3/2/94	Hernan	New file. Loads Fred patch.
	2	3/2/94	Hernan	Fixing pathnames.
	3	3/2/94	Hernan	New paths.
	4	3/4/94	Hernan	No need to define "ccl;" because it is defined
				already!
	5  	11/ 4/94	Hernan  	In addition to closing all the windows we need to
							kill all the frecs.
	6  	12/ 9/94	till    	Color Fred:
							changes to load-new-fred
							removed some other stuff
	7  	 3/ 1/95	Hernan  	New load files now that we have ported to Digitool's Fred 3.0
							We no longer require that annoying compile step
							before building.
	3  	 2/ 2/96	sidney  	compile the fred patch to a fasl file
	2  	 4/ 3/96	Hernan  	Fixing the pathname of the file to be loaded.
	3  	 2/27/97	Hernan  	
|# ;(do not edit past this line!!)
