;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories


;;;

(in-package :cl-user)

(labels ((strip-redef-warnings (window)
            (let ((buf (fred-buffer window))
                  (start 0)
                  (end 0))
              (loop
                (setq start (buffer-string-pos buf ";Warning:  FUNCTION " :start start))
                (when start
                  (setq end (buffer-string-pos buf "; While executing: RECORD-SOURCE-FILE" :start start)))
                (if (and start end)
                  (buffer-delete buf start (+ end 38))
                  (return)))))
         (make-filenames-bold (window)
            (let ((buf (fred-buffer window))
                  (start 0)
                  (end 0))
              (loop
                (setq start (buffer-string-pos buf ";Loading \"" :start end))
                (when start
                  (setq end (buffer-line-end buf start)))
                (if (and start end)
                  (buffer-set-font-spec buf '(:bold) start end)
                  (return)))))
         (save-transcript ()
            (let ((w (front-window :class 'listener))
                  (filename (multiple-value-bind (sec min hr date month year day) (get-decoded-time)
                              (declare (ignore day sec))
                              (format nil "ccl;SK8build-xscript ~a-~a-~a ~a'~a" month date (mod year 100) hr min))))
              (when w
                (strip-redef-warnings w)
                (make-filenames-bold w)
                (ccl::window-save-file w filename)))))
  (save-transcript))

#|
	Change History (most recent last):
	1	5/6/93	Sidney	
	2  	12/ 9/94	till    	Color Fred:
							Change to make-filenames-bold
	3  	 1/30/95	sidney  	changed from using unintern when we wanted to fmakunbound to just use labels
	4  	 3/ 1/95	Hernan  	Updating source to work with new Fred 3.0 code.
	2  	 7/28/95	sidney  	don't know if it will work, but replaced MCL2.0 idiom for top listener with MCL3.0 idiom
	2  	 2/27/97	Hernan  	
|# ;(do not edit past this line!!)
