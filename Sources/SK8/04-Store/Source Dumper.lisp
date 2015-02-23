;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories


;;;

(in-package :SK8Development)

;;; go from anonymous object to its made-up temporary name
(defvar *anonymous-object-name-table*)

(defun temporary-name (obj)
  (gethash obj *anonymous-object-name-table*))

;;; count of anonymous object (used for suffixing made-up anonymous object names
(defvar *anonymous-object-count* 0)

(defun plain-script (obj h)
  (multiple-value-bind (unused1 unused2 unused3 script globals locals handlers breakpts
                        wstate wexps returns indent edit doc)
                       (ps::getHandlerInfo ;; (sk8::project obj)
                                           (name h)
                                           obj
                                           (ccl::method-qualifiers h)
                                           :version (version h))
    (declare 
     (ignore unused1 unused2 unused3 handlers breakpts wstate wexps returns indent edit doc))
    (values script globals locals)))

(defun plain-function-script (proj name)
  (multiple-value-bind (version private? locked? script globals locals)
                       (ps::getFunctionInfo proj name :version (ps::activefunction proj name))
    (declare (ignore version  private? locked? translation))
    (values script globals locals)))

(defun specialObjectstring (obj myproj)
  (let* ((objectname (objectname obj))
         proj baseparname)
    (cond (objectname)
          ((gethash obj *anonymous-object-name-table*))
          ((and (setq baseparname (objectname (baseparent obj)))
                (eq (setq proj (project obj)) myproj))
           (setq objectname (format nil "ANONYMOUS_~a_~a" baseparname (incf *anonymous-object-count*)))
           (setf (gethash obj *anonymous-object-name-table*) objectname)
           objectname)
          (proj
           (format nil "~%## ERROR reference to anonymous \"~a\" in project ~a   ERROR ##~%"
                   (objectstring obj) (objectname proj)))
          ((objectstring obj)))))

(define-handler SK8::GenerateSK8ScriptForHandler (object h name-string targetproj anonymous strm)
  (multiple-value-bind (script globals locals)
                       (plain-script me h)
    (declare (ignore locals))
    (when script
      (setq globals (delete (sk8::sk8_id me) (delete t (delete nil globals))))
      (with-input-from-string (instrm script)
        (if anonymous
          (multiple-value-bind (hproj hname hobj hqual)
                               (!exactLfunInfo (method-function h))
            (declare (ignore hproj hobj))
            ;; throw away the "on" line and generate an anonymous one
            ;; this is using internal functions from the sk8script editor to generate the first line of the handler
            (format strm "~%with targetObject ~a~%~a~%"
                    name-string
                    (handlerIDstring hname nil hqual
                                         :declaration t :project targetproj))
            (read-line instrm nil :eof))
          (format strm "~%~a~%" (read-line instrm nil :eof)))
        ;; note that the output functions for the rest of this handler
        ;; write the newline at the end of the string, not the beginning
        (when globals
          (format strm "~cGlobal ~a" #\tab (legal-sk8script-name-string (car globals) targetproj))
          (dolist (i (cdr globals))  (format strm ", ~a" (legal-sk8script-name-string i targetproj)))
          (format strm "~%"))
        ; write out handler, white out "Global"... lines, if any
        (loop
          (multiple-value-bind (inchunk positionOfTrouble rescode)
                               (read-next-sk8script-form instrm)
            (declare (ignore positionOfTrouble))
            (cond ((eq rescode :comment) (write-line inchunk strm))
                  (rescode (return))
                  ((not (let ((gstart (position-if-not #'(lambda (i)
                                                           (memq i '(#\Tab #\Space)))
                                                       inchunk)))
                          (and gstart
                               (>= (length inchunk) (+ gstart 6))   ; (length "Global") -> 6
                               (string-equal "Global" inchunk :start2 gstart :end2 (+ gstart 6)))))
                   (write-line inchunk strm)))))
        (if anonymous
          (format strm "end with~%")
          )))))

(define-handler SK8::GenerateSK8ScriptForHandlers (object name-string targetproj strm)
  (when (mf::object-class-p me)
    (let ((anonymous (or (eq me targetproj) (null (objectname me))))) ;; to allow for script to be independent of project name
      (dolist (h (localhandlers me))
        (sk8::GenerateSK8ScriptForHandler me h name-string targetproj anonymous strm)
        )
      )))

(define-handler sk8::TextDumpProjectFunction (project name &optional (strm t))
  (multiple-value-bind (txt globs locs)
                       (plain-function-script me name)
    (when txt
      (setq globs (delete (sk8::sk8_id me) (delete t (delete nil globs))))
      (let ((endln (ccl::position #\newline txt)))
        (if (and endln (or globs locs))
          (progn
            (format strm "~%~a" (ccl::subseq txt 0 endln))
            (when globs (format strm "~%~cGlobal ~a" #\tab (legal-sk8script-name-string (car globs) me))
                  (dolist (i (cdr globs))  (format strm ", ~a" (legal-sk8script-name-string i me))))
            (format strm "~a~%" (ccl::subseq txt endln)))
          (format strm "~%~a~%" txt))))))

(define-handler sk8::TextDumpProjectFunctions (project &optional (strm t))
  (dolist (name (functions me)) ;; (mapcar #'name (functions me)), changed it to send symbols (hernan)
    (sk8::TextDumpProjectFunction me name strm))
  )

(define-handler sk8::writeSources (project &optional (aFileName (format nil "ccl;~a.sk8" (sk8::objectname me))))
  (ensureHandlersSaved me)
  (ps::copy-all-pending-handlers) ;; make sure everything being edited is flushed to disk before we do anything else
  (mapc #'sendUpdateEvent (contents stage))   ;; is this the right place to make things look clean?
  (with-open-file (strm aFileName :direction :output :if-does-not-exist :create :if-exists :supersede)
    (withLockedCursor AnimatedClock
      (walk-project me (make-instance 'SK8Script-CodeGenerator) strm t '() t nil nil nil))))


#|
Change History (most recent last):
	1		2/23/94	kleiman	NEW FILE: contains utilities to dump sources
			                         of a project file or loaded project into text
	2		3/3/94	Hernan	The Great Argument Name Renaming of 94!
	3		3/6/94	chip	renamed to writeSourcesToFile
	4		3/16/94	kleiman	writeSourcesToFile function -> writeSources handler
	5		7/14/94	kleiman	Finish dumper
	5		8/3/94	kleiman	
	6  	 9/14/94	chip    	simplified use of valuePropagatedFrom
	7  	 9/26/94	kend    	Fixed a few bugs; removed dead code; now calls new SK8Script code generator.
	8  	 9/26/94	kend    	Comment confusion
	9  	 9/29/94	kend    	Updated function plain-script for changes to ps::gethandlerInfo.
	10 	 9/29/94	kend    	Forgot one comment marker
	11 	 9/29/94	It      	bad end comment
	12 	 9/29/94	It      	bad end comment
	13 	10/19/94	sidney  	removed redundant handler definition
	14 	10/20/94	kend    	New update to getHandlerInfo parameters
	15 	10/28/94	kend    	GenerateSK8ScriptForHandlers: white out Globals line; use globals info from code db.
	16 	10/31/94	kend    	Add newline after "Global"... line.
	17 	11/ 2/94	kend    	Needed another newline output from GenerateScriptForHandlers
	18 	11/ 2/94	kend    	GenerateScriptForHandlers simple fix
	19 	11/13/94	sidney  	flush any script editor windows before saving project as text
	20 	11/21/94	sidney  	Updated function plain-script for changes to ps::getFunctionInfo.
	21 	11/23/94	sidney  	1201967: corrected wrong use of return args in call to ps::getFunctionInfo
	22 	12/26/94	sidney  	add :skipInitialization keyword to new to support save as text
	23 	 1/12/95	sidney  	remove a bunch of dead code
	24 	 3/ 8/95	sidney  	use animated clock cursor in save as text
	25 	 3/29/95	sidney  	1233452: correct output syntax for globals whose names need to be escaped
	26 	 3/29/95	sidney  	correct egregious error introduced by typing in last fix too late at night
	27 	 3/30/95	sidney  	make output indepent of name of project
	28 	 3/31/95	sidney  	deal with complex handlers such as "set foo" on anonymous objjects on the project object
	29 	 4/ 3/95	sidney  	1235400: correctly process handler with global statement that shouldn't be there, and keep project in globals if handler is not on the project object
	30 	 4/ 4/95	sidney  	1235808: missing newline
	31 	 4/13/95	rod     	Fixing call to animatedclock
	32 	 4/26/95	kend    	Changed 'fileName' to 'aFileName' to avoid (M)CL's broken implementation of lexical scoping {can't shadow globals}
	2  	 6/19/95	sidney  	same fix for globals in functions as was done for handlers: don't output T and NIL as globals
	3  	 7/ 5/95	sidney  	abstract out some parts of generating sk8scrtipt for handlers and functions to use in fasl save/load
	4  	 8/28/95	Brian   	
	5  	11/30/95	sidney  	read-next-sk8script-form noo longer takes a second argument
	6  	 1/19/96	sidney  	removing/rewriting refrences to old sk8script compiler
	2  	 5/13/96	Hernan  	Uses the new API of read-next-sk8script-form.
	3  	 7/29/96	Hernan  	Various fixes to allow save as text to work.
	4  	 8/ 1/96	Hernan  	Why map the function names to strings? This looses the 
						package information that is already in the symbol... .
	5  	 2/27/97	Hernan  	
|# ;(do not edit past this line!!)
