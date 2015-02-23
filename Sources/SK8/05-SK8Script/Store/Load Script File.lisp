;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories

(in-package :sk8dev)

;;; Brings up a fred window with the error printed in the mini buffer. The error
;;; part of the script is automatically selected. This function is designed to work
;;; from read-next-sk8script-form and also from the Fred SK8Script utilities file.
;;; The meaning of the arguments:
;;; endPos: the position in the stream where we stoped reading the file.
;;; sourceFile: pathname of file we are reading
;;; formTranslated: the string that we were compiling. Needed to count how many lines long it is.
;;; errorList: a list of error structs (as returned by translateScriptCommandOrExpr) or a string.
;;;         If a string, the string is the description of the error.

(defun bring-up-error-on-fred-window (endPos sourceFile formTranslated errorList &key (causeerror nil))
  (let* ((errorObj (and (listp errorList) (car errorList)))
         (errorDescription (if (stringp errorList) 
                             errorList
                             (if errorObj (descriptionOfError errorObj) ""))))
    (when (null sourceFile)
      (messageTouser (format nil "Error found while evaluating in an unsaved source file.  Form was:~%\"~a\"~%Error is \"~a\"." 
                             formTranslated
                             errorDescription))
      (return-from bring-up-error-on-fred-window nil))
    (let* ((theWindow (ed sourceFile))
           (theBuffer (fred-buffer theWindow))
           (bufSize (buffer-size theBuffer))
           (formSize (count #\Newline formTranslated))
           (lineOfError (if errorObj (lineOfError errorObj) 0))
           (streamPosition endPos)
           (positionOfError (buffer-line-start theBuffer streamPosition (- formsize))))
      (setf positionOfError (buffer-line-start theBuffer positionOfError lineOfError))
      (collapse-selection theWindow nil)
      (if errorObj
        (set-selection-range theWindow
                             (min (+ positionOfError (car (characterPositionOfError errorObj))) bufSize)
                             (min (+ positionOfError (cadr (characterPositionOfError errorObj))) bufSize))
        (set-mark theBuffer (min positionOfError bufSize)))
      (fred-update theWindow)
      (ccl::clear-mini-buffer-maybe theWindow)
      (set-mini-buffer theWindow "Error: ")
      (set-mini-buffer theWindow errorDescription)
      (ed-beep)
      (when causeerror
        (cerror "Error found while loading SK8Script file.  See the text window of the file for more information.  Continue from this error will continue loading the file AFTER the form that caused the error."
                'error))
      )))


;; given an input stream, return the next chunk of sk8script text from it
;; should there be a values indicating it is a comment?
;; eof-form is value to return at eof
;; stream is left positioned so that this can be called again for next form
               
(defun read-next-sk8script-form (source)
  (let ((multilineforms '(("on " . "end ") ("with " . "end with") ("repeat " . "end repeat")))
        endmultiline 
        multilineStartPos
        inchunk inline)
    (setq inchunk (read-line source nil nil)) ; Read next line
    (flet ((line-continues? (currLine)
             (let* ((pos (position #\Â currLine :from-end t :test #'eq))
                    (newpos (when pos (position-if-not #'(lambda (i)
                                                           (memq i '(#\Tab #\Space)))
                                                       currLine :start (incf pos)))))
               (and pos
                    (or (eql pos (length currLine))
                        (not newpos)      ;;;this finds next non whitespace after continuation.   we are ok if there is none or...
                        (string= "--" currLine :start2 newpos :end2 (+ 2 newpos))  ;;;...if it is a comment
                        ))))
           (comment? (currLine)
             (let ((newpos (position-if-not #'(lambda (i)
                                                (memq i '(#\Tab #\Space)))
                                            currLine)))
               (when (or (null newpos)  ;; blank line is treated as comment
                         (string= "--" currLine :start2 newpos :end2 (+ 2 newpos)))
                 :comment)))
           (lisp-block-comment? (currLine)
             (let ((newpos (position-if-not #'(lambda (i)
                                                (memq i '(#\Tab #\Space)))
                                            currLine)))
               (when (and newPos 
                          (or (string= "#|" currLine :start2 newpos :end2 (+ 2 newpos))
                              (string= "(*" currLine :start2 newpos :end2 (+ 2 newpos))
                              ))
                 :comment))))
      (cond
       ((null inchunk)
        (close source)
        (values nil nil :eof))
       ((lisp-block-comment? inChunk)
        (setf multilineStartPos (- (ccl::stream-position source) (length inchunk))) 
        ;; Throw lines away until we find the end of the block comment.
        (loop
          (setq inchunk (read-line source nil nil))
          (unless inchunk
            ;; eof before reading the end of the block comment? Complain!
            (close source)
            (return (values (format nil "LOADSCRIPT ERROR: end of file while looking for the end of a block comment")
                            multilineStartPos
                            :error)))
          ;; Is the end block here? We are done.
          (when (or (search "|#" inchunk) (search "*)" inchunk))
            (return (values inchunk nil :comment)))))
       ((setf endmultiline (cdr (assoc inchunk multilineforms
                                       :test #'(lambda(a b)
                                                 (let ((l (length b)))
                                                   (and (>= (length a) l)
                                                        (string-equal (subseq a 0 l) b)))))))
        (setf multilineStartPos (- (ccl::stream-position source) (length inchunk)))
        (let ((lengthend (length endmultiline)))
          (loop
            (setq inline (read-line source nil nil))
            ;;(update-progress)
            (unless inline
              (close source)
              (return (values (format nil "LOADSCRIPT ERROR: end of file while trying to load the following multiple line form:~%~a" inchunk)
                              multilineStartPos
                              :error)))
            (setq inchunk (concatenate 'string inchunk #.(string #\Newline) inline))
            (when (and (>= (length inline) lengthend)
                       (string-equal (subseq inline 0 lengthend)  endmultiline))
              (return inchunk)))))
       ;; It's a command that continues on the next line
       ((line-continues? inchunk)
        (setf multilineStartPos (- (ccl::stream-position source) (length inchunk)))
        (loop
          (setq inline (read-line source nil nil))
          ;;(update-progress)
          (unless inline
            (close source)
            (return (values (format nil "LOADSCRIPT ERROR: end of file while trying to load following command:~%~a" inchunk)
                            multilineStartPos
                            :error)))
          (setq inchunk (concatenate 'string inchunk #.(string #\Newline) inline))
          (unless (line-continues? inline)
            (return (values inchunk nil (comment? inchunk))))))
       (t (values inchunk nil (comment? inchunk)))))))
     
;;; scriptfile-forms-eval-or-compile -- does the actual work of reading a SK8Script source file and either evaling or returning Lisp forms
;;;   myfile - the filen name to load
;;;   project - either a project object, false, or a string.
;;;           If a project, then it's the project into which to load the sources
;;;           If false, then look for the name of the project into which to load the sources
;;;             in the first line of the text file following the characters "-->" (which must be the first
;;;             characters in the file)
;;;           If a string, then create a project with that name and load the sources into that project. If the
;;;             project with that name already exists, then use that project.
;;;  inproject - if the project argument is a string, then this is the project into which the new project should
;;;           be created. (Default = SK8 project)
;;;  verbose - print warnings and progress into the console box or the listener
;;;          0 = shows progress dialog, no other output
;;;          1 = prints name of each file being loaded, shows progress dialog
;;;          2 = 1 + prints each statement being loaded, shows progress dialog
;;;          false = prints nothing, no progress dialog
;;;  console - if true, prints verbose statements into lisp listener (the default when in our internal dev mode), else into message log
;;;  warnings - flag passed to the script translator, t or nil, controls logging of warning messages
;;;
;;; eval-p - if true forms are evaled as they are t ranslated and the function returns T or NIL for success or failure
;;;        if false, the forms are not evald and the function returns the list of lisp forms so that they can be compiled, evald or whatever

(defun scriptfile-forms-eval-or-compile (myfile verbose proj inproject console warnings eval-p)
  (declare (ignore warnings))
  (when (and verbose (not (member verbose '(0 1 2)))) (error "Verbose must be false, 0, 1 or 2"))
  (flet ((projnamed (proj)
           (dolist (p (knownChildren Project))
             (if (equalp (symbol-name (sk8::sk8_id p)) proj)
               (return p)))))
      (with-open-file (source myfile)
        (let ((filename (mac-file-namestring myfile)))
          (unwind-protect
            (let (*load-file-namestring* ; used by loadfilephysicalname
                  inchunk positionOfTrouble rescode result forms)
              (declare (special *load-file-namestring*))
              (labels ((msg (msg &optional error)
                         (if console (print msg) (sendToLog msg))
                         (when error
                           (messagetouser msg)
                           (error msg))))
                (when (stringp proj)
                  (cond ((setq result (projnamed proj))
                         (setq proj result))
                        (t (setq proj (new project :objectname proj :project inproject)))))
                
                ;; If project wasn't provided, attempt to find out which it is from 1st line's '-->' delimiter:
                (unless proj
                  (setq inchunk (read-line source nil nil)) ; Read first line
                  (unless inchunk
                    (close source)
                    (return-from scriptfile-forms-eval-or-compile (if eval-p t nil)))
                  (when (and (>= (length inchunk) 3)
                             (string-equal "-->" inchunk :end2 3))
                    (unless (and (setq proj (subseq inchunk 3))
                                 (setq proj (read-from-string proj nil))
                                 (setq proj (symbol-name proj))
                                 (setq proj (projnamed proj)))
                      (msg (format nil "LOADSCRIPT ERROR: Skipped loading file ~a: no project provided" filename))
                      (return-from scriptfile-forms-eval-or-compile nil))))
                (unless proj (error "Project not provided"))
                
                (multiple-value-setq (inchunk positionOfTrouble rescode)
                  (read-next-sk8script-form source)) ; Read next form
                
                (setq *load-file-namestring* myfile)
                (withproject proj
                  (when (and verbose (> verbose 0))
                    (msg (format nil "~a in project ~a SK8Script file ~a..."
                                 (if eval-p "Loading" "Translating") (objectstring proj) filename)))
                    (when eval-p (mapc #'sendUpdateEvent (contents stage)))
                    (loop
                      (case rescode
                        (:eof
                         (return))
                        (:error
                         (bring-up-error-on-fred-window (1- positionOfTrouble) myFile "" inchunk :causeerror t)
                         (return))
                        (:comment nil)
                        (t
                         (when (eql verbose 2)
                           (msg (format nil "> ~a" inchunk)))
                         (sk8-multival-bind (fail? errors locals globals result)
                                            (translateScriptCommandOrExpr proj inchunk)
                           (if fail?
                             (bring-up-error-on-fred-window (1- (ccl::stream-position source)) myFile inchunk errors :causeerror t)
                             (if eval-p
                               (progn 
                                 (setf result (evaluateScriptTranslation result))
                                 ;; When there is a valid target project AND the thing we just
                                 ;; compiled was a function or handler, save it into the store. 
                                 (when (and (not (memq proj (list sk8 ui))) 
                                            (or (and (symbolp result) (fboundp result))
                                                (eq (type-of result) 'standard-method)))
                                   (PS::savehandlerVersionFromText proj result inChunk locals globals)))
                               (push 
                                `(PS::savehandlerVersionFromText ,proj
                                                                 ,(skil::skil-compile-to-lisp (second result))
                                                                 ,inChunk 
                                                                 ',(get-handler-locals)
                                                                 ',(get-handler-globals))
                                forms) )))))
                      (multiple-value-setq (inchunk positionOfTrouble rescode)
                        (read-next-sk8script-form source)) ; Read next form
                      ) ; End of read loop
                    (when eval-p (mapc #'sendUpdateEvent (contents stage)))
                    )
                (when (and verbose (> verbose 0))
                  (msg (format nil "Finished ~a SK8Script file ~a in project ~a"
                               (if eval-p "loading" "translating") filename (objectstring proj))))
                (if eval-p
                  t
                  (nreverse forms))) ; SUCCESSFUL RETURN
              )
            )))
    ))


#|
	Change History (most recent last):
	1  	 4/ 3/96	Hernan  	New file. Implements loading script files. It also adds a 
						function to show errors in a Fred buffer when they happen.
	2  	 4/26/96	Brian   	Making errors in script loading cause a break so it stops
						loading.
	3  	 4/26/96	Brian   	fixing error message of bring-up-error...
	4  	 4/26/96	Brian   	
	5  	 4/26/96	Brian   	
	6  	 5/ 7/96	sidney  	remove use of withlockedcursor, which is not defined at this layer of build
	7  	 7/29/96	Hernan  	Adding error checking to bring-up-error-on-fred-window. No longer
						will crash when the position of error is beyond the buffer size.
	8  	11/ 7/96	Brian Roddy	Fixing bring-up-error to be more accurate.
	9  	12/13/96	Brian   	fixing scriptfile-forms-eval-or-compile to handle
						the noneval case by converting the skil forms
						to lisp before returning them.
	10 	 1/27/97	Brian Roddy	Handling error case when fred window has no
						source File.
	11 	 2/11/97	Brian Roddy	
	12 	 2/27/97	Hernan  	
|# ;(do not edit past this line!!)
