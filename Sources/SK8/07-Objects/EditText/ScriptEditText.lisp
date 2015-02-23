;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories



(in-package :sk8dev)

(provide "SCRIPTEDITTEXT")

(require "EDITTEXT" "objects;EditText:EditText")
(require "MESSAGETOUSER" "objects;Dialogs:MessageToUserDialogBox")

;;; NEEDS TO LOAD THE INDENTATION FILE...

(cl-user::sk8-build-files
 "objects;EditText:Indent SK8Script")

;;; requires SK8Script compiler interface. 

;;; This file implements ScriptEditText: an editText that knows
;;; how to format SK8Script, compile and flag syntax errors. That's it.

;;; _______________________________ 
;;; The Object.
;;; _______________________________ 

(new editText :objectName "ScriptEditText" :project SK8)

;;; Scripts do not wrap!

(setf (wrapping ScriptEditText) nil)

;;; _______________________________ 
;;; The Fred class for this type of script.
;;; _______________________________ 

(defclass script-fred (sk8-fred)
  ())

;;; This method is left here just in case we need to specialize script freds.

(defmethod new-fred ((original script-fred) &key newSize newActor newWptr)
  (declare (ignore-if-unused newSize newActor newWptr))
  (let ((newFred (call-next-method)))
    ;; Install the new comtab.
    (setf (slot-value newFred 'comtab) *script-fred-comtab*)
    ;; Return the new field.
    newFred))

(setf (editData ScriptEditText) (make-instance 'script-fred
                                     :actor ScriptEditText
                                     :comtab *script-fred-comtab*
                                     :save-buffer-p t
                                     :copy-styles-p nil
                                     :buffer-chunk-size 1024
                                     :view-font '("Geneva" 9 :plain)))

(set-view-size (editData ScriptEditText) #@(10 10))

(buffer-set-font-spec (fred-buffer (editData ScriptEditText)) '("Geneva" 9 :plain))
(multiple-value-bind (ff ms ignored1 ignored2) (font-codes '("Geneva" 9 :plain))
  (declare (ignore ignored1 ignored2))
  (ccl::set-buffer-empty-font-codes (fred-buffer (editData ScriptEditText)) ff ms))

(setf (boundsRect ScriptEditText) (boundsRect ScriptEditText))  ; get the innards' size in synch
(setf (tabLength ScriptEditText) 8)
(setf (wrapping ScriptEditText) nil)

;;; _______________________________ 
;;; Patches to comtab.
;;; _______________________________ 

;;; This handler is called when the user is about to kill some text by typing a non
;;; deleting character. This happens, for example, when several lines of text are selected
;;; and the user presses #\a. This handler is provided to allow the editor to do something
;;; about the breakpoints. 

(define-handler lowLevelDelete (ScriptEditText start end)
  (declare (ignore start end))
  )

(define-handler lowLevelInsert (ScriptEditText theString position)
  (declare (ignore theString position))
  )

(defmethod ed-delete-with-undo ((f script-fred) start end &optional save-p reverse-p append-p)
  (declare (ignore save-p reverse-p append-p))
  ;; ed-history-undo can actually pass a string here!
  (when (stringp end)
    (setq end (+ start (length end))))
  (let ((fieldActor (slot-value f 'actor)))
    (lowLevelDelete fieldActor start end)
    (call-next-method)))

(defmethod ed-insert-with-undo ((f script-fred) str &optional position append-p thefont)
  (declare (ignore-if-unused append-p theFont))
  (let ((scriptText (slot-value f 'actor)))
    (lowLevelInsert scriptText str position)
    (call-next-method f str (buffer-position (fred-buffer f)) append-p theFont)
    ))

;;; _______________________________ 
;;; Balloon help functionality.
;;; _______________________________ 

(defun writeInfoForIdentifier (sym strm &optional tryHard)
  (declare (ignore tryHard))
  (let ((something? nil)
        (projName (objectName (or (CCL::package-project (symbol-package sym)) MF::*sk8-project*)))
        (fcn (fboundp sym)))
    (when (functionp fcn)
      (let ((args (arglist fcn)))
        (format strm "'~a' is a ~a in project ~a.  ~:[It takes no arguments.~;Its argument syntax is:~%~]"
                sym
                (if (typep fcn 'standard-generic-function) "handler" "function")
                projName
                args)
        (when args (!printHandlerArgs strm (function-name fcn) nil args))
        (setq something? t)))
    (when (boundp sym)
      (when something? (terpri strm) (terpri strm))
      (format strm "'~a' is ~:[~;also ~]~a in project ~a.  Its value is:~%"
              sym
              something?
              (cond ((constant-symbol-p sym) "a global constant")
                    ((proclaimed-special-p sym) "a global variable")
                    (t "an undeclared variable"))
              projName)
      (let ((val (symbol-value sym)))
        (writeObject val strm t))
      (setq something? t))
    (unless something?
      (format strm "'~a' is undefined" sym))))

(defun infoStringForIdentifier (sym)
  (if (memq sym skil::*SK8Script-Tokens*)
    (format nil "'~a' is a reserved word in the SK8Script language." sym)
    (with-output-to-string (s)
      (writeInfoForIdentifier sym s))))

(defmacro preserving-mouseLoc (newHLoc newVloc &body body)
  (let* ((oldHLoc (gensym))
         (oldVLoc (gensym)))
    `(sk8-multival-bind (,oldHLoc ,oldVLoc) (mouseLoc Stage)
       (prog2
        (setf (mouseLoc Stage) (list ,newHloc ,newVLoc))
        (progn ,@body)
        (setf (mouseLoc Stage) (list ,oldHLoc ,oldVloc))))))

(define-sk8-function popUpBalloon nil (left top message &key endOnMouseUp)
  (setf left (round left))
  (setf top (round top))
  (let* ((where (make-point left top))
         (balloonsOn? (#_HMGetBalloons)))
    (when (> (length message) 255)
      (setf message (subseq message 0 255)
            (char message 254) #\É))
    ;; If this is not here the help manager kicks in providing info on the window
    ;; behind the mouse. Only after it has done that it moves to the right place and
    ;; shows our information. 
    (preserving-mouseLoc left top
      (rset gs:*help-message-record* :hmmessageRecord.hmmString message)
      (unless balloonsOn? (#_HMSetBalloons t))
      (#_HMShowBalloon gs:*help-message-record* where (%null-ptr) (%null-ptr) 0 0 1)
      (unwind-protect 
        (without-interrupts 
         (loop
           (if endOnMouseUp
             (when (up mouse)
               (return))
             (progn
               (getKeys)
               ;; We only return when all keys are up.
               (when (and (zerop (%get-long mf::*keymap* 0))
                          (zerop (%get-long mf::*keymap* 4))
                          (zerop (%get-long mf::*keymap* 8))
                          (zerop (%get-long mf::*keymap* 12)))
                 (return))))))
        (without-interrupts
         (#_HMRemoveBalloon)
         (#_FlushEvents #$everyEvent 0)
         (unless balloonsOn? (#_HMSetBalloons nil)))))))

(defun PopupBalloonForHelp (sym ll tt)
  (let* ((str (infoStringForIdentifier sym)))
    (popUpBalloon ll tt str)))

(define-handler targetProject (ScriptEditText)
  SK8)

;;; The offset argument is provided to allow a field with a breakpoints strip to work.

(define-handler identifierHelp (ScriptEditText &optional (offset 0))
  (let* ((field me)
         (fred (editData field))
         (buf (fred-buffer fred))
         (clos-window (gs:node-window me))
         (where (view-position clos-window))    
         (focusProj (targetproject me))
         theWord fredHpos fredVpos)
    (with-fred-port fred
      (setf fredHpos (fred-hpos fred))
      (setf fredVpos (fred-vpos fred)))
    (multiple-value-bind (start end) (buffer-word-bounds buf)
      (setf theWord (buffer-substring buf start end)))
    (sk8-multival-bind (ll tt rr bb) (boundsRect field)
      (declare (ignore rr bb))
      (sk8-multival-bind (realLeft realTop) 
                         (gs:real-Logical-To-Physical (gs:node-container field) ll tt)
        (incf realLeft (+ fredHpos offset))
        (incf realTop fredVpos)
        (incf realLeft (point-h where))
        (incf realTop (point-v where))
        (let ((sk8sym (let ((*package* (SK8::package focusProj)))
                        (read-from-string theWord nil nil)))
              (skilsym (let ((*package* (find-package :skil)))
                         (read-from-string theWord nil nil))))
          (cond
           ((and skilsym (symbolp skilsym) (memq skilsym skil::*SK8Script-Tokens*))
            (PopupBalloonForHelp skilsym realLeft realTop))
           ((and sk8sym (symbolp sk8sym))
            (PopupBalloonForHelp sk8sym realLeft realTop))
           (t
            (ed-beep))))))))

;;; _______________________________ 
;;; Evaluation functionality.
;;; _______________________________ 

(define-handler scriptInputCompleted (ScriptEditText translation &key originalText Locals Globals)
  (declare (ignore translation originalText Locals Globals)))

;;; This handler evaluates the code in the field and returns the
;;; value of the evaluation. If there is a syntax error, the place where
;;; the error happened is flagged by color coding. 

(define-handler evaluate (ScriptEditText &key ((:project proj)))
  (sk8-multival-bind (failure? errors locals globals res)
                     (translateScriptCommandOrExpr proj (text me))
    (declare (ignore locals globals))
    (if failure?
      (indicateScriptSyntaxError me errors)
      (evaluateScriptTranslation res))))

;;; OJO! The concept of 1 lineheight for the whole Fred is not sound anymore.
;;; We'll fix this whenever the need arises...  Hernan -.

(defun frec-lineheight (frec)
  (let* ((theVector (ccl::fr.lineheights frec))
         (theSize (length theVector)))
    (if (zerop theSize)
      12
      (if (zerop (aref theVector 0))
        12
        (aref theVector 0)))))

(defun ed-fred-maybe-shrink-lineheight (edFred pos)
  (let* ((frec (frec edFred))
         (buf (ccl::fr.wposm frec))
         (oldLineHeight (frec-lineheight frec))
         newLineHeight)
    (declare (fixnum oldLineHeight newLineHeight))
    (multiple-value-bind (ff ms) (if (eql 0 (buffer-size buf))
                                   (CCL::buffer-font-codes buf)
                                   (CCL::buffer-char-font-codes buf pos))
      (multiple-value-bind (ascent descent maxWidth leading) (font-codes-info ff ms)
        (declare (fixnum ascent descent leading) (ignore maxWidth))
        (setq newLineHeight (+ ascent descent leading))))
    (when (< newLineHeight oldLineHeight)
      (buffer-remove-unused-fonts buf))))

(defun restore-scriptEditText-fontAttributes (self start end &key doesnt-shrink-lineheight (update t))
  (let* ((f (editData self))
         (buf (fred-buffer f))
         (bufSize (buffer-size buf)))
    (cond ((null start)   (setq start 0))
          ((eql -1 start) (setq start bufSize)))
    (cond ((null end)     (setq end 0))
          ((eql -1 end)   (setq end bufSize)))
    (buffer-set-font-spec buf `(:plain (:color ,(mcl-color *script-color*))) start end)
    (unless doesnt-shrink-lineheight (ed-fred-maybe-shrink-lineheight f start))
    (when update (forceRedraw self :updateScrollers t))))

;;; Called when a newline is about to be inserted
;;;   Returns
;;;     t,  to indicate the script has been handled and the insertion should NOT be done
;;;     nil,  to indicate the script has NOT been handled and the insertion SHOULD be done
;;;

(defun maybe-handle-complete-script (scriptText &optional insertionPos firstDirtyLine)
  (declare (ignore firstDirtyLine))
  (let* ((proj (targetProject scriptText))
         (edFred (editData scriptText))
         (buf (fred-buffer edFred)))
    (unless insertionPos (setf insertionPos (buffer-position buf)))
    (set-mark buf (buffer-size buf))
    (let ((insertionLine (buffer-line buf insertionPos)))
      
      ;; Be clever: determine if this is an eval or just a plain newline without using the parser.
      ;; Requirements for evaluation:
      ;; Cursor at the last line AND the last line is not a continuation line AND the
      ;; indentation level of the last line is 0.
      ;; If any of these tests fails, no evaluation is done. 

      (if (and (eq insertionLine (buffer-line buf (buffer-size buf)))          ;; last line?
               (not (continuation-line? buf (buffer-position buf)))            ;; not a continuation line?
               (zerop (indent-this-line edFred t)))                            ;; indentation level is 0?
        (progn
          ;; The expression or command must be fully parsed to determine whether it's complete... 
          (set-mark buf 0)
          (sk8-multival-bind (failure? errors locals globals res)
                             (translateScriptCommandOrExpr proj (text scriptText))
            (restore-scriptEditText-fontAttributes scriptText 0 t :doesnt-shrink-lineheight t :update nil)
            (cond
             ;; Valid expression or command; handle the input and return t to prevent the insertion
             ((not failure?)
              ;; This used to be in a process-run-function, but that was when the events were handled in the MCL event process
                     ;; where the evaluation of the expression could hang up things. Now that the event is queued up in an application process
                     ;; we don't have to use process-run-function
              (with-focused-view nil 
                (indent-this-line edFred)
                (scriptInputCompleted scriptText res 
                                      :originalText (text scriptText) 
                                      :locals locals :globals globals))
              #+ignore     ;; what it was
              (process-run-function "Script Evaluation" 
                                    #'(lambda () 
                                        (with-focused-view nil 
                                          (indent-this-line edFred)
                                          (scriptInputCompleted scriptText res 
                                                                :originalText (text scriptText) 
                                                                :locals locals :globals globals))))
              t)
             ;; Incomplete expression or command; return nil to allow the insertion
             ;; ((scriptIncompleteError res) nil)
             ;; Syntax error; mark it and return t
             (t (indicateScriptSyntaxError scriptText errors)
                t))))
        ;; Insertion should be done.
        nil))))

(defun maybe-complete-script (me)
  (when (not (option-key-p))
    (let* ((f (editData me))
           (buf (fred-buffer f))
           (bufSize (buffer-size buf))
           (end bufSize))
      ;; Kill leading and trailing whitespace!
      (setq end bufSize)
      (loop
        (when (eql 0 end) (return))
        (unless (eq #\Return (buffer-char buf (decf end)))
          (return (incf end))))
      (unless (eql end bufSize) (buffer-delete buf end bufSize))
      (setq bufSize end
            end 0)
      (loop
        (when (eql end bufSize) (return))
        (unless (eq #\Return (buffer-char buf end))
          (return))
        (incf end))
      (unless (eql end 0) (buffer-delete buf 0 end))
      ;; When the whole thing's selected, pretend we hit return at the end
      (multiple-value-bind (s e) (selection-range f)
        (when (and (eql 0 s) (eql e bufSize))
          (evaluate me)
          t)))))

;;; For proper colors we need lengthy, complex user studies, 
;;; government grants, field trips to exotic countries...
(defvar *script-syntax-error-color* Red)
(defvar *script-color* Black)
(defvar *script-warning-color* brown)
(defvar *script-undefined-handler-color* vomit)

(define-handler displayMessage (ScriptEditText messageString)
  (messageToUser messageString))

(define-handler indicateScriptSyntaxError (ScriptEditText theError &key (record t))
  (when (listp theError) (setf theError (car theError)))
  (let* ((buf (fred-buffer (editData me)))
         (errorLine (lineOfError theError))
         (offset (buffer-line-start buf 0 errorLine))
         (errorPos (characterPositionOfError theError))
         (start nil)
         (end nil)
         (description (descriptionOfError theError)))
    (when (listp errorPos)
      (setf start (car errorPos)
            end (cadr errorPos)))
    ;; If the start and end were not provided, select the whole line.
    ;; otherwise, add the offset. 
    (if start (setf start (+ start offset)) (setf start offset))
    (if end (setf end (+ end offset)) (setf end (buffer-line-end buf start)))
    ;; If start and end are the same, insert a space to be able to show the error color.
    (when (eql start end)
      (buffer-insert buf " " start)
      (incf end))
    (cond
     (record
      (buffer-set-font-spec buf `(:bold :underline (:color ,(mcl-color *script-syntax-error-color*))) 
                            (min start (buffer-size buf))
                            (min end (buffer-size buf)))
      (forceRedraw me :updateScrollers t))
     (t
      (dotimes (j 3)
        (setSelection me start start)
        (sleep .05)
        (setSelection me start end)
        (sleep .05))))
    ;; This needed to be queued to deal with a crash situation that happens
    ;; when modalDialog gets called from the innards of Fred. To reproduce, type
    ;; into the messagebox "get the objects in project sk8". 
    (when description (displayMessage me description))
    ))

;;; _______________________________ 
;;; Other functionality.
;;; _______________________________ 

;;; When return happens, we ask maybe-handle-complete-script if it should be taken as an eval.
;;; If not, we pass on the key so that the comtab may do its work.

(define-handler returnInField (ScriptEditText)
  (if (shift-key-p)
    (call-next-method)
    (unless (maybe-handle-complete-script me)
      (call-next-method))))

(define-handler enterInField (ScriptEditText)
  (maybe-handle-complete-script me))

(define-handler keyDown (ScriptEditText key)
  (require-type key Character)
  (case key
    ((#\escape #\Help) (identifierHelp me))
    (otherwise
     (call-next-method))))


;;; _______________________________ 
;;; Junk
;;; _______________________________ 

(define-handler (setf dirty) (newValue ScriptEditText)
  (declare (ignore newValue))
  )

(define-handler dirty (ScriptEditText)
  nil)


#|
	Change History (most recent last):
	1  	 1/ 9/96	Hernan  	New file. A much simpler version of ScriptEditText.
	2  	 1/15/96	Hernan  	ScriptEditText, by default, evaluates expressions. For that
						I need to add all the maybe-complete-script machinery 
						from the original ScriptEditText.
	3  	 1/17/96	Hernan  	Removing indicate-syntax-error. No need for it. Its close
						cousin indicataScriptSyntaxError should suffice.
	1  	 1/22/96	Hernan  	
	2  	 1/22/96	Hernan  	ScriptEditText has to define scriptInputCompleted.
	3  	 1/25/96	Hernan  	Made editText restore the normal color after a compile.
	4  	 1/25/96	Hernan  	Making option-return be the way to evaluate a script
						editText. Return just adds a CR.
	5  	 1/26/96	Hernan  	Ooops. Return will be the way to evaluate the script.
						To add a CR, the user has to type option-return.
	6  	 1/29/96	Hernan  	Need to define ed-delete-with-undo on our comtab. This is
						required to catch the case where selected text is deleted
						as a result of pressing a key. Also added lowleveldelete and
						lowlevelinsert.
	7  	 1/30/96	Hernan  	ed-insert-with-undo has to call-next-method passing the 
						new position of the buffer, since lowLevelInsert may have
						changed it.
	8  	 2/16/96	Brian   	changing reference to result variable.
	2  	 4/10/96	Hernan  	define-system-handler->define-handler.
	3  	 4/19/96	Hernan  	Putting define-sk8-function back in the code.
	4  	 5/ 2/96	Hernan  	Fixing identifierHelp to work with the new SK8Script.
	5  	 5/ 8/96	Hernan  	Fixed returnInField to actually insert a CR if the script is not
						complete.
	6  	 5/21/96	Brian   	
	7  	10/ 9/96	Hernan  	In popupballoon, need to do the loop without-interrupts.
	8  	10/10/96	Hernan  	ed-delete-with-undo may take a string as the end argument.
	9  	11/ 7/96	Brian Roddy	Converting WITH function to a macro.
	10 	11/12/96	sidney  	name formerly anonymous process to ease debugging
	11 	11/25/96	Brian   	Changing process name
	12 	12/17/96	Brian Roddy	
	13 	12/17/96	Hernan  	Fixing indicateScriptSyntaxError to avoid running over the end of the buffer.
	14 	12/19/96	sidney  	don't need to evaluate expressions in another process now that sk8 stuff doesn't run in MCL event process
	15 	 1/27/97	Brian Roddy	
	16 	 2/27/97	Hernan  	
|# ;(do not edit past this line!!)
