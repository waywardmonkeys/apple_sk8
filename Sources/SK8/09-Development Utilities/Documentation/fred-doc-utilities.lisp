;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories



(in-package :ccl)

(defvar *doc-window-font* '("Palatino" 12 :plain))

(defvar *default-doc-editor-class* 'fred-doc-window)




;;; We define a new class of Fred Window with all our settings in it.

(defclass fred-doc-window (fred-window)
  ((last-modcnt :initform 0)))


(defconstant *doc-window-header-line*
  "======================================================================")

;;; _______________________________ 
;;; The templates.
;;; _______________________________ 

(defvar *header-template* 
"======================================================================

======================================================================")

(defvar *object-template* 
"======================================================================
OBJECT: 
(Engineer: )
======================================================================

__DESCRIPTION:
__EXAMPLE:
__SEE ALSO:

")

(defvar *property-template* 
"======================================================================
PROPERTY: 
======================================================================

__DESCRIPTION:
__GETTER ARGUMENTS:
__GETTER DESCRIPTION:
__SETTER ARGUMENTS:
__SETTER DESCRIPTION:
__EXAMPLE:
__SEE ALSO:

")

(defvar *handler-template* 
"======================================================================
HANDLER: 
======================================================================

__ARGUMENTS:
__DESCRIPTION:
__EXAMPLE:
__SEE ALSO:

")

;;; _______________________________ 
;;; Comtab Changes.
;;; _______________________________ 

;;; M-h -> adds a new header.
;;; Tab -> inserts a tab character.

(defvar *fred-doc-window-comtab* (copy-comtab *comtab*))

;;; Adds the header add the current position of the tab.

(defun add-header (w)
  (let ((TheBuf (fred-buffer w)))
    (ed-insert-with-undo w *Header-Template*)
    (set-mark theBuf (- (buffer-position theBuf) 61))))

(defun insert-object-template (w)
  (let* ((theBuf (fred-buffer w))
         (startPos (buffer-position theBuf)))
    (ed-insert-with-undo w *object-template*)
    (set-mark theBuf (+ startPos 79))))

(defun insert-property-template (w)
  (let* ((theBuf (fred-buffer w))
         (startPos (buffer-position theBuf)))
    (ed-insert-with-undo w *property-template*)
    (set-mark theBuf (+ startPos 81))))

(defun insert-handler-template (w)
  (let* ((theBuf (fred-buffer w))
         (startPos (buffer-position theBuf)))
    (ed-insert-with-undo w *handler-template*)
    (set-mark theBuf (+ startPos 80))))

(defun ed-insert-bullet (w)
  (ed-insert-char w #\¥))

(defun ed-insert-copyright (w)
  (ed-insert-char w #\251))

;;; Called when the user types #\newLine.

(defun newline-plus-tabbing (w)
  (let* ((theBuffer (fred-buffer w))
         (thePos (buffer-position theBuffer))
         (startPos (buffer-line-start theBuffer thePos))
         (numTabs 0)
         theChar)
    ;; Count the tabs.
    (loop 
      (setf theChar (buffer-char theBuffer startPos))
      (when (or (= startPos thePos)
                (not (char= theChar #\Tab))
                (char= theChar #\Newline))
        (return))
      (incf startPos)
      (incf numTabs))
    (ed-insert-char w #\Newline)
    (dotimes (i numTabs)
      (ed-insert-char w #\Tab))))

(defun toggle-to-code (w)
  (let* ((theBuffer (fred-buffer w))
         theString theLen)
    (multiple-value-bind (start end) (selection-range w)
      (when (= start end) (return-from toggle-to-code))
      ;; We have a selection!
      (setf theString (buffer-substring theBuffer start end))
      ;; Get rid of trailing #\.
      (when (char= (aref theString (1- (length theString))) #\.)
        (setf theString (subseq theString 0 (1- (length theString))))
        (decf end))
      (setf theLen (length theString))
      (if (and (> theLen 5)
               (string= (subseq theString 0 2) "[[")
               (string= (subseq theString (- theLen 2) theLen) "]]"))
        ;; Strip out code markers.
        (ed-replace-with-undo w start end (subseq theString 2 (- theLen 2)))
        ;; Add code markers.
        (ed-replace-with-undo w start end
                              (concatenate 'string "[[" theString "]]"))
        ))))
              
(comtab-set-key *fred-doc-window-comtab* '(:meta #\o) 'insert-object-template)
(comtab-set-key *fred-doc-window-comtab* '(:meta #\p) 'insert-property-template)
(comtab-set-key *fred-doc-window-comtab* '(:meta #\h) 'insert-handler-template)
(comtab-set-key *fred-doc-window-comtab* '(:control :meta #\b) 'ed-insert-bullet)
(comtab-set-key *fred-doc-window-comtab* '(:control :meta #\c) 'ed-insert-copyright)
(comtab-set-key *fred-doc-window-comtab* '(:control #\c) 'toggle-to-code)

(comtab-set-key *fred-doc-window-comtab* #\Newline 'newline-plus-tabbing)

(comtab-set-key *fred-doc-window-comtab* #\tab #'ed-self-insert)

;;; _______________________________ 
;;; The doc window's behavior.
;;; _______________________________ 

(defmethod SOURCESERVER::suppress-sourceserver-comments-p ((w fred-doc-window))
  t)

(defun ideal-doc-window-width ()
  (+ 25 (string-width *doc-window-header-line* *doc-window-font*)))

(defun set-doc-window-properties (w)
  (let ((readOnly? (window-buffer-read-only-p (window-key-handler w)))
        (buf (fred-buffer w)))
    (when readOnly? (%buffer-set-read-only buf nil))
    ;; Set the wrapping and the tabcount.
    (setf (fred-word-wrap-p w) t)
    (setf (fred-tabcount w) 3)
    ;; And install the new comtab.
    (setf (slot-value (window-key-handler w) 'comtab) *fred-doc-window-comtab*)
    ;; Set the font.
    (when readOnly? (%buffer-set-read-only buf nil))
    (buffer-set-font-spec buf *doc-window-font*)
    (buffer-set-font-spec buf *doc-window-font* 0 t)
    (when readOnly? (%buffer-set-read-only buf t))
    (setf (slot-value (window-key-handler w) 'file-modcnt) (buffer-modcnt buf))
    ;; (setf (slot-value w 'last-modcnt) (buffer-modcnt buf))
    ;; Set the size.
    (set-view-size w (ideal-doc-window-width)
                   (max 250 (point-v (view-size w))))))

(defmethod initialize-instance ((w fred-doc-window) &rest initargs)
  (declare (ignore-if-unused initargs))
  (let ((shown? (getf initargs :window-show t)))
    (when shown? 
      (remf initargs :window-show))
    (apply #'call-next-method w initargs)
    (set-doc-window-properties w)
    (when shown? (window-show w))))

(defmethod window-grow-event-handler ((w fred-doc-window) where)
  (declare (ignore where))
  (let* ((r (window-grow-rect w))
         (oldLeft (rref r :rect.left)))
    (setf (rref r :rect.left) (ideal-doc-window-width))
    (when (unwind-protect (call-next-method)
            (setf (rref r :rect.left) oldLeft))
      ;; (ed-refresh-screen w)
      )))

(defmethod window-revert ((w fred-doc-window) &optional dont-prompt)
  (declare (ignore dont-prompt))
  (with-quieted-view w
    (call-next-method)
    (set-doc-window-properties w))
  (fred-update w))

(defun find-current-doc-record (w &optional pos)
  (let ((buf (fred-buffer w)))
    (unless pos (setq pos (buffer-position buf)))
    (let ((prevHeaderEnd (buffer-backward-search buf *doc-window-header-line* pos))
          prevHeaderStart)
      (when (and prevHeaderEnd
                 (eql prevHeaderEnd (buffer-line-start buf prevHeaderEnd))
                 (setq prevHeaderStart
                       (buffer-backward-search buf *doc-window-header-line* prevHeaderEnd))
                 (eql prevHeaderStart (buffer-line-start buf prevHeaderStart)))
        (let* ((headingStart (buffer-line-start buf prevHeaderStart 1))
               (infoLineStart (buffer-line-start buf headingStart 1)))
          (when (or (eql infoLineStart prevHeaderEnd)
                    (eql (buffer-line-start buf infoLineStart 1) prevHeaderEnd))
            (values headingStart infoLineStart prevHeaderEnd)))))))

(defun maybe-update-doc-mods-date (w)
  (multiple-value-bind (headingPos infoStart infoEnd) (find-current-doc-record w)
    (when headingPos
      (let ((buf (fred-buffer w))
            (currTimeString (string-left-trim
                             '(#\Space)
                             (cl-user::get-time-string :no-time t))))
        (flet ((startNewInfoLine ()
                 (ed-insert-with-undo w (format nil "(modified: ~a)~@[~%~]"
                                                currTimeString
                                                (eql infoEnd infoStart))
                                      infoStart t)
                 (fred-update w)))
          (cond
           ;; There's no info line -- start a new one
           ((eql infoEnd (buffer-skip-fwd-wsp&comments buf infoStart (1+ infoEnd)))
            (startNewInfoLine))
           ;; There is an info line...
           (t
            (let ((modTimePos (buffer-forward-search buf "modified:" infoStart infoEnd)))
              (cond
               ;; There's no modTime info -- add it
               ((null modTimePos)
                (unless (and (setq modTimePos (buffer-forward-search buf #\( infoStart infoEnd))
                             (setq modTimePos (buffer-fwd-sexp buf (1- modTimePos) infoEnd)))
                  (return-from maybe-update-doc-mods-date (startNewInfoLine)))
                (ed-insert-with-undo w (format nil ", modified: ~a" currTimeString)
                                     (1- modTimePos) t)
                (fred-update w))
               ;; There is modTime info -- update it if necessary
               (t
                (setq modTimePos (buffer-fwd-skip-wsp buf modTimePos))
                (let ((modTimeEnd (or (buffer-forward-find-char buf ",)" modTimePos infoEnd)
                                      infoEnd)))
                  (setq modTimeEnd (buffer-bwd-skip-wsp buf (1- modTimeEnd)))
                  (unless (and (eql (length currTimeString) (- modTimeEnd modTimePos))
                               (buffer-substring-p buf currTimeString modTimePos))
                    (ed-replace-with-undo w modTimePos modTimeEnd currTimeString t)
                    (fred-update w)))))))))))))


(defun fred-command-affected-string (w)
  (let ((info (CCL::ed-history-get w)))
    (when (consp (first info))
      (setq info (first (the list info))))
    (if (listp (cdr info))
      (car (cdr (the list info)))
      (cdr (the list info)))))


(defun full-doc-record-killed-p (w)
  (when (memq (fred-last-command w) '(:clear :kill))
    (not (null (search *doc-window-header-line*
                       (fred-command-affected-string w)
                       :test #'eq)))))


(defun doc-record-modified-p (w)
  (if (full-doc-record-killed-p w)
    nil
    (let* ((buf (fred-buffer w))
           (pos (buffer-position buf))
           (endOfRecord (buffer-forward-search buf *doc-window-header-line* pos)))
      (unless endOfRecord
        (return-from doc-record-modified-p t))
      (decf endOfRecord (length *doc-window-header-line*))
      (if (eql endOfRecord (buffer-skip-fwd-wsp&comments buf pos (1+ endOfRecord)))
        ;; Return NIL if only whitespace was inserted or removed
        (find-if-not #'(lambda (c) (%str-member c WSP&CR))
                     (fred-command-affected-string w))
        t))))


(defmethod window-modified ((w fred-doc-window))
  (unless (or (boundp '*in-window-modified*)
              (not (doc-record-modified-p w)))
    (let ((*in-window-modified* t))
      (declare (special *in-window-modified*))
      (maybe-update-doc-mods-date w))))


(defmethod fred-update ((w fred-doc-window))
  (unless (view-quieted-p w)
    (let* ((buf (fred-buffer w))
           (lastModcnt (slot-value w 'last-modcnt)))
      (call-next-method)
      (unless (or (eql lastModcnt 0)
                  (eql lastModcnt (buffer-modcnt buf)))
        (setf (slot-value w 'last-modcnt) (buffer-modcnt buf))
        (window-modified w)))))

                             
(defmacro making-doc-window (&body body)
  `(let ((*default-editor-class* 'fred-doc-window))
     ,@body))

;;; _______________________________ 
;;; Tying Source files to doc files (control-.)
;;; _______________________________ 

(defun ed-new-docfile (theFile)
  (let* ((thePathname (make-pathname :defaults theFile))
         (theObject (pathname-name thePathname))
         (theFileType (pathname-type thePathname))
         newWindow)
    (setf newWindow (ed))
    (unless (or (string-equal theObject "globals")
                (string-equal theObject "constants")
                (string-equal theObject "functions"))
      (insert-object-template newWindow)
      (buffer-insert (fred-buffer newWindow) theObject)
      (fred-update newWindow))
    (setf (slot-value newWindow 'my-file-name) thePathname)
    (set-window-title newWindow (concatenate 'string theObject "." theFileType))
    newWindow))
    
(defun get-arglist (handlerObj type)
  (let* ((name nil)
         (lfun (cond
                ((symbolp handlerObj)
                 (setq name handlerObj)
                 (symbol-function handlerObj))
                
                ((functionp handlerObj)
                 (setq name (function-name handlerObj))
                 (when (typep name 'method) (setq name (method-name name)))
                 handlerObj)
                (t ;must be a method
                 (setq name (method-name handlerObj))
                 (method-function handlerObj))))
         (result (arglist lfun t)))
    (case type
      ((:getter :handler) (cdr result))
      (:setter (cddr result))
      (:function result))))

;;; Does not include the me and to args!!!

(defun build-arg-string (theArgs)
  (let ((result "")
        (argKind :required))
    (dolist (oneArg theArgs result)
      (case oneArg
        (&optional (setf argKind :optional))
        (&key      (setf argKind :keyword))
        (otherwise
         (let* ((argName      (if (listp oneArg) (first  oneArg) oneArg))
                ;; Unfortunately, an argument default value is given to us in source code form, not as
                ;; the object compiled.  Accordingly, we try to eval the thing, but this might fail
                ;; since our context is different from the context where the function was defined.
                ;; Hence the ignore-errors.  This is a mess.  -DY
                (defaultValue (and (listp oneArg) (ignore-errors (eval (second oneArg)))))
                starter
                theDefault
                requiredOrOptional
                (theSymbol (find-symbol (symbol-name argName) :sk8dev)))
           (case argKind
             (:required
              (setf starter "¥")
              (setf theDefault "")
              (setf requiredOrOptional ", required:"))
             ((:optional
               :keyword)
              (setf starter (if (eq argKind :optional) "¥" "¥with "))
              (if defaultValue
                (setf theDefault (concatenate 'string " (defaulting to " (sk8::objectString defaultValue :project sk8::sk8) ")"))
                (setf theDefault ""))
              (setf requiredOrOptional ", optional:")))
           (setf result (concatenate 'string
                                     result
                                     starter
                                     (pretty-symbol-name theSymbol)
                                     theDefault
                                     requiredOrOptional
                                     (string #\Newline)))))))))

(defun add-handler-documentation (theWindow theObject handlerName)
  (let ((theHandler (mf::find-handler handlerName theObject))
        argString)
    (when theHandler
      (when (listp theHandler) (setf theHandler (car theHandler)))
      (setf argString (build-arg-string (get-arglist theHandler :handler)))
      ;; Add the me arg.
      (setf argString (concatenate 'string 
                                   (format nil "¥me (a ~a), required:~%" (sk8::objectString theObject))
                                   argString))
      ;; Now move to the end of the file, insert the template with the args in it.
      (let* ((theBuf (fred-buffer theWindow))
             (startpos (buffer-size theBuf)))
        ;; Move the mark to the end.
        (set-mark thebuf startpos)
        ;; And insert the text.
        (buffer-insert theBuf *handler-template*)
        (set-mark theBuf (+ startPos 80))
        (buffer-insert theBuf handlerName)
        (set-mark theBuf (+ (buffer-position theBuf) 86))
        (terpri theWindow)
        (buffer-insert theBuf argString)
        (terpri theWindow)
        (fred-update theWindow))))
  t ; succeded
  )

(defun add-property-documentation (theWindow theObject propName)
  (let ((theGetter (mf::find-handler propName theObject))
        (theSetter (mf::find-handler (concatenate 'string "set " propName) theObject))
        (getterArgs "")
        (setterArgs ""))
    (when (listp theGetter) (setf theGetter (car theGetter)))
    (when (listp theSetter) (setf theSetter (car theSetter)))
    (when theGetter
      ;; [1] Getter args!
      (setf getterArgs (build-arg-string (get-arglist theGetter :getter)))
      ;; Add the me arg.
      (setf getterArgs (concatenate 'string 
                                    (format nil "¥me (a ~a), required:~%" (sk8::objectString theObject))
                                    getterArgs)))

    (when theSetter
      ;; [2] Setter args!
      (setf setterArgs (build-arg-string (get-arglist theSetter :setter)))
      ;; Add the me arg and the to arg.
      (setf setterArgs (concatenate 'string 
                                    (format nil "¥me (a ~a), required:~%" (sk8::objectString theObject))
                                    setterArgs
                                    (format nil "¥to, required:~%"))))

      ;; [3] Now move to the end of the file, insert the template with the args in it.
    (let* ((theBuf (fred-buffer theWindow))
           (startpos (buffer-size theBuf)))
      ;; Move the mark to the end.
      (set-mark thebuf startpos)
      ;; And insert the text.
      (buffer-insert theBuf *property-template*)
      (set-mark theBuf (+ startPos 81))
      (buffer-insert theBuf propName)
      (set-mark theBuf (+ (buffer-position theBuf) 108))
      (when theGetter
        (terpri theWindow)
        (buffer-insert theBuf getterArgs)
        (terpri theWindow))
      (set-mark theBuf (+ (buffer-position theBuf) 42))
      (when theSetter
        (terpri theWindow)
        (buffer-insert theBuf setterArgs)
        (terpri theWindow))
      (fred-update theWindow)))
  t ; succeded
  )
        
;;; Adds an entry for the new thing. Assumes it is not already there.

(defun add-documentation (theWindow theFile thingsName type)
  (let ((theObject (read-from-string (concatenate 'string "sk8dev::" (pathname-name theFile)))))
    (setf theObject (symbol-value theObject))
    ;; If the type is handler, could it be that we are looking at a property?
    (when (and (or (eq type :handler) (eq type :setter))
               (member thingsName (sk8::properties theObject) 
                       :test #'string-equal :key #'symbol-name))
      (setf type :property))
    ;; No? If it is a setter add the "set " to the name.
    (let ((thingsActualName (if (eq type :setter)
                              (concatenate 'string "set " thingsName)
                              thingsName)))
      ;; Have the object. Now get the info.
      (when (case type
              ((:handler :setter) (add-handler-documentation theWindow theObject thingsActualName))
              (:property          (add-property-documentation theWindow theObject thingsActualName))
              (:constant) ; not implemented yet
              (:global)   ; not implemented yet
              (:function) ; not implemented yet
              )
        (find-doc-definition theFile thingsName type t)
        ))))

(defun find-doc-definition (theFile thingsName type secondTry)
  (let (theWindow)
    ;; [1] Open the right file given the actor.
    (making-doc-window
      (if (probe-file thefile)
        (setf theWindow (ed theFile))
        (setf theWindow (ed-new-docFile theFile))))
    ;; [3] Look for the handler specified.
      (let ((theBuffer (fred-buffer theWindow))
          pos)
      ;; We expect the entry to be followed immediately by a newline
      (setf pos (buffer-forward-search theBuffer 
                                           (concatenate 'string ": " thingsName (string #\newline))
                                           0
                                           (buffer-size theBuffer)))
      (if pos 
        (window-scroll theWindow (1- pos)) ;; 1- because the search includes the newline
        ;; Did not find it? Add it!
        (unless secondTry
          (add-documentation theWindow theFile thingsName type))))))

;;; The object is a sk8 object. thingName is a string.
;;; Type is :handler, :property, :function, :global or :constant.

(defun find-doc (theObject thingsName &optional type)
  (let ((theFile (concatenate 'string "ccl;Documentation:Text:" 
                              (if (stringp theObject)
                                theObject
                                (sk8::objectName theObject))
                                ".doc")))
    (find-doc-definition theFile thingsName type nil)))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  The main entry point for finding the documentation given:
;;;    - the type of thing, its name [, its object]
;;;    - a method
;;;    - a function
;;;    - an object
;;;    - or just a symbol or string
;;;

(defun edit-documentation (thingOrType &optional name obj)
  (when (and (null name) (null obj))
    ;; Try to figure it out from a single argument...
    (typecase thingOrType
      ;; A handler (or property)
      (METHOD
       (setq name (method-name thingOrType)
             obj (SK8::object thingOrType)
             thingOrType :handler)
       (cond ((listp name)
              (setq name (second name)
                    thingOrType :property))
             ((fboundp (setf-function-name name))
              (setq thingOrType :property))))
      ;; A generic function; we may have to let the user select from list of handlers (if there's more than one)
      (GENERIC-FUNCTION
        (return-from edit-documentation
          (guess-edit-documentation thingOrType)))
      ;; A function
      (FUNCTION
       (setq name (function-name thingOrType)
             thingOrType :function))
      ;; An object
      ((or SK8::OBJECT BUILT-IN-CLASS)
       (setq obj thingOrType
             thingOrType :object))
      ;; A string, symbol, or list; we have to take a guess at it, and maybe let the user select from the possibilities
      ((or STRING SYMBOL CONS)
       (return-from edit-documentation
         (guess-edit-documentation thingOrType)))
      
      (t
       (format t "If you can figure out why we got here, maybe you can fix the code?  Thanks.~&")
       (break)
       (ed-beep))))
  
  ;; We've got the info we need to specifically identify the doc record
  (let (fileName)
    (when (eq thingOrType :object)
      (setq obj (or obj name)))
    (when obj
      (cond
       ((symbolp obj) (setq obj (pretty-symbol-name obj)))
       ((not (stringp obj)) (setq obj (SK8::objectName obj)))))
    (when (eq thingOrType :object)
      (setq name obj))
    (when (symbolp name)
      (setq name (pretty-symbol-name name)))
    (case thingOrType
      (:function (setq fileName "functions"))
      (:constant (setq fileName "constants"))
      (:global   (setq fileName "globals"))
      (:object   (setq fileName obj))
      ((:handler :property :setter) (setq fileName obj))
      (t
       (format t "If you can figure out why we got here, maybe you can fix the code?  Thanks.~&")
       (break)
       (return-from edit-documentation (ed-beep))))
    
    (find-doc-definition
     (concatenate 'string "ccl;Documentation:Text:" fileName ".doc") 
     (if (symbolp name) (pretty-symbol-name name) name)
     thingOrType
     nil)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  GUESS-EDIT-DOCUMENTATION
;;;     A helper function for edit-documentation; given a symbol, string or generic-function, this decides what
;;;     documentation record is meant (possibly asking the user to select from multiple choices) and then brings
;;;     it up with edit-documentation.
;;;
;;;  EXISTING-SYMBOLS-NAMED and SYMBOLS-OF-INTEREST are support functions for guess-edit-documentation.
;;;


(defun existing-symbols-named (str)
  (let ((possibilities nil)
        sym)
    (dolist (pkg %all-packages% possibilities)
      (unless (eq pkg *keyword-package*)
        (when (setq sym (find-symbol str pkg))
          (pushnew sym possibilities :test #'eq))))))


(defun symbols-of-interest (thing)
  (let ((setter? nil)
        name)
    (when thing
      (when (stringp thing)
        (setq thing (intern-symbol thing *package*)))
      (cond
       ((consp thing)
        (unless (eq (first thing) 'SETF)
          (return-from symbols-of-interest nil))
        (setq thing (second thing)
              setter? t))
       ((symbolp thing)
        (when (eq (symbol-package thing) *setf-package*)
          (setq thing (read-from-string (symbol-name thing))
                setter? t)))
       (t
        (return-from symbols-of-interest nil)))
      
      (setq name (symbol-name thing))
      (let ((possibilities nil))
        (cond
         (setter?
          (dolist (sym (existing-symbols-named name))
            (when (and (setq sym (setf-function-name sym)) (fboundp sym))
              (pushnew sym possibilities :test #'eq))))
         (t
          (dolist (sym (existing-symbols-named name))
            (when (or (fboundp sym) (and (boundp sym) (proclaimed-special-p sym)))
              (pushnew sym possibilities :test #'eq)))))
        possibilities))))


(defun guess-edit-documentation (thing)
  (let ((things nil)
        syms)
    (when (edit-toolbox-reference thing)
      (return-from guess-edit-documentation))
    (flet ((reap-global (sym)
             (push (list (if (constantp sym)
                           (if (string-equal (SK8::objectName (symbol-value sym))
                                             (symbol-name sym))
                             :object
                             :constant)
                           :global)
                         sym)
                   things))
           (reap-function (sym)
             (let ((fcn (symbol-function sym)))
               (if (not (typep fcn 'standard-generic-function))
                 (push (list :function sym) things)
                 (let ((name (function-name fcn))
                       (type :handler)
                       obj)
                   (cond
                    ((listp name)
                     (setq type :property
                           name (second name)))
                    ((fboundp (setf-function-name name))
                     (setq type :property)))
                   (dolist (meth (generic-function-methods fcn))
                     (when (and (setq obj (SK8::object meth))
                                (SK8::objectName obj))
                       (push (list type name obj) things))))))))
      (declare (dynamic-extent #'reap-global #'reap-function))
      
      (cond
       ((typep thing 'generic-function)
        (reap-function (function-name thing)))
       
       ((setq syms (symbols-of-interest thing))
        ;; Simplify a bit
        (when (and (cdr syms)
                   (or (and (every #'fboundp syms) (not (some #'boundp syms)))
                       (and (every #'boundp syms) (not (some #'fboundp syms)))))
          (setq syms (list (first syms))))
        (dolist (sym syms)
          (when (boundp sym) (reap-global sym))
          (when (fboundp sym) (reap-function sym)))))

      (if (null things)
        (progn
          (format t "If you can figure out why we got here, maybe you can fix the code?  Thanks.~&")
          (break))
        (if (null (cdr things))
          (apply #'edit-documentation (first things))
          (labels ((describe-doc-item (item strm)
                     (let ((type (pop item)))
                       (write-pname (symbol-name type) :capitalize strm)
                       (when (or (eq type :handler) (eq type :property))
                         (write-string " of " strm)
                         (SK8::writeObject (second item) strm t))))
                   (doc-item-string (item)
                     (with-output-to-string (strm)
                       (describe-doc-item item strm))))
            (setf things (sort things #'(lambda (h1 h2)
                                          (string-lessp (doc-item-string h1)
                                                        (doc-item-string h2)))))
            (let (selection-window)
              (setq selection-window
                    (select-item-from-list things
                                           #| :initial-selections (if bestGuess (list bestGuess) '()) |#
                                           :table-print-function #'describe-doc-item
                                           :window-title (concatenate 'string
                                                                      "Documentation for "
                                                                      (pretty-symbol-name (first syms)))
                                           :modeless t
                                           :default-button-text "Find it"
                                           :action-function
                                           #'(lambda (selected-items)
                                               (when (option-key-p) (window-close selection-window))
                                               (apply #'edit-documentation (first selected-items))))))))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Support functions for picking information off a window...
;;;

(defun ed-current-doc-definition-form (w)
  (let* ((theBuffer (fred-buffer w))
         (startPos (buffer-backward-search theBuffer (format nil "~%(")))
         sexp type name obj)
    (when startPos
      (let ((*package* (window-package w)))
        (declare (special *package*))
        (setq sexp (ignore-errors (buffer-current-sexp theBuffer (1+ startPos)))))
      (case (first sexp)
        ((SK8::DEFINE-HANDLER SK8::DEFINE-SYSTEM-HANDLER)
         (setq type :handler
               name (second sexp))
         (let ((args (third sexp)))
           (when (keywordp args) (setq args (fourth sexp)))
           (cond
            ((listp name)
             (setq obj (second args)
                   type :setter))
            ((sk8dev::!withHandlerName? name)
             (setq obj (second args)))
            (t
             (setq obj (first args))))))
        (SK8::DEFINE-SK8-FUNCTION
          (setq type :function
                name (second sexp)))
        (SK8::DEFINE-SK8-CONSTANT
          (setq type :constant
                name (second sexp)))
        (SK8::DEFINE-SK8-VAR
          (setq type :global
                name (second sexp)))
        ;; An object or a property
        (SK8::NEW
         (setq name (ignore-errors (buffer-current-sexp theBuffer)))
         (when (or (symbolp name) (stringp name))
           (setq obj (getf sexp :objectName))
           (setq type (if (string-equal (string name) (string obj))
                        :object
                        :property))))
        ;; A property
        (SK8::ADDPROPERTY
         (setq type :property
               name (second (third sexp))
               obj (second sexp))))
      
      (when (and type name)
        (values type name obj)))))
    

(defun ed-current-doc-symbol (w)
  (multiple-value-bind (s e) (selection-range w)
    (collapse-selection w nil)
    (let* ((pos (min s e))
           (buf (fred-buffer w))
           (form (ignore-errors (ed-current-sexp w pos)))
           (setter? nil))
      (when form
        (cond
         ((listp form)
          (when (and (eq (first form) 'SETF)
                     (symbolp (setq form (second form))))
            (setf-function-name form)))
         ((symbolp form)
          (let* ((name (symbol-name form))
                 (trimmedName (string-trim "[]-." name)))
            (unless (eql (length name) (length trimmedName))
              (setq form (intern trimmedName (symbol-package form))
                    name trimmedName))
            (cond
             ((or (and (or (string= name "SET") (string= name "SETF"))
                       (setq setter? t))
                  (string= name "WITH"))
              (setq form (ignore-errors
                          (ed-current-sexp w (buffer-fwd-skip-wsp buf (buffer-fwd-sexp buf pos)))))
              (when (symbolp form)
                (if setter?
                  (setf-function-name form)
                  (intern-symbol (concatenate 'string "WITH " (symbol-name form)) (symbol-package form)))))
             (t
              form)))))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  The main entry point for getting documentation from a window...
;;;

(defun ed-edit-documentation (w)
  (multiple-value-bind (type name obj) (ed-current-doc-definition-form w)
    (if type
      ;; Found a surrounding "definition form", so go to the appropriate record
      (edit-documentation type name obj)
      ;; Failed to find a surrounding "definition form", so take a guess with the current symbol
      (let ((sym (ed-current-doc-symbol w)))
        (if sym
          (edit-documentation sym)
          (progn
            (format t "If you can figure out why we got here, maybe you can fix the code?  Thanks.~&")
            (break)))))))


(comtab-set-key *comtab* '(:control #\.) 'ed-edit-documentation)
(comtab-set-key *fred-doc-window-comtab* '(:control #\.) 'ed-edit-documentation)



;;; _______________________________ 
;;; Sorting the doc file.
;;; _______________________________ 

(defun parse-thing-name (theStr)
  (string-trim '(#\Newline #\Space)
               (subseq theStr (1+ (position #\: theStr)) (length theStr))))

;;; Runs through the file. Produces a list of all the blocks in it.

(defun list-blocks (w)
  (let ((theBuffer (fred-buffer w))
        (startPos 0)
        (curpos 0)
        headerLine thingsName
        result)
    (loop
      ;; Find start of block.
      (setf curpos
            (ccl::buffer-forward-search theBuffer ccl::*doc-window-header-line* curpos))
      (unless curpos (return))
      (setf startPos (- curpos 70))
      ;; get the header line.
      (setf headerLine (buffer-substring theBuffer 
                                         (1+ curpos)
                                         (buffer-line-end theBuffer (1+ curpos))))
      (setf thingsName (parse-thing-name headerLine))
      ;; Add my startpos to the first previous thing in the file.
      (when (Car result)
        (push startPos (car result)))
      ;; Add this item to the result.
      (push (list thingsName startPos) result)
      ;; Jump the end of the header. 
      (setf curpos
            (ccl::buffer-forward-search theBuffer ccl::*doc-window-header-line* curpos))
      (incf curpos))
    ;; Add nil to the last thing.
    (push nil (car result))
    ;; Return them in the right order.
    (reverse result)))

(defun sort-doc-file (w)
  (let* ((newWindow (make-instance 'ccl::fred-doc-window :window-show nil))
         (newBuffer (fred-buffer newWindow))
         (originalListing (list-blocks w))
         (theBuffer (fred-buffer w))
         start end objectBlock theString)
    ;; Sort the list by name. Leave the object at the head.
    (setf objectBlock (pop originalListing))
    (setf originalListing
          (sort originalListing #'(lambda (x y) 
                                    (string< (string-upcase x)
                                             (string-upcase y)))
                :key #'cadr))
    (push objectBlock originalListing)
    ;; Now copy the text to the dummy window.
    (dolist (c originalListing)
      (setf start (third c) end (first c))
      (setf theString (buffer-substring theBuffer 
                                        start 
                                        (or end (buffer-size theBuffer))))
      (buffer-insert newBuffer theString))
    ;; Now copy the whole thing to the original window.
    (buffer-delete theBuffer 0 (buffer-size theBuffer))
    (buffer-insert theBuffer 
                   (buffer-substring newBuffer 0 (buffer-size newBuffer))
                   0
                   *doc-window-font*)))

(comtab-set-key *fred-doc-window-comtab* '(:control :meta #\s) 'sort-doc-file)
      
;;; _______________________________ 
;;; Letting us load doc files from the open file dialog.
;;; _______________________________ 

(let ((theFun (slot-value (cadr (menu-items *file-menu*)) 'ccl::menu-item-action))
      newFun)
  (setf newFun 
        #'(lambda (&optional w)
            (if (option-key-p)
              (making-doc-window
                (funcall theFun w))
              (funcall theFun w))))
  (setf (slot-value (cadr (menu-items *file-menu*)) 'ccl::menu-item-action)
        newFun))

#|
	Change History (most recent last):
	1	8/1/94	Hernan	New file. Implements a new fred window with
				nice shortcuts for our documentation.
	2	8/1/94	Hernan	Adding the template functions.
	3	8/2/94	Hernan	Adding meta point for functions and everyone
				else.
	4	8/2/94	Hernan	Expanding ctrl-. to work with properties defined
				in properties blcok or with addProperty.
	5	8/2/94	Hernan	Doc files end with ".doc
	6	8/3/94	Hernan	Adding a keyboard shortcut to add the copyright
				character to the text.
	7	8/3/94	chip	fixed text-style setup when doc windows are opened
	8	8/3/94	chip	added window-revert method (to set the font properly when you check out a doc file); added auto modification-date-stamping
	9	8/3/94	chip	made the modification-date-stamping a little more picky in determining whether the record was modified
	10	8/5/94	Hernan	Adding a function to turn the selection into code.
				Also added a function to sort the whole by handler
				or property name.
	11	8/5/94	Hernan	Adding a function to find the doc file programatically.
	12	8/9/94	Hernan	Ctrl-. now adds all the info it can about a new
				handler or property.
	13	8/9/94	Hernan	Adding the arg description to the args section and
				not the description section!!!
	14	8/9/94	Hernan	Fixed build-arg-string to lookup the symbol since
				that way we have access to any saved case info.
	15	8/9/94	dy	handle optional args and take a (bad) stab at arg default values
	16	8/11/94	dy	search doc file for the exact name not a prefix
	17	8/11/94	dy	setter-only property, other enhancements
	18	8/22/94	chip	added *default-doc-editor-class* global
	19 	 8/25/94	chip    	ctrl-. improvements -- now it works anywhere (including doc windows)
	20 	 8/25/94	chip    	took out an extraneous beep
	21 	11/ 1/94	dy      	sort choices in guess-edit-documentation
	22 	11/ 1/94	dy      	fix ed-current-doc-definition-form to call buffer-current-sexp with *package* set to the package of the window
	23 	11/ 4/94	dy      	
	24 	11/ 7/94	dy      	add think ref support to guess-edit-documentation
	25 	 1/31/95	dy      	change edit-think-reference to edit-toolbox-reference
	26 	 3/ 2/95	dy      	prepend sk8dev:: in front of the string before trying to evaluate it as a symbol in add-documentation.  Also put format statements and (break) calls in place of the (ed-beep)s.
	27 	 3/ 2/95	dy      	don't use with-debugging-features
	2  	 6/23/95	Hernan  	Fixed initialize-instance and set-doc-window-properties
							to deal with window-key-targets...
	3  	 1/19/96	sidney  	removing/rewriting refrences to old sk8script compiler
	4  	 2/ 7/96	Hernan  	Fixed some bugs preventing opening of some files using the
						doc window style.
	2  	 2/27/97	Hernan  	
|# ;(do not edit past this line!!)
