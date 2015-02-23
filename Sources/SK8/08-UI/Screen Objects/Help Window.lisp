;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories


;;;

(in-package :UIDevelopment)

(SK8-declare-syms :UI :public ; Updated  8-04-94   2:24 pm
                  UI::DISPLAYCONSTANTDOCUMENTATION UI::DISPLAYFUNCTIONDOCUMENTATION UI::DISPLAYHANDLERDOCUMENTATION
                  UI::DISPLAYOBJECTDOCUMENTATION UI::DISPLAYPROPERTYDOCUMENTATION UI::DISPLAYVARIABLEDOCUMENTATION)


(new UISimpleWindow :objectname "HelpWindow" :project ui)

(setf (actor PBMenuHelp) helpwindow)


(define-handler clearReferences (HelpWindow &key ((:objects theobjects)))
  )

(setf (text HelpWindow) "SK8 Documentation")

(define-handler SetUpForProject (HelpWindow &key ((:project theproject)))
  )


(setf (sk8::menubar HelpWindow) nil)
(setf (resizer HelpWindow) t)
(setf (zoombox HelpWindow) t)


(define-handler resized (HelpWindow)
  (let (hSize vSize)
    (call-next-method)
    (sk8-multival-setf (hSize vSize) (size me))
    (setBoundsRect HelpWindowFindLabel 9 (+ *windowtop* 8) (- hsize 9) (+ *windowtop* 20))
    (setBoundsRect HelpWindowInputField 18 (+ *windowtop* 30) (- hsize 18) (+ *windowtop* 50))
    ;(setBoundsRect HelpWindowParentButton (- hsize 74) (+ *windowtop* 60) (- hsize 20) (+ *windowtop* 81))
    ;(setBoundsRect HelpWindowChildrenButton (- hsize 147) (+ *windowtop* 60) (- hsize 80) (+ *windowtop* 81))
    (setBoundsRect HelpWindowDisplayer *WindowLeft* (+ *windowtop* 65) (- hsize *WindowRight*) (- vsize *windowBottom*))
    ))
;;;(resized HelpWindow)
(defparameter *no-doc-message* "Sorry, no documentation is available for this item.")


(define-handler EnteringStage (HelpWindow)
  (setf (keytarget me) HelpWindowInputField)
  (setSelection (textfield HelpWindowInputField) 0 -1))

(new uiBigLabel :objectname "HelpWindowFindLabel" :project ui)
(setf (container HelpWindowFindLabel) HelpWindow)
(setf (text HelpWindowFindLabel) "Showing Documentation On:")
(sendtoback HelpWindowFindLabel)


(new standardqueryfield :container HelpWindow :objectName "HelpWindowInputField" :project ui)
(hide (handle HelpWindowInputField))
;;(unlocktext (textfield HelpWindowInputField))

#|
(define-handler sk8::pasteClipboardToSelection ((textfield HelpWindowInputField)) )
(define-handler sk8::cutSelectionToClipboard ((textfield HelpWindowInputField)) )
(define-handler sk8::clearSelection ((textfield HelpWindowInputField)) )
|#

(define-handler draggingMouseEnter ((textfield HelpWindowInputField) actorDragged)
  (when (or (eq actorDragged ObjectDataRect) 
            (eq actorDragged Propertydatarect)
            (eq actorDragged handlerdatarect)
            (eq actorDragged functiondatarect)
            (eq actorDragged globalDataRect)
            (eq actorDragged constantDataRect)
            )
    (setf (highlight (container me)) t)))

(define-handler createTextDisplayItem ((historymenu HelpWindowInputField) theval)
  (uidisplaystring theval)
  )

(define-handler keydown ((textfield HelpWindowInputField) thekey)
  (cond
   ((eq thekey #\tab)
    (setf (keytarget (sk8::window me)) (picker HelpWindowDisplayer)))
   ((or (eq thekey #\Enter) (eq thekey #\Return))
    (process-help-entry (text (textfield HelpWindowInputField)))
    )
   (t
    (call-next-method))
   )
  )

(defun try-removing-quotes (str)
  (if (string= (subseq str 0 1) "'") (setf str (subseq str 1)))
  (if (string= (subseq str (1- (length str)) (length str)) "'") (setf str (subseq str 0 (1- (length str)))))
  str
  )

(defun process-help-entry (thetext &optional theobj functionPrecedence?)
  (let ((*package* (find-package "SK8"))
        theWords
        temp
        obj
        outputForm
        ok)
    (mapWordChunks thetext nil nil #'(lambda (s e) (push (subseq thetext s e) thewords)))
    (setf thewords (nreverse theWords))
    (if (and thewords (string-equal (car thewords) "the"))
      (setf thewords (cdr thewords)))
    (if (and thewords (string-equal (car thewords) "handler") (not (= (length thewords) 1))) ;;; need to check for the "handler" object
      (setf thewords (cdr thewords)))
    (when thewords
      ;; If the words start with "set" remove set from there.
      (when (string-equal (car theWords) "set") (setf theWords (cdr theWords)))
      (cond
       ;; [1] Global or constant?
       ((and (or (string-equal (car thewords) "global") (string-equal (car thewords) "constant")) (cadr thewords))
        (setf temp (try-removing-quotes (cadr thewords)))
        (setf temp (read-from-string temp nil nil))
        (setf (outputobjects HelpWindowInputField) (list temp))
        (setf ok t)
        )
       ;; [2] one word... make best guess. A handler of current object? A function?
       ((and (= (length thewords) 1)         
             (setf temp (read-from-string (car thewords) nil nil))
             (functionp (fboundp temp))
             (or (not (boundp temp)) (and functionPrecedence?
                                          theobj
                                          (memq theobj (implementors temp)))) ;;; whether to take precedence of handler or object...
             (symbol-function temp))
        (setf outputForm (process-assumed-handlerOrProp theObj temp))
        ;; Was not a handler of the focused object?
        (unless outputForm
          (let ((implementors (implementors temp)))
            (if implementors
              (progn
                (setf implementors (remove-if #'(lambda (x) (neq (project x) sk8)) implementors))
                (cond 
                 ((not implementors)
                  (setf outputform nil))
                 ((= 1 (length implementors))
                  (setf outputForm (process-assumed-handlerOrProp (car implementors) temp)))
                 (t
                  (setf outputForm (process-assumed-handlerOrProp 
                                    ;; system arg has to be t while our modal dialog code is messed up. Hernan.
                                    (selectFromCollectionDialog implementors :title "Select an Implementor" :system t) ;; (format nil "View the ~a of which object?" temp))
                                    temp)))))
              (setf outputForm (list (symbol-function temp))))))
        (if outputForm
          (setf (inputobjects HelpWindowInputField) outputForm)
          (ed-beep))
        (setf ok outputForm))
       ;; [3] Function.
       ((or (and (string-equal (car thewords) "function") (cadr thewords))
            (and (> (length (car thewords)) 2)
                 (string= "()" (subseq (car thewords) (- (length (car thewords)) 2) (length (car thewords)))))
            (and (cadr thewords)
                 (string-equal (cadr thewords) "(function") )
            )
        (if (string-equal (car thewords) "function")
          (setf temp (cadr thewords))
          (setf temp (car thewords)))
        (if (string= "()" (subseq temp (- (length temp) 2) (length temp)))
          (setf temp (subseq temp 0 (- (length temp) 2))))
        (if (string= (subseq temp 0 1) "[") (setf temp (subseq temp 1)))
        (setf temp (read-from-string temp nil nil))
        (setf temp (and (fboundp temp) (symbol-function temp)))
        (when temp
          (setf (outputobjects HelpWindowInputField) (list temp))
          (setf ok t))
        )
       ;; [4] Handler or property.
       ((and (string-equal (cadr thewords) "of") (= (length thewords) 3))
        (setf temp (car thewords))
        (if (string= (subseq temp 0 1) "[") (setf temp (subseq temp 1)))
        (setf temp (read-from-string temp nil nil))
        
        (setf obj (third thewords))
        (if (string= (subseq obj (1- (length obj)) (length obj)) "]") (setf obj (subseq obj 0 (1- (length obj)))))
        (setf obj (read-from-string obj nil nil))
        
        (setf obj (and (boundp obj) (symbol-value obj)))
        (when obj
          (setf ok t)
          (cond 
           ((sk8dev::tis-a-property? obj temp) ;; (memq temp (properties obj))
            (setf (outputobjects HelpWindowInputField) (list (list temp obj))))
           ((memq obj (implementors temp))
            (setf (outputobjects HelpWindowInputField) (list  (mf::find-handler temp obj))))
           (t 
            (setf ok nil)))))
       ;; [5] Give up!
       ((and (= (length thewords) 1)         
             (setf temp (read-from-string (car thewords) nil nil))
             (boundp temp)
             )
        (setf ok t)
        (setf (outputobjects HelpWindowInputField) (list (symbol-value temp)))
        )
       )
      (unless ok
        (messagetouser "Sorry, there is nothing in the system corresponding to that entry." :beep t)))))

(define-handler menuselect ((historymenu HelpWindowInputField))
  (let ((item (selecteditem me)))
    (setf (items me) (cons item (delete item (items me))))
    (setf (inputobjects (container me)) (if (or (not (listp item)) (= (length item) 2))
                                          (list item)
                                          item))
    (setf (selecteditem me) nil)))

(setf (items (historymenu HelpWindowInputField)) (list actor (list 'boundsrect actor) (list 'fillcolor actor) (list 'text actor)))
(setf (MaximumLength (historymenu HelpWindowInputField)) 11)

(define-handler dropped ((textfield HelpWindowInputField) droppee)
  (draggingMouseLeave me droppee)
  (when (or (eq droppee ObjectDataRect) 
            (eq droppee Propertydatarect)
            (eq droppee handlerdatarect)
            (eq droppee functiondatarect)
            (eq droppee globalDataRect)
            (eq droppee constantDataRect)
            )
    (bringToFront (sk8::window me))
    (setf (keytarget (sk8::window me)) (sk8::window me))
    (cond
     ((eq droppee ObjectDataRect)
      (setf (inputobjects (container me)) (if (listp (object objectdatarect)) 
                                            (object objectdatarect)
                                            (list (object objectdatarect)))))
     ((eq droppee Propertydatarect)
      (setf (inputobjects (container me)) (list (list (propertyname Propertydatarect) (car (objects Propertydatarect))))))
     ((eq droppee handlerdatarect)
      (setf (inputobjects (container me)) (list (Handler handlerdatarect))))
     ((eq droppee functiondatarect)
      (setf (inputobjects (container me)) (functions functiondatarect)))
     ((eq droppee globalDataRect)
      (setf (inputobjects (container me)) (globals globalDataRect)))
     ((eq droppee constantDataRect)
      (setf (inputobjects (container me)) (Constants constantDataRect)))
     )
    ))


#|
(defun functionDocumentation (func)
  (list "FUNCTION" "Jism"))
(defun constantDocumentation (const)
  (list "Constant" "Jism"))
(defun variableDocumentation (vari)
  (list "Variable" "Jism"))
|#

(define-handler (setf InputObjects) (curInput HelpWindowInputField)
  (sk8dev::withLockedCursor animatedClock
    (if curinput
      (progn
        (tickeventclock)
        (setf (outputobjects me) (if (listp curInput) 
                                   curInput
                                   (list curInput)))
        )
      (progn
        (setf (text (textfield me)) "")
        (setf (outputobjects me) nil))
      )))

(define-handler (setf outputobjects) (theval HelpWindowInputField)
  (unless (and (null theval) (null (inputobject (picker HelpWindowDisplayer)))) 
    (sk8dev::withLockedCursor animatedClock
      (withActorLocked (me)
        (setf theval (car theval))
        (setf (inputobject (picker HelpWindowDisplayer)) theval)
        (when theval
          (setf (text (textfield me)) (createTextDisplayItem (historymenu me) theval)))
        (setf theval (outputobject (picker helpwindowdisplayer)))
        (call-next-method (list theval) me)
        (unless (string= (text (picker HelpWindowDisplayer)) *no-doc-message*)
          (when (car (outputobjects me))
            (when theval (addItem (historymenu me) theval))
            (setf (text (textfield me)) (createTextDisplayItem (historymenu me) (car (outputobjects me))))
            )
          (when (and theVal (neq (keyTarget (sk8::window me)) (picker HelpWindowDisplayer)))
            (setf (keytarget (sk8::window me)) (picker HelpWindowDisplayer)))))
      )))

(new uitextlistforcorners :objectname "HelpWindowDisplayer" :project ui)
(setf (container HelpWindowDisplayer) HelpWindow)
(setf (pickerprototype HelpWindowDisplayer) EditText)
;;(unlocktext (picker HelpWindowDisplayer))
(setf (framesize (picker HelpWindowDisplayer)) '(0 0))
;(setf (textsize (picker HelpWindowDisplayer)) 12)
;(setf (textfont (picker HelpWindowDisplayer)) "Geneva")
(setf (title HelpWindowDisplayer) "Documentation:")
(setf (tablength (picker HelpWindowDisplayer)) 2)

(define-handler keydown ((picker HelpWindowDisplayer) thekey)
  (if (eq thekey #\tab)
    (setf (keytarget (sk8::window me)) (textfield HelpWindowInputField)))
  (when (memq thekey '(#\uparrow #\downarrow #\backarrow #\forwardarrow 
                       #\pageup #\pagedown #\home #\end))
    (call-next-method))
  )

(define-handler sk8::pasteClipboardToSelection ((picker HelpWindowDisplayer)) )
(define-handler sk8::cutSelectionToClipboard ((picker HelpWindowDisplayer)) )
(define-handler sk8::clearSelection ((picker HelpWindowDisplayer)) )
(addproperty (picker HelpWindowDisplayer) 'inputObject)
(addproperty (picker HelpWindowDisplayer) 'outputObject)
(define-handler (setf inputobject) (theval (picker HelpWindowDisplayer))
  (setvalue 'inputobject me theval)
  ;;;;DO THE SEARCH
  (withactorlocked (me)
    (cond
     ((null theval)
      (setf (text me) "")
      (setf (outputobject me) nil)
      )
     ((listp theval) 
      (sk8dev::withLockedCursor animatedClock
        (displayPropertyDocumentation me (cadr theval) (car theval)))
      )
     ((is-a theval handler) 
      (sk8dev::withLockedCursor animatedClock
        (displayHandlerDocumentation me theval)))
     ((is-a theval sk8::function) 
      (sk8dev::withLockedCursor animatedClock
        (displayFunctionDocumentation me theval)))
     ((symbolp theval) 
      (sk8dev::withLockedCursor animatedClock
        (if (constantp theval)
          (displayConstantDocumentation me theval)
          (displayVariableDocumentation me theval))))
     (t
      (sk8dev::withLockedCursor animatedClock
        (displayobjectDocumentation me theval)))
     )
    ))


(defun Make-Doc-String (str Header)
  (if (and str  (not (string= str "")))
    (concatenate 'string 
                 "©©" Header "¨¨" (string #\newline)
                 str (string #\newline)
                 )
    ""))

(defun Make-Arg-String (args Header)
  (let ((newstr ""))
    (when (and args (or (not (stringp args)) (not (string= args ""))))
      (setf newstr (concatenate 'string 
                                "©©" Header "¨¨" (string #\newline)
                                ))
      (dolist (i args)
        (setf newstr (concatenate 'string 
                                  newstr
                                  "¥ "
                                  (if (stringp (car i)) (car i) (objectstring (car i)))
                                  (if (cadr i)
                                    (concatenate 'string " (" (if (stringp (cadr i)) (cadr i) (objectstring (cadr i))) ")")
                                    "")
                                  (if (caddr i)
                                    (concatenate 'string ", " (if (stringp (caddr i)) (caddr i) (if (caddr i) "Required" "Optional")))
                                    "")
                                  (string #\&)
                                  ;; (string #\newline)
                                  (if (and (cadddr i) (stringp (cadddr i)) (not (string= (cadddr i) "")))
                                    (concatenate 'string "     " (cadddr i) (string #\&)) ;; (string #\newline))
                                    "")
                                  )))
      (setf newstr (concatenate 'string 
                                newstr  ;; (string #\newline) 
                                (string #\&)
                                ))
      newstr
      )))

(defmacro doc-string-test (str curcount char)
  `(and (char= (aref ,str ,curcount) ,char) (char= (aref ,str (1+ ,curcount)) ,char)))

(defun Break-Into-Words (str)
  (let (theWords)
    (mapWordChunks str nil nil #'(lambda (s e) (push (subseq str s e) theWords)))
    (nreverse theWords)))

(defun paragraphize-except-code (theField)
  (let ((endPos (size theField :how 'characters))
        (curpos 0))
    (loop
      (setf curPos (sk8::Search theField (string #\newline) :start curpos :beepifnotfound nil))
      (when (or (null curpos)
                (>= curpos (- endpos 2)))
        (return))
      (when curpos
        (when (and (not (char= (character (text theField :start (1+ curpos) :end (+ curpos 2))) #\Newline))
                   (not (char= (character (text theField :start (1- curpos) :end curpos)) #\Newline))
                   (not (eq (textFont theField :start (1- curpos) :end curpos) CourierFont)))
          ;; Add a CR and increment the curpos.
          (setf (text theField :start curpos :end curpos) (String #\Newline))
          (incf curpos))
        (incf curpos)))
    ;; Now replace every #\& by a #\Newline.
    (setf curpos 0)
    (loop
      (Setf curpos (sk8::Search thefield (string #\&) :start curpos :beepifnotfound nil))
      (if curpos
        (setf (text theField :start curpos :end (1+ curpos)) (string #\newline))
        (return))
      (incf curpos))
    ))
    
;;; Returns true if the word to be marked as hypertext is really such a thing...

(defun really-a-link? (str startpos endpos)
  (let ((theWord (subseq str startpos endpos))
        theSymbol)
    (unless (or (char= (aref theWord 0) #\')
                (string-equal theWord "False")
                (string-equal theWord "True"))
      ;; should not represent a symbol.
      ;; Now we return t if it is bound or fbound.
      (setf thesymbol (let ((*package* (package sk8::sk8))) (read-from-string theWord nil nil)))  ;;what a kludge, uh, system
      (and (not (string-equal theword (text (textfield HelpWindowInputField))))
           theSymbol
           (symbolp theSymbol)
           (or (boundp theSymbol)
               (fboundp theSymbol))))))

;;; This version is about 50% faster than the original in average.
;;; It conses 5 times less than the original.

(define-handler parseText ((picker HelpWindowDisplayer) str)
  (let* ((len (length str))
         (curCount 0)
         (offset 0)
         lastCourierStart courierPairs lastBoldStart boldPairs words)
    ;; Pass 1: remembering where things are.
    (loop
      (if (>= curcount (- len 2)) (return))
      (cond
       ((doc-string-test str curCount #\[) 
        (incf curCount 2) (incf offset 2)
        (setf lastCourierStart (- curcount offset)))
       ((doc-string-test str curCount #\])
        (if lastCourierStart
          (push (list lastCourierStart (- curCount offset)) courierPairs)
          (print "Warning: Encountered a ]] without a corresponding [["))
        (setf lastCourierStart nil)
        (incf curCount 2) (incf offset 2))
       ((doc-string-test str curCount #\©) 
        (incf curCount 2) (incf offset 2)
        (setf lastBoldStart (- curcount offset)))
       ((doc-string-test str curCount #\¨)
        (if lastBoldStart
          (push (list lastBoldStart (- (1- curcount) offset)) boldpairs)
          (print "Warning: Encountered a ]] without a corresponding [["))
        (setf lastCourierStart nil)
        (incf curCount 2) (incf offset 2))
       ;; Deal with notes.
       ((or (char= (aref str curCount) #\[) (char= (aref str curCount) #\]))
        (incf offset)
        (incf curCount))
       (t
        (incf curcount))))
    ;; Pass 2: rebuilding the string.
    (setf str (delete-if #'(lambda (theChar) (memq theChar '(#\] #\[ #\© #\¨))) str))
    ;; Set the text and the styles.
    (setf (text me) str)
    ;; Reset the text attributes of the field.
    (setf (textStyle me) '(plain)
          (textColor me) Black
          (textSize me) 9
          (textFont me) GenevaFont)
    ;; Process the hyperlinks, code voices and titles.
    (dolist (i courierpairs)
      (setf (textfont me :start (car i) :end (cadr i)) CourierFont)
      (setf (textsize me :start (car i) :end (cadr i)) 10)
      (setf words (Break-Into-Words (subseq str (car i) (cadr i))))
      (when (= (length words) 1)
        (when (really-a-link? str (car i) (cadr i)) 
          (setf (textColor me :start (car i) :end (cadr i)) Blue)
          (setf (textstyle me :start (car i) :end (cadr i)) '(underline)))))
    (dolist (i boldpairs)
      (setf (textstyle me :start (car i) :end (cadr i)) '(bold)))
    ;; Now make sure all CR's are double unless the textFont at the CR-1 location is the
    ;; code font...
    (paragraphize-except-code me)
    ))

#|

(define-handler parseText ((picker HelpWindowDisplayer) str)
  (let* ((len (length str))
         (curCount 0)
         lastCourierStart
         courierPairs
         lastBoldStart
         boldPairs
         words)
    (loop
      (if (>= curcount (- len 2)) (return))
      (cond
       ((doc-string-test str curCount #\[) 
        (setf str (delete #\[ str :start curcount :end (+ curcount 2)))
        (setf str (delete #\[ str :start curcount :end (+ curcount 2)))
        (decf len 2)
        (setf lastCourierStart curcount)
        )
       ((doc-string-test str curCount #\])
        (setf str (delete #\] str :start curcount :end (+ curcount 2)))
        (setf str (delete #\] str :start curcount :end (+ curcount 2)))
        (decf len 2)
        (if lastCourierStart
          (push (list lastCourierStart curcount) courierPairs)
          (print "Warning: Encountered a ]] without a corresponding [["))
        (setf lastCourierStart nil)
        )
       ((doc-string-test str curCount #\©) 
        (setf str (delete #\© str :start curcount :end (+ curcount 2)))
        (setf str (delete #\© str :start curcount :end (+ curcount 2)))
        (decf len 2)
        (setf lastBoldStart curcount)
        )
       ((doc-string-test str curCount #\¨)
        (setf str (delete #\¨ str :start curcount :end (+ curcount 2)))
        (setf str (delete #\¨ str :start curcount :end (+ curcount 2)))
        (decf len 2)
        (if lastBoldStart
          (push (list lastBoldStart curcount) boldpairs)
          (print "Warning: Encountered a ]] without a corresponding [["))
        (setf lastCourierStart nil)
        )
       (t
        (incf curcount))))
    (setf (text me) str)
    (dolist (i courierpairs)
      (setf (textfont me :start (car i) :end (cadr i)) CourierFont)
      (setf words (Break-Into-Words (subseq str (car i) (cadr i))))
      (when (= (length words) 1)
        (setf (textColor me :start (car i) :end (cadr i)) Blue)
        (setf (textstyle me :start (car i) :end (cadr i)) '(underline)))
      (setf (textsize me :start (car i) :end (cadr i)) 10))
    (dolist (i boldpairs)
      (setf (textstyle me :start (car i) :end (cadr i)) '(bold)))))

|#

(define-handler mousewithin ((picker HelpWindowDisplayer))
  (let* ((edFred (editData me))
         (mousePos (sk8dev::focusing-fred-on-its-window (view-mouse-position edFred)))
         (charPos (min (fred-point-position edFred mousePos)
                       (buffer-size (fred-buffer edFred)))))
    (if (equal (textColor me :start charpos :end charpos) Blue)
      (unless (eq (cursor stage) CursorHelp) (setf (cursor stage) CursorHelp))
      (progn
        (call-next-method)
        (unless (eq (cursor stage) IbeamCursor) (setf (cursor stage) IbeamCursor))))))

(setf (wantsmousewithin (picker HelpWindowDisplayer)) t)

(defun process-assumed-handlerOrProp (theObj theSym)
  (let ((thehand nil))
    (cond ((sk8dev::tis-a-property? theObj theSym) (list (list thesym theobj)))
          ((setf thehand (car (remove-if-not #'(lambda (x) (eq (name x) thesym)) (handlers theobj))))
           (list thehand)))))

(define-handler mousedown ((picker HelpWindowDisplayer))
  (let* ((edFred (editData me))
         (buf (fred-buffer edFred))
         (mousePos (sk8dev::focusing-fred-on-its-window
                     (view-mouse-position edFred)))
         (charPos (fred-point-position edFred mousePos))
         (outs (car (outputobjects HelpWindowInputField)))
         (theobj (if (listp outs) (second outs) outs))
         )
    (setf theobj (if (is-a theobj handler) (object theobj) theobj))
    (if (equalp (textColor me :start charpos :end charpos) Blue)
      (multiple-value-bind (start end) (buffer-word-bounds buf charPos)
        (process-help-entry (buffer-substring buf start end) theobj t))
      (call-next-method))))

(define-handler displayPropertyDocumentation ((picker HelpWindowDisplayer) obj prop)
  (let ((newstr ""))
    (sk8-multival-bind (newobj description getterdesc getterargs setterdesc setterargs example seealso) 
                       (PropertyDocumentation obj prop)
      (setf newstr (concatenate 'string newstr (make-doc-string description "Description of Property:")))
      (setf newstr (concatenate 'string newstr (make-doc-string getterdesc "Getter Description:")))
      (setf newstr (concatenate 'string newstr (Make-Arg-String getterargs "Getter Arguments:")))
      (setf newstr (concatenate 'string newstr (make-doc-string setterdesc "Setter Description:")))
      (setf newstr (concatenate 'string newstr (Make-Arg-String setterargs "Setter Arguments:")))
      (setf newstr (concatenate 'string newstr (make-doc-string example "Example:")))
      (setf newstr (concatenate 'string newstr (make-doc-string seealso "See Also:")))
      (when (string= newstr "") 
        (setf newstr *no-doc-message*))
      ;; (setf (text me) newstr)
      (parseText me newStr)
      (setf (outputobject me) (list prop newobj))
      )))

(define-handler displayobjectDocumentation ((picker HelpWindowDisplayer) obj)
  (let ((newstr ""))
    (sk8-multival-bind (newobj description example seealso) (objectDocumentation obj)
      (setf newstr (concatenate 'string newstr (make-doc-string description "Description of Object:")))
      (setf newstr (concatenate 'string newstr (make-doc-string example "Example:")))
      (setf newstr (concatenate 'string newstr (make-doc-string seealso "See Also:")))
      (when (string= newstr "") (setf newstr *no-doc-message*))
      ;; (setf (text me) newstr)
      (parseText me newStr)
      (setf (outputobject me) newobj))))

(define-handler displayHandlerDocumentation ((picker HelpWindowDisplayer) hand)
  (let ((newstr "")
        (handlername (name hand))
        )
    (setf handlername (if (listp handlername) (cadr handlername) handlername))
    (if (sk8dev::tis-a-property? (object hand) handlerName) ;; (memq handlername (properties (object hand)))
      (displaypropertydocumentation me (object hand) handlername)
      (sk8-multival-bind (newhand description args example seealso) 
                         (HandlerDocumentation hand)
        (setf newstr (concatenate 'string newstr (make-doc-string description "Description of Handler:")))
        (setf newstr (concatenate 'string newstr (Make-Arg-String args "Arguments:")))
        (setf newstr (concatenate 'string newstr (make-doc-string example "Example:")))
        (setf newstr (concatenate 'string newstr (make-doc-string seealso "See Also:")))
        (when (string= newstr "") (setf newstr *no-doc-message*))
        ;; (setf (text me) newstr)
        (parseText me newStr)
        (setf (outputobject me) newhand)
        ))))

(define-handler displayFunctionDocumentation ((picker HelpWindowDisplayer) func)
  (let ((newstr ""))
    (sk8-multival-bind (description args example seealso) 
                       (FunctionDocumentation func)
      (setf newstr (concatenate 'string newstr (make-doc-string description "Description of Function:")))
      (setf newstr (concatenate 'string newstr (make-Arg-string args "Arguments:")))
      (setf newstr (concatenate 'string newstr (make-doc-string example "Example:")))
      (setf newstr (concatenate 'string newstr (make-doc-string seealso "See Also:")))
      (when (string= newstr "") (setf newstr *no-doc-message*))
      ;; (setf (text me) newstr)
      (parseText me newStr)
      (setf (outputobject me) func)
      )))

(define-handler displayConstantDocumentation ((picker HelpWindowDisplayer) const)
  (let ((newstr ""))
    (sk8-multival-bind (description example seealso) 
                       (ConstantDocumentation const)
      (setf newstr (concatenate 'string newstr (make-doc-string description "Description of Constant:")))
      (setf newstr (concatenate 'string newstr (make-doc-string example "Example:")))
      (setf newstr (concatenate 'string newstr (make-doc-string seealso "See Also:")))
      (when (string= newstr "") (setf newstr *no-doc-message*))
      ;; (setf (text me) newstr)
      (parseText me newStr)
      (setf (outputobject me) const)
      )))

(define-handler displayVariableDocumentation ((picker HelpWindowDisplayer) vari)
  (let ((newstr ""))
    (sk8-multival-bind (description example seealso) 
                       (VariableDocumentation vari)
      (setf newstr (concatenate 'string newstr (make-doc-string description "Description of Variable:")))
      (setf newstr (concatenate 'string newstr (make-doc-string example "Example:")))
      (setf newstr (concatenate 'string newstr (make-doc-string seealso "See Also:")))
      (when (string= newstr "") (setf newstr *no-doc-message*))
      ;; (setf (text me) newstr)
      (parseText me newStr)
      (setf (outputobject me) vari)
      )))


#|
(new UIButton :objectname "HelpWindowParentButton" :project ui)
(setf (container HelpWindowParentButton) HelpWindow)
(setf (text HelpWindowParentButton) "Parents")

(define-handler click (HelpWindowParentButton)
  (let ((theval (car (outputobjects HelpWindowInputField))))
    (cond
     ((null theval))
     ((listp theval) 
      (setf (inputobjects HelpWindowInputField) 
            (list (car (propertydocumentation (cadr theval) (car theval) :inherited t))))
      )
     ((is-a theval handler) )
     ((is-a theval sk8::function) )
     ((symbolp theval) )
     (t
      (setf (inputobjects HelpWindowInputField) 
            (list (car (objectdocumentation theval :inherited t)))))
     )
    ))

(new UIButton :objectname "HelpWindowChildrenButton" :project ui)
(setf (container HelpWindowChildrenButton) HelpWindow)
(setf (text HelpWindowChildrenButton) "Children")
(setf (enabled HelpWindowChildrenButton) nil)
(define-handler click (HelpWindowChildrenButton)
  )
|#

(setSize HelpWindow 350 280)
(resized HelpWindow)
(setf (minimumsize HelpWindow) '(210 235))
(setf (top HelpWindow :resizing nil) 130)
(setf (left HelpWindow :resizing nil) 230)


#|
	Change History (most recent last):
	1	8/3/94	rod	A New File for Beta!  Online help.
	12	8/8/94	rod	Should all work now (yeah RIGHT!)
	15	8/8/94	rod	
	16	8/8/94	rod	
	17	8/9/94	rod	
	18 	 8/23/94	rod     	
	19 	 8/25/94	rod     	
	20 	 8/25/94	rod     	
	21 	 8/25/94	rod     	
	22 	 8/26/94	rod     	
	23 	 8/26/94	rod     	
	24 	 8/30/94	rod     	Making resized call-next-method
	25 	 9/ 6/94	rod     	
	26 	11/ 8/94	rod     	Fixing pasting into the textfield.
	27 	11/10/94	rod     	Updated mousedown and mousewithin for use
							with the new fred.
	28 	11/11/94	rod     	
	29 	11/16/94	rod     	minor tune ups.
	30 	12/13/94	Hernan  	Making hypertext use textcolor.
	31 	12/16/94	rod     	Adding underlines.
	32 	12/21/94	Hernan  	Passing the text to parseText to avoid setting the text twice. Also added extra CRs to separate
							paragraphs and restricted the things that can be
							considerer hypertext.
	33 	12/21/94	Hernan  	Final touches...
	34 	12/21/94	Hernan  	Fixed lookup from hypertext to bring up a list of
							the implementors of a handler or property when
							there is more than 1.
	35 	12/22/94	rod     	Fixing the mousedown and the text entry to
							use the same functions.  Added nicer error 
							messages.   Added option dialog.  Made guesses
							for mousedown smarter.
	36 	12/22/94	rod     	using a generic function for createtextdisplayitem
	37 	 1/10/95	Hernan  	Really-a-link does not die when the symbol is 
							really a number.
	38 	 1/23/95	rod     	
	39 	 1/24/95	rod     	
	40 	 1/25/95	Hernan  	1214681: the select dialog that comes up gets its
							title cropped when the name of the handler in
							question is slightly long. Replacing the nice mssg.
							with a silly one that does not include the handler
							name.
	41 	 1/25/95	Hernan  	When a documentation entry is displayed, the 
							help window focuses on the panel that shows the
							text of the docs.
	42 	 2/ 1/95	rod     	Making it so it doesn't make a link out of the 
							object being viewed.
	43 	 2/ 1/95	rod     	Changing default location of documentation
	44 	 2/ 3/95	Hernan  	WithLockedCursor is now correct. I check this file
							out and back in to force it to be recompiled.
	45 	 2/13/95	Hernan  	Fixed process-help-entry to treat things that have
							a setter and a getter as properties. This is has to
							happen in order to show some documentation that
							is written as properties who do not show up in the
							localVirtualProperties handler of the object in
							question (obvious example: partnerScroller which
							cannot be in the localVirtualProperties because 
							the getter requires an extra argument which 
							really messes up the object editor).
	46 	 2/14/95	rod     	oops, making sure that setting the location occurs
							after setting the size.
	47 	 3/ 7/95	Hernan  	Made the selectFromCollectionDialog use the
							system dialog instead of our custom one. This is
							because our modal dialog code is not written
							correctly and hangs the machine in weird cases.
	48 	 3/15/95	Hernan  	mouseWithin of the picker of HelpWindowDisplayer
							now checks that the charPos is not greater than 
							the number of characters in the buffer.
	49 	 3/21/95	Hernan  	1230630: fixing process-help-entry to deal with
							things like "set location".
	50 	 3/22/95	Hernan  	1230871: tis-a-property? should be used to test
							for propertyness.
	51 	 4/ 7/95	Hernan  	1237158: process-help-entry deals with the case when the
							symbol-function is not a function but something else (like
							getting the symbol-function of 'if)
	2  	 6/23/95	Hernan  	1250515: use tis-a-property? instead of (memq x (properties obj)) 
							for test of propertydom... This is faster and consistent 
							with the DB functions.
	2  	 5/ 7/96	sidney  	Changes for new object system
	3  	 2/27/97	Hernan  	
|# ;(do not edit past this line!!)
