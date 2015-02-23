;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories


;;;

(in-package :SK8Development)

(SK8-declare-syms :SK8 :public ; Updated 11-13-96  12:12 pm
                  SK8::SCRIPTBACKTRACEMENU SK8::SCRIPTVERSIONDELETE SK8::SCRIPTVERSIONDELETEOTHERS
                  SK8::EDITORLINEARTEXTPICKER)

;;; ___________________________________________________________________________________________
;;; ___________________________________________________________________________________________
;;; ___________________________________________________________________________________________
;;;                                    HELPER FUNCTIONALITY:
;;; ___________________________________________________________________________________________
;;; ___________________________________________________________________________________________
;;; ___________________________________________________________________________________________



;;; ______________________________________________________________
;;; ______________________________________________________________
;;;                    Hooks to Project saving/closing
;;; ______________________________________________________________
;;; ______________________________________________________________


(define-handler ensureHandlersSaved (Project &key (dialog t))
  (DECLARE (IGNORE DIALOG)) ; *** FOR NOW
  (dolist (w (contents stage))
    (when (and (is-a w ScriptEditorWindow)
               (eq (first (inputHandlerID w)) me))
      (let ((editorField (editor w)))
        (when (and (dirty editorField)
                   (not (displayingActiveVersion editorField)))
          (saveVersionDisplayed editorField))))))


(define-handler dismissHandlerEditors (Project &key (saving t) (dialog t))
  (DECLARE (IGNORE DIALOG)) ; *** FOR NOW
  (dolist (w (contents stage))
    (when (and (is-a w ScriptEditorWindow)
               (eq (first (inputHandlerID w)) me))
      (when (not saving) (setf (dirty (editor w)) nil))
      (setf (container w) nil))))

;;; ______________________________________________________________
;;; ______________________________________________________________
;;; The EditorLinearTextPicker.
;;; ______________________________________________________________
;;; ______________________________________________________________

(new linearTextPicker :project sk8 :objectname "EditorLinearTextPicker")

(define-handler mousedown ((myfield sk8::EditorLinearTextPicker))
  ;; Only do this stuff when there are items. Avoiding the call to
  ;; hilite-selection avoids the empty line.
  (when (items (container me))
    (call-next-method)
    (when (runningmode (editor (sk8::window me)))
      (let* ((curText (selectedItems me))
             (curhandlerid (inputHandlerId (editor (sk8::window me))))
             (curValue (getLocalVarInLocalContext curhandlerid curText)))
        (popUpBalloon (h mouse) (v mouse) 
                      (format nil "Local ~a has the value ~a"
                              curText
                              (objectstring curValue :project (first curhandlerid)))
                      :endOnMouseUp t)
        ))
    ))

(bestsize (myfield sk8::EditorLinearTextPicker))
(bestsize (myscroller sk8::EditorLinearTextPicker))

;;; ___________________________________________________________________________________________
;;; ___________________________________________________________________________________________
;;; ___________________________________________________________________________________________
;;;                                    The Editor
;;; ___________________________________________________________________________________________
;;; ___________________________________________________________________________________________
;;; ___________________________________________________________________________________________


;;; ______________________________________________________________
;;; ______________________________________________________________
;;; The window
;;; ______________________________________________________________
;;; ______________________________________________________________


;; *** Since this had a bootstrapping value so the error system wouldn't die during the build
(let ((ccl::*warn-if-redefine-kernel* nil))
  (makunbound 'SK8::ScriptEditorWindow))

(new rectangle :objectname "ScriptEditorWindow" :project SK8
     :properties '((watcher :value nil)
                   (backtraceList :value nil)
                   ))

;;; "Promote" these three properties of ScriptEditorWindow's editor up to ScriptEditorWindow itself:

(define-handler versionDisplayed (ScriptEditorWindow)
  (versionDisplayed (editor me)))

(define-handler (setf versionDisplayed) (version ScriptEditorWindow)
  (setf (versionDisplayed (editor me)) version))

(define-handler availableVersions (ScriptEditorWindow)
  (availableVersions (editor me)))

(define-handler inputHandler (ScriptEditorWindow)
  (inputHandler (editor me)))

(define-handler (setf inputHandler) (handlerObj ScriptEditorWindow)
  (setf (inputHandler (editor me)) handlerObj))

(define-handler inputHandlerID (ScriptEditorWindow)
  (inputHandlerID (editor me)))

(define-handler (setf inputHandlerID) (handlerID ScriptEditorWindow)
  (setf (inputHandlerID (editor me)) handlerID))


;;; Finds the place to put the ediotr.
(define-handler bestSize (ScriptEditorWindow)
  (let ((defaultLeft 40)
        (defaultTop 40))
    (SK8-multival-bind (width height) (size me)
      ;;;Go through the existing windows and...
      (dolist (w (contents stage))
        (when (is-a w ScriptEditorWindow)
          ;; ...Stack it on top of the frontmost editor window
          (SK8-multival-bind (left top right bottom) (boundsRect w)
            (setq left (+ left 20)
                  top (+ top 20)
                  right (+ left width)
                  bottom (+ top height))
            ;; If it would be off the screen, put it in the default position
            (SK8-multival-bind (stageRight stageBottom) (size Stage)
              (when (or (>= right stageRight) (>= bottom stageBottom))
                (setq left defaultLeft
                      top defaultTop
                      right (+ left width)
                      bottom (+ top height))))
            (setBoundsRect me left top (+ left width) (+ top height) :justMoving t)
            (return-from bestSize))))
      ;; If there's no other editor window; use the default position
      (setBoundsRect me defaultLeft defaultTop (+ defaultLeft width) (+ defaultTop height) :justMoving t))))

(define-handler resetLayout (ScriptEditorWindow)
  (let ((editor (editor me)))
    (when (running editor) (switch-out-of-running-mode editor)))
  (setf (visible (third (menus (sk8::menubar me)))) nil)
  (let ((watcher (watcher me)))
    (when watcher
      (when (visible watcher) (do-watcher-toggle me))
      (setup-expressionWatcher-items watcher nil nil :update nil)))
  (let* ((mySplitter (myVarSplitter me))
         (mySplitterTop (top mySplitter))
         (prototypicalSplitterTop (top (myVarSplitter ScriptEditorWindow))))
    (unless (= mySplitterTop prototypicalSplitterTop)
      (setf (top mySplitter :resizing nil) prototypicalSplitterTop)
      (fix-splitter-damage mySplitter mySplitterTop))
    (SK8-multival-bind (width height) (size ScriptEditorWindow)
      (SK8-multival-bind (wWidth wHeight) (size me)
        (unless (and (= wWidth width) (= wHeight height)) (setSize me width height))))))

(define-handler enteringStage (ScriptEditorWindow)
  (setf (keyTarget me) (editor me)))

(define-handler leavingStage (ScriptEditorWindow)
  (let ((scriptEditor (editor me))
        (watcher (watcher me)))
    (when (running scriptEditor)
      (editorDebugAbort me)
      (if (running scriptEditor) (switch-out-of-running-mode scriptEditor)))
    (cond
     ((dirty scriptEditor)
      (saveVersionDisplayed scriptEditor))
     ((dirty watcher)
      (save-handler-watchExpressions scriptEditor (itemsList (picker watcher)))))
    (setvalue 'inputhandlerid (editor me) nil)  ;;; clear the id when we close...
    (setf (globalvariables (editor me)) nil)
    (setf (localvariables (editor me)) nil)
    (discard me)))

(define-handler activate (ScriptEditorWindow)
  (call-next-method)
  (let ((exprWatcher (watcher me)))
    (when (and exprWatcher (visible exprWatcher))
      (update exprWatcher))))

;;; Returns how much to offset things by accounting for the presence of the watcher.

(defun watcherVOffset (theEditorWindow)
  (let ((exprWatcher (watcher theEditorWindow)))
    (if (and exprWatcher (visible exprWatcher))
      (height exprWatcher)
      0)))

(setf (size ScriptEditorWindow) '(425 310))
(setf (fillcolor scriptEditorWindow) white) ;; Since totally covered!!!

(setf (windowStyle scriptEditorWindow) 'documentWithZoom)
(setFramesize scriptEditorWindow 0 0)

(define-handler minimumSize (scriptEditorWindow)
  (let ((theWatcher (watcher me))
        (minWidth 315)
        (minHeight 150))
    (when (and theWatcher (visible theWatcher))
      (SK8-multival-bind (watcherMinWidth watcherMinHeight) (minimumSize theWatcher)
        (declare (ignore watcherMinWidth))
        (incf minHeight watcherMinHeight)))
    (SK8-multivals minWidth minHeight)))

;;; NOTE: this will take a long time!!!
;;; menubar takes --> 1.6 secs to create.
;;; totalTime        -->  5.1 secs.

(define-handler initialize (ScriptEditorWindow original isNew initArgs)
  (declare (ignore original isNew initArgs))
  ;; [0] Copy the tags...
  (call-next-method)
  ;; [1] Initialize the splitter.
  (let ((actorsAbove (list (myGlobals me) (myLocals me)))
        (actorsBelow (list (editor me) (myVScroller me)))
        (actorsAffected (list (myGlobals me) (myLocals me)
                              (editor me) (myVScroller me)))
        (absoluteTop 37)
        (absoluteBottom 258))
    (splitActors (myVarSplitter me) actorsAbove actorsBelow
                 actorsAffected absoluteTop absoluteBottom)))

(define-handler resized (scriptEditorWindow)
  (let (hsize vSize)
    (declare (Special hSize vSize))
    (call-next-method)
    (sk8-multival-setf (hSize vSize) (size me))
    (setf (absoluteBottom (myVarSplitter me)) (- vSize 14))
    (setf (absoluteBottom (myEditorSplitter me)) (1+ vSize))
    (gs:docontents (theActor me)
      (bestSize theActor))
    (if (watcher me) (bestsize (watcher me)))   ;;;make sure the watcher is updated last.
    ))

(define-handler mouseEnter (ScriptEditorWindow)
  (setf (cursor Stage) StandardCursor))


(define-handler uppermostHandlerIDinBacktrace (ScriptEditorWindow)
  (let (sk8scriptForm 
        (backtraceList (backtraceList me)))
    (when backtraceList
      (dolist (i backtraceList)
        (when (and (not sk8scriptform) 
                   (ps::lfunHasSources (first i))
                   (second i))
          (setf sk8scriptForm i)))
      (second sk8scriptForm))))

(defun handleridequal (h1 h2)
  (and (equal (first h1) (first h2))
       (equal (second h1) (second h2))
       (equal (third h1) (third h2))
       (equal (fourth h1) (fourth h2))))

;;; _____________________________________________________________________________________________
;;; _____________________________________________________________________________________________
;;; _____________________________________________________________________________________________
;;; Editor Debug Commands
;;; _____________________________________________________________________________________________
;;; _____________________________________________________________________________________________
;;; _____________________________________________________________________________________________

;;; These three methods are called from the menus and the buttons. They call the
;;; real debugger functions that do the work. The indirection is required to allow
;;; the editor to do things appropriate to the operation at hand.

;;; Should the first two close the window? 

(define-handler editorDebugGo (ScriptEditorWindow)
  (let ((inputHandlerId (inputHandlerId (editor me))))
    (unless (or (not (backtracelist me))
                (handleridequal (uppermostHandlerIDinBacktrace me) inputHandlerId))
      (setf inputHandlerId (uppermostHandlerIDinBacktrace me)))
    (debugGo inputHandlerId)))

(define-handler editorDebugAbort (ScriptEditorWindow)
  (let ((inputHandlerId (inputHandlerId (editor me))))
    (when (and (backtracelist me)
               (not (handleridequal (uppermostHandlerIDinBacktrace me) inputHandlerId)))
      (setf inputHandlerId (uppermostHandlerIDinBacktrace me)))
    (debugAbort inputHandlerId)))

;;; Moves to the next line to be stepped. 

(define-handler editorDebugStep (ScriptEditorWindow)
  ;;Do the step
  (let ((nextLine (debugStep (inputHandlerId (editor me)))))
    ;;update the editor to reflect the current position
    (update-script-running-mode (editor me) nil nil nextLine))
  ;;update the expression watcher to the new values
  (let ((exprWatcher (watcher (sk8::window me))))
    (when (and exprWatcher (visible exprWatcher))
      (update exprWatcher)))
  )

;;; This function brings up a handler editor focused on the handler or function that
;;; has the runtime attention problem. The arguments mean:
;;;    handlerId,         a list of the form (proj handlerName obj qualifier)
;;;    lineNumber,        the # of the SK8Script line where the problem is
;;;    type,              :error or :breakpoint
;;;    message,           a string, only required on error
;;;    availableActions   a list, possible values are :abort, :go, :step.

(define-sk8-function bringUpHandlerWithRuntimeProblem nil (handlerId
                                                                 lineNumber
                                                                 type
                                                                 message
                                                                 availableActions
                                                                 backtraceList)
  (declare (ignore availableActions))
  (destructuring-bind (proj hName &optional hObject hqualifier) handlerId
    (let* ((wind (editHandler proj hname hobject hqualifier nil)))
      ;; Having brought up the editor, show the error line.
      (update-script-running-mode (editor wind) nil type linenumber)
      ;; Setup the calling chain...
      (setf (backtracelist wind) backtracelist)
      ;;and reset the cursor to avoid any annoying remaining watch cursors, etc.
      (when (neq (cursor stage) standardcursor)
        (setf (cursorlocked stage) nil)
        (setf (cursor stage) standardcursor))
      ;; show the buttons appropriate...
      (update (buttonpanel wind))
      ;; if an error, show the dialog with the error message.
      (when (eq type :error)
        (when message (messageToUser message))
        (setf (currentErrorMessage (editor wind)) (or message "Nested Error"))))))



;;; _____________________________________________________________________________________________
;;; _____________________________________________________________________________________________
;;; _____________________________________________________________________________________________
;;; THE BUTTON PANEL
;;; _____________________________________________________________________________________________
;;; _____________________________________________________________________________________________
;;; _____________________________________________________________________________________________

(new Rectangle :objectname "ScriptWindowButtons" :project sk8)

(setf (private ScriptWindowButtons) t)  ; not in public api

(setFramesize ScriptWindowButtons 0 0)
(setBoundsRect ScriptWindowButtons 200 0 350 18)
(setf (container scriptWindowButtons) scriptEditorWindow)
(setf (fillcolor scriptWindowButtons) white)
(hide ScriptWindowButtons)
(tagpart scriptEditorWindow scriptWindowButtons 'buttonPanel)

(define-handler bestsize (scriptWindowButtons)
  (let ((h (width (container me))))
    (setBoundsRect me 230 0 h 18)))

(define-handler resized (scriptWindowButtons)
  (gs:docontents (theButton me)
    (bestsize theButton)))

(define-handler update (scriptWindowButtons)
  (let* ((editorField (editor (container me)))
         (enabled? (not (runtimeError editorField)))
         (goButton (goButton me))
         (stepButton (stepButton me)))
    (setf (enabled goButton) enabled?)
    (setf (enabled stepButton) (and enabled?
                                    (handleridequal (inputhandlerid editorField) (uppermostHandlerIDinBacktrace (container me)))
                                    ))
    ))

;;; The button prototype.

(new roundRect :objectname "ScriptEditorButton" :project sk8)
(setf (textFont ScriptEditorButton) "Geneva")
(setf (textSize ScriptEditorButton) 9)
(setf (height ScriptEditorButton) 14)
(setf (roundedness scriptEditorButton) '(12 12))
(setf (autoHighlight scriptEditorButton) t)

(define-handler enabled (ScriptEditorButton)
  (eq 'normal (mouseSensitivity me)))

(define-handler (setf enabled) (flag ScriptEditorButton)
  (if flag
    (unless (enabled me)
      (setf (textColor me) Black
            (mouseSensitivity me) 'normal))
    (when (enabled me)
      (setf (textColor me) Gray
            (mouseSensitivity me) 'invisible))))

;;; The buttons.

(new ScriptEditorButton :objectname "ScriptGoButton" :project sk8)
(setf (text ScriptGoButton) "Go")
(setf (container ScriptGoButton) scriptWindowButtons)
(setf (width ScriptGoButton) 31)
(setf (location scriptgobutton) '(59 9))
(tagpart scriptWindowButtons ScriptGoButton 'goButton)

(define-handler bestSize (scriptGoButton)
  (let ((h (width (container me))))
    (setf (h me) (- h 91))))

(define-handler mouseUp (scriptGoButton)
  (editordebugGo (container (container me))))

(new ScriptEditorButton :objectname "ScriptStepButton" :project sk8)
(setf (text ScriptStepButton) "Step")
(setf (container ScriptStepButton) scriptWindowButtons)
(setf (width ScriptStepButton) 31)
(setlocation scriptstepButton 93 9)
(tagpart scriptWindowButtons ScriptStepButton 'stepButton)

(define-handler bestSize (scriptStepButton)
  (let ((h (width (container me))))
    (setf (h me) (- h 57))))

(define-handler mouseUp (scriptStepButton)
  (editorDebugStep (container (container me)))
  )

(new ScriptEditorButton :objectname "ScriptAbortButton" :project sk8)
(setf (text ScriptAbortButton) "Abort")
(setf (container ScriptAbortButton) scriptWindowButtons)
(setf (width ScriptAbortButton) 31)
(setlocation scriptAbortButton 127 9)

(define-handler bestSize (scriptAbortButton)
  (let ((h (width (container me))))
    (setf (h me) (- h 23))))

(define-handler mouseUp (scriptAbortButton)
  (editorDebugAbort (container (container me))))


;;; _____________________________________________________________________________________________
;;; _____________________________________________________________________________________________
;;; _____________________________________________________________________________________________
;;; The Variable Panes.
;;; _____________________________________________________________________________________________
;;; _____________________________________________________________________________________________
;;; _____________________________________________________________________________________________

;;; The height of these guys does not change!!!

(new EditorLinearTextPicker :objectname "ScriptGlobals" :project sk8)
(setf (container scriptglobals) scriptEditorWindow)
(setBoundsRect scriptglobals -1 17 175 48)
(setf (text scriptGlobals) "Globals")
(tagpart scriptEditorWindow scriptGlobals 'myGlobals)

(define-handler bestsize (ScriptGlobals)
  (let ((width (width (container me))))
    (setf (right me) (/ width 2))))

(define-handler createTextdisplayItem (ScriptGlobals theItem)
  (sk8::name theItem))

(new EditorLinearTextPicker :objectname "ScriptLocals" :project sk8)
(setf (container ScriptLocals) scriptEditorWindow)
(setf (text ScriptLocals) "Locals")
(setBoundsRect ScriptLocals 174 17 351 48)
(tagpart scriptEditorWindow scriptLocals 'myLocals)

(define-handler bestsize (ScriptLocals)
  (let ((width (width (container me))))
    (sk8-multival-bind (ll tt rr bb) (boundsRect me)
      (setBoundsRect me (1- (/ width 2)) tt (1+ width) bb))))

(define-handler createTextdisplayItem (ScriptLocals theItem)
  (sk8::name theItem))




;;; _____________________________________________________________________________________________
;;; _____________________________________________________________________________________________
;;; _____________________________________________________________________________________________
;;; Scriptfield and Scrollers
;;; _____________________________________________________________________________________________
;;; _____________________________________________________________________________________________
;;; _____________________________________________________________________________________________

(new EditorScriptEditText :objectname "scriptField" :project sk8)
(setBoundsRect scriptField -1 49 336 261)
(setf (container scriptField) scriptEditorWindow)
(tagpart scriptEditorWindow scriptField 'editor)

(define-handler bestsize (scriptField)
  (sk8-multival-bind (h v) (size (container me))
    (sk8-multival-bind (ll tt rr bb) (boundsRect me)
      (setBoundsRect me ll tt (- h 14) (- v 14 (watcherVOffset (container me)))))))


(new scroller :objectname "scriptVScroller" :project sk8)
(setBoundsRect scriptVScroller 335 49 351 261)
(setf (container scriptVScroller) scriptEditorWindow)
(tagpart scriptEditorWindow scriptVScroller 'myVScroller)
(setf (partnerVScroller ScriptField :affixing nil) ScriptVScroller)

(define-handler bestsize (scriptVScroller)
  (sk8-multival-bind (h v) (size (container me))
    (setBoundsRect me (- h 15) (top me) (1+ h) 
                   (- v 14 (watcherVOffset (container me))))))

(new scroller :objectname "scriptHScroller" :project sk8)
(setBoundsRect scriptHScroller 20 260 336 276)
(setf (container scriptHScroller) scriptEditorWindow)
(tagpart scriptEditorWindow scriptHScroller 'myHScroller)
(setf (partnerHScroller ScriptField :affixing nil) ScriptHScroller)

(define-handler bestsize (scriptHScroller)
  (sk8-multival-bind (h v) (size (container me))
    (let ((realBottom (- v (watcherVOffset (container me)))))
      (setBoundsRect me 20 (- realBottom 15) (- h 14) (1+ realBottom)))))





;;; _____________________________________________________________________________________________
;;; _____________________________________________________________________________________________
;;; _____________________________________________________________________________________________
;;; The resize box.
;;; _____________________________________________________________________________________________
;;; _____________________________________________________________________________________________
;;; _____________________________________________________________________________________________

(new rectangle :objectname "ScriptEditorResizer" :project sk8)

(setf (private ScriptEditorResizer) t)  ; not a public api

(setBoundsRect ScriptEditorResizer 335 260 351 276)
(setf (fillcolor ScriptEditorResizer) ResizeBoxRenderer)
(setf (container scriptEditorResizer) scriptEditorWindow)
(tagpart scriptEditorWindow ScriptEditorResizer 'myResizer)

(setframesize ScriptEditorResizer 0 0)

(define-handler bestsize (ScriptEditorResizer)
  (sk8-multival-bind (h v) (size (container me))
    (let ((realBottom (- v (watcherVOffset (container me)))))
      (setBoundsRect me (- h 15) (- realBottom 15) (1+ h) (1+ realBottom)))))

(define-handler mousedown (ScriptEditorResizer)
  (resize (sk8::window me) 'bottomright))






;;; _____________________________________________________________________________________________
;;; _____________________________________________________________________________________________
;;; _____________________________________________________________________________________________
;;; Splitters
;;; _____________________________________________________________________________________________
;;; _____________________________________________________________________________________________
;;; _____________________________________________________________________________________________

;;; _______________________________
;;; Splitter #1: resizes the variable panes.
;;; _______________________________

(new splitter :objectname "varPaneSplitter" :project sk8)
(setBoundsRect varPaneSplitter 0 47 350 50)
(setf (fillcolor varPaneSplitter) black)
(setf (container varPaneSplitter) scriptEditorWindow)

(tagpart scriptEditorWindow varPaneSplitter 'myVarSplitter)

(define-handler bestsize (varPaneSplitter)
  (let ((w (width (container me))))
    (setf (right me) w)))


;;; _______________________________
;;; Splitter #2: resizes editor and vScroller.
;;; _______________________________

(new splitter :objectname "editorSplitter" :project sk8)
(setBoundsRect editorSplitter 0 275 350 278)
(setf (fillcolor editorSplitter) black)
(setf (container editorSplitter) scriptEditorWindow)

(tagpart scriptEditorWindow editorSplitter 'myEditorSplitter)

(define-handler bestsize (editorSplitter)
  (let* ((theWindow (container me))
         (theWatcher (watcher theWindow))
         h v)
    (sk8-multival-setf (h v) (size theWindow))
    (if theWatcher
      (setBoundsRect me (left me) (- v (height theWatcher)) 
                     (1+ h) (+ (- v (height theWatcher)) 3))
      (setf (right me) h))))

(defun hook-up-editorSplitter (theSplitter edWindow)
  (let ((actorsAbove (list (editor edWindow) (myvScroller edWindow)))
        (actorsBelow (list (watcher edWindow)))
        (actorsAffected (list (editor edWindow) (myvScroller edWindow)
                              (myHScroller edWindow) 
                              (watcher edWindow)))
        (absoluteTop (top (editor edWindow)))
        (absoluteBottom (bottom (watcher edWindow))))
    (splitActors theSplitter actorsAbove actorsBelow
                 actorsAffected absoluteTop absoluteBottom)))

;;; Make sure the absolute top is up to date.

(define-handler drag (editorSplitter &key live otherActors dropEvents DraggedOverEvents 
                                      draggingMouseWithinEvents onStage constrainingRect)
  (setf (absoluteTop me) (top (editor (container me))))
  (call-next-method))





;;; _____________________________________________________________________________________________
;;; _____________________________________________________________________________________________
;;; _____________________________________________________________________________________________
;;; THE MENUBAR
;;; _____________________________________________________________________________________________
;;; _____________________________________________________________________________________________
;;; _____________________________________________________________________________________________


(new sk8::menubar :objectname "ScriptWindowMenubar" :project sk8)
(setf (container ScriptWindowMenubar) scriptEditorWindow)
(setBoundsRect ScriptWindowMenubar 0 0 230 18)
(setFrameSize ScriptWindowMenubar 0 0)

(tagpart scriptEditorWindow ScriptWindowMenubar 'SK8::menubar)

(define-handler mouseEnter (scriptWindowMenubar)
  (setf (cursor stage) cursorMenu))

(define-handler mouseLeave (scriptWindowMenubar)
  (setf (cursor stage) standardCursor))

(new menu :objectname "ScriptMenu" :project sk8)
(setf (textFont scriptMenu) "Geneva")
(setf (textSize scriptMenu) 9)
(setf (textstyle scriptMenu) '(bold))

(define-handler actorTextSize (scriptMenu &key theText)
  (declare (ignore theText))
  (sk8-multival-bind (h v) (call-next-method)
    (sk8-multivals (- h 6) v)))

(defun editorField-from-menuItem (theMenuItem)
  (editor (container (sk8::menubar (menu theMenuItem)))))
(defun windowKeyTarget-from-menuItem (theMenuItem)
  (keyTarget (container (sk8::menubar (menu theMenuItem)))))
(defun editorWindow-from-menuItem (theMenuItem)
  (sk8::window (SK8::menubar (menu theMenuItem))))

;;; _______________________________
;;; The Menus
;;; _______________________________

;;; (1) Edit Menu.

(new scriptMenu :objectname "ScriptEditMenu" :project sk8)
(setf (text scriptEditMenu) "Edit")
(setf (sk8::menubar ScriptEditMenu) ScriptWindowMenubar)

(new menuitem :objectname "ScriptToolsAddProperty" :project sk8
     :text "New Property..." :commandKey #\R :menu ScriptEditMenu)

(define-handler update (ScriptToolsAddProperty)
  (let ((handlerID (inputHandlerID (editorField-from-menuItem me))))
    (setf (text me) (format nil "New Property For ~a..."
                            (objectString (or (third handlerID) (first handlerID)))
                            ))))

(define-handler menuSelect (ScriptToolsAddProperty)
  (let* ((editorField (editorField-from-menuItem me))
         (handlerID (inputHandlerID editorField))
         (edFred (editData editorField))
         (buf (fred-buffer edFred)))
    (addPropertyDialog (or (third handlerID) (first handlerID))
                       :property (multiple-value-call 'buffer-substring buf (selection-range edFred)))))

(new menuitem :objectname "ScriptToolsEditOther" :project sk8
     :text "Create New Handler..." :commandKey #\N :menu ScriptEditMenu)

(define-handler update (ScriptToolsEditOther)
  (let ((handlerID (inputHandlerID (editorField-from-menuItem me))))
    (setf (text me) (format nil "New Handler For ~a..."
                            (objectString (or (third handlerID) (first handlerID)))
                            ))))

(define-handler menuSelect (ScriptToolsEditOther)
  (let ((handlerID (inputHandlerID (editorField-from-menuItem me))))
    (addHandlerDialog (or (third handlerID) (first handlerID)))))

(new menuitem :objectname "ScriptToolsDelete" :project sk8
     :text "Remove This Handler..." :commandKey #\D :menu ScriptEditMenu)

(define-handler update (ScriptToolsDelete)
  (setf (enabled me) (inputhandler (editorField-from-menuItem me)))
  )

(define-handler menuSelect (ScriptToolsDelete)
  (removeHandlerDialog (inputhandler (editorField-from-menuItem me))))

;;__________________________________________________________________

(new menuSpacer :project sk8 :menu ScriptEditMenu)


;;__________________________________________________________________

(new menuitem :objectname "scriptAllPurposeToggler" :project sk8
     :text "Expression Watcher" :menu ScriptEditMenu)

(define-handler update (scriptAllPurposeToggler)
  (setf (checkmark me) (and (watcher (sk8::window (menu me))) (visible (watcher (sk8::window (menu me)))))))

(setf (private scriptAllPurposeToggler) t)  ; not in public api

(tagpart scriptEditorWindow scriptAllPurposeToggler 'myToggler)

(defun do-watcher-toggle (scriptwind)
  (let* ((watcher (watcher scriptWind))
         (theSplitter (myEditorSplitter scriptWind))
         watcherHeight)
    (unless watcher
      (setf watcher (new RuntimeExpressionWatcher :project (project scriptWind))
            (watcher scriptWind) watcher
            (visible watcher) nil
            (container watcher) scriptWind)
      ;; Hook up the splitter.
      (hook-up-editorSplitter theSplitter scriptWind)
      ;; Load the watchexpressions list from the store
      (when (inputhandler scriptwind)
        (let ((watchExpressions (get-handler-watchExpressions (editor scriptWind))))
          (setup-expressionWatcher-items watcher nil watchExpressions :update nil))))
    (setq watcherHeight (height watcher))
    (withActorLocked (scriptWind)
      (cond
       ((visible watcher)
        (hide watcher)
        (hide theSplitter)
        (setf (keyTarget scriptWind) (editor scriptWind))
        (show (myResizer scriptWind))
        (setf (bottom scriptWind :resizing t) (- (bottom scriptWind) watcherHeight)))
       (t
        (update watcher)
        (show watcher)
        (setf (keyTarget scriptWind) watcher)
        (show theSplitter)
        (hide (myResizer scriptWind))
        (setf (bottom scriptWind :resizing t) (+ (bottom scriptWind) watcherHeight)))))))

(define-handler menuselect (scriptAllPurposeToggler)
  (do-watcher-toggle (sk8::window (menu me))))


;;__________________________________________________________________


(new menuSpacer :project sk8 :menu ScriptEditMenu)


;;__________________________________________________________________
;; Printing.

(new menuitem :objectname "ScriptEditPrint" :project sk8
     :text "Print" :CommandKey #\P :menu ScriptEditMenu)

(define-handler menuSelect (ScriptEditPrint)
  (let* ((scriptWind (editorWindow-from-menuItem me)))
    (window-hardcopy (editData (editor scriptWind)) t)))

;;__________________________________________________________________
;; STACK EDITORS
;; Stacks nicely all of the open editing windows...

(new menuItem :objectName "ScriptEditStackWindows" :project sk8)
(setf (text ScriptEditStackWindows) "Stack SK8Script Editors")
(setf (menu ScriptEditStackWindows) ScriptEditMenu)
(setf (commandkey ScriptEditStackWindows) #\E)

(define-handler Update (ScriptEditStackWindows)
  (let ((editorsAround? (find-if #'(lambda (x) (inheritsFrom x ScriptEditorWindow))
                                 (contents Stage))))
    (setf (enabled me) editorsAround?)))

(define-handler menuSelect (ScriptEditStackWindows)
  (let ((eds (remove-if-not #'(lambda (x) (inheritsFrom x scriptEditorWindow)) (contents stage)))
        (curh 20) (curv 50))
    (dolist (i (nreverse eds))
      (sk8-multival-bind (hh vv) (size i)
        (setBoundsRect i curh curv (+ curh hh) (+ curv vv)))
      (incf curh 20)
      (incf curv 20))))

(new menuSpacer :project sk8 :menu ScriptEditMenu)

(new menuitem :objectname "ScriptEditUndo" :project sk8
     :text "Undo" :CommandKey #\Z :menu ScriptEditMenu)

(define-handler menuSelect (ScriptEditUndo)
  (undo (windowKeyTarget-from-menuItem me)))


(new menuitem :objectname "ScriptEditCut" :project sk8
     :text "Cut" :CommandKey #\X :menu ScriptEditMenu)

(define-handler menuSelect (ScriptEditCut)
  (cutSelectionToClipboard (windowKeyTarget-from-menuItem me)))

(new menuitem :objectname "ScriptEditCopy" :project sk8
     :text "Copy" :CommandKey #\C :menu ScriptEditMenu)

(define-handler menuSelect (ScriptEditCopy)
  (copySelectionToClipboard (windowKeyTarget-from-menuItem me)))

(new menuitem :objectname "ScriptEditPaste" :project sk8
     :text "Paste" :CommandKey #\V :menu ScriptEditMenu)

(define-handler menuSelect (ScriptEditPaste)
  (pasteClipboardToSelection (windowKeyTarget-from-menuItem me)))

(new menuitem :objectname "ScriptEditClear" :project sk8
     :text "Clear" :menu ScriptEditMenu)

(define-handler menuSelect (ScriptEditClear)
  (clearSelection (windowKeyTarget-from-menuItem me)))

(new menuitem :objectname "ScriptEditSelectAll" :project sk8
     :text "Select All" :CommandKey #\A :menu ScriptEditMenu)

(define-handler menuSelect (ScriptEditSelectAll)
  (setSelection (windowKeyTarget-from-menuItem me) 0 -1))

(new menuSpacer :project sk8 :menu ScriptEditMenu)

(new menuitem :objectname "ScriptEditClose" :project sk8
     :text "Close" :CommandKey #\W :menu ScriptEditMenu)

(define-handler menuSelect (ScriptEditClose)
  (sk8::close (editorWindow-from-menuItem me)))




;;;;;-----------------------------------------------------------------------------------------------
;;;;;-----------------------------------------------------------------------------------------------
;;; (2) Version Menu and related Version Functions
;;;;;-----------------------------------------------------------------------------------------------
;;;;;-----------------------------------------------------------------------------------------------

(new scriptMenu :objectname "ScriptVersionMenu" :project sk8)
(setf (text ScriptVersionMenu) "Version")
(setf (sk8::menubar ScriptVersionMenu) ScriptWindowMenubar)

(new menuitem :objectname "ScriptVersionActivate" :project sk8
     :text "Activate Current Version" :commandKey #\S :menu ScriptVersionMenu)

(define-handler update (ScriptVersionActivate)
  (let ((editor (editor (editorWindow-from-menuItem me))))
    (setf (enabled me) (not (running editor)))))
#|
(and (not (running editor))
   (or (not (displayingActiveVersion editor))
       (optionKeyDown)))
|#

(define-handler menuSelect (ScriptVersionActivate)
  (if (displayingActiveVersion (editor (editorWindow-from-menuItem me)))
    (messagetouser "Already displaying compiled version." :beep t)
    (withLockedCursor AnimatedClock
      (activateVersionDisplayed (editor (editorWindow-from-menuItem me))))))

(new menuSpacer :project sk8 :menu ScriptVersionMenu)

(new menuitem :objectname "ScriptVersionDelete" :project sk8
     :text "Delete Current Version" :menu ScriptVersionMenu)

(define-handler menuSelect (ScriptVersionDelete)
  (let* ((scriptWind (editorWindow-from-menuItem me))
         (editorField (editor scriptWind)))
    (deleteVersionDisplayed editorField :warning t)
    (update-scriptEditorVersionMenu scriptWind)))

(new menuitem :objectname "ScriptVersionDeleteOthers" :project sk8
     :text "Delete Earlier Versions" :menu ScriptVersionMenu)

(define-handler menuSelect (ScriptVersionDeleteOthers)
  (let ((scriptWind (editorWindow-from-menuItem me)))
    (deleteEarlierVersions (editor scriptWind) :warning t)
    (update-scriptEditorVersionMenu scriptWind)))

(new menuSpacer :project sk8 :menu ScriptVersionMenu)

;;;_______________________________
;;The Backtrace Menu
;;;_______________________________
(new scriptMenu :objectname "ScriptBacktraceMenu" :project sk8)
(addparent SK8:ScriptBacktraceMenu pickerMenu)
(setf (text SK8:ScriptBacktraceMenu) "Calling Chain")
(setf (sk8::menubar SK8:ScriptBacktraceMenu) ScriptWindowMenubar)

(define-handler items (SK8:ScriptBacktraceMenu)
  (let* ((editorWindow (sk8::window me))
         (editor (editor editorWindow)))
    (when (running editor)
      (backtracelist editorWindow)
      )))

(define-handler CreateTextDisplayItem (SK8:ScriptBacktraceMenu theItem)
  (call-next-method me (first theitem)))

(define-handler update (SK8:ScriptBacktraceMenu)
  (call-next-method)
  (setf (text me) "Calling Chain")
  (dolist (i (menuitems me))
    (setf (enabled i) (ps::lfunhassources (first (item i))))))

(define-handler menuselect (SK8:ScriptBacktraceMenu)
  (when (selecteditem me)
    (sk8-multival-bind (lfun id lineNum) (selecteditem me)
      (bringUpHandlerWithRuntimeProblem id
                                        lineNum
                                        (runningMode (editor (sk8::window me)))
                                        nil 
                                        (if (eq (runningMode (editor (sk8::window me))) :error)
                                          '(:abort)
                                          '(:go :abort))
                                        (backtraceList (sk8::window me))))))

;;;_______________________________
;;The version Items.
;;;_______________________________

(new menuitem :objectname "ScriptMenuVersionItem" :project SK8)

(defun scriptEditor-versionMenu (scriptEditorWind)
  (second (menus (SK8::menubar scriptEditorWind))))
(defun scriptVersionMenu-versionItems (theVersionMenu)
  (nthcdr 5 (menuitems theVersionMenu)))

(define-handler menuSelect (ScriptMenuVersionItem)
  (let* ((editorWind (editorWindow-from-menuItem me))
         (editorField (editor editorWind))
         (theVersionMenu (menu me))
         (versionItems (scriptVersionMenu-versionItems theVersionMenu))
         (selectedVersion (nth (position me versionItems :test #'eq) (availableVersions editorField))))
    (unless (eql selectedVersion (versionDisplayed editorField))
      (setf (versionDisplayed editorField) selectedVersion))))

(defun update-scriptEditorVersionMenu (me) ; the window
  (let* ((editorField (editor me))
         (activeVersionID (activeVersion editorField))
         (currentVersionID (versionDisplayed editorField))
         (theVersionMenu (scriptEditor-versionMenu me))
         (versionItems (scriptVersionMenu-versionItems theVersionMenu))
         (versionIDs (availableVersions editorField))
         item itemText)
    ;; Update the title to indicate whether we're showing the active version
    (gs:with-queued-drawing
      (setf (textStyle theVersionMenu) (if (eql currentVersionID activeVersionID)
                                         '(bold)
                                         '(bold italic)))
      )
    ;; Set up the items (creating new ones if needed)
    (dolist (id versionIDs)
      (setq item (pop versionItems))
      (unless item (setq item (new ScriptMenuVersionItem :project SK8 :menu theVersionMenu)))
      (setq itemText (string-left-trim *whitespace-charbag* (cl-user::get-time-string :time id)))
      (when (eql id activeVersionID) (setq itemText (concatenate 'string itemText "  (active)")))
      (setf (text item) itemText
            (checkMark item) (eql id currentVersionID)))
    ;; If there were too many items in the menu, get rid of the extraneous ones
    (dolist (remainingItem versionItems) 
      (setf (menu remainingItem) nil))))

(define-handler leavingVersionDisplayed (scriptField)
  ;; If the field is dirty then saveVersionDisplayed will be called, which will save this info
  (unless (dirty me)
    (let* ((scriptEditor (container me))
           (watcher (watcher scriptEditor)))
      (when (dirty watcher)
        (save-handler-watchExpressions scriptEditor (itemsList (picker watcher)))))))

(defun dumb-handler-string (theHandler &key (name nil) (obj nil))
  (let ((theName (or name (and thehandler (name theHandler))))
        (theObj (or obj (and thehandler (object theHandler)))))
    (if theObj
      (format nil "~a of ~a" theName (objectString theObj :project (project theObj)))
      (format nil "The Function ~a" theName))))

(define-handler prepareForNewHandler (scriptField handlerID &key
                                                      justCreated private locked
                                                      watchingState watchExpressions
                                                      returnType documentation editorInfo)
  (declare (ignore justCreated private locked watchingState returnType documentation editorInfo))
  (let* ((scriptWind (container me))
         (exprWatcher (watcher scriptWind)))
    (if (null handlerid) (setf handlerid (list nil nil nil nil)))
    (destructuring-bind (proj handlerName &optional handlerObject handlerQualifier) handlerID
      (declare (ignore proj handlerQualifier))
      ;; Update the window's title
      (setf (windowTitle scriptWind)
            (dumb-handler-string nil :name handlername :obj handlerobject))
      
      ;; Update the window's Version menu
      (update-scriptEditorVersionMenu scriptWind)
      (when exprWatcher
        (setup-expressionWatcher-items exprWatcher nil watchExpressions :update (visible exprWatcher))))))


(define-handler prepareForNewHandlerVersion (scriptField version &key
                                                              oldVersion justCreated private locked
                                                              watchingState watchExpressions
                                                              returnType documentation editorInfo)
  (declare (ignore version private locked watchingState returnType documentation editorInfo))
  (let* ((scriptWind (container me))
         (exprWatcher (watcher scriptWind)))
    ;; If we just started a new version while in the active version, give a prominent indication of that fact
    (when (and justCreated (eql oldVersion (activeVersion me)))
      (gs:with-queued-drawing
        (let ((theVersionMenu (scriptEditor-versionMenu (container me))))
          (setf (highlight theVersionMenu) t)
          (sleep 1/50)
          (setf (highlight theVersionMenu) nil)
          (setf (textstyle theVersionMenu) '(bold italic)))))
    ;; Update the window's Version menu
    (update-scriptEditorVersionMenu scriptWind)
    ;; Update the expression watcher. 
    (when (and (not justCreated) exprWatcher)
      (setup-expressionWatcher-items exprWatcher nil watchExpressions
                                     :update (visible exprWatcher)))))


;;; Close the window if the handler's been disposed
;;;
(define-handler handlerDisposed (scriptField &key ((:project proj)) name)
  (declare (ignore proj name))
  (setf (container (container me)) nil))


(define-handler handlerVersionSaved (scriptField version &key active)
  (declare (ignore version))
  ;; Update the window's Version menu (to indicate which version is now active)
  (when active (update-scriptEditorVersionMenu (container me))))


(define-handler saveVersionDisplayed (scriptField &key translation watchingState &allow-other-keys)
  (declare (ignore watchingState))
  (let* ((scriptWind (container me))
         (watcher (watcher scriptWind)))
    (call-next-method me :translation translation
                      :watchingState nil
                      :watchExpressions (if watcher
                                          (itemsList (picker watcher))
                                          :PRESERVE)
                      ;; #####*** EVENTUALLY HOOK THESE GUYS UP!
                      :private :PRESERVE :locked :PRESERVE :returnType :PRESERVE :documentation :PRESERVE
                      :editorInfo :PRESERVE)))

(define-handler prepareForRunningMode (scriptField running)
  (let* ((scriptWind (container me))
         (buttonPanel (buttonPanel scriptWind))
         (ourMenubar (sk8::menubar scriptWind))
         (exprWatcher (watcher scriptWind))
         curEditor)
    (when running
      (update buttonPanel))
    (setf (visible buttonPanel) running)
    (setf (visible (third (menus ourMenubar))) running)
    (when (and exprWatcher (visible exprWatcher))
      (update exprWatcher))
    (when (and (not running)
               (backtracelist scriptWind))
      (dolist (i (rest (backtracelist scriptwind)))
        (when (and (ps::lfunhassources (First i))
                   (not (handleridequal (inputhandlerid me) (second i))))
          (setf curEditor (findHandlerEditor (first (second i)) (second (second i)) (third (second i)) (fourth (second i)) ))
          (when (and curEditor (running (editor curEditor)))
            (switch-out-of-running-mode (editor curEditor))))
        )
      (when (debugActive (inputHandlerId scriptWind))
        (editorDebugAbort scriptWind))
      ))
  )

(define-handler handlerStepped (scriptField)
  (let* ((scriptWind (container me))
         (buttonPanel (buttonPanel scriptWind))
         (exprWatcher (watcher scriptWind)))
    (update buttonPanel)
    (when (and exprWatcher (visible exprWatcher))
      (update exprWatcher))))

(define-handler (setf globalVariables) :after (globals scriptField)
                (setf (items (myGlobals (container me))) globals))

(define-handler (setf localVariables) :after (locals scriptField)
                (setf (items (myLocals (container me))) locals)
                (let ((watcher (watcher (container me))))
                  (when (and watcher (visible watcher))
                    (update watcher :localsChanged t))))


;;; _____________________________________________________________________________________________
;;; _____________________________________________________________________________________________
;;; _____________________________________________________________________________________________
;;; Cleanup
;;; _____________________________________________________________________________________________
;;; _____________________________________________________________________________________________
;;; _____________________________________________________________________________________________


(bringToFront editorSplitter)
(hide editorSplitter)

(bringToFront VarPaneSplitter)



#|
	Change History (most recent last):
	1	11/5/93	hernan	New file. Implements the SK8Script editor window
				in SK8.
	9  	 9/ 3/96	Brian   	Reworked and simplified this file.  Removed all outdated code.
	10 	 9/12/96	Brian   	
	11 	10/10/96	Brian   	Closing a running scriptWindow now aborts the
						execution.
	12 	10/11/96	Brian   	updating the buttonpanel at runtimeerror.
	13 	10/11/96	Brian   	
	14 	10/14/96	Brian   	
	15 	10/17/96	Hernan  	Fixing prepare for new handler to handle null handlerids.
	16 	11/14/96	Brian   	Adding calling chain menu and related infrastructure.
	17 	11/14/96	Hernan  	Removed LinearTextPicker to be able to use it in other
						projects.
	18 	12/17/96	Hernan  	Fixing dumb-handler-string to deal with functions.
	19 	 2/11/97	Brian Roddy	
	20 	 2/27/97	Hernan  	
|# ;(do not edit past this line!!)
