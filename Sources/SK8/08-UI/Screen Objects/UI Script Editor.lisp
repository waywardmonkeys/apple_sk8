;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories


;;;

(in-package :UIDevelopment)



(defun UpdateHandlerEditors (NewHandler OldHandler Editor)
  (let ((hid (inputhandlerid editor))
        )
    (unless OldHandler
      (updateScriptEditor Editor NewHandler hid))
    (if (or (< (length hid) 3) (not (third hid)))
      (if (and (eq (container projectOverviewer) stage)
               (eq (targetproject ui) (first hid))
               (eq (currentTool ProjectOverviewer) POBFunctions))
        (setf (inputproject (picker POFunctionSheet)) (inputproject (picker POFunctionSheet)) ))
      (sk8dev::do-sk8-window-actors (i)
        (when (is-a i objecteditor)
          (setf (inputobjects (picker (hsheet i))) (inputobjects (picker (hsheet i))))))
      )
    ))

;;; ______________________________________________________________
;;; ______________________________________________________________
;;;              UI's version of the handler editor
;;; ______________________________________________________________
;;; ______________________________________________________________


(new ScriptEditorWindow :objectname "UIScriptEditor" :project ui)
(addparent UIScriptEditor UISimpleWindow)
(setf (zoombox UIScriptEditor) t)
(setf (resizer UIScriptEditor) t)
(setf (text UIScriptEditor) "Script Editor")
(addproperty uiscripteditor 'items)

(define-handler leavingStage (UIScriptEditor)
  (when (eq me (scriptEditor ScriptWarningsWindow))
    (clearWarnings ScriptWarningsWindow))
  (call-next-method))

;;; Reports warnings using the warnings window. 

(define-handler indicateScriptCompileWarnings ((editor UIScriptEditor) warnings)
  (call-next-method)
  (reportWarnings ScriptWarningsWindow (container me) (first (inputHandlerId me)) warnings))

(define-handler updateEditor (UIScriptEditor &key (propertyname nil) (items nil))
  (let ((handID (inputhandlerid me)))
    (when (and (third handid) (memq (third handid) items))
      (withactorlocked (me)
        (setf (text (editor me)) (ps::transform-script-for-new-object (text (editor me)) (first handid) (third handid)))
        (setf (text (textfield (nfield me))) (createtextdisplayitem (historymenu (nfield me)) (inputhandlerid me)))
        (setf (globalvariables (editor me)) nil)
        ;;;update globals????
        )))
  )

(define-handler clearReferences (UIScriptEditor &key ((:objects theobjects)))
  (when theobjects
    (if (is-a (first theobjects) project)
      (progn
        (setf (items (historymenu (nfield UIscripteditor)))
              (remove-if #'(lambda (x) (eq (first x) (first theobjects)))
                         (items (historymenu (nfield UIscripteditor)))))
        (unless (neq (first (inputhandlerid me)) (first theobjects)) (setf (container me) nil))
        (setf (items me) (delete-if #'(lambda (x) (eq (first x) (first theobjects))) (items me)))
        )
      (if (and (third (inputhandlerid me)) (memq (third (inputhandlerid me)) theobjects))
        (setf (container me) nil)))
    (let (newlist newbie)
      (setf (items (historymenu OENameField))
            (items (historymenu OENameField))) ;;; clears info will do all the work...
      (dolist (i (items uiscripteditor))
        (unless (memq (first i) theobjects)
          (setf newbie (list (first i)))
          (dolist (j (cdr i))
            (unless (memq (third (first j)) theobjects)
              (setf newbie (append newbie (list j)))))
          (setf newlist (append newlist (list newbie)))))
      (setf (items uiscripteditor) newlist))
    )
  )


(defun storeScriptEds (proj)
  (when proj
    (let* ((alleds (remove-if-not #'(lambda (x) (and (is-a x uiscripteditor) (eq (first (inputhandlerid x)) proj)))
                                            (reverse (contents stage))))
          (ScriptEds (mapcar #'(lambda (x) (list (inputhandlerid x) (boundsrect x))) alleds)))
      (setf (items uiscripteditor) (delete-if #'(lambda (x) (eq (first x) proj)) (items uiscripteditor)))
      (push (cons proj ScriptEds) (items uiscripteditor)))
    ))

(defun getScriptEds (proj)
  (let ((ScriptEds nil))
    (dolist (i (items uiscripteditor))
      (if (eq (first i) proj) 
        (setf ScriptEds (cdr i))))
    ScriptEds
    ))

(define-handler SetUpForProject (UIScriptEditor &key ((:project theproject)))
  (let ((HandlerIds (getScriptEds theproject))
        (openEds (remove-if-not #'(lambda (x) (is-a x uiscripteditor))
                                (contents stage)))
        theId
        theEd)
    (dolist (i HandlerIds)
      (setf theId (first i))
      (if openEds
        (progn
          (setf theEd (pop openeds))
          (setf (boundsrect theEd) (cadr i))
          (setf (inputhandlerid theEd) theId)
          )
        (edithandler (first theId) (cadr theId) (caddr theId) nil nil nil (cadr i))))
    (dolist (i openeds) (setf (container i) nil))
    (storescripteds theproject)
    ))

(define-handler enteringStage (UIScriptEditor)
  (storescripteds (targetproject ui))
  (call-next-method))


;;;;(storescripteds flozzum)
;;;;(getScriptEds foo)

(let ((globs (myglobals UIScriptEditor))
      (locs (mylocals UIScriptEditor))
      (scr1 (new uiverticalscroller :project ui))
      (scr2 (new uiverticalscroller :project ui)))
  
  (setf (container (myscroller globs)) nil)
  (setf (container (myscroller locs)) nil)
  (setf (container scr1) globs)
  (setf (container scr2) locs)
  (setf (partnervscroller (myField globs)) scr1)
  (setf (partnervscroller (myField locs)) scr2)
  (addparent scr1 linearPickerScroller)
  (addparent scr2 linearPickerScroller)
  (setf (myscroller globs) (partnervscroller (myField globs)))
  (setf (myscroller locs) (partnervscroller (myField locs)))
  
  (define-handler bestsize ((myField (myglobals UIScriptEditor)))
    (sk8-multival-bind (h v) (size (container me))
      (sk8-multival-bind (textH textV) (actorTextSize (container me))
        (setBoundsRect me textH 4 (- h (if *MacStyleInterface* 16 13)) (- v 2)))))
  
  (define-handler bestsize ((myscroller (myglobals UIScriptEditor)))
    (sk8-multival-bind (h v) (size (container me))
      (setBoundsRect me (- h (if *MacStyleInterface* 16 13)) 0 h v)))
  
  (define-handler bestsize ((myField (mylocals UIScriptEditor)))
    (sk8-multival-bind (h v) (size (container me))
      (sk8-multival-bind (textH textV) (actorTextSize (container me))
        (setBoundsRect me textH 4 (- h (if *MacStyleInterface* 16 13)) (- v 2)))))
  
  (define-handler bestsize ((myscroller (mylocals UIScriptEditor)))
    (sk8-multival-bind (h v) (size (container me))
      (setBoundsRect me (- h (if *MacStyleInterface* 16 13)) 0 h v)))
  
  (bestsize (myField (myglobals UIScriptEditor)))
  (bestsize (myscroller (myglobals UIScriptEditor)))
  (bestsize (myField (mylocals UIScriptEditor)))
  (bestsize (myscroller (mylocals UIScriptEditor)))

  (setf (fillcolor (myglobals UIScriptEditor)) shadowedrenderer)
  (setf (fillcolor (mylocals UIScriptEditor)) shadowedrenderer)
  (setf (texthoffset (myglobals UIScriptEditor)) 2)
  (setf (texthoffset (mylocals UIScriptEditor)) 2)
  (setf (textvoffset (myglobals UIScriptEditor)) 2)
  (setf (textvoffset (mylocals UIScriptEditor)) 2)
  (setf (fillcolor (myField (myglobals UIScriptEditor))) shadowwhite)
  (setf (fillcolor (myField (mylocals UIScriptEditor))) shadowwhite)

  )



(define-handler bestsize ((myresizer uiscripteditor))
  (moveoffstage me))

(new uiverticalscroller :objectname "UISEVScroller" :project ui)
  
(setf (container (myvscroller UIScriptEditor)) nil)
(setf (container UISEVScroller) UIScriptEditor)
(setf (myvscroller UIScriptEditor) UISEVScroller)
(setf (partnervscroller (editor UIScriptEditor)) UISEVScroller)

(new uihorizontalscroller :objectname "UISEHScroller" :project ui)
  
(setf (container (myHScroller UIScriptEditor)) nil)
(setf (container UISEHScroller) UIScriptEditor)
(setf (myhscroller UIScriptEditor) UISEHScroller)
(setf (partnerhscroller (editor UIScriptEditor)) UISEHScroller)

(define-handler resized (UIScriptEditor)
  (unless (< (height me) 45)
    (call-next-method))
  (bringtofront (buttonpanel me))
  (sk8-multival-bind (hSize vSize) (size me)
    (setf (absoluteBottom (myVarSplitter me)) (- vSize 19))
    (setf (absoluteBottom (myEditorSplitter me)) (1+ vSize))
    ))
(setf (fillcolor (buttonPanel UIScriptEditor)) uimiddle)
(setframesize (third (contents (buttonPanel UIScriptEditor))) 0 0)
(setframesize (second (contents (buttonPanel UIScriptEditor))) 0 0)
(setframesize (first (contents (buttonPanel UIScriptEditor))) 0 0)

(define-handler resized ((buttonPanel UIScriptEditor))
  (setboundsrect (third (contents me)) 0 -1 38 20)
  (setboundsrect (second (contents me)) 37 -1 75 20)
  (setboundsrect (first (contents me)) 74 -1 112 20)
  )
(dolist (i (contents (buttonpanel UIScriptEditor))) (addparent i uibutton))

(SK8-declare-syms :UI :public ; Updated 12-21-94   4:41 pm
                  UI::RDBREPLACEBUTTON UI::UISEMENUBAR)
(setf (objectname (SK8::menubar UIScriptEditor)) "UISEMenubar")
(setf (framesize UISEMenubar) '(0 0))


#|
(new menuitem :objectname "ScriptWatchingHandlerTracer" :project ui
     :text "Handler Tracer" :menu (cadr (menus UISEMenubar)))

(define-handler menuSelect (ScriptWatchingHandlerTracer)
  (bringup handlertracer))
|#

;;; (define-handler menuselect (versionmenu...) (call-next-method) (update-editors...))

(addparent UISEMenubar uimenubaractor)

;;;Get rid of duplicate menuitems....
(let ((cnt 0))
  (dolist (i (menuitems (first (menus uisemenubar))))
    (incf cnt)
    (when (>= cnt 9) (setf (menu i) nil))
    ))


(new menuitem :objectname "seSearchReplaceWindow" :project ui)
(setf (menu seSearchReplaceWindow) (first (menus uisemenubar)))
(setf (commandkey seSearchReplaceWindow) #\F)
(setf (Text seSearchReplaceWindow) "Search/Replace")
(bringcloser seSearchReplaceWindow)
(bringcloser seSearchReplaceWindow)
(define-handler menuselect (seSearchReplaceWindow)
  (let* ((win (sk8::window (sk8::menubar (menu me))))
         (editor (editor win)))
    (when editor
      (setf (editor rdbwindow) editor)
      (setf (text rdbwindow) (concatenate 'string "Search and Replace of " (pretty-symbol-name (second (inputhandlerid win)))))
      (bringup rdbwindow))))


(define-handler menuselect ((first (menuitems (second (menus uisemenubar)))))
  (let* ((editor (sk8::window (sk8::menubar (menu me))))
         (xx (inputhandler editor)))
    (when (eq editor (scriptEditor ScriptWarningsWindow))
      (clearWarnings ScriptWarningsWindow))
    (when (call-next-method)
      (UpdateHandlerEditors (inputhandler editor) xx editor))))

(dolist (i (menus uisemenubar))
  (addparent i uimenuactor))


(define-handler menuselect ((sixth (menuitems (first (menus uisemenubar)))))
  (let* ((scriptWind (sk8::window (menu me)))
         (watcher (watcher scriptWind))
         (theSplitter (myEditorSplitter scriptWind))
         watcherHeight)    
    (setq watcherHeight (round (height watcher)))
    (cond
     ((visible watcher)
      (setf (checkmark me) nil)
      (hide watcher)
      (hide theSplitter)
      (setf (keyTarget scriptWind) (editor scriptWind))
      (setf (bottom scriptWind :resizing t) (- (- (bottom scriptWind) watcherHeight) 18))
      )
     (t
      (setf (checkmark me) t)
      (update watcher)
      (show watcher)
      (setf (keyTarget scriptWind) watcher)
      (show theSplitter)
      (setf (bottom scriptWind :resizing t) (+ (+ (bottom scriptWind) watcherHeight) 18))
      (bestsize thesplitter)
      (bestsize watcher)
      ))))

;;;--------Now we add a widget and deal with the resize stuff------------

#|
(define-handler minimumSize (UIScriptEditor)
  (if (squashed (SK8::zoomBox (titlebar me)))
    (sk8-multivals (first (call-next-method)) 20)
    (call-next-method)))
|#

(setf (minimumsize UIScriptEditor) '(345 200))

(new StandardInputField :objectname "ScriptEditorInputField" :project ui)
(setf (container ScriptEditorInputField) UIScriptEditor)
(resized ScriptEditorInputField)
(tagpart UIScriptEditor ScriptEditorInputField 'nfield)
(define-handler (setf OutputObjects) (theval ScriptEditorInputField)
  (when (and (listp theval) (= (length theval) 4))
    (call-next-method)
    (setf (inputhandlerid (container me)) theval)
    ))
(define-handler menuselect ((HistoryMenu ScriptEditorInputField))
  (let* ((item (selecteditem me))
        (othereds (remove-if-not #'(lambda (x) (is-a x scripteditorwindow)) (contents stage)))
        val)
    (setf (items me) (cons item (delete item (items me))))
    (if (setf val (member item othereds :test #'equal :key #'inputhandlerid)) 
      (progn
        (bringup (first val))
        (setf (keytarget (first val)) (editor (first val))))
      (progn
        (setf (text (textfield (container me))) (sk8dev::handlerIDstring (second item) (third item) (fourth item) :project (first item)))
        (setf (outputobjects (container me)) Item)
        (setf (selecteditem me) nil)))))


(define-handler draggingMouseEnter ((textfield ScriptEditorInputField) actorDragged)
  (when (or (eq actorDragged handlerdatarect)
            (and (eq actorDragged ObjectDataRect) 
                 (not (listp (object ObjectDataRect)))
                 (is-a (object ObjectDataRect) handler)))
    (call-next-method))
  )

(define-handler dropped ((textfield ScriptEditorInputField) droppee)
  (draggingmouseleave me droppee)
  (when (and (eq droppee handlerdatarect)
             (or (is-a (handler handlerdatarect) handler)
                 (is-a (handler handlerdatarect) sk8::function)))
    (setf (inputhandler (sk8::window me)) (handler handlerdatarect))
    (bringup (sk8::window me))
    (setf (keytarget (sk8::window me)) (editor (sk8::window me)))
    )
  (when (and (eq droppee ObjectDataRect) 
             (not (listp (object ObjectDataRect)))
             (or (is-a (object ObjectDataRect) handler)
                 (is-a (object ObjectDataRect) sk8::function))
            )
    (setf (inputhandler (sk8::window me)) (object ObjectDataRect))
    (bringup (sk8::window me))
    (setf (keytarget (sk8::window me)) (editor (sk8::window me)))
    ))

(define-handler deactivateText ((textfield ScriptEditorInputField))
  (withactorlocked (me)
    (let ((xx (text me)))
      (call-next-method)
      (setf (text me) xx))))


(define-handler mousedown ((textfield ScriptEditorInputField))
  (setf (boundsrect HandlerDataRect :physical t) (boundsrect me :physical t))
  (setf (objects HandlerDataRect) (list (third (inputhandlerid (sk8::window me)))))
  (setf (handler HandlerDataRect) (inputhandler (sk8::window me)))
  (setf (ComponentFrom HandlerDataRect) me)
  (drag HandlerDataRect :live nil :onstage t :draggingMouseWithinEvents t :draggedOverEvents t :dropevents t)
  t)

(define-handler bestsize (ScriptEditorInputField)
  (let ((h (width (container me))))
    (setBoundsRect me 9 (+ 25 *WindowTop*) (- h 8) (+ 45 *WindowTop*))))

#|
(define-handler bestsize ((buttonPanel UIScriptEditor))
  (let ((h (width (container me))))
    (setBoundsRect me 230 45 h 63)))
|#

(define-handler bestsize ((buttonPanel UIScriptEditor))
  (let ((h (width (container me))))
    (bringtofront me)
    (setBoundsRect me (- h 131) 20 (- h 20) 38)))


(setf (top (mylocals UIScriptEditor) :resizing nil) 70)
(setf (top (myglobals UIScriptEditor) :resizing nil) 70)
(setf (left (myglobals UIScriptEditor)) 6)
(setf (Framesize (mylocals UIScriptEditor)) '(0 0))
(setf (Framesize (myglobals UIScriptEditor)) '(0 0))
(setf (Framesize (myfield (mylocals UIScriptEditor))) '(0 0))
(setf (Framesize (myfield (myglobals UIScriptEditor))) '(0 0))

(new sifinsetrect :objectname "SEInsetRect" :project ui)
(setf (container SEInsetRect) UIScriptEditor)
(tagpart UIScriptEditor SEInsetRect 'insetrect)
(sendtoback SEInsetRect)
(setframesize (editor uiscripteditor) 0 0)
(hide (handle scripteditorinputfield))

(defun uiwatcherVOffset (theEditorWindow)
  (let ((exprWatcher (watcher theEditorWindow)))
    (if (and exprWatcher (visible exprWatcher))
      (+ (height exprWatcher) 16)
      0)))


(define-handler bestsize ((myglobals UIScriptEditor))
  (let ((width (width (container me))))
    (setf (top me :resizing t) (+ 50 *WindowTop*))
    (setf (right me) (- (truncate width 2) 2))))
(define-handler bestsize ((myLocals UIScriptEditor))
  (let ((width (width (container me))))
    (sk8-multival-bind (ll tt rr bb) (boundsRect me)
      (setBoundsRect me (+  2 (truncate width 2)) (+ 50 *WindowTop*) (- width 6) bb))))

(define-handler bestsize (SEInsetRect)
  (sk8-multival-bind (h v) (size (container me))
    (setBoundsRect me 
                   *windowLeft*
                   (top me)
                   (- h *windowRight* (if *MacStyleInterface* 16 13))
                   (- v (if *MacStyleInterface* 16 18) (uiwatcherVOffset (container me))))))
(define-handler bestsize ((editor uiscripteditor))
  (sk8-multival-bind (h v) (size (container me))
    (setBoundsRect me 
                   (+ *windowLeft* (if *MacStyleInterface* 1 4))
                   (top me)
                   (- h *windowRight* (if *MacStyleInterface* 16 15))
                   (- v (if *MacStyleInterface* 17 21) (uiwatcherVOffset (container me))))))

(define-handler bestsize ((myVScroller uiscripteditor))
  (sk8-multival-bind (h v) (size (container me))
    (setBoundsRect me (- h *windowRight* (if *MacStyleInterface* 16 13)) (top me) (- h *windowRight*)
                   (- v (if *MacStyleInterface* 16 19) (uiwatcherVOffset (container me))))))


(define-handler bestsize ((myHScroller uiscripteditor))
  (sk8-multival-bind (h v) (size (container me))
    (let ((realBottom (- v (uiwatcherVOffset (container me)))))
      (setBoundsRect me 
                     *WindowLeft* 
                     (- realBottom (if *MacStyleInterface* 16 18))
                     (- h *windowRight* (if *MacStyleInterface* 16 13))
                     (- realBottom (if *MacStyleInterface* 0 5))))))

(defun updateScriptEditor (editor handlerobj handlerid)
  (if (null handlerid) (setf handlerid (list nil nil nil nil)))
  (destructuring-bind (proj handlerName &optional handlerObject handlerQualifier) handlerID
    (let* ((sic (nfield editor))
           (tf (textfield sic))
           (os (or (and handlerobj (objectstring handlerobj))
                   ;; Update the window's title
                   (concatenate 'string "[" (sk8dev::handlerIDstring handlerName handlerObject handlerQualifier :project proj) "]"))))
      (unless (string= os (text tf))
        (setf (text tf) os))
      (when handlerobj (addItem (historymenu sic) (list proj handlerName handlerObject handlerQualifier))))))

(define-handler prepareForNewHandler ((editor uiscripteditor) handlerID &key
                                          justCreated private locked
                                          watchingState watchExpressions
                                          returnType documentation editorInfo)
  (declare (ignore justCreated private locked
                   watchingState watchExpressions
                   returnType documentation editorInfo))
  (call-next-method)
  (updateScriptEditor (container me) (inputhandler (container me)) handlerid)
  )


(addparent (myVarSplitter UIScriptEditor) uilittlesplitter)
(setf (framesize (myVarSplitter uiscripteditor)) '(0 0))
(define-handler bestsize ((myVarSplitter uiscripteditor))
  (let ((w (width (container me))))
    (setf (left me :resizing t) (if *MacStyleInterface* 0 5))
    (setf (right me :resizing t) (- w (if *MacStyleInterface* 0 5)))))

(setf (absoluteTop (myVarSplitter uiscripteditor)) 100)
(setf (top (myVarSplitter UIScriptEditor) :resizing nil) 
      (bottom (myglobals uiscripteditor)))
(setf (bottom (myVarSplitter uiscripteditor)) (+ (top (myVarSplitter uiscripteditor)) 13))
(setf (top (insetrect uiscripteditor) :resizing t) 
      (bottom (myVarSplitter uiscripteditor)))
(setf (top (editor uiscripteditor) :resizing t) 
      (+ 4 (bottom (myVarSplitter uiscripteditor))))
(setf (top (myVScroller uiscripteditor) :resizing t) 
      (bottom (myVarSplitter uiscripteditor)))
(locktext (textfield (nfield UIscripteditor)))
(define-handler mousedown ((handle (nfield UIscripteditor)))
  )

(define-handler update ((historymenu (nfield UIscripteditor)))
  (call-next-method)
  (dolist (i (menuitems me))
    (unless (memq (first (item i)) (okprojects (targetproject ui)))
      (setf (menu i) nil)))
  )

(define-handler createtextdisplayitem ((historymenu (nfield UIscripteditor)) theval)
  (if (and theval (listp theval) (= (length theval) 4))
    (sk8dev::handlerIDstring (second theval) (third theval) (fourth theval) :project (first theval))
    "Huh?!?!  Report as a bug that this item was in the menu!")
  )

(define-handler (setf items) (theval (historymenu (nfield UIscripteditor)))
  (if (eq me (historymenu (nfield UIscripteditor)))
    (progn
      (storescripteds (targetproject ui))
      (sk8::setValue 'items me 
                     (remove-duplicates 
                      (delete-if-not #'(lambda (x) 
                                         (and x 
                                              (listp x) 
                                              (= (length x) 4)
                                              (not (and (second x)
                                                        (memq (second x) (uiClearedObjects))))))
                                     theval)
                      :test #'equal)))
    (progn
      ; (call-next-method)
      (setf (items (historymenu (nfield UIscripteditor))) theval)
      (setf (slot-value me 'items) nil)
      )))

(define-handler items ((historymenu (nfield UIscripteditor)))
  (if (eq me (historymenu (nfield UIscripteditor)))
    (progn
      (sk8::setValue 'items me 
                     (delete-if-not #'(lambda (x) 
                                        (and x 
                                             (listp x) 
                                             (= (length x) 4)
                                             (not (and (third x)
                                                       (memq (third x) (uiClearedObjects))))))
                                    (sk8::getValue 'items me)))
      (sk8::getValue 'items me))
    (items (historymenu (nfield UIscripteditor)))))


(sk8dev::do-watcher-toggle uiscripteditor)
(sk8dev::do-watcher-toggle uiscripteditor)
(tagpart uiscripteditor (watcher uiscripteditor) 'watcher)
(setf (objectname (watcher UIScriptEditor)) "UIWatcher")
(hide (resizer (watcher UIScriptEditor)))
;;(setf (items (historymenu (nfield UIscripteditor))) nil)

(setf (fillcolor UIWatcher) red)
(setframesize UIWatcher 0 0)
(setframesize (myeditorsplitter UIScriptEditor) 0 0)
(addparent UIWatcher uitextlistforcorners)
(setf (titlebar uiwatcher) uiwatcher)

(new uiverticalscroller :objectname "UIWatcherScroller" :project ui)
(setf (container (scroller UIWatcher)) nil)
(setf (container UIWatcherScroller) UIWatcher)
(setf (partnervscroller (picker UIWatcher)) UIWatcherScroller)
(setf (scroller UIWatcher) (partnervscroller (picker UIWatcher)))

(new sifinsetrect :objectname "UIWInsetRect" :project ui)
(setf (container UIWInsetRect) UIWatcher)
(tagpart UIWatcher UIWInsetRect 'insetrect)
(sendtoback UIWInsetRect)
(setframesize (expressionField UIWatcher) 0 0)
(setframesize (picker UIWatcher) 1 1)

(setf (fillcolor (valTitleLabel UIWatcher)) uimiddle)
(setf (fillcolor (exprTitleLabel UIWatcher)) uimiddle)
(setframesize (valTitleLabel UIWatcher) 1 1)
(setframesize (exprTitleLabel UIWatcher) 1 1)
(setf (framecolor (valTitleLabel UIWatcher)) black)
(setf (framecolor (exprTitleLabel UIWatcher)) black)

(define-handler resized (UIWatcher)
  (SK8-multival-bind (width height) (size me)
    (let* ((titleHeight 17)
           (scrollerWidth (if *MacStyleInterface* 16 13))
           (growBoxHeight (if *MacStyleInterface* 16 13))
           (growBoxTop (- height growBoxHeight))
           (scrollerLeft (1+ (- width scrollerWidth)))
           (vsplitterTop (round (* (vsplitterLocation me) height)))
           (hsplitterLeft (round (* (hsplitterLocation me) width))))
      
      ;;; Fix the absolute bottom of the splitter!
      (setf (absoluteBottom (vSplitter me)) height)
      (withActorLocked (me)
        (setBoundsRect (exprTitleLabel me) 0 0 20 titleHeight)
        (setBoundsRect (valTitleLabel me) 20 0 width titleHeight)
        (setBoundsRect (picker me) 0 (1- titleHeight) scrollerLeft (1+ vsplitterTop))
        
        (setBoundsRect (hsplitter me) hsplitterLeft (1- titleHeight) (+ 2 hSplitterLeft) vsplitterTop)
        (moved (hsplitter me)) ; *** WHY ISN'T THIS GETTING CALLED AUTOMATICALLY?!
        
        ;; Adam: you might want to preserve the vertical position of everything above the splitter...
        (setBoundsRect (vsplitter me) 0 vsplitterTop width (+ vsplitterTop 2))
        (setBoundsRect (insetrect me) 0 (1+ vsplitterTop) scrollerLeft height)
        (setBoundsRect (expressionField me) 4 (+ vsplitterTop 5) (- scrollerLeft 2) (- height 2))
        (setBoundsRect (scroller me) (1- scrollerLeft) (1- titleHeight) width (1+ growBoxTop))
        (setBoundsRect (resizer me) (1- scrollerLeft) growBoxTop width height)


        ))))

(define-handler initialize (UIWatcher original isNew initArgs)
  (declare (ignore original isNew initArgs))
  (call-next-method)
  (let ((actorsAtBottom (list (insetrect me) (ExpressionField me)))
        (absoluteBottom (height me))
        (absoluteTop 20)
        (actorsAbove (list (picker me) (hsplitter me)))
        (actorsAffected (list (picker me) (hsplitter me) (insetrect me) (ExpressionField me))))
    (splitActors (vSplitter me) actorsAbove actorsAtBottom ActorsAffected
                 absoluteTop absoluteBottom)))


(define-handler bestSize (UIWatcher)
  (when (visible me)
    (SK8-multival-bind (width height) (size (container me))
      (setBoundsRect me *windowLeft* (+ 14 (bottom (myhscroller (container me))))
                     (- width *windowLeft*) (- height *windowbottom*)))))


(define-handler bestsize ((myeditorsplitter UIscripteditor))
  (let* ((theWindow (container me))
         (theWatcher (watcher theWindow))
         top
         h v)
    (sk8-multival-setf (h v) (size theWindow))
    (setf top (+ (bottom (myhscroller thewindow)) 0))
    (if theWatcher
      (setBoundsRect me (if *MacStyleInterface* 0 5) top 
                     (- h (if *MacStyleInterface* 0 5)) (+ top 13 ))
      (setf (right me) h))))

(addparent (myeditorsplitter UIscripteditor) uilittlesplitter)


(bringtofront (SK8::menubar UIscripteditor))

(setf (top UIscripteditor :resizing nil) 100)
(setf (left UIscripteditor :resizing nil) 100)


(define-handler initialize (UIscripteditor original isNew initArgs)
  (declare (ignore original isNew initArgs))
  ;; [0] Copy the tags...
  (call-next-method)
  (setf (growboxrect me) (first (remove-if-not #'(lambda (x) (is-a x UIWindowGrower)) (contents me))))
  (setf (top (myVarSplitter me) :resizing nil) 
        (bottom (myglobals me)))
  (setf (bottom (myVarSplitter me)) (+ (top (myVarSplitter me)) 13))
  (setf (top (insetrect me) :resizing t) 
        (bottom (myVarSplitter me)))
  (setf (top (editor me) :resizing t) 
        (+ 4 (bottom (myVarSplitter me))))
  (setf (top (myVScroller me) :resizing t) 
        (bottom (myVarSplitter me)))
  (bestsize (nfield me))
  (let ((actorsAbove (list (myGlobals me) (myLocals me)))
        (actorsBelow (list (insetrect me) (editor me) (myVScroller me)))
        (actorsAffected (list (myGlobals me) (myLocals me) (insetrect me)
                              (editor me) (myVScroller me)))
        (absoluteTop 100)
        (absoluteBottom 258))
    (splitActors (myVarSplitter me) actorsAbove actorsBelow
                 actorsAffected absoluteTop absoluteBottom))
  (let ((actorsAbove (list (editor me) (myvScroller me) (insetrect me)))
        (actorsBelow (list (watcher me)))
        (actorsAffected (list (editor me) (myvScroller me) (insetrect me)
                              (myHScroller me) 
                              (watcher me)))
        (absoluteTop (top (editor me)))
        (absoluteBottom (bottom (watcher me))))
    (splitActors (myeditorsplitter me) actorsAbove actorsBelow
                 actorsAffected absoluteTop absoluteBottom))  )


(resetLayout UIScriptEditor)
(resized UIScriptEditor)


;;; Highlighting the fields...
(define-handler (setf Highlight) (theval (editor uiscripteditor))
  (setf (inverts me) nil)
  (if theval
    (setf (fillcolor me) white)
    (setf (fillcolor me) shadowwhite)
    )
  (call-next-method))

(define-handler (setf Highlight) (theval (expressionfield (watcher uiscripteditor)))
  (setf (inverts me) nil)
  (if theval
    (setf (fillcolor me) white)
    (setf (fillcolor me) shadowwhite)
    )
  (call-next-method))



#|
(setf ui::UIScriptEditor sk8dev::UIScriptEditor)
(setf (objectname *) "UIScriptEditor")

|#
;;; 2 editors to be cached for users convenience! (what?)

(let ((theEditor (new UIscripteditor :project ui)))
  (discard theEditor))

(let ((theEditor (new UIscripteditor :project ui)))
  (discard theEditor))

(setf sk8::ScriptEditorPrototype ui::UIscripteditor)


;;;;____________________________________________________________________________________________
;;;;____________________________________________________________________________________________
;;;;____________________________________________________________________________________________
;;;;____________________________________________________________________________________________

(new UISimpleWindow :objectname "RDBWindow" :project UI
     :properties '((Editor :value nil)))
(setf (doFirstClick RDBWindow) t)
(setf (sk8::menubar RDBWindow) nil)
(setf (resizer RDBWindow) nil)
(setf (zoombox RDBWindow) nil)
(setf (text RDBWindow) "Search and Replace")
(setlocation RDBWindow 333 205)
(setSize RDBWindow 367 120)

(new uiBigLabel :project UI :container RDBWindow :objectname "RDBSearchLabel" :text "Search For")
(setf (top RDBSearchLabel :resizing nil) 32)
(setf (left RDBSearchLabel :resizing nil) 8)
(new uiBigLabel :project UI :container RDBWindow :objectname "RDBReplaceLabel" :text "Replace With")
(setf (top RDBReplaceLabel :resizing nil) 55)
(setf (left RDBReplaceLabel :resizing nil) 8)

(new edittext :project UI :container RDBWindow  :textfont espysansfont :textsize 9
     :objectName "RDBSearchField")
(setBoundsRect RDBSearchField 90 30 355 50)
(setf (autotab RDBSearchField) t)
(setf (highlightcolor RDBSearchField) uilightcolor)
(setf (framecolor RDBSearchField) framescore)
(setf (framesize RDBSearchField) '(2 2))
(tagpart RDBWindow RDBSearchField 'searchfield)

(define-handler keyDown (RDBSearchField thechar)
  (if (or (eq theChar #\Enter) (eq theChar #\Return))
    (click RDBReplaceButton)
    (call-next-method)))

(new edittext :project UI :container RDBWindow  :textfont espysansfont :textsize 9
     :objectName "RDBReplaceField")
(setBoundsRect RDBReplaceField 90 53 355 73)
(setf (autotab RDBReplaceField) t)
(setf (highlightcolor RDBReplaceField) uilightcolor)
(setf (framecolor RDBReplaceField) framescore)
(setf (framesize RDBReplaceField) '(2 2))
(tagpart RDBWindow RDBReplaceField 'ReplaceField)

(define-handler keyDown (RDBReplaceField thechar)
  (if (or (eq theChar #\Enter) (eq theChar #\Return))
    (click RDBReplaceButton)
    (call-next-method)))

(define-handler activatetext (RDBSearchField)
  (call-next-method)
  (setselection me 0 -1)
  )
(define-handler activatetext (RDBReplaceField)
  (call-next-method)
  (setselection me 0 -1)
  )

(new UIButton :objectName "RDBSearchButton" :project UI)
(setf (container RDBSearchButton) RDBWindow)
(setf (text RDBSearchButton) "Search")
(setBoundsRect RDBSearchButton 100 82 170 103)
(define-handler click (RDBSearchButton)
  (let ((ed (editor (UI::window me)))
        (txt (text (searchfield (UI::window me))))
        val)
    (when ed
      (setf val (UI::search ed txt :beepIfNotFound t :inselection nil :casesensitive nil))
      (when val
        (setselection ed val (+ val (length txt)))
        )
      (lightforceredraw ed)
      )))

(new UIButton :objectName "RDBReplaceSearchButton" :project UI)
(setf (container RDBReplaceSearchButton) RDBWindow)
(setf (text RDBReplaceSearchButton) "Replace/Search")
(setBoundsRect RDBReplaceSearchButton 180 82 270 103)
(define-handler click (RDBReplaceSearchButton)
  (let ((ed (editor (UI::window me)))
        (txt (text (searchfield (UI::window me))))
        (txt2 (text (replacefield (UI::window me))))
        val)
    (when (and ed text txt2)
      (UI::replace ed txt2 :searchstring txt :beepIfNotFound t :inselection t :casesensitive nil)
      (setf val (UI::search ed txt :beepIfNotFound nil :inselection nil :casesensitive nil))
      (when val
        (setselection ed val (+ val (length txt)))
        )
      (lightforceredraw ed)
      )))

(new roundrect :objectname "RDBReplaceButtonHighlight" :project UI)
(setf (container RDBReplaceButtonHighlight) RDBWindow)
(setf (framesize RDBReplaceButtonHighlight) '(2 2))
(setf (fillcolor RDBReplaceButtonHighlight) transparent)
(setf (mousesensitivity RDBReplaceButtonHighlight) 'transparent)
(setf (roundedness RDBReplaceButtonHighlight) '(11 11))
(setBoundsRect RDBReplaceButtonHighlight 277 79 352 103)

(new UIButton :objectname "RDBReplaceButton" :project UI)
(setf (container RDBReplaceButton) RDBWindow)
(setf (text RDBReplaceButton) "Replace All")
(setBoundsRect RDBReplaceButton 280 82 353 103)
(define-handler click (RDBReplaceButton)
  (let ((ed (editor (UI::window me)))
        (txt (text (searchfield (UI::window me))))
        (txt2 (text (replacefield (UI::window me))))
        )
    (when (and ed text txt2)
      (UI::replace ed txt2 :searchstring txt :beepIfNotFound nil :allOccurrences t :casesensitive nil)
      (lightforceredraw ed)
      )))

(define-handler EnteringStage (RDBWindow)
  (call-next-method)
  (setf (keyTarget  me) RDBSearchField)
  (setSelection RDBSearchField 0 -1)
  )

(bringtofront RDBReplaceButtonHighlight)

;;(setf (container RDBWindow) stage)
;;(setf (Editor RDBWindow) foo::julio)


#|
	Change History (most recent last):
	1	3/3/94	rod	Welcome, my son, welcome...
	2	3/4/94	rod	
	3	3/4/94	rod	
	4	3/7/94	rod	
	5	3/9/94	rod	Doing Project Switching and Reference Clearing.
	6	3/10/94	rod	
	7	3/10/94	rod	
	8	3/17/94	rod	
	9	3/21/94	rod	
	10	3/21/94	kleiman	setvalue -> sk8::setValue
	11	3/26/94	rod	Minimum size stuff.
	12	3/28/94	rod	Added some robustness to the history menu.
	13	3/29/94	rod	Fixing various bugs with top bar.
	14	3/30/94	rod	Making sure handlers don't always go away.
	15	3/30/94	rod	
	16	4/1/94	rod	Now having a global variable to specify the editor prototype.
	17	4/11/94	rod	New UI Stuff
	18	4/11/94	Brian	
	19	4/13/94	Hernan	Avoiding use of contents when possible.
	20	4/15/94	rod	
	21	4/29/94	rod	
	22	4/29/94	rod	
	23	4/29/94	rod	
	24	4/29/94	rod	
	25	5/2/94	sidney	
	26	5/2/94	rod	Making the toggler for the watcher a menuitem.
	28	5/9/94	rod	
	29	5/22/94	rod	Here begins the experimentation with a Mac look 
				to the UI.
	30	6/3/94	rod	
	30	6/3/94	rod	
	31	6/3/94	Brian	
	32	6/4/94	rod	Fixing the history menu so it can deal with
				functions and handlers.  It now stores the 
				handlerID of the viewed handler.   Note that this
				is a list of four items.
	33	6/23/94	rod	Got rid of tablepickerscroller.
	34	7/6/94	rod	Getting rid of UIColorize.
	35	7/15/94	rod	
	36	7/15/94	rod	Now the Script Editor handles project switching
				a lot more nicely.
	37	7/19/94	chip	the "activate" menuitem's menuselect handler now only updates the handler editors if the activate succeeds
	38 	 9/13/94	rod     	
	39 	 9/14/94	rod     	Fixing growbox.
	40 	10/18/94	rod     	improved menu select to not allow duplicate handler editors.
	41 	12/ 5/94	rod     	various graphic fixes.
	42 	12/21/94	rod     	
	43 	 1/25/95	rod     	Fixing problem where children editors could hold
							references to cleared objects.
	44 	 1/25/95	rod     	
	45 	 1/25/95	rod     	Making storeScriptEds clear properly.
	46 	 2/16/95	sidney  	readable argument names for initialize handler
	47 	 2/20/95	rod     	
	2  	 1/19/96	sidney  	remove references to sk8script compiler internal functions
	3  	 1/25/96	Hernan  	Making the editor bring up the warnings window when 
						compiler warnings.
	4  	 1/26/96	Hernan  	When an editor window goes away, it takes the associated
						warnings window with it.
	5  	 2/14/96	Brian   	Cleaning up our need for a population.
	2  	 4/16/96	brian   	removing (nthitem call.
	3  	 5/23/96	Brian   	
	4  	 9/ 3/96	Brian   	Fixing bugs with menus and the drag zone.
	5  	10/11/96	Brian   	Adding warnings.
	6  	10/11/96	Brian   	Making recompile clear the warnings window.
	7  	10/17/96	Hernan  	Fixed update script editor for destructuring bind.
	8  	12/12/96	Hernan  	Need to recompile do-sk8-window-actors.
	9  	 2/27/97	Hernan  	
|# ;(do not edit past this line!!)
