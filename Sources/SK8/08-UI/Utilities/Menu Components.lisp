;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories



(in-package :sk8development)

(SK8-declare-syms :SK8 :public ; Updated  3-06-94   3:09 pm
                  SK8::EDITHANDLERMENUITEM SK8::HANDLERNAME SK8::MBAREDMENU)


;;--------------------------------------------------------------------------------------------
;;; MenuEditor Components 
;;--------------------------------------------------------------------------------------------


(new menuitem :objectname "EditHandlerMenuItem" :project sk8)
(addproperty EditHandlerMenuItem 'HandlerName)
(addproperty EditHandlerMenuItem 'object)
(define-handler menuselect (EditHandlerMenuItem)
  (when (object me)
      (edithandlerdialog (object me) :handlername (handlername me))))
(define-handler update (EditHandlerMenuItem)
  (setf (enabled me) (and (object me) (handlername me)))
  (setf (text me)
        (if (object me)
          (concatenate 'string "Edit " (simpleobjectstring (handlername me)) " of " (objectstring (object me)))
          (concatenate 'string "Edit " (simpleobjectstring (handlername me)) ))))




;;;;---------------------------------------------------------------------------------------------------------------------------
;;;;---------------------------------------------------------------------------------------------------------------------------
;;;;  MenuBarEditorPicker  Allows editing of menubars and their menus...
;;;;---------------------------------------------------------------------------------------------------------------------------
;;;;---------------------------------------------------------------------------------------------------------------------------

(new tablepicker :objectname "MenuBarEditorPicker" :project sk8
     :properties '(inputMenubar
                   outputMenu 
                   (editing :inherit :value t)))
(addparent MenuBarEditorPicker BrowserComponent)
(setf (EditorIncluded MenuBarEditorPicker) t)
(setf (textsize MenuBarEditorPicker) 12)
(setf (textfont MenuBarEditorPicker) "Chicago")
(setf (selectionstyle MenuBarEditorPicker) 'single) ;; HEW
(setf (rowspacing MenuBarEditorPicker) 6)
(setf (rowlinessize MenuBarEditorPicker) 0)
(setf (columnlinessize MenuBarEditorPicker) 0)

(setf (autotab MenuBarEditorPicker) t)

(addInputPort MenuBarEditorPicker 'inputMenubar :signature SK8::Menubar)
(addOutputPort MenuBarEditorPicker 'outputMenu :signature Menu)



;;;;________________________MENUS___________________________________

(new menu :objectname "MBarEdMenuBar" :project sk8
     :Properties '(BrowserComponent))
(setf (text MBarEdMenuBar) "Menubar")

(new menuitem :objectname "MBarEdMenuBarNew" :menu MBarEdMenuBar :project sk8)
(setf (text MBarEdMenuBarNew) "New Menubar")
(define-handler menuselect (MBarEdMenuBarNew)
  (setf (inputmenubar (browsercomponent (menu me))) 
        (getnewfromuser sk8::menubar :project (targetproject (browsercomponent (menu me)))))
  (setf (keytarget (sk8::window (browsercomponent (menu me)))) (browsercomponent (menu me))))

(new menuitem :objectname "MBarEdMenuBarDispose" :menu MBarEdMenuBar :project sk8)
(define-handler menuselect (MBarEdMenuBarDispose)
  (let ((men (inputmenubar (browsercomponent (menu me)))))
    (when (disposeDialog men)
      (dolist (i (menus men))
        (setf (sk8::menubar i) nil))
      (setf (inputmenubar (browsercomponent (menu me))) nil)  )))
(define-handler update (MBarEdMenuBarDispose)
  (setf (enabled me) (inputmenubar (browsercomponent (menu me))))
  (setf (text me)
        (if (inputmenubar (browsercomponent (menu me)))
          (concatenate 'string "Clear References to " (objectstring (inputmenubar (browsercomponent (menu me)))))
          "Clear References")))
(new menuspacer :objectname "MBarEdMenuBarSpace" :menu MBarEdMenuBar :project sk8)
(new EditHandlerMenuItem :objectname "MBarEdMenuBarUpdate" :menu MBarEdMenuBar :project sk8)
(define-handler object (MBarEdMenuBarUpdate)
  (inputmenubar (browsercomponent (menu me))))
(setf (handlername MBarEdMenuBarUpdate) 'Update)
(new EditHandlerMenuItem :objectname "MBarEdMenuBarMenuSelect" :menu MBarEdMenuBar :project sk8)
(define-handler object (MBarEdMenuBarMenuSelect)
  (inputmenubar (browsercomponent (menu me))))
(setf (handlername MBarEdMenuBarMenuSelect) 'MenuSelect)

(new menu :objectname "MBarEdMenu" :project sk8
     :Properties '(BrowserComponent MenuPrototype))
(setf (text MBarEdMenu) "Menu")
(define-handler update (MBarEdMenu)
  (let ((theval (and (BrowserComponent me) 
                     (inheritsfrom (BrowserComponent me) MenuBarEditorPicker)
                     (editing (BrowserComponent me))
                     (inputmenubar (BrowserComponent me))
                     )))
    (dolist (i (menuitems me))
      (setf (enabled i) theval)))
  (call-next-method))
(new menuitem :objectname "MBarEdMenuNew" :menu MBarEdMenu :project sk8)
(define-handler menuselect (MBarEdMenuNew)
  (let* ((bc (browsercomponent (menu me)))
         (proto (or (MenuPrototype (menu me)) menu)) 
         (im (InputMenubar bc))
         layer
         newbie)
    (when proto
      (setf layer (selection bc))
      (when layer (setf layer (caar layer)))
      (setf newbie (new proto :project (project im)))
      (when newbie
        (withActorLocked (bc)
          (setf (sk8::menubar newbie) im)
          (setf (text newbie) "Untitled")
          (when layer (setf (layer newbie) (1+ layer)))
          (setf (InputMenubar bc) im)
          (setf (selecteditems bc) (list newbie))
          (setf (keytarget (sk8::window bc)) bc)
          (selectioncompleted bc))))
    ))
(define-handler update (MBarEdMenuNew)
  (let ((proto (or (MenuPrototype (menu me)) menu)))
    (if (eq (keytarget (sk8::window (browsercomponent (menu me)))) (browsercomponent (menu me))) 
      (setf (commandkey me) #\N)
      (setf (commandkey me) nil))
    (setf (text me)
          (concatenate 'string "Add New " (objectstring proto) " to Menubar"))))
(new menuitem :objectname "MBarEdMenuDispose" :menu MBarEdMenu :project sk8)
(define-handler menuselect (MBarEdMenuDispose)
  (keydown (BrowserComponent (menu me)) #\delete))
(define-handler update (MBarEdMenuDispose)
  (setf (enabled me) (selecteditems (browsercomponent (menu me))))
  (setf (text me)
        (if (car (selecteditems (BrowserComponent (menu me))))
          (concatenate 'string "Clear References to " (objectstring (car (selecteditems (BrowserComponent (menu me))))))
          "Clear References")))
(new menuspacer :objectname "MBarEdMenuSpace" :menu MBarEdMenu :project sk8)
(new EditHandlerMenuItem :objectname "MBarEdMenuUpdate" :menu MBarEdMenu :project sk8)
(define-handler object (MBarEdMenuUpdate)
  (car (selecteditems (browsercomponent (menu me)))))
(setf (handlername MBarEdMenuUpdate) 'Update)
(new EditHandlerMenuItem :objectname "MBarEdMenuMenuSelect" :menu MBarEdMenu :project sk8)
(define-handler object (MBarEdMenuMenuSelect)
  (car (selecteditems (browsercomponent (menu me)))))
(setf (handlername MBarEdMenuMenuSelect) 'MenuSelect)


(define-handler menuprototype (MenuBarEditorPicker)
  (list MBarEdMenuBar MBarEdMenu))

(define-handler createDisplayItem (MenuBarEditorPicker theItem)
  (createTextDisplayItem me theItem))


(define-handler (setf InputMenubar) (theval MenuBarEditorPicker)
  (let ((sel (cadr (selecteditems me))))
    (setf (slot-value me 'InputMenubar) theval)
    (tickeventclock)
    (setf (items me) (and theval (menus theval) (list (menus theval))))
    (if (memq sel (and theval (menus theval)))
      (setf (selecteditems me) (list sel))
      (if theval
        (setf (selecteditems me) (list (car (menus theval))))
        (setf (selecteditems me) nil)))
    (selectioncompleted me)
    ))


(define-handler doubleclick (MenuBarEditorPicker)
  (when (and (items me) (eq (eventactor) me))
    (keydown me #\return)))


(define-handler keydown (MenuBarEditorPicker thechar)
  (cond
#|
   ((and (items me) (or (eq theChar #\enter) (eq theChar #\Return)))
    (let* ((theselection (selection me))
           (colselected (and theselection (caar theselection)))
           (thevaluetext (aref (imagearray me) (1- colselected) 0  )))
      (withactorlocked (me)
        (setf (framecolor (editor me)) yellow)
        (DisplayEditor me :indices (list colselected 1) :selectedText t :defaulttext thevaluetext)
        (forceredraw me))))
|#
   ((and (items me) (eq theChar #\Delete))
    (let* ((theselection (selection me))
           (colselected (and theselection (caar theselection)))
           (thevalue (aref (items me) (1- colselected) 0  )))
      (when (DisposeDialog thevalue)
        (withactorlocked (me)
          (setf (menu thevalue) nil)
          (setf (inputMenuBar me) (inputMenuBar me))
          (when (items me) (setf (selection me) (list (list (max 1 (1- colselected)) 1))))
          (forceredraw me)
          ))))
   (t (moveoffstage (editor me)) (call-next-method)))
  )

(define-handler mousedown (MenuBarEditorPicker) (moveoffstage (editor me)) (call-next-method))

(define-handler createTextDisplayItem (MenuBarEditorPicker theitem)
  (if (inheritsfrom theitem menu)
    (text theitem)
    "ERROR: NOT A MENU!!!!"))

(define-handler selectionCompleted (MenuBarEditorPicker)
  (call-next-method)
  (when (selection me) (showselection me))
  (setf (outputmenu me) (car (selecteditems me))))

(define-handler keydown ((editor MenuBarEditorPicker) thechar)
  (call-next-method)
  (setf (aref (imagearray (container me)) (1- (car (edititem me))) (1- (cadr (edititem me)))) (text me))
  (computecolumnwidths (container me))
  (unless (or (eq thechar #\Return) (eq thechar #\enter))
    (setf (boundsrect me) (itemboundsrect (container me) (edititem me)))
    ))

(define-handler ReturnAction (MenuBarEditorPicker str ind)
  (let* ((theselection (selection me))
         (colselected (and theselection (caar theselection)))
         (theitem (aref (items me) (1- colselected) 0))
         (thevaluetext (text (editor me)))
         )
    (setf (text theitem) thevaluetext)
    (setf (aref (imagearray me) (1- colselected) 0) thevaluetext)
    (recomputesizes me)))
(define-handler EscapeAction (MenuBarEditorPicker)
  )

(define-handler ExtendedMouseDown (MenuBarEditorPicker)
  (setf (boundsrect ObjectDataRect :physical t) (itemboundsrect me (car (selection me)) :physical t))
  (setf (object ObjectDataRect) (car (selecteditems me)))
  (withcursor standardcursor
    (drag ObjectDataRect :live nil :onstage t :draggingMouseWithinEvents t :draggedOverEvents t :dropevents t))
  t)

(define-handler draggingMouseEnter (MenuBarEditorPicker actorDragged)
  (when (and (eq actorDragged ObjectDataRect)
             (object objectdatarect)
             (or (inheritsfrom (object actorDragged) sk8::menubar)
                 (inheritsfrom (object actorDragged) Menu)  
                 (inheritsfrom (object actorDragged) MenuItem)
                 (and (listp (object actorDragged))
                      (every #'(lambda (x) (is-a x menu)) (object actorDragged)))
                 (and (listp (object actorDragged))
                      (every #'(lambda (x) (is-a x menuitem)) (object actorDragged)))))
    (setf (highlight me) t)))

(define-handler draggingMouseLeave (MenuBarEditorPicker actorDragged)
  (setf (highlight me) nil))

(define-handler draggingMouseWithin (MenuBarEditorPicker actorDragged)
  (when (and (eq actorDragged ObjectDataRect) (object objectdatarect))
    (cond
     ((or (inheritsfrom (object actorDragged) menu)
          (and (listp (object actorDragged))
               (every #'(lambda (x) (is-a x menu)) (object actorDragged))))
      (flashline me :orientation :horizontal))
     ((or (inheritsfrom (object actorDragged) MenuItem)
          (and (listp (object actorDragged))
               (every #'(lambda (x) (is-a x menuitem)) (object actorDragged))))
      (flashItem me)))))

(define-handler (setf horizontalScroll) (theval MenuBarEditorPicker)
  (call-next-method 0 me))

(define-handler dropped (MenuBarEditorPicker droppee)
  (withActorLocked (me)
    (draggingMouseLeave me droppee)  
    (when (and (eq droppee ObjectDataRect) (object objectdatarect))
      (let ((val (object droppee))
            (theguy (getitemposition me))
            NewLayer)
        (unless (listp val) (setf val (list val)))
        (cond
         ((is-a (car val) sk8::menubar)
          (setf (inputmenubar me) (object droppee)))
         ((every #'(lambda (x) (is-a x menu)) val)
          (when theguy
            (bringup (sk8::window me))
            (setf (keytarget (sk8::window me)) me)
            (setf NewLayer (first theguy))
            (dolist (i val)
              (if (and (eq (sk8::menubar i) (inputmenubar me))
                       (< (layer i) newlayer))
                (decf newlayer))
              (setf (sk8::menubar i) (inputmenubar me))
              (setf (layer i) NewLayer))
            (setf (inputmenubar me) (inputmenubar me))
            (setf (selecteditems me) val)
            (selectioncompleted me)
            ))
         ((every #'(lambda (x) (is-a x menuitem)) val)
          (when theguy
            (bringup (sk8::window me))
            (setf (keytarget (sk8::window me)) me)
            (setf (first theguy) (gs:range 1 (first theguy) (columns me)))
            (setf (second theguy) (gs:range 1 (second theguy) (rows me)))
            (mapcar #'(lambda (x) (setf (menu x) (element me theguy))) val)
            (setf (selection me) theguy)
            (forceredraw me)
            (selectioncompleted me)
            (when (container me)
              (dolist (i (deepcontents (sk8::window me)))
                (when (and (inheritsfrom i MenuEditorPicker) (eq (inputmenu i) (element me theguy)))
                  (setf (selecteditems i) val)
                  (selectioncompleted i))))
            )))))))

;;;;---------------------------------------------------------------------------------------------------------------------------
;;;;---------------------------------------------------------------------------------------------------------------------------
;;;;  MenuEditorPicker  Allows editing of menus and their menuitems...
;;;;---------------------------------------------------------------------------------------------------------------------------
;;;;---------------------------------------------------------------------------------------------------------------------------

(new tablepicker :objectname "MenuEditorPicker" :project sk8
     :properties '(InputMenu
                   OutputMenuItem 
                   (Editing :inherit :value t)))
(addparent MenuEditorPicker BrowserComponent)

(setf (EditorIncluded MenuEditorPicker) t)
(setf (selectbyrows MenuEditorPicker) t)
(setf (textsize MenuEditorPicker) 12)
(setf (textfont MenuEditorPicker) "Chicago")
(setf (selectionstyle MenuEditorPicker) 'multiple) ;; HEW
(setf (rowspacing MenuEditorPicker) 6)
(setf (columnSpacing MenuEditorPicker) 10)
(setf (rowlinessize MenuEditorPicker) 1)
(setf (columnlinessize MenuEditorPicker) 1)

;(addparent MenuEditorPicker BrowserComponent)
(setf (autotab MenuEditorPicker) t)

(addInputPort MenuEditorPicker 'InputMenu :signature Menu)
(addOutputPort MenuEditorPicker 'OutputMenuItem :signature Menuitem)



(define-handler (setf horizontalScroll) (theval MenuEditorPicker)
  (call-next-method 0 me))

(define-handler (setf columnwidths) (theval MenuEditorPicker)
  (call-next-method ))

(define-handler resized (MenuEditorPicker)
  (let* ((ww (width me)))
    (call-next-method)
    (setf (columnwidths me) (list 15 (round (max 1 (- ww 60))) 15))
    (showselection me)))

(define-handler (setf items) (theval MenuEditorPicker)
  (sk8dev::withLockedCursor animatedClock
    (if (null theval)
      (call-next-method)
      (let* (
             (i 0))
        (tickeventclock)
        (setupitems me :dimensions (list 3 (length theval)))
        (SetupSelectionArray me)
        (tickeventclock)
        (setf (cl::slot-value me 'imageArray) (new array :dimensions (dimensions me)))
        (dolist (j theval)
          (setf (aref (items me) 0 i) (checkmark j))
          (setf (aref (items me) 1 i) j)
          (setf (aref (items me) 2 i) (commandkey j))
          (setf (aref (imagearray me) 0 i) (if (checkmark j) (string (checkmark j)) ""))
          (setf (aref (imagearray me) 1 i) (text j))
          (setf (aref (imagearray me) 2 i) (if (is-a j menu)
                                             ">"
                                             (if (commandkey j) (string (commandkey j)) "")))
          (incf i))
        (tickeventclock)
        (ComputeRowHeights me)
        (resized me)
        (setf (verticalscroll me) 0)
        ))))
(resized MenuEditorPicker)
(setf (selectionborder MenuEditorPicker) 0)





(new menu :objectname "MenuForMenuEditorPickers" :project sk8
     :Properties '(BrowserComponent MenuItemPrototype))
(setf (text MenuForMenuEditorPickers) "Item")
(define-handler update (MenuForMenuEditorPickers)
  (let ((theval (and (BrowserComponent me) 
                     (inheritsfrom (BrowserComponent me) MenuEditorPicker)
                     (editing (BrowserComponent me))
                     (inputmenu (BrowserComponent me))
                     )))
    (dolist (i (menuitems me))
      (setf (enabled i) theval)))
  (call-next-method))
(new menuitem :objectname "MenuForMenuEditorPickersNewMenuItem" :menu MenuForMenuEditorPickers :project sk8)
(define-handler menuselect (MenuForMenuEditorPickersNewMenuItem)
  (let* ((bc (browsercomponent (menu me)))
         (proto (or (menuitemprototype (menu me)) menuitem))
         (im (inputMenu bc))
         layer
         newbie)
    (when proto
      (setf layer (selection bc))
      (when layer (setf layer (cadar layer)))
      (setf newbie (new proto :project (project im)))
      (when newbie
        (withActorLocked (bc)
          (setf (menu newbie) im)
          (when layer (setf (layer newbie) (1+ layer)))
          (setf (inputMenu bc) im)
          (setf (selecteditems bc) (list newbie))
          (selectioncompleted bc)))
      )))
(define-handler update (MenuForMenuEditorPickersNewMenuItem)
  (let ((proto (or (menuitemprototype (menu me)) menuitem)))
    (if (eq (keytarget (sk8::window (browsercomponent (menu me)))) (browsercomponent (menu me))) 
      (setf (commandkey me) #\N)
      (setf (commandkey me) nil))
    (setf (text me)
          (concatenate 'string "Add New " (objectstring proto) " to Menu"))))
(new menuitem :objectname "MenuForMenuEditorPickersDisposeMenuItem" :menu MenuForMenuEditorPickers :project sk8)
(define-handler menuselect (MenuForMenuEditorPickersDisposeMenuItem)
  (keydown (BrowserComponent (menu me)) #\delete))
(define-handler update (MenuForMenuEditorPickersDisposeMenuItem)
  (setf (enabled me) (selecteditems (browsercomponent (menu me))))
  (setf (text me)
        (if (cadr (selecteditems (BrowserComponent (menu me))))
          (concatenate 'string "Clear References to " (objectstring (cadr (selecteditems (BrowserComponent (menu me))))))
          "Clear References")))
(new menuspacer :objectname "MenuEdItemSpace" :menu MenuForMenuEditorPickers :project sk8)
(new EditHandlerMenuItem :objectname "MenuEdItemUpdate" :menu MenuForMenuEditorPickers :project sk8)
(define-handler object (MenuEdItemUpdate)
  (cadr (selecteditems (BrowserComponent (menu me)))))
(setf (handlername MenuEdItemUpdate) 'Update)
(new EditHandlerMenuItem :objectname "MenuEdItemMenuSelect" :menu MenuForMenuEditorPickers :project sk8)
(define-handler object (MenuEdItemMenuSelect)
  (cadr (selecteditems (BrowserComponent (menu me)))))
(setf (handlername MenuEdItemMenuSelect) 'MenuSelect)



(define-handler ItemBoundsRect (MenuEditorPicker tuple &key (physical nil))
  (let ((row (cadr tuple))
        (col (car tuple)))
    (if (down mouse)
      (sk8-multival-bind (ll1 tt1 rr1 bb1) 
                         (call-next-method me (list 1 row) :physical physical)
        (sk8-multival-bind (ll2 tt2 rr2 bb2) 
                           (boundsrect me  :physical t)
          (sk8-multival-bind (hh ww) 
                             (framesize me )
            (setf hh (round hh))
            (list (+ ll2 hh) tt1 (- rr2 hh) bb1))))
      (sk8-multival-bind (ll1 tt1 rr1 bb1) 
                         (call-next-method)
        (list ll1 tt1 rr1 bb1))
      )
    ))

(define-handler menuprototype (MenuEditorPicker)
  MenuForMenuEditorPickers)

(define-handler createDisplayItem (MenuEditorPicker theItem)
  (createTextDisplayItem me theItem))

(define-handler (setf InputMenu) (theval MenuEditorPicker)
  (let ((sel (cadr (selecteditems me))))
    (setf (slot-value me 'InputMenu) theval)
    (setf (items me) (and theval (menuitems theval)))
    (if (memq sel (and theval (menuitems theval)))
      (setf (selecteditems me) (list sel))
      (if theval
        (setf (selecteditems me) (list (car (menuitems theval))))
        (setf (selecteditems me) nil)))
    (selectioncompleted me)
    ))

(define-handler click (MenuEditorPicker)
  (when (and (items me) (eq (eventActor) me))
    (let ((pp (pointonwhichpart me (eventH) (eventV) :part 'index))) ;; HEW
      (if (eql (car pp) 2)
        (keydown me #\return)))))

(define-handler keydown (MenuEditorPicker thechar)
  (cond
   ((and (items me) (or (eq theChar #\enter) (eq theChar #\Return)))
    (let* ((theselection (selection me))
           (rowselected (and theselection (cadar theselection)))
           (thevaluetext (aref (imagearray me) 1 (1- rowselected))))
      (withactorlocked (me)
        (setf (framecolor (editor me)) yellow)
        (DisplayEditor me :indices (list 2 rowselected) :selectedText t :defaulttext thevaluetext)
        (forceredraw me))))
   ((and (items me) (eq theChar #\Delete))
    (let* ((theselection (selection me))
           (rowselected (and theselection (cadar theselection)))
           (thevalue (element me (list 2 rowselected))))
      (when (DisposeDialog thevalue)
        (withactorlocked (me)
          (setf (menu thevalue) nil) ;;;****DISPOSE HERE AND IN THE ABOVE KEYDOWNS, AND MBARMENU
          (setf (inputMenu me) (inputMenu me))
          (when (items me) (setf (selection me) (list (list 2 (max 1 (1- rowselected))))))
          (forceredraw me)
          ))))
   (t (moveoffstage (editor me)) (call-next-method)))
  )

(define-handler mousedown (MenuEditorPicker) (moveoffstage (editor me)) (call-next-method))


(define-handler createTextDisplayItem (MenuEditorPicker theitem)
  (if (inheritsfrom theitem menuitem)
    (text theitem)
    (call-next-method)))

(define-handler selectionCompleted (MenuEditorPicker)
  (call-next-method)
  (when (selection me) (showselection me))
  (setf (outputmenuitem me) (cadr (selecteditems me))))

(define-handler ReturnAction (MenuEditorPicker str ind)
  (let* ((theselection (selection me))
         (rowselected (and theselection (cadar theselection)))
         (theitem (element me (list 2 rowselected)))
         (thevaluetext (text (editor me)))
         )
    (setf (text theitem) thevaluetext)
    (setf (aref (imagearray me)  1 (1- rowselected)) thevaluetext)))
(define-handler EscapeAction (MenuEditorPicker)
  )

(define-handler ExtendedMouseDown (MenuEditorPicker)
  (setf (boundsrect ObjectDataRect :physical t) (selectedItemsBoundsRect me))
  (setf (object ObjectDataRect) (remove-if-not #'(lambda (x) (or (is-a x menuitem) (is-a x menu))) (selecteditems me)))
  (withcursor standardcursor
    (drag ObjectDataRect :live nil :onstage t :draggingMouseWithinEvents t :draggedOverEvents t :dropevents t))
  t)
(define-handler draggingMouseEnter (MenuEditorPicker actorDragged)
  (when (eq actorDragged ObjectDataRect)
    (let ((obj (object actorDragged)))
      (unless (listp obj) (setf obj (list obj)))
      (if (every #'(lambda (x) (or (is-a x menu) (is-a x menuitem))) obj)
        (setf (highlight me) t)))))
(define-handler draggingMouseLeave (MenuEditorPicker actorDragged)
  (setf (highlight me) nil))
(define-handler draggingMouseWithin (MenuEditorPicker actorDragged)
  (when (eq actorDragged ObjectDataRect)
    (let ((obj (object actorDragged)))
      (unless (listp obj) (setf obj (list obj)))
      (if (every #'(lambda (x) (or (is-a x menu) (is-a x menuitem))) obj)
        (flashline me))
      )))
(define-handler dropped (MenuEditorPicker droppee)
  (withActorLocked (me)
    (draggingMouseLeave me droppee)  
    (when (eq droppee ObjectDataRect)
      (let ((obj (object droppee)))
        (unless (listp obj) (setf obj (list obj)))
        (when (every #'(lambda (x) (or (is-a x menu) (is-a x menuitem))) obj)
          (let ((theguy (GetLinePosition me))
                NewLayer)
            (when theguy
              (bringup (sk8::window me))
              (setf (keytarget (sk8::window me)) me)
              (setf NewLayer (max 1 (second theguy)))
              (dolist (i (reverse obj))
                (if (and (eq (menu i) (inputMenu me))
                         (< (layer i) newlayer))
                  (decf newlayer))
                (setf (menu i) (inputMenu me))
                (setf (layer i) NewLayer))
              (setf (inputmenu me) (inputmenu me))
              (setf (selecteditems me) obj)
              (selectioncompleted me)
              ))))
      )))

#|

;;;;---------------------------------------------------------------------------------------------------------------------------
;;;;---------------------------------------------------------------------------------------------------------------------------
;;;;  SimpleMenuBarEditor  Allows getting of  menus
;;;;---------------------------------------------------------------------------------------------------------------------------
;;;;---------------------------------------------------------------------------------------------------------------------------

(new rectangle :objectName "SimpleMenuBarEditor" :project sk8
     :properties '(InputMenuBar OutputMenuBar
                   (Editing :inherit :value t)))
(addparent SimpleMenuBarEditor BrowserComponent)

(addInputPort SimpleMenuBarEditor 'InputMenuBar :signature SK8::Menubar)
(addOutputPort SimpleMenuBarEditor 'OutputMenuBar :signature SK8::Menubar)


(new label :objectName "SMBELabel" :project sk8)
(setf (text SMBELabel) "Menubar:")
(setf (container SMBELabel) SimpleMenuBarEditor)
(tagpart SimpleMenuBarEditor SMBELabel 'label)

(new GetObjectField :objectName "SMBEGetField" :project sk8)
(setf (inputPrototype SMBEGetField) sk8::MenuBar)
(setf (container SMBEGetField) SimpleMenuBarEditor)
(tagpart SimpleMenuBarEditor SMBEGetField 'GetField)

(define-handler (setf outputobject) :after (theval SMBEGetField)
                (setf (OutputMenuBar (container me)) theval)
                )

(new reliefButton :objectName "SMBEUpdateButton" :project sk8)
(setf (text SMBEUpdateButton) "Edit Update")
(setf (textsize SMBEUpdateButton) 9)
(setf (textstyle SMBEUpdateButton) '(plain)) ;; HEW
(setf (container SMBEUpdateButton) SimpleMenuBarEditor)
(tagpart SimpleMenuBarEditor SMBEUpdateButton 'UpdateButton)
(define-handler Click (SMBEUpdateButton)
  (let ((outm (outputmenubar (container me))))
    (when (and (editing (container me)) outm)
      (EditHandlerDialog outm :handlername 'Update))))

(new reliefButton :objectName "SMBEMenuSelectButton" :project sk8)
(setf (text SMBEMenuSelectButton) "Edit MenuSelect")
(setf (textsize SMBEMenuSelectButton) 9)
(setf (textstyle SMBEMenuSelectButton) '(plain)) ;; HEW
(setf (container SMBEMenuSelectButton) SimpleMenuBarEditor)
(tagpart SimpleMenuBarEditor SMBEMenuSelectButton 'MenuSelectButton)
(define-handler Click (SMBEMenuSelectButton)
  (let ((outm (outputmenubar (container me))))
    (when (and (editing (container me)) outm)
      (EditHandlerDialog outm :handlername 'MenuSelect))))

(define-handler resized (SimpleMenuBarEditor)
  (sk8-multival-bind (hsize vsize) (size me)
    (setf (left (label me) :resizing nil) 7)
    (setf (top (label me) :resizing nil) 9)
    (setBoundsrect (GetField me) 10 33 (- hsize 10) 50)
    (setBoundsrect (UpdateButton me) (- hsize 80) 10 (- hsize 10) 27)
    (setBoundsrect (MenuSelectButton me) (- hsize 190) 10 (- hsize 90) 27)
    ))               

(define-handler (setf InputMenuBar) (theval SimpleMenuBarEditor)
  (setf (slot-value me 'InputMenuBar) theval)
  (when (or (not theval) (inheritsfrom theval sk8::menubar))
    (setf (inputobject (getfield me)) theval)))

;;;;---------------------------------------------------------------------------------------------------------------------------
;;;;---------------------------------------------------------------------------------------------------------------------------
;;;;  SimpleMenuEditor  Allows getting of  menus
;;;;---------------------------------------------------------------------------------------------------------------------------
;;;;---------------------------------------------------------------------------------------------------------------------------

(new rectangle :objectName "SimpleMenuEditor" :project sk8
     :properties '(InputMenu OutputMenu
                   (Editing :inherit :value t)))
(addparent SimpleMenuEditor BrowserComponent)

(addInputPort SimpleMenuEditor 'InputMenu :signature Menu)
(addOutputPort SimpleMenuEditor 'OutputMenu :signature Menu)


(new label :objectName "SMELabel" :project sk8)
(setf (text SMELabel) "Menu:")
(setf (container SMELabel) SimpleMenuEditor)
(tagpart SimpleMenuEditor SMELabel 'label)

(new GetObjectField :objectName "SMEGetField" :project sk8)
(setf (inputPrototype SMEGetField) menu)
(setf (container SMEGetField) SimpleMenuEditor)
(tagpart SimpleMenuEditor SMEGetField 'GetField)

(define-handler (setf outputobject) :after (theval SMEGetField)
                (setf (OutputMenu (container me)) theval))

(new reliefButton :objectName "SMEUpdateButton" :project sk8)
(setf (text SMEUpdateButton) "Edit Update")
(setf (textsize SMEUpdateButton) 9)
(setf (textstyle SMEUpdateButton) '(plain)) ;; HEW
(setf (container SMEUpdateButton) SimpleMenuEditor)
(tagpart SimpleMenuEditor SMEUpdateButton 'UpdateButton)
(define-handler Click (SMEUpdateButton)
  (let ((outm (outputmenu (container me))))
    (when (and (editing (container me)) outm)
      (EditHandlerDialog outm :handlername 'Update))))

(new reliefButton :objectName "SMEMenuSelectButton" :project sk8)
(setf (text SMEMenuSelectButton) "Edit MenuSelect")
(setf (textsize SMEMenuSelectButton) 9)
(setf (textstyle SMEMenuSelectButton) '(plain)) ;; HEW
(setf (container SMEMenuSelectButton) SimpleMenuEditor)
(tagpart SimpleMenuEditor SMEMenuSelectButton 'MenuSelectButton)
(define-handler Click (SMEMenuSelectButton)
  (let ((outm (outputmenu (container me))))
    (when (and (editing (container me)) outm)
      (EditHandlerDialog outm :handlername 'MenuSelect))))

(define-handler resized (SimpleMenuEditor)
  (sk8-multival-bind (hsize vsize) (size me)
    (setf (left (label me) :resizing nil) 7)
    (setf (top (label me) :resizing nil) 9)
    (setBoundsrect (GetField me) 10 33 (- hsize 10) 50)
    (setBoundsrect (UpdateButton me) (- hsize 80) 10 (- hsize 10) 27)
    (setBoundsrect (MenuSelectButton me) (- hsize 190) 10 (- hsize 90) 27)
    ))               

(define-handler (setf InputMenu) (theval SimpleMenuEditor)
  (setf (slot-value me 'InputMenu) theval)
  (when (or (not theval) (inheritsfrom theval menu))
    (setf (inputobject (getfield me)) theval)))


;;;;---------------------------------------------------------------------------------------------------------------------------
;;;;---------------------------------------------------------------------------------------------------------------------------
;;;;  SimpleMenuItemEditor  Allows editing of  menuitems.
;;;;---------------------------------------------------------------------------------------------------------------------------
;;;;---------------------------------------------------------------------------------------------------------------------------

(new  rectangle :objectname "SimpleMenuItemEditor" :project sk8
     :properties '(InputMenuItem OutputMenuItem
                   (Editing :inherit :value t)))

(addparent SimpleMenuItemEditor BrowserComponent)

(addInputPort SimpleMenuItemEditor 'InputMenuItem :signature Menuitem)
(addOutputPort SimpleMenuItemEditor 'OutputMenuItem :signature Menuitem)


(new label :objectName "SMIELabel" :project sk8)
(setf (text SMIELabel) "MenuItem:")
(setf (container SMIELabel) SimpleMenuItemEditor)
(tagpart SimpleMenuItemEditor SMIELabel 'label)

(new GetObjectField :objectName "SMIEGetField" :project sk8)
(setf (inputPrototype SMIEGetField) MenuItem)
(setf (container SMIEGetField) SimpleMenuItemEditor)
(tagpart SimpleMenuItemEditor SMIEGetField 'GetField)

(define-handler (setf outputobject) :after (theval SMIEGetField)
                (setf (checked (EnabledCheck (container me))) (and theval (enabled theval)))
                (setf (text (CommandKey (container me))) (and theval (commandkey theval) (string (CommandKey theval))))
                (setf (text (CheckMark (container me))) (and theval (CheckMark theval) (string (CheckMark theval))))
                (setf (OutputMenuItem (container me)) theval))

(new checkbox :objectname "SMIEEnabledCheck" :project sk8)
(setf (text SMIEEnabledCheck) "Enabled")
(setf (textsize SMIEEnabledCheck) 10)
(setf (container SMIEEnabledCheck) SimpleMenuItemEditor)
(define-handler check (SMIEEnabledCheck)
  (call-next-method)
  (when (and (editing (container me)) (OutputMenuItem (container me)))
    (setf (enabled (OutputMenuItem (container me))) (checked me)))
  )
(tagpart SimpleMenuItemEditor SMIEEnabledCheck 'EnabledCheck)

(new rectangle :objectname "SMIECommandKey" :project sk8)
(setf (text SMIECommandKey) "")
(setf (textsize SMIECommandKey) 10)
(setf (container SMIECommandKey) SimpleMenuItemEditor)
(define-handler keydown (SMIECommandKey theChar)
  (when (and (editing (container me)) (OutputMenuItem (container me)))
    (cond
     ((alphanumeric thechar)
      
      (setf (text me) (string thechar))
      (setf (commandkey (OutputMenuItem (container me))) thechar))
     ((eq thechar #\delete)
      (setf (text me) "")
      (setf (commandkey (OutputMenuItem (container me))) nil)))
    (when (container me)
      (dolist (i (deepcontents (sk8::window me)))
        (when (and (inheritsfrom i MenuEditorPicker) (eq (inputmenu i) (menu (OutputMenuItem (container me)))))
          (setf (inputmenu i) (inputmenu i))
          (setf (selecteditems i) (list (OutputMenuItem (container me))))
          (selectioncompleted i))))
    ))
(define-handler mousedown (SMIECommandKey)
  (when (and (editing (container me)) (OutputMenuItem (container me)))
    (setf (keytarget (sk8::window me)) me)
    ))
(define-handler activatetext (SMIECommandKey)
  (browserhilight me)
  )
(define-handler deactivatetext (SMIECommandKey)
  (browserunhilight me)
  )
(tagpart SimpleMenuItemEditor SMIECommandKey 'CommandKey)
(new label :objectname "SMIECommandKeyLabel" :project sk8)
(setf (text SMIECommandKeyLabel) "Command Key")
(setf (textsize SMIECommandKeyLabel) 10)
(setf (container SMIECommandKeyLabel) SimpleMenuItemEditor)
(tagpart SimpleMenuItemEditor SMIECommandKeyLabel 'CommandKeyLabel)

(new rectangle :objectname "SMIECheckMark" :project sk8)
(setf (text SMIECheckMark) "")
(setf (textsize SMIECheckMark) 10)
(setf (container SMIECheckMark) SimpleMenuItemEditor)
(define-handler keydown (SMIECheckMark theChar)
  (when (and (editing (container me)) (OutputMenuItem (container me)))
    (cond
     ((alphanumeric thechar)
      (setf (text me) (string thechar))
      (setf (checkMark (OutputMenuItem (container me))) thechar))
     ((eq thechar #\delete)
      (setf (text me) "")
      (setf (checkMark (OutputMenuItem (container me))) nil)))
    (when (container me)
      (dolist (i (deepcontents (sk8::window me)))
        (when (and (inheritsfrom i MenuEditorPicker) (eq (inputmenu i) (menu (OutputMenuItem (container me)))))
          (setf (inputmenu i) (inputmenu i))
          (setf (selecteditems i) (list (OutputMenuItem (container me))))
          (selectioncompleted i))))
    ))
(define-handler mousedown (SMIECheckMark)
  (when (and (editing (container me)) (OutputMenuItem (container me)))
    (setf (keytarget (sk8::window me)) me)
    ))
(define-handler activatetext (SMIECheckMark)
  (browserhilight me)
  )
(define-handler deactivatetext (SMIECheckMark)
  (browserunhilight me)
  )
(tagpart SimpleMenuItemEditor SMIECheckMark 'CheckMark)
(new label :objectname "SMIECheckMarkLabel" :project sk8)
(setf (text SMIECheckMarkLabel) "checkMark")
(setf (textsize SMIECheckMarkLabel) 10)
(setf (container SMIECheckMarkLabel) SimpleMenuItemEditor)
(tagpart SimpleMenuItemEditor SMIECheckMarkLabel 'CheckMarkLabel)

(new reliefButton :objectName "SMIEUpdateButton" :project sk8)
(setf (text SMIEUpdateButton) "Edit Update")
(setf (textsize SMIEUpdateButton) 9)
(setf (textstyle SMIEUpdateButton) '(plain)) ;; HEW
(setf (container SMIEUpdateButton) SimpleMenuItemEditor)
(tagpart SimpleMenuItemEditor SMIEUpdateButton 'UpdateButton)
(define-handler Click (SMIEUpdateButton)
  (let ((outm (outputMenuItem (container me))))
    (when (and (editing (container me)) outm)
      (EditHandlerDialog outm :handlername 'Update))))

(new reliefButton :objectName "SMIEMenuSelectButton" :project sk8)
(setf (text SMIEMenuSelectButton) "Edit MenuSelect")
(setf (textsize SMIEMenuSelectButton) 9)
(setf (textstyle SMIEMenuSelectButton) '(plain)) ;; HEW
(setf (container SMIEMenuSelectButton) SimpleMenuItemEditor)
(tagpart SimpleMenuItemEditor SMIEMenuSelectButton 'MenuSelectButton)
(define-handler Click (SMIEMenuSelectButton)
  (let ((outm (outputMenuItem (container me))))
    (when (and (editing (container me)) outm)
      (EditHandlerDialog outm :handlername 'MenuSelect))))

(define-handler resized (SimpleMenuItemEditor)
  (sk8-multival-bind (hsize vsize) (size me)
    (setf (left (label me) :resizing nil) 7)
    (setf (top (label me) :resizing nil) 9)
    (setBoundsrect (GetField me) 10 33 (- hsize 10) 50)
    (setBoundsrect (UpdateButton me) (- hsize 80) 10 (- hsize 10) 27)
    (setBoundsrect (MenuSelectButton me) (- hsize 190) 10 (- hsize 90) 27)
    
    (setboundsrect (CommandKey me) 
                   (+ 10 )
                   (+ 5 (bottom (GetField me))) 
                   (+ (height (EnabledCheck me)) 10 )
                   (+ 5 (height (EnabledCheck me)) (bottom (GetField me))))
    (setf (left (CommandKeyLabel me) :resizing nil) (right (commandkey me)))
    (setf (top (CommandKeyLabel me) :resizing nil) (top (commandkey me)))
    (setboundsrect (CheckMark me) 
                   (+ 20 (right (CommandKeyLabel me)))
                   (+ 5 (bottom (GetField me))) 
                   (+ 20 (height (EnabledCheck me)) (right (CommandKeyLabel me)))
                   (+ 5 (height (EnabledCheck me)) (bottom (GetField me))))
    (setf (left (CheckMarkLabel me) :resizing nil) (right (CheckMark me)))
    (setf (top (CheckMarkLabel me) :resizing nil) (top (commandkey me)))
    
    (setf (top (EnabledCheck me) :resizing nil) (+ 5 (bottom (CommandKeyLabel me))))
    (setf (left (EnabledCheck me) :resizing nil) 10)
    
    ))

(define-handler (setf InputMenuItem) (theval SimpleMenuItemEditor)
  (setf (slot-value me 'InputMenuItem) theval)
  (when (or (not theval) (inheritsfrom theval menuitem))
    (setf (inputobject (getfield me)) theval)))

|#



#|
	Change History (most recent last):
	1	11/5/93	rod	
	2	11/5/93	kleiman	
	4	11/19/93	rod	
	5	11/22/93	rod	
	6	11/22/93	rod	
	7	11/30/93	rod	
	8	12/3/93	rod	
	9	12/3/93	rod	
	10	12/10/93	rod	
	11	12/13/93	rod	
	12	12/17/93	till	#.'s be gone: keydown
	14	1/24/94	rod	
	15	2/12/94	kleiman	renaming
	16	2/21/94	hernan	The Final Renaming for Alpha! (yeah, right...)
	17	2/21/94	hernan	window -> sk8::window.
	18	2/25/94	hernan	Using symbols instead of keywords for options!!!
	19	3/3/94	Hernan	The Great Argument Name Renaming of 94!
	20	3/4/94	kleiman	addparent avoided where possible
	21	3/4/94	kleiman	rectify changes due initialize bug
	22	3/7/94	rod	
	23	3/9/94	rod	Doing Project Switching and Reference Clearing.
	24	3/10/94	rod	
	25	3/11/94	rod	
	26	3/21/94	rod	
	27	3/30/94	rod	
	28	3/30/94	rod	
	29	3/30/94	rod	WatchCursor stuff.
	30	4/20/94	rod	
	32	5/6/94	rod	
	33	7/6/94	Hernan	1169269: replacing the eventX variables with
				functions.
	34	8/22/94	rod	
	35 	 9/12/94	rod     	
	36 	10/ 7/94	chip    	took out all initialize handlers (ports now handled automatically)
	37 	 2/ 3/95	Hernan  	WithLockedCursor is now correct. I check this file
							out and back in to force it to be recompiled.
	38 	 2/16/95	rod     	Fixing reordering menus in the menubar.
	2  	10/ 9/96	Hernan  	Calling showselection with the right args.
	3  	10/ 9/96	Hernan  	Calling showselection with the right args.
	4  	 2/27/97	Hernan  	
|# ;(do not edit past this line!!)
