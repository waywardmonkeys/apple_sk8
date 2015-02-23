;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories


;;;


(in-package :uidev)

;; Symbols inserted into the appropriate preload file  6-25-94   5:11 am
(SK8-declare-syms :UI :public ; Updated  6-25-94   5:09 am
                  UI::MEMENUSMENU UI::MEMENUSMENUBAR UI::MEMENUSMENUITEMS)


;;______________________________________________________________________
;;______________________________________________________________________
;; MenuEditor

(new UIsimplewindow :ObjectName "MenuEditor" :project ui
          :properties '(outputmenu outputmenuitem outputmenubar))
(addproperty menueditor 'inputobjects)
;;;(dolist (i (deepcontents menueditor)) (setf (objectname i) nil))
;;;(setf (objectname menueditor) nil)
(setf (sk8::menubar MenuEditor) t)
(setf (resizer MenuEditor) nil)
(setf (zoombox MenuEditor) nil)

(define-handler clearReferences (MenuEditor &key ((:objects theobjects)))
  (if (and theobjects (not (memq (inputobjects me) theobjects)) (not (memq (inputMenubar MEMenuBarProxy) theobjects)))
    (withActorLocked (me)
      (setf theobjects (remove-if-not #'(lambda (x) (or (is-a x menu) (is-a x menuitem))) theobjects))
      (dolist (theobject theobjects)
        (cond 
         ((is-a theobject menuitem)
          (setf (menu theobject) nil)
          (setf (inputmenu MEMenuProxy) (inputmenu MEMenuProxy)))
         ((is-a theobject menu)
          (setf (menu theobject) nil)
          (setf (sk8::menubar theobject) nil)
          (setf (inputmenu MEMenuProxy) (inputmenu MEMenuProxy))
          (setf (inputMenubar MEMenuBarProxy) (inputMenubar MEMenuBarProxy)))
         )))
    (setf (inputobjects me) nil)))

(define-handler SetUpForProject (MenuEditor &key ((:project theproject)))
  (setf (text me) (concatenate 'string (objectstring theproject :project theproject) " Menu Editor"))
  (if (and (inputobjects me)
           (not (memq (project (inputobjects me)) (okprojects theproject))))
    (setf (inputobjects me) nil)))

(define-handler (setf inputobjects) (theval menueditor)
  (withactorlocked (me)
    (setf (text me) (concatenate 'string (objectstring (targetproject ui) :project (targetproject ui)) " Menu Editor"))
    (let ((ok t))
      (cond 
       ((null theval)
        (setf (inputmenubar MEMenubarProxy) nil)
        (setf (inputmenu MEMenuProxy) nil)
        (setf (inputmenu currentmenuproxy) nil)
        (setf (inputmenu submenuproxy) nil)
        (setf (keytarget me) nil)
        
        )
       ((inheritsfrom theval sk8::menubar)
        (unless (eq (inputmenubar MEMenubarProxy) theval) 
          (setf (inputmenubar MEMenubarProxy) theval)
          (setf (keytarget me) MEMenubarProxy)
          (when (items MEMenubarProxy)
            (setf (selecteditems MEMenubarProxy) (list (aref (items MEMenubarProxy) 0 0)))
            (selectioncompleted MEMenubarProxy)))
        )
       ((inheritsfrom theval menu)
        (cond
         ((and (menu theval) (menu (menu theval)))
          (messagetouser "Sorry, we only deal with hierarchical menus which are 1 level deep.")
          (setf ok nil))
         ((menu theval)
          (setf (inputobjects me) (menu theval))
          (setf (keytarget me) MEMenuProxy)
          (setf ok nil))
         ((sk8::menubar theval)
          (setf (keytarget me) MEMenuTitle)
          (setf (inputmenubar MEMenubarProxy) (sk8::menubar theval))
          (setf (selecteditems MEMenubarProxy) (list theval))
          (selectioncompleted MEMenubarProxy))
         (t
          (setf (keytarget me) MEMenuTitle)
          (setf (inputmenubar MEMenubarProxy) nil)
          (setf (inputmenu MEMenuProxy) theval)
          )))
       (t 
        (setf ok nil)))
      (when ok (sk8::setValue 'inputobjects me theval))
      )))

(define-handler (setf keytarget) (theval menueditor)
  (when (or (null theval) (inputobjects me) (inputmenubar MEMenubarProxy) (inputmenu MEMenuProxy))
    (call-next-method)))

(define-handler enteringStage (menueditor)
  (setf (inputobjects me) nil)
  (call-next-method))


(define-handler uiedit (sk8::menubar)
  (bringup menueditor)
  (setf (inputobjects menueditor) me))
(define-handler uiedit (sk8::menu)
  (bringup menueditor)
  (setf (inputobjects menueditor) me))
(define-handler uiedit (pickermenu)
  (editobjects me))

;;;--------------------------------------------------------------------------------------------------

(new Rectangle :objectname "MEName" :project ui)
(setf (textlocation MEName) 'centerleft)
(setf (texthoffset MEName) 5) 
(setf (textsize MEName) 9) 
(setf (textfont MEName) "Geneva") 
(setf (framesize MEName) '(2 2)) 
(setf (frameColor MEName) framescore) 
(setf (fillcolor MEName) shadowwhite)
(addproperty MEName 'menutype)
(define-handler draggingMouseEnter (MEName actorDragged)
  (when (and (menutype me)
             (eq actorDragged objectdatarect)
             (neq (componentfrom objectdatarect) me)
             (is-a (object objectdatarect) (menutype me)))
    (setf (fillcolor me) white))
    )
(define-handler draggingMouseLeave (MEName actorDragged)
  (setf (fillcolor me) shadowwhite)
  )
(define-handler dropped (MEName droppee)
  (when (and (menutype me) (eq droppee objectdatarect)
             (neq (componentfrom objectdatarect) me)
             (is-a (object objectdatarect) (menutype me)))
    (draggingmouseleave me droppee)
    (bringup (sk8::window me))
    (setf (inputobjects menueditor) (object objectdatarect))
    ))
(define-handler item (MEName)
  nil
  )
(define-handler mousedown (MEName)
  (let ((i (item me)))
    (when i
      (setf (boundsrect ObjectDataRect :physical t) (boundsrect me :physical t))
      (setf (object ObjectDataRect) i)
      (setf (ComponentFrom ObjectDataRect) me)
      (withcursor standardcursor
        (drag ObjectDataRect :live nil :onstage t :draggingMouseWithinEvents t :draggedOverEvents t :dropevents t)
        )
      )))

(new uibiglabel :objectname "MELabel" :project ui)

(new MELabel :objectname "MEObjectsLabel" :project ui)
(setf (container MEObjectsLabel) MenuEditor)
(setf (text MEObjectsLabel) "Objects:")
(setf (textsize MEObjectsLabel) 9)

;;;(dolist (i (deepcontents menueditor)) (if (is-a i melabel) (setf (textsize i) 9)))

(new Rectangle :objectname "MEObjectLine" :project ui :framecolor framescore :container MenuEditor)

(new MELabel :objectname "MEBarNameLabel" :project ui)
(setf (container MEBarNameLabel) MenuEditor)
(setf (text MEBarNameLabel) "Menubar:")
(new MEName :objectname "MEBarName" :project ui)
(setf (container MEBarName) MenuEditor)
(setf (menuType MEBarName) sk8::menubar)
(define-handler item (MEBarName)
  (inputMenuBar MEMenuBarProxy)
  )

(new MELabel :objectname "MEMenuNameLabel" :project ui)
(setf (container MEMenuNameLabel) MenuEditor)
(setf (text MEMenuNameLabel) "Menu:")
(new MEName :objectname "MEMenuName" :project ui)
(setf (container MEMenuName) MenuEditor)
(setf (menuType MEMenuName) menu)
(define-handler item (MEMenuName)
  (inputMenu MEMenuProxy)
  )

(new MELabel :objectname "MEItemNameLabel" :project ui)
(setf (container MEItemNameLabel) MenuEditor)
(setf (text MEItemNameLabel) "Menuitem:")
(new MEName :objectname "MEItemName" :project ui)
(setf (container MEItemName) MenuEditor)
(setf (menuType MEItemName) nil)  ;;;;**** should support setf (inputobjects of menueditor to a menuitem...
(define-handler item (MEItemName)
  (outputmenuitem MEMenuProxy)
  )

;;;;------------------------------------------------------------------------------------------------

(new Rectangle :objectname "MEMenuLine" :project ui :framecolor framescore :container MenuEditor)


(new Rectangle :objectname "MenuProxy" :project ui)
(addproperty MenuProxy 'InputMenu)
(addproperty MenuProxy 'showsSubMenus :initialvalue t)
(setframesize menuproxy 0 0)
(setf (fillcolor MenuProxy) transparent)
(new Rectangle :objectname "MPWhiteSheet" :project ui)
(setf (container MPWhiteSheet) MenuProxy)
(tagpart MenuProxy MPWhiteSheet 'whitesheet)
(new Rectangle :objectname "MPBlackSheet" :project ui)
(setf (container MPBlackSheet) MenuProxy)
(setf (fillcolor MPBlackSheet) black)
(tagpart MenuProxy MPBlackSheet 'BlackSheet)
(define-handler resized (MenuProxy)
  (let (hSize vSize)
    (sk8-multival-setf (hSize vSize) (size me))
    (setBoundsrect (whitesheet me) 0 0 (- hsize 2) (- vsize 2))
    (setBoundsrect (BlackSheet me) 2 2 hsize vsize)
    ))

;;(setf (objectname menuproxyline) nil)

(new rectangle :objectname "MenuProxyLine" :project ui)
(define-handler minimumsize (MenuProxyLine) '(1 1))
(setSize MenuProxyLine 50 1)
(setf (framecolor menuProxyLine) darkgray)
(new rectangle :objectname "MenuProxyBox" :project ui)
(addproperty MenuProxyBox 'menu)
(setf (textsize MenuProxyBox) 9)
(setf (textfont MenuProxyBox) "geneva")
(setf (textlocation MenuProxyBox) 'centerleft)
(setf (texthoffset MenuProxyBox) 3)
(setf (fillcolor MenuProxyBox) graytone90)
(setf (framecolor MenuProxyBox) uirectangleoutbevel)
(setf (mousesensitivity MenuProxyBox) 'normal)
(new polygon :objectname "MenuProxyBoxArrow" :project ui)
(setf (container MenuProxyBoxArrow) MenuProxyBox)
(setf (mousesensitivity MenuProxyBoxArrow) 'transparent)
(setf (fillcolor MenuProxyBoxArrow) black)
(setf (points MenuProxyBoxArrow) '(0 0 5 5 0 10))
(tagpart MenuProxyBox MenuProxyBoxArrow 'arrow)
(define-handler resized (MenuProxyBox)
  (setLocation (arrow me) (- (width me) 10) (round (height me) 2)))
(setSize MenuProxyBox 60 15)

(define-handler mousedown (MenuProxyBox)
  (withactorlocked (me)
    (setf (inputmenu memenuproxy) (menu me))
    (setf (selecteditems memenubarproxy) nil)
    (lightforceredraw memenubarproxy)
    (dolist (i (contents (container me)))
      (if (eq (baseparent i) menuproxybox)
        (setf (fillcolor i) (fillcolor menuproxybox))))
    (setf (fillcolor me) highlighted)
    (setf (inputmenu SubMenuProxy) (menu me))
    (if (menuitems (menu me))
      (progn
        (setf (top SubMenuProxy :resizing nil :physical t) (top me :physical t))
        (if (< (- (right (sk8::window me) :physical t) (right me :physical t)) (width submenuproxy))
          (progn
            (setf (right SubMenuProxy :resizing nil :physical t) (+ (left me :physical t) 2))
            (sendtoback SubMenuProxy))
          (progn
            (setf (left SubMenuProxy :resizing nil :physical t) (right me :physical t))
            (bringtofront SubMenuProxy))
          ))
      (moveoffstage SubMenuProxy))
    )
  )

(define-handler (setf InputMenu) (theval MenuProxy)
  (withactorlocked (me)
    (let* (curitem 
           (curloc 4) 
           (lastmenu nil)
           (whitesheet (whitesheet me))
           (ww (width whitesheet)))
      (sk8::setValue 'inputmenu me theval)
      (dolist (i (contents whitesheet))
        (setf (container i) nil)
        (if (is-a i menuproxybox) (setf (menu i) nil))
        (discard i))
      (when (is-a theval menu)
        (dolist (i (menuitems theval))
          (if (or (not (showsSubMenus me)) (is-a i menuitem))
            (progn
              (setf curitem (recycled MenuProxyLine :in ui))
              (setf lastmenu nil)
              (setf (container curitem) whitesheet)
              (setf  (boundsrect curitem) (list 9 (+ curloc 2) (- ww 9) (+ curloc 3)))
              (incf curloc 4))
            (progn 
              (if lastmenu
                (decf curloc 3)
                (incf curloc 4))
              (setf lastmenu t)
              (setf curitem (recycled MenuProxyBox :in ui))
              (setf (fillcolor curitem) (fillcolor menuproxybox))
              (setf (container curitem) whitesheet)
              (setf (menu curitem) i)
              (setf (text curitem) (text i))
              (setboundsrect curitem 1 curloc (1- ww) (+ curloc 16))
              (incf curloc 18)))
          )
        (if (menuitems theval)
          (sk8-multival-bind (ll tt rr bb) (boundsrect me)
            (setboundsrect me ll tt rr (+ tt curloc 8)))
          (moveoffstage me))))
    ))

(sendtoback MPBlacksheet)
(resized MenuProxy)

;;;(mapcar #'(lambda (x) (setf (points x) '(0 0 6 6 0 11))) (knownchildren MenuProxyBoxArrow))
;;;(mapcar #'(lambda (x) (setf (fillcolor x) hilighted)) (knownchildren MenuProxyBox))
;;;(mapcar #'(lambda (x) (setf (framecolor x) uirectangleoutbevel)) (knownchildren MenuProxyBox))

(new menuproxy :objectname "CurrentMenuProxy" :project ui :container menueditor)
(setf (showsSubMenus CurrentMenuProxy) t)
(setf (width CurrentMenuProxy) 80)
(moveoffstage currentmenuproxy)
(define-handler (setf InputMenu) (theval CurrentMenuProxy)
  (unless theval 
    (moveoffstage me)
    (setf (inputmenu submenuproxy) nil))
  (call-next-method)
  (moveoffstage SubMenuProxy))


(new menuproxy :objectname "SubMenuProxy" :project ui :container menueditor)
(setf (showsSubMenus SubMenuProxy) nil)
(setf (width SubMenuProxy) 80)
(moveoffstage SubMenuProxy)
;;;(setf (container menuproxy) nil)
;;;(select CurrentMenuProxy)


;;______________________________________________________________________


(new MELabel :objectname "MEMenuBarProxyLabel" :project ui)
(setf (container MEMenuBarProxyLabel) MenuEditor)
(setf (text MEMenuBarProxyLabel) "Menubar Proxy:")


(new MenuBarEditorPicker :objectName "MEMenubarProxy" :project ui)
(setf (container MEMenubarProxy) MenuEditor)
(setf (fillcolor MEMenubarProxy) shadowedRenderer)
(setf (textsize MEMenubarProxy) 9)
(setf (textfont MEMenubarProxy) "Geneva")
(setf (rowspacing MEMenubarProxy) 1)
(setf (columnLinesSize MEMenubarProxy) 1)
(setf (highlightcolor MEMenubarProxy) transparent)
(define-handler targetProject (MEMenubarProxy) (targetproject ui))
(define-handler (setf Highlight) (theval MEMenubarProxy)
  (setf (inverts me) nil)
  (if theval
    (setf (fillcolor me) white)
    (setf (fillcolor me) shadowwhite)
    )
  (call-next-method))

;;______________________________________________________________________

(new Rectangle :objectname "MEMenuItemLine" :project ui :framecolor framescore :container MenuEditor)

(new MELabel :objectname "MESelectedMenuLabel" :project ui)
(setf (container MESelectedMenuLabel) MenuEditor)
(setf (text MESelectedMenuLabel) "Selected Menu:")

(new Edittext :objectName "MEMenuTitle" :project ui)
(setFrameSize MEMenuTitle 1 1)
(setf (autotab MEMenuTitle) t)
(setf (fillcolor MEMenuTitle) white)
(setf (framecolor MEMenuTitle) black)
(setf (container MEMenuTitle) MenuEditor)
(setf (textsize MEMenuTitle) 12)
(setf (textfont MEMenuTitle) "Chicago")
(setf (text MEMenuTitle) "")
(define-handler activatetext (MEMenuTitle)
  (call-next-method)
  (setselection me 0 -1)
  )
(define-handler keydown (MEMenuTitle thechar)
  (if (or (eq thechar #\enter) (eq thechar #\return))
    (setselection me 0 -1)
    (let ((m (inputmenu MEMenuProxy)))
      (when m
        (call-next-method)
        (withactorlocked (me)
          (sk8-multival-bind (ll tt rr bb) (boundsrect me)
            (declare (ignore rr))
            (setboundsrect me ll tt (+ ll (car (actortextsize me)) 15) bb))
          (setf (text m) (text me))
          (if (menu m)
            (dolist (i (contents (whitesheet CurrentMenuProxy)))
              (if (eq (fillcolor i) highlighted)
                (setf (text i) (text me))))
            (when (inputmenubar MEMenubarProxy)
              (let* ((theselection (selection MEMenubarProxy))
                     (colselected (and theselection (caar theselection))))
                (when colselected
                  (setf (aref (imagearray MEMenubarProxy) (1- colselected) 0) (text me))
                  (computecolumnwidths MEMenubarProxy))))
            ))))))
(addparent MEMenuTitle BrowserComponent)
(define-handler resized (MEMenuTitle)
  (setf (left MEMenuTitleBox :resizing nil) (right me))
  ;;(if (eq (keytarget menueditor) me) (surroundObject (highlighterhalo MenuEditor) me ))
  )

(new rectangle :objectname "MEMenuTitleBox" :project ui)
(setf (container MEMenuTitleBox) MenuEditor)
(new polygon :objectname "MenuTitleArrowDown" :project ui)
(setf (container MenuTitleArrowDown) MEMenuTitleBox)
(setf (mousesensitivity MenuTitleArrowDown) 'transparent)
(setf (fillcolor MenuTitleArrowDown) black)
(setf (points MenuTitleArrowDown) '(0 0 5 5 10 0))
(new polygon :objectname "MenuTitleArrowRight" :project ui)
(setf (container MenuTitleArrowRight) MEMenuTitleBox)
(setf (mousesensitivity MenuTitleArrowRight) 'transparent)
(setf (fillcolor MenuTitleArrowRight) black)
(setf (points MenuTitleArrowRight) '(0 0 5 5 0 10))
(define-handler resized (MEMenuTitleBox)
  (dolist (i (contents me))
    (setf (location i :physical t) (location me :physical t))))
(setBoundsRect MEMenuTitleBox 0 0 20 20)



(new MenuEditorPicker :objectname "MEMenuProxy" :project ui)
(setf (container MEMenuProxy) MenuEditor)
(define-handler dropped (MEMenuProxy droppee)
  (withActorLocked (me)
    (call-next-method)  
    (when (eq droppee ObjectDataRect)
      (cond
       ((inheritsfrom (object droppee) Menu)
        (bringup (sk8::window me))
        (setf (inputmenubar MEMenubarProxy) (inputmenubar MEMenubarProxy))
        (setf (selecteditem MEMenubarProxy) (inputmenu me))
        (selectioncompleted MEMenubarProxy))
       ))))
(define-handler targetProject (MEMenuProxy) (targetproject ui))
(define-handler (setf Highlight) (theval MEMenuProxy)
  (setf (inverts me) nil)
  (if theval
    (setf (fillcolor me) white)
    (setf (fillcolor me) shadowwhite)
    )
  (call-next-method))

(new uiverticalscroller :objectname "MEScroller" :project ui)
(setf (container MEScroller) MenuEditor)
(setf (partnervscroller MEMenuProxy) MEScroller)

(define-handler activatetext (MEMenuProxy)
  (withactorlocked (menueditor)
    (call-next-method)
    ;;(setboundsrect (highlighterhalo menueditor) 0 0 16 0 :relative t)
  ))

;;______________________________________________________________________

(new BrowserMenubar :otherparents uimenubaractor :objectname "MEMenuBar" :project ui)
     
(setf (container MEMenuBar) MenuEditor)
(tagpart MenuEditor MEMenuBar 'Bar)
(setup MEMenuBar)
(setf (objectname (first (menus MEMenuBar))) "MEMenusMenuBar")
(setf (objectname (second (menus MEMenuBar))) "MEMenusMenu")
(setf (objectname (third (menus MEMenuBar))) "MEMenusMenuItems")
(addparent MEMenusMenuBar uimenuactor)
(addparent MEMenusMenu uimenuactor)
(addparent MEMenusMenuItems uimenuactor)
(new menuitem :objectname "MEAddSubMenu" :project ui)
(setf (text MEAddSubMenu) "Add SubMenu")
(setf (menu MEAddSubMenu) MEMenusMenu)
(bringtofront MEAddSubMenu)
(define-handler update (MEAddSubMenu)
  (setf (enabled me) (inputmenu MEMenuproxy)))
(define-handler menuselect (MEAddSubMenu)
  (let ((newmen (getnewfromuser menu :project (targetproject ui))))
    (withactorlocked (menueditor)
      (setf (menu newmen) (inputmenu MEMenuproxy))
      (setf (inputmenu MEMenuproxy) (inputmenu MEMenuproxy))
      (setf (selecteditem MEMenuProxy) newmen))))

(new menuitem :objectname "MEEditExistingMenubar" :project ui)
(setf (text MEEditExistingMenubar) "Edit Existing Menubar")
(setf (menu MEEditExistingMenubar) MEMenusMenuBar)
(bringtofront MEEditExistingMenubar)
(sendfarther MEEditExistingMenubar)
(define-handler update (MEEditExistingMenubar)
  (setf (enabled me) t))
(define-handler menuselect (MEEditExistingMenubar)
  (setf (inputobjects menueditor) (getfromuser sk8::menubar :project (targetproject ui))))

(new menuitem :objectname "MEPutMenubarInStage" :project ui)
(setf (text MEPutMenubarInStage) "Put Menubar In Stage")
(setf (menu MEPutMenubarInStage) MEMenusMenuBar)
(bringtofront MEPutMenubarInStage)
(sendfarther MEPutMenubarInStage)
(sendfarther MEPutMenubarInStage)
(define-handler update (MEPutMenubarInStage)
  (setf (enabled me) (inputmenubar MEMenubarProxy)))
(define-handler menuselect (MEPutMenubarInStage)
  (setf (sk8::menubar stage) (inputmenubar MEMenubarProxy)))


(new menuitem :objectname "MEEditExistingMenu" :project ui)
(setf (text MEEditExistingMenu) "Edit Existing Menu")
(setf (menu MEEditExistingMenu) MEMenusMenu)
(bringtofront MEEditExistingMenu)
(define-handler update (MEEditExistingMenu)
  (setf (enabled me) t))
(define-handler menuselect (MEEditExistingMenu)
  (setf (inputobjects menueditor) (getfromuser menu :project (targetproject ui))))



(new menuitem :objectname "MEEditNewMenu" :project ui)
(setf (text MEEditNewMenu) "Edit New Menu")
(setf (menu MEEditNewMenu) MEMenusMenu)
(bringtofront MEEditNewMenu)
(define-handler update (MEEditNewMenu)
  (setf (enabled me) t))
(define-handler menuselect (MEEditNewMenu)
  (setf (inputobjects menueditor) (getnewfromuser menu :project (targetproject ui))))

(sendfarther MEAddSubMenu)

;;______________________________________________________________________


(define-handler (setf inputmenubar) :after (theval MEMenubarProxy)
                (if theval
                  (progn
                    (show me)
                    (setf (text MESelectedMenuLabel) "Selected Menu:")
                    (setf (textcolor MEMenuBarProxyLabel) black)
                    (setf (textcolor MEBarName) black)
                    (setf (textcolor MEBarNameLabel) black))
                  (progn
                    (hide me)
                    (setf (text MESelectedMenuLabel) "Popup Menu:")
                    (setf (inputmenu currentmenuproxy) nil)
                    (setf (inputmenu submenuproxy) nil)
                    (moveoffstage currentmenuproxy)
                    (moveoffstage submenuproxy)
                    (setf (textcolor MEMenuBarProxyLabel) gray)
                    (setf (textcolor MEBarName) gray)
                    (setf (textcolor MEBarNameLabel) gray)))
                
                (setf (text MEBarName) (if theval 
                                         (objectstring theval :project (project theval))
                                         "")))
                
(define-handler (setf outputmenu) :after (theval MEMenubarProxy)
                (withactorlocked (menuEditor)
                  
                  (setf (inputmenu MEMenuProxy) theval)
                  ))



(define-handler (setf inputmenu) :after (theval MEMenuProxy)
                (withactorlocked (me)
                  (if theval
                    (progn
                      (unless (visible me)
                        (show me)
                        (show MEScroller)
                        (show MEMenuTitleBox)
                        (show MEMenuTitle))
                      (setf (textcolor MESelectedMenuLabel) black)
                      (setf (textcolor MEMenuName) black)
                      (setf (textcolor MEMenuNameLabel) black))
                    (progn
                      (when (visible me)
                        (hide me)
                        (hide MEScroller)
                        (hide MEMenuTitleBox)
                        (hide MEMenuTitle))
                      (setf (text MESelectedMenuLabel) "Menu Proxy:")
                      (setf (textcolor MESelectedMenuLabel) gray)
                      (setf (textcolor MEMenuName) gray)
                      (setf (textcolor MEMenuNameLabel) gray)))
                  (cond 
                   ((not theval)
                    (setf (inputmenu CurrentMenuProxy) theval)
                    (moveoffstage currentmenuproxy))
                   ((and (not (menu theval))
                         (inputmenubar MEMenubarProxy))
                    (let* ((theselection (selection MEMenubarProxy))
                             (col (and theselection (caar theselection))))
                        (setf (inputmenu CurrentMenuProxy) theval)
                        (setf (top CurrentMenuProxy :resizing nil)  (bottom MEMenubarProxy))    
                        (setf (left CurrentMenuProxy :resizing nil :physical t) 
                              (car (itemboundsrect MEMenubarProxy (list col 1) :physical t)))
                        (moveonstage currentmenuproxy))
                    ))
                  (let ((txt (if theval (text theval)
                                 "")))
                    (sk8-multival-bind (ll tt rr bb) (boundsrect MEMenuTitle)
                      (setboundsrect MEMenuTitle ll tt (+ 15 ll (car (actortextsize MEMenuTitle :thetext txt))) bb))
                    (cond 
                     ((or (not theval) (sk8::menubar theval))
                      (hide MEMenuTitleBox))
                     ((sk8::menu theval)
                      (show MEMenuTitleBox)
                      (hide MenuTitleArrowDown)
                      (show MenuTitleArrowRight))
                     (t
                      (show MEMenuTitleBox)
                      (hide MenuTitleArrowRight)
                      (show MenuTitleArrowDown)))
                    (setf (text MEMenuTitle) txt)
                    (setf (text MEMenuName) (if theval 
                                              (objectstring theval :project (project theval))
                                              "")))
                  ))
(define-handler (setf outputmenuitem) :after (theval MEMenuProxy)
                (if theval
                  (progn
                    (setf (text MEItemName) (objectstring theval :project (project theval)))
                    (setf (textcolor MEItemName) black)
                    (setf (textcolor MEItemNameLabel) black))
                  (progn
                    (setf (text MEItemName) "")
                    (setf (textcolor MEItemName) gray)
                    (setf (textcolor MEItemNameLabel) gray)))
                )


;;______________________________________________________________________

(define-handler resized (MenuEditor)
  (withactorlocked (me)
    (let (hsize vsize)
      (declare (special hsize vsize))
      (sk8-multival-setf (hsize vsize) (size me))
      (call-next-method)
      (bestsize MEMenuBar)
      (setBoundsRect MEObjectsLabel 8 45 100 60)
      (setBoundsRect MEObjectLine 12 63 (- hsize 10) 65)

      (setBoundsRect MEBarNameLabel 8 70 70 85)
      (setBoundsRect MEBarName 80 70 (- hsize 20) 85)
      (setBoundsRect MEMenuNameLabel 8 90 70 105)
      (setBoundsRect MEMenuName 80 90 (- hsize 20) 105)
      (setBoundsRect MEItemNameLabel 8 110 70 125)
      (setBoundsRect MEItemName 80 110 (- hsize 20) 125)
      (setBoundsRect MEMenuLine 12 133 (- hsize 10) 135)

      (setBoundsrect MEMenuBarProxyLabel 8 140 200 155)
      (setBoundsrect MEMenubarProxy 10 165 (- hsize 10) 181)    
      (setf (top CurrentMenuProxy :resizing nil)  (bottom MEMenubarProxy))    

      (setBoundsRect MEMenuItemLine 12 263 (- hsize 10) 265)
      (setBoundsrect MESelectedMenuLabel 8 270 200 285)
      (setf (top MEMenuTitleBox :resizing nil)  295)    
      (setBoundsrect MEMenuTitle 20 295 100 316)
      (setBoundsrect MEMenuProxy 20 315 300 (- vsize 10))
      (setBoundsrect MEScroller (1- (right MEMenuProxy))  315 (+ (right MEMenuProxy) (if *MacStyleInterface* 15 12)) (- vsize 10))
      )))
(setBoundsRect MenuEditor 40 25 370 465)


 

(bringtofront MEMenuProxy)
(bringtofront MEMenuTitle)
(bringtofront MEMenubarProxy)


(resized MenuEditor)
(setf (inputobjects MenuEditor) nil)


#|

(setf (container menueditor) stage)


|#

#|
	Change History (most recent last):
	1	10/27/93	rod	
	2	10/29/93	rod	
	3	11/3/93	rod	
	4	11/3/93	kleiman	
	5	11/3/93	rod	
	6	11/5/93	rod	
	7	11/6/93	rod	
	9	11/22/93	rod	
	10	11/29/93	rod	
	11	11/30/93	rod	
	12	11/30/93	rod	
	13	11/30/93	rod	
	14	12/2/93	rod	
	15	12/3/93	rod	
	16	12/3/93	rod	
	17	12/17/93	till	#.'s be gone: evaluate, evaluate, evaluate
	19	2/12/94	kleiman	renaming
	21	2/21/94	hernan	The Final Renaming for Alpha! (yeah, right...)
	22	2/25/94	hernan	Using symbols instead of keywords for options!!!
	23	3/3/94	Hernan	The great handler argument name renaming of 94!
	25	3/7/94	rod	addparent avoided where possible
	26	3/7/94	Brian	Fixed bugs while building.
	27	3/7/94	rod	
	27	3/7/94	rod	
	27	3/7/94	rod	
	28	3/7/94	Hernan	Declaring memenusmenuitems.
	29	3/8/94	rod	
	30	3/9/94	rod	Doing Project Switching and Reference Clearing.
	31	3/11/94	rod	
	32	3/15/94	rod	Adding Drag and Drop
	33	3/15/94	rod	
	34	3/16/94	rod	
	35	3/16/94	Hernan	Declaring symbols...
	36	3/21/94	rod	
	37	3/21/94	kleiman	setvalue -> sk8::setValue
	38	3/29/94	rod	
	39	3/30/94	rod	
	42	5/6/94	rod	Removing reference to simple window.  No longer
				needed in the redesign of the UI.
	41	4/12/94	Hernan	Avoiding use of contents when necessary.
	43	6/17/94	rod	Fixing objectstring
	44	6/17/94	rod	Fixing objectstring
	45	6/21/94	rod	Fixing minor window title bug.
	46	6/23/94	Brian	
	47	6/25/94	sidney	declared something public so the file would compile and load ok
	2  	 2/27/97	Hernan  	Getting rid of UIColorize.
						497/25/94rodGetting rid of zoombox.
						50  8/30/94rod     Making resized call-next-method
						51  9/ 6/94rod     
						52  9/12/94rod     
						53  9/21/94rod     
						54 11/ 1/94rod     adding uiedit stuff and fixing default colors.
						55 11/ 8/94rod     being extra careful to clear references.
						56 11/ 8/94rod     dealing with clearing of lastselected.
						57  4/19/95rod     Fixing Resized to handle MacStyleInterface.
						2   2/14/96Brian   removing mf::docontents
|# ;(do not edit past this line!!)
