;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories


;;;

(in-package :uidev)

(SK8-declare-syms :UI :public ; Updated  2-26-94   3:48 pm
                   UI::CGEENDSLIDE   UI::CGEMENU 
                   UI::CGESTARTSLIDE UI::CRGBMENU 
                   UI::CRGBSLIDE     UI::GEMENU
                   UI::HEMENU        UI::IREMENU
                   UI::MREPICKY      UI::PMPBMENU )

;;______________________________________________________________________
;;______________________________________________________________________
;;These should be moved someplace like the system browser... ***

(defun oktoedit (me)
  (or (and (neq (project me) sk8) (neq (project me) ui))
      (yesornodialog (concatenate 'string "That object is in the " (objectstring (project me)) " project.  Edit it anyway?")
                     :cancel nil)))

(define-handler uiEdit (object)
  (when (oktoedit me)
    (if (and (listp me) (= (length me) 1))
      (uiedit (car me))
      (editobjects (if (listp me) me (list me))))))

(define-handler uiEdit (handler)
  (when (oktoedit me)
    (withCursor watchCursor
      (EditHandlerObjectDialog me))))

(define-handler uiEdit (sk8::function)
  (when (oktoedit me)
    (withCursor watchCursor
      (EditHandlerObjectDialog me))))

(define-handler uiEdit (rgbcolor)
  (when (oktoedit me)
    (if (eq me rgbcolor)
      (call-next-method)
      (getnewfromuser me :project (targetproject ui)))))

;;______________________________________________________________________
;;______________________________________________________________________
;;______________________________________________________________________
;; The Color Picker Window  ---> This is our control center for all the color editors
;;______________________________________________________________________
;;______________________________________________________________________
;;______________________________________________________________________

(new UISimpleWindow :objectname "ColorPalette" :project ui)
(setf (sk8::menubar ColorPalette) nil)
(setf (resizer ColorPalette) t)
(setf (zoombox ColorPalette) t)
(addproperty ColorPalette 'inputobject)

(setf (minimumsize colorpalette) '(265 150))
(define-handler clearReferences (ColorPalette &key ((:objects theobjects)))
  (if theobjects
    (when (or (memq (inputobject me) theobjects)
              (some #'(lambda (x) (memq (inputobject me) (parents x))) theobjects))
      (setupforproject me :project (targetproject ui)))
    (setf (items ColorPaletteColorPicker) nil))
  )

(define-handler SetUpForProject (ColorPalette &key ((:project theproject)))
  (setf (text me) (concatenate 'string (objectstring theproject :project theproject) " Color Palette"))
  (let ((par (inputobject me)))
    (if (or (not (memq (project par) (okprojects (targetproject ui))))
            (memq par (uiClearedObjects)))
      (progn
        (setf (inputobject me) rgbcolor)
        (setf (defaultmenuitem ColorPalettemenu) 1))
      (setf (inputobject me) (inputobject me)))))


(setboundsrect ColorPalette 100 100 300 310)
(resized ColorPalette)

(new Rectangle :project UI :container ColorPalette 
     :objectName "ColorPaletteNameDisplayer")
(setf (fillcolor ColorPaletteNameDisplayer) shadowedRenderer)
(setf (framesize ColorPaletteNameDisplayer) '(0 0))
(setf (textsize ColorPaletteNameDisplayer) 9)
(setf (textstyle ColorPaletteNameDisplayer) '(plain))
(setf (textlocation ColorPaletteNameDisplayer) 'centerleft)
(setf (texthoffset ColorPaletteNameDisplayer) 6)

(new colorpicker :otherparents browsercomponent :project UI :container ColorPalette 
     :objectName "ColorPaletteColorPicker")

(setf (fillcolor ColorPaletteColorPicker) uimiddle)
(setframesize ColorPaletteColorPicker 2 2)
(setf (LineColor ColorPaletteColorPicker) nil)
(setf (rowLinesSize ColorPaletteColorPicker) 3)
(setf (columnLinesSize ColorPaletteColorPicker) 3)
(setf (framecolor ColorPaletteColorPicker) FrameScore)
(setf (highlightcolor ColorPaletteColorPicker) cyan)
(define-handler (setf fillcolor) (theval ColorPaletteColorPicker)
  (call-next-method uimiddle me))

(new UIPopup :project UI :container ColorPalette :objectName "ColorPaletteMenu")
(addparent ColorPaletteMenu pickermenu)

(define-handler items (ColorPalettemenu)
  (let* ((filt (targetProject ui))
         (clears (uiClearedObjects))
         (reqprojs (and filt (cons filt (requiredprojects filt))))
         ourguys)
    (mapKnownDescendants
     renderer
     #'(lambda (i)
         (dolist (child (knownchildren i))
           (when (and (memq (project child) reqprojs)
                      (not (memq child clears)))
             (push i ourguys)
             (return)))))
    (setf ourguys (cons rgbcolor (delete rgbcolor ourguys)))
    (setf (defaultmenuitem me) (1+ (or (position (inputobject colorpalette) ourguys) 0)))
    ourguys))

(define-handler createTextDisplayItem (ColorPalettemenu theval)
  (objectstring theval :project (targetProject ui)))

(define-handler menuselect (ColorPalettemenu)
  (setf (inputobject colorpalette) (selectedItem me)))

(define-handler (setf inputobject) (theval ColorPalette)
  (setvalue 'inputobject me theval)
  (sk8::withlockedcursor animatedClock
    (withactorlocked (ColorPalette)
      (tickeventclock)
      (setf (text ColorPaletteMenu) (objectstring theval :project (targetProject ui)))
      (setf (left ColorPaletteMenu :resizing nil) 10)
      (tickeventclock)
      (setf (items ColorPalettecolorpicker) 
            (delete-duplicates (append (if (eq (targetproject ui) sk8)
                                         nil
                                         (remove-if-not #'(lambda (x) (progn (tickeventclock) (memq theval (parents x))))
                                                        (objects (targetproject ui))))
                                       (knownchildren theval))
                               :test #'eq))
      (tickeventclock)
      (selectioncompleted ColorPalettecolorpicker))))

(define-handler setboundsrect (ColorPaletteMenu left top right bottom &key physical relative justmoving)
  (call-next-method me (+ 7 *WindowLeft*) (+ 10 *WindowTop*) (+ (if relative (- (right me) (left me)) (- right left)) (+ 7 *WindowLeft*)) (+ 28 *WindowTop*) :physical nil :relative nil :justmoving nil))

(define-handler (setf items) (theval ColorPaletteColorPicker)
  (let* ((filt (targetProject ui))
         (reqprojs (and filt (cons filt (requiredprojects filt)))))
    (if filt (setf theval (delete-if #'(lambda (x) (memq x (uiClearedObjects)))
                                     (remove-if-not #'(lambda (x) (memq (project x) reqprojs)) theval))))
    (call-next-method theval me)))

(define-handler selectionCompleted (ColorPaletteColorPicker)
  (setf (text ColorPaletteNameDisplayer) 
        (objectstring (car (selecteditems me)) :project (targetproject ui))))

(define-handler doubleclick (ColorPaletteColorPicker)
  (click ColorPaletteOKButton))

(define-handler keydown (ColorPaletteColorPicker thechar)
  (if (or (eq thechar #\return) (eq thechar #\enter))
    (click ColorPaletteOKButton)
    (if (eq theChar #\Delete)
      (let ((sels (car (selecteditems me)))
            (par (inputobject colorpalette)))
        (when (and sels (DisposeDialog sels))
          (setf (items me) (knownchildren par))
          ))
      (call-next-method))))

(define-handler extendedmousedown (ColorPaletteColorPicker)
  (selectioncompleted me)  ;;; update the name
  (setf (boundsrect ObjectDataRect :physical t) (itemboundsrect me (car (selection me)) :physical t))
  (setf (object ObjectDataRect) (car (selecteditems me)))
  (setf (ComponentFrom ObjectDataRect) me)
  (withcursor standardcursor
    (let ((theguy (drag ObjectDataRect :live nil :onstage t :draggingMouseWithinEvents t 
                        :draggedOverEvents t :dropevents t)))
      (if (and (oktoselect? theguy) (neq theguy stage) (neq (project theguy) ui))
        (cond 
         ((optionkeydown)
          (undoableset 'framecolor  theguy (car (selecteditems me))))
         ((commandkeydown)
          (undoableset 'textcolor  theguy (car (selecteditems me))))
         (t
          (undoableset 'fillcolor  theguy (car (selecteditems me)))))))
    t))

(new uiButton :objectname "ColorPaletteOKButton" :project UI)
(setf (container ColorPaletteOKButton) ColorPalette)
(setf (text ColorPaletteOKButton) "Edit")

(define-handler click (ColorPaletteOKButton)
  (call-next-method)
  (uiEdit (car (selecteditems ColorPaletteColorPicker)))
  (lightforceredraw ColorPalette))

(new uiButton :objectname "ColorPaletteDialogNewButton" :project UI)
(setf (container ColorPaletteDialogNewButton) ColorPalette)
(setf (text ColorPaletteDialogNewButton) "New")
(define-handler click (ColorPaletteDialogNewButton)
  (let ((theval (getnewfromuser (inputobject colorpalette)
                                :project (targetProject ui))))
    (withactorlocked (ColorPaletteColorPicker)
      (setf (inputobject colorpalette) (inputobject colorpalette))
      (setf (selecteditems ColorPaletteColorPicker) (list theval))
      (selectionCompleted ColorPaletteColorPicker))))

(new uiButton :objectname "ColorPaletteDialogDuplicateButton" :project UI)
(setf (container ColorPaletteDialogDuplicateButton) ColorPalette)
(setf (text ColorPaletteDialogDuplicateButton) "Copy")
(define-handler click (ColorPaletteDialogDuplicateButton)
  (let ((theval (getduplicatefromuser (car (selecteditems ColorPaletteColorPicker))
                                      :project (targetProject ui))))
    (withactorlocked (ColorPaletteColorPicker)
      (setf (items ColorPaletteColorPicker) (knownchildren (inputobject colorpalette)))
      (setf (selecteditems ColorPaletteColorPicker) (list theval))
      (selectionCompleted ColorPaletteColorPicker))))


(define-handler resized (ColorPalette)
  (let (hSize vSize)
    (declare (special hSize vSize))
    (sk8-multival-setf (hSize vSize) (size me))
    (setf vsize (max vsize 100))
    (setf (v ColorPalettemenu) (+ 20 *WindowTop*))
    (setf (left  ColorPalettemenu :resizing nil) 13)
    (setBoundsRect ColorPaletteColorPicker (+ 7 *WindowLeft*) (+ 35 *WindowTop*) (- hSize (+ 5 *WindowRight*)) (- vSize 61))
    (setBoundsRect ColorPaletteNameDisplayer (+ 7 *WindowLeft*) (- vSize 57) (- hSize (+ 7 *WindowRight*)) (- vSize 40))
    (setBoundsRect ColorPaletteDialogDuplicateButton (- hSize 220) (- vSize 32) (- hSize 166) (- vSize 11))
    (setBoundsRect ColorPaletteDialogNewButton (- hSize 154) (- vSize 32) (- hSize 100) (- vSize 11))
    (setBoundsRect ColorPaletteOKButton (- hSize 88) (- vSize 32) (- hSize 34) (- vSize 11))
    (call-next-method)
    ))
(setSize ColorPalette 335 275)

(define-handler enteringStage (ColorPalette)
  (setf (text me) (concatenate 'string (objectstring (targetProject ui) :project (targetproject ui)) " Color Palette"))
  (setf (keytarget me) ColorPaletteColorPicker)
  (setf (inputobject colorpalette) rgbcolor)
  (setf (defaultmenuitem ColorPalettemenu) 1)
  (show ColorPalettemenu)
  (resized ColorPalette))



#|


________________________________________________________________________________________
9/4/96
I am removing this from the final build.  It all works fine, but the editors
aren't very good.  They too closely follow the SK8 model of things, rather than
the user model of things.  Really, we should have some Director style tools,
including a paint tool and a cast window.

We could make this into a library.
________________________________________________________________________________________


;;;;________________COMMENTING ALL OF THIS STUFF OUT FOR FINAL RELEASE________________


(define-handler uiEdit (imageRenderer)
  (when (oktoedit me)
    (withCursor watchCursor
      (withActorLocked (imageRendererEditor)
        (setf (inputRenderer imageRendererEditor) me)
        (bringup imageRendererEditor)))))

(define-handler uiEdit (gradient)
  (when (oktoedit me)
    (withCursor watchCursor
      (withActorLocked (GradientEditor)
        (setf (inputRenderer GradientEditor) me)
        (bringup GradientEditor)))))

(define-handler uiEdit (complexgradient)
  (when (oktoedit me)
    (withCursor watchCursor
      (withActorLocked (complexGradientEditor)
        (setf (inputRenderer complexGradientEditor) me)
        (bringup complexGradientEditor)))))

(define-handler uiEdit (bevelrenderer)
  (when (oktoedit me)
    (withCursor watchCursor
      (withActorLocked (BevelEditor)
        (setf (inputRenderer BevelEditor) me)
        (bringup BevelEditor)))))

(define-handler uiEdit (complexrgbcolor)
  (when (oktoedit me)
    (withCursor watchCursor
      (withActorLocked (ComplexRGBEditor)
        (setf (inputRenderer ComplexRGBEditor) me)
        (bringup ComplexRGBEditor)))))

(define-handler uiEdit (hatch)
  (when (oktoedit me)
    (withCursor watchCursor
      (withActorLocked (hatchEditor)
        (setf (inputRenderer hatchEditor) me)
        (bringup hatchEditor)))))

(define-handler uiEdit (multirenderer)
  (when (oktoedit me)
    (withCursor watchCursor
      (withActorLocked (multirendererEditor)
        (setf (inputRenderer multirendererEditor) me)
        (bringup multirendererEditor)))))



;;______________________________________________________________________
;;______________________________________________________________________
;;______________________________________________________________________
;;______________________________________________________________________
;;A few toys to play with...
;;______________________________________________________________________
;;______________________________________________________________________
;;______________________________________________________________________
;;______________________________________________________________________



(new rectangle :ObjectName "SimpleRendererEditor" :project UI
     :properties '(salientProperties propertyValues))
(setf (fillcolor SimpleRendererEditor) uimiddle)
(addparent SimpleRendererEditor BrowserComponent)
(new roundrect :ObjectName "SimpleRendererEditorView" :project UI)
(setf (container SimpleRendererEditorView) SimpleRendererEditor)
(setf (fillcolor SimpleRendererEditorView) uimiddle)
(tagpart SimpleRendererEditor SimpleRendererEditorView 'View)
(define-handler resized (SimpleRendererEditor) 
  (sk8-multival-bind (hh vv) (size me)
    (setboundsrect (view me) 10 10 (- hh 10) (- vv 10))))

(define-handler inputRenderer (SimpleRendererEditor)
  (let ((f (fillcolor (view me))))
    (if (eq f uimiddle)
      nil
      f)))
(define-handler (setf inputRenderer) (theval SimpleRendererEditor)
  (if theval
    (let ((props (salientProperties me))
          (vals nil))
      (setf (fillcolor (view me)) theval)
      (dolist (i props)
        (setf vals (cons (funcall i theval) vals)))
      (setf (propertyValues me) (nreverse vals)))
    (progn
      (setf (fillcolor (view me)) uimiddle)
      (setf (propertyValues me) nil))
    ))
(define-handler reset (SimpleRendererEditor)
  (let ((obj (inputRenderer me)) setter (vals (propertyValues me)))
    (when obj
      (undoableset (salientProperties me) obj vals)
      (lightforceredraw me))))

(define-handler uiColorize (SimpleRendererEditor)
  (setf (fillcolor me) uimiddle)
  (setf (framecolor me) framescore)
  (setf (framesize me) '(2 2))
  )

;;______________________________________________________________________
;; RendererEditorWindow: our prototype window...

(new UISimpleWindow :ObjectName "RendererEditorWindow" :project ui
     :properties '(inputRenderer))
(setf (sk8::menubar RendererEditorWindow) nil)
(setf (resizer RendererEditorWindow) nil)
(setf (zoombox RendererEditorWindow) nil)


(define-handler clearReferences (RendererEditorWindow &key ((:objects theobjects)))
  (if (or (not theobjects)
          (memq (inputrenderer me) theobjects))
    (setf (inputRenderer me) nil))
  )

(define-handler SetUpForProject (RendererEditorWindow &key ((:project theproject)))
  (when (inputRenderer me)
    (unless (memq (project (inputRenderer me)) (okprojects theproject))
      (setf (inputRenderer me) nil))))


(define-handler updateEditor (RendererEditorWindow &key (propertyname nil) (items nil))
  (let ((obj (inputrenderer me))
        (re (simplerenderereditor me))
        oldvals)
    (when (and obj (memq obj items) (memq propertyname (salientproperties re)))
      (setf oldvals (propertyValues re))
      (setf (inputrenderer me) (inputrenderer me))
      (setf (propertyValues re) oldvals)
      )))

(setBoundsRect RendererEditorWindow 110 124 518 366)

(define-handler (setf container) (theval RendererEditorWindow)
  (call-next-method)
  (unless theval
    (setf (inputrenderer me) nil)
    ))

(define-handler resized (RendererEditorWindow)
  (withactorlocked (me)
    (let (hSize vSize)
      (declare (special hSize vSize))
      (sk8-multival-setf (hSize vSize) (size me))
      (mapc #'bestsize (contents me))
      (call-next-method)
      )))

(new GetObjectField :objectName "REWGetField" :project UI)
(setf (container REWGetField) RendererEditorWindow)
(setf (textsize (textfield REWGetField)) 9)
(tagpart RendererEditorWindow REWGetField 'GetField)
(setf (fillcolor REWGetField) ShadowWhite)
(define-handler bestSize (REWGetField)
  (declare (special hsize vsize))
  (setboundsrect me 10 30 (- hsize 10) 50))
(define-handler (setf outputobject) (theval REWGetField)
  (call-next-method)
  (setf (inputRenderer (container me)) theval))

(define-handler targetProject (REWGetField) (targetproject ui))

(define-handler (setf Highlight) (theval REWGetField)
  (withactorlocked (me)
    (setf (inverts me) nil)
    (if theval
      (progn
        (setselection (textfield me) 0 -1)
        (setf (fillcolor (textfield me)) white))
      (setf (fillcolor (textfield me)) shadowwhite)
      )
    (call-next-method)
    (setf (framecolor me) black)))

(new uiButton :ObjectName "REWSetButton" :project ui)
(setf (container REWSetButton) RendererEditorWindow)
(setf (text REWSetButton) "Redraw")
(tagpart RendererEditorWindow  REWSetButton 'SetButton)
(define-handler bestSize (REWSetButton)
  (declare (special hsize vsize))
  (unless (< vsize 30)
    (setboundsrect me (- hsize 69) (- vsize 30) (- hsize 15) (- vsize 9))))
(define-handler click (REWSetButton)
  (sk8::withlockedcursor animatedclock
    (let* ((ig (inputrenderer (sk8::window me)))
          (okprojects (append (requiringprojects (project ig)) (okprojects (project ig)))))
      (lightforceredraw ColorPaletteColorPicker)
      (when ig
        (dolist (win (delete-if-not #'(lambda (x) (memq (project x) okprojects)) (contents stage)))
          (dolist (i (cons win (deepcontents win)))
            (when (and (inheritsfrom i actor)
                       (or (eq (fillcolor i) ig)
                           (eq (framecolor i) ig)
                           (eq (textcolor i) ig)))
              (tickeventclock)
              (lightforceredraw i))))))))

(new uiButton :ObjectName "REWResetButton" :project ui)
(setf (container REWResetButton) RendererEditorWindow)
(setf (text REWResetButton) "Reset")
(tagpart RendererEditorWindow  REWResetButton 'ResetButton)
(define-handler bestSize (REWResetButton)
  (declare (special hsize vsize))
  (unless (< vsize 30)
    (setboundsrect me (- hsize 135) (- vsize 30) (- hsize 81) (- vsize 9))))
(define-handler click (REWResetButton)
  (when (inputRenderer (sk8::window me))
    (sk8::withlockedcursor animatedclock
      (reset (simpleRendererEditor (sk8::window me)))
      (setf (inputRenderer (sk8::window me)) (inputRenderer (sk8::window me)))
      (lightforceredraw (simpleRendererEditor (sk8::window me)))
      )))


;;______________________________________________________________________
;;For a popup of choices

(new rectangle :ObjectName "PopupBox" :project ui)
(setframesize PopupBox 2 2)
(setf (mousesensitivity PopupBox) 'transparent)
(setf (framecolor PopupBox) FrameScore)
(setf (fillcolor PopupBox) uimiddle)
(define-handler bestSize (PopupBox)
  (declare (special hsize vsize))
  (setboundsrect me 10 140 160 210))

(new uibiglabel :objectname "PopupBoxLabel" :project ui)
(setf (container PopupBoxLabel) PopupBox)
(tagpart PopupBox  PopupBoxLabel 'label )

(new PickerMenu :ObjectName "PopupBoxmenu" :project ui)
(addparent popupboxmenu uipopup)
(setf (container PopupBoxmenu) PopupBox)
(setf (text PopupBoxmenu) "SET ME!!!")
(tagpart PopupBox  PopupBoxmenu 'menu)
(define-handler menuselect (PopupBoxmenu)
  (let ((item (selecteditem me)))
    (withactorlocked (me)
      (setf (text me) (objectstring Item  :project (targetproject ui)))
      (resized (container me)))
    ))
(define-handler mouseenter (PopupBoxmenu)
  (setf (cursor stage) standardcursor))
(define-handler mousedown (PopupBoxmenu)
  (when (inputRenderer (sk8::window me))
    (call-next-method)
    (setf (keytarget (sk8::window me)) (sk8::window me))
    ))

(define-handler resized (PopupBox)
  (setf (left (label me) :resizing nil) 5)
  (setf (top (label me) :resizing nil) 5)
  (setlocation (menu me) (round (width me) 2) 35))

;;;;
;;;;  PenModeGetter
;;;;

(new PopupBox :ObjectName "PenModePopUpBox" :project ui
     :properties '(InputPenMode))

(define-handler (setf inputPenMode) :after (theval PenModePopUpBox)
                (setf (selecteditem (menu me)) theval)
                (menuselect (menu me)))

(setf (text (label PenModePopUpBox)) "Pen Mode:")
(setf (objectname (menu PenModePopUpBox)) "PMPBMenu")
(setf (items PMPBMenu) '(srccopy srcor srcxor srcbic notsrccopy
                         notsrcor notsrcxor notsrcbic patcopy 
                         pator patxor patbic notpatcopy notpator
                         notpatxor notpatbic grayishtextor Blend addpin
                         addover subpin addmax admax subover admin
                         dithercopy Transparent highlight))

(setf (text PMPBMenu) (CreateTextDisplayItem PMPBMenu 'patcopy))

;;______________________________________________________________________
;;For getting a rgbcolor from the wheel

(new rectangle :ObjectName "SimpleRGBColorGetter" :project ui
     :properties '(outputcolor inputcolor))
(setframesize SimpleRGBColorGetter 2 2)
(setf (framecolor SimpleRGBColorGetter) FrameScore)
(setf (fillcolor SimpleRGBColorGetter) uimiddle)
(define-handler initialize (SimpleRGBColorGetter original isNew initArgs)
  (declare (ignore original isNew initArgs))
  (call-next-method)
  (setf (fillcolor (swatch me)) (new rgbcolor :project (project me)))
  )

(define-handler (setf InputColor) (theval SimpleRGBColorGetter)
  (sk8::setValue 'inputcolor me theval)
  (if theval
    (if (inheritsfrom theval rgbcolor)
      (progn
        (setf (forered (fillcolor (swatch me))) (forered theval))
        (setf (foreblue (fillcolor (swatch me))) (foreblue theval))
        (setf (foregreen (fillcolor (swatch me))) (foregreen theval))
        (lightforceredraw me)
        )
      (error "Need to set it to a rgbcolor"))
    (progn
      (setf (forered (fillcolor (swatch me))) 0)
      (setf (foreblue (fillcolor (swatch me))) 0)
      (setf (foregreen (fillcolor (swatch me))) 0)
      (lightforceredraw me)
      )))

(new uibiglabel :objectname "SRCGLabel" :project ui)
(setf (text SRCGLabel) "Start Color")
(setf (container SRCGLabel) SimpleRGBColorGetter)
(tagpart SimpleRGBColorGetter  SRCGLabel 'label )
(setf (mousesensitivity SRCGLabel) 'transparent) ;; HEW

(new rectangle :ObjectName "SRCGSwatch" :project ui)
(setf (container SRCGSwatch) SimpleRGBColorGetter)
(tagpart SimpleRGBColorGetter  SRCGSwatch 'Swatch )
(setf (mousesensitivity SRCGSwatch) 'transparent) ;; HEW

(define-handler resized (SimpleRGBColorGetter)
  (sk8-multival-bind (hh vv) (size me)
    (setf (left (label me) :resizing nil) 5)
    (setf (v (label me)) (round vv 2))
    (setboundsrect (swatch me) (- hh vv 2) 5 (- hh 5) (- vv 5))
    ))

(define-handler (setf Highlight) (theval SimpleRGBColorGetter)
  (setf (inverts me) nil)
  (if theval
    (setf (frameColor me) cyan)
    (setf (frameColor me) FrameScore)
    )
  (call-next-method))

(define-handler draggingMouseEnter (SimpleRGBColorGetter actorDragged)
  (when (and (inputRenderer (sk8::window me))
             (or (and (eq actorDragged ObjectDataRect) (inheritsfrom (object objectdatarect) rgbcolor)) 
                 (and (eq actorDragged Propertydatarect) (inheritsfrom (value Propertydatarect) rgbcolor))))
    (setf (highlight me) t)))

(define-handler draggingMouseLeave (SimpleRGBColorGetter actorDragged)
  (when (highlight me) (setf (highlight me) nil)))

(define-handler dropped (SimpleRGBColorGetter droppee)
  (when (inputRenderer (sk8::window me))
    (let (newcol)
      (withActorLocked (me)
        (draggingMouseLeave me droppee)
        (cond
         ((and (eq droppee ObjectDataRect) (inheritsfrom (object objectdatarect) rgbcolor))
          (setf newcol (object objectdatarect)))
         ((and (eq droppee Propertydatarect) (inheritsfrom (value Propertydatarect) rgbcolor))
          (setf newcol (value Propertydatarect))))
        (when newcol
          (withActorLocked (me)
            (bringToFront (sk8::window me))
            (setf (inputColor me) newcol)
            (setf (outputcolor me) (fillcolor me))
            (setf (keytarget (sk8::window me)) (sk8::window me))
            )
          )))))

(define-handler mousedown (SimpleRGBColorGetter)
  (when (inputRenderer (sk8::window me))
    (getnewfromuser (fillcolor (swatch me)) :project (targetproject ui))
    (lightforceredraw me)
    (setf (outputcolor me) (fillcolor me))
    (setf (keytarget (sk8::window me)) (sk8::window me))
    ))
(define-handler mouseenter (SimpleRGBColorGetter)
  (setf (cursor stage) standardcursor))

(setsize SimpleRGBColorGetter 100 30)
(resized SimpleRGBColorGetter)

;;______________________________________________________________________
;;For getting a renderer...

(new SimpleRGBColorGetter :ObjectName "simpleRendererGetter" :project ui)
(setframesize simpleRendererGetter 2 2)
(define-handler initialize (simpleRendererGetter original isNew initArgs)
  (declare (ignore original isNew initArgs))
  (call-next-method)
  (setf (fillcolor (swatch me)) black)
  )

(define-handler (setf InputColor) (theval simpleRendererGetter)
  (sk8::setValue 'inputcolor me theval)
  (if theval
    (if (inheritsfrom theval renderer)
      (setf (fillcolor (swatch me)) theval)
      (error "Need to set it to a renderer"))
    (setf (fillcolor (swatch me)) black)))

(define-handler draggingMouseEnter (simpleRendererGetter actorDragged)
  (when (and (inputRenderer (sk8::window me))
             (or (and (eq actorDragged ObjectDataRect) (inheritsfrom (object objectdatarect) renderer)) 
                 (and (eq actorDragged Propertydatarect) (inheritsfrom (value Propertydatarect) renderer))))
    (setf (highlight me) t)))

(define-handler dropped (simpleRendererGetter droppee)
  (when (inputRenderer (sk8::window me))
    (let (newcol)
      (withActorLocked (me)
        (draggingMouseLeave me droppee)
        (cond
         ((and (eq droppee ObjectDataRect) (inheritsfrom (object objectdatarect) renderer))
          (setf newcol (object objectdatarect)))
         ((and (eq droppee Propertydatarect) (inheritsfrom (value Propertydatarect) renderer))
          (setf newcol (value Propertydatarect))))
        (when newcol
          (withActorLocked (me)
            (bringToFront (sk8::window me))
            (setf (inputcolor me) newcol)
            (setf (outputcolor me) newcol)
            (setf (keytarget (sk8::window me)) (sk8::window me))
            )
          )))))

(define-handler mousedown (simpleRendererGetter)
  (when (inputRenderer (sk8::window me))
    (undoableset 'fillcolor (swatch me) (getfromuser renderer 
                                                     :project (targetproject ui)
                                                     :multiplevalues nil 
                                                     ))
    (lightforceredraw me)
    (setf (outputcolor me) (fillcolor (swatch me)))
    (setf (keytarget (sk8::window me)) (sk8::window me))
    ))
(define-handler mouseenter (simpleRendererGetter)
  (setf (cursor stage) standardcursor))

(setsize simpleRendererGetter 100 30)
(resized simpleRendererGetter)

;;______________________________________________________________________
;;For getting a Pattern Resource

(new SimpleRGBColorGetter :ObjectName "PatternGetter" :project ui)
(setframesize PatternGetter 2 2)
(define-handler initialize (PatternGetter original isNew initArgs)
  (declare (ignore original isNew initArgs))
  (call-next-method)
  (setf (fillcolor (swatch me)) (new imagerenderer :project (project me) :media BlackPatternRSRC))
  )

(define-handler inputPattern (PatternGetter)
  (media (fillcolor (swatch me))))

(define-handler (setf inputPattern) (theval PatternGetter)
  (if theval
    (if (inheritsfrom theval bwpattern)
      (setf (media (fillcolor (swatch me))) theval)
      (error "Can only accept BWPatterns!"))
    (setf (media (fillcolor (swatch me))) BlackPatternRSRC))
  (lightforceredraw me))


(define-handler draggingMouseEnter (PatternGetter actorDragged)
  (when (inputRenderer (sk8::window me))
    (let (theval)
      (cond
       ((eq actorDragged ObjectDataRect) 
        (setf theval (object objectdatarect)))
       ((eq actorDragged Propertydatarect)
        (setf theval (value Propertydatarect))))
      (if (or (inheritsfrom theval bwpattern)
              (and (inheritsfrom theval imagerenderer)
                   (inheritsfrom (media theval) bwpattern)))
        (setf (highlight me) t)))))

(define-handler dropped (PatternGetter droppee)
  (when (inputRenderer (sk8::window me))
    (let (newcol theval)
      (withActorLocked (me)
        (draggingMouseLeave me droppee)
        (cond
         ((eq droppee ObjectDataRect) 
          (setf theval (object objectdatarect)))
         ((eq droppee Propertydatarect)
          (setf theval (value Propertydatarect))))
        (if (inheritsfrom theval bwpattern)
          (setf newcol theval))
        (if (and (inheritsfrom theval imagerenderer)
                 (inheritsfrom (media theval) bwpattern))
          (setf newcol (media theval)))
        (when newcol
          (withActorLocked (me)
            (bringToFront (sk8::window me))
            (setf (media (fillcolor (swatch me))) newcol)
            (setf (fillcolor (swatch me)) (fillcolor (swatch me)))
            (setf (outputcolor me) newcol)
            )
          )))))

(define-handler mousedown (PatternGetter)
  (let (newbie)
    (when (inputRenderer (sk8::window me))
      (setf newbie (getfromuser bwpattern :project (targetproject ui)))
      (setf (media (fillcolor (swatch me))) newbie)
      (setf (fillcolor (swatch me)) (fillcolor (swatch me)))
      (setf (outputcolor me) newbie)
      )))
(define-handler mouseenter (PatternGetter)
  (setf (cursor stage) standardcursor))

(setsize PatternGetter 100 30)
(resized PatternGetter)

;;______________________________________________________________________
;;For getting a value from 0 to 65535

(new rectangle :ObjectName "OpColorSlider" :project ui)
(setframesize OpColorSlider 2 2)
(setf (framecolor OpColorSlider) FrameScore)
(setf (fillcolor OpColorSlider) uimiddle)
(setf (mousesensitivity OpColorSlider) 'transparent) ;; HEW

(new uibiglabel :objectname "OCSLabel" :project ui)
(setf (text OCSLabel) "Op Color")
(setf (container OCSLabel) OpColorSlider)
(tagpart OpColorSlider  OCSLabel 'label )
(setf (mousesensitivity OCSLabel) 'transparent) ;; HEW
(define-handler (setf inputpenmode) (theval OpColorSlider)
  (withactorlocked (me)
    (cond
     ((eql theval 'blend) (setf (text (label me)) "Degree of Blend"))
     ((eql theval 'subPin) (setf (text (label me)) "Pinned Color"))
     ((eql theval 'addPin) (setf (text (label me)) "Pinned Color"))
     (t (setf (text (label me)) "Slider has no effect")))
    (resized me)))

(new slider :ObjectName "OCSSlider" :project ui)
(setf (container OCSSlider) OpColorSlider)
(tagpart OpColorSlider  OCSSlider 'Slider)
(setf (minimumValue OCSSlider) 0)
(setf (maximumValue OCSSlider) 65535)
(setf (scrollStep OCSSlider) 1)
(setf (pageStep OCSSlider) 1000)
(setf (thumbview OCSSlider) 1)

(define-handler resized (OpColorSlider)
  (sk8-multival-bind (hh vv) (size me)
    (setf (left (label me) :resizing nil) 5)
    (setf (top (label me) :resizing nil) 5)
    (setboundsrect (Slider me) 10 25 (- hh 10) 50)
    ))
(setsize OpColorSlider 100 60)
(resized OpColorSlider)


;;______________________________________________________________________
;;______________________________________________________________________
;; GradientEditor

(new RendererEditorWindow :ObjectName "GradientEditor" :project ui)
(setf (text GradientEditor) "Gradient Editor")
(setf (inputPrototype (GetField GradientEditor)) gradient)

(new simpleRendererEditor :ObjectName "GESimpleGradientEditor" :project ui)
(setf (container GESimpleGradientEditor) GradientEditor)
(setf (salientProperties GESimpleGradientEditor) '(direction startred startgreen startblue endred endgreen endblue scalefactor))
(tagpart GradientEditor  GESimpleGradientEditor 'simpleRendererEditor)
(define-handler bestSize (GESimpleGradientEditor)
  (declare (special hsize vsize))
  (setboundsrect me 150 60 (- hsize 10) (- vsize 35)))

(new SimpleRGBColorGetter :ObjectName "GEStartColor" :project ui)
(setf (container GEStartColor) GradientEditor)
(setf (text (label GEStartColor)) "Start Color:")
(tagpart GradientEditor  GEStartColor 'StartColor)
(define-handler bestSize (GEStartColor)
  (declare (special hsize vsize))
  (setboundsrect me 10 60 140 90))
(define-handler   (setf outputcolor) :after (theval GEStartColor)
                  (withcursor watchcursor
                    (let ((rend (inputrenderer (sk8::window me)))
                          (newobj (fillcolor (swatch me))))
                      (unless (and rend theval (= (forered newobj) (startred rend)) (= (foreblue newobj) (startblue rend)) (= (foreGreen newobj) (startgreen rend)))
                        (undoableset (list 'startred 'startblue 'startgreen) rend (list (forered newobj) (foreblue newobj) (foreGreen newobj)))
                        (lightForceRedraw (view (simpleRendererEditor (sk8::window me))))))))

(new SimpleRGBColorGetter :ObjectName "GEEndColor" :project ui)
(setf (container GEEndColor) GradientEditor)
(setf (text (label GEEndColor)) "End Color:")
(tagpart GradientEditor  GEEndColor 'EndColor)
(define-handler bestSize (GEEndColor)
  (declare (special hsize vsize))
  (setboundsrect me 10 100 140 130))
(define-handler   (setf outputcolor) :after (theval GEEndColor)
                  (withcursor watchcursor
                    (let ((rend (inputrenderer (sk8::window me)))
                          (newobj (fillcolor (swatch me))))
                      (unless (and rend theval (= (forered newobj) (endred rend)) (= (foreblue newobj) (endblue rend)) (= (foreGreen newobj) (endGreen rend)))
                        (undoableset (list 'endred 'endblue 'endGreen) rend (list (forered newobj) (foreblue newobj) (foreGreen newobj)))
                        (lightForceRedraw (view (simpleRendererEditor (sk8::window me))))))))

(new PopupBox :ObjectName "SimpleGradientDirectionGetter" :project ui)
(setf (container SimpleGradientDirectionGetter) gradienteditor)
(tagpart gradienteditor  SimpleGradientDirectionGetter 'SimpleDirectionGetter)
(define-handler bestSize (SimpleGradientDirectionGetter)
  (declare (special hsize vsize))
  (setboundsrect me 10 140 140 200))
(setf (text (label SimpleGradientDirectionGetter)) "Direction:")
(setf (objectname (menu SimpleGradientDirectionGetter)) "GEmenu")
(setf (items GEmenu) '(horizontal vertical rect oval shape)) ;; HEW
(setf (text GEmenu) (objectstring 'horizontal)) ;; HEW
(define-handler menuSelect (GEmenu)
  (let ((item (selecteditem me))
        (wind (inputRenderer (sk8::window me))))
    (unless (and wind (eq item (direction wind)))
      (undoableset 'direction wind item)
      (lightforceredraw (view (simpleRendererEditor (sk8::window me))))
      (call-next-method)
      )))

(define-handler (setf inputRenderer) (theval gradienteditor)
  (setvalue 'inputrenderer me theval)
  (call-next-method)
  (if (inheritsfrom theval gradient)
    (withActorLocked (me)
      (setf (text (textfield (GetField me))) (objectstring theval :project (targetproject ui)))
      
      (setf (forered (fillcolor (swatch (startcolor me)))) (startred theval))
      (setf (foreblue (fillcolor (swatch (startcolor me)))) (startblue theval))
      (setf (foreGreen (fillcolor (swatch (startcolor me)))) (startGreen theval))
      
      (setf (forered (fillcolor (swatch (EndColor me)))) (endred theval))
      (setf (foreblue (fillcolor (swatch (EndColor me)))) (endblue theval))
      (setf (foreGreen (fillcolor (swatch (EndColor me)))) (endGreen theval))
      
      (setf (text (menu (SimpleDirectionGetter me))) (objectstring (direction theval)))
      
      (lightForceRedraw (startcolor me))
      (lightForceRedraw (EndColor me))
      (setf (inputRenderer (simpleRendererEditor me)) theval) 
      )
    (withActorLocked (me)
      (setf (text (textfield (GetField me))) "")
      (setf (inputColor (startcolor me)) nil)
      (setf (inputColor (EndColor me)) nil)
      (setf (text (menu (SimpleDirectionGetter me))) (objectstring 'horizontal))
      (setf (inputRenderer (simpleRendererEditor me)) nil) 
      )))

(setf (inputRenderer gradienteditor) nil)
(resized gradienteditor)
(setboundsrect gradienteditor 100 100 425 309)

;;______________________________________________________________________
;;______________________________________________________________________
;; ComplexGradientEditor

(new GradientEditor :ObjectName "ComplexGradientEditor" :project ui)
(setf (text ComplexGradientEditor) "ComplexGradient Editor")
(setf (inputPrototype (GetField ComplexGradientEditor)) ComplexGradient)

(setf (objectname (simpleRendererEditor complexgradienteditor)) "CGESimpleGradientEditor")
(setf (salientProperties CGESimpleGradientEditor) '(direction startred startgreen startblue endred endgreen endblue scalefactor penmode endopred endopgreen endopblue startopred startopgreen startopblue))
(define-handler bestSize (CGESimpleGradientEditor)
  (declare (special hsize vsize))
  (setboundsrect me 170 60 (- hsize 10) (- vsize 35)))

(new PenModePopUpBox :ObjectName "CGEPenModeGetter" :project ui)
(setf (container CGEPenModeGetter) complexgradienteditor)
(tagpart complexgradienteditor  CGEPenModeGetter 'PenModeGetter)
(setf (text (label CGEPenModeGetter)) "Pen Mode:")
(setf (objectname (menu CGEPenModeGetter)) "CGEMenu")
(setf (text CGEMenu) (CreateTextDisplayItem CGEMenu 'patcopy))
(define-handler MenuSelect (CGEMenu)
  (let ((item (selecteditem me))
        (rend (inputRenderer (sk8::window me))))
    (call-next-method)
    (setf (inputpenmode (startOpColor (sk8::window me))) item)
    (setf (inputpenmode (endOpColor (sk8::window me))) item)
    (setf (text me) (CreateTextDisplayItem me Item))
    (unless (and rend (eq item (penmode rend)))
      (undoableset 'penmode rend item)
      (lightforceredraw (view (simpleRendererEditor (sk8::window me))))
      )))
(setf (items CGEMenu) '(srccopy srcor srcxor srcbic notsrccopy
                        notsrcor notsrcxor notsrcbic patcopy 
                        pator patxor patbic notpatcopy notpator
                        notpatxor notpatbic grayishtextor Blend addpin
                        addover subpin addmax admax subover admin
                        dithercopy Transparent highlight))

(new OpColorSlider :ObjectName "CGEStartOpSlider" :project ui)
(setf (text (label CGEStartOpSlider)) "Start Op Color:")
(setf (container CGEStartOpSlider) complexgradienteditor)
(tagpart complexgradienteditor  CGEStartOpSlider 'startOpColor)
(setf (objectname (slider CGEStartOpSlider )) "CGEStartSlide")

(define-handler (setf currentValue) (value CGEStartSlide)
  (call-next-method)
  (let* ((val (round value))
         (re (simpleRendererEditor (sk8::window me)))
         (obj (inputrenderer re)))
    (when (and obj (not (= (startopred obj) val)) (not (= (startOpBlue obj) val)) (not (= (startOpGreen obj) val)))
      (undoableset (list 'startOpRed 'startOpBlue 'startOpGreen) obj (list val val val))
      (lightforceredraw (view re)))))

(new OpColorSlider :ObjectName "CGEEndOpSlider" :project ui)
(setf (text (label CGEEndOpSlider)) "End Op Color:")
(setf (container CGEEndOpSlider) complexgradienteditor)
(tagpart complexgradienteditor  CGEEndOpSlider 'EndOpColor)
(setf (objectname (slider CGEEndOpSlider )) "CGEEndSlide")
(define-handler (setf currentValue) (value CGEEndSlide)
  (call-next-method)
  (let* ((val (round value))
         (re (simpleRendererEditor (sk8::window me)))
         (obj (inputrenderer (sk8::window me))))
    (when (and obj (not (= (endOpRed obj) val)) (not (= (endOpBlue obj) val)) (not (= (endOpGreen obj) val)))
      (undoableset (list 'endOpRed 'endOpBlue 'endOpGreen) obj (list val val val))
      (lightforceredraw (view re))))
  )

(define-handler bestSize ((startcolor complexgradienteditor))
  (declare (special hsize vsize))
  (setboundsrect me 10 60 160 90))

(define-handler bestSize (CGEStartOpSlider)
  (declare (special hsize vsize))
  (setboundsrect me 10 100 160 160))

(define-handler bestSize ((endcolor complexgradienteditor))
  (declare (special hsize vsize))
  (setboundsrect me 10 170 160 200))

(define-handler bestSize (CGEEndOpSlider)
  (declare (special hsize vsize))
  (setboundsrect me 10 210 160 270))

(define-handler bestSize (CGEPenModeGetter)
  (declare (special hsize vsize))
  (setboundsrect me 10 280 160 335))

(define-handler bestSize ((simpledirectiongetter complexgradienteditor))
  (declare (special hsize vsize))
  (setboundsrect me 10 345 160 400))

(define-handler (setf inputRenderer) (theval complexgradienteditor)
  (call-next-method)
  (if (inheritsfrom theval complexgradient)
    (withActorLocked (me)
      (setf (currentValue (slider (startOpColor me))) (startopRed theval))
      (setf (currentValue (slider (endOpColor me))) (endOpRed theval))
      (setf (inputpenmode (PenModeGetter me)) (penmode theval))
      )
    (withActorLocked (me)
      (setf (currentValue (slider (startOpColor me))) 0)
      (setf (currentValue (slider (endOpColor me))) 0)
      (setf (inputpenmode (PenModeGetter me)) nil)
      )))

(setf (inputRenderer complexgradienteditor) nil)
(setboundsrect complexgradienteditor 85 30 425 440)
(resized complexgradienteditor)

;;______________________________________________________________________
;;______________________________________________________________________
;; BevelEditor

(new RendererEditorWindow :ObjectName "BevelEditor" :project ui)
(setf (text BevelEditor) "Bevel Editor")

(setf (inputPrototype (GetField BevelEditor)) BevelRenderer)

(new simpleRendererEditor :ObjectName "BevelDisplay" :project ui)
(setf (salientProperties BevelDisplay) '(toprenderer leftrenderer rightrenderer bottomrenderer))
(setf (container BevelDisplay) BevelEditor)
(tagpart BevelEditor  BevelDisplay 'simplerenderereditor)
;;to fake rectangleness:
(setf (roundedness (view BevelDisplay)) '(0 0))
(define-handler resized (BevelDisplay)
  (setboundsrect (view me) 0 0 (width me) (height me)))
(addparent BevelDisplay Halo)
(setf (insetSize BevelDisplay) '(10 10))
(setf (framesize BevelDisplay) '(0 0))
(setf (framesize (view BevelDisplay)) '(0 0))
#|
(new Halo :ObjectName "BevelDisplay" :project ui)
(setf (insetSize BevelDisplay) '(10 10))
(setf (fillcolor BevelDisplay) (new bevelRenderer :project ui))
|#

(define-handler bestSize (BevelDisplay)
  (declare (special hsize vsize))
  (setboundsrect me 160 100 (- hsize 160) (- vsize 75)))


(new simpleRendererGetter :ObjectName "BETop" :project ui)
(setf (container BETop) BevelEditor)
(setf (text (label BETop)) "Top Renderer:")
(tagpart BevelEditor  BETop 'TopRenderer)
(setsize BETop 150 30)
(define-handler bestSize (BETop)
  (declare (special hsize vsize))
  (setlocation me (round hsize 2) 80))
(define-handler   (setf outputcolor) :after (theval BETop)
                  (let ((rend (inputRenderer (sk8::window me))))
                    (unless (and rend (eq theval (topRenderer rend)))
                      (undoableset 'topRenderer rend theval)
                      (lightforceredraw (view (simpleRendererEditor (sk8::window me)))))))

(new simpleRendererGetter :ObjectName "BELeft" :project ui)
(setf (container BELeft) BevelEditor)
(setf (text (label BELeft)) "Left Renderer:")
(tagpart BevelEditor  BELeft 'leftRenderer)
(setsize BELeft 150 30)
(define-handler bestSize (BELeft)
  (declare (special hsize vsize))
  (setboundsrect me (+ *WindowLeft* 4) (+ 45 (round (- vsize 80) 2)) 150 (+ 75 (round (- vsize 80) 2)))
  )
(define-handler   (setf outputcolor) :after (theval BELeft)
                  (let ((rend (inputRenderer (sk8::window me))))
                    (unless (and rend (eq theval (leftRenderer rend)))
                      (undoableset 'leftRenderer rend theval)
                      (lightforceredraw (view (simpleRendererEditor (sk8::window me)))))))

(new simpleRendererGetter :ObjectName "BEBottom" :project ui)
(setf (container BEBottom) BevelEditor)
(setf (text (label BEBottom)) "Bottom Renderer:")
(tagpart BevelEditor  BEBottom 'BottomRenderer)
(setsize BEBottom 170 30)
(define-handler bestSize (BEBottom)
  (declare (special hsize vsize))
  (setlocation me (round hsize 2) (- vsize 50)))
(define-handler (setf outputcolor) :after (theval BEBottom)
                (let ((rend (inputRenderer (sk8::window me))))
                    (unless (and rend (eq theval (BottomRenderer rend)))
                      (undoableset 'BottomRenderer rend theval)
                      (lightforceredraw (view (simpleRendererEditor (sk8::window me)))))))

(new simpleRendererGetter :ObjectName "BERight" :project ui)
(setf (container BERight) BevelEditor)
(setf (text (label BERight)) "Right Renderer:")
(tagpart BevelEditor  BERight 'RightRenderer)
(setsize BERight 150 30)
(define-handler bestSize (BERight)
  (declare (special hsize vsize))
  (setboundsrect me (- hsize *WindowLeft* 150) (+ 45 (round (- vsize 80) 2)) (- hsize *WindowLeft* 4) (+ 75 (round (- vsize 80) 2)))
  )
(define-handler (setf outputcolor) :after (theval BERight)
                (let ((rend (inputRenderer (sk8::window me))))
                    (unless (and rend (eq theval (RightRenderer rend)))
                      (undoableset 'RightRenderer rend theval)
                      (lightforceredraw (view (simpleRendererEditor (sk8::window me)))))))

(define-handler (setf inputRenderer) (theval BevelEditor)
  (setvalue 'inputrenderer me theval)
  (if (inheritsfrom theval BevelRenderer)
    (withActorLocked (me)
      (setf (text (textfield (getfield me))) (objectstring theval :project (targetproject ui)))
      (setf (fillcolor (swatch (topRenderer me))) (topRenderer theval))
      (setf (fillcolor (swatch (leftRenderer me))) (leftRenderer theval))
      (setf (fillcolor (swatch (rightRenderer me))) (rightRenderer theval))
      (setf (fillcolor (swatch (bottomRenderer me))) (bottomRenderer theval))
      (setf (inputRenderer (simpleRendererEditor me)) theval) 
      )
    (withActorLocked (me)
      (setf (text (textfield (getfield me))) "")
      (setf (fillcolor (swatch (topRenderer me))) black)
      (setf (fillcolor (swatch (leftRenderer me))) black)
      (setf (fillcolor (swatch (rightRenderer me))) black)
      (setf (fillcolor (swatch (bottomRenderer me))) black)
      (setf (inputRenderer (simpleRendererEditor me)) nil) 
      ))
  )

(setf (inputRenderer BevelEditor) nil)
(setboundsrect BevelEditor 90 140 495 380)

;;______________________________________________________________________
;;______________________________________________________________________
;; ComplexRGBEditor

(new RendererEditorWindow :ObjectName "ComplexRGBEditor" :project ui)
(setf (text ComplexRGBEditor) "ComplexRGBColor Editor")

(setf (inputPrototype (GetField ComplexRGBEditor)) complexrgbcolor)

(new simpleRendererEditor :ObjectName "GESimpleComplexRGBEditor" :project ui)
(setf (container GESimpleComplexRGBEditor) ComplexRGBEditor)
(tagpart ComplexRGBEditor  GESimpleComplexRGBEditor 'simpleRendererEditor)
(define-handler bestSize (GESimpleComplexRGBEditor)
  (declare (special hsize vsize))
  (setboundsrect me 170 60 (- hsize 10) (- vsize 35)))
(setf (salientProperties GESimpleComplexRGBEditor) '(penmode opred opgreen opblue forered foregreen foreblue backred backgreen backblue media))

(new SimpleRGBColorGetter :ObjectName "CRGBForeColorGetter" :project ui)
(setf (container CRGBForeColorGetter) ComplexRGBEditor)
(setf (text (label CRGBForeColorGetter)) "Fore Color:")
(tagpart ComplexRGBEditor  CRGBForeColorGetter 'ForeColor)
(define-handler bestSize (CRGBForeColorGetter)
  (declare (special hsize vsize))
  (setboundsrect me 10 60 160 90))
(define-handler   (setf outputcolor) :after (theval CRGBForeColorGetter)
                  (withcursor watchcursor
                    (let* ((curInput (fillcolor (swatch me)))
                           (re (simpleRendererEditor (sk8::window me)))
                           (obj (inputrenderer re)))
                      (when (and obj (not (= (forered obj) (forered curInput))) (not (= (foreblue obj) (foreblue curInput))) (not (= (foreGreen obj) (foreGreen curInput))))
                        (undoableset (list 'forered 'foreblue 'foreGreen) obj (list (forered curInput) (foreblue curInput) (foreGreen curInput)))
                        (lightforceredraw (view re))))))

(new SimpleRGBColorGetter :ObjectName "CRGBBackColorGetter" :project ui)
(setf (container CRGBBackColorGetter) ComplexRGBEditor)
(setf (text (label CRGBBackColorGetter)) "Back Color:")
(tagpart ComplexRGBEditor  CRGBBackColorGetter 'BackColor)
(define-handler bestSize (CRGBBackColorGetter)
  (declare (special hsize vsize))
  (setboundsrect me 10 100 160 130))
(define-handler   (setf outputcolor) :after (theval CRGBBackColorGetter)
                  (withcursor watchcursor
                    (let* ((curInput (fillcolor (swatch me)))
                           (re (simpleRendererEditor (sk8::window me)))
                           (obj (inputrenderer re)))
                      (when (and obj (not (= (backRed obj) (forered curInput))) (not (= (backblue obj) (foreblue curInput))) (not (= (backGreen obj) (foreGreen curInput))))
                        (undoableset (list 'backRed 'backblue 'backGreen) obj (list (forered curInput) (foreblue curInput) (foreGreen curInput)))
                        (lightforceredraw (view re))))))

(new PatternGetter :ObjectName "CRGBPatternGetter" :project ui)
(setf (container CRGBPatternGetter) ComplexRGBEditor)
(setf (text (label CRGBPatternGetter)) "Pattern:")
(tagpart ComplexRGBEditor  CRGBPatternGetter 'bwPattern)
(define-handler bestSize (CRGBPatternGetter)
  (declare (special hsize vsize))
  (setboundsrect me 10 140 160 170))
(define-handler   (setf outputcolor) :after (theval CRGBPatternGetter)
                  (let ((rend (inputRenderer (sk8::window me))))
                    (unless (and rend (eq theval (media rend)))
                      (undoableset 'media rend theval)
                      (lightforceredraw (view (simpleRendererEditor (sk8::window me))))))
                  )

(new PenModePopUpBox :ObjectName "PenModeGetter" :project ui)
(setf (container PenModeGetter) ComplexRGBEditor)
(tagpart ComplexRGBEditor  PenModeGetter 'PenModeGetter)
(define-handler bestSize (PenModeGetter)
  (declare (special hsize vsize))
  (setboundsrect me 10 180 160 235))
(setf (text (label PenModeGetter)) "Pen Mode:")
(setf (objectname (menu PenModeGetter)) "CRGBMenu")
(setf (text CRGBMenu) (CreateTextDisplayItem CRGBMenu 'patcopy)) ;; Said #$Invert
(define-handler MenuSelect (CRGBMenu)
  (let ((item (selecteditem me))
        (rend (inputRenderer (sk8::window me))))
    (call-next-method)
    (setf (inputpenmode (OpColor (sk8::window me))) item)
    (setf (text me) (CreateTextDisplayItem me Item))
    (unless (and rend (eq (penmode rend) item))
      (undoableset 'penmode (inputRenderer (sk8::window me)) item)
      (lightforceredraw (view (simpleRendererEditor (sk8::window me))))
      )))

(new OpColorSlider :ObjectName "CRGBOpColorSlider" :project ui)
(setf (text (label CRGBOpColorSlider)) "Op Color:")
(setf (container CRGBOpColorSlider) ComplexRGBEditor)
(tagpart ComplexRGBEditor  CRGBOpColorSlider 'OpColor)
(define-handler bestSize (CRGBOpColorSlider)
  (declare (special hsize vsize))
  (setboundsrect me 10 245 160 305))
(setf (objectname (slider CRGBOpColorSlider )) "CRGBSlide")
(define-handler (setf currentValue) (value CRGBSlide)
  (call-next-method)
  (let* ((val (round value))
         (re (simpleRendererEditor (sk8::window me)))
         (obj (inputrenderer (sk8::window me))))
    (when (and obj (not (= (OpRed obj) val)) (not (= (OpBlue obj) val)) (not (= (OpGreen obj) val)))
      (undoableset (list 'OpRed 'OpBlue 'OpGreen) obj (list val val val))
      (lightforceredraw (view re)))))

(define-handler (setf inputRenderer)  (theval ComplexRGBEditor)
  (setvalue 'inputRenderer me theval)
  (if (inheritsfrom theval complexrgbcolor)
    (withActorLocked (me)
      (setf (text (textfield (getfield me))) (objectstring theval :project (targetproject ui)))
      
      (setf (forered (fillcolor (swatch (Forecolor me)))) (Forered theval))
      (setf (foreblue (fillcolor (swatch (Forecolor me)))) (Foreblue theval))
      (setf (foreGreen (fillcolor (swatch (Forecolor me)))) (ForeGreen theval))
      
      (setf (forered (fillcolor (swatch (BackColor me)))) (Backred theval))
      (setf (foreblue (fillcolor (swatch (BackColor me)))) (Backblue theval))
      (setf (foreGreen (fillcolor (swatch (BackColor me)))) (BackGreen theval))
      
      (setf (currentValue (slider (OpColor me))) (opRed theval))
      (setf (inputpenmode (PenModeGetter me)) (penmode theval))
      
      (lightForceRedraw (Forecolor me))
      (lightForceRedraw (BackColor me))
      (setf (inputpattern (bwPattern me)) (media theval))
      (setf (inputRenderer (simpleRendererEditor me)) theval) 
      )
    (withActorLocked (me)
      (setf (text (textfield (GetField me))) "")
      (setf (inputColor (Forecolor me)) nil)
      (setf (inputColor (BackColor me)) nil)
      (setf (currentValue (slider (OpColor me))) 0)
      (setf (inputPenMode (PenModeGetter me)) nil)
      (setf (inputPattern (bwPattern me)) nil)
      (setf (inputRenderer (simpleRendererEditor me)) nil) 
      
      )))

(setf (inputRenderer ComplexRGBEditor) nil)
(setboundsrect ComplexRGBEditor 100 75 430 390)
(resized ComplexRGBEditor)

;;______________________________________________________________________
;;______________________________________________________________________
;; HatchEditor

(new RendererEditorWindow :ObjectName "HatchEditor" :project ui)
(setf (text HatchEditor) "Hatch Editor")

(setf (inputPrototype (GetField HatchEditor)) Hatch)

(new simpleRendererEditor :ObjectName "HESimpleHatchEditor" :project ui)
(setf (container HESimpleHatchEditor) HatchEditor)
(setf (salientProperties HESimpleHatchEditor) '(backgroundRenderer foreGroundRenderer spacing pensize hatchtype startat))
(tagpart HatchEditor  HESimpleHatchEditor 'simpleRendererEditor)
(define-handler bestSize (HESimpleHatchEditor)
  (declare (special hsize vsize))
  (setboundsrect me 165 60 (- hsize 10) (- vsize 35)))

(new SimpleRendererGetter :ObjectName "HEForeRenderer" :project ui)
(setf (container HEForeRenderer) HatchEditor)
(setf (text (label HEForeRenderer)) "Foreground:")
(tagpart HatchEditor  HEForeRenderer 'ForeGroundRenderer)
(define-handler bestSize (HEForeRenderer)
  (declare (special hsize vsize))
  (setboundsrect me 10 60 155 90))
(define-handler   (setf outputcolor) :after (theval HEForeRenderer)
                  (let ((rend (inputRenderer (sk8::window me))))
                    (unless (eq (foreGroundRenderer rend) (fillcolor (swatch me)))
                      (undoableset 'foreGroundRenderer rend (fillcolor (swatch me)))
                      (lightforceredraw (view (simpleRendererEditor (sk8::window me)))))))

(new SimpleRendererGetter :ObjectName "HEbackgroundRenderer" :project ui)
(setf (container HEbackgroundRenderer) HatchEditor)
(setf (text (label HEbackgroundRenderer)) "Background:")
(tagpart HatchEditor  HEbackgroundRenderer 'backgroundRenderer)
(define-handler bestSize (HEbackgroundRenderer)
  (declare (special hsize vsize))
  (setboundsrect me 10 100 155 130))
(define-handler   (setf outputcolor) :after (theval HEbackgroundRenderer)
                  (let ((rend (inputRenderer (sk8::window me))))
                    (unless (eq (backgroundRenderer rend) (fillcolor (swatch me)))
                      (undoableset 'backgroundRenderer rend (fillcolor (swatch me)))
                      (lightforceredraw (view (simpleRendererEditor (sk8::window me)))))))

(new PopupBox :ObjectName "SimpleHatchDirectionGetter" :project ui)
(setf (container SimpleHatchDirectionGetter) HatchEditor)
(tagpart HatchEditor  SimpleHatchDirectionGetter 'SimpleDirectionGetter)
(define-handler bestSize (SimpleHatchDirectionGetter)
  (declare (special hsize vsize))
  (setboundsrect me 10 140 155 195))
(setf (text (label SimpleHatchDirectionGetter)) "Direction:")
(setf (objectname (menu SimpleHatchDirectionGetter)) "HEmenu")
(setf (items HEmenu) '(horizontal vertical both)) ;; HEW
(setf (text HEmenu) (objectstring 'horizontal)) ;; HEW
(define-handler MenuSelect (HEmenu)
  (let ((item (selecteditem me))
        (rend (inputRenderer (sk8::window me))))
    (unless (and rend (eq (hatchtype rend) item))
      (undoableset 'hatchtype rend item)
      (lightforceredraw (view (simpleRendererEditor (sk8::window me))))
      (call-next-method)
      )))

(new twonumberpropertyeditor :ObjectName "SimplePenSizeGetter" :project ui)
(setf (propertyname SimplePenSizeGetter) 'pensize)
(setf (container SimplePenSizeGetter) HatchEditor)
(setf (fillcolor SimplePenSizeGetter) uimiddle)
(setf (framecolor SimplePenSizeGetter) framescore)
(setf (framesize SimplePenSizeGetter) '(2 2))
(copytextproperties (label SimplePenSizeGetter) uibiglabel)
(setf (fillcolor (label SimplePenSizeGetter)) uimiddle)
(tagpart HatchEditor  SimplePenSizeGetter 'PenSizeGetter)
(define-handler bestSize (SimplePenSizeGetter)
  (declare (special hsize vsize))
  (setboundsrect me 10 205 155 235))
(setf (text (label SimplePenSizeGetter)) "Pen Size:")
(define-handler writeValue (SimplePenSizeGetter)
  (call-next-method)
  (lightForceRedraw (simpleRendererEditor (sk8::window me))))
(define-handler resized (SimplePenSizeGetter)
  (sk8-multival-bind (hh vv) (size me)
    (setBoundsRect (label me) 1 1 hh (1- vv))
    (setBoundsRect (number1 me) (- hh 80) 5 (- hh 45) (- vv 5))
    (setBoundsRect (number2 me) (- hh 40) 5 (- hh 6) (- vv 5))))

(new numberpropertyeditor :ObjectName "SimpleSpacingGetter" :project ui)
(setf (propertyname SimpleSpacingGetter) 'spacing)
(setf (container SimpleSpacingGetter) HatchEditor)
(setf (fillcolor SimpleSpacingGetter) uimiddle)
(setf (framecolor SimpleSpacingGetter) framescore)
(setf (framesize SimpleSpacingGetter) '(2 2))
(copytextproperties (label SimpleSpacingGetter) uibiglabel)
(setf (fillcolor (label SimpleSpacingGetter)) uimiddle)
(tagpart HatchEditor  SimpleSpacingGetter 'spacingGetter)
(define-handler bestSize (SimpleSpacingGetter)
  (declare (special hsize vsize))
  (setboundsrect me 10 245 155 275))
(setf (text (label SimpleSpacingGetter)) "Spacing:")
(define-handler writeValue (SimpleSpacingGetter)
  (call-next-method)
  (lightForceRedraw (simpleRendererEditor (sk8::window me))))
(define-handler resized (SimpleSpacingGetter)
  (sk8-multival-bind (hh vv) (size me)
    (setBoundsRect (label me) 1 1 hh (1- vv))
    (setBoundsRect (number1 me) (- hh 46) 5 (- hh 6) (- vv 5))))

(define-handler (setf inputRenderer) (theval HatchEditor)
  (setvalue 'inputrenderer me theval)
  (if (inheritsfrom theval Hatch)
    (withActorLocked (me)
      (setf (text (textfield (getfield me))) (objectstring theval :project (targetproject ui)))
      (setf (inputcolor (foreGroundRenderer me)) (foreGroundRenderer theval))
      (setf (inputcolor (backgroundRenderer me)) (backgroundRenderer theval))
      (setf (text (menu (SimpleDirectionGetter me))) (objectstring (hatchtype theval)))
      (setf (objects (PenSizeGetter me)) (list theval))
      (setf (objects (spacingGetter me)) (list theval))
      (lightForceRedraw (foreGroundRenderer me))
      (lightForceRedraw (backgroundRenderer me))
      (setf (inputRenderer (simpleRendererEditor me)) theval) 
      )
    (withActorLocked (me)
      (setf (text (textfield (GetField me))) "")
      (setf (objects (PenSizeGetter me)) nil)
      (setf (objects (spacingGetter me)) nil)
      (setf (inputColor (foreGroundRenderer me)) black)
      (setf (inputColor (backgroundRenderer me)) black)
      (setf (text (menu (SimpleDirectionGetter me))) (objectstring 'horizontal))
      (setf (inputRenderer (simpleRendererEditor me)) nil) 
      )))

(setf (inputRenderer HatchEditor) nil)
(resized HatchEditor)
(setboundsrect hatcheditor 100 100 425 385)

;;______________________________________________________________________
;;______________________________________________________________________
;; MultiRendererEditor

(new RendererEditorWindow :ObjectName "MultiRendererEditor" :project ui)
(setf (text MultiRendererEditor) "MultiRenderer Editor")

(setf (inputPrototype (GetField MultiRendererEditor)) MultiRenderer)

(new SimpleRendererEditor :ObjectName "MRESimpleMultiRendererEditor" :project ui)
(setf (container MRESimpleMultiRendererEditor) MultiRendererEditor)
(setf (salientProperties MRESimpleMultiRendererEditor) '(rendererlist))
(tagpart MultiRendererEditor  MRESimpleMultiRendererEditor 'simpleRendererEditor)
(define-handler bestSize (MRESimpleMultiRendererEditor)
  (declare (special hsize vsize))
  (setboundsrect me 190 60 (- hsize 10) (- vsize 35)))

(new uitextlist :objectname "MRERendererPile" :project ui)
(setf (pickerPrototype MRERendererPile) objectpilepicker)
(setf (text (titlebar MRERendererPile)) "Renderer Pile")
(setf (editing (picker MRERendererPile)) t)
(setf (container MRERendererPile) MultiRendererEditor)
(tagpart MultiRendererEditor  MRERendererPile 'RendererPile )
(define-handler bestSize (MRERendererPile)
  (declare (special hsize vsize))
  (setboundsrect me *windowLeft* 60 170 (- vsize *windowBottom*)))
(setf (objectname (picker MRERendererPile)) "MREPicky")
(define-handler (setf items) :after (theval MREPicky)
                (let ((rend (inputRenderer (sk8::window me))))
                  (unless (and rend (equal (rendererlist rend) theval))
                    (undoableset 'rendererlist rend theval)
                    (lightforceredraw (view (simpleRendererEditor (sk8::window me)))))))

(define-handler draggingMouseEnter (MREPicky actorDragged)
  (when (and (eq actordragged objectdatarect) (inheritsfrom (object objectdatarect) renderer))
    (call-next-method)))
(define-handler draggingMouseWithin (MREPicky actorDragged)
  (when (and (eq actordragged objectdatarect) (inheritsfrom (object objectdatarect) renderer))
    (call-next-method)))
(define-handler dropped (MREPicky actorDragged)
  (when (and (eq actordragged objectdatarect) (inheritsfrom (object objectdatarect) renderer))
    (call-next-method)))
(define-handler keydown (MREPicky thechar)
  (when (and (items me) (selecteditems me)) 
    (if (eq thechar #\delete)
      (setf (items me) (remove (car (selecteditems me)) (items me)))
      (call-next-method)))
  )

(define-handler (setf inputRenderer) (ourval MultiRendererEditor)
  (setvalue 'inputrenderer me ourval)
  (if (inheritsfrom ourval MultiRenderer)
    (withActorLocked (me)
      (setf (text (textfield (GetField me))) (objectstring ourval :project (targetproject ui)))
      
      (setf (inputobjects (picker (rendererpile me))) (rendererlist ourval))
      (setf (inputRenderer (simpleRendererEditor me)) ourval) 
      )
    (withActorLocked (me)
      (setf (text (textfield (GetField me))) "")
      (setf (inputobjects (picker (rendererpile me))) nil)
      (setf (inputRenderer (simpleRendererEditor me)) nil) 
      )))

(setf (inputRenderer MultiRendererEditor) nil)
(resized MultiRendererEditor)
(setboundsrect MultiRendererEditor 100 100 455 340)

;;______________________________________________________________________
;;______________________________________________________________________
;; ImageRendererEditor

(new RendererEditorWindow :ObjectName "ImageRendererEditor" :project ui)
(setf (text ImageRendererEditor) "ImageRenderer Editor")

(setf (inputPrototype (GetField ImageRendererEditor)) ImageRenderer)

(new simpleRendererEditor :ObjectName "SimpleImageRendererEditor" :project ui)
(setf (container SimpleImageRendererEditor) ImageRendererEditor)
(setf (salientProperties SimpleImageRendererEditor) '(backgroundrenderer media renderstyle translucentcolor hoffset voffset))
(tagpart ImageRendererEditor  SimpleImageRendererEditor 'SimpleRendererEditor)
(define-handler bestSize (SimpleImageRendererEditor)
  (declare (special hsize vsize))
  (setboundsrect me 180 60 (- hsize 10) (- vsize 35)))

;;to fake rectangleness:
(setf (roundedness (view SimpleImageRendererEditor)) '(0 0))
(define-handler resized (SimpleImageRendererEditor)
  (setboundsrect (view me) 0 0 (width me) (height me)))

(new SimpleRendererGetter :ObjectName "IREbackgroundRenderer" :project ui)
(setf (container IREbackgroundRenderer) ImageRendererEditor)
(setf (text (label IREbackgroundRenderer)) "Background:")
(tagpart ImageRendererEditor  IREbackgroundRenderer 'backgroundRenderer)
(define-handler bestSize (IREbackgroundRenderer)
  (declare (special hsize vsize))
  (setboundsrect me 10 60 170 90))
(define-handler   (setf outputcolor) :after (ourval IREbackgroundRenderer)
                  (let ((rend (inputRenderer (sk8::window me))))
                    (unless (and rend (eq (backgroundRenderer rend) (fillcolor (swatch me))))
                      (undoableset 'backgroundRenderer (inputrenderer (sk8::window me)) (fillcolor (swatch me)))
                      (lightForceRedraw (view (simpleRendererEditor (sk8::window me)))))))

(new PopupBox :ObjectName "RenderStyleGetter" :project ui)
(setf (container RenderStyleGetter) ImageRendererEditor)
(tagpart ImageRendererEditor  RenderStyleGetter 'SimpleRenderStyleGetter)
(define-handler bestSize (RenderStyleGetter)
  (declare (special hsize vsize))
  (setboundsrect me 10 100 170 160))
(setf (text (label RenderStyleGetter)) "Render Style:")
(setf (objectname (menu RenderStyleGetter)) "IREmenu")
(setf (items IREmenu) '(Renderstretched Renderunstretched Rendertiled))
(setf (text IREmenu) (objectstring 'Renderstretched))
(define-handler MenuSelect (IREmenu)
  (let ((item (selecteditem me))
        (rend (inputRenderer (sk8::window me))))
    (unless (and rend (eq (renderstyle rend) item))
      (undoableset 'renderstyle (inputrenderer (sk8::window me)) Item)
      (lightForceRedraw (view (simpleRendererEditor (sk8::window me))))
      (call-next-method)
      )))

(new twonumberpropertyeditor :ObjectName "IREOffsetGetter" :project ui)
(setf (container IREOffsetGetter) ImageRendererEditor)
(setf (fillcolor IREOffsetGetter) uimiddle)
(setf (framecolor IREOffsetGetter) framescore)
(setf (framesize IREOffsetGetter) '(2 2))
(copytextproperties (label IREOffsetGetter) uibiglabel)
(setf (fillcolor (label IREOffsetGetter)) uimiddle)
(tagpart ImageRendererEditor  IREOffsetGetter 'IREOffsetGetter)
(define-handler bestSize (IREOffsetGetter)
  (declare (special hsize vsize))
  (setboundsrect me 10 170 170 200))
(setf (text (label IREOffsetGetter)) "Offset:")
(define-handler value (IREOffsetGetter)
  (let ((obj (inputrenderer (sk8::window me))))
    (if obj
      (list (hoffset obj) 
            (voffset obj))
      (list 0 0))))
(define-handler writeValue (IREOffsetGetter)
  (let ((val (currentvalue me))
        (obj (inputrenderer (sk8::window me))))
    (unless (and obj (= (hoffset obj) (car val)) (= (voffset obj) (cadr val)))
      (undoableset (list 'hoffset 'voffset) obj (list (car val) (cadr val)))
      (lightForceRedraw (simpleRendererEditor (sk8::window me))))))
(define-handler resized (IREOffsetGetter)
  (sk8-multival-bind (hh vv) (size me)
    (setBoundsRect (label me) 1 1 hh (1- vv))
    (setBoundsRect (number1 me) (- hh 92) 5 (- hh 52) (- vv 5))
    (setBoundsRect (number2 me) (- hh 46) 5 (- hh 6) (- vv 5))))


(define-handler (setf inputRenderer) (theval ImageRendererEditor)
  (setvalue 'inputrenderer me theval)
  (if (inheritsfrom theval imagerenderer)
    (withActorLocked (me)
      (setf (text (textfield (getfield me))) (objectstring theval :project (targetproject ui)))
      (setf (objects (IREOffsetGetter me)) (list (fillcolor (simpleRendererEditor me))))                    
      (setf (inputcolor (backgroundRenderer me)) (backgroundRenderer theval))
      (setf (text (menu (SimpleRenderStyleGetter me))) (objectstring (renderstyle theval)))
      (setf (inputRenderer (simpleRendererEditor me)) theval) 
      )
    (withActorLocked (me)
      (setf (text (textfield (GetField me))) "")
      (setf (objects (IREOffsetGetter me)) nil)                    
      (setf (inputcolor (backgroundRenderer me)) black)
      (setf (text (menu (SimpleRenderStyleGetter me))) (objectstring nil))
      (setf (inputRenderer (simpleRendererEditor me)) nil) 
      ))
  )

(setf (inputRenderer ImageRendererEditor) nil)
(resized ImageRendererEditor)
(setboundsrect ImageRendererEditor 100 100 430 309)

;;;;________________COMMENTING ALL OF THIS STUFF OUT FOR FINAL RELEASE________________
|#




#|
	Change History (most recent last):
	1	12/10/93	rod	
	75 	12/ 4/94	rod     	fixing redraw color.
	76 	12/ 5/94	sidney  	move definition of uicolor of SimpleRendererEditor to this file
	77 	12/ 5/94	sidney  	removed a damn set container to the stage.
	78 	12/ 5/94	rod     	Fixing undo updates.
	79 	12/ 5/94	rod     	Finally making multiple undo work nicely.  Speeding up redraw.
	80 	12/ 5/94	rod     	various fixes.
	85 	 2/16/95	sidney  	readable argument names for initialize handler
	2  	 2/14/96	Brian   	fixing all external package references.
	2  	 9/ 5/96	Brian   	Commenting out everything but the color palette.  They 
						take up half a meg of memory and are rarely used.
	3  	 2/27/97	Hernan  	
|# ;(do not edit past this line!!)
