;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories


;;;

(in-package :UIDevelopment)

;; for debugging
(eval-when (:compile-toplevel :load-toplevel)
(unless (equal (dotsize SelectionHaloResizer) '(6 6))
  (error "1: (dotsize SelectionHaloResizer) somehow got munged in build to ~s"
         (dotsize SelectionHaloResizer))))


(SK8-declare-syms :UI :public ; Updated  5-04-94   1:20 pm
                  UI::DRAWTOOLS UI::TOOLSECTION)


;; for debugging
(eval-when (:compile-toplevel :load-toplevel)
(unless (equal (dotsize SelectionHaloResizer) '(6 6))
  (error "2: (dotsize SelectionHaloResizer) somehow got munged in build to ~s"
         (dotsize SelectionHaloResizer))))


;; Symbols inserted into the appropriate preload file  2-25-94   2:36 am
(SK8-declare-syms :UI :special
                  UI::SELECTTOOL)

;; for debugging
(eval-when (:compile-toplevel :load-toplevel)
(unless (equal (dotsize SelectionHaloResizer) '(6 6))
  (error "3: (dotsize SelectionHaloResizer) somehow got munged in build to ~s"
         (dotsize SelectionHaloResizer))))

;;______________________________________________________________________
;;______________________________________________________________________
;; DrawPaletteTool Palette

(new UISimpleWindow :objectName "DrawPalette" :project ui
     :properties '(currentTool lockedPalette items targetproject))
(boundsrect drawpalette)
(setboundsrect drawpalette 5 20 144 303)
(setf (currentTool DrawPalette) nil)
(setf (doFirstClick DrawPalette) t)
(setf (sk8::menubar DrawPalette) nil)
(setf (resizer DrawPalette) nil)
(setf (zoombox DrawPalette) nil)

;; for debugging
(eval-when (:compile-toplevel :load-toplevel)
(unless (equal (dotsize SelectionHaloResizer) '(6 6))
  (error "-1: (dotsize SelectionHaloResizer) somehow got munged in build to ~s"
         (dotsize SelectionHaloResizer))))

(defun storeTools (proj tools)
  (when (or (eq proj sk8) (not (inheritsfrom proj project))) (setf proj nil))
  (when proj
    (setf (items drawpalette) (delete-if #'(lambda (x) (eq (car x) proj)) (items drawpalette)))
    (push (cons proj tools) (items drawpalette)))
  )
(defun gettools (proj)
  (let ((tools (list nil)))
    (dolist (i (items drawpalette))
      (if (eq (car i) proj) 
        (setf tools (cdr i))))
    tools
    ))
(defun setupfor (proj)
  (let ((tools (gettools proj)))
    (if (car tools)
      (unless (equal (inputobjects userpalette) tools) (setf (inputobjects userpalette) tools))
      (when (inputobjects userpalette) (setf (inputobjects userpalette) nil))))
  (lightforceredraw userpalette))

;; for debugging
(eval-when (:compile-toplevel :load-toplevel)
(unless (equal (dotsize SelectionHaloResizer) '(6 6))
  (error "0: (dotsize SelectionHaloResizer) somehow got munged in build to ~s"
         (dotsize SelectionHaloResizer))))

(define-handler clearReferences (DrawPalette &key ((:objects theobjects)))
  (when theobjects
    (if (is-a (car theobjects) project)
      (progn
        (setf (items me) (delete-if #'(lambda (x) (eq (car x) (car theobjects))) (items me)))
        (setf (inputobjects userpalette) (remove-if #'(lambda (x) (memq (project x) (okprojects (car theobjects)))) (inputobjects userpalette)))
        )
      (withActorLocked (me)
        (setf (inputobjects userpalette) (remove-if #'(lambda (x) (memq x theobjects)) (inputobjects userpalette)))
        (setf (inputobjects librarypalette :includeselection nil) (remove-if #'(lambda (x) (memq x theobjects)) (inputobjects librarypalette)))
        (storeTools (targetProject ui) (remove-if #'(lambda (x) (memq x theobjects)) (gettools (targetProject ui))))
        ))))

;; for debugging
(eval-when (:compile-toplevel :load-toplevel)
(unless (equal (dotsize SelectionHaloResizer) '(6 6))
  (error "1: (dotsize SelectionHaloResizer) somehow got munged in build to ~s"
         (dotsize SelectionHaloResizer))))

;;; This is lame in that it doesn't remember the objects.
(define-handler SetUpForProject (DrawPalette &key ((:project theproject)))
  (unless (memq (project (selecteditem librarypalettemenu)) (okprojects theproject))
    (setf (selecteditem librarypalettemenu) sk8)
    (menuselect librarypalettemenu))
  (setupfor theproject)
  )

;; for debugging
(eval-when (:compile-toplevel :load-toplevel)
(unless (equal (dotsize SelectionHaloResizer) '(6 6))
  (error "2: (dotsize SelectionHaloResizer) somehow got munged in build to ~s"
         (dotsize SelectionHaloResizer))))

(define-handler deactivate (DrawPalette)
  (if (neq (activeMode) pickactormode) (call-next-method)))

;; for debugging
(eval-when (:compile-toplevel :load-toplevel)
(unless (equal (dotsize SelectionHaloResizer) '(6 6))
  (error "3: (dotsize SelectionHaloResizer) somehow got munged in build to ~s"
         (dotsize SelectionHaloResizer))))

(define-handler activate (DrawPalette)
  (when (neq (activeMode) pickactormode)
    (call-next-method)))

;; for debugging
(eval-when (:compile-toplevel :load-toplevel)
(unless (equal (dotsize SelectionHaloResizer) '(6 6))
  (error "4: (dotsize SelectionHaloResizer) somehow got munged in build to ~s"
         (dotsize SelectionHaloResizer))))

;;;----------------------------------
(new halo :objectname "DTHighlighter" :project ui)

;; for debugging
(eval-when (:compile-toplevel :load-toplevel)
(unless (equal (dotsize SelectionHaloResizer) '(6 6))
  (error "5: (dotsize SelectionHaloResizer) somehow got munged in build to ~s"
         (dotsize SelectionHaloResizer))))

(setf (container DTHighlighter) DrawPalette)

;; for debugging
(eval-when (:compile-toplevel :load-toplevel)
(unless (equal (dotsize SelectionHaloResizer) '(6 6))
  (error "6: (dotsize SelectionHaloResizer) somehow got munged in build to ~s"
         (dotsize SelectionHaloResizer))))

(setf (insetsize DTHighlighter) '(2 2))

;; for debugging
(eval-when (:compile-toplevel :load-toplevel)
(unless (equal (dotsize SelectionHaloResizer) '(6 6))
  (error "7: (dotsize SelectionHaloResizer) somehow got munged in build to ~s"
         (dotsize SelectionHaloResizer))))

(setf (fillcolor DTHighlighter) uilightcolor)

;; for debugging
(eval-when (:compile-toplevel :load-toplevel)
(unless (equal (dotsize SelectionHaloResizer) '(6 6))
  (error "8: (dotsize SelectionHaloResizer) somehow got munged in build to ~s"
         (dotsize SelectionHaloResizer))))

(moveoffstage DTHighlighter)

;; for debugging
(eval-when (:compile-toplevel :load-toplevel)
(unless (equal (dotsize SelectionHaloResizer) '(6 6))
  (error "9: (dotsize SelectionHaloResizer) somehow got munged in build to ~s"
         (dotsize SelectionHaloResizer))))

(tagpart DrawPalette DTHighlighter 'Highlighter)
;;(setf (objectname DTHighlighter) nil)
;; for debugging
(eval-when (:compile-toplevel :load-toplevel)
(unless (equal (dotsize SelectionHaloResizer) '(6 6))
  (error "10: (dotsize SelectionHaloResizer) somehow got munged in build to ~s"
         (dotsize SelectionHaloResizer))))

;;______________________________________________________________________
;;______________________________________________________________________
;; Popuppalette stuff

(new PopUpPaletteItem :objectname "RectCornerToCorner" :project ui 
     :colorToUse uiColorRectCornerToCorner :valuetoReturn 'cornertocorner)
(new PopUpPaletteItem :objectname "rectCenterToCorner" :project ui 
     :colorToUse uiColorRectCenterToCorner :valuetoReturn 'centerOut)
(new PopUpPaletteItem :objectname "OvalCornerToCorner" :project ui 
     :colorToUse uiColorovalCornerToCorner :valuetoReturn 'cornertocorner)
(new PopUpPaletteItem :objectname "OvalCenterToCorner" :project ui 
     :colorToUse uiColorOvalCenterToCorner :valuetoReturn 'centerOut)
(new PopUpPaletteItem :objectname "RRectCornerToCorner" :project ui 
     :colorToUse uiColorRRectCornerToCorner :valuetoReturn 'cornertocorner)
(new PopUpPaletteItem :objectname "RrectCenterToCorner" :project ui 
     :colorToUse uiColorRRectCenterToCorner :valuetoReturn 'centerOut)
(new PopUpPaletteItem :objectname "PolygonPolygon" :project ui 
     :colorToUse uiColorPolygon :valuetoReturn 'Points)
(new PopUpPaletteItem :objectname "PolygonRegular" :project ui 
     :colorToUse uiColorRegularPolygon :valuetoReturn 'Regular)
(new PopUpPaletteItem :objectname "PolygonFreehand" :project ui 
     :colorToUse uiColorFreehand :valuetoReturn 'FreeHand)
(new PopUpPaletteItem :objectname "PolygonSymegon" :project ui 
     :colorToUse uiColorSymegon :valuetoReturn 'Symegon)
(new PopUpPaletteItem :objectname "LineSimpleLine" :project ui 
     :colorToUse uiColorSimpleLine :valuetoReturn 'SimpleLine)
(new PopUpPaletteItem :objectname "SelectionRectGrow" :project ui 
     :colorToUse uiColorselectRectGrow :valuetoReturn :RectGrow)
(new PopUpPaletteItem :objectname "SelectionRectShrink" :project ui 
     :colorToUse uiColorselectRectShrink :valuetoReturn :RectShrink)
(new PopUpPaletteItem :objectname "SelectionLassoGrow" :project ui 
     :colorToUse uiColorselectLassoGrow :valuetoReturn :LassoGrow)
(new PopUpPaletteItem :objectname "SelectionLassoShrink" :project ui 
     :colorToUse uiColorselectLassoShrink :valuetoReturn :LassoShrink)

;; for debugging
(eval-when (:compile-toplevel :load-toplevel)
(unless (equal (dotsize SelectionHaloResizer) '(6 6))
  (error "11: (dotsize SelectionHaloResizer) somehow got munged in build to ~s"
         (dotsize SelectionHaloResizer))))

(defun colorForSelectionTool ()
  (let ((options (getpopupitems 'selection))
        (val uiColorselectRectGrow) ;;default in case of errors...
        (currentstyle (styletodraw selecttool)))
    (dolist (i options)
      (if (eq (valuetoreturn i) currentstyle)
        (setf val (colorToUse i))))
    val)
  )

(defun GetPopUpItems (act)
  (if (equal act 'SELECTION)
    (list SelectionRectGrow SelectionRectShrink SelectionLassoGrow SelectionLassoShrink)
    (cond 
     ((or (eq act oval) (inheritsFrom act oval)) (list OvalCornerToCorner OvalCenterToCorner))
     ((or (eq act RoundRect) (inheritsFrom act RoundRect)) (list RRectCornerToCorner RrectCenterToCorner))
     ((or (eq act linesegment) (inheritsFrom act linesegment)) (list LineSimpleLine))
     ((or (eq act polygon) (inheritsFrom act polygon)) (list PolygonPolygon PolygonRegular PolygonFreehand PolygonSymegon))
     (t (list RectCornerToCorner rectCenterToCorner)))))

;;______________________________________________________________________
;;______________________________________________________________________
;; DrawPaletteTool prototype

(new drawtool :project ui :objectname "DrawPaletteTool")
(setf (ActorToDraw DrawPaletteTool) nil)
(addproperty DrawPaletteTool 'stuckDown)

;; for debugging
(eval-when (:compile-toplevel :load-toplevel)
(unless (equal (dotsize SelectionHaloResizer) '(6 6))
  (error "12: (dotsize SelectionHaloResizer) somehow got munged in build to ~s"
         (dotsize SelectionHaloResizer))))

(define-handler DrawWith (DrawPaletteTool)
  (let (container startH startV newbie connectFrom connectTo)
    (if (is-a (actorToDraw me) connector)
      (progn
        (SK8-multival-setf (connectFrom startH startV) (userPickActor))
        (when (and connectFrom (is-a connectFrom actor) (neq (container connectFrom) stage))
          (SK8-multival-bind (endh endv) (dragrubberband :hanchor startH :vanchor startV :width 2 :height 2)
            (setf connectTo (actorathvcoordinates endh endv))
            (if (and connectTo (is-a connectTo actor) (eq (container connectTo) (container connectFrom)))
              (setf newbie (connect connectFrom connectTo :theconnector (actorToDraw me)))
              )))
        (unless newbie 
          (ed-beep)
          (cond
           ((or (not connectFrom) (not connectTo) (neq (container connectTo) (container connectFrom)))
            (sendToLog "You can only connect two actors in the same window."))
           ((eq (container connectFrom) stage)
            (SendToLog "You can't connect windows."))
           ((or (eq ConnectTo stage) (eq connectFrom stage))
            (SendToLog "You can't connect to the stage."))
           (t
            (SendToLog "Invalid connection.")))
          (getattention messagebox))
        )
      (progn
        (SK8-multival-setf (container startH startV) (userPickActor))
        (when container
          (setf newbie (call-next-method me :startH (round startH) :startV (round startV) :container container 
                                         :project (if (eq container stage) (targetProject ui) (project container)))))))
    (when newbie
      (unless (listp newbie) (setf newbie (list newbie)))
      (unless (stuckdown me) (select newbie))
      (when (eq (project (car newbie)) sk8)
        (ed-beep)
        (sendToLog "Warning: Actors created in the SK8 project can not be saved!")
        )
      (updateEditors newbie :propertyname 'container))
    (setf (itemover pickactormode) nil
          (selecteditem pickactormode) nil)
    (and container (neq container 'NothingClicked))
    ))

(define-handler CallUpPopUp (DrawPaletteTool)
  (let* (obj
         (puitems (GetPopUpItems (actorToDraw me)))
         (curitem nil))
    (setq obj (selectFromPopUpPalette puitems :x (h me :physical t) :y (v me :physical t) :currentItem curitem))
    (when obj
      (setf (StyleToDraw me) (valueToReturn obj))
      )))

(define-handler activateTool (DrawPaletteTool)
  (when (eq (currentTool drawpalette) me)
    (if (and (DrawWith me) (stuckDown me))
      (ActivateTool me)
      (progn
        (DeselectTools drawpalette)
        (if (not (visible selectionhalo)) (show selectionhalo))))))

(define-handler toggle (DrawPaletteTool)
  (if (stuckDown me)
    (progn
      (setf (currentTool drawpalette) nil)
      (moveoffstage (Highlighter drawpalette))
      (setf (fillcolor (Highlighter drawpalette)) yellow))
    (progn
      (setf (stuckDown me) t)
      (surroundObject (Highlighter drawpalette) me)
      (setf (fillcolor (Highlighter drawpalette)) cyan))))

(define-handler mouseDown (DrawPaletteTool)
  (if (cursorlocked stage)
    (ed-beep)
    (let* ((TheAct (actorToDraw me))
           (dp drawpalette)
           (stck (and (currenttool dp) (stuckdown (currenttool dp)))))
      ;;;;We then select the tool
      (unstick DrawPalette)
      (if stck (setf (stuckdown (currenttool dp)) t))
      (when TheAct
        (if (eq me (currentTool dp))
          (toggle me)
          (withActorLocked (dp)
            (setf (currentTool dp) me)
            (setf (stuckDown me) nil)
            (setf (fillcolor (Highlighter dp)) yellow)
            (surroundObject (Highlighter dp) me))))
      (when (currentTool dp)
        (let ((AmOK t))
          (dotimes (i 6)
            (sk8-multival-bind (x y) (mouseloc stage)
              (sk8::wait-time-period second 0.05)
              (when (or (up mouse)
                        (and (neq (actorathvcoordinates x y) librarypalette)
                             (neq (actorathvcoordinates x y) userpalette)))
                (setf AmOK nil)
                (return)
                )))
          (when AmOK
            (CallUpPopUp me)))
        (activateTool me)))))

(define-handler (setf ActorToDraw) (theActor DrawPaletteTool)
  (when (actorToDraw me)
    (logoutswatches me (fillcolor me)))
  (if theActor
    (if (inheritsFrom theActor Actor)
      (progn
        (if (or (eq theactor polygon) (inheritsFrom theActor Polygon)) 
          (if (or (eq theactor linesegment) (inheritsFrom theActor linesegment) )
            (setf (StyleToDraw me) 'simpleLine)
            (setf (StyleToDraw me) 'points))
          (setf (StyleToDraw me) 'cornerToCorner))
        )
      (ed-beep))
    (if (eq (currentTool drawpalette) me) (clearModes drawpalette)))
  (call-next-method))


;;___________________________________________________________________
;;___________________________________________________________________
;; THE SELECTION TOOL

(new sk8::SelectionTool :project ui :objectname "UISelectionTool")
(setf (SelectionFunction UISelectionTool) #'FindSelectedActorsGrow)
(new drawPaletteTool :project ui :objectname "SelectTool")
(setf (slot-value SelectTool 'actorTodraw) 'SELECTION)
(setf (styletodraw selecttool) :rectgrow)
(setf (fillcolor selecttool) lightgray)
(setf (textcolor selecttool) black)
(new rectangle :objectname "selecttooldisplayrect" :project ui)
(setf (text selecttool) "Selection")
(setf (container selecttooldisplayrect) SelectTool)
(setf (fillcolor selecttooldisplayrect) uiColorselectRectGrow)
(setf (mousesensitivity selecttooldisplayrect) 'transparent)
(setf (size selecttooldisplayrect) '(23 23))
(setf (h selecttooldisplayrect) (round (width selecttool) 2))
(setf (v selecttooldisplayrect) 14)
(setf (container SelectTool) nil)

;; for debugging
(eval-when (:compile-toplevel :load-toplevel)
(unless (equal (dotsize SelectionHaloResizer) '(6 6))
  (error "13: (dotsize SelectionHaloResizer) somehow got munged in build to ~s"
         (dotsize SelectionHaloResizer))))

(define-handler actorToDraw (SelectTool &key) 'selection)
(define-handler avoiding (UISelectionTool) (remove-if-not #'(lambda (x) (eq (project x) ui)) (contents stage)))

(define-handler activateTool (SelectTool)
  (when (eq (currentTool drawpalette) me)
    (unwind-protect
      (let (container startH startV ActList)
        (SK8-multival-setf (container startH startV) (userPickActor))
        (if container
          (progn
            (if (neq (itemover pickactormode) (selecteditem pickactormode))
              (progn
                (setf ActList (selecteditem pickactormode))
                (#_FlushEvents #$everyEvent 0))
              (setf ActList (delete-if-not #'(lambda (x) (memq (project x) (okprojects (project (sk8::window container)))))
                                           (SelectWith UISelectionTool :startH (round startH) :startV (round startV) :container stage)
                                           )))
            (select ActList :extend (shiftkeydown)))
          ;(deselect selectionhalo)
          )
        (setf (itemover pickactormode) nil
              (selecteditem pickactormode) nil)
        (if (and (stuckDown me) actlist)
          (progn
            (if (not (visible selectionhalo)) (show selectionhalo))
            (ActivateTool me))
          (withCursor watchcursor
            (DeselectTools drawpalette)
            (if (not (visible selectionhalo)) (show selectionhalo))
            ))))))

(define-handler CallUpPopUp (SelectTool)
  (call-next-method)
  (case (styletodraw SelectTool)
    (:lassoshrink
     (setf (lasso UISelectionTool) t)
     (setf (SelectionFunction UISelectionTool) #'FindSelectedActors)
     )
    (:lassogrow
     (setf (lasso UISelectionTool) t)
     (setf (SelectionFunction UISelectionTool) #'FindSelectedActorsGrow))
    (:rectshrink
     (setf (lasso UISelectionTool) nil)
     (setf (SelectionFunction UISelectionTool) #'FindSelectedActors))
    (:rectgrow
     (setf (lasso UISelectionTool) nil)
     (setf (SelectionFunction UISelectionTool) #'FindSelectedActorsGrow))
    )
  (when (items userpalette)
    (setf (inputobjects userpalette) (inputobjects userpalette))
    ))

;;___________________________________________________________________
;;___________________________________________________________________
;;___________________________________________________________________
;;___________________________________________________________________


(new tablePicker :objectname "DrawPaletteTools" :project ui)
(addparent DrawPaletteTools MixinForObjectPiles)
(addproperty DrawPaletteTools 'drawTools)
(setf (framesize DrawPaletteTools) '(0 0))
(setf (editing DrawPaletteTools) t)

(setf (visible selecttool) nil)
;;;(uicolorize DrawPaletteTools)

;; for debugging
(eval-when (:compile-toplevel :load-toplevel)
(unless (equal (dotsize SelectionHaloResizer) '(6 6))
  (error "14: (dotsize SelectionHaloResizer) somehow got munged in build to ~s"
         (dotsize SelectionHaloResizer))))

(define-handler createTextdisplayItem (DrawPaletteTools theItem)
  (cond 
   ((and (stringp theitem) (string= theitem "Selection")) "Selection         ")
   ((stringp theitem) theitem)
   (t (objectstring theitem :Project (project theitem))))
  )

(define-handler FlashLine (DrawPaletteTools )
  (let ((theguy (GetLinePosition me)))
    (when theguy
      (sk8-multival-bind (ll tt rr bb) (itemboundsrect me theguy :physical t)
        (setf ll (round (left me :physical t))
              tt (round tt)
              rr (round (right me :physical t))
              bb (round bb))
        (decf bb 1)
        (drawxorline ll bb rr bb)
        (sleep 0.01)
        (drawxorline ll bb rr bb)))))

(define-handler draggingMouseEnter (DrawPaletteTools actorDragged)
  (when (editing me)
    (if (eq actordragged ObjectDataRect)
      (let ((xx (object objectdatarect)))
        (unless (listp xx) (setf xx (list xx)))
        (if (and (every #'(lambda (x) (is-a x actor)) xx)
                 (every #'(lambda (x) (objectname x)) xx))
          (setf (highlight me) t)
          (setf (highlight me) nil)))
      (setf (highlight me) nil))))

(define-handler draggingMouseWithin (DrawPaletteTools actorDragged)
  (if (highlight me)
    (flashline me)
    nil))

(define-handler dropped (DrawPaletteTools droppee)
  (withActorLocked (me)
    (when (highlight me)
      (draggingMouseLeave me droppee)
      (let ((theguy (getlineposition me))
            (obj (object ObjectDataRect))
            its
            NewLayer AlreadyHere)
        (dotimes (i (cadr (array-dimensions (items me))))
          (pushnew (aref (items me) 0 i) its))
        (setf its (nreverse its))
        (if (and (stringp (car its)) (string= (car its) "Selection")) (setf its (cdr its)))
        (when theguy
          (bringToFront (sk8::window me))
          (unless (listp obj) (setf obj (list obj)))
          (dolist (i obj) (setf (prototype i) t))
          (setf NewLayer (1- (cadr theguy)))
          (dolist (i obj)
            (setf AlreadyHere (position i its :test #'eq))
            (when AlreadyHere
              (setf its (delete i its :test #'eq))
              (if (< AlreadyHere newlayer)
                (decf newlayer)))
            (setf its (gs:moveToPosition i (cons i its) newlayer)))
          (setf (inputobjects me) its)
          )))))

(define-handler targetProject (DrawPaletteTools) (targetproject ui))
(define-handler (setf Highlight) (theval DrawPaletteTools)
  (setf (inverts me) nil)
  (if theval
    (setf (fillcolor me) white)
    (setf (fillcolor me) shadowwhite)
    )
  (call-next-method))

;;;;This is complex because it reuses stuff...
(define-handler (setf inputobjects) (theval DrawPaletteTools &key (includeSelection t))
  (setf theval (remove-duplicates theval :test #'eq))
  (withactorlocked (me)
    (sk8dev::withlockedcursor animatedclock
      (moveoffstage dthighlighter)
      (let (xx 
            yy 
            newlist
            newbie
            newdrawtools
            (myitems (items me))
            (curdrawtools (drawtools me)))
        (if (eq (car curdrawtools) selecttool) (setf curdrawtools (cdr curdrawtools)))
        (when myitems
          (dotimes (i (cadr (array-dimensions myitems)))
            (pushnew (aref myitems 1 i) xx))
          )
        (when includeSelection
          (pushnew (colorForSelectionTool) xx))
        (setf newlist (nreverse (mapcar #'(lambda (x) (list x (or (and myitems
                                                                       (let (oldswatch)
                                                                         (dotimes (i (cadr (array-dimensions myitems)))
                                                                           (if (eq (aref (items me) 0 i) x) 
                                                                             (setf oldswatch (aref myitems 1 i))))
                                                                         oldswatch
                                                                         ))
                                                                  (generateswatch x me)))) (reverse theval))))
        (when includeSelection
          (pushnew (list "Selection" (colorForSelectionTool)) newlist))
        (setf (items me) newlist)
        (when newlist 
          (setf (columnwidths me) (list (max (car (columnwidths me)) 65) (car (rowheights me)))))
        (dolist (i theval)
          (setf newbie nil)
          (dolist (j curdrawtools)
            (if (eq (actortodraw j) i)
              (setf newbie j)))
          (if newbie
            (setf curdrawtools (delete newbie curdrawtools :test #'eq))
            (progn
              (setf newbie (recycled drawpalettetool :in ui))
              (setf (actortodraw newbie) i)
              (hide newbie)
              (setf (container newbie) me)))
          (push newbie newdrawtools))
        (dolist (i curdrawtools)
          (setf (container i) nil)
          (discard i))
        (setf (drawTools me) (if includeSelection
                               (cons selecttool (nreverse newdrawtools))
                               (nreverse newdrawtools)))
        (setvalue 'inputobjects me theval)
        (setf (items LibraryPalette) (items LibraryPalette))
        (setf (items userpalette) (items userpalette))
        (when (and newlist (items userpalette) (items LibraryPalette))
          (if (> (car (columnwidths LibraryPalette)) (car (columnwidths userpalette)))
            (setf (columnwidths userpalette) (columnwidths LibraryPalette))
            (setf (columnwidths LibraryPalette) (columnwidths userpalette)))
          )
        (when newlist 
          (dotimes (i (cadr (array-dimensions (items me))))
            (pushnew (aref (items me) 1 i) yy)
            )
          (logoutswatches me (delete-if #'(lambda (x) (or (memq x yy) (not (is-a x swatchrenderer)))) xx))))))
  (bestsize drawpalette)
  )
(dotimes (i 10)
  (discard (recycled drawpalettetool :in ui)))

;; (setf (inputobjects drawpalettetools) (knownchildren actor))
;; (setf (highlightselection userpalette) nil)
;; (setf (highlightselection librarypalette ) nil)

(setf (text DrawPaletteTools) " ")
(setf (textfont DrawPaletteTools) EspySansBoldFont)
(setf (textsize DrawPaletteTools) 9)
(setf (highlightselection drawpalettetools) nil)

;; for debugging
(eval-when (:compile-toplevel :load-toplevel)
(unless (equal (dotsize SelectionHaloResizer) '(6 6))
  (error "15: (dotsize SelectionHaloResizer) somehow got munged in build to ~s"
         (dotsize SelectionHaloResizer))))

(define-handler mousedown (DrawPaletteTools)
  (when (items me)
    (sk8-multival-bind (hh vv) (mouseloc stage)
      (let ((index (pointonwhichpart me hh vv :part 'index)))
        (cond 
         ((eql (car index) 1)
          (setf (selection me) (list index))
          (sleep 0.2)
          (if (and (down mouse) (not (stringp (aref (items me) (1- (car index)) (1- (cadr index))))))
            (extendedmousedown me)) )
         ((eql (car index) 2)
          (mousedown (nth (1- (cadr index)) (drawtools me)))
          )
         (t 
          nil)))
      )))

;;___________________________________________________________________
;;___________________________________________________________________

;;___________________________________________________________________
;;___________________________________________________________________
;;___________________________________________________________________

(setf (container DrawPaletteTools) nil)



(new uibiglabel :objectname "UserPaletteLabel" :project ui)
(setf (container UserPaletteLabel) DrawPalette)
(setf (text UserPaletteLabel) "Your Tools")

(new UTLInsetRect :objectname "UserPaletteRect" :project ui)
(setf (container UserPaletteRect) DrawPalette)
(setf (fillcolor UserPaletteRect) ShadowedRendererWithEdges)

(new drawpalettetools :objectname "UserPalette" :project ui)
(setf (container UserPalette) DrawPalette)
(setf (editing UserPalette) t)
(tagpart DrawPalette UserPalette 'UserPalette)
(setf (container selecttool) UserPalette)

;; for debugging
(eval-when (:compile-toplevel :load-toplevel)
(unless (equal (dotsize SelectionHaloResizer) '(6 6))
  (error "16: (dotsize SelectionHaloResizer) somehow got munged in build to ~s"
         (dotsize SelectionHaloResizer))))

(define-handler (setf inputobjects) (theval UserPalette &key (includeSelection t))
  (call-next-method)
  (storetools (targetproject ui) theval))


(new uibiglabel :objectname "LibraryPaletteLabel" :project ui)
(setf (container LibraryPaletteLabel) DrawPalette)
(setf (text LibraryPaletteLabel) "Library")

(new pickermenu :objectname "LibraryPaletteMenu" :project ui)
(setf (container LibraryPaletteMenu) DrawPalette)
(addparent LibraryPaletteMenu uipopup)
(setf (text LibraryPaletteMenu) "Browsers")
(setf (items LibraryPaletteMenu) (list sk8 Scroller picker edittext linesegment browsercomponent dialogbox drawpalette nil)) ;; sk8::macbutton

;; for debugging
(eval-when (:compile-toplevel :load-toplevel)
(unless (equal (dotsize SelectionHaloResizer) '(6 6))
  (error "17: (dotsize SelectionHaloResizer) somehow got munged in build to ~s"
         (dotsize SelectionHaloResizer))))

#|
(define-handler items (LibraryPaletteMenu)
  (append (list sk8 Scroller picker edittext linesegment browsercomponent dialogbox drawpalette) 
          (remove-if-not #'(lambda (item)
                             (delete-if-not #'prototype (delete-if-not #'(lambda (x) (sk8::is-a x actor)) 
                                                                       (rootobjects item))))
                             (libraries (targetproject ui)))
          )
  )
|#
(define-handler createtextdisplayitem (LibraryPaletteMenu theitem)
  (cond
   ((eq theitem sk8) "Shapes")
   ((eq theitem browsercomponent) "Browsers")
   ((eq theitem dialogbox) "Dialogs")
   ((eq theitem drawpalette) "Palettes")
   ((eq theitem picker) "Pickers")
   ((eq theitem edittext) "Text")
   ((eq theitem Scroller) "Widgets")
   ((eq theitem linesegment) "Lines")
   ;; ((eq theitem sk8::macbutton) "Mac Widgets")
   ((eq theitem nil) "None")
   ;;(t (objectstring theitem :project (targetproject ui))) ;;unneeded now that libraries are gone.
   )
  )
(define-handler menuselect (LibraryPaletteMenu)
  (let ((item (selecteditem me)))
    (withactorlocked (me)
      (setf (text me) (createtextdisplayitem me item))
      (resized drawpalette)
      )
    (setf (inputobjects librarypalette :includeselection nil) 
          (cond
           ((eq item sk8) (list rectangle oval roundrect polygon linesegment maskedactor))
           ((eq item browsercomponent) (list browserpaper browsermenubar queryfield namefield GetObjectField
                                             objectlist HierarchicalObjectList ObjectPile
                                             propertylist handlerlist propertysheet handlersheet ProjectDataSheet HandlerViewer
                                             ValueText ValueEditorPicker propertycontrolpanel))
           ((eq item dialogbox) (list dialogbox dialogboxbutton dialogboxhighlightedbutton dialogboxcancelbutton))
           ((eq item drawpalette) (list drawtoolpalette drawtool selectiontool selectiondots halo))
           ((eq item picker) (list Textlist picker styledpicker multilinepicker icontextpicker hierarchicalpicker tablepicker colorpicker))
           ((eq item edittext) (list textfield edittext scriptedittext))
           ((eq item Scroller) (list label RadioButton CheckBox Scroller menu pickermenu sk8::menubar movierectangle treeviewer treeoverviewer))
           ((eq item linesegment) (list linesegment arrow connector directionalconnector))
           ;((eq item sk8::macbutton) (list sk8::MacModalDialog sk8::MacMovableModalDialog sk8::MacAlertBox sk8::MacButton
           ;                                sk8::MacRadioButton sk8::MacCheckBox sk8::MacScrollBar sk8::MacScrollingList
           ;                                sk8::MacPopupMenu sk8::MacProgressIndicator sk8::MacRoundIndicator sk8::MacBarberPole sk8::MacFinderArrow))
           ((eq item nil) nil)
           ;;;(t (delete-if-not #'prototype (delete-if-not #'(lambda (x) (sk8::is-a x actor)) (rootobjects item)))) ;;unneeded now that libraries are gone.
           ))
    ;;(fixtext) ;;unneeded now that libraries are gone.
    ))

(new UTLInsetRect :objectname "LibraryPaletteRect" :project ui)
(setf (container LibraryPaletteRect) DrawPalette)

(new drawpalettetools :objectname "LibraryPalette" :project ui)
(setf (container LibraryPalette) DrawPalette)
(setf (editing LibraryPalette) nil)
(tagpart DrawPalette LibraryPalette 'ToolSection)

;; for debugging
(eval-when (:compile-toplevel :load-toplevel)
(unless (equal (dotsize SelectionHaloResizer) '(6 6))
  (error "18: (dotsize SelectionHaloResizer) somehow got munged in build to ~s"
         (dotsize SelectionHaloResizer))))

(sendtoback LibraryPaletteRect)
(sendtoback UserPaletteRect)

;; for debugging
(eval-when (:compile-toplevel :load-toplevel)
(unless (equal (dotsize SelectionHaloResizer) '(6 6))
  (error "19: (dotsize SelectionHaloResizer) somehow got munged in build to ~s"
         (dotsize SelectionHaloResizer))))

;;___________________________________________________________________
;;___________________________________________________________________
;;___________________________________________________________________

(define-handler DeselectTools (DrawPalette)
  (withActorLocked (me)
    (clearModes me)
    (setf (currentTool me) nil)
    (setf (fillcolor (Highlighter me)) yellow)
    (moveoffstage (Highlighter me))
    ))
(define-handler clearModes (DrawPalette)
  (withActorLocked (DrawPalette)
    (loop
      (if (eq (activeMode) pickactormode)
        (exitmode (activeMode))
        (return)))
    (unstick DrawPalette)
    (setf (currentTool me) nil)
    ))

(define-handler unstick (DrawPalette)
  (dolist (i (append (drawtools (toolsection me)) (drawtools (UserPalette me))))
    (when (inheritsFrom i DrawPaletteTool)
      (setf (stuckDown i) nil))))

;;___________________________________________________________________
;;___________________________________________________________________

;;___________________________________________________________________
;; Set it all up 

(define-handler tools (drawpalette)
  (inputobjects (toolsection me)))

(define-handler (setf tools) (thetools drawpalette)
  (setf (inputobjects (toolsection me) :includeselection nil) thetools))

(define-handler bestsize (DrawPalette)
  (let (ll tt rr bb)
    (withactorlocked (me)
      (setf ll (left drawpalette))
      (setf tt (top drawpalette))
      (setboundsrect UserPalette (+ 4 *WindowLeft*) (+ 28 *WindowTop*) 1000 1000)
      (sk8-multival-bind (lli tti rri bbi) (itemboundsrect UserPalette (list 2 (rows UserPalette)) :physical t)
        (declare (ignore lli tti))
        (setf rr (+ *WindowRight* 3 rri))
        (setf (bottom UserPalette :physical t) bbi))
      (let ((iu (items userpalette)))
        (when iu
          (dotimes (i (cadr (array-dimensions (items userpalette))))
            (setf (boundsrect (nth i (drawtools userpalette)) :physical t)
                  (itemboundsrect userpalette (list 2 (1+ i)) :physical t)))))      
      (resized me)
      (setboundsrect LibraryPalette (+ 4 *WindowLeft*) (+ 6 (bottom LibraryPaletteMenu)) 1000 1000)
      (let ((il (items LibraryPalette)))
        (if il
          (sk8-multival-bind (lli tti rri bbi) (itemboundsrect LibraryPalette (list 2 (rows LibraryPalette)) :physical t)
            (declare (ignore lli tti))
            (setf rr (max rr (+ *WindowRight* 2 rri)))
            (setf bb (+ *windowBottom* 2 bbi))
            (dotimes (i (cadr (array-dimensions (items LibraryPalette))))
              (setf (boundsrect (nth i (drawtools LibraryPalette)) :physical t)
                    (itemboundsrect LibraryPalette (list 2 (1+ i)) :physical t))))
          (setf bb (+ (bottom UserPalette :physical t) 45))))
      (resized DrawPalette)
      )
    (sk8-multival-bind (ll1 tt1 rr1 bb1) (boundsrect drawpalette)
      (declare (ignore ll1 tt1))
      (unless (and (= rr rr1) (= bb bb1))
        (setboundsrect drawpalette ll tt rr bb)
        ;;(fixtext) ;;Unneeded now that libraries are gone...
        ))))
;;;(bestsize DrawPalette)

#| Unneeded now that libraries are gone...
(defun fixtext ()
  (when (> (right LibraryPaletteMenu) (- (width DrawPalette) 9))
    (withactorlocked (DrawPalette)
      (setf (right LibraryPaletteMenu :resizing t) (- (width DrawPalette) 9))
      (setf (text LibraryPaletteMenu) 
            (sk8dev::compute-maybe-truncated-text (text LibraryPaletteMenu) (- (- (width DrawPalette) 9) (- (right LibraryPaletteLabel) 3) 18) "É"))
      (setf (left LibraryPaletteMenu :resizing nil) (- (right LibraryPaletteLabel) 3))
      (setf (right LibraryPaletteMenu :resizing t) (- (width DrawPalette) 9))
      )))
|#
(define-handler resized (DrawPalette)
  (withActorLocked (me)
    (let (hSize vSize)
      (declare (special hSize vSize))
      (sk8-multival-setf (hSize vSize) (size me))
      ;;;(setBoundsRect DrawPaletteAddButton (- hsize (+ 36 *WindowRight*)) (+ 6 *WindowTop*) (- hsize (+ 3 *WindowRight*)) (+ 23 *WindowTop*))
      (setf (top UserPaletteLabel :resizing nil) (+ 8 *WindowTop*))
      (setf (left UserPaletteLabel :resizing nil) (+ 1 *WindowLeft*))
      (setBoundsRect UserPaletteRect (+ 0 *WindowLeft*) (+ 3 (bottom UserPaletteLabel)) (- hsize (+ 1 *WindowRight*)) (+ 2 (bottom UserPalette)))
      (setBoundsRect UserPalette (+ 4 *WindowLeft*) (+ 28 *WindowTop*) (- hsize (+ 3 *WindowRight*)) (bottom UserPalette))
      (setf (top LibraryPaletteLabel :resizing nil) (+ 6 (bottom UserPaletteRect)))
      (setf (left LibraryPaletteLabel :resizing nil) (+ 1 *WindowLeft*))
      (setf (top LibraryPaletteMenu :resizing nil) (+ 3 (bottom UserPaletteRect)))
      (setf (left LibraryPaletteMenu :resizing nil) (- (right LibraryPaletteLabel) 3))
      
      (setBoundsRect LibraryPaletteRect (+ 0 *WindowLeft*) (+ 2 (bottom LibraryPaletteMenu)) (- hsize (+ 1 *WindowRight*)) (- vsize *windowBottom* 0))
      (setBoundsRect LibraryPalette (+ 4 *WindowLeft*) (+ 6 (bottom LibraryPaletteMenu)) (- hsize (+ 3 *WindowRight*)) (- vsize *windowBottom* 2))
      )
    (call-next-method)
    ))

(setf (text DrawPalette) "Draw Tools")

(bringtofront DTHighlighter)
;;(setboundsrect drawpalette 5 20 140 296)
;;(resized DrawPalette)

;; for debugging
(eval-when (:compile-toplevel :load-toplevel)
(unless (equal (dotsize SelectionHaloResizer) '(6 6))
  (error "20: (dotsize SelectionHaloResizer) somehow got munged in build to ~s"
         (dotsize SelectionHaloResizer))))

;;;(setf (container drawpalette) stage)
;;;(setf (keytarget drawpalette) nil)
;;(gc)
;;;(setf (tools drawPalette) (list  namefield ))
;;;(setf (tools drawPalette) (list rectangle oval roundrect polygon linesegment halo selectiondots textlist))

(define-handler enteringstage (drawPalette)
  (setf (highlightcolor userpalette) transparent)
  (setf (highlightcolor LibraryPalette) transparent)
  )

;; for debugging
(eval-when (:compile-toplevel :load-toplevel)
(unless (equal (dotsize SelectionHaloResizer) '(6 6))
  (error "21: (dotsize SelectionHaloResizer) somehow got munged in build to ~s"
         (dotsize SelectionHaloResizer))))

#|
	Change History (most recent last):
	14	6/28/93	Brian Roddy	resized nicer
	22	10/1/93	rod	Removed Node References and Added With-fast-slots.
	24	10/13/93	hernan	snapShotColor -> pixelRenderer
	28	10/19/93	rod	Made it so you could make new palettes.
	46	11/17/93	rod	Added three states to buttons on, locked, off
	53	11/29/93	rod	Fixed paren bug
	54	11/29/93	rod	PrintMessage->Logit
	56	11/30/93	rod	Changed default sizes
	58	12/2/93	hernan	Getting rid of leaveOnStage.
	61	12/17/93	till	#.'s be gone: BestSize, BestSize, BestSize
	62	12/22/93	rod	backRenderer->BackgroundRenderer
				Obsoleted ComplexPatternRenderer
				backgroundColor->BackgroundRenderer
	67	1/26/94	sidney	move definition of generateswatch to before first use
	72	2/12/94	kleiman	name changes
	73	2/14/94	sidney	rename children to knownchildren
	75	2/18/94	sidney	it looks like ss:!waitimeperiod's name is now ss::wait-time-period
	76	2/21/94	hernan	The Final Renaming for Alpha! (yeah, right...)
	77	2/21/94	hernan	window -> sk8::window.
	78	2/25/94	sidney	declare selecttool, which is created too trickily for the scanner to catch
	79	2/25/94	hernan	Using symbols instead of keywords for user
				visible options.
	84	3/1/94	rod	Fixing size problem
	85	3/2/94	Hernan	Porting to Fred 3.0
	87	3/5/94	rod	addproperty avoided wherever possible
	92	3/9/94	rod	Doing Project Switching and Reference Clearing.
	106	3/29/94	rod	Fixing doubleclick
	108	4/4/94	rod	Fixed selecting when tools are stuck down
	109	4/11/94	rod	Removing reference to simple window.  No longer
				needed in the redesign of the UI.
	111	4/12/94	Hernan	Avoiding use of contents when not necessary.
	116	4/29/94	dy	MovieRectangle
	122	5/4/94	sidney	Fixing stupid typo.
	127	5/13/94	till	Made click (of DrawPaletteAddButton) cons 1/20
				of what it used to.
	128	5/22/94	rod	Here begins the experimentation with a Mac look 
				to the UI.
	129	6/3/94	rod	
	130	6/3/94	rod	
	131	6/3/94	rod	
	132	6/3/94	rod	
	133	6/3/94	rod	
	134	6/4/94	sidney	add check for nil that became necessary with the previous change
	135	6/12/94	rod	Changing "Palette" to "Palettes
	136	6/14/94	rod	Fixing bug which is preventing the style popup
				from appearing on extended mousedown.
	137	6/17/94	rod	Fixing stupidity bug in clear references.
	138	6/17/94	rod	Ensuring Palette Remembers the User Palette
				Objects.
	139	6/27/94	Hernan	1170049: style options become symbols.
	141	7/13/94	rod	
	142	7/15/94	rod	
	143	7/17/94	rod	Clearing references from pickactormode after
				using it.
	144	7/17/94	rod	Speeding up change of project.
	145	7/20/94	rod	1175330
	146	7/22/94	rod	Added Connection tool stuff.
	147	7/26/94	rod	Fixing title of "SK8" objects.
	148	 8/23/94	rod     	
	149	 8/30/94	rod     	Making resized call-next-method
	150	 9/ 1/94	rod     	Making the updates of the swatches be lazy.
	151	 9/ 1/94	Hernan  	
	152	 9/ 8/94	rod     	
	153	 9/ 8/94	rod     	
	154	 9/ 8/94	rod     	
	155	 9/ 8/94	rod     	
	156	 9/12/94	rod     	
	157	 9/26/94	rod     	Fixing selection styles.
	158	 9/26/94	rod     	Fixing Icon.
	159	10/ 4/94	rod     	Moving lazy stuff to all swatches
	160	10/19/94	rod     	Removing libraries.
	161	10/20/94	rod     	Fixing color underline in shadow renderer.
	162	10/31/94	rod     	removing search field.
	163	11/21/94	rod     	
	164	11/21/94	rod     	fixing selection to do proper avoiding.
	165	11/21/94	rod     	insuring selection works on the stage as a 
							container.
	166	12/ 3/94	rod     	Adding treeviewer and treeoverviewer.
	167	12/16/94	rod     	Adding MacWidgets.
	168	 1/16/95	rod     	disabling dragging the text "selection" from
							the palette.
	169	 2/ 1/95	rod     	Speeding up set inputobjects.
	170	 2/ 1/95	rod     	premaking some tools for speed.
	171	 2/ 1/95	rod     	Adding None option to library for space.
	172	 2/ 3/95	Hernan  	WithLockedCursor is now correct. I check this file
							out and back in to force it to be recompiled.
	173	 2/17/95	rod     	
	174	 4/13/95	rod     	
	175	 4/26/95	rod     	Fixing select tool to select even when you
							click on another project.
	2  	 1/16/96	Hernan  	Temporarily commenting out all references to the macWidgets
						until the new compiler is in place and in working order.
	3  	 2/14/96	Brian   	removing reference to macframes.
	2  	 4/19/96	Brian   	changing package of wait-time-period
	3  	 7/ 7/96	sidney  	changes for native PPC build
	4  	 2/27/97	Hernan  	
|# ;(do not edit past this line!!)
