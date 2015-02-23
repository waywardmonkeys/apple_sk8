;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories


;;;

(in-package :UIDevelopment)

(SK8-declare-syms :UI :public ; Updated  4-25-95   4:20 pm
                  UI::DRAGGERBOUNDS UI::LEFTOFFSET UI::MAYBEREMOVE UI::OLDOBJECTNAME UI::RESIZERBOUNDS
                  UI::SHOWSDRAGGER UI::SHOWSRESIZER UI::SHOWSTITLE UI::TITLEBOUNDS)


(new object :objectname "UIPreferences" :project ui
     :properties '(haloSize HaloOutSet LiveMotion ShowSelectionHaloTitleBar))
(setf (haloSize UIPreferences) '(10 10))
(setf (HaloOutSet UIPreferences) 2)
(setf (ShowSelectionHaloTitleBar UIPreferences) t)
(when t ;;;; should check processor for this!!!
  (setf (LiveMotion UIPreferences) t))


;; THE SELECTION HANDLING CODE

(new Rectangle :objectName "SelectionHalo"  :project ui
     :properties '(cachedBounds
                   SelectedItems
                   HOutset
                   VOutset
                   oldobjectname
                   leftoffset
                   draggerbounds
                   titlebounds
                   resizerbounds
                   showsdragger
                   showstitle
                   showsresizer
                   busy))
(setf (busy selectionhalo) nil)
(setf (showsdragger SelectionHalo) t)
(setf (showstitle SelectionHalo) t)
(setf (showsresizer SelectionHalo) t)

(define-handler clearReferences (SelectionHalo &key ((:objects theobjects)))
  (if theobjects
    (selectionok me)
    (select nil)))

(define-handler SetUpForProject (SelectionHalo &key ((:project theproject)))
  (select (delete-if-not #'(lambda (x) (memq (project x) (okprojects theproject))) 
                         (selecteditems me))))

(define-handler SetHaloSize (SelectionHalo Val val2 &key (WithOutSet 0))
  (withActorLocked (me)
    (setf (HOutset me)  (+ withoutset Val))
    (setf (VOutset me) (+ withoutset Val2))
    (recomputesize me)
    ))

(setFrameSize SelectionHalo 0 0)
(setf (mouseSensitivity SelectionHalo) 'transparent)
(setf (wantsIdle SelectionHalo) t)
(setf (floating SelectionHalo) t)
(setf (dofirstclick selectionhalo) t)
(setf (colordepth selectionhalo) nil)

(define-handler updateEditor (selectionhalo &key (propertyname nil) (items nil))
  (declare (ignore propertyname items))
  nil)

(define-handler idle (selectionhalo)
  (when (and (not (busy me)) (selecteditems me) (selectionok me))
    (when (and (neq (oldobjectname me) :multiple)
               (not (string= (oldobjectname me) (objectname (car (selecteditems me))))))
      (bestSize SelectionHaloTitleBar)
      (setf (oldobjectname me) (objectname (car (selecteditems me)))))
    (unless (equal (cachedBounds me) (actorsbounds (selecteditems me) :physical t))
      (recomputesize me))))

(define-handler activate (selectionhalo)
  (setf (keyTarget me) me)
  (setf (currentCommandKey system) me)
  (bestsize SelectionHaloTitleBar)
  (call-next-method)
  t)

(define-handler deactivate (selectionhalo)
  (call-next-method)
  (when (selecteditems me) ;; this prevents an infinite loop
    (maybeRemove Me)
    )
  )

(define-handler maybeRemove (selectionhalo)
  (sk8-multival-bind (hh vv) (location mouse)
    (unless (eq (container (actorathvcoordinates hh vv)) projectbuildermenubar)
      (deselect selectionhalo))
    ))



;; the doodads
;;______________________________________________________________________
;;______________________________________________________________________
;; DRAGGER
;; Allows the selection to be dragged around the screen.

;; dragger
(new rectangle :objectName "SelectionHaloActorDragger" :project ui)
(setf (container SelectionHaloActorDragger) SelectionHalo)
(setf (fillColor SelectionHaloActorDragger) UILightColor)
(setf (wantsMouseWithin SelectionHaloActorDragger) t)
(setFrameSize SelectionHaloActorDragger 1 1)
(setf (FrameColor SelectionHaloActorDragger) graypattern)

(defun ok-to-drag-p ()
  (let (containers)
    (dolist (i (selecteditems selectionhalo))
      (setf containers (append (sk8::containers i) containers)))
    (not (intersection (selecteditems selectionhalo) containers))
    )
  )

(define-handler mouseDown (SelectionHaloActorDragger)
  (let ((sels (selecteditems selectionhalo)))
    (setf (busy (container me)) t)
    (let ((cont (container me))
          temp)    
      (when (selectionOK cont)
        (if (commandKeyDown)
          (selectFromMenu SelectionHaloPopUp :h (sk8::eventH) :v (sk8::eventV) :defaultMenuItem 0)
          (progn
            (if (controlKeyDown)
              (withCursor
                CursorCrossHairDeep
                (dragSelectionDeep cont))
              (if (eq (fillcolor me) uiLightColor)
                (progn
                  (setf temp (mapcar #'boundsrect sels))
                  (drag cont :live (or (and (not (livemotion uipreferences))
                                            (optionkeydown))
                                       (and (not (optionkeydown))
                                            (livemotion uipreferences))))
                  (unless (every #'(lambda (x y) (equal x y)) temp (mapcar #'boundsrect sels))
                    (setf (objectlist UndoableSetLog) sels)
                    (setf (PropertyName UndoableSetLog) 'boundsrect)
                    (setf (valuelist UndoableSetLog) temp))
                  )
                (progn
                  (if (ok-to-drag-p)
                    (if (= 1 (length sels))
                      (messageToUser "The selected item is not 'draggable.'" :beep t)
                      (messageToUser "Some of the selected items are not 'draggable.'" :beep t))
                    (messageToUser "The selected items cannot be dragged, because some of the selected items are contained by other selected items." :beep t))
                  (select sels))))))))
    (setf (busy (container me)) nil)))

;;MCL3.0
(define-handler drag (SelectionHalo &key (live nil))
  (let ((items (SelectedItems me)))
    (moveoffstage me)
    ;; Send the update event to every window since the halo might overlap any window.
    (dolist (c (contents Stage))
      (sendUpdateEvent c))
    (drag (Car items) :otherActors (cdr items) :live live)
    (recomputeSize me)
    (updateEditors (SelectedItems selectionHalo) :propertyname 'boundsrect)
    ))

(define-handler dragselectionDeep (SelectionHalo)
  (let* ((items (SelectedItems me))
         (oldx (sk8::eventH))
         (oldy (sk8::eventV))
         ContainerUndoInfo newcont
         offX offY dropObj top bottom left right xOffset yOffset)
    (sk8-multival-bind (hLoc vLoc) (location (car items) :physical t)
      (setf xOffset (- (round (eventH)) hLoc)
            yOffset (- (round (eventV)) vLoc)))
    (moveoffstage me)
    ;; Send the update event to every window since the halo might overlap any window.
    (dolist (c (contents Stage))
      (sendUpdateEvent c))
    (setf (cursor stage) cursorcrosshairdeep)
    (setf (objectlist UndoableSetLog) items)
    (setf (PropertyName UndoableSetLog) 'boundsrect)
    (setf (valuelist UndoableSetLog) (mapcar #'boundsrect items))
    (setf ContainerUndoInfo (mapcar #'container items))
    (sk8-multival-bind (x y) (dragActors items xOffset yOffset nil)
      (setf dropObj (actorathvcoordinates x y))
      (setf offX (- x oldX) 
            offy (- y oldY)))
    (withCursor watchCursor
      (if (or (and (eq dropobj stage)
                   (eq (container (car items)) stage))
              (and (neq dropobj stage)
                   (or (memq dropobj items)
                       (some #'(lambda (x)   ;;;;selected items contains
                                 (containedby dropobj x)) items))))
        (dolist (i items) 
          (hide i)
          (sk8-multiVal-setf (left top right bottom) (boundsRect i :physical t))
          (setBoundsRect i (+ offX left) (+ offY top)
                         (+ offX right) (+ offY bottom) :physical t :justMoving t)
          (show i))
        (if  (and dropObj (eq (project dropobj) (project (car items))))
          (dolist (i items) 
            (hide i)
            (setf newcont dropobj)
            (sk8-multiVal-setf (left top right bottom) (boundsRect i :physical t))
            (setf (container i) dropObj)
            (setBoundsRect i (+ offX left) (+ offY top)
                           (+ offX right) (+ offY bottom) :physical t :JustMoving t)
            (show i))
          (dolist (i items)
            (ed-beep)
            (setf newcont stage)
            (hide i)
            (sk8-multiVal-setf (left top right bottom) (boundsRect i :physical t))
            (setf (container i) stage)
            (setBoundsRect i (+ offX left) (+ offY top)
                           (+ offX right) (+ offY bottom) :physical t :justMoving t)
            (show i)))))
    (select items)
    (when newcont
      (setf (PropertyName UndoableSetLog) (list 'boundsrect 'container))
      (setf (valuelist UndoableSetLog) (list (valuelist UndoableSetLog) ContainerUndoInfo)))
    
    (updateEditors (SelectedItems selectionHalo) :propertyname 'boundsrect)
    (updateEditors (SelectedItems selectionHalo) :propertyname 'container)
    ))

(define-handler mouseWithin (SelectionHaloActorDragger)
  (if (commandKeyDown)
    (when (neq (activeMode) pickactormode)
      (setf (cursor stage) CursorMenu))
    (if (controlKeyDown)
      (setf (cursor stage) CursorCrossHairDeep)
      (when (neq (activeMode) pickactormode)
        (setf (cursor stage) standardcursor)))))

(define-handler mouseLeave (SelectionHaloActorDragger)
  (setf (cursor stage) standardCursor))


;;______________________________________________________________________
;;______________________________________________________________________
;; GENERIC RESIZER
;; Resize the rectangle any corner

(new selectionDots :objectName "SelectionHaloResizer" :project ui)
(setf (fillColor SelectionHaloResizer) black)
(setFrameSize SelectionHaloResizer 0 0)
(setf (wantsMouseWithin SelectionHaloResizer) t)
(setf (container SelectionHaloResizer) SelectionHalo)
(setf (dotsize SelectionHaloResizer) '(6 6))


;;; TEMPORARY: until I find out why this actor does not think it has moved when
;;;            it gets to recompute its fill region... (This could just be a recompile everything problem).
;;;            (Hern‡n).

(gs:offsetsRegions! (gs:node-flags ui::selectionhaloresizer) 0)

(define-handler mouseDown (SelectionHaloResizer)
  (setf (busy SelectionHalo) t)
  (let ((Items (selectedItems SelectionHalo)) 
        how)
    (when (selectionOk SelectionHalo)
      (if (commandKeyDown)
        (selectFromMenu SelectionHaloPopUp :h (sk8::eventH) :v (sk8::eventV) :defaultMenuItem 0)
        (let ((OkToResize t)
              temp)
          (dolist (i Items)
            (if (not (resizable i)) (setf OkToResize nil)))
          (if OkToResize
            (progn
              (setq how (pointOnWhichPart me (eventH) (eventV)))
              (when how
                (unwind-protect
                  (sk8::withCursor standardCursor
                    (moveoffstage SelectionHalo)
                    (SendUpdateEvent (car Items))
                    
                    (setf temp (mapcar #'boundsrect (selecteditems selectionhalo)))
                    (resize (car Items) how 
                            :otherActors (cdr Items)
                            :live (or (and (not (livemotion uipreferences))
                                           (optionkeydown))
                                      (and (not (optionkeydown))
                                           (livemotion uipreferences))))
                    (unless (every #'(lambda (x y) (equal x y)) temp (mapcar #'boundsrect (selecteditems selectionhalo)))
                      (setf (objectlist UndoableSetLog) (selecteditems selectionhalo))
                      (setf (PropertyName UndoableSetLog) 'boundsrect)
                      (setf (valuelist UndoableSetLog) temp))
                    
                    (recomputeSize SelectionHalo)))))
            (progn
              (ed-beep)
              (if (= 1 (length Items))
                (messageToUser "The selected item is not 'resizable.'")
                (messageToUser "Some of the selected items are not 'resizable.'"))
              (select Items)))
          ))))
  (setf (busy SelectionHalo) nil)
  (updateEditors (SelectedItems selectionHalo) :propertyname 'boundsrect)
  )


(define-handler mouseWithin (SelectionHaloResizer)
  (when (neq (activeMode) pickactormode)
    (if (commandKeyDown)
      (setf (cursor stage) CursorMenu)
      (setf (cursor stage) standardCursor))))

(define-handler mouseLeave (SelectionHaloResizer)
  (setf (cursor stage) standardCursor))

;;______________________________________________________________________
;;______________________________________________________________________
;; Object name title
;; Brings up popup

(new rectangle :objectName "SelectionHaloTitleBar" :project ui)
(setf (fillColor SelectionHaloTitleBar) shadowwhite)
(setf (textColor SelectionHaloTitleBar) black)
(setf (textStyle SelectionHaloTitleBar) '(plain))
(setf (textfont SelectionHaloTitleBar) EspySansFont)
(setf (textLocation SelectionHaloTitleBar) 'centerleft)
(setf (text SelectionHaloTitleBar) "Title")
(setf (textSize SelectionHaloTitleBar) 9)
(setFrameSize SelectionHaloTitleBar 1 1)
(setf (container SelectionHaloTitleBar) SelectionHalo)

(setf (texthoffset SelectionHaloTitleBar) 3)
(setf (textVoffset SelectionHaloTitleBar) -1)

(new rectangle :objectName "SelectionHaloPopupRect" :project ui)
(setf (fillColor SelectionHaloPopupRect) UIMenuOut)
(setf (framecolor SelectionHaloPopupRect) uirectangleoutbevel)
(setf (container SelectionHaloPopupRect) SelectionHalo)
(setf (framesize SelectionHaloPopupRect) '(0 0))

(define-handler mouseDown (SelectionHaloPopupRect)
  (when (selectionOK (container me))
    (selectFromMenu SelectionHaloPopUp :relativeactor me :position 'bottomRightAligned
                    :defaultMenuItem 0)))

(define-handler mouseDown (SelectionHaloTitleBar)
  (let ((sels (selecteditems selectionhalo)))
    (setf (boundsrect ObjectDataRect :physical t) (boundsrect me :physical t))
    (setf (object ObjectDataRect) (if (eql (length sels) 1) (car sels) sels))
    (setf (componentfrom ObjectDataRect) selectionhalo)
    (withcursor standardcursor
      (drag ObjectDataRect :live nil :onstage t :draggingMouseWithinEvents t :draggedOverEvents t :dropevents t))
    ))


;;______________________________________________________________________
;; selectionok
;; called by our functions to make sure our selection is no longer pointing to a deleted object.

(define-handler selectionOK (SelectionHalo)
  (let* ((cursel (SelectedItems me))                   ;;; note that this could be false to start with
         validList)  ;;; all items which from initial list which are not disposed.
    (if (every #'container cursel)
      t
      (progn
        ;;; (setf validList (remove-if #'disposed cursel))
        (setf validList (delete-if-not #'container validList))
        (if validList
          (select validlist)
          (progn
            (deselect me)
            nil)))
      )))

;;______________________________________________________________________
;; DESELECT
;; Removes all of the objects from a selection object and disposes of the selection.

(define-handler deSelect (SelectionHalo)
  (dolist (i (menuItems SelectionHaloPopUplocalHandlers)) 
    (setf (menu i) nil))
  (Setf (SelectedItems me) nil)
  (Setf (oldobjectname me) :multiple)
  (Setf (container me) nil))


;;______________________________________________________________________
;; SELECT
;; Given an object list, returns a selection object

(defun select (objList &key (extend nil) (printObjs nil))
  (withActorLocked (selectionHalo)
    (if (not (listp objlist)) (setf objlist (list objlist)))
    (setf objlist (delete-if #'(lambda (x) (eq (sk8::window x) selectionhalo)) objlist))
    (if (or extend objlist)
      (progn
        (let ((otherObjs (selectedItems SelectionHalo)))
          (if (and (not extend) (equal objList OtherObjs))    ;;; reselected the same thing so only need to make sure we are sized nice
            (recomputeSize SelectionHalo)
            (progn                                            ;;; otherwise we need to redo the selection
              (when (and extend otherObjs)
                (setq objList (set-exclusive-or otherObjs objList)))
              (when (some #'(lambda (x) (locked (sk8::window x))) objlist)
                (sendtolog "Warning: The selected actors are locked.  You must unlock the window of these actors for the graphics to be updated." :attention t)
                (ed-beep))
              (setf (SelectedItems SelectionHalo) objList)
              (setf (fillColor SelectionHaloActorDragger) 
                    (if (and (every #'draggable objList) (ok-to-drag-p)) uilightcolor graypattern))
              (setf (fillColor selectionhaloresizer) (if (every #'draggable objList) black graypattern))
              (if (eql (length objlist) 1)
                (Setf (oldobjectname SelectionHalo) (objectname (car objlist)))
                (Setf (oldobjectname SelectionHalo) :multiple))
              (recomputeSize SelectionHalo)
              (bringup selectionhalo)
              (when printObjs
                (if (= (length objlist) 1)
                  (sendToLog (ObjectString (car objlist)))
                  (sendToLog (ObjectString objlist))))
              objlist))))
      (deselect selectionhalo)))
  )

;;______________________________________________________________________
;; RESIZED
;; scales the selection

(define-handler resized (SelectionHalo)
  )


(defun calcTBSize (me Bottom Middle) 
  (let ((vals (actortextsize SelectionHaloTitleBar))
        left)
    (sk8-multival-bind (th tv) vals
      (incf th 23)
      (setf left (- middle (ceiling th 2)))
      (sk8-multivals (ceiling left)
                     (ceiling (+ bottom 3))
                     (ceiling (+ left th))
                     (ceiling (+ bottom 3 18)))
      )))

(defun calctbtext ()
  (if (= (length (selectedItems selectionhalo)) 1)
    (objectString (car (selectedItems selectionhalo)) :project (project (car (selectedItems selectionhalo))))
    (format nil "~s Items" (length (selectedItems selectionhalo)))))





;;______________________________________________________________________
;; RECOMPUTESIZE
;; Makes the selection the corrent size for its items.




(define-handler recomputeSize (SelectionHalo)
  (sk8-multival-bind (ll tt rr bb) (actorsbounds (selectedItems me) :physical t)
    (let (DragLL DragTT DragRR DragBB
                 ResizeLL ResizeTT ResizeRR ResizeBB
                 TitleLL TitleTT TitleRR TitleBB
                 HaloLL HaloTT HaloRR HaloBB
                 (hout (houtset me))
                 (vout (voutset me))
                 (halfdot (truncate (car (dotsize SelectionHaloResizer)) 2)))
      (setf (cachedBounds me) (list ll tt rr bb))  ;;; cache the actor bounds here so on idle we can check them to see if they change.
      
      (setf ResizeLL (- ll halfdot) ResizeTT (- tt halfdot) ResizeRR (+ rr halfdot) ResizeBB (+ bb halfdot)) 
      
      (decf ll hout) 
      (decf tt vout)
      (incf rr hout)
      (incf bb vout)
      (setf DragLL ll DragTT tt DragRR rr DragBB bb) 
      
      (setf (text SelectionHaloTitleBar) (calctbtext))
      (sk8-multival-setf (TitleLL TitleTT TitleRR TitleBB) (calcTBSize SelectionHaloTitleBar DragBB (+ DragLL (/ (- DragRR DragLL) 2))))
      
      (setf HaloLL (min TitleLL DragLL) HaloTT DragTT HaloRR (max TitleRR DragRR) HaloBB TitleBB) 
      
      (setf (leftOffset me) (if (< titleLL DragLL) (- (- dragll titlell)) 0))
      (setf (draggerbounds me) (list (- DragLL HaloLL) (- DragTT HaloTT) (- DragRR HaloLL) (- DragBB HaloTT)))
      (setf (titlebounds me) (list (- TitleLL HaloLL) (- TitleTT HaloTT) (- TitleRR HaloLL) (- TitleBB HaloTT)))
      (setf (resizerbounds me) (list (- ResizeLL HaloLL) (- ResizeTT HaloTT) (- ResizeRR HaloLL) (- ResizeBB HaloTT)))
      
      ;; This should not be necessary...
      (let ((flags (gs:node-flags me)))
        (gs:boundsDirty! flags)
        (gs:fillDirty! flags)
        (gs:frameDirty! flags))
         
      (if (showsresizer me) 
        (bringtofront selectionhaloresizer)
        (sendtoback selectionhaloresizer))
      (setboundsrect SelectionHaloTitleBar (- TitleLL HaloLL) (- TitleTT HaloTT) (- TitleRR HaloLL 20) (- TitleBB HaloTT))
      (setboundsrect SelectionHaloPopupRect (- TitleRR HaloLL 20) (- TitleTT HaloTT) (- TitleRR HaloLL) (- TitleBB HaloTT))
      (setf (boundsrect SelectionHaloResizer) (resizerbounds me))
      (setf (boundsrect SelectionHaloActorDragger) (draggerbounds me))
      (setboundsrect me HaloLL HaloTT HaloRR HaloBB :physical t)
      )))

(sendtoback SelectionHaloActorDragger)

(define-handler makeBoundsRegion (SelectionHalo)
  (gs:let+ ((physRect (gs:recompute-physicalBoundsRect me))
            (pll (gs:rect-left physRect))
            (ptt (gs:rect-top physRect))
            (temp (:region))
            (r (:rect))
            (hout (houtset me))
            (vout (voutset me))
            (loffset (leftoffset me))
            (sels (selecteditems me))
            (resizerbounds (resizerbounds me))
            (dotsize (dotsize selectionhaloresizer))
            (bounds (gs:node-BoundsRegion me))
            ll2 tt2 rr2 bb2)
    (sk8-multival-bind (ll tt rr bb) (cachedbounds me)
      (declare (ignore rr bb))
      (#_setEmptyRgn bounds)
      (when (showsdragger me)
        (sk8-multival-bind (ll1 tt1 rr1 bb1) (draggerbounds me)
          (declare (ignore tt1))
          (set-qd-rect r (truncate (+ pll ll1)) (truncate ptt) (truncate (+ pll rr1)) (truncate (+ ptt bb1)))
          (#_rectRgn temp r)
          (#_UnionRgn bounds temp bounds))
        
        (when sels
          (dolist (i sels)
            (sk8-multival-setf (ll2 tt2 rr2 bb2) (boundsrect (sk8::window i) :physical t))
            (gs:recompute-bounds-region i (gs:node-flags i))
            (#_copyrgn (gs:node-boundsRegion i) temp)
            (#_offsetrgn temp (+ hout (- ll2 ll loffset)) (+ vout (- tt2 tt)))
            (#_diffRgn bounds temp bounds))
          ))
      (when (showsresizer me)
        (#_SetEmptyRgn temp)
        (sk8dev::make-dots-region temp 
                                  (truncate (+ pll (first resizerbounds))) (truncate (+ ptt (second resizerbounds)))
                                  (truncate (+ pll (third resizerbounds))) (truncate (+ ptt (fourth resizerbounds)))
                                  (first dotsize) (second dotsize))
        (#_UnionRgn bounds temp bounds))
      
      (when (showstitle me)
        (sk8-multival-bind (ll1 tt1 rr1 bb1) (titlebounds me)
          (set-qd-rect r (truncate (+ pll ll1)) (truncate (+ ptt tt1)) (truncate (+ pll rr1)) (truncate (+ ptt bb1)))
          (#_rectRgn temp r)
          (#_UnionRgn bounds temp bounds)))
      (gs:boundsDirty! (gs:node-flags me) 0))))

(setHaloSize selectionhalo 10 10 :withoutset 2)


#|
(setf (showsdragger SelectionHalo) nil)
(setf (showstitle SelectionHalo) nil)
(setf (showsresizer SelectionHalo) nil)

(setf (showsdragger SelectionHalo) t)
(setf (showstitle SelectionHalo) t)
(setf (showsresizer SelectionHalo) t)
  (resized me)
  (gs:recompute-physicalBoundsRect SelectionHalo)
  (gs:recompute-bounds-region SelectionHalo (gs:node-flags SelectionHalo))
|#


#|
(setf (showsdragger SelectionHalo) nil)
(setf (showstitle SelectionHalo) nil)
(setf (showsresizer SelectionHalo) nil)

(setf (showsdragger SelectionHalo) t)
(setf (showstitle SelectionHalo) t)
(setf (showsresizer SelectionHalo) t)
  (resized me)
  (gs:recompute-physicalBoundsRect SelectionHalo)
  (gs:recompute-bounds-region SelectionHalo (gs:node-flags SelectionHalo))
|#

;;____________________________________________________________________________________________
;;____________________________________________________________________________________________
;;____________________________________________________________________________________________
;;____________________________________________________________________________________________
;; The Selection Halo Pop Up Menu
;;____________________________________________________________________________________________
;;____________________________________________________________________________________________
;;____________________________________________________________________________________________
;;____________________________________________________________________________________________

(new uiMenuActor :objectName "SelectionHaloPopUp" :project ui)

(setf (menufillcolor selectionhalopopup) graytone90)
(setf (menutextcolor selectionhalopopup) black)

(define-handler update (SelectionHaloPopUp)
  (makeMenu SelectionHaloPopUplocalHandlers)
  (call-next-method))


;;____________________________________________________________________
;; sk8 Name

(new menuItem :objectName "SelectionHaloPopUpName" :project ui)
(setf (text SelectionHaloPopUpName) "Name...")
(setf (menu SelectionHaloPopUpName) SelectionHaloPopUp)

(define-handler update (SelectionHaloPopUpName)
  (if (some #'objectname (selecteditems selectionhalo))
    (setf (text me) "Change Name...")
    (setf (text me) "Name...")))

(define-handler menuSelect (SelectionHaloPopUpName)
  (let* ((items (selectedItems selectionHalo))
         (item (car items)))
    (when item
      (process-run-function "Selection Halo Name"
                            #'(lambda()
                                (objectNameDialog items)
                                (deselect selectionhalo)
                                (bestsize SelectionHaloTitleBar)
                                (select items))))))

;;
;;____________________________________________________________________
;; EDIT PROPERTIES

(new menuItem :objectName "SelectionHaloPopUpeditProperties" :project ui)
(setf (text SelectionHaloPopUpeditProperties) "Edit")
(setf (menu SelectionHaloPopUpeditProperties) SelectionHaloPopUp)

(define-handler menuSelect (SelectionHaloPopUpeditProperties)
  (process-run-function "Selection Halo EditProp" #'(lambda()
                                (uiedit (selectedItems selectionHalo)))))


;;____________________________________________________________________
;; DESELECT

(new menuItem :objectName "SelectionHaloPopUpDeselect" :project ui)
(setf (text SelectionHaloPopUpDeselect) "Deselect")
(setf (menu SelectionHaloPopUpDeselect) SelectionHaloPopUp)

(define-handler menuSelect (SelectionHaloPopUpDeselect)
  (deselect selectionHalo))

;;;____________________________________________________________________
;; DISPOSE

(new menuItem :objectName "SelectionHaloPopUpDispose" :project ui)
(setf (text SelectionHaloPopUpDispose) "Clear References")
(setf (menu SelectionHaloPopUpDispose) SelectionHaloPopUp)

(define-handler menuSelect (SelectionHaloPopUpDispose)
  (process-run-function "Selection Halo Dispose" #'(lambda()
                                (let ((sels (selectedItems selectionHalo)))
                                  (deselect selectionHalo)
                                  (disposedialog sels)
                                  ))))

;;____________________________________________________________________
(new menuItem :objectName "SelectionHaloPopUpspace1" :project ui)
(setf (text SelectionHaloPopUpspace1) "-")
(setf (enabled SelectionHaloPopUpspace1) nil)
(setf (menu SelectionHaloPopUpspace1) SelectionHaloPopUp)

;;____________________________________________________________________
;; Tag

(new menuItem :objectName "SelectionHaloTagPart" :project ui)
(setf (text SelectionHaloTagPart) "Tag...")
(setf (menu SelectionHaloTagPart) SelectionHaloPopUp)

(define-handler update (SelectionHaloTagPart)
  (setf (enabled me) (and (= 1 (length (selecteditems selectionhalo)))
                          (neq (container (car (selecteditems selectionhalo))) stage))))

(define-handler menuSelect (SelectionHaloTagPart)
  (let* ((items (selectedItems selectionHalo))
         (item (car items)))
    (when item
      (process-run-function "Selection Halo TagPart" #'(lambda()
                                    (tagDialog item)
                                    (bestsize SelectionHaloTitleBar)
                                    (select items))))))

#|
;;____________________________________________________________________
;; Make keyTarget

(new menuItem :objectName "SelectionHaloPopUpMakekeyTarget" :project ui)
(setf (text SelectionHaloPopUpMakekeyTarget) "Set to keyTarget")
(setf (menu SelectionHaloPopUpMakekeyTarget) SelectionHaloPopUp)

(define-handler menuSelect (SelectionHaloPopUpMakekeyTarget)
  (let ((items (selectedItems selectionHalo)))
    (dolist (i items)
       (setf (keytarget (sk8::window i)) i)
       (bringtofront (sk8::window i))
       )))

|#

;;____________________________________________________________________
;;____________________________________________________________________
;; ARRANGE MENU
;; Has various ordering commands

(new uiMenuActor :objectName "SelectionHaloPopUpArrange" :project ui)
(setf (text SelectionHaloPopUpArrange) "Layering")
(setf (menu SelectionHaloPopUpArrange) SelectionHaloPopUp)

;;____________________________________________________________________
;; SEND TO BACK

(new menuItem :objectName "SelectionHaloPopUpArrangeSTB" :project ui)
(setf (text SelectionHaloPopUpArrangeSTB) "Send To Back")
(setf (menu SelectionHaloPopUpArrangeSTB) SelectionHaloPopUpArrange)

(define-handler menuSelect (SelectionHaloPopUpArrangeSTB)
  (let ((items (selectedItems selectionHalo)))
    (when items
      (process-run-function "Selection Halo STB"
                            #'(lambda()
                                (mapc #'SendToBack items)
                                (if (selecteditems selectionhalo) 
                                  (when (and (= (length items) 1) (not (objectname (car items))))
                                    (setf (text SelectionHaloTitleBar) (calctbtext)))
                                  (select items))
                                )))
    ))


;;____________________________________________________________________
;; SEND FARTHER

(new menuItem :objectName "SelectionHaloPopUpArrangeSF" :project ui)
(setf (text SelectionHaloPopUpArrangeSF) "Send Farther")
(setf (menu SelectionHaloPopUpArrangeSF) SelectionHaloPopUpArrange)

(define-handler menuSelect (SelectionHaloPopUpArrangeSF)
  (let ((items (selectedItems selectionHalo)))
    (when items
      (process-run-function "Selection Halo ArrangeSF"
                            #'(lambda()
                                (mapc #'SendFarther items)
                                (if (selecteditems selectionhalo) 
                                  (when (and (= (length items) 1) (not (objectname (car items))))
                                    (setf (text SelectionHaloTitleBar) (calctbtext)))
                                  (select items)))))
    ))


;;____________________________________________________________________
;; BRING CLOSER

(new menuItem :objectName "SelectionHaloPopUpArrangeBC" :project ui)
(setf (text SelectionHaloPopUpArrangeBC) "Bring Closer")
(setf (menu SelectionHaloPopUpArrangeBC) SelectionHaloPopUpArrange)

(define-handler menuSelect (SelectionHaloPopUpArrangeBC)
  (let ((items (selectedItems selectionHalo)))
    (when items
      (process-run-function "Selection Halo ArrangeBC"
                            #'(lambda()
                                (mapc #'BringCloser items)
                                (if (selecteditems selectionhalo) 
                                  (when (and (= (length items) 1) (not (objectname (car items))))
                                    (setf (text SelectionHaloTitleBar) (calctbtext)))
                                  (select items)))))
    ))

;;____________________________________________________________________
;; BRING TO FRONT

(new menuItem :objectName "SelectionHaloPopUpArrangeBTF" :project ui)
(setf (text SelectionHaloPopUpArrangeBTF) "Bring To Front")
(setf (menu SelectionHaloPopUpArrangeBTF) SelectionHaloPopUpArrange)

(define-handler menuSelect (SelectionHaloPopUpArrangeBTF)
  (let ((items (selectedItems selectionHalo)))
    (when items
      (process-run-function "Selection Halo ArrangeBTF"
                            #'(lambda()
                                (mapc #'BringToFront items)
                                (if (selecteditems selectionhalo) 
                                  (when (and (= (length items) 1) (not (objectname (car items))))
                                    (setf (text SelectionHaloTitleBar) (calctbtext)))
                                  (select items)))))
    ))

;;____________________________________________________________________
;; Layout Tool

(new menuItem :objectName "HaloLayout" :project ui)
(setf (text HaloLayout) "Arrange...")
(setf (menu HaloLayout) SelectionHaloPopUp)

(define-handler menuSelect (HaloLayout)
  (let ((items (selectedItems selectionHalo)))
    (process-run-function "Selection Halo HaloLayout"
                          #'(lambda()
                              (layoutdialog items)
                              (select items)))))

;;____________________________________________________________________
;; Snapshot Tool

(new menuItem :objectName "HaloSnapshot" :project ui)
(setf (menu HaloSnapshot) SelectionHaloPopUp)

(define-handler update (HaloSnapshot)
  (setf (enabled me) (and (targetproject ui) (neq (targetproject ui) sk8)))
  (if (= 1 (length (selecteditems selectionhalo)))
    (setf (text me) "Take a Snapshot")
    (setf (text me) "Take Snapshots")))

(define-handler menuSelect (HaloSnapshot)
  (process-run-function
   "Halo Snapshot"
   #'(lambda()
       (sk8::withlockedcursor animatedclock
         (let ((items (selectedItems selectionHalo))
               newbie)
           (dolist (i items)
             (tickeventclock)
             (push (new imagerenderer :project (targetproject ui)) newbie)
             (capturepicture (car newbie) i))
           (new-objects-to-overviewer (targetproject ui) (nreverse newbie))
           (select items))))))

#|
;;____________________________________________________________________
;; DETACH

(new menuItem :objectName "SelectionHaloPopUpDetach" :project ui)
(setf (text SelectionHaloPopUpDetach) "Detach")
(setf (menu SelectionHaloPopUpDetach) SelectionHaloPopUp)

(define-handler menuSelect (SelectionHaloPopUpDetach)
  (let ((items (selectedItems selectionHalo)))
    (when items
      (process-run-function nil
                            #'(lambda()
                                (withActorLocked (selectionHalo)
                                  (deselect selectionHalo)
                                  (UndoableSet 'container items nil)
                                  ))))))
|#
;;____________________________________________________________________
(new menuItem :objectName "SelectionHaloPopUpspace2" :project ui)
(setf (text SelectionHaloPopUpspace2) "-")
(setf (enabled SelectionHaloPopUpspace2) nil)
(setf (menu SelectionHaloPopUpspace2) SelectionHaloPopUp)


;;____________________________________________________________________
;; Add Property

(new menuItem :objectName "SelectionHaloPopUpAddProperty" :project ui)
(setf (text SelectionHaloPopUpAddProperty) "New Property...")
(setf (menu SelectionHaloPopUpAddProperty) SelectionHaloPopUp)

(define-handler menuSelect (SelectionHaloPopUpAddProperty)
  (let ((items (selectedItems selectionHalo)))
    (process-run-function nil
                          #'(lambda()
                              (addpropertydialog (selecteditems selectionhalo))
                              (select items)))))

;;____________________________________________________________________
;; NEW HANDLER

(new menuItem :objectName "SelectionHaloPopUpnewHandler" :project ui)
(setf (text SelectionHaloPopUpnewHandler) "New Handler...")
(setf (menu SelectionHaloPopUpnewHandler) SelectionHaloPopUp)

(define-handler update (SelectionHaloPopUpnewHandler)
  (setf (enabled me) (= (length (selectedItems selectionHalo)) 1)))

(define-handler menuSelect (SelectionHaloPopUpnewHandler)
  (let* ((items (selectedItems selectionHalo))
         (theObj (car items))
         )
    ;; name it, if it isn't already named.
    (setf (cursor stage) standardcursor)
    (process-run-function nil
                          #'(lambda()
                              (unless (EditHandlerDialog theObj)
                                (select items))
                              ))))

;;____________________________________________________________________
;; local HANDLER

(new uiMenuActor :objectName "SelectionHaloPopUplocalHandlers" :project ui)
(setf (text SelectionHaloPopUplocalHandlers) "Local Handlers")
(setf (menu SelectionHaloPopUplocalHandlers) SelectionHaloPopUp)

(new menuItem :objectName "EHmenuItem" :project ui
     :properties '(myhandler))

(define-handler update (SelectionHaloPopUplocalHandlers)
  (let ((items (selectedItems selectionHalo)))
    (if (= (length items) 1)
      (setf (enabled me) (menuItems me))
      (setf (enabled me) nil))
    ))

(define-handler menuSelect (EHmenuItem)
  (setf (cursor stage) standardcursor)
  (process-run-function nil
                        #'(lambda()
                            (withCursor watchCursor
                              (editHandlerobjectdialog (myhandler me)))
                            )))

(define-handler makeMenu (SelectionHaloPopUplocalHandlers)
  (let* (newitem 
         (items (selectedItems selectionHalo))
         (theObj (car items))
         (lhs (localHandlers theObj))
         )
    (dolist (i (menuItems me)) 
      ;; (dispose i)
      (setf (menu i) nil))
    (if lhs
      (dolist (i lhs)
        (setf newItem (new EHmenuItem :project ui))
        (setf (myhandler newitem) i)
        (setf (text newitem) (objectString i))
        (setf (menu newitem) me)))))

;;____________________________________________________________________
;; Cut/copy/paste/clear.


(define-handler sk8::cutSelectionToClipboard (selectionHalo)
  (let ((theItems (selectedItems me)))
    (when theItems
      ;; They are actors. Set the containers to nil and add them to
      ;; the clipboard.
      (deselect me)
      (dolist (c theItems)
        (setf (container c) nil))
      (addToClipboard theItems sk8Clipboard :copy nil)
      )))

(define-handler sk8::copySelectionToClipboard (selectionHalo)
  (when (selectedItems me)
    (let* ((theItems (selectedItems me))
           (newItems (mapcar #'copy theItems)))
      ;; Offset them by something.
      (dolist (c newItems)
        (setLocation c 5 5 :relative t :physical t))
      (addToClipboard newItems sk8Clipboard :copy nil))))

(define-handler sk8::pasteClipboardToSelection (selectionHalo) 
  (when (selectedItems me)
    (let ((theContainer (container (car (selectedItems me))))
          newActors)
      (setf newActors (getFromClipboard sk8Clipboard actor (targetproject ui) :copy t ))
      (dolist (c newActors)
        (setf (container c) theContainer))
      (select newactors))))

(define-handler clearSelection (selectionHalo)
  (menuSelect SelectionHaloPopUpDispose))

(define-handler commandKeyEvent (selectionHalo theChar)
  (case theChar
    (#\c (sk8::copySelectionToClipboard me))
    (#\x (sk8::cutSelectionToClipboard me))
    (#\v (sk8::pasteClipboardToSelection me))
    (otherwise (commandkeyevent ProjectBuilderMenubar thechar))
    ))


#|
;;____________________________________________________________________
;; Moviefy

(new menuItem :objectName "SelectionHaloPopUpMoviefy" :project ui)
(setf (text SelectionHaloPopUpMoviefy) "Moviefy")
(setf (menu SelectionHaloPopUpMoviefy) SelectionHaloPopUp)

(define-handler menuSelect (SelectionHaloPopUpMoviefy)
  (let* ((tla (currentwindow))
         (items (and tla (selectedItems tla)))
         (theObj (car items)))
    (process-run-function nil
                          #'(lambda()
                              (if (not (inheritsFrom theobj quicktimemovie))
                                (moviefy theobj))
                              (linkmovietofile theObj)))))
|#


#|
	Change History (most recent last):
	3	5/11/93	Brian Roddy	Moviefy menu
	4	5/11/93	Brian Roddy	Added a lot of deselect halos in the menuitems for the demo
	5	5/11/93	Brian Roddy	fixed drag selection deep to hide halo
	6	5/24/93	Hernan	Changing menus to conform to new menus!
	7	5/24/93	Hernan	Ooops! All menus used by the UI descend from
				uiMenuActor (not menu!).
	8	5/28/93	Hernan	Changed the way menus deal with colors.
	9	6/1/93	Brian Roddy	
	10	6/1/93	Brian Roddy	Added set keyTarget option
	21	6/28/93	rod	Made it use Hernan's wonderful resize otherActors
	22
	115	8/1/94	Flosm	
	116	8/2/94	rod	More menu reordering.
	117	8/2/94	chip	
	118	8/9/94	rod	Fixed bug where selection halo was reappearing
				when new handler was chosen.
	119	 8/30/94	rod     	
	120	 9/ 2/94	rod     	
	121	 9/ 2/94	rod     	
	122	 9/ 2/94	sidney  	espysans -> espysansFont.
	123	 9/12/94	rod     	
	124	 9/12/94	rod     	Fixing dragging of multiple items where one item
							is contained by another item.
	125	 9/21/94	Hernan  	Getting rid of useless variable in makeBoundsRegion.
							Also, as instructed by Brian, changed 
							sk8dev::getContainers to sk8::containers.
	126	 9/26/94	rod     	Fixed the Arrange menu to update fully.
	127	 9/28/94	Hernan  	The selectionHaloResizer cannot use the region
							offsetting optimization.
	128	11/ 1/94	rod     	having edit use uiedit protocol.
	129	12/ 5/94	rod     	
	130	12/16/94	rod     	Warning the user if they select a locked window.
	131	 1/17/95	rod     	
	132	 1/30/95	rod     	Fixing that bug of the halo not being lined
							up.
	133	 2/ 3/95	Hernan  	WithLockedCursor is now correct. I check this file
							out and back in to force it to be recompiled.
	134	 2/16/95	rod     	clear reference work.
	135	 3/ 3/95	Hernan  	1224138: in dragSelectionDeep, the boundsRect
							should be set with justMoving set to True.
	136	 3/ 8/95	Hernan  	1225933: when dragging the halo we send an
							update event to every window in order to erase
							the old image of the halo. This is ne because the
							halo might be overlaping any window.
	137	 4/25/95	rod     	
	138	 4/26/95	rod     	
	139	 4/27/95	rod     	Removing "MaybeRemove" from deactivate.  Why
							am i getting an additional deactivate event???
	140	 4/27/95	rod     	Fixing maybeRemove to only work when the
							halo is onstage (duh).
	141	 4/28/95	rod     	1244549 and 1244554: Fixing clear referencing
							menu to be simple.
	142	 4/28/95	Brian   	Fixing typo.
	3  	 2/14/96	Brian   	fixing calls to external packages.
	4  	 2/15/96	Brian   	removing references to dataobject.
	2  	 4/10/96	Hernan  	define-system-handler->define-handler.
	3  	 7/ 7/96	sidney  	changes for native PPC build
	4  	 9/30/96	sidney  	changes to put sk8 windows in separate threads
	5  	10/13/96	sidney  	prevent infinite loop deactivating selectionhalo
	6  	11/12/96	sidney  	name some anonymous threads
	7  	 2/11/97	Brian Roddy	
	8  	 2/13/97	Brian Roddy	
	9  	 2/27/97	Hernan  	
|# ;(do not edit past this line!!)
