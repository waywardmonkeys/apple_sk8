;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories


;;;
(in-package :uidevelopment)

(SK8-declare-syms :UI :public ; Updated  3-17-94   2:39 pm
                  UI::ACTOROKFORCONTENTS)


(new UISimpleWindow :objectname "StageOverviewer" :project ui)
(setf (sk8::menubar StageOverviewer) nil)
(setf (resizer StageOverviewer) t)
(setf (zoombox StageOverviewer) t)

(setf (actor PBMenuStageViewer) stageOverviewer)

(define-handler clearReferences (StageOverviewer &key ((:objects theobjects)))
  (if theobjects
    (when (and theobjects (some #'(lambda (x) (is-a x actor) (eq (project x) (targetProject ui))) theobjects))
      (updateEditor me))  ;;;;;;;;;;;*****NEED TO ADDRESS!!!
    (let ((itos (items (offstagelist me)))
          (its (items (stagelist me))))
      (logoutswatches (picker SOStageList) (mapcar #'swatch its)) 
      (logoutswatches (picker SOStageList) (mapcar #'swatch itos)) 
      (setf (items (stagelist me)) nil)
      (setf (items (offstagelist me)) nil)
      ))
  )

(define-handler SetUpForProject (StageOverviewer &key ((:project theproject)))
  (setf (text me) (concatenate 'string (objectstring theproject :project theproject) " Stage Monitor"))
  (updateEditor me))


(define-handler updateEditor (StageOverviewer &key (propertyname nil) (items nil))
  (declare (ignore propertyname items))
  (when (container me)
    (sk8::withlockedcursor animatedClock
      (withactorlocked (me)
        (let* ((psl (picker (stagelist me)))
               (posl (picker (offstagelist me)))
               (allits (append (items psl) (items posl)))
               (itlist (remove-if-not #'(lambda (x) (eq (state x) 'sk8::open)) 
                                      allits))
               (nosl (newselection (OffstageList me)))
               (nsl (newselection (stageList me)))
               stillgo iosl isl)
          (mapcar #'(lambda (x) (setf (depth x) 0)) allits)
          (setf (cacheditems (offstagelist me)) allits)
          (setf (cacheditems (stagelist me)) allits)
          (tickeventclock)
          (setupitems (offstagelist me))
          (tickeventclock)
          (setupitems (stagelist me))
          (loop
            (setf stillgo nil)
            (dolist (i itlist)
              (dolist (j (items (stagelist me)))
                (when (eq (value j) (value i)) 
                  (push i stillgo)
                  (if (hasContents (value j))
                    (openItem (picker (stagelist me)) j)
                    (setf (state j) nil))))
              (dolist (j (items (offstagelist me)))
                (when (eq (value j) (value i)) 
                  (push i stillgo)
                  (if (hasContents (value j))
                    (openItem (picker (offstagelist me)) j)
                    (setf (state j) nil)))))
            (setf itlist (set-difference itlist stillgo))
            (unless stillgo (return))
            )
          (tickeventclock)
          (setf isl (items psl)
                iosl (items posl))
          (dolist (i allits)
            (unless (or (memq i isl) (memq i iosl))
              (logoutswatches (picker SOStageList) (swatch i))
              ))
          (setf (cacheditems (offstagelist me)) nil)
          (setf (cacheditems (stagelist me)) nil)
        (tickeventclock)
          (when nsl
            (unless (listp nsl) (setf nsl (list nsl)))
            (setf (selecteditems psl) (or (remove-if-not #'(lambda (x) (memq (value x) nsl)) isl)
                                          (car isl)))
            (setf (newselection (stagelist me)) nil))
          (when nosl
            (unless (listp nosl) (setf nosl (list nosl)))
            (setf (selecteditems posl) (or (remove-if-not #'(lambda (x) (memq (value x) nosl)) iosl )
                                           (car iosl)))
            (setf (newselection (offstagelist me)) nil))
          )))))

;;;;__________________________________________________________
(new UIHOL :objectname "ContentsViewer" :project ui
     :properties '(defaultContainer cachedItems newselection))

(setf (relation (picker ContentsViewer)) 'contents)
(setf (arrowsize (picker ContentsViewer)) '(16 16))
(setf (alphabeticalDisplay (picker ContentsViewer)) nil)

(define-handler GenerateItems ((picker ContentsViewer) ItemList)
  (let (newlist 
        (SwatchesShown (SwatchesShown me))
        (relation (relation me))
        (p (project me))
        cached?)
    (dolist (i ItemList)
      (setf cached? nil)
      (dolist (j (cachedItems (container me)))
        (when (eq (value j) i)
          (setf cached? j)
          (setf (state j) (if (and relation (memq relation (properties i)) 
                                   (funcall relation i))
                            'closed 
                            nil))))
      (push (or cached?
                (new hierarchicalpickeritem 
                     :project p 
                     :value i
                     :state (if (and relation (memq relation (properties i)) 
                                     (funcall relation i))
                              'closed 
                              nil)
                     :swatch (if swatchesshown (generateswatch i (picker SOStageList)) nil))) ;;; We say swatch is in one of the two and make sure that when this is redraw, so is the other.  
            newlist)
      )
    newlist))

#|
(define-handler doubleclick ((picker ContentsViewer))
  (if (selecteditems me)
    (editobjects (mapcar #'value (selecteditems me)))
    (call-next-method)))
|#



;;;This logs out the swatches of the items being closed.  This is duplicate code
;;;and there should be a nicer way of doing this...  **
(define-handler CloseItem ((picker ContentsViewer) TheItem)
  (let* ((newits (copy-list (items me)))
         (lenny (length newits))
         (pos1 (position theitem newits))
         (pos2 (1+ pos1))
         (dep (depth theitem))
         (theDead nil))
    (withactorlocked (me)  
      (loop 
        (if (or (= pos2 lenny) (<= (depth (nth pos2 newits)) dep)) (return))
        (incf pos2))
      (setf theDead (subseq newits (1+ pos1) pos2))
      (logoutswatches (picker SOStageList) (mapcar #'swatch thedead))
      (call-next-method))))


(defun ActorOKForContents (me)
  (let ((act (object objectDataRect))
        (dc (defaultcontainer (container me))))
    (unless (listp act) (setf act (list act)))
    (and (neq (componentfrom objectdatarect) me)
         (every #'(lambda (x) (is-a x Actor)) act)
         (every #'(lambda (x) (eq (project x) (targetproject ui))) act)
         (every #'(lambda (x) (neq (container x) dc )) act))
    ))

(define-handler (setf Highlight) (theval (picker ContentsViewer))
  (setf (inverts me) nil)
  (if theval
    (setf (fillcolor me) white)
    (setf (fillcolor me) shadowwhite)
    )
  (call-next-method))

(define-handler draggingMouseEnter ((picker ContentsViewer) actorDragged)
  (if (and (eq actordragged objectDataRect)
           (ActorOKForContents me))
    (setf (highlight me) t)
    (call-next-method)))

(define-handler draggingMouseWithin ((picker ContentsViewer) actorDragged)
  (if (eq actordragged objectDataRect)
    (let* ((obj (object objectDataRect))
           (powp (pointonwhichpart me (car (mouseloc stage)) (cadr (mouseloc stage))
                                   :part 'ItemAndPosition))
           (val (and (eq (cadr powp) 'icon) (value (nth (car powp) (items me)))))
           (conts (and val (sk8::containers val))))
      (unless (listp obj) (setf obj (list obj)))
      (if (and (eq (cadr powp) 'icon)
               (every #'(lambda (x) (is-a x Actor)) obj)
               (every #'(lambda (x) (neq x val)) obj)
               (every #'(lambda (x) (eq (project x) (targetproject ui))) obj)
               (every #'(lambda (x) (neq (container x) val)) obj)
               (every #'(lambda (x) (not (memq x conts))) obj)
               )
        (progn
          (unless (highlight me) (setf (highlight me) t))
          (flashitem me)
          )
        (unless (ActorOKForContents me) (draggingmouseleave me actordragged))
        ))
    (call-next-method)))

(define-handler dropped ((picker ContentsViewer) droppee)
  (when (highlight me)
    (call-next-method)
    (withActorLocked (me)
      (let* ((xx (car (mouseloc stage)))
             (yy (cadr (mouseloc stage)))
             (obj (object objectDataRect))
             (powp (pointonwhichpart me xx yy
                                     :part 'ItemAndPosition))
             (val (and (eq (cadr powp) 'icon) (value (nth (car powp) (items me)))))
             (dc (defaultcontainer (container me))))
        (if (eq (cadr powp) 'icon)
            (progn
              (undoableset 'container obj val)
              (setf (newselection (container me)) obj)
              (bringtofront stageoverviewer)
              (setf (keytarget stageoverviewer) me))
            (progn
              (undoableset 'container obj dc)
              (setf (newselection (container me)) obj)
              (bringtofront stageoverviewer)
              (setf (keytarget stageoverviewer) me))
            )))))


(define-handler draggingMouseEnter ((titlebar ContentsViewer) actorDragged)
  (let ((cf  (componentfrom objectdatarect)))
    (setf (componentfrom objectdatarect) nil)
    (draggingMouseEnter (picker (container me)) actorDragged )
    (setf (componentfrom objectdatarect) cf)))

(define-handler draggingMouseLeave ((titlebar ContentsViewer) actorDragged)
  (draggingMouseLeave (picker (container me)) actorDragged ))

;;;This stuff is a duplicate of some of the dropped stuff above...
(define-handler dropped ((titlebar ContentsViewer) droppee)
  (setf (componentfrom objectdatarect) nil)
  (when (highlight (picker (container me)))
    (withActorLocked (me)
      (undoableset 'container (object objectDataRect) (defaultcontainer (container me)))
      (setf (newselection (container me)) (object objectDataRect))
      (bringtofront stageoverviewer)
      (setf (keytarget stageoverviewer) me))))

;;;;__________________________________________________________
(new ContentsViewer :objectname "SOStageList" :project ui)
(setf (container SOStageList) StageOverviewer) 
(setf (defaultContainer SOStageList) stage)
(setf (relation (picker SOStageList)) 'contents)
(setf (selectionstyle (picker SOStageList)) 'discontiguous)
(setf (title SOStageList) "On Stage")
(tagpart StageOverviewer SOStageList 'StageList)

(define-handler resized (SOStageList)
  (call-next-method)
  (setboundsrect (insetrect me) 0 0 0 4 :relative t)
  )

(define-handler doubleclick ((picker SOStageList))
  (if (selecteditems me)
    (progn
      (sendtoback (sk8::window me))
      (select (mapcar #'value (selecteditems me)))
      )
    (call-next-method)))

(define-handler setupitems (SOStageList)
  (let ((proj (targetProject ui)))
    (setf (inputobjects (picker me)) 
          (and proj
               (neq proj sk8)
               (delete-if #'(lambda (x) (memq x (uiClearedObjects)))
                                         (delete-if-not #'container 
                                                        (sk8::windows proj)))))
    (mapcar #'(lambda (x) (setf (depth x) 0)) (items (picker me)))
    ))

(define-handler lightforceredraw ((picker SOStageList))
  (call-next-method)
  (lightforceredraw SOOffStageList))

(define-handler keydown ((picker SOStageList) thechar)
  (if (eq thechar #\tab)
    (setf (keytarget stageoverviewer) (picker SOOffstagelist))
    (call-next-method))
  )



;;;;__________________________________________________________
(new ContentsViewer :objectname "SOOffStageList" :project ui)
(addparent SOOffStageList uiTextListForCorners)
(setf (container SOOffStageList) StageOverviewer)
(setf (relation (picker SOOffStageList)) 'contents)
(setf (selectionstyle (picker SOOffStageList)) 'discontiguous)
(setf (title SOOffStageList) "Off Stage")
(setf (alphabeticaldisplay SOOffStageList) nil)
(setf (defaultContainer SOOffStageList) nil)
(hide (titlebar SOOffStageList))
(tagpart StageOverviewer SOOffStageList 'OffStageList)

;;;;__________________________________________________________________________________________
  
(define-handler setupitems (SOOffStageList)
  (let ((proj (targetProject ui)))
    (setf (inputobjects (picker me)) 
          (and proj
               (neq proj sk8)
               (delete-if #'(lambda (x) (memq x (uiClearedObjects)))
                                         (delete-if #'container (sk8::windows proj)))))
    (mapcar #'(lambda (x) (setf (depth x) 0)) (items (picker me)))
    ))
(define-handler keydown ((picker SOOffStageList) thechar)
  (if (eq thechar #\tab)
    (setf (keytarget stageoverviewer) (picker SOStageList))
    (call-next-method))
  )

;_________________________________________________________________________________________________________

(new uiSplitter :objectname "SOSplitter" :project ui
     :properties '((Proportion :value 0.6)))

(setf (container SOSplitter) StageOverviewer)
(setf (text SOSplitter) "Off Stage")
(tagpart StageOverviewer SOSplitter 'Splitter)
(define-handler drag (SOSplitter &key live otherActors dropEvents DraggedOverEvents 
                                  draggingMouseWithinEvents onStage constrainingRect)
  (declare (ignore live otherActors dropEvents DraggedOverEvents 
                   draggingMouseWithinEvents onStage constrainingRect))
  (withActorLocked (me)
    (call-next-method)
    (setf (proportion me) 
          (/ (- (top me) (absolutetop me)) (- (absoluteBottom me) (absolutetop me))))
    (resized (sk8::window me)))
  )
(define-handler bestsize (SOSplitter)
  (let* ((width (width (container me)))
         (v (+ (absoluteTop me) (* (- (absoluteBottom me) (absoluteTop me)) (Proportion me)))))
    (setBoundsRect me (if *MacStyleInterface* *windowLeft* 5) v (- width (if *MacStyleInterface* *windowRight* 5)) (+ v 2))
    ))

;;;because the splitter covers the titlebar we alias to it...
(define-handler draggingMouseEnter (SOSplitter actorDragged)
  (draggingMouseEnter (titlebar SOOffstageList) actorDragged ))
(define-handler draggingMouseLeave (SOSplitter actorDragged)
  (draggingMouseLeave (titlebar SOOffstageList) actorDragged ))
(define-handler dropped (SOSplitter droppee)
  (dropped (titlebar SOOffstageList) droppee ))



(define-handler setBoundsRect (SOSplitter left top right bottom 
                                             &key physical relative justMoving)
  (let ((abstop (absoluteTop me))
        (absbot (absoluteBottom me)))
    (unless relative
      (setf top (+ (+ 23 abstop) (* 36 (floor (- top (+ 23 abstop)) 36))))
      (if (< top (+ 38 abstop))
        (progn
          (setf (visible SOStageList) nil)
          (setf (keytarget StageOverviewer) (picker SOOffStageList))
          (setf top abstop ))
        (unless (visible SOStageList)
          (setf (visible SOStageList) t)
          ))
      (if (> top (- absbot 38))
        (progn
          (setf (visible SOOffStageList) nil)
          (setf (keytarget StageOverviewer) (picker SOStageList))
          (setf top (- absbot 19) ))
        (unless (visible SOOffStageList)
          (setf (visible SOOffStageList) t)
          ))
      )
    (call-next-method me left top right (+ top 19) :physical physical 
                      :relative relative :justmoving justmoving)
    ))


(define-handler setBoundsRect (StageOverviewer left top right bottom 
                                                  &key physical relative justMoving)
  (unless (or relative (< (- bottom top) 20))
    (setf bottom (+ top 43 (* 36 (floor (- bottom top 43) 36))))
    )
  (call-next-method me left top right bottom :physical physical 
                    :relative relative :justmoving justmoving)
  )



(splitActors SOSplitter 
             (list SOStageList) (list SOOffStageList)
             (list SOStageList SOOffStageList)
             30 500)

;_________________________________________________________________________________________________________
;_________________________________________________________________________________________________________

(define-handler resized (StageOverviewer)
  (let (hsize vsize)
    (declare (special hsize vsize))
    (withActorLocked (me)
      (sk8-multival-setf (hsize vsize) (size me))
      (setBoundsRect (StageList me) 0 23 hsize vsize)
      (setBoundsRect (OffStageList me) 0 0 hsize (- vsize 25))
      (setf (absoluteTop (Splitter me)) (+ 3 *WindowTop*))
      (setf (absoluteBottom (Splitter me)) (- vSize 20))
      (bestsize (splitter me))
      (setBoundsRect (StageList me) *WindowLeft* (+ 3 *WindowTop*) (- hsize *WindowRight*) (top (splitter me)))
      (setBoundsRect (OffStageList me) *WindowLeft* (bottom (splitter me)) (- hsize *WindowRight*) (- vsize *WindowBottom*))
      (call-next-method))
    ))
(setboundsrect StageOverviewer 350 25 575 402)
(setboundsrect (Splitter StageOverviewer) 5 182 100 201)
(resized StageOverviewer)

(setf (minimumsize stageoverviewer) '(155 180))
(define-handler enteringStage (StageOverviewer)
  (setf (text me) (concatenate 'string (objectstring (targetProject ui) :project (targetProject ui)) " Stage Monitor"))
  (setf (keytarget me) (picker (stagelist me)))
  (updateeditor me))

#|
	Change History (most recent last):
	1	2/14/94	rod	
	2	2/14/94	rod	
	3	2/14/94	rod	
	4	2/15/94	rod	
	5	2/15/94	rod	
	8	2/22/94	kleiman	The Final Renaming for Alpha! (yeah, right...)
				72/21/94hernanwindows -> sk8::windows.
	9	2/25/94	rod	
	10	2/25/94	hernan	Using symbols instead of keywords for user
				visible options.
	11	2/28/94	rod	
	12	2/28/94	hernan	Avoiding calling dispose directly.
	13	2/28/94	hernan	Avoiding calling dispose directly.
	14	3/1/94	brian	Fixed missing setupitems
	15	3/3/94	Hernan	The great handler argument name renaming of 94!
	17	3/5/94	rod	Changing draw palette
	18	3/8/94	rod	
	19	3/9/94	rod	Doing Project Switching and Reference Clearing.
	20	3/9/94	rod	Doing Project Switching and Reference Clearing.
	21	3/10/94	rod	
	22	3/11/94	rod	
	23	3/17/94	rod	
	24	3/17/94	rod	Drag Multiple.
	25	3/23/94	rod	
	26	3/23/94	rod	Yet more GC work.
	27	3/26/94	rod	
	28	3/26/94	rod	Minimum size stuff.
	29	3/26/94	rod	
	30	3/28/94	rod	
	31	3/29/94	rod	
	32	3/30/94	rod	WatchCursor stuff.
	35	4/18/94	rod	
	36	4/20/94	rod	
	37	4/22/94	Hernan	Made resized do its thing with the actor locked.
	42	7/13/94	rod	
	43 	 8/24/94	rod     	
	44 	 9/ 6/94	rod     	
	45 	 9/12/94	rod     	
	46 	 9/21/94	rod     	
	47 	10/10/94	rod     	Fixing keyword args...
	48 	11/ 7/94	rod     	Fixing drop on splitter.
	49 	11/15/94	rod     	qualifying open symbol
	50 	 2/ 3/95	Hernan  	WithLockedCursor is now correct. I check this file
							out and back in to force it to be recompiled.
	2  	 2/14/96	Brian   	fixing population stuff
	3  	 2/14/96	Brian   	fixing calls to external packages.
	2  	 2/27/97	Hernan  	
|# ;(do not edit past this line!!)
