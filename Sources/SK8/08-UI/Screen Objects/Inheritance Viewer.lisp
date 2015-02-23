;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories


;;;
(in-package :uidevelopment)


(new UISimpleWindow :objectname "InheritanceViewer" :project ui
      :properties '(SelectedItems recalcOnScroll oldselection))
(setf (sk8::menubar InheritanceViewer) t)
(setf (resizer InheritanceViewer) t)
(setf (zoombox InheritanceViewer) t)
(setf (minimumsize InheritanceViewer) '(215 184))

(define-handler clearReferences (InheritanceViewer &key ((:objects theobjects)))
  (if theobjects
    (withActorLocked (me)
      (cond 
       ((some #'(lambda (theobject) (memq theobject (selecteditems me))) theobjects)
        (setf (selecteditems me) (remove-if #'(lambda (x) (memq x theobjects)) (selecteditems me))))
       ((some #'(lambda (theobject) (or (some #'(lambda (x) (eq x theobject)) (items (picker ivchildren))) 
                                        (some #'(lambda (x) (eq x theobject)) (items (picker ivparents))))) theobjects)
        (setf (selecteditems me) (selecteditems me)))
       ))
    (setf (selecteditems me) nil)))


(setf (oldselection InheritanceViewer) object)
(setf (actor PBMenuInheritanceViewer) inheritanceviewer)

(define-handler (setf SelectedItems) (theval InheritanceViewer)
  (sk8::withLockedCursor animatedClock
    (withActorlocked (me)
      (unless (listp theval) (setf theval (list theval)))
      (sk8::setValue 'SelectedItems me theval)
      (unless (equal (outputobjects ivnamefield) theval) (setf (inputObjects ivnamefield) theval))
      (setupparents me)
      (setUpChildren me :clipping t)
      (forceredraw me)
      )))

(define-handler setupparents (inheritanceviewer)
  (let ((theval (selecteditems me))
        pars numpar numanc)
    (setf pars (and theval (parents (car theval))))
    (dolist (i (cdr theval))
      (setf pars (remove-if-not #'(lambda (x) (memq x (parents i))) pars)))  ;;; Don't use intersection cause we need to preserve the ordering...
    (setf numpar (length pars))
    (setf numanc (length (remove-duplicates (apply #'append (mapcar #'ancestors theval))
                                            :test #'eq)))
    (setf (text ParentInfo) (concatenate 'string 
                                         (objectstring numpar)
                                         (if (> (length theval) 1)
                                           " Shared" "")
                                         (if (= numpar 1)
                                           " Parent and "
                                           " Parents and ")
                                         (objectstring numanc)
                                         (if (> (length theval) 1)
                                           " Shared" "")
                                         (if (= numanc 1)
                                           " Ancestor"
                                           " Ancestors")))
    (setf (inputObjects (picker ivparents)) pars)
    ))

(define-handler setUpChildren (inheritanceviewer &key (clipping nil))
  (let ((theval (selecteditems me))
        kids numkids)
    (setf kids (and theval (knownchildren (car theval))))
    (when (and theval (targetproject ui) (neq (targetproject ui) sk8))
      (mapprojectobjects (targetproject ui) 
                         #'(lambda (x) (when (and (not (objectname x))
                                                  (not (memq x kids))
                                                  (some #'(lambda (y) (memq y theval)) (parents x))) 
                                         (push x kids))))
      (setf kids (delete-if #'(lambda (x) (memq x (uiClearedObjects))) kids)))
    (dolist (i (cdr theval))
      (setf kids (intersection kids (knownchildren i) :test #'eq)))
    (setf kids (remove-if #'(lambda (x) (eq (project x) ui)) kids))
    ; (setf kids (remove-if #'private kids))
    (setf kids (mapcar #'(lambda (x) (list (objectstring x) x)) kids))
    (setf kids (sort kids #'string-lessp :key #'car))
    (setf kids (mapcar #'cadr kids))
    (setf numkids (length kids))
    (withactorlocked (me)
      (setf (inputObjects (picker ivchildren)) nil)
      (setf (inputObjects (picker ivchildren)) kids))
    ;  (setup ChildrenConnections)
    (setf (text ChildrenInfo) (concatenate 'string 
                                           (objectstring numkids)
                                           (if (> (length theval) 1)
                                             " Shared" "")
                                           (if (= numkids 1)
                                             " KnownChild"
                                             " KnownChildren")
                                           ))))

;;;;__________________________________________________________

(setf (text InheritanceViewer) "Inheritance Overviewer")

#|
(setf (container IVcLeft) nil)
(setf (container IVcright) nil)
|#
;;;;__________________________________________________________

(new UITextlist :objectName "IVParents" :project ui)
(setf (pickerprototype IVParents) ObjectPilePicker)
(setf (container IVParents) InheritanceViewer)
(setf (title IVParents) "Parents")
(setf (editing (picker IVParents)) t)
(setf (selectionstyle (picker IVParents)) 'discontiguous)
(setf (alphabeticaldisplay (picker IVParents)) nil)

(define-handler doubleclick ((picker IVParents))
  (when (and (items me) (selecteditems me))
    (setf (selecteditems inheritanceviewer) (selecteditems me))))

(define-handler keydown ((picker IVParents) theChar)
  (cond 
   ( (and (eq thechar #\delete) (car (selecteditems me)))
     (when (removeparentdialog (selectedItems Inheritanceviewer)  (car (selecteditems me)))
       (withactorlocked (me)
         (setupParents inheritanceViewer)
         (forceredraw inheritanceViewer))))
   ((or (eq theChar #\Return) (eq theChar #\Enter)) 
    (doubleclick me))
   ((eq theChar #\tab) 
    (setf (keytarget INheritanceviewer) (textfield IVNamefield)))
   (t (call-next-method))))
(define-handler draggingMouseEnter ((picker IVParents) actorDragged)
  (let ((xx (object ObjectDataRect)))
    (unless (listp xx) (setf xx (list xx)))
    (when (or (neq actorDragged objectdatarect)
              (and ;(eq actorDragged objectdatarect)
               (not (some #'(lambda (x) (memq x (selectedItems Inheritanceviewer))) xx))
               (or
                (every #'(lambda (x) (memq x (items (picker ivparents)))) xx)
                (not (some #'(lambda (x) (memq x (ancestors (car (selectedItems Inheritanceviewer))))) xx)))
               (not (some #'(lambda (x) (memq (car (selectedItems Inheritanceviewer)) (ancestors x))) xx))
               ))
      (call-next-method))))

(define-handler draggingMouseWithin ((picker IVParents) actorDragged)
  (when (highlight me)
    (call-next-method))
  )
(define-handler dropped ((picker IVParents) droppee)
  (when (highlight me)
    (let ((sels (selecteditems inheritanceViewer))
          io)
      (draggingMouseLeave me droppee)
      (call-next-method)
      (when (eq droppee objectdatarect)
        (setf io (inputobjects me))
        (when (yesornodialog (concatenate 'string 
                                          "Do you wish to change the Parents of "
                                          (objectstring (if (= (length sels) 1) (car sels) sels) :project (targetproject ui))
                                          " to "
                                          (objectstring io :project (targetproject ui)))
                             :cancel nil
                            ) 
          (dolist (i sels)
            (ChangeParents i io)))
        (setupparents inheritanceViewer)
        ))))

(new StandardInputField :objectName "IVNameField" :project ui)
(setf (container IVNameField) InheritanceViewer)
(hide (historymenu IVNameField))
(define-handler (setf items) (theval (historymenu IVNameField))
  nil)

(define-handler (setf outputobjects) (theval IVNameField)
  (call-next-method)
  (unless (equal theval (selecteditems InheritanceViewer)) (setf (selecteditems InheritanceViewer) theval)))

(define-handler keydown ((textfield IVNameField) theChar)
  (cond
   ((eq theChar #\tab) 
    (setf (keytarget InheritanceViewer) (picker IVChildren)))
   (t (call-next-method))))

(new UITextListForCorners :objectName "IVChildren" :project ui)
;;(addparent (picker IVChildren) styledpicker)
;;(uicolorize (picker IVChildren))
;;(uicolorize IVParents)
(setf (selectionstyle (picker IVChildren)) 'discontiguous)
(setf (container IVChildren) InheritanceViewer)
(setf (title IVChildren) "Children")
(define-handler keydown ((picker IVChildren) theChar)
  (cond
   ((or (eq theChar #\Return) (eq theChar #\Enter)) 
    (doubleclick me))
   ((eq theChar #\tab) 
    (setf (keytarget InheritanceViewer) (picker IVParents)))
   (t (call-next-method))))
(define-handler doubleclick ((picker IVChildren))
  (when (and (items me) (selecteditems me))
    (setf (selecteditems inheritanceviewer) (selecteditems me))))


;;; Returns... {font,size,style,color}

(define-handler createTextItemStyle ((picker IVChildren) item theString position)
  (declare (ignore  theString position))
  (sk8-multivals nil nil (if (memq item (knownchildren (car (selecteditems InheritanceViewer)))) '(plain) '(italic)) nil))


;;;----------------------------------------------------------------------------------------------------------
;;;----------------------------------------------------------------------------------------------------------

(new uiMenuBarActor :otherparents browserMenuBar :objectname "IVMenubar" :project ui)

(setf (container IVMenubar) InheritanceViewer)
(tagpart InheritanceViewer IVMenubar 'Bar)

(new uimenuactor :objectname "parentsMenu" :project ui)
(setf (sk8::menubar parentsMenu) ivmenubar)
(setf (text parentsMenu) "Parents")

(new menuitem :objectname "AddParentMenuItem" :project ui)
(setf (menu AddParentMenuItem) parentsMenu)
(setf (text AddParentMenuItem) "Add Parent")
(define-handler update (AddParentMenuItem)
  (setf (enabled me) (items ivparents))
  )
(define-handler menuselect (AddParentMenuItem)
  (when (addParentDialog (selectedItems Inheritanceviewer)  object)
    (withactorlocked (Inheritanceviewer)
      (setupparents inheritanceViewer)
      (forceredraw inheritanceViewer)))  )

(new menuitem :objectname "RemoveParentMenuItem" :project ui)
(setf (menu RemoveParentMenuItem) parentsMenu)
(setf (text RemoveParentMenuItem) "Remove Parent")
(define-handler update (RemoveParentMenuItem)
  (setf (enabled me) (items ivparents))
  (setf (text me) (if (selecteditems ivparents)
                    (concatenate 'string "Remove Parent " 
                                 (objectString (car (selecteditems ivparents)) :project (targetproject ui)))
                    "Remove Parent"
                    ))
  )
(define-handler menuselect (RemoveParentMenuItem)
  (keydown IVParents #\delete))

(new menuspacer :objectname "IVMenuspacer" :project ui)
(setf (menu IVMenuspacer) parentsMenu)

(new menuitem :objectname "SpliceParentMenuItem" :project ui)
(setf (menu SpliceParentMenuItem) parentsMenu)
(setf (text SpliceParentMenuItem) "Splice in a New Parent")
(define-handler update (SpliceParentMenuItem)
  (setf (enabled me) (items ivparents))
  )
(define-handler menuselect (SpliceParentMenuItem)
  (let (name x)
    (setf name (getanswerfromuser "Please enter an ObjectName for the New Parent" :allowspaces nil))
    (setf  x (splicenewparent (selectedItems Inheritanceviewer) :objectname name))
    (when x (setf (prototype x) t)))
  (setupparents inheritanceviewer)
  )

(new (menuprototype objectpicker) :project ui :objectname "ChildrenMenu")
(setf (sk8::menubar ChildrenMenu) ivmenubar)
(setf (text ChildrenMenu) "Children")
(setf (browsercomponent childrenMenu) (picker ivchildren))
(define-handler menuselect ((car (menuitems childrenmenu)))
  (getNewFromUser (car (selecteditems inheritanceviewer)) :project (targetproject ui))
  (setupchildren inheritanceviewer :clipping t))
(define-handler update ((car (menuitems childrenmenu)))
  (let ((sels (car (selecteditems inheritanceviewer))))
    (setf (commandkey me) #\N)
    (setf (enabled me) sels)
    (setf (text me)
          (if sels
            (concatenate 'string "New " (objectstring sels :project (targetproject ui)))
            "New"))))
(addparent ChildrenMenu uimenuactor)

(setf (menu (new menuspacer :project ui)) ChildrenMenu)

(new menuitem :objectname "IVKnownUnknown" :project ui)
(setf (menu IVKnownUnknown) ChildrenMenu)
(setf (commandkey IVKnownUnknown) #\K)
(define-handler update (IVKnownUnknown)
  (setf (text me) "Mark child as Known/Unknown")
  (let ((xx (selecteditems ivchildren))
        ok)
    (when (and xx
               (not (some #'objectname xx)))
      (setf ok t)
      (if (every #'(lambda (x) (memq x (knownchildren (car (selecteditems InheritanceViewer))))) xx)
        (setf (text me) "Remove Items from KnownChildren")
        (setf (text me) "Add Items to KnownChildren"))
      )
    (setf (enabled me) ok)))
(define-handler menuselect (IVKnownUnknown)
  (let ((xx (selecteditems ivchildren)))
    (if (every #'(lambda (x) (memq x (knownchildren (car (selecteditems InheritanceViewer))))) xx)
      (setf (knownchildren (car (selecteditems InheritanceViewer))) 
            (delete-if #'(lambda (x) (memq x xx)) (knownchildren (car (selecteditems InheritanceViewer)))))
      (setf (knownchildren (car (selecteditems InheritanceViewer))) 
            (delete-duplicates (append xx (knownchildren (car (selecteditems InheritanceViewer)))) :test #'eq))
      )
    (setupchildren inheritanceviewer)
    (setf (selecteditems ivchildren) xx)
    )
  )

;;;----------------------------------------------------------------------------------------------------------
;;;----------------------------------------------------------------------------------------------------------

(define-handler resized (InheritanceViewer)
  (sk8-multival-bind (hsize vsize) (size me)
    (let ((objwidth (round hsize 2)))
      (withActorLocked (me)
        (setBoundsRect (Bar me) *BarLeft* *WindowTop* hsize (+ 20 *WindowTop*))
        (setboundsrect IVParents *WindowLeft* (+ 70 *WindowTop*) (- objwidth 10) (- vsize *WindowBottom*))
        (setboundsrect IVNameField (+ *WindowLeft* 10) (+ 25 *WindowTop*) (- hsize *WindowRight* 10) (+ 60 *WindowTop*))
        (setboundsrect IVChildren (+ objwidth 10) (+ 70 *WindowTop*) (- hsize *WindowRight*) (- vsize *WindowBottom*))
        (call-next-method))
      )))
;;;----------------------------------------------------------------------------------------------------------

(new uibiglabel :objectname "ParentInfo" :project ui)
(setf (container ParentInfo) InheritanceViewer)
(hide ParentInfo)

(new uibiglabel :objectname "ChildrenInfo" :project ui)
(setf (container ChildrenInfo) InheritanceViewer)
(hide ChildrenInfo)
;;;----------------------------------------------------------------------------------------------------------

#|
(new tbSketchRenderer :objectname "ParentConnections" :project ui)
(setf (backgroundrenderer ParentConnections) transparent)
(setf (pensize ParentConnections) '(2 2))
(setf (foregroundrenderer ParentConnections) black)

(define-handler setup (ParentConnections)
  (let* ((topper (round (- (bottom ivparents) 3)))
         (num (min 4 (columns ivparents))) ;;; 4 should be computable...
         (incer (car (columnwidths ivparents)))
         startpoint
         newlist)
    (if incer (incf incer (columnspacing ivparents)))
    (setf startpoint (and incer (round (+ (round incer 2) (left ivparents)))))
    (setf (startpoint me) (list (round (h ivnamefield)) (ceiling (- (top ivnamefield) 1))))
    (dotimes (i num)
      (push (list startpoint topper) newlist)
      (incf startpoint incer))
    (setf (linepoints me) newlist)
    ))

(new tbSketchRenderer :objectname "ChildrenConnections" :project ui)
(setf (backgroundrenderer ChildrenConnections) transparent)
(setf (pensize ChildrenConnections) '(2 2))
(setf (foregroundrenderer ChildrenConnections) black)
(define-handler setup (ChildrenConnections)
  (let* ((topper (round (+ (top ivchildren) 3)))
         (num (min 4 (columns ivchildren))) ;;; 4 should be computable...
         (incer (car (columnwidths ivchildren)))
         startpoint
         newlist)
    (if incer (incf incer (columnspacing ivchildren)))
    (setf startpoint (and incer (round (+ (round incer 2) (left ivchildren)))))
    (setf (startpoint me) (list (round (h ivnamefield)) (truncate (- (bottom ivnamefield) 3))))
    (dotimes (i num)
      (push (list startpoint topper) newlist)
      (incf startpoint incer))
    (setf (linepoints me) newlist)
    ))

(new multirenderer :objectname "IVColor" :project ui)
(setf (rendererlist IVcolor) (list uimiddle ParentConnections ChildrenConnections))
(setf (fillcolor InheritanceViewer) IVColor)
|#

;;;;;;;;;;;;-----------------------------------------------------------------------------------------------------------------
(setboundsrect InheritanceViewer 100 26 400 390)
;;;(setf (container InheritanceViewer) stage)
(setf (size inheritanceviewer) '(269 287))
;;;(setf (SelectedItems InheritanceViewer) reliefbutton)

(setf (SelectedItems InheritanceViewer) nil)

(define-handler enteringStage (inheritanceViewer)
  (setf (keytarget me) (textfield IVNameField))
  (setf (selecteditems me) (oldselection me))
  (setf (oldselection me) nil))

(define-handler leavingStage (InheritanceViewer)
  (if (every #'objectname (selecteditems me))
    (setf (oldselection me) (selecteditems me))
    (setf (oldselection me) nil))
  (call-next-method))

#|
	Change History (most recent last):
	1	2/15/94	rod	
	2	2/16/94	rod	
	3	2/18/94	rod	
	4	2/21/94	rod	
	5	2/21/94	hernan	The Final Renaming for Alpha! (yeah, right...)
	6	2/23/94	rod	
	7	2/23/94	rod	
	8	2/23/94	rod	
	9	2/25/94	rod	
	10	2/25/94	hernan	Using symbols instead of keywords for user
				visible options.
	11	2/26/94	rod	hilighter->highlighter
	12	2/26/94	kleiman	
	13	3/3/94	Hernan	The great handler argument name renaming of 94!
	14	3/4/94	kleiman	addproperty avoided wherever possible
	15	3/4/94	Brian	
	16	3/8/94	rod	
	17	3/9/94	rod	Doing Project Switching and Reference Clearing.
	18	3/10/94	rod	
	19	3/18/94	rod	
	20	3/21/94	kleiman	setvalue -> sk8::setValue
	21	3/29/94	rod	
	22	3/30/94	rod	WatchCursor stuff.
	23	3/31/94	rod	
	24	3/31/94	Hernan	Using withLockedCursor instead of withCursor.
	25	4/11/94	rod	Removing reference to simple window.  No longer
				needed in the redesign of the UI.
	26	4/11/94	Brian	
	27	4/13/94	rod	More checks on addparent stuff.
	28	4/20/94	rod	
	29	4/22/94	Hernan	Made resized do its thing with the actor locked.
~~	30	4/26/94	rod	Redoing drag stuff
	31	4/27/94	rod	Redoing this puppy for a2
	32	4/27/94	Brian	
	33	4/27/94	rod	
	34	4/28/94	rod	
	35	4/28/94	rod	
	36	4/28/94	rod	
	39	6/3/94	rod	Here begins the experimentation with a Mac look 
				to the UI.
				386/1/94rodNew layout
	40	6/12/94	rod	
	41	6/12/94	rod	Making the children menu behave as a ui menu.
	42	6/12/94	rod	Making the children menu behave as a ui menu.
	43	6/12/94	rod	Making Parent's list have the proper
				prototype.
	44	6/12/94	rod	Making sure the children don't include any clears.
	45	6/14/94	rod	Marking new parents which have been spliced
				in as prototypes.
	46	6/27/94	rod	Making sure keytarget gets set on stage entry.
~~	47	7/6/94	rod	
	48	7/8/94	rod	
	49 	 9/ 7/94	rod     	
	50 	 9/13/94	rod     	Fixing enabled of New Child menuitem.
	51 	10/13/94	rod     	
	52 	 2/ 3/95	Hernan  	WithLockedCursor is now correct. I check this file
							out and back in to force it to be recompiled.
	53 	 4/25/95	rod     	Fixing YesOrNoDialog and MessageToUser calls to 
							not set the heights so it can be computed 
							manually.
	2  	12/ 8/95	Brian   	
	3  	 2/14/96	Brian   	
	3  	 2/27/97	Hernan  	define-system-handler->define-handler.
|# ;(do not edit past this line!!)
