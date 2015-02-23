;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories


;;;



(in-package :SK8Dev)

(SK8-declare-syms :SK8 :public ; Updated 10-27-94  11:33 am
                  SK8::FIRSTLEVELDEPENDENCIES SK8::allDependencies)



;; given an object, return a collection of its 1st level dependencies
;; which are not in the SK8 project
(define-handler FirstLevelDependencies (SK8::Object)
  (let ( (my-parents (parents me))
         (sk8-codegenerator (make-instance 'SK8Script-CodeGenerator))
         (results-set (make-hash-table :test #'eq))  ;; (make-set) 
         (order-num 0)
       )
    (labels ;; syntactic sugar tastes sweeter
      ( (add-to-set (obj a-set)
          (let ( (proj (SK8::project obj)) )
            (when (and proj (not (eq proj SK8::SK8)))
              ;; just use obj as key for fast membership test
              (setf (gethash obj a-set nil) T))  
        ) )
        (set->list (a-set)
          (let ( (result '()) )
            (maphash #'(lambda (key val)
                         (declare (ignore val))
                         (push key result))
                     a-set)
            result)
        )
      )

      (dolist (a-parent my-parents) (add-to-set a-parent results-set))

      (dolist (prop-name (filtered-inherited-properties me 
                                                        (make-instance 
                                                          'CodeGenerator-SK8-object-info-class 
                                                          :sk8-object me
                                                          :creation-order-number (incf order-num))
                                                        sk8-codegenerator))
        (add-to-set (getPropertyValue me prop-name) results-set)
      )

      (dolist (prop-name (filtered-local-properties me 
                                                    (make-instance 
                                                          'CodeGenerator-SK8-object-info-class 
                                                          :sk8-object me
                                                          :creation-order-number (incf order-num))
                                                    sk8-codegenerator))
        (add-to-set (getPropertyValue me prop-name) results-set)
      )

      (set->list results-set)
) ) )


;; given an object, return the transitive closure of all its dependencies
;; which are not in the SK8 project

(defun allDependencies (objs)
  (let ( (dependents-queue (make-instance 'queue)) 
         (results-set (make-hash-table :test #'eq))  ;; (make-set)
       )
    (labels ;; syntactic sugar tastes sweeter
      ( (add-to-set (obj a-set)
          ;; just use obj as key for fast membership test
          (setf (gethash obj a-set nil) T) 
        )
        (set-member? (obj a-set)
          (gethash obj a-set nil))
        (set->list (a-set)
          (let ( (result '()) )
            (maphash #'(lambda (key val)
                         (declare (ignore val))
                         (push key result))
                     a-set)
            result)
        )
        (process (obj) ; da work gets done here...
          (add-to-set obj results-set)
          (dolist (an-obj (FirstLevelDependencies obj))
            (let ( (proj (SK8::project obj)) )
              (when (and proj (not (eq proj SK8::SK8)) (not (set-member? an-obj results-set)))
                (push-last an-obj dependents-queue)))
        ) )
      )

      (dolist (obj objs)
        (push-last obj dependents-queue))

      (ccl::while (not (empty? dependents-queue))
        (process (pop-first dependents-queue)))

      (set->list results-set) 
) ) )



(in-package :uidevelopment)


(new UISimpleWindow :objectname "SaveAsEditor" :project ui)
(addproperty SaveAsEditor 'showingDependencies)
(setf (sk8::menubar SaveAsEditor) nil)
(setf (resizer SaveAsEditor) t)
(setf (zoombox SaveAsEditor) t)

(define-handler clearReferences (SaveAsEditor &key ((:objects theobjects)))
  (if theobjects
    ()
    (progn
      (setf (showingDependencies SaveAsEditor) nil)
      (setf (inputobjects (picker SaveAsPile)) nil)
      (setf (inputobjects (picker SaveAsDepedencies)) nil))))

(define-handler SetUpForProject (SaveAsEditor &key ((:project theproject)))
  (enteringstage me))

(define-handler (setf showingDependencies) (theval SaveAsEditor)
  (sk8dev::withLockedCursor animatedClock
    (setvalue 'showingDependencies me theval)
    (tickeventclock)
    (setf (title SaveAsDepedencies) (if theval "All Other Dependent Objects:" "Objects Depending on Selection:"))
    ;(setf (enabled SaveAsAuditButton) (not theval))
    (tickeventclock)
    (if theval
      (let ((objs (remove-if #'(lambda (x) (or (symbolp x) (is-a x handler) (is-a x sk8::function))) (items (picker saveaspile)))))
        (setf (inputobjects (picker SaveAsDepedencies)) 
              (remove-if #'(lambda (x) (memq x objs)) (sk8dev::alldependencies objs))))
      (setup (picker SaveAsDepedencies)))
    (tickeventclock)
    ))

;;;;__________________________________________________________

(new UIButton :objectname "SaveAsButton" :Project ui)
(define-handler enabled (SaveAsButton)
  (and (neq (targetproject ui) sk8)
       (neq (targetproject ui) ui)
       (items (picker SaveAsPile))))
(setf (container SaveAsButton) SaveAsEditor)
(setf (text SaveAsButton) "Save")
(define-handler click (SaveAsButton)
  (when (and (enabled me)
             (or (not (items (picker SaveAsDepedencies)))
                 (yesornodialog "The objects being saved have dependecies.  These dependencies will be cleared.   Continue?" :cancel nil)))
    #|
    (let ((fileobj (newfiledialog :title "Save Text in File:" :project (targetproject ui))))
      (when fileobj
        (sk8dev::withLockedCursor animatedClock
          ;;FILL ME IN
          )))
    |#
    (messagetouser "coming soon!" :beep t)
    ))

#|
(new UIButton :objectname "SaveAsAuditButton" :Project ui)
(setf (container SaveAsAuditButton) SaveAsEditor)
(setf (text SaveAsAuditButton) "Show all Dependencies")
(define-handler click (SaveAsAuditButton)
  (when (enabled me)
    (setf (showingDependencies SaveAsEditor) t)))
(define-handler enabled (SaveAsAuditButton)
  (items (picker SaveAsPile)))
|#
;;;;__________________________________________________________

(new uitextlist :objectname "SaveAsPile" :project ui)
(setf (pickerprototype SaveAsPile) ObjectPilePicker)
(setf (container SaveAsPile) SaveAsEditor)
(define-handler editing ((picker SaveAsPile))
  (neq (targetproject ui) sk8))
(setf (text (titlebar SaveAsPile)) "Saved Items (Drop in Here):")
(tagpart SaveAsEditor SaveAsPile 'ObjectPile)

(define-handler createTextDisplayItem ((picker SaveAsPile) theval)
  (if (symbolp theval)
    (concatenate 'string 
                 (if (constantp theval)
                   "The constant  "
                   "The global  "
                   )
                 (objectstring theval :project (targetproject me))
                 )
    (objectstring theval :project (targetproject me)))
  )

(define-handler doubleclick ((picker SaveAsPile))
  (let ((si (car (selecteditems me))))
    (when si (uiedit si))))

(define-handler draggingMouseEnter ((picker SaveAsPile) actorDragged)
  (when (and (editing me) (neq actordragged handlerdatarect))
    (if (or (eq actordragged globaldatarect) 
            (eq actordragged constantdatarect))
      (setf (highlight me) t)
      (call-next-method))))

(define-handler draggingMouseWithin ((picker SaveAsPile) actorDragged)
  (when (and (editing me) (neq actordragged handlerdatarect))
    (if (or (eq actordragged globaldatarect) 
            (eq actordragged constantdatarect))
      (flashline me)
      (call-next-method))))

(define-handler dropped ((picker SaveAsPile) droppee)
  (when (and (editing me) (neq droppee handlerdatarect))
    (withActorLocked (me)
      (call-next-method)
      (cond
       ((or (eq droppee globaldatarect) 
            (eq droppee constantdatarect))
        (let ((theguy (getlineposition me))
              (obj (cond
                    ((eq droppee globaldatarect) (globals globaldatarect))
                    ((eq droppee constantdatarect) (constants constantdatarect))
                    ))
              (its (copy-list (items me)))
              NewLayer AlreadyHere)
          (when (is-a me hierarchicalpicker) (setf its (mapcar #'value its)))
          (when theguy
            (bringToFront (sk8::window me))
            (setf (keytarget (sk8::window me)) me)
            (unless (listp obj) (setf obj (list obj)))
            (setf NewLayer (if (inheritsfrom me tablepicker) (cadr theguy) theguy))
            (dolist (i obj)
              (setf AlreadyHere (position i its :test #'eq))
              (when AlreadyHere
                (setf its (delete i its :test #'eq))
                (if (< AlreadyHere newlayer)
                  (decf newlayer)))
              (setf its (sk8dev::moveToPosition i (cons i its) newlayer)))
            (setf (inputobjects me) its)
            (setf (selecteditems me) obj)
            (selectioncompleted me)
            ))))
      )))

(define-handler (setf items) (theval (picker SaveAsPile))
  (call-next-method)
  (setf (enabled saveasbutton) theval)
  (setf (showingDependencies SaveAsEditor) t))

(define-handler keydown ((picker SaveAsPile) thechar)
  (if (eq thechar #\delete)
    (let ((sels (selecteditems me)))
      (setf (items me) (remove-if #'(lambda (x) (memq x sels)) (items me)))
      )
    (if (eq thechar #\tab)
      (setf (keytarget SaveAsEditor) (picker SaveAsDepedencies))
      (call-next-method))))
(define-handler selectionCompleted ((picker SaveAsPile))
  (call-next-method)
  (when (and (items me) (not (showingdependencies saveaseditor)))
    (setup (picker SaveAsDepedencies)))
  )

(new uitextlistforcorners :objectname "SaveAsDepedencies" :project ui)
(setf (container SaveAsDepedencies) SaveAsEditor)
(setf (alphabeticaldisplay SaveAsDepedencies) t)
(setf (text (titlebar SaveAsDepedencies)) "Objects also Saved:")
(define-handler keydown ((picker SaveAsDepedencies) thechar)
  (unless (eq thechar #\delete)
    (if (eq thechar #\tab)
      (setf (keytarget SaveAsEditor) (picker SaveAsPile))
      (call-next-method))))
(define-handler doubleclick ((picker SaveAsDepedencies))
  (let ((si (car (selecteditems me))))
    (when si (uiedit si))))
(define-handler setup ((picker SaveAsDepedencies))
  (let* ((sels (selecteditems (picker SaveAsPile))))
    (setf (inputobjects me) (and sels (firstleveldependencies (car sels))))
    ))

;;;;__________________________________________________________

(define-handler enteringstage (SaveAsEditor)
  (withactorlocked (me)
    (setf (text me) (concatenate 'string (objectstring (targetproject ui) :project (targetproject ui)) " Object Saver"))
    (setf (enabled saveasbutton) nil)))

(define-handler resized (SaveAsEditor)
  (let (hsize vsize)
    (declare (special hsize vsize))
    (withActorLocked (me)
      (call-next-method)
      (sk8-multival-setf (hsize vsize) (size me))
      ;(setboundsrect SaveAsAuditButton (- hsize *WindowRight* 125) (+ 18 *WindowTop*) (- hsize *WindowRight* 10) (+ 39 *WindowTop*))
      (setboundsrect SaveAsButton (+ *windowLeft* 10) (+ 18 *WindowTop*) (+ *windowLeft* 64) (+ 39 *WindowTop*))
      (setboundsrect SaveAsPile *WindowLeft* (+ 45 *WindowTop*)
                     (- (round hsize 2) 5) (- vsize *WindowBottom*))
      (setboundsrect SaveAsDepedencies (+ (round hsize 2) 5) (+ 45 *WindowTop*)
                     (- hsize *WindowRight*) (- vsize *WindowBottom*))
      )))

(setboundsrect SaveAsEditor 110 24 563 301)
(setf (minimumsize SaveAsEditor) '(325 225))

(resized SaveAsEditor)
;;;(setf (container SaveAsEditor) stage)
;;;(setf (container SaveAsEditor) nil)
;;;;   (size SaveAsEditor)




#|
	Change History (most recent last):
	1  	10/19/94	rod     	New File for Ken's Save As Text functionality.
							This is a first cut at a window to replace the 
							library editor.
	2  	10/19/94	rod     	All the basic functionality is here.  Need to fill in
							the areas marked with *** to get the rest to work.
	3  	10/27/94	rod     	
	4  	10/27/94	rod     	
	5  	10/27/94	rod     	
	6  	10/27/94	rod     	redoing layout.
	7  	11/ 3/94	rod     	
	8  	 2/ 3/95	Hernan  	WithLockedCursor is now correct. I check this file
							out and back in to force it to be recompiled.
	3  	 2/27/97	Hernan  	define-sk8-function->defun.
|# ;(do not edit past this line!!)
