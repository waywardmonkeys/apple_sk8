;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories

; FILE          "Project-Parent-Order.lisp"
; IMPLEMENTS       Sort of project objects in creation order; parents 1st
; AUTHOR              Ken Dickey
; DATE                  1994 July 26
; LAST UPDATE     1994 July 27 -- changed "queue" from a struct to a class
; COPYRIGHT (c)   1994 Apple Computer, Inc. ; All rights reserved
; NOTES                This seems like a lot of work.  Check against Sidney's solution.
; STATUS              compiles; tested; incomplete.

;; ALGORITHM
;
; The requirements of the algorithm are that objects are ordered by creation.
; I.e. parents come before children.  This is bascially a topological sort
; with the relation (a < b) being (a is-an-ancestor-of b).
;
; The outputs of this algorithm are
;  [1] An ordered list of objects
;  [2] A table of object->creation-order-number
;
; The initial strategy is to go for a simple, O(n) algorithm that conses up
; the wazoo.
;
; The basic strategy is to start with sk8:object--which has no parents--order
; its children by level and position, then do so with the children, recusivley.
; Where there are multiple parents, the deepest parent is used to determine the 
; level of the child.  {This is approximately a breadth 1st walk of the inheritance
; tree}.
;
; In order to use the above approach, we have to know all children of each parent.
; As this information is not explicit, we have to reconstruct it.  This is done by
; walking the objects in a project and adding the parents of each to a table.  The
; table maps object->record; record: object X list(parents) X list(children). 
; As each object is added to the table, its parents are checked.  If the parents are
; not in the table, they are added as well.  This table is called the inheritance-table.
;
; Once the inheritance-table has been constructed, it is time to determine the 
; (level X position) of each object therein.  Starting from sk8:object (level 1),
; each child which does not have other parents than SK8:object are added to the level.
; If there are parents other than SK8:object, they are removed from the parent's list 
; of the child {thus the deepest parent determines the level of the child}.  So we
; have level: list(records) and the total ordering is list(level).  At this point we
; no longer need the inheritance-table.  The list(level) is walked and each object
; which is a member of the given project is added to a new table called the 
; creation-order-table.  At this time, each object is also numbered with a creation
; number.  At the end of this process, we have a list( object X creation-number ) and
; a creation-order-table of (object -> object X creation-number).
;
; Note that the inheritance-table and level-list have information for objects which 
; are not in the indicated project.  The creation-order-table and creation-list
; contain only objects in the indicated project {and subprojects}.
;
; The code below short-cuts the above description in several places but you get 
; the idea.

;; {@@@ STILL TO DO 
;;     Handle nested projects
;;     White out "internal" objects {temp & project files, ProjectBuilderPreferences, ...}
;; }@@@



; CL needs modules... 8^(

(in-package :SK8Dev) 



; internal record def -- should be scoped inside of make-creation-table-and-list
(defstruct inheritance-info 
  parents    ; list of obj
  object-itself     ; obj
  children)  ; list of obj  @@{could use list of inheritance-info here}.


(defvar *output-non-project-info*  nil) ;; for debug

;; default filter method for what we don't want to write out
(define-handler dontSaveScript (object)
  (dontSave me))

(define-handler dontSaveScript (sk8::file)
  (let ((proj (sk8::project me)))
    (or (and proj (or (eq (sk8::file proj) me)
                      (eq (sk8::swapfile proj) me)))
        (call-next-method))))

(define-handler dontSaveScript (sk8::media)
  (or
   ;; (null (resourceid me)) Why? This stops saving of file based media. 
   (and (eq :project (getvalue 'file me))
        (null (find me (knowndescendants translator)
                    :test #'(lambda(a b)
                              (is-a a (finalobject b))))))
   (call-next-method)))

;; default filter method for properties we don't want to write out or want to write out different
;;; *** IMPORTANT: The base handler for saveToScript takes care of propagatable properties. It is important that the handlers
;;; of other objects use call-next-method to get a value out of the slot if this is going to work.

(define-handler saveToScript (object propname)
  (let ((value (SaveToStore me propname)))
    (if (mf::dynamic-slotvalue-inheriter-p value)
      (mf::%slotvalue-inheriter-value value)
      value)))

;;; Special properties are saved but the value saved is what you get when you call
;;; saveToScript on the property. Filter of values... It has nothing to do with
;;; preventing a property from saving. 

#|

;;; WHY? WHY? WHY? WHY? WHY? WHY? WHY? WHY? WHY? WHY? WHY? WHY? 
;;; WHY? WHY? WHY? WHY? WHY? WHY? WHY? WHY? WHY? WHY? WHY? WHY? 
;;; WHY? WHY? WHY? WHY? WHY? WHY? WHY? WHY? WHY? WHY? WHY? WHY? 

(define-handler localpropertiestosavespecially (Media)
  (if (eq me Media)
    '(resourceId sk8::File)
    '()))

|#

;;; SAVETOSCRIPT is not a predicate!!! It should return the value that you want saved. 

(define-handler saveToScript (Media propname)
  (if (and (or (eq propname 'sk8::file) (eq propname 'sk8::resourceId))
           (or (null (resourceid me))
               (and (eq :project (getvalue 'file me))
                    (find me (knowndescendants translator)
                          :test #'(lambda(a b)
                                    (is-a a (finalobject b)))))))
    (mf::find-parent-slot-value me propname nil)  ;;*** this is a kludge that supresses outputting a setter command for this property
    (call-next-method)))

#|

(define-handler propertiesToSaveSpecially (Media)
  (let ((result (call-next-method)))
    (if (eq me Media)
      result
      (if (keywordp (getValue 'sk8::file me))
        ;; media in external file: save the properties to make media point to
        ;; the file and resourceId. 
        result
        ;; resource in project: do not save these props.
        (remove 'sk8::file (remove 'resourceId result))
        ))))

|#

(define-handler propertiesToSaveAsFalse (Media)
  (unless (eq me Media)
    (let* ((file? (keywordp (getValue 'sk8::File me)))
           (resId? (or file? (null (resourceId me)))))
      (cond ((and file? resId?) '(resourceId SK8::File mediaData))
            (file? '(SK8::File mediaData))
            (resId? '(resourceId mediaData))
            (t '(mediaData))))))

(define-handler saveToScript (Actor propname)
  (let ((val (call-next-method)))
    (if (and (eq propname 'sk8::flags)
             (consp val)
             (not (consp (cdr val))))
      (list (car val) (cdr val))
      val)))

; the big kahuna
(defmethod make-creation-table-and-list ( (sk8-project SK8::project) SK8-object-info-maker )
  (let ((inheritance-table    (make-hash-table :test #'eq)) ; obj -> inheritance-info
        (inheritance-list     (make-instance 'queue)) ; list of inheritance-info
        (creation-order-table (make-hash-table :test #'eq))
        (creation-order-list  (make-instance 'queue)) ; list of SK8-object-info
        (creation-number      1 )
        (target-projects      (list sk8-project))
        )
    (labels
      ((add-inheritance-info-for-object (obj)
         (unless (and (target-project? (project obj)) (dontSaveScript obj))
           (let ((probe (gethash obj inheritance-table NIL)))
             (unless probe
               (let ((pars (parents obj)))
                 (unless (or pars (eq obj Object))
                   (error "Internal SK8 error: no parents for object ~s" obj))
                 ;;(when debug (format t "~%adding inheritance info for ~s" obj))
                 (let ((obj-rec (make-inheritance-info :parents pars :object-itself obj)))
                   (setf (gethash obj inheritance-table) obj-rec)
                   (dolist (parent pars)
                     (unless (and (target-project? (project parent)) (dontSaveScript parent))
                       (add-inheritance-info-for-object parent)
                       (add-child-to-parent-rec obj (gethash parent inheritance-table)))
                     )
                   )))
             )))
       (add-child-to-parent-rec (obj parent-rec)
         ;;(when debug (format t "~%add-child-to-parent-rec child: ~s parent: ~s" 
         ;;                       obj (inheritance-info-object-itself parent-rec)))
         (unless (memq obj (inheritance-info-children parent-rec))
           (push obj (inheritance-info-children parent-rec)))
         )
       (add-to-creation-order (obj)
         ;;(when debug (format t "~%add-to-creation-order ~s" obj))
         (let ( (cr (funcall SK8-object-info-maker obj creation-number)) ) 
           (incf creation-number)
           (setf (gethash obj creation-order-table) cr)
           (push-last cr creation-order-list)
           ) )
       (build-cr-list ()
         (unless (empty? inheritance-list)
           (process-object-inheritance (pop-first inheritance-list))
           (build-cr-list) ; Boy, I hope MCL does optimized tail recursion!
           ) )
       (process-object-inheritance (obj-rec)
         (when obj-rec
           (let ( (parent (inheritance-info-object-itself obj-rec)) )
             ;;(when debug (format t "~%process-object-inheritance for ~s" parent))
             (dolist (kid (inheritance-info-children obj-rec))
               (let ( (kid-rec (gethash kid inheritance-table)) )
                 ; only push kid for last parent
                 (if (only-parent? parent kid-rec)
                   (push-last kid-rec inheritance-list)
                   (remove-parent parent kid-rec)))
               )
             (if (target-project? (project parent))
               (add-to-creation-order parent)
               (when *output-non-project-info*
                 (format t "~%Non-project Object: ~s" parent))
               )
             )))
       (only-parent? (parent obj-rec)
         ;;(when debug 
         ;;   (format t "~%only-parent? ~s ~s" parent (inheritance-info-parents obj-rec)))
         ; parents must be list of 1 parent to match
         (let* ( (parents (inheritance-info-parents obj-rec))
                 (result (and (eq parent (car parents)) 
                              (null (cdr parents)))) 
                 )
           ;;(when debug (format t " yields: ~s" result))
           result)
         )
       (remove-parent (parent obj-rec)
         ;;(when debug 
         ;;   (format t "~%remove-parent ~s ~s" parent (inheritance-info-parents obj-rec)))
         (setf (inheritance-info-parents obj-rec) 
               (remove parent (inheritance-info-parents obj-rec))
               ) )
       (target-project? (proj)
         ;;(when debug 
         ;;   (format t "~%~s yields: ~s" proj (member proj target-projects :test #'eq)))
         (member proj target-projects :test #'eq) ;; handle nested projects
         )
       )
      
      (gc)  ; Project object table is weak on values
      ; We cons a lot, so can't wrap a without-gc here
         ; @@ Should wrap a without-interrupts or without-dispatch or somethin'
      
      ; walk project objects to build the inheritance table
      (mapProjectObjects sk8-project #'add-inheritance-info-for-object)
      
      ; seed the inheritance list (really a queue)
      (push-last (gethash sk8:object inheritance-table) inheritance-list)
      ; build the requisit results
      (build-cr-list)
      
      ; return the results
      (values creation-order-table creation-order-list)
      ) ) )



;;                             --- E O F ---

#|
	Change History (most recent last):
	1  	 9/26/94	kend    	Sorts project objects in creation order; parents 1st.
	2  	 1/12/95	sidney  	changes to save as text to be more like binary store
	3  	 1/21/95	sidney  	deal with special cases
	4  	 1/24/95	sidney  	more tweaking to suport save as text of strange objects
	5  	 1/26/95	sidney  	tweaked again: deal with sk8-fred, sk8-menu, etc. clos objects
	6  	 2/ 2/95	sidney  	don't crash when project has no objects
	7  	 3/ 8/95	dy      	add localPropertiesToSaveSpecially of Actor
	8  	 3/ 8/95	sidney  	don't write out named media objects whose resource is in the project file
	9  	 3/ 8/95	sidney  	add the rest of the sk8 objects that have media that may already be in the project
	10 	 3/ 9/95	sidney  	localPropertiesToSaveSpecially was already defined incompatibly elsewhere
	11 	 3/13/95	sidney  	some media objects should be written out after all. actually this should be done right soon.
	12 	 3/14/95	sidney  	changing the way we deal with media objects in the project: copy the resources to the generated script file and import them from there
	13 	 3/21/95	sidney  	pixmaps are now like other media, so we can remove special case code for them
	14 	 4/19/95	sidney  	was setting file of imported media to nil
	2  	 4/19/96	Brian   	Removing typetable
	3  	 5/ 7/96	sidney  	Changes for new object system
	4  	 8/ 1/96	Hernan  	Fixed propertiesToSaveSpecially of Media to actually do
						not save specially when the media is file based.
	5  	 8/ 9/96	Hernan  	Fixing the way media saves its properties. It the media is
						file based, the properties have to be saved!!!
	6  	 2/27/97	Hernan  	
|# ;(do not edit past this line!!)
