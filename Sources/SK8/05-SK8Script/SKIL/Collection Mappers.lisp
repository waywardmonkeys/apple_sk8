;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories

(in-package :sk8dev)

(SK8-declare-syms :SK8 :public ; Updated  7-26-96  11:00 am
                  SK8::ANYINDEX SK8::COLLECTIONSUBSEQUENCE SK8::DOFULLRANGEMAPPER SK8::DOSINGLEITEMMAPPER
                  SK8::DOSPLICECOLLECTIONS SK8::FULLRANGEMAPPER SK8::INSERTATEND SK8::INSERTINFRONT
                  SK8::ITEMRANGEWITHFILTERS SK8::ITEMRANGEWITHFILTERSANDBEFORE SK8::MIDDLEINDEX
                  SK8::NTHITEMWITHFILTERS SK8::NTHITEMWITHFILTERSANDSTART SK8::SPLICECOLLECTIONS)


;-----------------------------------------------------------------------------
;-----------------------------------------------------------------------------
;-----------------------------------------------------------------------------
;  This file contains add on functions that are used in the generated code
;  They can be written in SK8Script so they are automatically portable.
;  Of course, since this is for code generation of path expressions, the
;     sk8script for these functions cant use and path expressions.
;-----------------------------------------------------------------------------
;-----------------------------------------------------------------------------
;-----------------------------------------------------------------------------
(define-handler collectionLength (collection typeFilter whereFilter)
  (let ((cnt 0) 
        currentItem
        (currentState (initialVisitState me)))
    (when currentState
      (loop 
        (setf currentItem (elementAtVisitState me currentState))
        (when (and (or (not typeFilter) (is-a currentItem typeFilter)) 
                   (or (not whereFilter) (funcall whereFilter currentItem)))
          (incf cnt))
        (if (isFinalVisitState me currentState) (return))
        (setf currentState (succeedingvisitstate me currentState))))
    cnt))

(define-handler collectionNthState (collection n)
  (let ((cnt 0) 
        res
        (currentState (initialVisitState me)))
    (when currentState
      (loop 
        (incf cnt)
        (when (>= cnt n)
          (setf res currentState)
          (return))
        (when (isFinalVisitState me currentState)
          (setf res nil)
          (return))
        (setf currentState (succeedingvisitstate me currentState))))
    res))

(define-handler collectionNth (collection n)
  (let ((vs (collectionNthState me n)))
    (if vs
      (elementatvisitstate me vs)
      nil)))

(define-handler collectionNth (list n)
  (if (>= n 1)
    (nth (1- n) me)
    nil))
(define-handler collectionNth (vector n)
  (if (and (>= n 1) (<= n (length me)))
    (aref me (1- n))
    nil)
  )

;;-------------------------------------------------------------------------------------------------------------------------

(define-handler collectionSubsequence (collection start end) 
  (if (<= start end)
    (let ((newcollection (if (listp me) nil (sk8::new (baseparent me) :project (or (project me) sk8))))
          (cnt start)
          (currentState (collectionNthState me start)))
      (when currentState
        (loop 
          (setf newcollection (insertAtEnd (elementAtVisitState me currentState) newcollection))
          (when (or (isFinalVisitState me currentState)
                    (>= cnt end))
            (return))
          (incf cnt)
          (setf currentState (succeedingvisitstate me currentState))))
      newcollection)
    Undefined))
(define-handler collectionSubsequence (vector start end) 
  (subseq me (1- start) end))
(define-handler collectionSubsequence (table start end) 
  (error "Can't get a subsequence of a Table because Tables are unordered"))
(define-handler collectionSubsequence (AliasedCollection start end) 
  (collectionSubsequence (getpropertyvalue me (collectionProperty me)) start end))
;;-------------------------------------------------------------------------------------------------------------------------
(defun doSpliceCollections (me otherCollection startLoc) 
  (let ((insertState (collectionNthState me startLoc))
        (after? t)
        (currentState (initialVisitState otherCollection)))
    (when (< startLoc 1) 
      (setf startLoc 1)
      (setf after? nil))
    (when currentState
      (loop 
        (insertAtVisitState me insertState (elementAtVisitState otherCollection currentState) :after after?)
        (when (isFinalVisitState otherCollection currentState)
          (return))
        (setf insertState (succeedingvisitstate me insertState))
        (setf currentState (succeedingvisitstate otherCollection currentState))))
    me))

(define-handler spliceCollections (collection otherCollection startLoc) 
  (dospliceCollections me otherCollection startLoc))
(define-handler spliceCollections (List otherCollection startLoc) 
  (if (listp otherCollection)
    (progn
      (setf (nthcdr startLoc me) (nconc otherCollection (nthcdr startLoc me)))
      me)
    (dospliceCollections me otherCollection startLoc)))

;;-------------------------------------------------------------------------------------------------------------------------

(defun sk8::length (obj)
  (if (and (not (listp obj)) (not (vectorp obj)) (is-a obj collection))
    (collectionLength obj nil nil)
    (length obj)))

;;-------------------------------------------------------------------------------------------------------------------------


(defun InsertInFront (it col)
  (if (is-a col aliasedcollection) (setf col (getpropertyvalue col (collectionProperty col))))
  (if (is-a col text) 
    (progn
      (setf col (characters col))
      (basetext (insertAtVisitState col (initialvisitstate col) it  :after nil)))
    (insertAtVisitState col (initialvisitstate col) it :after nil)))

(defun insertAtEnd (it col)
  (if (is-a col aliasedcollection) (setf col (getpropertyvalue col (collectionProperty col))))
  (let ((isText? (is-a col text))
        (isString? (stringp col))
        currentState res)
    (when isText? 
      (setf CurrentState (cons 0 (length col)))  ;;we can speed this along for strings
      (setf col (characters col))
      )
    (unless isString?
      (setf currentState (initialVisitState col))
      (if currentState 
        (loop
          (if (isFinalVisitState col currentState) (return))
          (setf currentState (succeedingvisitstate col currentState)))))
    (if currentState
      (setf res (insertAtVisitState col currentState it :after t))
      (setf res (list it)))   ;;should be (collect list it)
    (if isText? (setf res (baseText res)))
    res
    ))

;;-------------------------------------------------------------------------------------------------------------------------

(define-handler middleIndex (collection typeFilter whereFilter)
  (ceiling (collectionlength me typeFilter whereFilter) 2))
(define-handler middleIndex (table typeFilter whereFilter)
  (declare (ignore typeFilter whereFilter))
  (error "Cannot get the MIDDLE item of a table."))

(define-handler AnyIndex (collection typeFilter whereFilter)
  (let ((len (collectionlength me typeFilter whereFilter)))
    (if (> len 0)
      (1+ (random len))
      nil)))
(define-handler AnyIndex (table typeFilter whereFilter)
  (let* ((keys (keys me)) len)
    (setf keys (remove-if-not #'(lambda (x) (let ((currentItem (elementAtVisitState me (visitStateAtKey me x)))) 
                                              (and (or (not typeFilter) (is-a currentItem typeFilter)) 
                                                   (or (not whereFilter) (funcall whereFilter currentItem)))))
                              keys))
    (setf len (length keys))
    (if (> len 0)
      (nth (random len) keys)
      nil)))

;;-------------------------------------------------------------------------------------------------------------------------


(define-handler nthItemWithFilters (collection n typeFilter whereFilter actionAtItem)
  (let (currentItem
        foundItem
        (completecount 0)
        (cnt 0)
        (currentState (initialVisitState me)))
    (when currentState
      (loop
        (incf completecount)
        (if (or typefilter wherefilter)
          (setf currentItem (elementAtVisitState me currentState))
          (setf currentItem 'bogusValue))
        (when (and (or (not typeFilter) (is-a currentItem typeFilter)) 
                   (or (not whereFilter) (funcall whereFilter currentItem)))
          (when (eq currentItem 'bogusValue) (setf currentItem (elementAtVisitState me currentState)))
          (incf cnt)
          (setf foundItem currentItem)
          (if (and n (= cnt n)) (return)))
        (when (isFinalVisitState me currentState)
          (if n (setf foundItem nil))
          (return))
        (setf currentState (succeedingvisitstate me currentState))))
    (if (and foundItem actionAtItem)
      (funcall actionAtItem me foundItem currentState completecount)
      foundItem)
    ))

(define-handler nthItemWithFiltersAndStart (collection n typeFilter whereFilter 
                                                            direction start
                                                            actionAtItem)
  (let (currentItem
        theResult
        foundItem
        foundState
        foundCnt
        (cnt 0)
        (completecount 0)
        (currentState (initialVisitState me)))
    (when currentState
      (loop
        (setf currentItem (elementAtVisitState me currentState))
        (incf completecount)
        (if (and (eq direction 'before) (>= completecount start)) (return))
        (when (and (or (neq direction 'after) (> completecount start))
                   (or (not typeFilter) (is-a currentItem typeFilter)) 
                   (or (not whereFilter) (funcall whereFilter currentItem)))
          (incf cnt)
          (setf foundItem currentItem)
          (setf foundState currentState)
          (setf foundCnt completecount)
          (if (eq direction 'before) 
            (setf theResult (nconc theResult (list (list foundItem foundState foundCnt))))
            (if (and n (= cnt n)) (return))))
        (when (isFinalVisitState me currentState)
          (if n (setf foundItem nil))
          (return))
        (setf currentState (succeedingvisitstate me currentState))))
    (when (eq direction 'before) 
      (let ((index (or (and n (- (length theResult) n))
                       0))
            curItem)
        (if (>= index 0) (setf curitem (nth index theResult)))
        (sk8-multival-setf (foundItem foundState foundCnt) curItem)))
    (if foundCnt
      (if actionAtItem
        (funcall actionAtItem me foundItem foundState foundCnt)
        foundItem)
      nil)
    ))

(defun doSingleItemMapper (me 
                              n 
                              typeFilter
                              whereFilter 
                              actionAtItem
                              actiontype
                              direction
                              startLocation)
  (unless startLocation (setf startLocation 0))
  (let (res 
        (coll me)
        (specialAction? (memq actiontype '(sk8::RemoveInternal sk8::InsertBefore sk8::InsertAfter SET))))
    (cond
     ((or (not (= startLocation 0))
          (eq direction 'before)
          specialAction?)
      (if (and specialAction? (or (listp me) (stringp me))) (setf coll (sk8::copy coll)))
      (setf res 
            (nthItemWithFiltersAndStart coll n typeFilter whereFilter 
                                        direction startLocation actionAtItem)))
     (t
      (setf res
            (nthItemWithFilters coll n typeFilter whereFilter actionAtItem)))
     )
    res ;;(if (and specialAction? (not res)) coll res)
    ))


;;;We do this because shadows of collection like array can't call-next-method.  they have to call the doSingleItemMapper directly.
(define-handler singleItemMapper (collection 
                                     n 
                                     typeFilter
                                     whereFilter 
                                     actionAtItem
                                     actiontype
                                     direction
                                     startLocation)
  (doSingleItemMapper me n typeFilter whereFilter actionAtItem actiontype direction startLocation))



;;______________________________________________________________________________________________________________


(define-handler itemRangeWithFilters (collection beginIndex endIndex typeFilter whereFilter 
                                                     startLocation actionAtItem createList? setting?)
  (let (currentItem
        (coll me)
        (cnt 0)
        (completecount 0)
        (currentState (initialVisitState me))
        theResult
        )
    (when currentState
      (loop
        (setf currentItem (elementAtVisitState coll currentState))
        (incf completecount)
        (when (and (> completecount startLocation)
                   (or (not typeFilter) (is-a currentItem typeFilter)) 
                   (or (not whereFilter) (funcall whereFilter currentItem)))
          (incf cnt)
          (if (>= cnt beginIndex)
            (let ((curres (if actionAtItem
                            (funcall actionAtItem coll currentItem currentState completecount)
                            currentItem)))
              (when (and setting? (is-a coll indirectTextVector)) 
                (setf coll curres)
                (setf currentState (collectionnthstate coll completeCount))
                )
              (if createList?
                (setf theResult (nconc theResult (list curres)))
                (setf coll curres))
              ))
          (if (and endIndex (>= cnt endIndex)) (return))
          )
        (if (or (not currentState) (isFinalVisitState coll currentState)) (return))
        (setf currentState (succeedingvisitstate coll currentState))))
    (if (or (not endIndex) (>= cnt beginIndex))
      (if createList? theResult coll)
      nil)
    ))


(define-handler itemRangeWithFiltersAndBefore (collection beginIndex endIndex typeFilter whereFilter 
                                                               endLocation
                                                               actionAtItem
                                                               createList?
                                                               setting?)
  (let (currentItem
        curres 
        (coll me)
        (cnt 0)
        (completecount 0)
        (currentState (initialVisitState me))
        theResult)
    (when currentState
      (loop
        (setf currentItem (elementAtVisitState me currentState))
        (incf completecount)
        (if (>= completecount endlocation) (return))
        (when (and (or (not typeFilter) (is-a currentItem typeFilter)) 
                   (or (not whereFilter) (funcall whereFilter currentItem)))
          (incf cnt)
          (if actionAtItem
            (setf theResult (nconc theResult (list (list currentItem nil completecount))))  ;;nil is filled in dynamically...
            (setf theResult (nconc theResult (list currentItem))))
          )
        (if (isFinalVisitState me currentState) (return))
        (setf currentState (succeedingvisitstate me currentState)))
      (let (curEnd curStart (len (length theResult)))
        (setf curEnd (- len (1- beginIndex)))
        (setf curStart (- len (or endIndex len)))
        (if (< curStart 0) (setf curstart 0))
        (if (and (>= curStart 0) (<= curStart len)  (>= curEnd 0) (<= curEnd len))
          (setf theresult (subseq theresult curStart curEnd))
          (setf theresult nil))))
    (when actionAtItem
      (setf theResult (mapcar #'(lambda (args) 
                                  (setf (second args) (collectionnthstate coll (third args)))
                                  (setf curres (apply actionAtItem (cons coll args)))
                                  (when (and setting? (is-a coll indirectTextVector))
                                    (setf coll curres)
                                    )
                                  curres) theResult))
      )
    (if createList?
      theResult
      coll)
    )
  )


(defun doFullRangeMapper (me 
                             beginIndex endIndex 
                             typeFilter
                             whereFilter 
                             actionAtItem
                             actiontype
                             direction
                             startLocation)
  (unless startLocation (setf startLocation 0))
  (let (theResult 
        reverseit?
        (specialAction? (memq actionType
                              '(sk8::RemoveInternal sk8::InsertBefore 
                                sk8::InsertAfter set))))
    (cond 
     ((and (not beginIndex) (not endIndex))
      (error "Both indices cannot be last."))
     ((not beginindex)
      (setf reverseIt? t))
     ((and endindex (> beginIndex endIndex))
      (setf reverseIt? t)))
    (if reverseit?
      (let ((tmp endIndex))
        (setf endIndex beginIndex)
        (setf beginIndex tmp)))
    (if (and specialAction? (or (listp me) (stringp me))) (setf me (sk8::copy me)))
    (setf theResult
          (cond
           ((eq direction 'before)
            (itemRangeWithFiltersAndBefore me
                                           beginIndex endIndex typeFilter whereFilter 
                                           startLocation actionAtItem
                                           (not specialAction?)
                                           (eq actiontype 'set)))
           (t
            (itemRangeWithFilters me
                                  beginIndex endIndex typeFilter whereFilter 
                                  startLocation 
                                  actionAtItem 
                                  (not specialAction?)
                                  (eq actiontype 'set)
                                  )) 
           ))
    (if (and reverseit? (not specialAction?))
      (nreverse theResult)
      theResult ;;(if (and specialAction? (not theResult)) me theResult)
      )
    ))

;;;We do this because shadows of collection like array can't call-next-method.  they have to call the doFullRangeMapper directly.
(define-handler fullRangeMapper (collection 
                                    beginIndex endIndex 
                                    typeFilter
                                    whereFilter 
                                    actionAtItem
                                    actiontype
                                    direction
                                    startLocation)
  (doFullRangeMapper me beginIndex endIndex typeFilter
                     whereFilter actionAtItem 
                     actiontype direction startLocation))


;;______________________________________________________________________________________________________________________________

(define-handler singleItemMapper (table n typeFilter whereFilter 
                                           actionAtItem actionType direction startLocation)
  (declare (ignore-if-unused direction startLocation))
  (let (res)
    (cond
     ((or (eq direction 'before)
          (and startLocation (not (= startlocation 0))))
      (error "Before and After cannot be used in Tables"))
     ((and (not (or (not n)
                    (numberp n)))
           typefilter)
      (error "Table accesses must be of the form \"item key\" or \"type integer\""))
     (typefilter
      (setf res (call-next-method))
      (when (and (numberp res) (eq actionType 'sk8::PositionInternal))
        (setf res (nth (1- res) (keys me)))))
     (t
      (when whereFilter
        (error "Accessing a key in a table cannot have a type or a where filter."))
      (cond 
       ((eq actionType 'sk8::PositionInternal)
        (setf res (if (position n (keys me)) n nil)))
       (actionAtItem
        (when (and (eq actionType 'set)
                   (not (visitStateAtKey me n)))
          (setElementAtKey me n nil))
        (setf res (funcall actionAtItem
                           me
                           (elementAtVisitState me (visitStateAtKey me n))
                           (visitStateAtKey me n)
                           0)))
       (t 
        (setf res (elementAtVisitState me (visitStateAtKey me n)))))))
    res))

(define-handler fullRangeMapper (table beginIndex endIndex typeFilter whereFilter 
                                          actionAtItem actionType direction startLocation)
  (declare (ignore-if-unused whereFilter direction actionAtItem startLocation))
  (let (res)
    (cond
     ((or (eq direction 'before)
          (and startLocation (not (= startlocation 0))))
      (error "Before and After cannot be used in Tables"))
     ((and (not (or (not beginIndex)
                    (numberp beginIndex)))
           (not (or (not endIndex)
                    (numberp endIndex)))
           typefilter)
      (error "Table accesses must be of the form \"item key\" or \"type integer\""))
     ((or typefilter
          (and (= beginindex 1)
               (not endindex)))
      (setf res (call-next-method))
      (when (eq actionType 'sk8::PositionInternal)
        (let ((keys (keys me)))
          (setf res (mapcar #'(lambda (x) (nth (1- x) keys)) res)))))
     (t
      (error "Only a single key can be accessed at a time")
      ))
    res))


;;______________________________________________________________________________________________________________________________

(defun mapping-convert-index (ind)
  (cond
   ((or (numberp ind) (listp ind))
    ind)
   ((arrayp ind)
    (astype list ind))
   (t
    (error "unknown indexing?!?"))))

(define-handler singleItemMapper (array 
                                     n 
                                     typeFilter
                                     whereFilter 
                                     actionAtItem 
                                     actionType
                                     direction
                                     startLocation)
  (let ((listIndex? (listp n))
        res)
    (cond
     ((and typeFilter
           listIndex?)
      (error "A type filter cannot be used for a direct array access.  Item should be used instead."))
     (listIndex?
      (setf n (1+ (visitStateAtIndex me n)))))
    ;;;next line is a kludge, it should be (setf res (call-next-method n typeFilter whereFilter actionAtItem direction startLocation))
    (setf res (doSingleItemMapper me n typeFilter whereFilter actionAtItem actionType direction startLocation))
    (if (eq actionType 'sk8::PositionInternal)
      (mapping-convert-index (indexAtVisitState me (1- res)))
      res)
    ))

(define-handler fullRangeMapper (array 
                                    beginIndex endIndex 
                                    typeFilter
                                    whereFilter 
                                    actionAtItem 
                                    actionType
                                    direction
                                    startLocation)
  (let ((listIndex? (listp beginIndex))
        res)
    (unless (or (and (integerp beginIndex) (or (integerp endIndex) (not endindex)))
                (and (listp beginIndex) (listp endIndex)))
      (error "When referencing arrays, both indexes must be an integer or both must be a list."))
    (when listIndex?
      (setf beginIndex (1+ (visitStateAtIndex me beginIndex)))
      (setf endIndex (1+ (visitStateAtIndex me endIndex))))
    (setf res (doFullRangeMapper me beginIndex endIndex typeFilter whereFilter actionAtItem actionType direction startLocation))
    (if (eq actionType 'sk8::PositionInternal)
      (mapcar #'(lambda (x) (mapping-convert-index (indexAtVisitState me (1- x)))) res)
      res)
    ))

;;______________________________________________________________________________________________________________________________

(defparameter *special-selection-types-functions* (list (list word 'words)
                                                              (list paragraph 'paragraphs)
                                                              (list line 'lines) 
                                                              (list character 'characters)))
(defun type-filter-value (the-type)
  (if (and (listp the-type)
           (eq (third the-type) 'sk8::object))
    (fourth the-type)
    the-type))

(define-handler singleItemMapper (Text 
                                     n 
                                     typeFilter
                                     whereFilter 
                                     actionAtItem 
                                     actionType
                                     direction
                                     startLocation)
  (let (specialTypeFilter res)
    (if typeFilter
      (setf specialTypeFilter (second (assoc (type-filter-value typeFilter) *special-selection-types-functions*)))
      (setf specialTypeFilter 'characters))
    (unless specialTypeFilter
      (error "Text can have type filters only of word, paragraph, line or character"))
    (setf res (doSingleItemMapper (funcall specialTypeFilter me) n 
                                  nil whereFilter actionAtItem actionType
                                  direction startLocation))
    (if (is-a res IndirectTextVector) (setf res (baseText res)))
    res)
  )

(define-handler fullRangeMapper (Text  
                                    beginIndex endIndex 
                                    typeFilter
                                    whereFilter 
                                    actionAtItem 
                                    actionType
                                    direction
                                    startLocation)
  (let (specialTypeFilter res)
    (if typeFilter
      (setf specialTypeFilter (second (assoc (type-filter-value typeFilter) *special-selection-types-functions*)))
      (setf specialTypeFilter 'characters))
    (unless specialTypeFilter
      (error "Text can have type filters only of word, paragraph, line or character"))
    (setf res (doFullRangeMapper (funcall specialTypeFilter me) beginIndex endIndex 
                                 nil whereFilter actionAtItem actionType
                                 direction startLocation))
    (if (is-a res IndirectTextVector) (setf res (baseText res)))
    res
    ))

;;______________________________________________________________________________________________________________________________

(define-handler singleItemMapper (AliasedCollection 
                                     n 
                                     typeFilter
                                     whereFilter 
                                     actionAtItem 
                                     actionType
                                     direction
                                     startLocation)
  (singleItemMapper (getpropertyvalue me (collectionProperty me))
                    n 
                    typeFilter
                    whereFilter 
                    actionAtItem 
                    actionType
                    direction
                    startLocation)
  )

(define-handler fullRangeMapper (AliasedCollection  
                                    beginIndex endIndex 
                                    typeFilter
                                    whereFilter 
                                    actionAtItem 
                                    actionType
                                    direction
                                    startLocation)
  (fullRangeMapper (getpropertyvalue me (collectionProperty me))
                   beginIndex endIndex 
                   typeFilter
                   whereFilter 
                   actionAtItem 
                   actionType
                   direction
                   startLocation)
  )






#|
(data foo)
(itemRangeWithFilters foo 1 nil nil nil 0 nil t)

(untrace)
(itemRangeWithFilters '(1 2 3 4 5 6) 1 nil nil nil nil)

(run-it "on testfun
 objectname of any actor whose fillcolor is white in uiselection
end testfun
")
(run-it "on testfun
  fillcolor of every actor in uiselection
end testfun
")
(time (dotimes (i 10000) (testfun)))
(time (testfun))
(time (nconc (nconc (nconc nil (list white)) (list white)) (list white)))

|#

#|
	Change History (most recent last):
	2  	 4/ 3/96	Brian   	New collection mappers.
	3  	 4/ 3/96	Hernan  	moving skil functions
	6  	 4/11/96	Brian   	Basic array access.
	7  	 4/12/96	Brian   	making arrays work nicely.
	9  	 4/17/96	Brian   	fixing various return values.
	10 	 4/19/96	Brian   	Fixing problem with items that = false.
	13 	 4/22/96	Brian   	
	14 	 5/ 2/96	Brian   	removing optional from args.
	15 	 5/ 2/96	Brian   	
	16 	 5/ 2/96	Brian   	
	17 	 5/ 6/96	Brian   	Removing dashes from SKIL locals.
	18 	 5/10/96	Brian   	
	19 	 7/26/96	Brian Roddy	
	20 	 9/ 5/96	Brian   	
	21 	 9/ 5/96	Brian   	
	22 	 2/27/97	Hernan  	
|# ;(do not edit past this line!!)
