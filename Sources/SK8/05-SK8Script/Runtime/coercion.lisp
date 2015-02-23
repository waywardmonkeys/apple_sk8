;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories


;;;

(in-package :SK8Development)




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Some support functions...
;;;


(defun maybe-coercion-error (obj type)
  (if (is-a obj type)
    obj
    (SK8-error CoercionError :object obj :type type)))


(defun maybe-coerce-collection-to-string (coll)
  (let* ((size (SK8::length coll))
         (str (make-string size))
         (i -1)
         (currentState (initialVisitState coll))
         item)
    (loop
      (if (isFinalVisitState coll currentState) (return))
      (setf item (elementAtVisitState coll currentState))
      (setf (aref str (incf i))
            (cond
             ((characterp item) item)
             ((stringp item) (if (eql (length item) 1)
                               (aref item 0)
                               (return-from maybe-coerce-collection-to-string nil)))
             ((and (mf::!textp item) (eql (SK8::length item) 1))
              (elementAtVisitState item (initialVisitState item)))
             (t
              (return-from maybe-coerce-collection-to-string nil))))
      (setf currentState (succeedingVisitState coll currentState)))
    str))


(defun string-to-number (str &optional start end)
  (declare (fixnum start end))
  (unless start (setq start 0))
  (unless end (setq end (length str)))
  (let ((real-start start)
        (real-end end)
        (i start)
        possible-end ch num)
    (declare (fixnum i) (character ch))
    (tagbody
      :RETRY
      ;; Find the first valid char that could start the number: start
      (loop
        (when (eq i end) (go :NOPE))
        (when (position (aref str i) "0123456789.-+")       ; It can only start with one of these...
          (setq start i)
          (return))
        (incf i))
      
      (when (or (eq (setq ch (aref str i)) #\+) (eq ch #\-)) (incf i))
      
      ;; Find the end of the integral part
      (loop
        (when (eq i end) (go :FOUND-END))
        (unless (position (aref str i) "0123456789") (return))
        (incf i))
      
      (when (eq (aref str i) #\.)
        (incf i)
        ;; Find the end of the fractional part
        (loop
          (when (eq i end) (go :FOUND-END))
          (unless (position (aref str i) "0123456789") (return))
          (incf i)))
      
      (when (position (aref str i) "eEsS")
        (setq possible-end i)
        (incf i)
        (cond
         ((position (aref str i) "-+0123456789")
          (incf i)
          ;; Find the end of the exponent part
          (loop
            (when (eq i end) (return))
            (unless (position (aref str i) "0123456789") (return))
            (incf i))
          ;; Back end up if it would be an invalid ending
          (loop
            (when (eq (decf i) start) (return))
            (unless (position (aref str i) "eEsS-+")            ; ...but can't end with one of these.
              (incf i)
              (return))))
         (t
          (setq i possible-end))))
      
      :FOUND-END
      (cond
       ((numberp (setq num (read-from-string str nil nil :start start :end i)))
        (return-from string-to-number num))
       (t
        (unless (eql start end)
          (setq i (1+ start))
          (go :RETRY))))
      
      :NOPE
      (SK8-error CoercionError :object (subseq str real-start real-end) :type Number))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; numberAs
;;;

(define-handler numberAs (Object obj &key ((:project proj)))
  (declare (ignore proj))
  (maybe-coercion-error obj me))

(defmethod numberAs ((me (eql Character)) obj &key ((:project proj)))
  (declare (ignore proj))
  (if (<= 0 obj 255)
    (code-char obj)
    (call-next-method)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; stringAs
;;;

(define-handler stringAs (Object obj &key ((:project proj)))
  (declare (ignore proj))
  (maybe-coercion-error obj me))

(defmethod stringAs ((me (eql Character)) obj &key ((:project proj)))
  (declare (ignore proj))
  (if (eql 1 (length obj))
    (char obj 0)
    (call-next-method)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; collectionAs
;;;

(define-handler collectionAs (Object obj &key ((:project proj)))
  (declare (ignore proj))
  (maybe-coercion-error obj me))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; objectAs
;;;
;;;   The secondary entry point, called by asType's default handler;  dispatches on the given object.
;;;   The default handler dispatches one of the more specific type-dispatched coercers (numberAs,
;;;   stringAs, collectionAs) or raises a CoercionError to indicate its failure
;;;

(define-handler objectAs (Object &key type ((:project proj)))
  (cond
   ((numberp me)
    (numberAs type me :project proj))
   ((mf::!textp me)
    (stringAs type (if (stringp me) me (objectAsString me  :project proj))
              :project proj))
   ((mf::!collectionp me)
    (collectionAs type me :project proj))
   (t
    (maybe-coercion-error me type))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; objectAsNumber
;;;

(define-handler objectAsNumber (Number &key type ((:project proj)))
  (declare (ignore proj))
  (cond
   ((null type)
    me)
   ((or (eq type Integer) (eq type BigInteger))
    (values (round me)))
   ((eq type SmallInteger)
    (if (fixnump (setq me (round me)))
      me
      (call-next-method)))
   ((mf::clos-class-shadow-p type)
    (coerce me (class-name type)))
   (t
    (maybe-coercion-error me type))))

(define-handler objectAsNumber (Text &key type ((:project proj)))
  (declare (ignore proj))
  (let ((newstr (string-to-number me)))
    (if type
      (objectAsNumber newstr :type type)
      newstr)))

(define-handler objectAsNumber (Character &key type ((:project proj)))
  (declare (ignore proj))
  (cond
   ((digit-char-p me)
    (setq me (- (char-int me) #.(char-int #\0)))
    (if type
      (objectAsNumber me :type type)
      me))
   (t
    (call-next-method))))

(define-handler objectAsNumber (Object &key type ((:project proj)))
  (declare (ignore proj))
  (maybe-coercion-error me type))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; objectAsString
;;;

(define-handler objectAsString (Text &key ((:project proj)))
  (declare (ignore proj))
  (text me))

(define-handler objectAsString (String &key ((:project proj)))
  (declare (ignore proj))
  me)

(define-handler objectAsString (Character &key ((:project proj)))
  (declare (ignore proj))
  (string me))

(define-handler objectAsString (Symbol &key ((:project proj)))
  (declare (ignore proj))
  (SS::pretty-symbol-name me))

(define-handler objectAsString (Collection &key ((:project proj)))
  (if (is-a me text)
    (or (maybe-coerce-collection-to-string me)
        (call-next-method))
    (objectString me :project proj)))

(define-handler objectAsString (Object &key ((:project proj)))
  (objectString me :project proj))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; objectAsList
;;;

(define-handler objectAsList (List &key ((:project proj)))
  (declare (ignore proj))
  me)

(define-handler objectAsList (Vector &key ((:project proj)))
  (declare (ignore proj))
  (CCL::coerce-to-list me))

(define-handler objectAsList (Collection &key ((:project proj)))
  (declare (ignore proj))
  (let* ((head (list nil))
         (lst head)
         (currentState (initialVisitState me)))
    (loop
      (if (isFinalVisitState me currentState) (return))
      (setf (cdr lst) (cons (elementAtVisitState me currentState) nil)
            lst (cdr lst))
      (setf currentState (succeedingVisitState me currentState)))
    (cdr head)))

(define-handler objectAsList (Object &key ((:project proj)))
  (declare (ignore proj))
  (maybe-coercion-error me List))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; asType
;;;
;;;   The "generic" entry point;  dispatches on the given type.
;;;   The default handler dispatches one of the more specific object-dispatched coercers (objectAsNumber,
;;;   objectAsString, objectAsList) or the general object-dispatched coercer (objectAs)
;;;

(define-handler asType (Object obj &key ((:project proj)))
  (cond
   ((not (CCL::standard-object-p me))                         (objectAs obj :type me :project proj))
   ((eq me Text)             (if (mf::!textp obj) obj (objectAsString obj :project proj)))
   ((eq me String)                                    (objectAsString obj :project proj))
   ((eq me List)                                      (objectAsList obj :project proj))
   ((eq me Collection) (if (mf::!collectionp obj) obj (objectAsList obj :project proj)))
   ((and (mf::clos-class-shadow-p me) (memq Number (CCL::%inited-class-cpl me)))
    (objectAsNumber obj :type (unless (eq me Number) me) :project proj))
   (t (objectAs obj :type me :project proj))))

(defmethod asType ((me (eql Vector)) obj &key ((:project proj)))
  (declare (ignore proj))
  (coerce (objectAsList obj) 'vector))

(defmethod asType ((me (eql Array)) obj &key ((:project proj)))
  (declare (ignore proj))
  (if (arrayp obj)
    obj
    (labels ((list-dimensionality (theList)
               (if (null theList)
                 '(0)
                 (let ((thisDimension 0)
                       (subDimensionality t))
                   (declare (fixnum thisDimension))
                   (flet ((common-dimensions (dims1 dims2)
                            (if (not (listp dims1))
                              dims2
                              (let ((numCommon 0))
                                (dolist (d dims1)
                                  (if (eql d (pop dims2))
                                    (incf numCommon)
                                    (return)))
                                (subseq dims1 0 numCommon)))))
                     (dolist (item theList)
                       (incf thisDimension)
                       (if (and subDimensionality (consp item))
                         (setq subDimensionality (common-dimensions subDimensionality
                                                                    (list-dimensionality item)))
                         (setq subDimensionality nil)))
                     (cons thisDimension subDimensionality))))))
      (let* ((theList (objectAsList obj))
             (dimensions (list-dimensionality theList)))
        (make-array dimensions
                    :initial-contents theList
                    :adjustable NIL)))))


(defmethod asType ((me (eql Symbol)) obj &key ((:project proj)))
  (intern-symbol (if (stringp obj)
                   obj
                   (objectAsString obj :project proj))
                 (package proj)))

#|
	Change History (most recent last):
	7		1/11/94	hernan	self -> me
	8		2/25/94	chip	added project arg to objectAs
	9		3/6/94	chip	whole new two-tier protocol for generic coercion
	1		3/6/94	chip	moved to function library folder
	2		3/6/94	chip	slight cleanup
	3		3/8/94	chip	fixed glitch in stringAs dispatch
	4		3/11/94	kleiman	fixed typo in asType
	5		4/22/94	chip	fixed a couple coercions for characters
	6		6/29/94	chip	case-saving-intern --> intern-symbol/intern-legal-varName/intern-legal-handlerName (for radar #107741)
	7		7/14/94	chip	fixed call to %inited-class-cpl in asType of Object (radar #1174597)
	8		7/21/94	chip	defined asType for Array -- uses the list's "dimensionality" (radar #1175270)
	9		8/17/94	chip	fixed coercion "base-case" not to raise error if the thing is already of the given type (radar #1174597)
	10 	10/20/94	chip    	simplified some coercions; added some notes
	11 	10/31/94	chip    	fixed asType for numbers -- it was getting the class of the built-in-type, instead of using it as the class (radar #1196351)
	12 	 3/ 7/95	me      	Fixed coercion of a list of indirect characters to a string (in maybe-coerce-collection-to-string)
	2  	 4/10/96	Hernan  	Removing case preserving reader macro.
	4  	 4/19/96	Brian   	Fixing method congruency problems.
	5  	 4/22/96	Brian   	
	6  	 5/ 2/96	Brian   	
	7  	 5/ 7/96	sidney  	remove obsolete function all
	8  	 5/ 8/96	Hernan  	Has to recompile.
	9  	 5/13/96	Brian   	removing a with-temp-string.
	10 	 5/20/96	Brian   	fixing calling args of objectAs
	11 	 9/ 3/96	Hernan  	Removing calls to internal functions...
	12 	11/26/96	Hernan  	currentCharacter -> elementAtVisitState.
	13 	 2/27/97	Hernan  	
|# ;(do not edit past this line!!)
