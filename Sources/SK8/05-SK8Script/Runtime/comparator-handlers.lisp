;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories


;;;


(in-package :SK8Development)

(define-handler equalTo (Object other &key ((:exactMatch exactMatch)))
  (declare (ignore exactMatch))
  (eq me other))
(define-handler greaterThan (Object other)
  ;; The default handler exists (in order to indicate the protocol) but simply raises an error
  (SK8-error UndefinedHandlerError :handlerName 'greaterThan :arguments (list me other)))

(define-handler equalTo (Number other &key exactMatch)
  (if (numberp other)
    (if exactMatch
      (eql me other)
      (= me other))
    nil))
(define-handler greaterThan (Number other)
  (> me other))

(define-handler equalTo (Character other &key exactMatch)
  (unless (characterp other)
    (when (inheritsFrom other Text)
      (let ((otherStr (text other)))
        (if (and otherStr (eql 1 (length otherStr)))
          (setq other (char otherStr 0))
          (return-from equalTo nil)))))
  (if exactMatch
    (eq me other)
    (char-equal me other)))
(define-handler greaterThan (Character other)
  (unless (characterp other)
    (when (inheritsFrom other Text)
      (let ((otherStr (text other)))
        (if (and otherStr (eql 1 (length otherStr)))
          (setq other (char otherStr 0))
          (return-from greaterThan nil)))))
  (> (the fixnum (char-code (char-upcase me)))
     (the fixnum (char-code (char-upcase other)))))


(define-handler equalTo (String otherStr &key exactMatch)
  (when (inheritsFrom otherStr Text)
    (setf otherStr (text otherStr))
    (when (null otherStr)
      (unless (characterp otherStr)
        (return-from equalTo nil)))
    (if exactMatch
      (string= me otherStr)
      (string-equal me otherStr))))
(define-handler greaterThan (String otherStr)
  (when (inheritsFrom otherStr Text)
    (setf otherStr (text otherStr))
    (when (string-greaterp me otherStr) t)))



(define-handler equalTo (Text otherStr &key exactMatch)
  (if (inheritsFrom otherStr Text)
    (let (str)
      (setf str (text me))
      (setf otherStr (text otherStr))
      (when (null otherStr)
        (if (characterp otherStr)
          (setq otherStr otherStr)
          (return-from equalTo nil)))
      (if exactMatch
        (string= str otherStr)
        (string-equal str otherStr)))
    (eq me otherStr)))

(define-handler greaterThan (Text other)
  (when (inheritsFrom other Text) 
    (let ((str (text me))
          (otherStr (text other)))
      ;; Make it work for chars and raise an error for non-textual things
      (when (null otherStr) (setq otherStr other))
      (when (string-greaterp str otherStr) t))))

(define-handler equalTo (Collection other &key exactMatch)
  (if (and (is-a me text) (is-a other collection))
    (and (containsSubCollection me other nil nil :exactMatch exactMatch)
         (eql (SK8::length me) (SK8::length other)))
    (eq me other)))

(define-handler greaterThan (Collection other)
  (let ((curState (initialVisitState me)))
    (unless curstate (return-from greaterThan nil))
    (loop
      (when (isFinalVisitState me curState) (return-from greaterThan t))
      (when (sk8::<= (elementAtVisitState me curstate) other)
        (return-from greaterThan nil))
      (setf curstate (succeedingVisitState me curstate)))
    nil))




#|
	Change History (most recent last):
	6		1/11/94	hernan	self -> me
	7		3/10/94	chip	equalTo for Collection now heeds collectionLike
	8  	10/24/94	chip    	!collection= --> collection-equal
	9  	11/ 7/94	chip    	fixed Collection's greaterThan + added one for Character (radar #1197925)
	10 	11/11/94	chip    	filled in a few more handlers
	11 	12/22/94	me      	Fix object-type-related bug in equalTo
	2  	 6/26/95	Hernan  	equalTo of Text should check if the other argument is a 
							Text collection. If not, eq is called.
	2  	 4/10/96	Hernan  	Removing case preserving reader macro.
	3  	 4/16/96	Brian   	
	4  	 4/16/96	Brian   	
	5  	 4/19/96	Brian   	
	6  	 4/22/96	Brian   	
	7  	 5/ 2/96	Brian   	
	8  	 5/10/96	Brian   	removing bogus reference to collectionLike
	9  	 9/19/96	Brian   	
	10 	 9/19/96	Brian   	
	11 	 2/27/97	Hernan  	
|# ;(do not edit past this line!!)
