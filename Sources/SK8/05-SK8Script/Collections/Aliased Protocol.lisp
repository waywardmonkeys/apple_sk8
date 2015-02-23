;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories


;;;

(in-package :SK8Development)

(SK8-declare-syms :SK8 :public ; Updated  4-20-96  12:17 pm
                  SK8::ALIASEDCOLLECTION SK8::COLLECTIONPROPERTY)


(new collection :objectname "AliasedCollection" :project sk8)
(define-handler collectionProperty (AliasedCollection)
  (error "Need to write a collectionProperty handler on this collection to specify the name of the property where the collection is located."))

(define-handler collect (AliasedCollection &rest objects)
  (apply 'collect (cons (funcall (collectionProperty me) me) objects) ))

(define-handler initialVisitState (AliasedCollection &rest initargs)
  (apply 'initialVisitState (cons (funcall (collectionProperty me) me) initargs) ))

(define-handler succeedingVisitState (AliasedCollection 
                                                stateObj)
  (succeedingVisitState (funcall (collectionProperty me) me) stateObj))

(define-handler elementAtVisitState (AliasedCollection stateObj)
  (elementAtVisitState (funcall (collectionProperty me) me) stateObj))

(define-handler setElementAtVisitState (AliasedCollection stateObj newVal)
  (setElementAtVisitState (funcall (collectionProperty me) me) stateObj newVal))

(define-handler removeVisitState (AliasedCollection stateObj)
  (removeVisitState (funcall (collectionProperty me) me) stateObj))

(define-handler indexAtVisitState (AliasedCollection stateObj)
  (indexAtVisitState (funcall (collectionProperty me) me) stateObj))

(define-handler isFinalVisitState (AliasedCollection stateObj)
  (isFinalVisitState (funcall (collectionProperty me) me) stateObj))

(define-handler visitStateAtIndex (AliasedCollection indexObj)
  (visitStateAtIndex (funcall (collectionProperty me) me) indexObj))

(define-handler keyAtVisitState (AliasedCollection stateObj)
  (keyAtVisitState (funcall (collectionProperty me) me) stateObj))

(define-handler visitStateAtKey (AliasedCollection keyObj)
  (visitStateAtKey (funcall (collectionProperty me) me) keyObj))

(define-handler setElementAtKey (AliasedCollection keyObj newVal)
  (setElementAtKey (funcall (collectionProperty me) me) keyObj newVal))

(define-handler insertAtVisitState (AliasedCollection stateObj obj
                                                               &key (after 'bogus-val))
  (if (eq after 'bogus-val)
    (insertAtVisitState (funcall (collectionProperty me) me) stateObj obj)
    (insertAtVisitState (funcall (collectionProperty me) me) stateObj obj :after after)))

#|
	Change History (most recent last):
	1  	 4/22/96	Brian   	
	2  	 5/ 2/96	Brian   	
	3  	 2/27/97	Hernan  	
|# ;(do not edit past this line!!)
