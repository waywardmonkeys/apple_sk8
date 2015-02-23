;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories


;;;

(in-package :SK8Development)

(SK8-declare-syms :SK8 :public ; Updated  4-20-96  12:13 pm
                  sk8::COLLECT sk8::collectionLength sk8::collectionNth sk8::collectionNthState
                  sk8::CONTAINSSUBCOLLECTION sk8::ELEMENTATVISITSTATE sk8::INDEXATVISITSTATE
                  sk8::INITIALVISITSTATE sk8::INSERTATVISITSTATE sk8::KEYATVISITSTATE sk8::POSITIONOFITEM
                  sk8::REMOVEVISITSTATE sk8::REQUIREDPROTOCOLUNIMPLEMENTED sk8::SETELEMENTATKEY
                  sk8::SETELEMENTATVISITSTATE sk8::SUCCEEDINGVISITSTATE sk8::VISITSTATEATINDEX
                  sk8::VISITSTATEATKEY)




(define-handler RequiredProtocolUnimplemented (Collection 
                                                          handlerName
                                                          paramList)
  (error "REQUIRED PROTOCOL NOT IMPLEMENTED FOR: ~A~%~
          Required Handler: ~A~%~
          Parameter list:   ~S~%"
         me handlerName paramList))



#|
   FAKE COLLECTION OBJECTS

   This file implements a set of objects that
   simulate collection behavior using LISP data structures.
   In this fake implementation the Taxon Protocols
   are implemented as methods on Collection

|#
;-----------------------------------------------------------------------------
; HANDLER DEFINITIONS
;-----------------------------------------------------------------------------
#|
    ABOUT COLLECTION PROTOCOL HANDLERS

    There are three categories of handler in the Collection
    Protocol:

    - Provided Handlers
        Handlers that an implementor can count on to be provided
        by the SK8 system. An implementor can simply assume that
        such handlers will work correctly with any object that 
        otherwise obeys the Collection Protocol's requirements.

    - Defined Handlers
        Handlers that are defined by the Protocol, but for which
        no default implementation is provided. Implementors of new 
        collection objects must define implementations of such handlers.

    - Optional Handlers
        Handlers whose signature and behavior are defined, but
        which are not required by the Protocol. Implementors of new
        collections may define implementations of such handlers, but
        are not required to. 
|#
;-----------------------------------------------------------------------------
; Collection
;-----------------------------------------------------------------------------

#| collect (collectionType, {elt1, elt2 ... eltn}) => [Collection]
 ----------------------------------------------------------
   Required Handler

   Creates a collection of the specified type, initialized
   with the elements provided. The format of the element
   arguments varies with the type of collection being created.
|#
(define-handler collect (Collection &rest objects)
  (declare (ignore objects))
  (RequiredProtocolUnimplemented me 
                                 'collect
                                 '(objects...)))

;-----------------------------------------------------------------------------
; Visitation handlers
;-----------------------------------------------------------------------------

#|
    ABOUT VISIT STATES

    The Collection Protocol specifies a logical Taxon named
    VisitState with no associated protocol. Each particular
    state object is allowed to have any protocol that is
    appropriate, subject only to the constraint that it
    can be used with the Collection Protocol for the collections
    for which it is treated as a visitation state.

    A visitation state is a data object that represents a
    reference into the elements of a collection; the
    reference is considered to correspond to the incremental
    state of an iteration over all elements of the collection.
    A visitation state is only valid in reference to the
    collection object for which it was created, and then only
    during the course of a given iteration. Any side-effect
    on the structure of the collection can invalidate the
    state object, and the protocol does not require the
    implementor to detect such an invalidation. The only
    legitimate uses of VisitationState objects are as
    parameters to the Visitation handlers defined below.
|#

#| initialVisitstate) => [Any (a VisitState object)]
 ----------------------------------------------------------
   Required Handler

   Creates a starting visit state for the collection.
   Some specializations of this handler may accept or require
   parameters that provide more information about how to create 
   the initial state.   
|#
(define-handler initialVisitState (Collection &rest initargs)
  (declare (ignore initArgs))
  (error "The object ~a does not support support collection operations" me))


#| succeedingVisitstate (col, stateObj) {with offset offsetObj} 
     => [Any (a VisitState object)]
 ----------------------------------------------------------
   Required Handler

   Returns a VisitState object corresponding to an element of
   the collection. The state returned is considered to be a state
   subsequent to the provided stateObj (that is, a state that is
   visited after stateObj). Offset specifies which subsequent
   state is meant, and the format of the offset is defined by
   the implementor of the collection (in most cases the offset
   would be an integer; thus succeedingVisitstate(X, state1, 3)
   means "the third state after state1".) For many collection types,
   particularly unordered ones, offset has no meaning.
|#
(define-handler succeedingVisitState (Collection 
                                                stateObj)
  (declare (ignore stateObj))
  (error "The object ~a does not support support collection operations" me))


#| elementAtVisitstate (col, stateObj) => [Object | Absent]
 ----------------------------------------------------------
   Required Handler

   Returns the element of the collection at the visit state
   stateObj.
|#
(define-handler elementAtVisitState (Collection stateObj)
  (declare (ignore stateObj))
  (error "The object ~a does not support support collection operations" me))

;-----------------------------------------------------------------------------
; -- End of Visitation handlers --
;-----------------------------------------------------------------------------


;-----------------------------------------------------------------------------
; TXContentMutable
;-----------------------------------------------------------------------------


#| setElementAtVisitState (col, stateObj, newVal) => col
 ----------------------------------------------------------
   Required Handler

   Returns col after replacing the element at stateObj
   with newVal.
|#
(define-handler setElementAtVisitState (Collection
                                                  stateObj
                                                  newVal)
  (declare (ignore stateObj newVal))
  (error "The collection ~a does not support the setting operation" me))

;-----------------------------------------------------------------------------
; TXStructureMutable
;-----------------------------------------------------------------------------

#| removeVisitState (col, stateObj) => col
 ----------------------------------------------------------
   Required Handler

   Returns col after removing the visit state stateObj.
   Any element that was stored at stateObj is no longer
   an element of col (unless it was also stored at another
   visit state).
|#
(define-handler removeVisitState (Collection stateObj)
  (declare (ignore stateObj))
  (error "The collection ~a does not support the remove operation" me))

;-----------------------------------------------------------------------------
; TXStructureMutableOrderedCollection
;-----------------------------------------------------------------------------

(define-handler insertAtVisitState (Collection 
                                             stateObj
                                             obj
                                             &key (after nil))
  (declare (ignore stateObj after obj))
  (error "The collection ~a does not support the insert operation" me))


;-----------------------------------------------------------------------------
; TXOrdered
;-----------------------------------------------------------------------------

#| indexAtVisitState (col, stateObj) => [Any (an Index object)]
 ----------------------------------------------------------
   Required Handler

   Returns the index object that corresponds to the
   visit state.
|#
(define-handler indexAtVisitState (Collection stateObj)
  (declare (ignore stateObj))
  (RequiredProtocolUnimplemented me 
                                 'IndexAtVisitState 
                                 '(Collection stateObj)))


#| visitStateAtIndex (col, indexObj) => [Any (a VisitState object)]
 ----------------------------------------------------------
   Required Handler

   Returns the visit state for col that corresponds to the
   provided index.
|#
(define-handler visitStateAtIndex (Collection indexObj)
  (declare (ignore indexObj))
  (RequiredProtocolUnimplemented me 
                                 'VisitStateAtIndex 
                                 '(Collection indexObj)))

;-----------------------------------------------------------------------------
; TXKeyed
;-----------------------------------------------------------------------------


#| keyAtVisitState (col, stateObj) => [Any (a Key object)]
 ----------------------------------------------------------
   Required Handler

   Returns a key object that corresponds to the supplied
   visit state on col, or, if there is no such valid
   state, returns $Absent.
|#
(define-handler keyAtVisitState (Collection stateObj)
  (declare (ignore stateObj))
  (RequiredProtocolUnimplemented me 
                                 'KeyAtVisitState 
                                 '(Collection stateObj)))

#| visitStateAtKey (col, keyObj) => [Any (a VisitState object)]
 ----------------------------------------------------------
   Required Handler

   Returns a visit state object for col that corresponds to
   the supplied key object, or, if the key object does not
   appear among col's elements, returns $Absent.
|#
(define-handler visitStateAtKey (Collection keyObj)
  (declare (ignore keyObj))
  (RequiredProtocolUnimplemented me 
                                 'VisitStateAtKey 
                                 '(Collection keyObj)))


;-----------------------------------------------------------------------------
; TXContinuous
;-----------------------------------------------------------------------------

;-----------------------------------------------------------------------------
; TXDiscrete
;-----------------------------------------------------------------------------

;-----------------------------------------------------------------------------
; TXContentMutableKeyedCollection
;-----------------------------------------------------------------------------

#| setElementAtKey (col, keyObj, newVal) => col
 ----------------------------------------------------------
   Required Handler

   Returns col after the value associated with the key keyObj
   has been replaced with newVal.
   NOTE: The ContentMutable version is not allowed to add keys
|#

(define-handler setElementAtKey (Collection
                                          keyObj
                                          newVal)
  (declare (ignore keyObj newVal))
  (RequiredProtocolUnimplemented me 
                                 'SetElementAtKey 
                                 '(Collection
                                   keyObj
                                   newVal)))
#|
	Change History (most recent last):
	1  	 4/22/96	Brian   	
	2  	 5/ 2/96	Brian   	Moved stuff to collectionMappers
	3  	 2/27/97	Hernan  	
|# ;(do not edit past this line!!)
