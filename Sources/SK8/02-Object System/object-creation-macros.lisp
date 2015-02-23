;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories


;;;


(in-package :macframes)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  The internal representation of creationRelations:
;;;
;;;    The ":hidden" handler 'internal-creationRelations' returns an object's creationRelations -- a list of <creationRelation-chain>s.  Each
;;;    <creationRelation-chain> is of the form (which is tree-like, in order to be able to "left-factor" relation chains):
;;;
;;;       ( [<relationInfo>]+  [ ( [<creationRelation-chain>]+ ) ] )
;;;
;;;    Each of the traversal-paths from start to leaf may contain exactly one occurrence of the marker %begin-simple-relations% (in place
;;;    of a <relationInfo>; relations coming before the marker are "creation" relations and relations coming after the marker are "simple"
;;;    relations, i.e. relations that are maintained, but that don't force creation of new objects.
;;;
;;;    Each <relationInfo> is simply of the form:
;;;
;;;       ( <relationName> . <plural?> )
;;;

(defvar *property-spec-value-marker* 'SK8::value)
(defvar *OLD-property-spec-value-marker* :value)
(defvar *property-spec-options* '((SK8::propagatedValue  . 1)
                                      (SK8::private  . 2)))
(defvar *OLD-property-spec-options* '((:inherit  . 1)
                                           (:private  . 2)))
(defmacro prop-spec-options-propagatable? (options)
  `(logbitp 0 (the fixnum ,options)))


;;; Merge into flags only the "inheritable" flags from otherFlags
;;;
(defmacro merge-SK8-flags (flags otherFlags)
  `(logior (the fixnum ,flags) (the fixnum (logand (the fixnum ,otherFlags) %inheritable-SK8-flags-mask))))


(SK8-declare-syms :SK8 :public ; since they act like a properties (for new/copy) but aren't
                  SK8::otherParents SK8::privateObject SK8::skipInitialization)


(defconstant *start-depth* 0)
(defconstant %begin-simple-relations% 0)
(defconstant %singular% nil)
(defconstant %plural% 0)

(defconstant *object-localCreationRelations* '(SK8::ports*))
(defconstant *io-port-creationRelations-chain* `((SK8::portObject . ,%singular%)))
(defconstant *output-port-creationRelations-chain* `((SK8::wiredTo . ,%plural%)))
(defconstant *port-creationRelations* `((,%begin-simple-relations% ,@*io-port-creationRelations-chain*)
                                            (,%begin-simple-relations% ,@*output-port-creationRelations-chain*)))
(SK8-declare-syms :SK8 :public
                  SK8::ports* SK8::[wiredTo*] SK8::[portObject])


(defmacro plural-relation? (relationInfo)
  `(cdr (the list ,relationInfo)))


(defconstant *creationRelationsInfo-size*
  (CCL::def-accessors CCL::%svref
    creationRelationsInfo-maxDepth
    creationRelationsInfo-objectsTail
    creationRelationsInfo-objects))

(defconstant *correspondenceInfo-size*
  (CCL::def-accessors CCL::%svref
    correspondenceInfo-newObject
    correspondenceInfo-originalObject
    correspondenceInfo-creationDepth
    correspondenceInfo-pendingRelationChains))



;;; Inits creationRelationsInfo-maxDepth to *start-depth*; inits creationRelationsInfo-objects &
;;; creationRelationsInfo-objectsTail to the same empty cons.
;;;
(defmacro with-creationRelationsInfo ((crinfoVar) &body body)
  (require-type crinfoVar 'symbol)
  `(CCL::with-managed-allocation
     (let ((,crinfoVar (CCL::%make-temp-uvector *creationRelationsInfo-size*)))
       (setf (creationRelationsInfo-maxDepth ,crinfoVar) *start-depth*
             (creationRelationsInfo-objectsTail ,crinfoVar)
             (setf (creationRelationsInfo-objects ,crinfoVar) (CCL::%temp-cons nil nil)))
       ,@body)))


;;; Makes a new correspondenceInfo, for insertion into creationRelationsInfo-objects; inits
;;; correspondenceInfo-pendingRelationChains to nil.
;;;
(defmacro make-correspondenceInfo (originalObject newObject currentDepth)
  (let ((info (gensym)))
    `(let ((,info (CCL::%make-temp-uvector *correspondenceInfo-size*)))
        (setf (correspondenceInfo-originalObject ,info) ,originalObject
              (correspondenceInfo-newObject ,info) ,newObject
              (correspondenceInfo-creationDepth ,info) ,currentDepth
              (correspondenceInfo-pendingRelationChains ,info) nil)
        ,info)))


;;; Insert the given correspondenceInfo at the end of crinfo's creationRelationsInfo-objects list
;;; [Inserting the objects in forward order (rather than reverse order) yields a more optimal lookup order.]
;;;
(defmacro insert-into-crinfo-objectsList (correspondenceInfo crinfo)
  (require-type crinfo 'symbol)
  `(setf (creationRelationsInfo-objectsTail ,crinfo)
         (setf (cdr (creationRelationsInfo-objectsTail ,crinfo)) (CCL::%temp-cons ,correspondenceInfo nil))))


(defmacro creationRelationsInfo-objects-youngest-first (crinfo)
  (require-type crinfo 'symbol)
  `(rplacd (creationRelationsInfo-objects ,crinfo)
           (nreverse (cdr (the list (creationRelationsInfo-objects ,crinfo))))))


(defmacro find-or-make-corresponding-object (originalObject crinfo targetProject new? currentDepth)
  (require-type crinfo 'symbol)
  (let ((obj (gensym)))
    `(let ((,obj ,originalObject))
       (or (find-object-correspondenceInfo ,obj ,crinfo)
           (make-object-and-creationGroup ,obj ,crinfo ,targetProject ,new? ,currentDepth)))))


(defmacro with-initializerArguments ((originalObject initializerArgumentsVar) &body body)
  (require-type initializerArgumentsVar 'symbol)
  (let ((conses (gensym)))
    `(let ((,conses (make-list (the fixnum (ash (the fixnum (length ,initializerArgumentsVar)) -1)))))
       (declare (dynamic-extent ,conses))
       (load-initializerArguments ,originalObject ,initializerArgumentsVar ,conses)
       ,@body)))


#|
	Change History (most recent last):
	1  	 9/23/94	chip    	new file
	2  	 9/30/94	chip    	creationRelations hash-table is no longer a constant
	3  	 9/30/94	chip    	broke "port relations" into separate constants for input and output ports
	4  	10/ 4/94	chip    	added with-initializerArguments macro
	5  	10/ 6/94	chip    	cleaned up "port relations" constants
	6  	10/ 7/94	chip    	moved property-spec stuff here
	7  	10/10/94	chip    	added merge-SK8-flags macro
	8  	10/11/94	chip    	added *OLD-property-spec-value-marker* & *OLD-property-spec-options* for backward compatibility
	9  	11/23/94	chip    	creationRelationsInfo & correspondenceInfo structs now use %svref instead of uvref
	10 	12/26/94	sidney  	add :skipInitialization keyword to new to support save as text
	2  	 5/ 7/96	sidney  	Changes for new object system
	2  	 2/27/97	Hernan  	
|# ;(do not edit past this line!!)
