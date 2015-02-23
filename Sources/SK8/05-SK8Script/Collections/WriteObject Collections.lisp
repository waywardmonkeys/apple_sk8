;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories

(in-package :sk8dev)

;;; New file. WriteObject method for special collections. NOT part of the collection protocol. 

;;; collectionlike used to be used in the collection protocol. now it is used to decide between to print formats

(define-handler collectionlike (Object)
  (declare (ignore me))
  nil)

(define-handler collectionlike (Collection)
  (unless (objectname me) t))

;; add this definition when we have indirectcollection
#|
(define-handler collectionlike (IndirectCollection)
  (unless (objectname me)
    (collectionlike (baseCollection me))))
|#

(define-handler collectionlike (Table)
  (declare (ignore me))
  nil)

(define-handler writeObject (Collection theStream rereadably)
  (unless (maybeWriteObjectName me theStream)
    (if (not (collectionLike me))
      (call-next-method)
      (let ((writeObjectRecursionLimit writeObjectRecursionLimit)
            (*collectionsBeingWritten* (cons me *collectionsBeingWritten*))
            (first t))
        (declare (dynamic-extent *collectionsBeingWritten*))
        (flet ((displayer (elt)
                 (if first
                   (setq first nil)
                   (write-string ", " theStream))
                 (writeObject elt theStream rereadably)))
          (declare (dynamic-extent #'displayer))
          (stream-tyo theStream #\{)
          (if (and (memq me (cdr *collectionsBeingWritten*)) (eql 0 (decf writeObjectRecursionLimit)))
            (write-string "[...]}" theStream)
            (fullRangeMapper me 1 nil nil nil 
                             #'(LAMBDA (arg1 curitem arg2 arg3) 
                                 (declare (ignore arg1 arg2 arg3))
                                 (displayer curitem))
                             'displayer  'AFTER 0))
          (stream-tyo theStream #\}))))))


#|
	Change History (most recent last):
	2  	 7/26/96	sidney  	make unnamed non-collection collections print out better
	3  	 2/27/97	Hernan  	
|# ;(do not edit past this line!!)
