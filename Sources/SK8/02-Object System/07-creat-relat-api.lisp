;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories

;;;-*- Mode: Lisp; Package: MACFRAMES -*-

(in-package :macframes)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  High-level creationRelations access API...
;;;

(define-handler sk8:creationRelations (SK8:Object)
  (nconc (internal-to-pretty-creationRelations (internal-creationRelations me))
         (when (is-a me SK8:Port)
           (internal-to-pretty-creationRelations *port-creationRelations*))
         *object-localCreationRelations*))


(define-handler sk8:localCreationRelations (SK8:Object)
  (cond
   ((or (eq me SK8:Object) (not (typep me 'sk8:object)))
    *object-localCreationRelations*)
   ((is-a me SK8:Port)
    (internal-to-pretty-creationRelations *port-creationRelations*))
   (t
    (internal-to-pretty-creationRelations (internal-localCreationRelations me)))))


(define-handler (setf sk8:localCreationRelations) (relations SK8:Object)
  (cond
   ((or (eq me sk8:Object) (not (typep me 'sk8:object)) (is-a me SK8:Port))
    (if (equalp relations (sk8:localCreationRelations me))
      relations
      (sk8::raiseUnsettablePropertyError 'creationRelations me)))
   (relations
    (setq relations (pretty-to-internal-creationRelations relations))
    (let* ((old-localRelations (internal-localCreationRelations me))
           (new-localRelations (if old-localRelations
                                 (set-difference relations old-localRelations :test #'equalp)
                                 relations))
           (all-relations (internal-creationRelations me)))
      ;; Ensure there are no duplicates of inherited relations
      (dolist (relation new-localRelations)
        (when (member relation all-relations :test #'equalp)
          (setq relations (delete relation relations :test #'equalp))))
      ;; Store the relations list
      (setf (internal-localCreationRelations me) relations)
      ;; Return the pretty representation
      (internal-to-pretty-creationRelations relations)))
   (t
    ;; No relations; just store the empty list
    (setf (internal-localCreationRelations me) nil))))

(define-handler SK8:tagPart (SK8:Object partObject partName)
  (require-type partName 'symbol)
  ;; Add the property if necessary
  (unless (slot-exists-p me partName)
    (SK8:addProperty me partName))
  ;; Make the creation relation if necessary
  (unless (creation-relation-p me partName)
    (push partName (SK8:localCreationRelations me)))
  ;; Initialize the property with the given object
  (SK8:setPropertyValue me partName partObject)
  partName)

#|
	Change History (most recent last):
	1  	 5/ 9/96	sidney  	New object system
	2  	11/26/96	Hernan  	raiseUnsettablePropertyError is in sk8 now.
	3  	 2/27/97	Hernan  	
|# ;(do not edit past this line!!)
