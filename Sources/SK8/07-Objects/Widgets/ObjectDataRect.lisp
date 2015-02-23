;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories

(in-package :sk8dev)

(provide "OBJECTDATARECT")

(require "RECTANGLE")

;;; _______________________________ 
;;; Objects for dragging and dropping data
;;; _______________________________ 

(new rectangle :project SK8 :objectname "ObjectDataRect"
     :properties '(object componentfrom))

(define-handler drop (objectDataRect theactor)
  (call-next-method)
  (when (eventWindow)
    (setf (keytarget (eventWindow)) (keytarget (eventWindow)))))

(define-handler clearReferences (ObjectDataRect)
  (setf (object me) nil)
  (setf (ComponentFrom me) nil))

(define-handler descriptionString (ObjectDataRect)
  (let ((obj (object me)))
    (when obj
      (objectstring obj :project (project (if (listp obj) (first obj) obj))))))


#|
	Change History (most recent last):
	2  	 4/19/96	Brian   	adding clearreferences
	3  	12/17/96	Brian Roddy	adding method to generate description of carried
						object to the dataRects.
	4  	 2/27/97	Hernan  	
|# ;(do not edit past this line!!)
