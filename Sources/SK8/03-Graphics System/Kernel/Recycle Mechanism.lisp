(in-package :SK8Development)

;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories



;; _____________________________________________________________
;; _____________________________________________________________
;; 
;; RECYCLE MECHANISM
;;
;;
;; _____________________________________________________________
;; _____________________________________________________________

#| Modification History

01-22-93 ruben id -> objectName

|#

;; THE GARBAGE COLLECTOR
;; Holds a list of objects available for GC

(defvar *actorsWithGarbage* (make-hash-table :Test #'eq :weak :key)) ;; actors who have garbage
(pushnew *actorsWithGarbage* mf::*all-weak-hash-tables*)

;;; DISCARD -- Call when you are done with an object. 

(define-handler discard (object)
  (let* ((parent (baseParent me))
         (currentItems (getHash parent *actorsWithGarbage*)))
    (if currentItems
      (pushNew me (ccl::population-data currentitems))
      (Setf (getHash parent *actorsWithGarbage*)
            (ccl::%cons-population (list me))))))

;;  COMPACTSK8
;; Goes through and gets rid of all of the temp objects.
;; It's a lot simpler now that dispose is no longer used
(defun compactSK8 ()
  (clrhash *actorsWithGarbage*)
  t)


;; RECYCLED
;; Returns a recycled object, if one is available.  Otherwise makes a new object.

(define-handler recycled (object &key (in (project me)))
  (let ((orphanPop (getHash me *actorsWithGarbage*))
        orphan)
    (when orphanPop
      (setf orphan (pop (ccl::population-data orphanPop)))
      (unless (ccl::population-data orphanPop)
        (remHash me *actorsWithGarbage*)))
    (or orphan
        (new me :project in))))

#|
	Change History (most recent last):
	2	5/24/93	Hernan	Moved menu discard methods to the menus file.
	3	5/25/93	rod	Recycled is now guaranteed to not return an
				object that has already been disposed!!!
	9	1/11/94	hernan	self -> me
	10	2/21/94	hernan	The Final Renaming for Alpha! (yeah, right...)
	11	2/28/94	hernan	Avoiding disposing things directly.
	12	3/3/94	Hernan	The Great Argument Name Renaming of 94!
	13	3/9/94	Hernan	Getting rid of disposed.
	14	3/14/94	sidney	Use population to hold discarded objects for recycling so they can gc
	2  	 2/27/97	Hernan  	
|# ;(do not edit past this line!!)
