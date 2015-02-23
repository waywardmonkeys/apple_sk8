(in-package :SK8DEV)

;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories



#|

Low-level callback functions often let you register the callback along with a
"refcon" (a reference constant) which is later passed as an argument to the callback
function.  In such situations you usually want the refcon to refer to a lisp
object.

Here are two private functions for this purpose.
  objectIDFromObject object (an Object)
  objectFromObjectID id (an integer)

They make use of two hash tables, mapping back and forth from an object to its id.
Each hash table is weak on the object.
It is assumed that the object you want to refer to will be around at least
as long as the callback is registered and might be called.

|#

(defvar *object-id-table*          (make-hash-table :weak :value :test 'eq))
(defvar *object-id-reverse-table* (make-hash-table :weak :key   :test 'eq))
(pushnew *object-id-table*         mf::*all-weak-hash-tables*)
(pushnew *object-id-reverse-table* mf::*all-weak-hash-tables*)

(defvar *object-id-next* most-negative-fixnum)

(define-sk8-function objectIDFromObject :private (theObject)
  "Generate an integer corresponding to theObject so that
a subsequent call to objectFromID with the number as argument
will return the object."
  (or (gethash theObject *object-id-reverse-table*)
      (let ((id *object-id-next*))
        (loop
          (unless (gethash id *object-id-table*)
            (return))
          (setf id (if (>= id most-positive-fixnum)
                     most-negative-fixnum
                     (1+ id))))
        (setf (gethash id        *object-id-table*)         theObject)
        (setf (gethash theObject *object-id-reverse-table*) id)
        (setf *object-id-next* (if (>= id most-positive-fixnum)
                                 most-negative-fixnum
                                 (1+ id)))
        id)))

(define-sk8-function objectFromObjectID :private (id)
  "Given an id (returned from objectIDFromObject), return the object
identified by id, or false if none.  An objectID no longer identifies
an object after the object has been reclaimed by the garbage collector."
  (gethash id *object-id-table*))

#| test

(defvar xyz1id nil)
(defvar xyz2id nil)
(defvar xyz3id1 nil)
(defvar xyz3id2 nil)
(new Rectangle :project SK8 :objectName "xyz1")
(new Rectangle :project SK8 :objectName "xyz2")
(new Rectangle :project SK8 :objectName "xyz3")
(setf *object-id-next* (1- most-positive-fixnum))
(clrhash *object-id-table*)
(setf xyz1id (objectIDFromObject xyz1))
(setf xyz2id (objectIDFromObject xyz2))
(setf xyz3id1 (objectIDFromObject xyz3))
(setf xyz3id2 (objectIDFromObject xyz3))

(progn
  (print xyz1id)
  (print xyz2id)
  (print xyz3id1)
  (print xyz3id2)
  (print (objectFromObjectID xyz1id))
  (print (objectFromObjectID xyz2id))
  (print (objectFromObjectID xyz3id1))
  nil)

(setf (objectName xyz1) nil)
(gc)

|#
#|
	Change History (most recent last):
	1	7/14/94	dy	
	2	7/15/94	dy	improve the comment
	2  	 2/27/97	Hernan  	
|# ;(do not edit past this line!!)
