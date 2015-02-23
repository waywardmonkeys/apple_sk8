;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories
(in-package :ccl)

;;; 8/95 Dave Yost, Apple ATG

#|

Someday, when weak-on-both hash-tables are implemented,
you'll be able to retrieve the object in the set that
matches the given test even if the set is weak.

|#
(defclass hash-set ()
  ((default :accessor hash-set-default :initarg :default)
   ;; MCL doesn't implement hash-table-weak, so we need a slot for it
   (weak                               :initarg :weak)
   (data    :reader   hash-set-data))
  (:default-initargs
    :default nil
    :weak nil)
  (:documentation "A set implemented with hashing."))

(defmethod initialize-instance ((set hash-set)
                                    &rest rest
                                    &key
                                    weak
                                    test
                                    size
                                    rehash-size
                                    rehash-threshold
                                    default
                                    contents)
  (declare (ignore weak test size rehash-size rehash-threshold default))
  (call-next-method)
  (with-slots (data) set
    (let ((weak-arg (getf rest :weak))
          (test-arg (getf rest :test))
          (weak-setting #+hash-tables-weak-on-both :both
                        #-hash-tables-weak-on-both :key
                        ))
      (when weak-arg
        (unless (eq test-arg #'eq)
          (error "A weak hash-set must test with eq"))
        (setf (getf rest :weak) weak-setting)
        (with-slots (weak) set
          (setf weak weak-setting))))
    (remf rest :default)
    (remf rest :contents)
    (setf data (apply #'make-hash-table rest))
    (dolist (elem contents)
      (hash-set-replace elem set))
    set))

(defun make-hash-set (&rest rest
                               &key
                               weak
                               test
                               size
                               rehash-size
                               rehash-threshold
                               default
                               contents)
  "Makes a new hash-set object.
The keyword arguments are as for a hash-table except for
weak
    a boolean,
default
    the value returned when you ask for the member
    of the set that satisfies the set's test, and
    there is no such member,
contents
    a list of initial set contents"

  (declare (ignore weak test size rehash-size rehash-threshold default contents))
  (apply #'make-instance 'hash-set rest))

(defmethod print-object ((set hash-set) the-stream)
  (with-slots (data default weak) set
    (print-unreadable-object (set the-stream :type t :identity t)
      (format the-stream "~S ~S size ~D/~D~:[ weak~]~:[ Locked~]"
              ':test (hash-table-test data)
              (hash-table-count data)
              (hash-table-size data)
              (not weak)
              (eql (ccl::nhash.lock data) 0)
              ))))

(defmethod hash-set-clear ((set hash-set))
  "Remove all members of the set"
  (clrhash (hash-set-data set))
  set)

(declaim (inline hash-set-replace))
;;; Put the object itself in as the value in case test is not #'eq.
(defmethod hash-set-replace (obj (set hash-set))
  "Ensure that obj is in the set.  If there was already
an object in the set that tested equal to obj, it is replaced."
  (with-slots (data weak) set
    (setf (gethash obj data) (if (eq weak :key)
                               t
                               obj))
    (values)))

(declaim (inline hash-set-remove))
;;; Put the object itself in as the value in case test is not #'eq.
(defmethod hash-set-remove (obj (set hash-set))
  "Ensure that obj is not in the set.
Returns t if there was such an entry, otherwise nil."
  (with-slots (data) set
    (remhash obj data)))

(declaim (inline hash-set-member))
(defmethod hash-set-member (obj (set hash-set))
 #+hash-tables-weak-on-both
 "Returns 2 values:
  t if obj is a member of the set, else nil;
  the object found in the set."
 #-hash-tables-weak-on-both
 "Returns t if obj is a member of the set, else nil.
Unless the set is weak, return as a second additional value
the object found in the set."
  (with-slots (data default weak) set
    (multiple-value-bind (result was-present) (gethash obj data default)
      (if (eq weak :key)
        was-present
        (values was-present
                result)))))

(defmethod map-hash-set (function (set hash-set))
    "calls function for each entry in hash-table, passing as argument
the object in the set. Entries should not be added or removed while
map-hash-set is in progress. map-hash-set returns nil."
  (with-slots (data) set
    (maphash #'(lambda (key val)
                 (declare (ignore val))
                 (funcall function key))
             data))
  (values))

(defmethod hash-set-test ((set hash-set))
  "Returns the test applied to determine set membership."
  (with-slots (data) set
    (hash-table-test data)))

(defmethod hash-set-count ((set hash-set))
  "Returns the number of objects in the set."
  (with-slots (data) set
    (hash-table-count data)))

(defmethod hash-set-size ((set hash-set))
  "Returns the current size of the set."
  (with-slots (data) set
    (hash-table-size data)))

(defmethod hash-set-rehash-size ((set hash-set))
  "Returns the current rehash-size of the set."
  (with-slots (data) set
    (hash-table-rehash-size data)))

(defmethod hash-set-rehash-threshold ((set hash-set))
  "Returns the current rehash-threshold of the set."
  (with-slots (data) set
    (hash-table-rehash-threshold data)))

(defmethod hash-set-weak ((set hash-set))
  "Returns t if the hash-set is weak, otherwise nil."
  (with-slots (data weak) set
    ;; MCL doesn't implement hash-table-weak
    (and weak t)))

(defmethod ensure-compatible-hash-sets ((set1 hash-set)
                                             (set2 hash-set))
  (unless (eq (hash-set-test set1)
              (hash-set-test set2))    (error "sets have incompatible test properties"))
  (unless (eq (hash-set-default set1)
              (hash-set-default set2)) (error "sets have incompatible default properties")))

(defmethod hash-set-union ((set1 hash-set)
                              (set2 hash-set)
                              &key
                              modify-set1
                              weak)
  "Returns a set of objects that are in set2
plus any objects from set1 that are not in set2.
If modify-set1 is non-nil, then modifies set1 and returns it;
otherwise returns a new set with the same test as set1 and
with weakness set according to the :weak keyword argument"
  (ensure-compatible-hash-sets set1 set2)
  (if modify-set1
    (progn
      (map-hash-set #'(lambda (elem)
                        (hash-set-replace elem set1))
                    set2)
      set1)
    (let ((result (make-hash-set :test (hash-set-test set1) :weak weak)))
      (dolist (set (list set1 set2))
        (map-hash-set #'(lambda (elem)
                          (hash-set-replace elem result))
                      set))
      result)))

(defmethod hash-set-intersection ((set1 hash-set)
                                      (set2 hash-set)
                                      &key
                                      modify-set1
                                      weak)
  "Returns a set of objects from set1 that are also in set2.
If modify-set1 is non-nil, then modifies set1 and returns it;
otherwise returns a new set with the same test as set1 and
with weakness set according to the :weak keyword argument"
  (ensure-compatible-hash-sets set1 set2)
  (if modify-set1
    (progn
      (let ((member-list (hash-set-list set1)))
        (dolist (elem member-list)
          (unless (hash-set-member elem set2)
            (hash-set-remove elem set1))))
      set1)
    (let ((result (make-hash-set :test (hash-set-test set1) :weak weak)))
      (map-hash-set #'(lambda (elem)
                        (when (hash-set-member elem set2)
                          (hash-set-replace elem result)))
                    set1)
      result)))

(defmethod hash-set-difference ((set1 hash-set)
                                    (set2 hash-set)
                                    &key
                                    modify-set1
                                    weak)
  "Returns a set of objects that are in set1 but not set2.
If modify-set1 is non-nil, then modifies set1 and returns it;
otherwise returns a new set with the same test as set1 and
with weakness set according to the :weak keyword argument"
  (ensure-compatible-hash-sets set1 set2)
  (if modify-set1
    (progn
      (map-hash-set #'(lambda (elem)
                        (hash-set-remove elem set1))
                    set2)
      set1)
    (let ((result (make-hash-set :test (hash-set-test set1) :weak weak)))
      (map-hash-set #'(lambda (elem)
                        (unless (hash-set-member elem set2)
                          (hash-set-replace elem result)))
                    set1)
      result)))

(defmethod hash-set-exclusive-or ((set1 hash-set)
                                      (set2 hash-set)
                                      &key
                                      modify-set1
                                      weak)
  "Returns a set of objects that are in set1 or set2 but not both.
If modify-set1 is non-nil, then modifies set1 and returns it;
otherwise returns a new set with the same test as set1 and
with weakness set according to the :weak keyword argument"
  (ensure-compatible-hash-sets set1 set2)
  (if modify-set1
    (progn
      (map-hash-set #'(lambda (elem)
                        (if (hash-set-member elem set1)
                          (hash-set-remove  elem set1)
                          (hash-set-replace elem set1)))
                    set2)
      set1)
    (let ((result (make-hash-set :test (hash-set-test set1) :weak weak)))
      (loop for set-1 in (list set1 set2)
            for set-2 in (list set2 set1)
            doing (map-hash-set #'(lambda (elem)
                                    (unless (hash-set-member elem set-2)
                                      (hash-set-replace elem result)))
                                set-1))
      result)))

(defmethod hash-set-list ((set hash-set))
  "Returns the members of set as a list."
  (let ((set-list '()))
    (map-hash-set #'(lambda (item)
                      (push item set-list))
                  set)
    set-list))

#| TESTS

(defvar *testset* nil)

(setf *testset* (make-instance 'hash-set))
(progn
  (print (hash-set-test    *testset*))
  (print (hash-set-default *testset*))
  (print (hash-set-weak    *testset*))
  (print (hash-set-data    *testset*))
  (values))
(hash-set-replace 'a *testset*)
(hash-set-member 'a *testset*)
(hash-set-member 'x *testset*)
(map-hash-set #'print *testset*)

(hash-set-remove 'a *testset*)
(map-hash-set #'print *testset*)

(hash-set-clear *testset*)
(map-hash-set #'print *testset*)

(setf *testset* (make-hash-set :contents '(a b c)))
(hash-set-count *testset*)
(hash-set-size  *testset*)

(let ((set1 (make-hash-set :contents '(a b)))
      (set2 (make-hash-set :contents   '(b c))))
  (print (hash-set-list (hash-set-union        set1 set2)))
  (print (hash-set-list (hash-set-intersection set1 set2)))
  (print (hash-set-list (hash-set-difference   set1 set2)))
  (print (hash-set-list (hash-set-exclusive-or set1 set2)))
  (values))

(progn
  (let ((set1 (make-hash-set :contents '(a b)))
        (set2 (make-hash-set :contents   '(b c))))
    (print (hash-set-list (hash-set-union        set1 set2 :modify-set1 t))))
  (let ((set1 (make-hash-set :contents '(a b)))
        (set2 (make-hash-set :contents   '(b c))))
    (print (hash-set-list (hash-set-intersection set1 set2 :modify-set1 t))))
  (let ((set1 (make-hash-set :contents '(a b)))
        (set2 (make-hash-set :contents   '(b c))))
    (print (hash-set-list (hash-set-difference   set1 set2 :modify-set1 t))))
  (let ((set1 (make-hash-set :contents '(a b)))
        (set2 (make-hash-set :contents   '(b c))))
    (print (hash-set-list (hash-set-exclusive-or set1 set2 :modify-set1 t))))
  (values))

;;; Test nil as a valid set member.
(setf *testset* (make-hash-set :default :not-present :contents '(nil a b)))
(map-hash-set #'print *testset*)
(hash-set-member 'a  *testset*)
(hash-set-member nil *testset*)
(hash-set-member 'x  *testset*)

(defvar *str1* "ab")
(defvar *str2* "ab")
(eq *str1* *str2*)

;; When #+hash-tables-weak-on-both or the hash-set is not weak, then
;; these next tests should return t when evaluated in this order:
(eq *str1* (multiple-value-bind (is-member value) (hash-set-member  *str1* *testset*)
             (declare (ignore is-member))
             value))
(eq *str1* (multiple-value-bind (is-member value) (hash-set-member  *str2* *testset*)
             (declare (ignore is-member))
             value))
(eq *str2* (progn
             (hash-set-replace *str2* *testset*)
             (multiple-value-bind (is-member value) (hash-set-member  *str2* *testset*)
               (declare (ignore is-member))
               value)))


|#

(export '(hash-set
          make-hash-set
          hash-set-default
          hash-set-weak
          hash-set-data
          hash-set-clear
          hash-set-replace
          hash-set-remove
          hash-set-member
          map-hash-set
          hash-set-test
          hash-set-count
          hash-set-size
          hash-set-rehash-size
          hash-set-rehash-threshold
          hash-set-union
          hash-set-intersection
          hash-set-difference
          hash-set-exclusive-or
          hash-set-list)
        *package*)
#|
	Change History (most recent last):
	1  	 8/10/95	dy      	
	2  	 8/11/95	dy      	Oops.  forgot hash-set-remove.
						Added :modify-set1 args to the binary operations.
	3  	 2/27/97	Hernan  	
|# ;(do not edit past this line!!)
