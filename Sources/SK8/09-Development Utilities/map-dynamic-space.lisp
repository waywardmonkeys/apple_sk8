;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories

;;; New %map-dynamic-space function for MCL 3.0
;;; 
;;; -- till, June 95
;;;
;;; -- more stuff, DY, July, 1995
;;;

(in-package :ccl)

(cl-user::sk8-build-files "ccl:sk8;development utilities;thermometer")

(defun print-short (obj)
  (let ((*print-length* 3)
      (*print-level* 2))
     (print obj)))

#|
(setf *print-length* 3)
(setf *print-level*  2)
|#

#|

(defvar stragglers nil)

#|  Make a project called Reapee  |#

(defun test-reap ()
  (sk8:new sk8:object :objectname "till" :project sk8::Reapee)
  (setf stragglers (ccl::%cons-population (list 'Reapee::till)))

  ; (setf (sk8::sk8_id Reapee::till) nil)
  ; (makunbound 'Reapee::till)

  (setf (sk8:knownChildren sk8:Object) (delete Reapee::till (sk8:knownChildren sk8:Object)))
  (clrhash (sk8:objectTable sk8:Reapee))
  (unintern 'Reapee::till :Reapee)
  )

(progn
  (test-reap)
  ; Blow away test-reap, which refers to the symbol!
  (setf (symbol-function 'test-reap) #'(lambda ()))
  )
t
t
(gc)

(progn
  (print (first (ccl::population-data stragglers)))
  t)

(print-references-to (first (ccl::population-data stragglers)))
(print (ccl::nth-reference-to
                           1
                           (first (ccl::population-data stragglers)))
       
(print-references-to (ccl::nth-reference-to
                           1
                           (first (ccl::population-data stragglers))))

(print-references-to (ccl::nth-reference-to
                           2
                           (first (ccl::population-data stragglers))))
|#

;;;=======================================================================

(defun funcall-unless-static (func obj even-if-static? &key even-if-unbound?)
  (when (if even-if-static?
          (or even-if-unbound?
              (not (eq obj (%unbound-marker-8))))
          (not (static? obj)))
    (funcall func obj)))

(let ((seen-types (make-hash-set)))
  (defun reset-seen-types ()
    (prog1
      seen-types
      (setf seen-types (make-hash-set))))
  (defun print-unseen-type (obj)
    (let ((type (our-type-of obj)))
      (unless (hash-set-member type seen-types)
        (trace-print () "Presumed static type: ~S, value: ~S" type obj)
        (hash-set-replace type seen-types)))))

(reset-seen-types)

(declaim (inline static?))
(defun static? (obj)
  (typecase obj
    (number    t)
    (null      t)
    (string    t)
    (character t)
    (cons      nil)
    (symbol    nil)
    (otherwise
     (cond
      ((eq obj t)                   t)
      ((eq obj (%unbound-marker-8)) t)
      ((gvectorp  obj)              nil)
      ((uvectorp  obj)              t)
      ((functionp obj)              nil)
      (t
       (print-unseen-type obj)
       t)))))

#| TESTS
(static? 3)
(static? "a")
(static? #\a)
(static? nil)
(static? t)
(static? '(1 2))
(static? :xyz)

|#


(eval-when (:compile-toplevel :execute)
  
  (defmacro do-function-refs-internal ((var lfun-vector) &body body)
    (let ((i (gensym)))
      `(dotimes (,i (ccl::%count-immrefs ,lfun-vector))
         (let ((,var (ccl::%nth-immediate ,lfun-vector ,i)))
           ,@body))))
  
  (def-accessors () %svref
    nhash.vector.link                     ; GC link for weak vectors
    nhash.vector.flags                    ; a fixnum of flags
    nhash.vector.free-alist               ; empty alist entries for finalization
    nhash.vector.finalization-alist       ; deleted out key/value pairs put here
    nhash.vector.weak-deletions-count     ; incremented when the GC deletes an element
    nhash.vector.hash                     ; back-pointer
    nhash.vector.deleted-count            ; number of deleted entries
    nhash.vector.cache-idx                ; index of last cached key/value pair
    nhash.vector.cache-key                ; cached key
    nhash.vector.cache-value              ; cached value
    )
  
  (defconstant $nhash.vector_overhead 10)
  
  (defconstant $nhash_weak_bit 12)        ; weak hash table
  (defconstant $nhash_weak_value_bit 11)  ; weak on value vice key if this bit set
  (defconstant $nhash_finalizeable_bit 10)
  (defconstant $nhash_weak_both_bit 9)  ; weak on both key and value if this bit set - not supported in MCL 3.0
  
  (defun make-hash-table (&key (test 'eql)
                                  (size 60)
                                  (rehash-size 1.5)
                                  (rehash-threshold .85)
                                  (hash-function nil)
                                  (weak nil)
                                  (finalizeable nil))
    (unless (and test (or (functionp test) (symbolp test)))
      (report-bad-arg test '(and (not null) (or symbol function))))
    (unless (or (functionp hash-function) (symbolp hash-function))
      (report-bad-arg hash-function '(or symbol function)))
    (unless (and (realp rehash-threshold) (<= 0.0 rehash-threshold 1.0))
      (report-bad-arg rehash-threshold '(real 0 1)))
    (unless (or (fixnump rehash-size) (and (realp rehash-size) (< 1.0 rehash-size)))
      (report-bad-arg rehash-size '(or fixnum (real 1 *))))
    (setq rehash-threshold (/ 1.0 (max 0.01 rehash-threshold)))
    (let ((default-hash-function
            (cond ((or (eq test 'eq) (eq test #'eq)) 
                   (setq test 0))
                  ((or (eq test 'eql) (eq test #'eql)) 
                   (setq test -1))
                  ((or (eq test 'equal) (eq test #'equal))
                   (setq test #'equal) #'%%equalhash)
                  ((or (eq test 'equalp) (eq test #'equalp))
                   (setq test #'equalp) #'%%equalphash)
                  ((null hash-function)
                   (error "No hash-function specified for non-standard test"))
                  (t (setq test (require-type test 'symbol))
                     hash-function))))
      (setq hash-function
            (if hash-function
              (require-type hash-function 'symbol)
              default-hash-function)))
    (when (and weak (neq weak :value) (neq test 0))
      (error "Only EQ hash tables can be weak on :key or :both."))
    (when (and finalizeable (not weak))
      (error "Only weak hash tables can be finalizeable."))
    (multiple-value-bind (size total-size)
                         (compute-hash-size (1- size) 1 rehash-threshold)
      (let* ((flags (if weak
                      (+ (ash 1 $nhash_weak_bit)
                         (ecase weak
                           ((t :key) 0)
                           (:value (ash 1 $nhash_weak_value_bit))
                           (:both  (ash 1 $nhash_weak_both_bit)))
                         (if finalizeable (ash 1 $nhash_finalizeable_bit) 0))
                      0))
             (hash (%cons-hash-table 
                    #'%no-rehash hash-function test
                    (%cons-nhash-vector total-size flags)
                    size rehash-threshold rehash-size)))
        (setf (nhash.vector.hash (nhash.vector hash)) hash)
        hash)))

  ;;; This is only used for building a list of all known types.
  (defun our-type-of (obj &optional no-gvector-detail)
    (flet ((complex-type-of (obj)
             (let ((type (type-of obj)))
               (if (consp type)
                 (first type)
                 type))))
      (cond
       ((consp obj)
        'cons)
       ((symbolp obj)
        'symbol)
       ((gvectorp obj)
        (cond
         ((= (%vect-subtype obj) $v_weakh)
          'population)
         ((= (%vect-subtype obj) $v_nhash)
          (let ((flags (nhash.vector.flags obj)))
            (cond
             ((not (logbitp $nhash_weak_bit flags))
              'hash-table-vector-strong)
             ((logbitp $nhash_weak_both_bit flags)
              'hash-table-vector-weak-on-both)
             ((logbitp $nhash_weak_value_bit flags)
              'hash-table-vector-weak-on-value)
             (t
              'hash-table-vector-weak-on-key))))
         (t
          (if no-gvector-detail
            'gvector
            (complex-type-of obj)))))
       ((ccl::%lfun-vector-p obj)
        'lfun-vector)
       (t
        (complex-type-of obj)))))
  
  ;;; Returns T when action is taken
  (declaim (inline iterate-object-contents))
  (defun iterate-object-contents (obj action &optional env)
    (when (functionp obj)
      (setf obj (%lfun-vector obj)))
    (cond
      ((static? obj) nil)
      ((consp obj)
       (funcall action env obj (first obj))
       (funcall action env obj (rest  obj))
       t)
      ((symbolp obj)
       (when (boundp obj)
         (funcall action env obj (symbol-value    obj)))
       (when (fboundp obj)
         (funcall action env obj (symbol-function obj)))
       (funcall action env obj   (symbol-name     obj))
       (funcall action env obj   (symbol-plist    obj))
       #|(funcall action env obj   (symbol-package  obj))|#
       t)
      ((gvectorp obj)
       (cond
        ((= (%vect-subtype obj) $v_weakh)
         ;; population
         nil)
        ((= (%vect-subtype obj) $v_nhash)
         (trace-print (:debug-references-to) ">>> Population ~S" obj)
         ;; hash-table vector
         (loop for index from 0 below (- $nhash.vector_overhead 2)
               do (when (funcall action env obj (uvref obj index))
                    (return-from iterate-object-contents t)))
         (let ((flags (nhash.vector.flags obj)))
           (cond
            ((not (logbitp $nhash_weak_bit flags))
             ;; not weak
             (loop for index from $nhash.vector_overhead below (uvsize obj)
                   do (when (funcall action env obj (uvref obj index))
                    (return-from iterate-object-contents t))))
            ((logbitp $nhash_weak_both_bit flags)
             ;; weak on both key and value - not supported by MCL 3.0
             nil)
            ((logbitp $nhash_weak_value_bit flags)
             ;; weak on value
             (loop for index from $nhash.vector_overhead below (uvsize obj) by 2
                   do (when (funcall action env obj (uvref obj index))
                    (return-from iterate-object-contents t))))
            (t
             ;; weak on key
             (loop for index from (1+ $nhash.vector_overhead) below (uvsize obj) by 2
                   do (when (funcall action env obj (uvref obj index))
                        (return-from iterate-object-contents t))))))
         t)
        (t
         (loop for index from 0 below (uvsize obj)
               do (funcall action env obj (uvref obj index)))
         t)))
      ((ccl::%lfun-vector-p obj)
       (do-function-refs-internal (each obj)
                                  (funcall action env obj each))
       t)
      (t
       (print-unseen-type obj)
       nil)))
  )

(defun map-references-from (func obj
                                     &key
                                     even-if-static?
                                     exception-list)
  (unless (member (type-of obj) exception-list)
    (reset-seen-types)
    (labels ((fuss (env obj ref)
               (declare (ignore env obj))
               (funcall-unless-static func ref even-if-static?)
               nil))
      (iterate-object-contents obj #'fuss))
    (values)))

(defun references-from (obj)
  (let ((result '()))
    (flet ((add-to-result (obj)
             (push obj result)))
      (map-references-from #'add-to-result obj))
    result))

(defun map-all-references-from (func obj
                                     &key
                                     even-if-static?
                                     exception-list)
  (unless (member (type-of obj) exception-list)
    (reset-seen-types)
    (let ((seen-objects (make-hash-set :test #'eq)))
      (labels ((fuss (env obj ref)
                 (declare (ignore env obj))
                 (funcall-unless-static func ref even-if-static?)
                 (unless (hash-set-member ref seen-objects)
                   (hash-set-replace ref seen-objects)
                   (iterate-object-contents ref #'fuss))))
        (iterate-object-contents obj #'fuss)))
    (values)))


;;;==========================================================================

;;; head is assumed to be a cons.
(defun last-or-self-ref (head &optional set)
  (let ((trail (or set (make-hash-set :weak t :test #'eq)))
        (cons head))
    (values (block looky
              (loop
                (let ((next (rest cons)))
                  (hash-set-replace cons trail)
                  (unless next
                    (return-from looky cons))
                  (unless (consp next)
                    (return-from looky cons))
                  (when (hash-set-member cons trail)
                    ;; circular - no tail
                    #| (trace-print () "No tail") |#
                    (return-from looky cons))
                  (setf cons next))))
            trail)))
#| TESTS
(last-or-self-ref nil)
(last-or-self-ref '(a b))
(last-or-self-ref '(a . b))

(defvar *cirque* (cons 1 2))
(defvar *circular-list* (list 1 2 3 4 5 6))
(progn (setf (rest *cirque*) *circular-list*)
       (setf (rest (last *circular-list*)) *cirque*)
       nil)
(let ((*print-length* 3)
      (*print-level* 2))
  (print (last-or-self-ref *circular-list*))
  nil)
|#

(defvar *population-conses* (make-hash-set :weak t :test #'eq))

(defun build-population-conses (&optional storage)
  (let ((set (or storage *population-conses*)))
    (hash-set-clear set)
    (flet ((dopop (obj)
             (when (and (gvectorp obj)
                        (= (%vect-subtype obj) $v_weakh))
               (let ((populist (population-data obj)))
                 (when populist
                   (last-or-self-ref populist set))))))
      (declare (dynamic-extent dopop))
      (%map-dynamic-space #'dopop))
    (trace-print () "population-conses built"))
  (values))

(declaim (inline population-member?))
(defun population-member? (obj)
  (and (consp obj)
       (hash-set-member obj *population-conses*)))
#| TEST
(build-population-conses)
|#

(defvar *list-tails* (make-hash-table :test #'eq :weak :both))

;;; probably broken
(defun build-list-tails ()
  (clrhash *list-tails*)
  (hash-set-clear *population-conses*)
  (gc)
  (build-population-conses *population-conses*)
  (flet ((visit (cons)
           (when (consp cons)
             (unless (hash-set-member (last-or-self-ref cons) *population-conses*)
               (unless (gethash cons *list-tails*)
                 (let ((tail (last-or-self-ref cons)))
                   (loop
                     (setf (gethash cons *list-tails*) tail)
                     (let ((next (rest cons)))
                       (when (or (null next)
                                 (not (consp next)))
                         (return-from visit))
                       (setf cons next))
                     (when (gethash cons *list-tails*)
                       (return)))))))))
    (declare (dynamic-extent visit))
    (%map-dynamic-space #'visit)))
#| TEST
(build-list-tails)
|#

(defun add-to-hash-entry-values (key new-value table)
  (let ((value (gethash-refs key table)))
    (if value
      (push new-value (population-data value))
      (puthash-refs key table (%cons-population (list new-value)))))
  (values))

;;; The sole purpose for this class is to get around the size limit for a hash table.
(defclass refs-hash-table ()
  ((conses :accessor refs-to-conses :initarg conses)
   (others :accessor refs-to-others :initarg others))
  (:default-initargs
   conses (make-hash-table :weak :both :test #'eq)
   others (make-hash-table :weak :both :test #'eq)))

(defmethod clrhash-refs ((table refs-hash-table))
  (with-slots (conses others) table
    (clrhash conses)
    (clrhash others)))

(defmethod gethash-refs (key (table refs-hash-table))
  (with-slots (conses others) table
    (gethash key (if (consp key) conses others))))

(defmethod puthash-refs (key (table refs-hash-table) new-value)
  (with-slots (conses others) table
    (puthash key (if (consp key) conses others) new-value)))

(defvar *refs-to* (make-instance 'refs-hash-table))

#|
We build up a reverse index of who points to whom.
Any reference to or from a cons that is part of a population-list is ignored.
A reference from a cons to another cons is considered uninteresting.
|#
(defun build-table-of-refs-to ()
  (clrhash-refs *refs-to*)
  (hash-set-clear *population-conses*)
  (gc)
  (build-population-conses *population-conses*)
  
  (let ((total 0))
    
    (trace-print () "Counting objects in dynamic space...")
    (flet ((visit (obj)
             (unless (population-member? obj)
               (incf total))))
      (declare (dynamic-extent #'visit))
      (%map-dynamic-space #'visit)
      (trace-print () "Number of dynamic objects to do: ~:D" total))
    
    (with-progress-dialog (dialog)
      (let (items-per-step
            counter
            (number-done 0))
        (setf items-per-step (ceiling (/ total (progress-dialog-steps dialog))))
        (setf counter items-per-step)
        
        (labels ((addit (env obj ref)
                   (unless (population-member? ref)
                     (unless (and (consp ref)
                                  (consp obj))
                       (add-to-hash-entry-values ref obj env))))
                 (visit (obj)
                   (unless (population-member? obj)
                     (when (zerop counter)
                       (incf number-done items-per-step)
                       (progress-dialog-position-increment dialog)
                       (event-dispatch)
                       (setf counter items-per-step))
                     (decf counter)
                     (iterate-object-contents obj #'addit *refs-to*))))
          (declare (dynamic-extent #'addit #'visit))
          (when-unwind
            (%map-dynamic-space #'visit)
            (clrhash-refs *refs-to*)
            )))))
  (hash-set-clear *population-conses*)
  (values))
#| TEST
(room)
(build-table-of-refs-to)
|#

;;; First we build up a weak set of all the conses that belong to populations.
;;; Then we filter out anything that's in that set.
(defun map-references-to (func target)
  (if (or (null target)
          (eq   target t))
    (trace-print () "The target is ~s" target)
    (let ((referencers (let ((ref-pop (gethash-refs target *refs-to*)))
                         (and ref-pop (population-data ref-pop)))))
      (if referencers
        (dolist (obj referencers)
          (funcall func obj))
        (progn
          (reset-seen-types)
          (let ((population-items (make-hash-set :weak t :test #'eq)))
            (labels ((check (env obj ref)
                       (declare (ignore env))
                       (and (eq target ref)
                            (progn
                              (funcall func obj)
                              t)))
                     (mapper1 (obj)
                       (when (and (gvectorp obj)
                                  (= (%vect-subtype obj) $v_weakh))
                         (let ((data (population-data obj)))
                           (if (consp data)
                             (loop for eachCons on data
                                   doing (hash-set-replace eachCons population-items))))))
                     (mapper2 (obj)
                       (unless (hash-set-member obj population-items)
                         (iterate-object-contents obj #'check))))
              (declare (dynamic-extent #'mapper1 #'mapper2))
              (%map-dynamic-space #'mapper1)
              (%map-dynamic-space #'mapper2)))))))
  (values))

(defun references-to (obj)
  (or (let ((ref-pop (gethash-refs obj *refs-to*)))
        (and ref-pop (population-data ref-pop)))
      (let ((result '()))
        (flet ((add-to-result (obj)
                 (push obj result)))
          (map-references-to #'add-to-result obj))
        result)))

;;; Starting from a given root, find all paths to target.
;;; The result is returned as a hash table:
;;;   key:   an object that references target
;;;   value: a list of lists, each of which is a path from the referencer back to the root
(defun find-rooted-references-to (target root)
  (reset-seen-types)
  (let ((found-items (make-hash-table :test #'eq))
        (stack '()))
    (labels ((check (env obj ref)
               (declare (ignore env))
               (cond
                ((eq target ref)
                 (let ((entry (gethash obj found-items))
                       (path (append (list obj) (copy-list stack))))
                   (if entry
                     (progn
                       (trace-print (:debug-references-to) "eq another ~S ~S" ref path)
                       (push path entry)
                       t)
                     (progn
                       (trace-print (:debug-references-to) "eq new ~S ~S" ref path)
                       ;; add it and continue iterating over obj's contents
                       (setf (gethash obj found-items) (list path))
                       nil))))
                ((member ref stack)
                 (trace-print (:debug-references-to) "seen ~S" ref)
                 nil)
                (t
                 (push obj stack)
                 (mapper ref)
                 (pop stack)
                 nil)))
             (mapper (obj)
               (trace-print-bracketing ((list "mapper of ~S" obj) :debug-references-to)
                 (iterate-object-contents obj #'check))))
      (declare (dynamic-extent #'check #'mapper))
      (mapper root))
    found-items))

(defun map-rooted-references-to (func target root)
  (maphash #'(lambda (key value)
               (declare (ignore value))
               (funcall func key))
           (find-rooted-references-to target root))
  (values))

(defun rooted-references-to (target root &key all-paths)
  (let ((result '()))
    (maphash #'(lambda (key value)
                 (if all-paths
                   (push (list key value) result)
                   (push key result)))
             (find-rooted-references-to target root))
    result))

(defun make-map-of-rooted-space (root)
  (reset-seen-types)
  (let ((references-from (make-hash-table :weak :key :test #'eq)))
    (labels ((check (refs obj ref)
               (declare (ignore obj))
               (unless (static? ref)
                 (push ref refs)
                 (mapper ref)))
             (mapper (obj)
               (unless (static? obj)
                 (trace-print-bracketing ((list "mapping ~S" obj) :debug-references-to)
                   (if (gethash obj references-from)
                     (trace-print (:debug-references-to) "seen ~S" obj)
                     (let ((refs '()))
                       (iterate-object-contents obj #'check refs)
                       (when refs
                         (trace-print (:debug-references-to) "new  ~S" obj)
                         ;; add it and continue iterating over obj's contents
                         (trace-print (:debug-references-to) "references from ~S to ~S" obj refs)
                         (setf (gethash obj references-from) refs))))))))
        (declare (dynamic-extent #'mapper))
        (mapper root))
      (let ((references-to (make-hash-table :weak :key :test #'eq)))
        (maphash #'(lambda (key value)
                     (trace-print-bracketing ((list "doing ~S" key) :debug-references-to)
                       (dolist (obj value)
                         (let ((refs (or (gethash obj references-to)
                                         (let ((new-refs '()))
                                           (setf (gethash obj references-to) new-refs)
                                           new-refs))))
                           (push key refs)))))
                 references-from)
        (values references-from
                references-to))))

(defun map-of-rooted-space (root)
  (multiple-value-bind (references-from references-to)
                       (make-map-of-rooted-space root)
    (let ((from '())
          (to   '()))
      (maphash #'(lambda (key value)
                   (when value
                     (push (list key value) from)))
               references-from)
      (maphash #'(lambda (key value)
                   (when value
                     (push (list key value) to)))
               references-to)
      (values from to))))

#| Test map-references-from, etc.

(defvar refsTo nil)
(defvar refsFrom nil)
(multiple-value-setq (refsFrom refsTo) (map-of-rooted-space sk8::Object))

(defparameter *print-leaves* nil)
(setf         *print-leaves*    nil)
(setf         *print-leaves*    t)

(defun test-map-references-from (thing)
  (map-references-from #'(lambda (obj)
                      (format t "~&======")
                      (print obj)
                      (map-references-from #'print obj :even-if-static? *print-leaves*)
                      )
                  thing
                  :even-if-static? *print-leaves*))

(defvar *c1* (cons 1 2))
(defvar *c2* (cons 2 3))
(defun *c1* () (print "hi"))
(test-map-references-from '*c1*)
(test-map-references-from *c1*)
(test-map-references-from #'*c1*)
(test-map-references-from (%lfun-vector #'*c1*))

(defvar *t1* nil)
(setf *t1* (make-hash-table))
(setf (gethash *c1* *t1*) *c2*)
(test-map-references-from *t1*)
(let ((*print-length* 3)
      (*print-level* 2))
  (rooted-references-to *c1* *t1* :all-paths t))

(defvar *t2* (make-hash-table :weak :value))
(setf (gethash *c1* *t2*) 5)
(test-map-references-from *t2*)

(defvar *t3* (make-hash-table :weak :key :test #'eq))
(setf (gethash *c1* *t3*) *c2*)
(test-map-references-from *t3*)

(defvar *c3* (list *c1* 42))
(defvar *c4* (list *c3* *c1*))
(push *c4* *c3*)
(defvar *c5* (list *c3* *c4*))

(rooted-references-to *c1* 3    :all-paths t)
(rooted-references-to *c1* t    :all-paths t)
(rooted-references-to *c1* nil  :all-paths t)
(rooted-references-to *c1* *c5*)
(rooted-references-to *c1* *c5* :all-paths t)
(map-of-rooted-space *c5*)

(gvectorp (find-package :cl))
(type-of (find-package :cl))

(map-references-from #'print (%cons-population '(*something* 2)) :even-if-static? t)
(map-references-from #'print '(*something* 2) :even-if-static? t)

|#

;;; Goes over all of dynamic space, calling func on each 
;;; object therein.
(defun %map-dynamic-space (func)
  (setq func (coerce-to-function func))
  ;; asave0: address of function
  ;; atemp0: scanning address 
  ;; atemp1: next address
  ;; dsave0: scanning address limit
  ;; da:     value at scanning address
  ;; db:     quicky temp
  (lap-inline ()
    (:variable func)
    (with-preserved-registers #(dsave0 asave0)
      (move.l (varg func 8) asave0)
      (move.l nilreg arg_z)
      (jsr_subprim $sp-consZnil)
      (move.l acc dsave0)
      (move.l (a5 $Pdynamic_cons_area) atemp1)
      (move.l (atemp1 $cons-area.gspace-start) atemp0)
      (prog#
       (move.l @atemp0 da)
       (move.l atemp0 atemp1)
       (if# (ne (progn (header-p da db)))
         (add ($ $t_cons) atemp0)
         (add.l ($ 8) atemp1)
         
         elseif# (eq (cmp.w ($ $symbol-header) da))
         (add ($ $t_symbol) atemp0)
         (add.l ($ $sym_size) atemp1)
         else#
         (add ($ $t_vector) atemp0)
         (header-length da db)
         (add.l ($ 11) db)
         (and.b ($ (lognot 7)) db)
         (add.l db atemp1))
       (vpush atemp1)
       (move.l atemp0 arg_z)
       (set_nargs 1)
       (jsr @asave0)
       (vpop atemp0)
       (until# (geu dsave0 atemp0)))))
  nil)


;;; Maps func over the objects that reference the given object.
(defun map-references-to-dons-original-version (func target)
  (labels ((mapper (ob)
             (when (ccl::%lfun-vector-p ob)
               (setq ob (ccl::%lfun-vector-lfun ob)))
             (when
               (cond ((consp ob)
                      (or (eq target (car ob))
                          (eq target (cdr ob))))
                     ((symbolp ob)
                      (or (and (boundp ob) (eq target (symbol-value ob)))
                          (and (fboundp ob) (eq target (symbol-function ob)))))
                     ((macptrp target) nil)
                     ((uvectorp ob)
                      (dotimes (i (uvsize ob))
                        (when (eq target (uvref ob i)) (return t))))
                     ((ccl::%lfun-vector-p ob)
                      (and (not (eq ob #'mapper))
                           (do-function-refs-internal (each ob)
                                                      (when (eq each target)
                                                        (return t))))))
               (funcall func ob))))
    (declare (dynamic-extent #'mapper))
    (%map-dynamic-space #'mapper))
  nil)


;;; The purpose of this is to do a version of map-references-to that does 
;;; something heroic with a cons.  Ie., it tries to shimmy up the list by 
;;; finding all the references to the each cons cell.

(defun map-references-to-don2 (func target)
  (if (or (null target)
          (eq   target t))
    (trace-print () "The target is ~s" target)
    (progn (labels ((mapper (ob)
                      (when (ccl::%lfun-vector-p ob)
                        (setq ob (ccl::%lfun-vector-lfun ob)))
                      (if (consp ob)
                        (cond ((eq target (car ob))
                               (funcall func (find-list-head ob)))
                              ((eq target (cdr ob))
                               (funcall func ob))))
                      (when (cond 
                             ((symbolp ob)
                              (or (and (boundp ob) (eq target (symbol-value ob)))
                                  (and (fboundp ob) (eq target (symbol-function ob)))))
                             ((macptrp target) nil)
                             ((uvectorp ob)
                              (dotimes (i (uvsize ob))
                                (when (eq target (uvref ob i)) (return t))))
                             ((ccl::%lfun-vector-p ob)
                              (and (not (eq ob #'mapper))
                                   (do-function-refs-internal (each ob)
                                     (when (eq each target)
                                       (return t))))))
                        (funcall func ob))))
             (declare (dynamic-extent #'mapper))
             (%map-dynamic-space #'mapper))
           nil)))


;;; Yuck, actually have to scan through once for each cons because
;;; of cases where multiple lists share the same cons, or circular 
;;; lists.
;;;
;;; (Well we don't *have* to, but I'm not in the mood to do some
;;; hairy caching optimization dealie right now.)

;;; Later... okay, here's a good contract:
;;; Returns the longest non-shared list.
(defun find-list-head (our-cons)
  (labels ((list-head-mapper (this-cons)
             (let ((found 0))
               (labels ((mapper (ob)
                          (when (and (typep ob 'cons)
                                     (eq (cdr ob) this-cons))
                            (incf found)
                            (list-head-mapper ob))))
                 (%map-dynamic-space #'mapper))
               (unless (= found 1) (return-from find-list-head this-cons))))
           (list-head-mapper-top (ob)
             (when (and (typep ob 'cons)
                        (eq (cdr ob) our-cons))
               (funcall #'list-head-mapper ob))))
    (%map-dynamic-space #'list-head-mapper-top)))



;;; As don2, but follows lists several orders of magnitude faster.
;;; Two passes are necessary.  The first one finds the actual cons,
;;; the second finds the list.

;;; Works but not that much faster.

(defun map-references-to-don3 (func target)
  (labels ((mapper (ob)
             (when (ccl::%lfun-vector-p ob)
               (setq ob (ccl::%lfun-vector-lfun ob)))
             (if (consp ob)
               (cond ((eq target (car ob))

                      (funcall func (find-list-head ob)))
                     ((eq target (cdr ob))
                      (funcall func ob))))
             (when (cond 
                    ((symbolp ob)
                     (or (and (boundp ob) (eq target (symbol-value ob)))
                         (and (fboundp ob) (eq target (symbol-function ob)))))
                    ((macptrp target) nil)
                    ((uvectorp ob)
                     (dotimes (i (uvsize ob))
                       (when (eq target (uvref ob i)) (return t))))
                    ((ccl::%lfun-vector-p ob)
                     (and (not (eq ob #'mapper))
                          (do-function-refs-internal (each ob)
                            (when (eq each target)
                              (return t))))))
               (funcall func ob))))
    (declare (dynamic-extent #'mapper))
    (%map-dynamic-space #'mapper))
  nil)


(defvar *flhf-ht* (make-hash-table))


;;; "List heads, list heads, rolly polly list heads.
;;;  List heads, list heads, eat them up, yummm."
;;;
;;; Find the heads of lists containing our object, ignoring sublists.
(defun map-list-heads-referencing (func ob)
  (let ((ht *flhf-ht*))
    (clrhash ht)
    (labels ((flhf-mapper (each-ob)
               (flet ((mark-it ()
                        (setf (gethash ob ht) t)
                        (return-from flhf-mapper)))
                 (when (consp each-ob)
                   ;; Most lists are real tiny.  Take care of them first.
                   (loop for (each-car . each-cdr) on each-ob
                         for count below 250
                         doing (cond ((eq ob each-car)
                                      (mark-it))
                                     ((null each-cdr)
                                      (return-from flhf-mapper))
                                     ((not (consp each-cdr))
                                      (if (eq ob each-cdr)
                                        (mark-it)
                                        (return-from flhf-mapper)))))
                   ;; Get serious on the rest.
                   (map-avoiding-circularities 
                    #'(lambda (each-car)
                        (when (eq ob each-car)
                          (mark-it)))
                    each-ob)))))
      (%map-dynamic-space #'flhf-mapper)
      (format t "~%finished flhf-mapper")
      ;; Nah, this won't be confusing.
      (maphash #'(lambda (key1 val1)
                   (declare (ignore val1))
                   (maphash #'(lambda (key2 val2)
                                (when (and val2
                                           (neq key2 key1)
                                           (member key2 key1 :key #'cdr))
                                  (setf (gethash key2 ht) nil)))
                            ht))
               ht)
      (format t "~%finished maphash")
      (maphash #'(lambda (key val)
                   (when val
                     (funcall func key)))
               ht)
      (clrhash ht)))
  nil)


(defun length-dot (l)
  (loop for count from 1
        for each-cdr = (cdr l) then (cdr each-cdr)
        doing (cond ((null each-cdr)
                     (return count))
                    ((not (consp each-cdr))
                     (return))
                    ((eq each-cdr l)
                     (return)))
        finally (return count)))
                    

(defun longest-list ()
  (let (longest-list
        (longest-length 0))
    (labels ((mapper (ob)
               (when (consp ob)
                 (let ((length (length-dot ob)))
                    (when (and length (> length longest-length))
                      (setf longest-length length)
                      (setf longest-list ob))))))
      (%map-dynamic-space #'mapper)
      longest-list)))
                             
(defun longest-list-excepting (e)
  (let (longest-list
        (longest-length 0))
    (labels ((mapper (ob)
               (when (consp ob)
                 (let ((length (length-dot ob)))
                   (when (and length (> length longest-length))
                     (unless (loop for each on e
                                   thereis (eq ob each))
                       (setf longest-length length)
                       (setf longest-list ob)))))))
      (%map-dynamic-space #'mapper)
      longest-list)))                                       




(defvar *mac-ht* (make-hash-table))

;;; Calls func on every car of the listt.
;;; If the list is circular, stops at the point of circularity.
;;; If the list is dotted, calls the func on the last cdr.
(defun map-avoiding-circularities (func listt)
  (let ((ht *mac-ht*))
    (clrhash ht)
    (loop for sublist on listt
          as (each-car . each-cdr) = sublist
          doing
          (funcall func each-car)
          (cond ((gethash each-cdr ht)
                 (return))
                ((null each-cdr)
                 (return))
                ((atom each-cdr)
                 (funcall func each-cdr)
                 (return))
                (t
                 (setf (gethash sublist ht) t))))
    (clrhash ht))
  nil)



(defun print-references-from (obj)
  (let ((*print-length* 4)
        (*print-level*  3))
    (map-references-from #'print obj)
    (values)))

(defun nth-reference-from (n obj)
  (let ((*print-length* 4)
        (*print-level*  3))
    (map-references-from #'(lambda (a)
                             (if (zerop n)
                               (return-from nth-reference-from a)
                               (decf n)))
                         obj)
    (values)))



;;; Note: these use Don's original, cuz I want them to.
(defun print-references-to (obj)
  (let ((*print-length* 4)
        (*print-level*  3))
    (map-references-to-don2 #'print obj)))


(defun nth-reference-to (n obj)
  (map-references-to-don2 #'(lambda (a)
                                               (if (zerop n)
                                                 (return-from nth-reference-to a)
                                                 (decf n)))
                                           obj))

#| Test map-references-to

;;; If it's a list or a vector, just tell us; don't print the whole damn thing.
(defun print-simple (obj)
  (typecase obj
    (list
     (print 'a_list))
    (vector
     (print 'a_vector))))

(progn
  (defvar *gt1* '(3))
  #|
  (defvar *gt1-list* (list *g1t*))
  (defvar *gt1-hash-table* (make-hash-table))
  (setf (gethash *gt1* *gt1-hash-table*) t)
  |#
  nil
  )
;;; Something's wrong here:
(map-references-to #'print-simple *gt1*)

(SK8:new SK8:Object :project SK8:SK8 :objectName "Orphan")
(map-references-to #'print sk8::Orphan)

|#

#| See what's inside a hash-table

(def-accessors (hash-table) %svref
  nil                                   ; 'HASH-TABLE
  nhash.rehashF                         ; function: rehashes if necessary
  nhash.keytransF                       ; transform key into (values primary addressp)
  nhash.compareF                        ; comparison function: 0 -> eq, -1 ->eql, else function
  nhash.rehash-bits                     ; bitset (array (unsigned-byte 32)) for rehash
  nhash.vector                          ; N <key,value> pairs; n relatively prime to & larger than all secondary keys
  nhash.lock                            ; fixnum: count of mappers in low 16 bits + bits for grow and rehash
  nhash.count                           ; Number of entries
  nhash.locked-additions                ; Alist to look in when table locked & not otherwise found
  nhash.fixnum                          ; (a5 $fwdnum)
  nhash.gc-count                        ; (a5 $relocnum) - incremented by full gc, growzone.
  nhash.grow-threshold                  ; Max # entries before grow
  nhash.rehash-ratio                    ; inverted rehash-threshold
  nhash.rehash-size			; rehash-size from user
  nhash.puthash-count                   ; number of times table has been rehashed or grown
  )

(defvar xhasht (make-hash-table :weak :value))
(let ((thevect (nhash.vector xhasht)))
  (print (uvsize thevect))
  (loop for index from 1 below (uvsize thevect)
        do (let ((theValue (uvref thevect index)))
             (unless (eq theValue (%unbound-marker-8))
               (print theValue)))))

|#

#| Find all the types in dynamic space

(defun compare-symbols (s1 s2 srting-compare-function)
  (apply srting-compare-function (list (symbol-name s1) (symbol-name s2))))

(defun make-symbol-compare-function (srting-compare-function)
  #'(lambda (s1 s2)
      (compare-symbols s1 s2 srting-compare-function)))

(defvar *all-types* nil)
(setf *all-types* (make-hash-set))

(%map-dynamic-space #'(lambda (obj)
                        (let ((type (our-type-of obj t)))
                          (hash-set-replace (if (listp type) (first type) type)
                                            *all-types*))))

(setf *all-types* (sort (hash-set-list *all-types*) (make-symbol-compare-function #'string<)))

(length *all-types*)
(progn
  (mapc #'print *all-types*)
  (values))

|#

#|

(defun build-type-histogram ()
  (hash-set-clear *population-conses*)
  (gc)
  (build-population-conses)

 (let ((buckets (make-hash-table)))
    
    (flet ((visit (obj)
             (unless (population-member? obj)
               (let* ((type (our-type-of obj))
                      (bucket (gethash type buckets)))
                 (if bucket
                   (incf (first bucket))
                   (setf (gethash type buckets) (list 1)))))))
      (declare (dynamic-extent #'visit))
      (%map-dynamic-space #'visit)

      (let ((result '()))
        (maphash #'(lambda (key value)
                     (push (list (first value) key) result))
                     buckets)
        (sort result #'(lambda (d1 d2)
                         (< (first d1)
                            (first d2))))
        result))))

(dolist (item (build-type-histogram))
  (format t "~7:D ~A~&" (first item) (second item)))
|#

#|

(map-references-from #'print (SK8::new sk8::object :project SK8::sk8))



|#

(export '(map-references-from
          references-from
          map-references-to
          references-to
          map-rooted-references-to
          rooted-references-to
          ))

#|
	Change History (most recent last):
	1  	 7/ 5/95	dy      	
	2  	 7/ 5/95	dy      	
	3  	 7/17/95	dy      	Abstract into new iterate-object-contents macro
						New version of map-references-to that is 2-pass
						find-rooted-references-to, map-of-rooted-space and friends
						Export some stuff
	4  	 7/17/95	dy      	Use new static? function instead of atom.
	5  	 7/17/95	till    	messin' around
	6  	 7/17/95	dy      	Change iterate-object-contents from a macro to a function.
	7  	 7/25/95	dy      	Make static? work correctly
	8  	 7/25/95	dy      	use the new print-unseen-type function
	9  	 7/26/95	Till    	add print-references-to, nth-reference
	10 	 7/28/95	Till    	map-references-to-don2 - but it isn't tested
	11 	 7/28/95	dy      	Added references-to and references-from
	12 	 7/31/95	Till    	map-references-to-don2 and find-list-head.
	13 	 8/ 2/95	Till    	Assorted stuff after "shimmy".
	14 	 8/ 2/95	dy      	inserted test-reap stuff at top; use don2 versions in print-blah functions
	15 	 8/ 2/95	dy      	print-references-from
	17 	 8/11/95	dy      	hash-set stuff moved from here.
						Has build-table-of-refs-to function.
						Requires c32.
	18 	 8/14/95	dy      	print-short function.
						enhanced our-type-of
	19 	 8/14/95	dy      	some cleanup
	20 	 8/14/95	dy      	Work around hash-table size limit
	21 	 8/14/95	Till    	fix map-references-to make-hash-set call
	22 	 8/14/95	dy      	
	2  	 2/27/97	Hernan  	
|# ;(do not edit past this line!!)
