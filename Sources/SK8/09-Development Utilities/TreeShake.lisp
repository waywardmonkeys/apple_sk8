;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories

(in-package :cl-user)

;;; can remove the ui with (closeproject ui) and add that format thingie.

;;; We want debugging ON unless compiling as a patch for SK8 0.9
(when (features-active? :ccl-3)
  (insert-features :debug-treeshake)
  )

(in-package :ccl)

(defun %proclaim-notconstant (sym)
  (old-lap-inline ()
    (move.l acc atemp0)
    (jsr_subprim $sp-check-sym)
    (bclr ($ $sym_bit_const) (atemp0 $sym.vbits))))

(defun remove-package (package-name)
  (trace-print-bracketing ((list "removing package ~A" package-name) :debug-treeshake)
    (let ((pkg (find-package package-name)))
      (do-symbols (v pkg)
        (when (eq (symbol-package v) pkg)
          (when (constantp v)
            (%proclaim-notconstant v))
          (setf (symbol-value v) nil)
          (setf (symbol-plist v) nil)
          (fmakunbound v)
          (unintern v pkg))))))

(in-package :cl-user)







;;;=======================================================================

(defun do-with-appropriate-output (function output &optional (if-exists :supersede))
  (unless if-exists
    (setf if-exists :supersede))
  (cond
   ((or (stringp   output)
        (pathnamep output))
    (catch :cancel
      (when (and (neq if-exists :supersede)
                 (neq if-exists :append)
                 (probe-file output))
        (setf output (ccl::if-exists :dialog output "Save names to:")))
      (with-open-file (output-stream output
                                     :direction :output
                                     :if-exists if-exists
                                     :if-does-not-exist :create)
        (funcall function output-stream))))
   (output
    (funcall function output))
   (t
    )))

;;; If destination is non-nil, prints out a list of names of reaped objects to that stream.
(defmacro with-appropriate-output ((output-stream destination &optional if-exists) &body body)
  `(do-with-appropriate-output ,`#'(lambda (,output-stream) ,@body) ,destination ,if-exists))
#| TEST
(with-appropriate-output (output "ccl:abcdefg" :supersede)
  (format output "~D~&" 42))
(with-appropriate-output (output "ccl:abcdefg" :append)
  (format output "~D~&" "appended"))
|#

;;;=======================================================================

(defun compare-symbols (s1 s2 srting-compare-function)
  (apply srting-compare-function (list (symbol-name s1) (symbol-name s2))))

(defun make-symbol-compare-function (srting-compare-function)
  #'(lambda (s1 s2)
      (compare-symbols s1 s2 srting-compare-function)))

#|
(sort (package-shadowing-symbols :sk8dev) (make-symbol-compare-function #'string<))
|#

;;;=======================================================================

(defun package-empty? (package)
  (unless (eq (type-of package) 'package)
    (setf package (find-package package)))
  (do-symbols (sym package)
    (when (eq (symbol-package sym) package)
      (return-from package-empty? nil)))
  t)

(defun symbol-shadowing? (theSym package)
  (dolist (sym (package-shadowing-symbols package))
    (when (eq sym theSym)
      (return-from symbol-shadowing? t)))
  nil)

(defun symbol-properties (theSym &optional package)
  (let ((myPackage (symbol-package theSym))
        (result '()))
    (unless package
      (setf package myPackage))
    (when (eq myPackage package)
      (push :interned result))
    (dolist (sym (package-shadowing-symbols package))
      (when (eq sym theSym)
        (push :shadowing result)
        (return)))
    (unless (do-external-symbols (sym package)
              (when (eq sym theSym)
                (push :external result)
                (return t)))
      (do-symbols (sym package)
        (when (eq sym theSym)
          (push :present result)
          (return))))
    (append (list (package-name myPackage)) result)))

(defun find-symbol-in-all-packages (name)
  (setf name (string-upcase name))
  (loop for package in (list-all-packages-sorted)
        collect (list (find-symbol name package) package)))

(defun print-package-symbols (package-list &optional (theStream nil theStreamSet))
  (setf package-list (mklist package-list))
  (dolist (package package-list)
    (let ((package-object (find-package package)))
    (let ((symbol-list '()))
      (do-symbols (sym package)
        (push sym symbol-list))
      #|TEST (setf symbol-list (subseq symbol-list 0 20))|#
      (setf symbol-list (sort symbol-list (make-symbol-compare-function #'string<)))
      (dolist (sym symbol-list)
        (when (eq (symbol-package sym) package-object)
          (format (or (and theStreamSet theStream) t) "~A~&" sym)))))))

(defun print-symbols-to-file (package-list &optional (theFilename t))
  (with-appropriate-output (theStream theFilename)
    (print-package-symbols package-list theStream)))

#| TESTS
(print-symbols-to-file '(:glisp) "ccl:a symbol-test")
(print-symbols-to-file '(:MinimalProject))
|#

(defun sort-package-list (package-list)
  (sort package-list
        #'(lambda (p1 p2)
            (or (null (package-use-list p1))
                (member p1 (package-use-list p2))
                    (< (length (package-use-list p1))
                       (length (package-use-list p2)))))))

(defun list-all-packages-sorted ()
  (sort-package-list (list-all-packages)))

;;; Put new-value into set or if new-value is already in other-set, then use the value found there instead.
(defun hash-set-replace-using-other-set (new-value set &optional other-set)
  (if other-set
    (multiple-value-bind (was-present other-value)
                         (hash-set-member new-value other-set)
      (hash-set-replace (if was-present
                        other-value
                        new-value)
                      set))
    (hash-set-replace new-value set)))

;;;=======================================================================

(defun do-with-sk8object-children-weakened (body-function)
  (let ((all-named-children (make-hash-table :test #'eq :weak :key))
         (number-of-objects-stashed  0)
         (number-of-children-stashed 0))
     (unwind-protect
       (progn
         (flet ((stashKnownChildren (theObject)
                  (let ((knownChildren (mf::slot-value-if-exists theObject 'sk8dev::knownChildren)))
                    (when knownChildren
                      ; (trace-print (:debug-treeshake) "~S" theObject)
                      (incf number-of-objects-stashed)
                      (setf (gethash theObject all-named-children)
                            (ccl::%cons-population knownChildren))
                      (let ((unnamedKnownChildren '()))
                        (dolist (obj knownChildren)
                          (if (sk8:objectName obj)
                            (incf number-of-children-stashed)
                            (push obj unnamedKnownChildren)))
                        (setf (sk8dev::knownChildren theObject) unnamedKnownChildren))))))
           ;; Stash knownChildren lists from all objects in a weak hashtable, and
           ;; remove named objects from all knownChildren lists.
           (ccl::trace-print-bracketing ("Stash knownChildren" :debug-treeshake)
             (dolist (obj (reverse (sk8:knownDescendants sk8:Object)))
               (stashKnownChildren obj))
             (stashKnownChildren sk8:Object)
             (trace-print (:debug-treeshake) "Stashed ~:D known children from ~:D objects"
                          number-of-children-stashed
                          number-of-objects-stashed)))
         
         (funcall body-function))
       
       (ccl::trace-print-bracketing ("Restore knownChildren" :debug-treeshake)
         (let ((number-of-children-restored 0))
           (flet ((restoreKnownChildren (key value)
                    (let ((knownChildren (ccl::population-data value)))
                      (when knownChildren
                        (incf number-of-children-restored (- (length knownChildren) (length (sk8:knownChildren key))))
                        (setf (sk8:knownChildren key) (ccl::population-data value))))))
             (maphash #'restoreKnownChildren all-named-children)
             (trace-print (:debug-treeshake) "Reaped ~:D known children"
                          (- number-of-children-stashed number-of-children-restored))))))))

(defmacro with-sk8object-children-weakened (() &body body)
  `(do-with-sk8object-children-weakened ,`#'(lambda() ,@body)))

;;;=======================================================================

;;; Returns population with all the stuff necessary to reconstitute
;;; a hashtable to it's original state.
;;; The population's data list looks like:
;;;  (:key key1 :val val1  :key key2 :val val2 ...)
;;; Any key or val might be missing from the list after gc.
(defun weaken-hash-table (table)
  (and table
       (let* ((populist '()))
         (maphash #'(lambda (key value)
                      (push  value populist)
                      (push :val   populist)
                      (push  key   populist)
                      (push :key   populist)
                      )
                  table)
         (clrhash table)
         #| (clear-hashtable-cache table) |#  ; not necessary; done by clrhash, I think.
         (ccl::%cons-population populist))))

;;; returns the number of deleted symbols.
(defun unweaken-hash-table (table population)
  (if table
    (let ((populist (ccl::population-data population))
          (count-removed-objects 0)
          (count-total-objects   0))
      (loop
        (unless (pop populist) ; pop :key
          (return))
        (incf count-total-objects)
        (let ((key (pop populist)) ; pop the key or :val
              value)
          (unless (eq key :val)
            (pop populist)) ;; pop :val
          (setf value (first populist))
          (unless (eq value :key)
            (pop populist)) ; pop the value
          (if (or (eq key   :val)
                  (eq value :key))
            ;; entry is invalid
            (incf count-removed-objects)
            ;; entry is valid
            (setf (gethash key table) value))))
      (values count-removed-objects
              count-total-objects))
    (values 0 0)))

(let ((table-populations '()))
  
  (defun weaken-hash-table-list (hash-table-list)
    (setf table-populations '())
    (dolist (table hash-table-list)
      (push (weaken-hash-table table) table-populations)))
  
  (defun unweaken-hash-table-list (hash-table-list)
    (dolist (table (reverse hash-table-list))
      (multiple-value-bind (reaped total)
                           (unweaken-hash-table table (pop table-populations))
        (trace-print (:debug-treeshake) "Reaped ~5:D of ~5:D from ~A" reaped total table)))
    (setf table-populations '())))

(defun do-with-hash-tables-weakened (hash-tables body-function)
  (unwind-protect 
     (progn
       (ccl::trace-print-bracketing ("Weakening hash-tables" :debug-treeshake)
         (weaken-hash-table-list hash-tables))
       (funcall body-function))
     (ccl::trace-print-bracketing ("Unweakening hash-tables" :debug-treeshake)
       (unweaken-hash-table-list hash-tables))))

(defmacro with-hash-tables-weakened ((hash-tables) &body body)
  `(do-with-sk8object-children-weakened ,hash-tables ,`#'(lambda() ,@body)))

;;;=======================================================================

;;; Reuse strings from previous-names if equal
(defun project-symbol-names (proj &optional previous-names)
  (let ((names (make-hash-set :test #'equal)))
    (maphash #'(lambda (key value)
                 (declare (ignore value))
                 (when (symbolp key)
                   (hash-set-replace-using-other-set (symbol-name key) names previous-names)))
             (sk8:objectTable proj))
    names))
#| TESTS
(let* ((names1 (project-symbol-names sk8:UI))
       (names2 (project-symbol-names sk8:UI names1)))
  (hash-set-list (hash-set-exclusive-or names1 names2))
  ;; result should be nil
  )
|#

(let ((project-stashes '()))
  
  (defun weaken-project-list (project-list)
    (setf project-stashes '())
    (dolist (proj project-list)
      (let ((names (project-symbol-names proj)))
        (push (list names
                    (weaken-hash-table (sk8:objectTable proj)))
              project-stashes))))
  
  (defun unweaken-project-list (project-list output)
    (let ((if-exists :supersede))
      (dolist (proj (reverse project-list))
        (let* ((project-stash    (pop project-stashes))
               (names-before  (first  project-stash))
               (population    (second project-stash)))
          (multiple-value-bind (reaped total)
                               (unweaken-hash-table (sk8:objectTable proj) population)
            (trace-print (:debug-treeshake) "Reaped ~5:D of ~5:D from ~A" reaped total proj))
          (do-with-appropriate-output #'(lambda (str)
                                          (ignore-errors
                                           (format str ">>> Project ~A~(~%~)"
                                                   proj
                                                   (sort (hash-set-list
                                                          (hash-set-difference
                                                           names-before
                                                           (project-symbol-names proj names-before)))
                                                         #'string<)))
                                          (setf if-exists :append))
                                      output if-exists)
          (setf project-stashes '()))))))

(defun do-with-project-objectTables-weakened (projects body-function output-stream)
  (setf projects (mklist projects))
  (unwind-protect 
    (progn
      (ccl::trace-print-bracketing ("Weakening projects" :debug-treeshake)
        (ccl::trace-print () "projects: ~s" projects)
        (weaken-project-list projects))
      (funcall body-function))
    (ccl::trace-print-bracketing ("Unweakening projects" :debug-treeshake)
      (unweaken-project-list projects output-stream))))

;;; If output-stream is non-nil, prints out a list of names of reaped objects to that stream.
(defmacro with-project-objectTables-weakened ((projects &optional output-stream) &body body)
  `(do-with-project-objectTables-weakened ,projects ,`#'(lambda() ,@body) ,output-stream))

#|
(with-project-objectTables-weakened ((sk8:knownChildren sk8:project) t)
  (gc))
|#

;;;=======================================================================

;; Returns a hash-set of names of all classes (as strings).
(defun all-class-names (&optional previous-names)
  (let ((names (make-hash-set :test #'equal)))
    (maphash #'(lambda (key value)
                 (declare (ignore value))
                 (hash-set-replace-using-other-set (format nil "~A::~A"
                                                           (package-name (symbol-package key))
                                                           (symbol-name key))
                                                   names
                                                   previous-names))
             ccl::%find-classes%)
    names))
#| TESTS
(let* ((names1 (all-class-names))
       (names2 (all-class-names names1)))
  (hash-set-list (hash-set-exclusive-or names1 names2))
  ;; result should be nil
  )
|#

(defun do-with-named-classes-weakened (body-function output)
  (let ((table ccl::%find-classes%)
        class-names
        stash-population)
    (unwind-protect
      ;; Weaken
      (progn
        (ccl::trace-print-bracketing ("Weakening class name hash-table" :debug-treeshake)
          (setf class-names (all-class-names))
          (setf stash-population (weaken-hash-table table)))
        (funcall body-function))
      ;; Unweaken
      (ccl::trace-print-bracketing ("Unweakening class name hash-table" :debug-treeshake)
        (multiple-value-bind (reaped total)
                             (unweaken-hash-table table stash-population)
          (trace-print (:debug-treeshake) "Reaped ~5:D of ~5:D from %find-classes%" reaped total))

          (do-with-appropriate-output #'(lambda (str)
                                          (ignore-errors 
                                           (format str ">>> Classes~(~%~A~)"
                                                   (sort (hash-set-list
                                                          (hash-set-difference
                                                           class-names
                                                           (all-class-names class-names)))
                                                         #'string<))))
                                      output :supersede)))))

(defmacro with-named-classes-weakened ((&optional output-stream) &body body)
  `(do-with-named-classes-weakened ,`#'(lambda() ,@body) ,output-stream))
#| TEST
(progn
  (defclass bye-bye ()
    ())
  (with-named-classes-weakened (t)
    (gc)))
|#

;;;=======================================================================

;;; Weakening a package means moving all it's symbols out of the symbol table vector
;;; and into a population.  Then garbage collecting will remove any symbols that 
;;; aren't being used by anybody.  Unweakening a package puts those symbols back.

;;; weaken-package returns a list of two populations with all the stuff necessary 
;;; to reconstitute the package to it's original state.
;;; The population list looks like:
;;;   (total-size-of-itab pos sym pos sym pos sym ... )
;;;
;;; This new version saves out both the itab and etab.
(defun weaken-package (package)
    (flet ((weaken-package-table (package-table)
             (let* ((populist nil)
                    (tab (car package-table))
                    (length (length tab)))
               (dotimes (i length)
                 (let ((sym (svref tab i)))
                   (when sym
                     (push sym populist)
                     (push i   populist))))
               (push length populist)
               (setf (car package-table) (vector nil))
               (ccl::%cons-population populist))))
    (list (weaken-package-table (ccl::pkg.itab package))
          (weaken-package-table (ccl::pkg.etab package)))))

;;; Args are a weakened-package (sounds like a vacation, "weekend package", get it?)
;;; and a list o' two populations from weakened-package above.
(defun unweaken-package (package populations)
  (let ((count-deleted-symbols 0)
        (count-symbols 0))
    (flet ((unweaken-package-table (population)
             (let* ((populist (ccl::population.data population))
                    (length (pop populist))
                    (tab (make-array length))
                    index)
               (when  (numberp (car (last populist))) (incf count-deleted-symbols))
               (dolist (each populist)
                 (cond ((numberp each)
                        (incf count-symbols)
                        ;; if index is already set, we've had a symbol yanked
                        (when index 
                          (setf (svref tab index) (ccl::%unbound-marker-8))
                          (incf count-deleted-symbols))
                        
                        (setq index each))
                       (t 
                        (setf (svref tab index) each)
                        (setf index nil))))
               tab)))
      (setf (car (ccl::pkg.itab package))
            (unweaken-package-table (first populations)))
      (setf (car (ccl::pkg.etab package))
            (unweaken-package-table (second populations)))
      (values count-deleted-symbols count-symbols))))

;;; Reuse strings from previous-names if equal
(defun package-symbol-names (pkg &optional previous-names)
  (let ((names (make-hash-set :test #'equal)))
    (do-symbols (sym pkg)
      (when (eq (symbol-package sym) pkg)
        (hash-set-replace-using-other-set (symbol-name sym) names previous-names)))
    names))
#| TESTS
(let* ((names1 (package-symbol-names :glisp))
       (names2 (package-symbol-names :glisp names1)))
  (hash-set-list (hash-set-exclusive-or names1 names2))
  ;; result should be nil
  )
|#

(let (package-stashes)
  
  (defun weaken-package-list (package-list)
    (setf package-stashes '())
    (dolist (pkg package-list)
      (let ((names (package-symbol-names pkg)))
        (push (list names
                    (weaken-package pkg))
              package-stashes))))
  
  (defun unweaken-package-list (package-list output)
    (let ((total-reaped 0)
          (if-exists :supersede))
      (dolist (pkg (reverse package-list))
        (let* ((pkg-name (package-name pkg))
               (package-stash    (pop package-stashes))
               (names-before  (first  package-stash))
               (population    (second package-stash)))
          (multiple-value-bind (reaped total) (unweaken-package pkg population)
            (incf total-reaped reaped)
            (trace-print (:debug-treeshake) "Reaped ~5:D of ~6:D from ~A" reaped total pkg-name))

          (do-with-appropriate-output #'(lambda (str)
                                          (ignore-errors 
                                           (format str ">>> Package ~A~(~%~A~)" 
                                                   pkg-name
                                                   (sort (hash-set-list
                                                          (hash-set-difference
                                                           names-before
                                                           (package-symbol-names pkg names-before)))
                                                         #'string<))))
                                      output if-exists))
        (setf if-exists :append))
      (setf package-stashes '())
      total-reaped)))

(defparameter *write-symbols-out* nil)

(defun do-with-package-symbols-weakened (packages body-function output-stream)
  (let ((pkgs (mklist packages))
        pkg-names)
    (loop for ps on pkgs 
          doing (unless (packagep (car ps))
                  (unless (setf (car ps) (find-package (car ps)))
                    (error "needs to be a package, or a package name"))))
    (setf pkg-names (loop for ps in pkgs
                          collect (package-name ps)))
    (unwind-protect 
      (progn
        (when *write-symbols-out*
          (ccl::trace-print-bracketing ("Output symbols to 1-symbols-before file" :debug-treeshake)
            (print-symbols-to-file pkgs "ccl:1-symbols-before")))
        (ccl::trace-print-bracketing ("Weakening packages" :debug-treeshake)
          (ccl::trace-print () "Packages: ~s" pkg-names)
          (weaken-package-list pkgs))
        (funcall body-function))
      (ccl::trace-print-bracketing ("Unweakening packages" :debug-treeshake)
        (trace-print (:debug-treeshake) "Total: ~:D" (unweaken-package-list pkgs output-stream)))
      (when *write-symbols-out*
        (ccl::trace-print-bracketing ("Output symbols to 2-symbols-after file" :debug-treeshake)
          (print-symbols-to-file pkgs "ccl:2-symbols-after"))))))

(defmacro with-package-symbols-weakened ((packages &optional output-stream) &body body)
  `(do-with-package-symbols-weakened ,packages ,`#'(lambda() ,@body) ,output-stream))
#| TESTS

(defun print-package-symbols2 (package-name &optional (str t))
  (let ((table (car (ccl::pkg.itab (find-package package-name)))))
    (dotimes (i (length table))
      (format str "~%~6d ~a" i (svref table i)))))

(with-open-file (f "ccl:glisp2-before" :direction :output :if-exists :supersede)
  (print-package-symbols2 :glisp f))

(with-package-symbols-weakened (:keyword t) 
  (gc))

(with-open-file (f "ccl:glisp2-after" :direction :output)
  (print-package-symbols2 :glisp f))

|#

#| Portable version.  BTW, it doesn't work.

(let (packages-sorted
      package-imports-table
      package-externs-table
      package-interns-table)

  (defun weaken-package-list (package-list)
    (setf packages-sorted       (sort-package-list (copy-list package-list)))
    (setf package-imports-table (make-hash-table))
    (setf package-externs-table (make-hash-table))
    (setf package-interns-table (make-hash-table))

    (dolist (package packages-sorted)
      (let* ((pkg-name (package-name package))
             (pkg-imports (make-hash-table :weak :key :test #'eq))
             (pkg-externs (make-hash-set :weak t))
             (pkg-interns (make-hash-set :weak t)))
        (ccl::trace-print () ">>> saving package ~A" pkg-name)
        (setf (gethash pkg-name package-imports-table) pkg-imports)
        (setf (gethash pkg-name package-externs-table) pkg-externs)
        (setf (gethash pkg-name package-interns-table) pkg-interns)
        
        (flet ((do-symbol (sym set)
                 (without-interrupts
                  (let ((home-package (symbol-package sym)))
                    (unless (eq home-package package)
                      (setf (gethash sym pkg-imports) home-package)))
                  (hash-set-replace sym set)
                  (unintern sym package))))
          (do-external-symbols (sym package)
            ;; Can't mess with nil because it's too hard to re-import it.
            (unless (eq sym nil)             (do-symbol sym pkg-externs)))
          (do-symbols          (sym package) (do-symbol sym pkg-interns)))
        #|
               (unless (package-empty? package)
                          ;; shouldn't happen
                 (print-package-symbols (list package))
                 (break))
               (print-sym-table "External" pkg-externs)
               (print-sym-table "Internal" pkg-interns)
               |#
        )))
         
  (defun unweaken-package-list (package-list)
    (flet ((do-pass (pass)
             (dolist (package packages-sorted)
               (let* ((pkg-name (package-name package))
                      (pkg-externs (gethash pkg-name package-externs-table))
                      (pkg-interns (gethash pkg-name package-interns-table)))
                 (ccl::trace-print () ">>> restoring package ~A" pkg-name)
                 #|
                        (print-package-symbols (list package)) ; should print nothing
                        (print-sym-table "External" pkg-externs)
                        (print-sym-table "Internal" pkg-interns)
                        |#
                 (labels ((mapper (set)
                            (map-hash-set #'(lambda (sym)
                                              (do-symbol sym set))
                                          set))
                          (do-symbol (sym set)
                            (case pass
                              (1
                               (let* ((pkg-imports  (gethash pkg-name package-imports-table))
                                      (home-package (gethash sym pkg-imports)))
                                 (when home-package
                                   (import sym home-package))))
                              (2
                               (shadowing-import sym package)
                               (when (eq set pkg-externs)
                                 (export sym package))))))
                   (when pkg-externs (mapper pkg-externs))
                   (when pkg-interns (mapper pkg-interns)))))))
      (do-pass 1)
      (do-pass 2))
    (setf packages-sorted       nil)
    (setf package-imports-table nil)
    (setf package-externs-table nil)
    (setf package-interns-table nil)
    0 ; Here we are aupposed to return the total number of symbols reaped
    ))
|#

;;;=======================================================================

(defun gc-gc-room (&optional (skipit nil))
  (unless skipit
    (gc)
    (gc)
    (room)))

(defun reap-unused-objects-in-package-list (package-list &key noGC)
  (setf package-list (mklist package-list))
  (gc-gc-room noGC)
  (unwind-protect
    (let ((projects (sk8:knownChildren sk8:project)))
      (with-project-objectTables-weakened (projects "ccl:reaped-sk8-objects")
        (with-sk8object-children-weakened ()
          (with-package-symbols-weakened (package-list "ccl:reaped-symbols")
            (mf::clear-all-caches)
            
            (room)
            (ccl::trace-print (:debug-treeshake) "**** GC ****")
            (gc-gc-room noGC)
            
            ))))
      (gc-gc-room noGC)))

#|
(defun reaper-function (oldProcess)
  (process-kill oldProcess)
  (let ((keepers '(*default-menubar*)))
    (declare (ignore keepers))
    (reap-unused-objects))
  (ccl::make-new-listener)
  (process-kill *current-process*))

(defun reap-all-but-mcl ()
  (process-run-function "Ojo" #'reaper-function *current-process*))
|#

;;; Hold on to your hat if you don't want it reaped when you call this.
(defun reap-unused-objects ()
  (let ((keepers-list (append (list-all-packages)
                            '(cl::nil cl::t))))
    (declare (ignore keepers-list))
    (reap-unused-objects-in-package-list (list-all-packages-sorted))))
#| Try it
(reap-unused-objects)
|#

(defun cleanup-non-runtime-stuff-for-buildStandalone ()
  (when (and (boundp 'sk8::ui) (sk8::is-a sk8::ui sk8::project))
    (setf ccl::*control-for-modified-error-system* :original-MCL) ; this should be part of buildStandalone when you toss Proj Builder
    
    ;; Everything from here on is new as of 8/95
    (setf ccl::%source-files% (make-hash-table :weak :key :test #'eq :size 0))
    
    ;; Is this all there is to the documentation?  I'm not so sure -DY
    (setf ccl::%documentation (make-hash-table :weak t :size 0 :test 'eq))
    
    (setf ccl::%lambda-lists% (make-hash-table :weak t :size 0 :test 'eq))
    
    ;; What about saved definitions and any other junk kept around for developers' benefit?
    
    (ccl::remove-package :glisp)
#|
    (ccl::remove-package :sourceserver)
    (ccl::remove-package :inspector)
    (ccl::remove-package :ansi-loop)
|#
    ;; nuke it too.
    (setf ccl::*old-menubar* nil)
    (setf sk8dev::*no-ui-windows* t)
    (trace-print-bracketing ("Close the UI project" :debug-treeshake)
      (sk8dev::closeproject sk8::ui))

#|
    ;; This is not the way to do this, you hear me, not the way to do this.
    (trace-print (:debug-treeshake) "stackwatcher")
    (loop for (slot-name) in (ccl::class-instance-slots (class-of sk8::stackwatcher))
          doing (setf (slot-value sk8::stackwatcher slot-name) nil))
    
    (trace-print (:debug-treeshake) "")
    (setf (sk8dev::selectionRenderer sk8::simplehistorygraphrenderer) nil)
    (trace-print (:debug-treeshake) "")
    (setf (sk8dev::highlightcolor (sk8::picker sk8::stackwatcherstacklist)) nil)  
|#
#|
    ;; these guys cling to UI objects.
    (makunbound 'sk8::messagebox)
    (makunbound 'sk8::scripteditorprototype)
    (makunbound 'sk8::*user-menubar-obj*)
    
    ;; the UI package is gonna go.
    (setf mf::*ui-package* nil)
    ;; delete the ui packages from this list (I don't have time to do this more elegantly).
    (setf (cdr mf::*sk8-dev-packages*) nil)
|#
   
    (reap-unused-objects)
    (setf ccl::*control-for-modified-error-system* :original-MCL) ; this should be part of buildStandalone when you toss Proj Builder
    ))
#| Try it
(cleanup-non-runtime-stuff-for-buildStandalone)
|#

(defun reap-sk8 ()
  (gc-gc-room)
  (trace-print-bracketing ("Close the UI project" :debug-treeshake)
    (setf sk8dev::*no-ui-windows* t)
    (sk8dev::closeproject sk8::ui))

  (gc-gc-room)
  (trace-print-bracketing ("Free *source-mapping-table*" :debug-treeshake)
    (setf ccl::*source-mapping-table* nil))

  (gc-gc-room)
  (trace-print-bracketing ("Remove the glisp package" :debug-treeshake)
    (ccl::remove-package :glisp))
  
  (reap-unused-objects)
  )
#| Try it
(reap-sk8)
|#

  

#| Screwing around

(defparameter *tiny-file-menu* (make-instance 'menu :menu-title "File" :help-spec 1100))
(add-new-item *tiny-file-menu* "Quit" 'ccl::confirmed-quit
                :command-key #\Q
                :help-spec 1111)
(defparameter *tiny-menubar* (list *apple-menu* *tiny-file-menu*))

(defun yikes ()
  (set-menubar *tiny-menubar*)
  (let ((keepers (list *tiny-menubar*)))
    (ccl::with-output-to-new-window (output)
      (let ((*standard-output* output))
        (reap-unused-objects)))))

(yikes)

(window-close *top-listener*)

|#

#| TESTS

(defun package-list-to-package-name-list (package-list)
  (loop for pkg in package-list
        collect (package-name pkg)))

(dolist (pkg (list-all-packages-sorted))
  (format t "~&~A ~S~&" (package-name pkg) (package-list-to-package-name-list (package-use-list pkg))))

(defun print-sym-table (name sym-table)
  (format t "~&~A:" name)
  (maphash #'(lambda (key value)
               (declare (ignore value))
               (print key))
           sym-table))

(defun package-name-list-to-package-list (package-name-list)
  (loop for name in package-name-list
        collect (find-package name)))

(defun print-package-symbols (package-list)
  (dolist (pkg package-list)
    (format t "~&Package: ~A" (package-name pkg))
    (let ((syms '()))
      (do-symbols (sym pkg)
        (push sym syms))
      (dolist (sym (sort syms #'(lambda (s1 s2)
                                  (string< (symbol-name s1) (symbol-name s2)))))
        (print sym)))))

(defmacro with-temporary-packages ((package-specs) &body body)
  `(unwind-protect
    (progn
      (dolist (package-spec ,package-specs)
        (make-package (first package-spec) :use (second package-spec)))
      (progn ,@body))
     (dolist (package (reverse (sort-package-list
                                 (package-name-list-to-package-list (loop for package-spec in ,package-specs
                                                                          collect (first package-spec))))))
       (delete-package package))))

(defun test-reap-unused-objects ()
  (with-temporary-packages ('((:test1 nil)
                              (:test2 :test1)))
    (let ((package-list  (package-name-list-to-package-list '(:test2 :test1))))
      (intern "INTERNAL1A" :test1)
      (intern "EXTERNAL1A" :test1)
      (intern "INTERNAL1B" :test1)
      (intern "EXTERNAL1B" :test1)
      
      (intern "INTERNAL2"  :test2)
      (intern "EXTERNAL2"  :test2)
      
      (let ((keepers (list (intern "INTERNAL1A-KEEPER" :test1)
                           (intern "EXTERNAL1A-KEEPER" :test1)
                           (intern "INTERNAL1B-KEEPER" :test1)
                           (intern "EXTERNAL1B-KEEPER" :test1)
                           (intern "EXTERNAL1C-KEEPER" :test1)
                           (intern "INTERNAL2-KEEPER"  :test2)
                           (intern "EXTERNAL2-KEEPER"  :test2))))
        (declare (ignore keepers))
        
        (shadowing-import (make-symbol "INTERNAL1B")        :test2)
        (shadowing-import (make-symbol "EXTERNAL1B")        :test2)
        (shadowing-import (make-symbol "INTERNAL1B-KEEPER") :test2)
        (shadowing-import (make-symbol "EXTERNAL1B-KEEPER") :test2)
        
        (export (list (find-symbol "EXTERNAL1A"        :test1)
                      (find-symbol "EXTERNAL1B"        :test1)
                      (find-symbol "EXTERNAL1A-KEEPER" :test1)
                      (find-symbol "EXTERNAL1B-KEEPER" :test1)
                      (find-symbol "EXTERNAL1C-KEEPER" :test1)) :test1)
        (export (list (find-symbol "EXTERNAL2"         :test2)
                      (find-symbol "EXTERNAL2-KEEPER"  :test2)
                      (find-symbol "EXTERNAL1B"        :test2)
                      (find-symbol "EXTERNAL1B-KEEPER" :test2)) :test2)

        (setf keepers (append keepers (list (find-symbol "INTERNAL1B-KEEPER" :test2)
                                            (find-symbol "EXTERNAL1B-KEEPER" :test2))))
        
        (format t "~&Keepers:")
        (dolist (sym keepers)
          (print sym))
        
;        (compile-file "ccl:burp2")
;        (load         "ccl:burp2")

        (format t "~&All symbols:~&")
        (print-package-symbols package-list)

        (reap-unused-objects-in-package-list package-list)

        (format t "~&Remaining symbols:~&")
        (print-package-symbols package-list)
        ))))

(test-reap-unused-objects)

|#

(defvar a1 '(1))
(defvar a2 '(2))
(defvar a4 '(4))
(defvar b1 (list 8 'a1))
(defvar b2 (list 16 'b1))
(defvar b3 (list 32 'b1 'b2 'a2))
(defvar b4 (list 64 'b1 'a2 'a4))

(defvar *root* (list a1
                      a2
                      a4
                      b1
                      b2
                      b3
                      b4))

(defun object-references (obj)
  (rest obj))

(defun object-size (obj)
  (first obj))

(defun find-blobs (root)
  (let ((leaves (make-hash-table))
        (nodes (make-hash-table)))
    (dolist (item root)
      (if (object-references item)
        (progn
          (unless (gethash item nodes)
            (setf (gethash item nodes) item))
          )
        (setf (gethash item leaves) (object-size item))))))

#|

(object-references b1)

|#


#|

Dave's original:
(defmacro with-package-symbols-weakened ((package-list) &body body)
  `(let ((packages-sorted (sort-package-list (copy-list ,package-list)))
         (package-externs-table (make-hash-table))
         (package-interns-table (make-hash-table))
         (package-shadows-table (make-hash-table))
         (package-imports-table (make-hash-table)))
     (unwind-protect
       (progn
         (ccl::trace-print-bracketing ("Output symbols to 1-symbols-before file" :debug-treeshake)
           (print-symbols-to-file packages-sorted "ccl:1-symbols-before"))
         (ccl::trace-print-bracketing ("Stash symbols" :debug-treeshake)
           (dolist (package packages-sorted)
             (let* ((pkg-name (package-name package))
                    (pkg-externs (make-hash-set :weak t))
                    (pkg-interns (make-hash-set :weak t))
                    (pkg-shadows (make-hash-set :weak t))
                    (pkg-imports (make-hash-table :weak :key :test #'eq)))
               (ccl::trace-print (:debug-treeshake) ">>> saving package ~A" pkg-name)
               (setf (gethash pkg-name package-externs-table) pkg-externs)
               (setf (gethash pkg-name package-interns-table) pkg-interns)
               (setf (gethash pkg-name package-shadows-table) pkg-shadows)
               (setf (gethash pkg-name package-imports-table) pkg-imports)
               
               (dolist (sym (package-shadowing-symbols package))
                 (hash-set-replace sym pkg-shadows))
               (do-external-symbols (sym package)
                 (unless (or (eq sym nil)
                             (eq sym t))
                   (let ((home-package (symbol-package sym)))
                     (unless (eq home-package package)
                       (setf (gethash sym pkg-imports) home-package)))
                   (hash-set-replace sym pkg-externs)
                   (unintern sym package)))
               (do-symbols (sym package)
                 (unless (or (eq sym nil)
                             (eq sym t))
                   (let ((home-package (symbol-package sym)))
                     (unless (eq home-package package)
                       (setf (gethash sym pkg-imports) home-package)))
                   (hash-set-replace sym pkg-interns)
                   (unintern sym package)))
               #|
               (print-package-symbols (list package)) ; should print nothing
               (print-sym-table "External" pkg-externs)
               (print-sym-table "Internal" pkg-interns)
               (print-sym-table "Shadows"  pkg-shadows)
               |#
               )))
         
         (progn ,@body))
       
       (unwind-protect
         (ccl::trace-print-bracketing ("Restore symbols" :debug-treeshake)
           (dolist (package packages-sorted)
             (let* ((pkg-name (package-name package))
                    (pkg-externs (gethash pkg-name package-externs-table))
                    (pkg-interns (gethash pkg-name package-interns-table))
                    (pkg-shadows (gethash pkg-name package-shadows-table))
                    (pkg-imports (gethash pkg-name package-imports-table)))
               (ccl::trace-print (:debug-treeshake) ">>> restoring package ~A" pkg-name)
               #|
               (print-package-symbols (list package)) ; should print nothing
               (print-sym-table "External" pkg-externs)
               (print-sym-table "Internal" pkg-interns)
               (print-sym-table "Shadows"  pkg-shadows)
               |#
               (when pkg-externs
                 (map-hash-set #'(lambda (sym)
                                   (let ((home-package (gethash sym pkg-imports)))
                                     (when home-package
                                       (import sym home-package)))
                                   (if (hash-set-member sym pkg-shadows)
                                     (shadowing-import sym package)
                                     (import           sym package))
                                   (export sym package))
                               pkg-externs))
               (when pkg-interns
                 (map-hash-set #'(lambda (sym)
                                   (let ((home-package (gethash sym pkg-imports)))
                                     (when home-package
                                       (import sym home-package)))
                                   (if (hash-set-member sym pkg-shadows)
                                     (shadowing-import sym package)
                                     (import           sym package)))
                               pkg-interns)))))
         (ccl::trace-print-bracketing ("Output symbols to 2-symbols-after file" :debug-treeshake)
           (print-symbols-to-file packages-sorted "ccl:2-symbols-after"))))))
|#

(export '(with-sk8object-children-weakened
           with-package-symbols-weakened
           reap-unused-objects-in-package-list
           reap-unused-objects)
        :cl-user)

(in-package :sk8dev)
(SK8-declare-syms :SK8 :public
                  SK8::sneakyDevMode
                   )
(defun SK8::sneakyDevMode () (dev-mode :force? t))
(in-package :cl-user)



#|
	Change History (most recent last):
	1  	 7/17/95	dy      	leave unnamed objects in knownChildren.
	2  	 7/18/95	till    	almost fixed the imported symbols problem
	3  	 7/21/95	till    	new simple way of weakening symbols
	4  	 7/21/95	dy      	put back older code commented out,
						add a few debugging functions
	5  	 7/25/95	dy      	Changes to DY's version inside comments
	6  	 7/25/95	Till    	Better package weakening.   Much better.  Won't explode.
	7  	 7/31/95	dy      	Better instrumentation on with-weakened-knownChildren
	8  	 8/ 2/95	dy      	Add with-project-object-symbols-weakened
	9  	 8/ 2/95	dy      	change name to with-project-objectTable-weakened
	10 	 8/ 2/95	dy      	fix unbalanced parens
	11 	 8/ 2/95	dy      	some fixes or other
	12 	 8/ 2/95	dy      	
	13 	 8/ 2/95	dy      	even more informative printouts when unweakening things
	14 	 8/ 2/95	dy      	~6:D instead of ~5:D
	15 	 8/ 3/95	till    	this'n'that
	16 	 8/ 3/95	Till    	make weaken-package and unweaken-package
						grok external symbols too.
	17 	 8/ 3/95	dy      	New reap-sk8 function
	18 	 8/ 3/95	dy      	balanced #| |# now
	19 	 8/ 3/95	dy      	gc-gc-room optional skipit arg
	20 	 8/ 3/95	Till    	oops
	21 	 8/ 3/95	Till    	
	22 	 8/ 3/95	dy      	New cleanup-for-buildStandalone function which is called by buildStandalone
	23 	 8/ 4/95	dy      	OK, OK, I fixed the macros
	24 	 8/ 4/95	till    	make (un)weaken-package actually kind'a work
	25 	 8/ 4/95	dy      	Oops.  It's supposed to be %source-files%.
						Also, set error handling to be :original-MCL.
	26 	 8/ 4/95	dy      	Modified (un)weaken-project-list to print names of reaped objects.  Probably doesn't work yet.
	27 	 8/ 4/95	dy      	It does work!
	28 	 8/ 4/95	dy      	Clear out ccl::%documentation
	29 	 8/ 4/95	dy      	Clear out ccl::%lambda-lists%
	30 	 8/ 5/95	dy      	Also print out reaped  symbols.
						Implemented hash-set-difference and use it.
	31 	 8/ 5/95	dy      	Had fun making hash-set a real CLOS class.
						Everything works now, unless I contradict this with a subsequent checkin.
	32 	 8/ 5/95	dy      	Fixed spelling of supersede
	33 	 8/ 5/95	dy      	fix typo
	34 	 8/ 5/95	dy      	OK.  *this* time it's all fixed.
	35 	 8/ 9/95	dy      	Clean up hash-set-replace.  Delete hash-set-add.
	36 	 8/11/95	dy      	hash-set stuff moved from here.
						Requires c32.
	37 	 8/16/95	Till    	add new stuff to cleanup-for-buildStandalone
	38 	 8/16/95	dy      	Fleshed out cleanup-non-runtime-stuff-for-buildStandalone (newly renamed), but then commented out the stuff that probably causes trouble.
						This may still not work yet.
	39 	 8/16/95	dy      	Also remove  :ansi-loop package
	40 	 8/16/95	dy      	Fix package-empty?
	41 	 8/18/95	dy      	what worked for the demo
	42 	 9/ 8/95	Till    	fix do-outputs, add ignore-errors
	3  	 2/27/97	Hernan  	Removing case preserving reader macro.
|# ;(do not edit past this line!!)
