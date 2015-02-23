;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories


;;;

(in-package :Macframes)

;; take care of this in build part 1
(unuse-package :sk8 :sk8dev)
(unuse-package :ui :uidev)


(let ((syms '(SK8-override SK8-share SK8-declare-syms SK8-proclaim-special))) 
  (export syms :Macframes)
  (import syms :SK8)
  (import syms :SK8Dev)
  (import syms :UI)
  (import syms :UIDev)
  (import syms :SK8Script))

(defun SK8-publish-syms* (symsToPublish fromPackage)
  (when symsToPublish
    (require-type (setq fromPackage (find-package fromPackage)) 'package)
    (export symsToPublish fromPackage)
    ;; This should be a bit more flexible, but it's correct for now
    (cond ((eq fromPackage (find-package :SK8))
           (import symsToPublish :sk8dev)
           (import symsToPublish :uidev)
           (import symsToPublish :UI)
           (export symsToPublish :UI))
          ((eq fromPackage (find-package :UI))
           (import symsToPublish :uidev)))))

;; symbols that are intended for internal use... put them in sk8dev or uidev package and don't export them from sk8
;; but do leave them in the sk8 or ui package. This all doesn't make that much sense, but it
;; is easier to leave the declarations the way they are than it is to fix everything
(defun SK8-privatize-syms* (symsToPrivatize fromPackage)
  (when symsToPrivatize
    (require-type (setq fromPackage (find-package fromPackage)) 'package)
    ;; This should be a bit more flexible, but it's correct for now
    (let ((toPackage (cond
                      ((eq fromPackage (find-package :SK8)) (find-package :SK8Development))
                      ((eq fromPackage (find-package :UI)) (find-package :UIDevelopment))
                      (t (error "Error in SK8 build. Private SK8 symbols must be declared in SK8 or UI package only")))))
      (dolist (sym symsToPrivatize)
        (let* ((symname (symbol-name sym))
               (pkg (symbol-package sym))
               (symfrom (find-symbol symname fromPackage))
               (symto (find-symbol symname toPackage)))
          ;; workaround some strange behavior that can happen as a result of this function being called at compile time
          (when (and symfrom (null pkg))
            (setf sym symfrom))
          (if symfrom
              (if (neq sym symfrom)
                (error "SK8 build error: symbol ~s should be in package ~s but is in package ~s" sym frompackage pkg)
                (unexport symfrom fromPackage))
              (progn
                (import sym frompackage)
                (setf symfrom sym)))
          (if symto
            (when (neq symfrom symto)
              (unintern symfrom frompackage)
              (import symto frompackage))
            (import symfrom topackage)))))))

(defun SK8-override-syms* (symsToOverride toPackage toOtherPackages)
  (when symsToOverride
    (flet ((resolve-export-conflicts (syms pkg)
             (dolist (conflict (CCL::check-export-conflicts syms pkg))
               (destructuring-bind (using-package inherited-sym) (cddr conflict)
                 (shadowing-import inherited-sym using-package)))
             (export syms pkg)))
      (setq toPackage (find-package toPackage)
            toOtherPackages (mapcar #'find-package toOtherPackages)
            symsToOverride (mapcar #'(lambda (s) (intern (symbol-name s) toPackage)) symsToOverride))
      (resolve-export-conflicts symsToOverride toPackage)
      (dolist (otherPkg toOtherPackages)
        (import symsToOverride otherPkg)
        (resolve-export-conflicts symsToOverride otherPkg)))))


(defun SK8-share-syms* (symsToShare toPackage)
  (when symsToShare
    (import symsToShare toPackage)
    (SK8-publish-syms* symsToShare toPackage)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  The "undefined" marker...
;;;

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar SK8::*Undefined* '#:*undefined*))   ;; use this for the undefined value

;;; Modify the compiler policy(s) so constants containing any standard-objects don't fold
;;; (This used to be earlier, but it can cause problems before *undefined* is defined

(flet ((SK8-substitutable-constant-p (sym val env)
         (declare (ignore sym env))
         ;; We don't allow the constant to fold if it's a standard-instance (i.e. it's most likely a SK8 object)
         (and (not (CCL::standard-object-p val))
              (not (eq sk8::*undefined* val)))))
  (set-current-compiler-policy (new-compiler-policy :allow-constant-substitution #'SK8-substitutable-constant-p))
  (set-current-file-compiler-policy (new-compiler-policy :allow-constant-substitution #'SK8-substitutable-constant-p)))

(defmethod print-object ((sk8::me ccl::immediate) strm)
  (if (eq sk8::me SK8::*undefined*)
    (print-object 'SK8::*undefined* strm)
    (call-next-method)))


;; force a symbol that will be an object to be declared as a constant, without recording the file
;; That means that when it is made into an object, SK8 will have to redefine the constant
;; publish the ones that have not yet been exported (published or overridden)
(defun SK8-proclaim-special (syms inPackage)
  (flet ((external-in-package (sym)
    (multiple-value-bind (s state)
                         (find-symbol (symbol-name sym) inPackage)
      (declare (ignore s))
      (eq state :external))))
    (declare (dynamic-extent not-external-in-package))
    (SK8-publish-syms* (remove-if #'external-in-package syms) inPackage))
  (dolist (sym syms)
    (ccl::%proclaim-special sym)
    (set sym SK8::*undefined*) ;; (ccl::define-constant sym SK8::*undefined*)
    ))

(defvar *fetch-exports-list* nil)
(defmacro SK8-declare-syms (inPackage publicOrPrivate &rest symbols)
  (cond
   (*fetch-exports-list*
    (setq *fetch-exports-list* symbols)
    nil)
   (t
    (unless (memq publicOrPrivate '(:public :private :special))
      (error "The publicOrPrivate must be either :PUBLIC, :PRIVATE or :SPECIAL"))
    (when symbols
      (setq symbols (reduce #'(lambda (current nextList)
                                (if (listp nextList)
                                  (append current (cdr nextList))
                                  (cons nextList current)))
                            symbols
                            :initial-value nil))
      (when symbols
        (let ((funcname 
               (case publicOrPrivate
                 (:private
                  'SK8-privatize-syms*)
                 (:public
                  'SK8-publish-syms*)
                 (:special
                  'SK8-proclaim-special))))
          `(eval-when (:compile-toplevel :load-toplevel :execute)
             (,funcname ',symbols ,inPackage))))))))

(defmacro SK8-override (toPackage (&rest toOtherPackages) &rest symsToOverride)
  (when symsToOverride
    `(eval-when (compile load eval)
       (SK8-override-syms* ',symsToOverride ,toPackage ',toOtherPackages))))


(defmacro SK8-share (toPackage &rest symsToShare)
  (when symsToShare
    `(eval-when (compile load eval)
       (SK8-share-syms* ',symsToShare ,toPackage))))


;; Doesn't really belong in this file.  But, hey, who's counting?
(defmethod make-load-form ((self package))
  `(find-package ,(CCL::make-keyword (package-name self))))

#|
	Change History (most recent last):
	4	2/20/94	sidney	Add new category of symbols: Specials are used for SK8 object names to proclaim them special
	2  	 8/17/95	sidney  	implement declaration of object names as constant *undefined* during build
	3  	 4/15/96	Hernan  	
	3  	 4/16/96	sidney  	move constant-folding compiler policy hack to here, after *undefined*
	4  	 5/ 7/96	sidney  	Changes for new object system
	5  	 5/ 9/96	sidney  	specials no longer publish already exported symbols
	7  	 7/ 7/96	sidney  	changes for native PPC build
	8  	 2/27/97	Hernan  	
|# ;(do not edit past this line!!)
