(in-package :sk8dev)

;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories



;;;
;;; Some macros so the low-level stuff can be used either in naked MCL or in SK8
;;; Also allows us to write documentation strings even if they are ignored

(defmacro defun_X (fname private? args &body body)
  #+SK8 `(defun ,fname ,args  . ,(if (stringp (car body)) (cdr body) body))
  #-SK8 (declare (ignore private?))
  #-SK8 `(progn
           (export ',fname)
           (defun ,fname ,args  ,@body))
  )

(defmacro defmacro_X (name &body body)
  #+SK8 `(defmacro ,name ,@body)
  #-SK8 `(progn
           (export ',name)
           (defmacro ,name ,@body))
  )

(defmacro defconstant_X (name value &optional documentation &body body)
  #+SK8 `(define-sk8-constant ,name ,value)
  #-SK8 (declare (ignore name value documentation body))
  )

(defmacro defconstant_XE (name value &optional documentation &body body)
  #+SK8 `(define-sk8-constant ,name ,value)
  #-SK8 (declare (ignore body))
  #-SK8 `(progn
           (export ',name)
           (defconstant ,name ,value . ,(if (stringp documentation) `(,documentation) nil)))
  )

(defmacro defvar_X (name value &optional documentation &body body)
  #+SK8 `(define-sk8-var ,name :initial-value ,value . ,(if (stringp documentation) body `(,documentation . ,body)))
  #-SK8 (declare (ignore name value documentation body))
  )

(defmacro defvar_XE (name value &optional documentation &body body)
  (declare (ignore-if-unused documentation body))
  #+SK8 `(define-sk8-var ,name :initial-value ,value) 
  ;; There is no point in adding any of this since it is ignored by define-sk8-var. Hernan
  ;; . ,(if (stringp documentation) body `(,documentation . ,body)))
  #-SK8 (declare (ignore body))
  #-SK8 `(progn
           (export ',name)
           (defvar ,name ,value . ,(if (stringp documentation) `(,documentation) nil)))
  )

(defmacro defpascal_X (name &body body)
  `(progn
     (export ',name)
     (defpascal ,name ,@body))
  )

(defmacro values_X (&body vals)
  #+SK8 `(sk8-multivals ,@vals)
  #-SK8 `(values ,@vals)
  )

(defmacro multiple-value-bind_X (&body body)
  #+SK8 `(sk8-multival-bind ,@body)
  #-SK8 `(multiple-value-bind ,@body)
  )

(defmacro trap-flag-arg (flagSymbol keywordArg flagBit)
  "Check if keywordArg was set.  If so, then store the given flag bit into the flags integer."
  `(when ,(intern (concatenate 'string (symbol-name keywordArg) "SET") cl::*package*)
     (setf ,flagSymbol (boole (if ,keywordArg boole-ior boole-andc1) ,flagBit ,flagSymbol))))

(defmacro assumeOSPtr ((me osPtrName) &body body)
  `(let ((,osPtrName (,osPtrName ,me)))
     (if ,osPtrName
       (progn ,@body)
       *undefined*)))

(defmacro assumeOSPtrWithError ((me osPtrName) &body body)
  `(let ((,osPtrName (,osPtrName ,me)))
     (if ,osPtrName
       (progn ,@body)
       #+SK8 (sk8-error GeneralProgrammaticError
                        :strings '("no OSPtr for ")
                        :objects (list ,me))
       #-SK8 (error (format nil "No OSPtr for ~S." me)))))

#| example of trap-flag-arg
(defun_X T_NewMovieGC nil
         (&key
          (flags 0)
          ((active active_)   nil active_Set)
          (dontAutoAlternates nil dontAutoAlternatesSet))
  "Returns movieGCOSPtr.
See IM-QuickTime p. 2-90"
  (declare (type integer flags))
  (trap-flag-arg flags active_                  #$newMovieActive)
  (trap-flag-arg flags dontAutoAlternates       #$newMovieDontAutoAlternates)
  (T_makeGCOSPtr (checking-toolbox-error (:pointer (#_GetMoviesError))
                                         (#_NewMovie flags))
                 #'T_DisposeMovie)) |#

(defmacro trap-flag-initializer (flagSymbol keywordArg flagBit)
  "Check if keywordArg was set (i.e. it is not *undefined*.
If so, then store the given flag bit into the flags integer."
  `(unless (eq ,keywordArg *undefined*)
     (setf ,flagSymbol (boole (if ,keywordArg boole-ior boole-andc1) ,flagBit ,flagSymbol))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant_XE TC_unknownError 32766 :private? t :project SK8) ; had to make this one up
  #-SK8 (defconstant $unknownError 32766); had to make this one up
  )

(defun %coerce-to-macptr (x)
  (typecase x
    (macptr  x)
    (integer (%int-to-ptr x))
    (null    (%null-ptr))
    (t       0)))

(defmacro with-coercing-to-macptr ((var x) &body body)
  `(let ((,var (%coerce-to-macptr ,x)))
     ,@body))
#| testing
(defvar xx1 nil)
(with-coercing-to-macptr (xx1 (%null-ptr)) xx1)
(with-coercing-to-macptr (xx1 nil) xx1)
(with-coercing-to-macptr (xx1 3) xx1)
|#


;;; The following macro calls a toolbox routine, then signals an error if warranted.
;;;
(defmacro checking-toolbox-error
          ((error-protocol &optional error-determining-form &key ok-errors error-result)
           toolbox-form)
  "Evaluate toolbox-form and return its result, ensuring that an error exception is raised
if an error occured.  Different toolbox traps report errors in different styles.  The
error-protocol argument is a keyword symbol which specifies the style of error result code
determination appropriate to the toolbox call.  It is either :OSErr, indicating that the
trap returns a result code directly, or one of several other symbols that indicating that
error-determining-form (or a default function) must be called to determine a result code.
If the result code is neither noErr nor a member of the optional ok-errors list,
checking-toolbox-error calls the function raiseSystemError with the error result code and
the trap name as arguments.  If the trap succeeds and ok-errors list is non-nil, then the
result code is returned as the only value when error-protocol is :OSErr or :void, otherwise
as the second of two values for other error-protocol choices.  If error-determining-form
returns a non-numeric result, raiseSystemError is called with that as the result code
argument and is responsible for doing something useful with it.  The error-protocol argument
 must be one of:
  :OSErr
     toolbox-form returns an OSErr.  If ok-errors is non-nil, returns the result code;
     otherwise returns no values.  You can specify an error-determining-form, which gets
     called in a context in which theError is bound to the OSErr result code.  The result
     of error-determining-form is tested for equality to $noErr to determine if there was
     really an error.
  :pointer
     toolbox-form returns a pointer or handle.  If the return value is eql
     (%null-ptr) it is an error.  Some of the really ancient traps, (e.g. Get1Resource,
     GetCicn) can return a nil pointer and still yield a result code of noErr; you must use
     the keyword argument :error-result to specify a substitute result code for these cases
     or a default error code of TC_unknownError will be assigned for you.  (There is no
     standard result code constant which could be used to mean \"unknown error\").
     If you specify :ok-errors with :pointer, and the trap return value is eql (%null-ptr),
     the return value is nil (False) rather than (%null-ptr).
     For most :pointer toolbox calls, you specify an error-determining-form which will
     then be called to find out what the error was.  The :pointer error protocol has a
     default for error-determining-form, which is (#_MemError).  If there is no way to find
     a system error code for the case when the return value is (%null-ptr), but you want
     to raise an error anyway, then specify :none as the error-determining-form (TC_unknownError
     will be the result code).
  :void
     toolbox-form doesn't return a value.  If ok-errors is non-nil, returns the result code;
     otherwise returns no values.  You must specify an error-determining-form which will
     be called to find out what the error was.
  :value
     toolbox-form returns a value which isn't useful for error determination.
     You must specify an error-determining-form which will be called to find out
     what the error was.
  :minus-one
     Returns -1 if there is an error.  You may specify an error-determining-form
     which will be called to find out what the error was; if you don't, TC_noErr
     will be the result code.
Some known values for error-determining-form are:
   (#_MemError)
   (#_ResError)
   (#_QDError)
   (#_PrError)
   (#_GetMoviesError)
Examples:
   (checking-toolbox-error (:OSErr) (#_CloseMovieFile resRefNum))
   (setf theError (checking-toolbox-error (:OSErr nil :ok-errors '(#.TC_fnfErr))
                    (#_FSMakeFSSpec vRefNum dirID fileNamePstr fsspecptr)))
   (setf theRegion (checking-toolbox-error (:pointer) (#_NewRgn)))
   (setf icon (checking-toolbox-error (:pointer :none :error-result #$resNotFound) (#_GetCicn)))
   (multiple-value-setq
     (resourceHandle resultCode)
     (checking-toolbox-error (:pointer nil
                              :error-result -191
                              :ok-errors '(-191 #.TC_resNotFound))
                             (#Get1Resource \"snd \" 1)))
   (checking-toolbox-error (:Pointer :none) (#_FindNextComponent aComponent looking))
To do:
   * Add a :checkedValue error-protocol as a general case of the :minus-one protocol.
   * Someday, checking-toolbox-error could know about most toolbox calls, so
     both the error-protocol and error-determining-form arguments could be optional.
     They would be needed only in the case of unknown toolbox calls, but would be
     checked if specified for known toolbox calls.
Origin:
  Dave Yost, Apple, 9/94"
  (let ((result (gensym))
        (errResult (gensym)))
    `(let ,(case error-protocol
             ((:OSErr :void) `(        ,errResult))
             (otherwise      `(,result ,errResult))
             )
       (unless ,(case error-protocol
                  ;; All of the following cases return t when there is no error, otherwise nil
                  (:OSErr
                   ;; calls that return an OSErr
                   (if error-determining-form
                     `(or (eql ,(require-trap-constant #$noErr)
                               (setq ,errResult ,toolbox-form))
                          (eql ,(require-trap-constant #$noErr)
                               (let ((theError ,errResult))
                                 (setq ,errResult ,error-determining-form))))
                     `(eql ,(require-trap-constant #$noErr)
                           (setq ,errResult ,toolbox-form))))
                  (:pointer
                   ;; calls that return a pointer or handle
                   (case error-determining-form
                     (:none
                      (when ok-errors
                        (error
                         "checking-toolbox-error with :pointer :none can't be used with :ok-erorrs"))
                      `(or (not (eql (ccl:%null-ptr)
                                     (setq ,result ,toolbox-form)))
                           (progn
                             (setq ,result nil)
                             (setq ,errResult
                                   ,(if error-result
                                      error-result
                                      TC_unknownError)) ;#$unknownErr would be better, if it existed
                             nil)))
                     (otherwise
                      `(or (not (eql (ccl:%null-ptr)
                                     (setq ,result ,toolbox-form)))
                           (progn
                             (setq ,result nil)
                             ,(if error-determining-form
                                `(setq ,errResult ,error-determining-form)
                                `(setq ,errResult (require-trap #_MemError)))
                             ,(if error-result
                                `(progn
                                   (when (= ,errResult (require-trap-constant #$noErr))
                                     (setq ,errResult ,error-result))
                                   nil)
                                nil))))))
                  (:void
                   ;; calls with no return value
                   (unless error-determining-form
                     (error
                      "checking-toolbox-error with :void requires the error-determining-form argument"))
                   `(progn
                      ,toolbox-form
                      (eql ,(require-trap-constant #$noErr) (setq ,errResult ,error-determining-form))))
                  (:value
                   ;; calls whose return value isn't useful for error determination
                   (unless error-determining-form
                     (error
                      "checking-toolbox-error with :value requires the error-determining-form argument"))
                   `(progn
                      (setq ,result ,toolbox-form)
                      (eql ,(require-trap-constant #$noErr) (setq ,errResult ,error-determining-form))))
                  (:minus-one
                   ;; Returns -1
                   (if error-determining-form
                     `(or (not (eql -1 (setq ,result ,toolbox-form)))
                          (eql ,(require-trap-constant #$noErr) (setq ,errResult ,error-determining-form)))
                     (progn
                       (when ok-errors
                         (error
                          "checking-toolbox-error with :minus-one and :ok-erorrs requires error-determining-form"))
                       `(progn
                          (setf ,errResult TC_unknownError) ;#$unknownErr would be better, if it existed
                          (not (eql -1 (setq ,result ,toolbox-form)))))))
                  (otherwise
                   ;; Unknown error determination protocol
                   (error
                    "Unknown error-protocol argument to checking-toolbox-error: ~A"
                    error-protocol)))
         ,(if ok-errors
            `(unless (member ,errResult ,ok-errors)
               (raiseSystemError ',(first toolbox-form) ,errResult))
            `(raiseSystemError ',(first toolbox-form) ,errResult)
            ))
       ,(if ok-errors
          (if (or (eql error-protocol :OSErr)
                  (eql error-protocol :void))
            errResult
            `(values_X ,result ,errResult))
          (if (or (eql error-protocol :OSErr)
                  (eql error-protocol :void))
            `(values_X)
            result)))))

(defun raiseSystemError (trapName errResult)
  #+SK8 (cond
         ((eql errResult #$noErr)
          (error (format nil "Unidentified system error while calling ~a" trapName)))
         ((numberp errResult)
          (error "System error ~a while calling ~a"
                 (format nil "~d" errResult)
                 trapName))
         ((stringp errResult)
          (sk8-error GeneralError :strings (list errResult)))
         (t
          (error errResult)))
  #-SK8 (error "OS error ~a while calling ~a"
               (if (numberp errResult)
                 (format nil "~d" errResult)
                 "(no error code)")
               trapName))

#|
;;; Test checking-toolbox-error by expanding these macros and looking at the code
(checking-toolbox-error (:OSErr) (#_CloseMovieFile 0))
(checking-toolbox-error (:OSErr) (TRAPS:_CloseMovieFile 0))
(checking-toolbox-error (:OSErr nil :ok-errors '(1 2)) (_XX))
(checking-toolbox-error (:OSErr (foo)) (_XX))        ;bad

(checking-toolbox-error (:pointer) (_XX))
(checking-toolbox-error (:pointer nil :error-result #$resNotFound) (_XX))
(checking-toolbox-error (:pointer) (ccl::%null-ptr))
(checking-toolbox-error (:pointer (values #$noErr)) (ccl::%null-ptr))
(checking-toolbox-error (:pointer (values #$invalidMovie)) (ccl::%null-ptr))
(checking-toolbox-error (:pointer :none) (_XX))
(checking-toolbox-error (:pointer :none :error-result #$resNotFound) (_XX))
(checking-toolbox-error (:pointer (_ERR)) (_XX))
(checking-toolbox-error (:pointer (_ERR) :error-result -191) (_XX))
(checking-toolbox-error (:pointer (_ERR) :ok-errors '(1 2)) (_XX))
(checking-toolbox-error (:pointer (_ERR) :error-result 3 :ok-errors '(1 2)) (_XX))
(checking-toolbox-error (:pointer (_ERR) :ok-errors oklist) (_XX))
(checking-toolbox-error (:pointer :none :ok-errors '(1 2)) (_XX))    ; bad

(checking-toolbox-error (:void (_ERR)) (_XX))
(checking-toolbox-error (:void (_ERR) :ok-errors '(1 2)) (_XX))
(checking-toolbox-error (:void) (_XX))               ;bad

(checking-toolbox-error (:value (_ERR)) (_XX))
(checking-toolbox-error (:value (_ERR) :ok-errors '(1 2)) (_XX))
(checking-toolbox-error (:value) (_XX))               ;bad

(checking-toolbox-error (:minus-one) (_XX))
(checking-toolbox-error (:minus-one (_ERR) :ok-errors '(1 2)) (_XX))
(checking-toolbox-error (:minus-one nil :ok-errors '(1 2)) (_XX))         ; bad

(checking-toolbox-error (:junk) (_XX))               ;bad
|#

;;;========================================================

#|
	Change History (most recent last):
	1	2/25/94	dy	
	2	2/25/94	kleiman	took out #~ thingies
	3	2/25/94	kleiman	put back the #~ s
	4	2/26/94	kleiman	added defvar_X
	5	3/4/94	dy	#+macframes instead of #+SK8Script
	6	3/4/94	dy	#+SK8 instead of #+macframes
	7	3/4/94	dy	fix defvar_X
	8	3/16/94	dy	raise a sk8 error correctly from a trap call
	9	3/23/94	dy	checking-toolbox-error :pointer must still raise an error if return value is nil but result code is noErr.  Also return no values from :OSErr or :void trap calls.
	10	3/28/94	dy	checking-toolbox-error: fix warning with :OSErr and :void, other fixes related to #$noErr
	11	6/13/94	dy	fix doc string for checking-toolbox-error
	12	7/20/94	dy	force recompilation
	13 	 8/29/94	dy      	rasiseSystemError non-int error type
	14 	 9/ 6/94	dy      	checking-toolbox-error :pointer rework
	15 	 9/ 6/94	dy      	do-flag-arg in Movies.lisp -> trap-flag-arg here
	16 	 9/15/94	dy      	:OSErr error-style can take an error-determining-form.
	17 	10/ 5/94	dy      	add trap-flag-initializer
	18 	10/17/94	dy      	change example in documentation string
	19 	10/31/94	sidney  	values_x is not right when a function can return different numbers of values!!
	20 	10/31/94	dy      	back out last change
	21 	11/16/94	dy      	Caught one case where (require-trap-constant #$noErr) was needed instead of just #$noErr
	22 	 2/22/95	dy      	Make raiseSystemError generate a SystemError when errResult is numeric
	23 	 3/10/95	dy      	moved here: assumeOSPtr and assumeOSPtrWithError
	2  	11/29/95	dy      	new: with-use-resource-file & with-open-and-use-resource-file
	3  	 1/ 8/96	dy      	move %coerce-to-macptr here from Movies.lisp, remove with-use-resource-file, moved elsewhere, clean up some #~, change some #-SK8 code
	4  	 1/15/96	Hernan  	Fixed defvar-XE to not pass documentation and body to 
						define-sk8-var who in any case would ignore these args.
	2  	 4/10/96	Hernan  	define-sk8-function->defun.
	2  	10/18/96	sidney  	SystemError behaves differently now, change raiseSystemError to match
	3  	 2/27/97	Hernan  	
|# ;(do not edit past this line!!)
