(in-package :sk8dev)

;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories



(eval-when (:compile-toplevel :execute)
  (require "LISPEQU")                   ; $flags_DisposXXX
  )

;;; You must specify a function to be called when a terminable
;;; macptr is garbage collected.  Traps are not functions, so we need
;;; a function wrapper for disposer trap we use.  This hash table
;;; associates a trap name with a function wrapper for that trap.
(defvar *termination-trap-wrappers* (make-hash-table :test 'eql))
; (setq *termination-trap-wrappers* (make-hash-table :test 'eql))

#|
(loop for value being the hash-values of *termination-trap-wrappers*
      using (hash-key key)
        do (pr "~A ~A~%" key value))
|#

#| 
;;; All of these should expand to the same thing
(makeGCOSPtr abc #_DisposeHandle)
(handleToGCHandle abc)
(handleToGCHandle abc :disposeFunctionOrTrap #_DisposeHandle)

;;; All of these should expand to the same thing
(makeGCOSPtr abc #_DisposePtr)
(PtrToGCPtr abc)
(PtrToGCPtr abc :disposeFunctionOrTrap #_DisposePtr)
|#

#|
"Argument theOSPtr is an existing OSPtr (Handle or Ptr).
Argument disposeFunctionOrTrap is a function returning t or a
trap name symbol (e.g. '#_DisposeHandle).  If theOSPtr is not
already a GCOSPtr, returns a new GCOSPtr, a copy of theOSPtr;
otherwise returns theOSPtr unchanged.
When a GCOSPtr is garbage collected, its disposeFunctionOrTrap is
called with the GCOSPtr as its sole argument.
If you're going to hand over responsibility for disposing a GCOSPtr
to a low-level object or some low-level code, then deactivate its
terminability with T_deactivateGCPtr.
Makes use of make-terminable-macptr or make-gcable-macptr
as appropriate."
|#
  
;; !! This seems to work now when you give it a trap, but it needs to do the right thing
;; if passed a function!
(defmacro makeGCOSPtr (theOSPtr disposeFunctionOrTrap &key terminateOnQuit master)
  (if (listp disposeFunctionOrTrap)
    `(makeGCOSPtrHelper ,theOSPtr
                        ,disposeFunctionOrTrap
                        ,terminateOnQuit
                        nil
                        ,master)
    `(makeGCOSPtrHelper ,theOSPtr
                        ',disposeFunctionOrTrap
                        ,terminateOnQuit
                        #'(lambda (x)
                            (require-trap ,disposeFunctionOrTrap x)
                            t)
                        ,master)))
  
(defun_X T_makeGCOSPtr nil (theOSPtr disposeFunctionOrTrap &key terminateOnQuit master)
  (makeGCOSPtrHelper theOSPtr disposeFunctionOrTrap terminateOnQuit nil master))

;;; A dummy function call used to prevent optimizing out the macptr reference from inside a closure.
(defun one-arg-function-returning-t (arg)
  t)

(defun makeGCOSPtrHelper (theOSPtr disposeFunctionOrTrap terminateOnQuit trapFunction master)
  (declare (ignore terminateOnQuit))  ;; now that MCL has macptr termination
  (let ((newGCOSPtr
         (if (terminable-macptr-p theOSPtr)
           theOSPtr
           (if (or (functionp disposeFunctionOrTrap)
                   (neq (symbol-package disposeFunctionOrTrap) (find-package "TRAPS")))
             (make-terminable-macptr theOSPtr
                                     (if master
                                       #'(lambda (macptr)
                                           (and (or (eq (ccl::%type-of macptr) 'ccl::dead-macptr)
                                                    (funcall disposeFunctionOrTrap macptr))
                                                (one-arg-function-returning-t master)))
                                       #'(lambda (macptr)
                                           (or (eq (ccl::%type-of macptr) 'ccl::dead-macptr)
                                               (funcall disposeFunctionOrTrap macptr)))))
             (let ((flag
                    (case disposeFunctionOrTrap
                      ;; You can use make-gcable-macptrs if termination is to be handled
                      ;; by one of the four termination traps covered by built-in code.
                      (#_DisposePtr ;(print "ptr")
                       ccl::$flags_DisposPtr)
                      (#_DisposeHandle ;(print "handle")
                       ccl::$flags_DisposHandle)
                      (#_DisposeWindow
                       ccl::$flags_DisposWindow)
                      (#_DisposeGWorld
                       ccl::$flags_DisposGWorld)
                      (t nil))))
               (if flag
                 (ccl::%setf-macptr (ccl::make-gcable-macptr flag) theOSPtr)
                 ;;If the desired termination is a function or some other trap, then you must
                 ;; use the more general (and less efficient) make-terminable-macptr
                 (let ((disposefunc (or (gethash disposeFunctionOrTrap *termination-trap-wrappers*)
                                        (setf (gethash disposeFunctionOrTrap *termination-trap-wrappers*)
                                              (or trapFunction
                                                  (eval `(function
                                                          (lambda (theOSPtr)
                                                            (require-trap ,disposeFunctionOrTrap theOSPtr)
                                                            t))))))))
                   (make-terminable-macptr
                    theOSPtr
                    (if master
                      #'(lambda(macptr)
                          (and (or (eq (ccl::%type-of macptr) 'ccl::dead-macptr)
                                   (funcall disposefunc macptr))
                               (one-arg-function-returning-t master)))
                      disposefunc)))))))))
    #+ignore ;; should be taken care of by MCL's macptr termination
    (when terminateOnQuit
      (pushnew newGCOSPtr (ccl::population-data mf::*macptrs-to-terminate-on-quit*)))
    newGCOSPtr))

#|
  "The argument is an existing ordinary handle (not a GCOSPtr).
It is an error if theHandle is not actually a handle.
Returns a new GCHandle copy of handle."
|#
(defmacro handleToGCHandle (handle &key terminationFunction terminateOnQuit master)
  `(makeGCOSPtrHelper ,handle
                      ,(if terminationFunction
                         terminationFunction
                         ''#_DisposeHandle)
                      ,terminateOnQuit
                      ,(if terminationFunction
                         `#'(lambda (x)
                              (require-trap ,terminationFunction x)
                              t)
                         `#'(lambda (x)
                              (require-trap #_DisposeHandle x)
                              t))
                      ,master))
  
(defun_X T_handleToGCHandle nil (handle &key terminationFunction terminateOnQuit master)
  (makeGCOSPtrHelper handle
                     (or terminationFunction '#_DisposeHandle)
                     terminateOnQuit
                     nil
                     master))

#|
    "The argument is an existing ordinary mac pointer (not a GCOSPtr).
It is an error if thePtr is not actually a mac pointer.
Returns a new GCPtr copy of ptr."
|#
(defmacro ptrToGCPtr (ptr &key
                            terminationFunction
                            terminateOnQuit
                            master)
  `(makeGCOSPtrHelper ,ptr
                      ,(if terminationFunction
                         terminationFunction
                         ''#_DisposePtr)
                      ,terminateOnQuit
                      ,(if terminationFunction
                         `#'(lambda (x)
                              (require-trap ,terminationFunction x)
                              t)
                         `#'(lambda (x)
                              (require-trap #_DisposePtr x)
                              t))
                      ,master))
  
(defun_X T_ptrToGCPtr nil (ptr &key terminationFunction terminateOnQuit master)
  (makeGCOSPtrHelper ptr
                     (or terminationFunction '#_DisposePtr)
                     terminateOnQuit
                     nil
                     master))

(defun_X T_deactivateGCOSPtr nil (gcOSPtr &key disposeNow)
  "If gcOSPtr has an associated termination action,
cancel that action and return t; else return nil.
If dispose-now is t, call the termination action
before canceling it.
Raise an error if gcOSPtr is not a macptr."
  (declare (ignore disposenow))
  (ccl::deactivate-macptr gcOSPtr #|:dispose-now disposeNow|# ))


#|  Shouldn't the new macptr termination in MCL3.0 take care of this?

(defun terminate-macptrs-on-quit ()
  (mapc #'(lambda (osptr)
            (T_deactivateGCOSPtr osptr :disposeNow t))
        (ccl::population-data mf::*macptrs-to-terminate-on-quit*)))

(pushnew #'terminate-macptrs-on-quit mf::*quit-functions*)

;; (SK8-declare-syms :SK8 :public
;;                   #~SK8::registerForDisposalOnQuit)
                  
(defun_X registerForDisposalOnQuit nil (theMemPointer)
  (unless (terminable-macptr-p theMemPointer)
    (error "not a MemPointer"))
  (pushnew theMemPointer (ccl::population-data mf::*macptrs-to-terminate-on-quit*)))
|#

#| ; some tests
(defun foo (x) x)
(setf disposeHandle '#_DisposeHandle)
(setf disposeHandleBad #_DisposeHandle)

(macroexpand-1 '(makeGCOSPtr h #_DisposeRgn))
(macroexpand-1 '(makeGCOSPtr h #_DisposePtr))
(macroexpand-1 '(makeGCOSPtr h #_DisposeHandle))
(macroexpand-1 '(makeGCOSPtr h 'foofunc))
(macroexpand-1 '(makeGCOSPtr h #'foofunc))
(macroexpand-1 '(handleToGCHandle h))
(macroexpand-1 '(ptrToGCPtr       h))

(progn
  (defvar h)
  (defvar gh1)
  (defvar gh2)
)

;;; test the makeGCOSPtr macro
(progn
  (setf h (#_NewHandle 3))
  (setf gh1 (makeGCOSPtr h #_DisposeHandle))
  (print (not (eq h gh1)))

  ;; This next bunch: you'd better do all or nothing or you can crash
  (setf gh2 (makeGCOSPtr gh1 #_DisposeHandle)) ; gets another reference to the same GCOSPtr
  (print (eq gh1 gh2))
  (T_deactivateGCOSPtr gh1)
  (setf gh2 nil)
  (print h) ; #<A Mac Handle, Unlocked, Size 3 #x253AC80> - address doesn't matter
  (gc)
  (print h) ; #<A Mac Handle, Unlocked, Size 3 #x253AC80>

  (setf gh2 (makeGCOSPtr gh1 #_DisposeHandle))
  (print (not (eq gh1 gh2)))
  (setf gh2 nil)
  (print h) ; #<A Mac Handle, Unlocked, Size 3 #x253AC80>
  (gc)
  (print h) ; #<A Mac Non-zone Pointer #x253AC80>
  )

;;; test the makeGCOSPtr macro with a Rgn
(progn
  (setf h (#_NewRgn))
  (setf gh1 (makeGCOSPtr h #_DisposeRgn))
  (print (not (eq h gh1)))

  ;; This next bunch: you'd better do all or nothing or you can
  (setf gh2 (makeGCOSPtr gh1 #_DisposeRgn))
  (print (eq gh1 gh2))
  (setf gh1 nil)
  (setf gh2 nil)
  (print h) ; #<A Mac Handle, Unlocked, Size 10 #x253AC80> - address doesn't matter
  (gc)
  (print h) ; #<A Mac Non-zone Pointer #x253AC80>
  )

;;; test the T_makeGCOSPtr function
(progn
  (setf h (#_NewHandle 3))
  (setf gh1 (T_makeGCOSPtr h '#_DisposeHandle))
  (print (not (eq h gh1)))
  (setf gh2 (T_makeGCOSPtr gh1 '#_DisposeHandle))
  (print (eq gh1 gh2))
  (setf gh1 nil)
  (setf gh2 nil)
  (print h) ; #<A Mac Handle, Unlocked, Size 3 #x253AC80> - address doesn't matter
  (gc)
  (print h) ; #<A Mac Non-zone Pointer #x253AC80>
  )

;;; test the T_makeGCOSPtr function with a Rgn
(progn
  (setf h (#_NewRgn))
  (setf gh1 (T_makeGCOSPtr h '#_DisposeRgn))
  (print (not (eq h gh1)))
  (setf gh2 (T_makeGCOSPtr gh1 '#_DisposeRgn))
  (print (eq gh1 gh2))
  (setf gh1 nil)
  (setf gh2 nil)
  (print h) ; #<A Mac Handle, Unlocked, Size 10 #x253AC80> - address doesn't matter
  (gc)
  (print h) ; #<A Mac Non-zone Pointer #x253AC80>
  )

;;; DANGER
;;; evaluate these forms before messing with the rest of the tests

(defmacro make-FastGCOSPtr (theOSPtr flag)
  (declare (ignore theOSPtr)) `(print ,flag))

(makeGCOSPtr h '#_DisposePtr)
(makeGCOSPtr h '#_DisposeHandle)
(makeGCOSPtr h disposeHandle)
(makeGCOSPtr h disposeHandleBad) ; should raise an error
(makeGCOSPtr h #_DisposeMovie) ; should raise an error
(makeGCOSPtr h #'foo)

(defun foo1 (x trap) (makeGCOSPtr x trap))
(foo1 h '#_DisposeRgn)
(foo1 h '#_DisposePtr)
(foo1 h #_DisposeRgn) ; should raise an error
(foo1 h disposeHandle)
(foo1 h disposeHandleBad) ; should raise an error
(foo1 h #'foo)
(hash-table-to-alist *termination-trap-wrappers*)
(funcall (gethash 'DisposeMoviex *termination-trap-wrappers*) 1)
(inspect (gethash '#_DisposeRgn *termination-trap-wrappers*))
|#

#|
	Change History (most recent last):
	1	2/25/94	dy	
	2	3/1/94	kleiman	Add require-trap where the trap requires it
	3	3/1/94	dy	comment out the macro versions of things
	4	3/3/94	sidney	SK8-Declare-syms in non-header file should not specify a category
	5	3/3/94	sidney	...but they should be in the correct format!
	6	3/23/94	Hernan	Making t_handleToGCHandle take an optional arg
				specifying the finalization function.
	7	3/28/94	dy	add terminateOnQuit option; remove source for macro forms of various calls
	8	3/29/94	sidney	check in to clear up SourceServer glitch in this file
	9	4/8/94	sidney	deactivate-macptr should be ccl::deactivate-macptr
	10 	 9/29/94	dy      	T_makeGCOSPtr can now take a symbol for a termination function
	11 	11/28/94	dy      	gc macro mods so library folder not required
	12 	 2/10/95	dy      	tweaks to invocations of macros that reduce to make-terminable-macptr
	13 	 4/28/95	sidney  	expose registerfordisposalonquit so john lilly can call it
	2  	 1/ 4/96	dy      	Clean up some #~ stuff
	3  	 7/ 7/96	sidney  	use MCL's definition of make-terminable-macptr by doing master/slave stuff here
	2  	10/21/96	sidney  	remove some code for terminable macptrs now handled by MCL
	3  	 2/27/97	Hernan  	
|# ;(do not edit past this line!!)
