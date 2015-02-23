;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories
(in-package :ccl)

;;; ---------------------------------------------------------------------
;;; Debug printout functions that use the same indentation as trace

(export 'if-debug)
(export 'when-debug)
(export 'trace-print)
(export 'debug-print)
(export 'trace-print-bracketing)
(export 'insert-features)
(export 'remove-features)
(export 'features-active?)

(defun insert-features (&rest keywords)
  "Ensures that the given keyword(s) are in cl:*features*.
Returns a list of specified keywords that were not already in cl:*features*."
  (prog1
    (set-difference keywords cl:*features*)
    (setf cl:*features* (union keywords cl:*features*))))

(defun remove-features (&rest keywords)
  "Ensures that the given keyword(s) are not in cl:*features*.
Returns a list of specified keywords that were removed from cl:*features*."
  (prog1
    (intersection keywords cl:*features*)
    (setf cl:*features* (set-difference cl:*features* keywords))))

;;; Used at compile time and at runtime.
(defun features-active? (&rest features)
  "Returns nil if there are any features arguments and none of them
is in cl:*features*; otherwise returns non-nil."
 (or (not (first features))
     (intersection features cl-user::*features*)))
#|
(insert-features :debug-test)
;;; All non-nil
(features-active?)
(features-active? nil)
(features-active? :debug-test)
(features-active? :debug-test :never)
;;; All nil
(features-active? :never)
(features-active? :never1 :never2)
|#

;;;===========================================

(defun debugString (str)
  (with-pstrs ((theStr str))
    (#_debugstr theStr)))

(defun heapCheck (&optional (comment "This hc no comment"))
  (debugstring (concatenate 'string comment ";hc;g")))

(defun msbug (&optional (message "You're here!"))
  (debugString message))

#| TESTS
(defun printPort (&optional (mssg ""))
  (let+ ((thePort (:pointer)))
    (#_getport thePort)
    (trace-print () "~a: currentport is ~a." mssg (%get-ptr thePort))))
|#

;;;===========================================

;;; Used only at compile time to determine if debug code should be compiled in.
(defun compile-debug-code (trigger-features
                              &optional
                              (compile-override nil compile-override-set))
  (if compile-override-set
    (eval compile-override)
    (apply #'features-active? (mklist trigger-features))))

(defmacro if-debug ((&optional
                          trigger-features
                          (compile-override nil compile-override-set))
                      if-part
                      else-part)
  "The trigger-features argument is either a keyword symbol or a list of them
(not evaluated at compile time).
Generates compiled code for the if-part only if any of the following is true:
 * trigger-features is nil
 * some of the trigger-features are in *features* at compile time
 * compile-override is specified and evaluates to non-nil at compile time
If compile-override is specified and evaluates to nil at compile time,
the else-part code is generated.
The code for if-part, if compiled in, will execute only if at runtime:
 * trigger-features is nil, or
 * some of the trigger-features are in *features*."
  (if (apply #'compile-debug-code
               trigger-features
               (if compile-override-set
                 `(,compile-override)
                 '()))
    (if trigger-features
      `(if (features-active? ,@(mklist trigger-features))
        (,@if-part)
        (,@else-part))
      `(,@if-part))
    `(,@else-part)))

#|
(if-debug ()
          (print "yes")
          (print "no"))

(progn
  3
  (if-debug (:never)
            (print "yes")
            nil))

(insert-features :debug-test)
(defvar comptestt t)
(defvar comptestnil nil)

(defun try1 ()
  (if-debug ()
      (print "always 1")
      (print "never 1"))
  (if-debug (nil)
      (print "always 2")
      (print "never 2"))
  (if-debug (:debug-test)
      (print "always 3")
      (print "never 3"))
  (if-debug ((:debug-test :never))
      (print "always 4")
      (print "never 4"))
  (if-debug (:debug-test comptestt)
      (print "always 5")
      (print "never 5"))
  (if-debug (:debug-test-maybe t)
      (print "sometimes 1")
      (print "sometimes not 1"))
  (if-debug ((:debug-test-maybe :never) t)
      (print "sometimes 2")
      (print "sometimes not 2"))
  ;;; Don't compile these in.  Thus they're never printed.
  (if-debug (:debug-test-maybe)
      (print "never 1")
      (print "always 1"))
  (if-debug (:debug-test nil)
      (print "never 2")
      (print "always 2"))
  (if-debug (nil comptestnil)
      (print "never 3")
      (print "always 3"))
  (values))

(insert-features :debug-test-maybe)
(try1)
(remove-features :debug-test-maybe)
(try1)

|#

(defmacro when-debug ((&optional
                          trigger-features
                          (compile-override nil compile-override-set))
                         &body body)
  "The trigger-features argument is either a keyword symbol or a list of them
(not evaluated at compile time).
Generates compiled code for the body only if any of the following is true:
 * trigger-features is nil
 * some of the trigger-features are in *features* at compile time
 * compile-override is specified and evaluates to non-nil at compile time
If compile-override is specified and evaluates to nil at compile time,
no code is generated.
The code for body, if compiled in, will execute only if at runtime:
 * trigger-features is nil, or
 * some of the trigger-features are in *features*."
  (if (apply #'compile-debug-code
               trigger-features
               (if compile-override-set
                 `(,compile-override)
                 '()))
    (if trigger-features
      `(and (features-active? ,@(mklist trigger-features))
        ,@body)
      `(progn ,@body))
    (values)))

#|

(when-debug () (print "yes"))

(progn
  3
  (when-debug (:never)
            (print "yes")))

(insert-features :debug-test)
(defvar comptestt t)
(defvar comptestnil nil)

(defun try1 ()
  (when-debug ()
      (print "always 1"))
  (when-debug (nil)
      (print "always 2"))
  (when-debug (:debug-test)
      (print "always 3"))
  (when-debug ((:debug-test :never))
      (print "always 4"))
  (when-debug (:debug-test comptestt)
      (print "always 5"))
  (when-debug (:debug-test-maybe t)
      (print "sometimes 1"))
  (when-debug ((:debug-test-maybe :never) t)
      (print "sometimes 2"))
  ;;; Don't compile these in.  Thus they're never printed.
  (when-debug (:debug-test-maybe)
      (print "never 1"))
  (when-debug (:debug-test nil)
      (print "never 2"))
  (when-debug (nil comptestnil)
      (print "never 3"))
  (values))

(insert-features :debug-test-maybe)
(try1)
(remove-features :debug-test-maybe)
(try1)

|#

;;;===========================================

(defun debug-print-helper (arg1 &rest rest)
  (debugString (if (stringp arg1)
                 (apply #'format nil (concatenate 'string arg1  ";g")
                        rest)
                 (format nil "~S;g" arg1)))
  (values))

(defmacro debug-print ((&optional
                          trigger-features
                          (compile-override nil compile-override-set))
                         message
                         &rest rest)
  "If message is a string, then construct a string thus:
  (apply #'format nil message rest)
Otherwise,
  (format nil \"~&~S~&\" message)
The string is passed to the DebugStr trap followed by \";g\".
The trigger-features and compile-override arguments operate as
in the when-debug macro."
  `(when-debug (,trigger-features ,@(if compile-override-set `(,compile-override) nil))
      (debug-print-helper ,message ,@rest)))

#| TESTS
;;; There are many more tests for trace-print, which is equivalent.
(debug-print () "ha")
|#

;;;===========================================

(defun trace-print-helper (prepended-string arg1 &rest rest)
  (with-undisturbed-graphics
    (trace-tab (not *trace-indent-active*))
    (if (stringp arg1)
      (apply #'format *trace-output* (concatenate
                                      'string
                                      (if (or (not *trace-indent-active*)
                                              (zerop (length prepended-string)))
                                        "" "~&")
                                      prepended-string
                                      arg1
                                      "~&")
             rest)
      (format *trace-output*
              (concatenate 'string
                           (if (or (not *trace-indent-active*)
                                   (zerop (length prepended-string)))
                             "" "~&")
                           prepended-string
                           "~S~&")
              arg1))
    (stream-force-output *trace-output*))
  (values))

(defmacro trace-print ((&optional
                          trigger-features
                          (compile-override nil compile-override-set))
                         message
                         &rest rest)
  "If message is a string, then
  Add \"~&\" to the beginning and end of message.
  (apply #'format *trace-output* message rest)
Otherwise,
  (format *trace-output* \"~&~S~&\" message)
Whichever happens, it's all done while preserving the current graphics
port so it won't mess up the graphics system.
The trigger-features and compile-override arguments operate as
in the when-debug macro."
  `(when-debug (,trigger-features ,@(if compile-override-set `(,compile-override) nil))
      (trace-print-helper "" ,message ,@rest)))

#| TESTS

(trace-print () "hah ~S" 3)
(trace-print () 3)

(insert-features :debug-test)

(defun try1 ()
  (trace-print (:debug-test)
      "ha ~S" "always 1")
  (trace-print ((:debug-test :never))
      "ha ~S" "always 2")
  (trace-print (:debug-test-maybe t)
      "ha ~S" "sometimes")
  ;;; Don't compile these in.  Thus they're never printed.
  (trace-print (:debug-test-maybe)
      "ha ~S" "never 1")
  (trace-print (:debug-test nil)
      "ha ~S" "never 2")
  (values))

(insert-features :debug-test-maybe)
(try1)
(remove-features :debug-test-maybe)
(try1)

|#

;;;===========================================

(defmacro trace-print-using-extra-indent (val &body body)
  (let ((tmpVal (gensym)))
    `(let ((,tmpVal ,val))
       (let ((*trace-indent-active* (if (eq ,tmpVal :no-change)
                                      *trace-indent-active*
                                      ,tmpVal)))
         (declare (special *trace-indent-active*))
         ,@body))))

#| TESTS

(progn
   (trace-print () *trace-indent-active*)
   (trace-print-using-extra-indent t
     (trace-print () "yes"))
   (trace-print () *trace-indent-active*)
   (trace-print-using-extra-indent (listp 'x)
     (trace-print () "no"))
   (trace-print-using-extra-indent t
     (trace-print () "yes")
     (trace-print-using-extra-indent :no-change
       (trace-print () "yes"))
     (trace-print-using-extra-indent t
       (trace-print () "yes"))
     (trace-print-using-extra-indent nil
       (trace-print () "no"))
     (values)))
|#

(defun trace-before-bracket (message)
  (when message
    (push-trace-level)
    (apply #'trace-print-helper "/ " message)))

(defun trace-after-bracket (message)
  (when message
    (apply #'trace-print-helper "\\ " message)
    (pop-trace-level)))

(defmacro trace-print-bracketing-helper (message &body body)
  "Execute body.  If message evaluates to non-nil, then before and
after executing body, print a line using message as the argument or list
of arguments to trace-print, and begin the lines with indentation and bracketing
characters a la trace output."
  (let ((the-message (gensym)))
    `(let ((,the-message (mklist ,message)))
       (trace-print-using-extra-indent
         (if (null ,the-message) :no-change nil)
         (trace-before-bracket ,the-message)
         (unwind-protect
           (trace-print-using-extra-indent (if (null ,the-message) :no-change t)
             ,@body)
           (trace-after-bracket ,the-message))))))

(defmacro trace-print-bracketing ((message
                                       &optional
                                       trigger-features
                                       (compile-override nil compile-override-set))
                                      &body body)
  "Execute body.  If message evaluates to non-nil, then before and
after executing body, print a line using message as the argument or list
of arguments to trace-print, and begin the lines with indentation and
bracketing characters as in the output of trace
Compiles in the extra degugging code only if any of the following is true:
 * trigger-features is nil
 * some of the trigger-features are in *features* at compile time
 * compile-override is specified and evaluates to non-nil at compile time
If compile-override is specified and evaluates to nil at compile time,
the extra debugging code is not generated.
In any case, the body is always executed."
  (unless message
    (error "trace-print-bracketing: message must not be the constant nil"))
  (if (apply #'compile-debug-code
             trigger-features
             (if compile-override-set
               `(,compile-override)
               '()))
    (if trigger-features
      `(trace-print-bracketing-helper (and (features-active? ,@(mklist trigger-features))
                                           ,message)
         ,@body)
      `(trace-print-bracketing-helper ,message ,@body))
    `(progn ,@body)))

#| TESTS

(trace-print-bracketing-helper '("Ha: ~S" 23)
  (trace-print () "hah"))
(trace-print-bracketing-helper nil
  (trace-print () "hah"))
(trace-print-bracketing-helper ()
  (trace-print () "hah"))
(defvar message1 nil)
(setf message1 '("Ha: ~S" 23))
(trace-print-bracketing-helper message1
  (trace-print () "hah"))
(setf message1 nil)
(trace-print-bracketing-helper message1
  (trace-print () "hah"))


(trace-print-bracketing-helper nil ; nil ok - no printouts
  (print 0)
  (trace-print-bracketing-helper "outie" ; atom ok
    (trace-print () 1)
    (trace-print-bracketing-helper nil
      (trace-print () 2)
      (trace-print-bracketing-helper '("midi") ; quoted list ok
        (trace-print () 3)
        (trace-print-bracketing-helper '("innie" 2 3)
          "done")))))

;;; Now mix trace-print with trace
(defun x1 (arg)
  (trace-print () "innermost")
  arg)
(defun x2 (arg) (x1 arg))
(defun x3 (arg)
  (trace-print () "indented")
  (trace-print-bracketing-helper nil
  (trace-print-bracketing-helper "outie"
    (trace-print () 1)
    (trace-print-bracketing-helper nil
      (trace-print () 2)
      (trace-print-bracketing-helper "midi"
        (trace-print () 3)
        (trace-print-bracketing-helper "innie"
          "done"))
      (x2 arg)))))
(defun x4 (arg) (x3 arg))
(defun x5 (arg) (x4 arg))

(trace x1 x2 x3 x4 x5)

(x1 1)
(x5 1)

;;; compile-time error:
(trace-print-bracketing nil "hah")
(trace-print-bracketing (nil) "hah")

(defvar nullmessage nil)
(trace-print-bracketing (nullmessage) (trace-print () "hah"))
(defvar always1message "always 1")

(insert-features :debug-test)
(defun try2 ()
  ;;; compile these in
  (trace-print-bracketing (always1message)
    (trace-print () "boo1"))
  (trace-print-bracketing ('("always 2") :debug-test)
    (trace-print () "boo2"))
  (trace-print-bracketing ('("always ~S" 3) (:debug-test :never))
    (trace-print () "boo3"))
  (trace-print-bracketing ('("always ~S" 4) :debug-test t)
    (trace-print () "boo4"))
  (trace-print-bracketing ('("always ~S" 5) :debug-test comptestt)
    (trace-print () "boo5"))
  (trace-print-bracketing ('("sometimes ~S" 6) :debug-test-maybe t)
    (trace-print () "boo6"))
  ;;; don't compile these in
  (trace-print-bracketing ('("never ~S" 7) :debug-test-maybe)
    (trace-print () "boo7"))
  (trace-print-bracketing ('("never ~S" 8) :debug-test nil)
    (trace-print () "boo8"))
  (trace-print-bracketing ('("never ~S" 9) nil comptestnil)
    (trace-print () "boo9"))
  )

(insert-features :debug-test-maybe)
(try2)
(remove-features :debug-test-maybe)
(try2)

|#

#|
	Change History (most recent last):
	1	7/20/94	dy	
	2	7/20/94	dy	oops.  :ccl
	3  	 1/18/95	dy      	stream-force-output
	4  	 1/25/95	dy      	fix pr and add some related functions
	5  	 1/25/95	dy      	comment out printPort
	6  	 1/31/95	dy      	new trace-print-bracketing macro
	7  	 2/13/95	dy      	Added conditional compile versions: cpr, cprm, ctrace-print-bracketing
	1  	 2/20/95	dy      	evolved from pr.lisp (now defunct)
	2  	 2/20/95	dy      	cleanup
	3  	 2/22/95	dy      	cleanups
	4  	 2/24/95	dy      	Add when-debug, consolidate the other stuff
	5  	 2/26/95	dy      	new if-debug macro
	6  	 3/22/95	dy      	tweak comment for trace-print
	2  	 2/27/97	Hernan  	
|# ;(do not edit past this line!!)
