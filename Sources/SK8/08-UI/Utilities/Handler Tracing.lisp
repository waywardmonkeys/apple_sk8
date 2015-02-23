;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories

(in-package :sk8dev)

;;; This is a nicer interface to calling trace. I will not expose it to SK8Script really, 
;;; unless we have to... Users can access this functionality through the Handler Tracer window. 

#|


(tr {symbol | (symbol {keyword-argument-pair}*) }*)

;; Valid keywords for a keyword-argument-pair to TR:

:when     Each time the given function is called, this form is evaluated (the form may refer
          to the ARGS variable -- a list of the arguments passed to the function).  If it
          returns a non-nil value the trace is invoked.

:step     Each time this trace is invoked, this form is evaluated (the form may refer to the ARGS
          variable -- a list of the arguments passed to the function).  If it returns a non-nil
          value the function is executed in the stepper (only works when function def was saved).

:callers  Each time this trace is invoked, this form is called for every item in the calling chain.
          If you are just printing to the listener, t would do the right thing. The form can access
          the variable STACKFUN which will be bound to each item in the calling chain. 

:before   Each time this trace is invoked, this form is evaluated before the function-call takes
          place (the form may refer to the ARGS variable -- a list of the arguments passed to the
          function).

:after    Each time this trace is invoked, this form is evaluated after the function-call has taken
          place (the form may refer to the VALS variable -- a list of the values returned by the
          function).

:object   A SK8 object. If provided, only the method of the object is traced. 

;; For example:

(tr (bob :when (eq (first args) t)
         :object number
         :before (break)
         :step (minusp (second args))
         :callers t))

|#





#|

;; Some tests...

(defun bob (a)
  (setq a (* 2 a))
  (when (> a 100)
    (setq a 100))
  a)


(tr (bob :when (> (first args) 3)
         :before (set-mini-buffer *top-listener* "Yo!  Bob's being called")
         :after (set-mini-buffer *top-listener* "")
         :step (= (first args) 99)
         :callers (< (first args) 10)
         ))

(bob 4)

|#

;;; If this is t, things traced to the system log also print to the listener. 

(defparameter *trace-output-to-listener* nil)

(defun tr-print-calling-chain (callers-fun)
  (let* (left-edge
         line-len
         tab-string
         pending
         (i 1)
         (stop-point 2)
         (n (loop (unless (ccl::%last-fn-on-stack i) (return (1- i))) (incf i)))
         temp len stackFun)
    (when *trace-output-to-listener*
      (setf left-edge (stream-column *trace-output*))
      (setf line-len (- (stream-line-length *trace-output*) left-edge 1))
      (setf tab-string (make-string (+ left-edge 2) :initial-element #\Space))
      (setf (char tab-string 0) #\Newline)
      (setf pending "")
      )
    (flet ((write-some-text (str)
             (setq temp (concatenate 'simple-string pending str)
                   len (length temp))
             (cond
              ((and (> len line-len) (> (length pending) 0))
               (princ pending *trace-output*)
               (princ tab-string *trace-output*)
               (setq pending str))
              ((>= len line-len)
               (princ (subseq temp 0 line-len) *trace-output*)
               (princ tab-string *trace-output*)
               (setq pending (subseq temp line-len)))
              (t
               (setq pending temp)))))
      (when *trace-output-to-listener* (trace-tab))
      (loop
        (when (< (decf n) stop-point) (return))
        (setf stackFun (ccl::%last-fn-on-stack n))
        (when *trace-output-to-listener*
          (write-some-text (or (prin1-to-string (function-name stackFun)) "*"))
          (write-some-text (if (> n stop-point) " -> " " :")))
        (funcall callers-fun stackFun))
      (when *trace-output-to-listener* (princ pending *trace-output*)))))

(defun tr-before-fun (invoked?-sym invoked?-fun callers?-fun before-fun function-and-args)
  (let ((args (cdr function-and-args))
        (function (car function-and-args)))
    (set invoked?-sym
         (when (or (null invoked?-fun) (funcall invoked?-fun args))
           (when (and callers?-fun)
             (tr-print-calling-chain callers?-fun))
           (when *trace-output-to-listener*
             (trace-tab)
             (format *trace-output* "~A invoked with ~A~%" function args)
             (stream-force-output *trace-output*))
           (when before-fun (funcall before-fun function args))
           t))))

(defun tr-after-fun (invoked?-sym after-fun function values)
  (when (symbol-value invoked?-sym)
    (let ((num-values (length values)))
      (when *trace-output-to-listener*
        (trace-tab)
        (cond ((eq num-values 1)
               (format *trace-output* "~A returned ~A~%" function (car values)))
              (t
               (format *trace-output* "~A returned ~A values :~%" function num-values)
               (dolist (val values)
                 (trace-tab) (format *trace-output* "    ~A~%" val))))
        (stream-force-output *trace-output*))
      (when after-fun (funcall after-fun function values)))))

(defvar *tr-funs* nil)

(defun tr-get-fun-syms-entry (function-name &optional no-create)
  (let ((entry (assoc function-name *tr-funs*)))
    (unless (or entry no-create)
      (setq entry (list function-name (make-symbol "BF") (make-symbol "AF") (make-symbol "SF")))
      (push entry *tr-funs*))
    (cdr entry)))

(defun tr-dispose-fun-syms-entry (function-name)
  (let ((entry (assoc function-name *tr-funs*)))
    (when entry
      (dolist (fun-sym (cdr entry))
        (fmakunbound fun-sym))
      (setq *tr-funs* (delete entry *tr-funs*)))))

(defun tr-dispose-unused-funs (symbols)
  (dolist (sym symbols)
    (when (consp sym) (setq sym (car sym)))
    (when (symbolp sym)
      (tr-dispose-fun-syms-entry sym))))

(defun trace-spec-from-tr-spec (spec)
  (when (symbolp spec) (return-from trace-spec-from-tr-spec spec))
  (let* ((function-name (car spec))
         (args (cdr spec))
         (obj (getf args :object))
         (when-form (getf args :when))
         (step-form (getf args :step))
         (callers-form (getf args :callers))
         (before-form (getf args :before))
         (after-form (getf args :after))
         
         (fun-syms-entry (tr-get-fun-syms-entry function-name))
         (before-fun-sym (first fun-syms-entry))
         (after-fun-sym (second fun-syms-entry))
         (step-fun-sym (third fun-syms-entry))
         
         (invoked?-sym (make-symbol "INV?"))
         
         (CCL::*SUPPRESS-COMPILER-WARNINGS* T)
         (*SAVE-DEFINITIONS* NIL)
         (*SAVE-LOCAL-SYMBOLS* NIL)

         (when-fun (when when-form
                     (eval `(function
                             (lambda (args)
                               (declare (ignore-if-unused args))
                               ,when-form)))))
         (callers-fun (when callers-form
                        (eval `(function
                                (lambda (stackFun)
                                  (declare (ignore-if-unused stackFun))
                                  ,callers-form)))))
         (before-form-fun (when before-form
                            (eval `(function
                                    (lambda (function args)
                                      (declare (ignore-if-unused args))
                                      ,before-form)))))
         (after-form-fun (when after-form
                           (eval `(function
                                   (lambda (function vals)
                                     (declare (ignore-if-unused args))
                                     ,after-form)))))
         
         (before-fun (eval `(defun ,before-fun-sym (&rest fun-and-args)
                              (tr-before-fun ',invoked?-sym
                                             ,when-fun ,callers-fun ,before-form-fun
                                             fun-and-args))))
         (after-fun (eval `(defun ,after-fun-sym (fun &rest vals)
                             (tr-after-fun ',invoked?-sym
                                           ,after-form-fun fun vals))))
         (step-test-fun (when step-form
                          (eval `(defun ,step-fun-sym (fun &rest args)
                                   (declare (ignore fun)
                                            (ignore-if-unused args))
                                   (and ,when-form ,step-form))))))
    `(,(if obj
         (handlerIdToHandlerObject (list (project obj) function-name obj))
         function-name)
      :before ,before-fun
      :after ,after-fun
      ,@(when step-test-fun `(:step ,step-test-fun)))))


(defmacro tr (&rest trace-specs)
  (cons 'trace (mapcar 'trace-spec-from-tr-spec trace-specs)))

(defmacro untr (&rest symbols)
  (let ((thingsToUntrace (gensym)))
    `(let ((,thingsToUntrace nil))
       (unless ',symbols (setf ,thingsToUntrace (trace)))
       (untrace ,@symbols)
       (tr-dispose-unused-funs (or ',symbols ,thingsToUntrace)))))

(export '(tr untr) :sk8dev)
(import '(tr untr) :sk8)
(import '(tr untr) :uidev)


#|
	Change History (most recent last):
	2  	 5/10/96	Hernan  	Adding callers functionality.
	3  	 5/10/96	Hernan  	Fixed untr to actually untrace methods that we are tracing.
	4  	11/26/96	Brian   	Fixing calls which use *trace-output* to only 
						happen when we are outputting to the listener.
						Otherwise a empty listener comes up.
	5  	 2/27/97	Hernan  	
|# ;(do not edit past this line!!)
