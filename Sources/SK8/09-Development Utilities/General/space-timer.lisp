;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories



(in-package :sk8dev)

;;; Don's latest function timer and space meter.
;;;
;;; Important functions to know about:
;;;   (add-timers <func-name> <func-name>...)
;;;      adds these timers and prints the functions currently timed
;;;      with no functions specified, just prints the functions currently timed
;;;   (remove-timers <func-name> <func-name>...)
;;;      removes these timers and prints the functions currently timed
;;;      with no functions specified, removes all timed-functions
;;;   (print-timer-totals)
;;;   (reset-timer-totals)

;;; When non-NIL, prints an entry every time a timed function is called.
(defparameter *timer-print-individual-calls* t)

;;; When non-NIL, prints the arguments each time a timed function is called.
(defparameter *timer-print-args* nil)

(defvar *timer-vector* nil)
(defparameter *timer-vector-length* 20)

(defstruct (timer (:type vector))
  func
  start-time   
  last-start   
  (excl-time      0)
  start-heap
  last-heap
  (excl-heap      0)
  (overhead-time  0)
  (overhead-heap  0))
  

;;; hash table is used only for summing the total time and consing.
(defvar *timer-hash-table* (make-hash-table))

(defstruct (entry (:type vector))
  (excl-time 0)
  (incl-time 0)
  (excl-heap 0)
  (incl-heap 0)
  (n-times   0))


(defvar *timer-depth* 0)


(defun initialize-timers ()
  (unless *timer-vector*
    (setf *timer-vector* (make-array *timer-vector-length*))
    (dotimes (i (length *timer-vector*))
      (setf (aref *timer-vector* i) (make-timer))))
  (dotimes (i (length *timer-vector*))
    (let ((timer (aref *timer-vector* i)))
      (setf (timer-func           timer) nil)
      (setf (timer-start-time     timer) nil)
      (setf (timer-last-start     timer) nil)
      (setf (timer-excl-time      timer) 0) 
      (setf (timer-overhead-time  timer) 0)
      (setf (timer-start-heap     timer) nil)
      (setf (timer-last-heap      timer) nil)
      (setf (timer-excl-heap      timer) 0)
      (setf (timer-overhead-heap  timer) 0)))
  (setf *timer-depth* 0)
  (maphash #'(lambda (key val) 
               (declare (ignore key))
               (setf (entry-excl-time val) 0)
               (setf (entry-incl-time val) 0)
               (setf (entry-excl-heap val) 0)
               (setf (entry-incl-heap val) 0)
               (setf (entry-n-times val) 0))
           *timer-hash-table*)
  t)

(defun print-timer-totals ()
  (let (totals 
        (grand-total-time 0)
        (grand-total-heap 0))
    (maphash #'(lambda (func entry) (push (cons func entry) totals)) *timer-hash-table*)
    (setq totals (sort totals #'string< :key #'(lambda (a) (symbol-name (car a)))))
    (dolist (total totals)
      (destructuring-bind (func . entry) total
        (let ((n (entry-n-times entry)))
          (format t "~%~s:~% ~3d times" func n)
          (unless (zerop n)
            (format t "~%  ~10d mSec,  ~10D mSec avg,  excl time"
                    (entry-excl-time entry)
                    (round (entry-excl-time entry) n))
            (format t "~%  ~10d mSec,  ~10d mSec avg,  incl time"
                    (entry-incl-time entry)
                    (round (entry-incl-time entry) n))
            (format t "~%  ~10d bytes, ~10d bytes avg, excl heap"
                    (entry-excl-heap entry)
                    (round (entry-excl-heap entry) n))
            (format t "~%  ~10d bytes, ~10d bytes avg, incl heap"
                    (entry-incl-heap entry)
                    (round (entry-incl-heap entry) n)))
          (incf grand-total-time (entry-excl-time entry))
          (incf grand-total-heap (entry-excl-heap entry)))))
    (format t "~%Total: ~d mSecs, ~d bytes" grand-total-time grand-total-heap)))

(defun print-timer-totals ()
  (let (totals)
    ;; get them in alphabetical order
    (maphash #'(lambda (func entry) (push (cons func entry) totals)) *timer-hash-table*)
    (setq totals (sort totals #'string< :key #'(lambda (a) (symbol-name (car a)))))
    ;; print'm
    (format t "~% Function           Times  Total time   Average Each   Total Consing   Average Each")
    (format t "~%                           excl/incl     excl/incl       excl/incl       excl/incl")
    (format t "~%                             (mSec)        (mSec)          (bytes)        (bytes)")
    (format t "~%-----------------------------------------------------------------------------------")
    (dolist (total totals)
      (destructuring-bind (func . entry) total
        (let ((n (entry-n-times entry)))
          (format t "~%~20s ~4d" func n)
          (unless (zerop n)
            (format t "~5d/~5D   ~5d/~5d    ~6d/~6d   ~6d/~6d"
                    (entry-excl-time entry)
                    (entry-incl-time entry)
                    (round (entry-excl-time entry) n)
                    (round (entry-incl-time entry) n)
                    (entry-excl-heap entry)
                    (entry-incl-heap entry)
                    (round (entry-excl-heap entry) n)
                    (round (entry-incl-heap entry) n)))))))
  (format t "~2%")
  t)







(defun reset-timer-totals ()
  (maphash #'(lambda (key entry) 
               (declare (ignore key))
               (setf (entry-excl-time entry) 0)
               (setf (entry-incl-time entry) 0)
               (setf (entry-excl-heap entry) 0)
               (setf (entry-incl-heap entry) 0)
               (setf (entry-n-times   entry) 0))
           *timer-hash-table*)
  t)


(defun add-timers (&rest functions)
  (unless *timer-vector*
    (initialize-timers))
  (let ((non-funcs nil))
    (dolist (each functions)
      (unless (or (fboundp each) (functionp each))
        (format t "~s is not an acceptable function" each)
        (setf non-funcs t)))
    (unless non-funcs
      (flet ((add-timer-internal (func)
               (when (gethash func *timer-hash-table*)
                 (error "~s already timed" func))
               (setf (gethash func *timer-hash-table*) (make-entry))
               (eval `(advise 
                       ,func 
                       (let ((this-timer (aref *timer-vector* *timer-depth*))
                             (last-timer (when (> *timer-depth* 0) 
                                           (aref *timer-vector* (1- *timer-depth*))))
                             (nowIsTheTime        (get-internal-real-time))
                             (heap       (ccl::lisp-heap-size)))
                         (when (< *timer-depth* (length *timer-vector*))
                           (setf (timer-func          this-timer) ',func)
                           (setf (timer-start-time    this-timer) nowIsTheTime)
                           (setf (timer-last-start    this-timer) nowIsTheTime)
                           (setf (timer-excl-time     this-timer) 0)
                           (setf (timer-overhead-time this-timer) 0)
                           (setf (timer-start-heap    this-timer) heap)
                           (setf (timer-last-heap     this-timer) heap)
                           (setf (timer-excl-heap     this-timer) 0)
                           (setf (timer-overhead-heap this-timer) 0)
                           ;; pause previous timer
                           (when last-timer
                             (incf (timer-excl-time last-timer) (- nowIsTheTime (timer-last-start last-timer)))
                             (incf (timer-excl-heap last-timer) (- heap (timer-last-heap last-timer)))))
                         (incf *timer-depth*)
                         (multiple-value-prog1 (:do-it)
                           ;; End and print this one
                           ;; (excl times can ignore the overhead)
                           (when (< *timer-depth* (length *timer-vector*))
                             (let* ((end-time (get-internal-real-time))
                                    (end-heap (ccl::lisp-heap-size))
                                    (incl-time (- end-time
                                                  (timer-start-time this-timer)
                                                  (timer-overhead-time this-timer)))
                                    (excl-time (+ (- end-time 
                                                     (timer-last-start this-timer))
                                                  (timer-excl-time this-timer)))
                                    (incl-heap (- end-heap
                                                  (timer-start-heap this-timer)
                                                  (timer-overhead-heap this-timer)))
                                    (excl-heap (+ (- end-heap 
                                                     (timer-last-heap this-timer))
                                                  (timer-excl-heap this-timer))))
                               ;; update the entry
                               ;; for the summary.
                               (let ((entry (gethash ',func *timer-hash-table*)))
                                 (incf (entry-incl-time entry) incl-time)
                                 (incf (entry-excl-time entry) excl-time)
                                 (when (plusp incl-heap) (incf (entry-incl-heap entry) incl-heap))
                                 (when (plusp excl-heap) (incf (entry-excl-heap entry) excl-heap))
                                 (incf (entry-n-times entry))
                                 ;; indent and print
                                 (when *timer-print-individual-calls*
                                   (when *timer-print-args*
                                     (format-tab *timer-depth*)
                                     (format t "~s" arglist))
                                   (format-tab *timer-depth*)
                                   (format t "Timer: ~s took ~d / ~d mSec, cons'd ~d / ~d bytes" 
                                           ',func excl-time incl-time excl-heap incl-heap))
                                 (let* ((post-time      (get-internal-real-time))
                                        (overhead-time  (- post-time end-time))
                                        (post-heap      (ccl::lisp-heap-size))
                                        (overhead-heap  (- post-heap end-heap)))
                                   ;; recover lost printing time
                                   (dotimes (i *timer-depth*)
                                     (let ((each-timer (aref *timer-vector* i)))
                                       (incf (timer-overhead-time each-timer) overhead-time)
                                       (incf (timer-overhead-heap each-timer) overhead-heap)))
                                   ;; unPause last timer
                                   (when last-timer
                                     (setf (timer-last-start last-timer) (get-internal-real-time))
                                     (setf (timer-last-heap last-timer) (ccl::lisp-heap-size)))))))
                           (decf *timer-depth*)))
                       :when :around))))
        (dolist (each functions)
          (add-timer-internal each))
        (timed-functions)))))

(defun timed-functions ()
  (let (funcs)
    (maphash #'(lambda (func entry) 
                 (declare (ignore entry))
                 (push func funcs)) *timer-hash-table*)
    (sort funcs #'string< :key #'symbol-name)))


(defun format-tab (n)
  (terpri)
  (dotimes (i n)
    (princ 
     (if (oddp n) ". " "  "))))

(defun remove-timers (&rest functions)
  (flet ((remove-timer-internal (func)
           (cond ((gethash func *timer-hash-table*)
                  (remhash func *timer-hash-table*)
                  (eval `(unadvise ,func :when :around)))
                 (t (warn "~s isn't timed" func)))))
    (cond (functions 
           (dolist (each functions)
             (remove-timer-internal each))
           (timed-functions))
          (t 
           (let ((timed-functions (timed-functions)))
             (dolist (each timed-functions)
               (remove-timer-internal each))
             (format t "~%Timers removed: ~{~s ~}" timed-functions)
             t)))))

(in-package :ccl)

(eval-when (eval compile)
  (unless (boundp 'traps::$ApplZone)
    ;; from SysEqu.lisp
    (defconstant traps::$ApplZone #x2aa)))

(defun lisp-heap-size ()
  (let ((macheap)
        (lispheap)
        (staticheap)
        (ispace)
        (gspace)
        (static-gspace)
        (static-ispace))
    (lap-inline ()
      (:variable macheap lispheap ispace gspace static-gspace static-ispace staticheap)
      (move.l (a5 $Pdynamic_cons_area) atemp1)
      (move.l (@w traps::$applzone) atemp0)
      (move.l @atemp0 dtemp0)
      (sub.l atemp0 dtemp0)
      (jsr_subprim $sp-mkulong)
      (move.l dtemp0 (varg macheap))
      (move.l (atemp1 $cons-area.ispace-end) dtemp0)
      (sub.l (atemp1 $cons-area.gspace-start) dtemp0)
      (jsr_subprim $sp-mkulong)
      (move.l dtemp0 (varg lispheap))
      (move.l (a5 $Pdynamic_cons_area) atemp1)
      (move.l (a5 $gfree) dtemp0)
      (sub.l (atemp1 $cons-area.gspace-start) dtemp0)
      (jsr_subprim $sp-mkulong)
      (move.l dtemp0 (varg gspace))
      (move.l (a5 $Pdynamic_cons_area) atemp1)
      (move.l (atemp1 $cons-area.ispace-end) dtemp0)
      (sub.l (a5 $ifree) dtemp0)
      (jsr_subprim $sp-mkulong)
      (move.l dtemp0 (varg ispace))
      (move.l (a5 $Pstatic_cons_area) dtemp0)
      (if# ne
        (move.l dtemp0 atemp0)
        (move.l (atemp0 $cons-area.gspace-end) arg_z)
        (sub.l (atemp0 $cons-area.gspace-start) arg_z)
        (jsr_subprim $sp-mkulong)
        (move.l acc (varg static-gspace))
        (move.l (a5 $Pstatic_cons_area) atemp0)
        (move.l (atemp0 $cons-area.ispace-end) arg_z)
        (sub.l (atemp0 $cons-area.ispace-start) arg_z)
        (jsr_subprim $sp-mkulong)
        (move.l acc (varg static-ispace))
        (move.l (a5 $Pstatic_cons_area) atemp0)
        (move.l (atemp0 $cons-area.ispace-end) arg_z)
        (sub.l (atemp0 $cons-area.gspace-start) arg_z)
        (jsr_subprim $sp-mkulong)
        (move.l acc (varg staticheap))))
    (+ gspace ispace)))




(defmacro with-clean-timers ((&optional print-calls print-args) &body body)
  `(prog2
    (progn (setf *timer-print-individual-calls* ,print-calls)
           (setf *timer-print-args* ,print-args)
           (reset-timer-totals))
    (progn ,@body)
    (print-timer-totals)))
   


#|
	Change History (most recent last):
	1	4/29/94	till	It's here!
	2  	 2/14/95	Hernan  	Cannot use the variable name "now".
	2  	 2/27/97	Hernan  	
|# ;(do not edit past this line!!)
