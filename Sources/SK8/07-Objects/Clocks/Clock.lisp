;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories


;;;

(in-package :sk8dev)

(provide "CLOCK")

(require "ABSTRACTCLOCK" "objects;Clocks:AbstractClock")

;;; I have reimplemented the clocks to be a lot simpler and have integrated them with the whole system.
;;; Hernan.

;;; Clocks can work in the following ways:

;;; * Real time clocks: specify the number of ticks per second they want.
;;; * Normal Clocks: specify a startTime and an endTime in the time frame of
;;;              its master clock.
;;; * slave clocks: specify their ticks by the number of master ticks that have
;;;             to happen before they tick.

;;; ____________
;;; Helpers.
;;; ____________

(defmacro time-in-interval (tim start end)
  `(cond ((null ,tim) nil)
         ((and (numberp ,start) (numberp ,end))
          (<= ,start ,tim ,end))
         ((eq ,start t)
          (or (eq ,end t) (<= ,tim ,end)))
         (t
          (>= ,tim ,start))))

(defun activate-masters (theclock)
  (when theclock
    (setf (slot-value theclock 'running) t)
    (activate-masters (master theclock))))

(defun slaves-running? (theclock)
  (dolist (c (slaves theclock) nil)
    (when (running c)
      (return-from slaves-running? t))))

(defun deactivate-masters (theclock)
  (when theclock 
    (unless (or (memq theClock mf::*clocks-running*) (slaves-running? theclock))
      (setf (slot-value theclock 'running) nil)
      (deactivate-masters (master theclock)))))

;;; _________
;;; The native SK8 clock
;;; _________

(new AbstractClock :objectname "Clock" :project sk8 :undisposable t
     :properties '((startTime :value 0)    ;; start of local time frame.
                   (endTime :value nil)    ;; end of local time frame.
                   (timePerTick :value 1)  ;; increment per tick.
                   (currentTime :value 0)  ;; the current time of the clock.
                   (master :value nil)
                   (slaves :value nil)
                   (running :value nil)
                   (masterOffset :value nil) ;; time in master's time frame at which
                   ;; this clock is started.
                   (ticking :value nil)    
                   ))

(define-handler start (Clock)
  (setf (running me) t))

(define-handler stop (Clock)
  (setf (running me) nil))

;;; MAYBE-TICK -- updates the internal state and ticks the clock if appropriate. This default
;;;             method always ticks the clock.

(define-handler maybe-tick :private (Clock)
  ;; [1] Should we tick?
  (if (ticking me)
    ;; If it is ticking, check if it should be stopped.
    (when (and (endTime me) (> (currentTime me) (endTime me)))
      (setf (ticking me) nil))
    ;; If it is not ticking, check if its master's time has crossed to the start
    ;; time of this clock.
    (when (> (currentTime (master me)) (masterOffset me))
      (setf (ticking me) t)))
  ;; [2] Update state and tick slaves.
  (when (ticking me)
    (let ((endTime (endTime me)))
      ;; Increment the time and tick!
      (setf (slot-value me 'currenttime) (+ (currenttime me) (timePerTick me)))
      ;; If the time is up. Stop running. Otherwise tick it.
      (if (and endTime (> (currentTime me) endTime))
        (stop me)
        (tick me)))))

;;; TICK -- calls maybe-tick on every one of its slaves.

(define-handler tick (Clock)
  (dolist (c (slaves me))
    (when (running c)
      (maybe-tick c))))

;;; Set Running -- adds the clock to the hierarchy of running clocks. Only this clock is added to
;;;            the *clocks-running* variable.

(define-handler (setf running) (boolean Clock)
  (if boolean
    ;; Add it to the clocks running. Also have to make all its masters run.
    (progn 
      (pushnew me mf::*clocks-running*)
      (activate-masters me))
    ;; Remove it from the clocks running and deactivate masters.
    (progn 
      (setf mf::*clocks-running* (remove me mf::*clocks-running*))
      (setf (slot-value me 'running) nil)
      (deactivate-masters (master me)))))

(define-handler (setf master) (master Clock)
  (let ((old (master me)))
    (when old
      (setf (slaves old) (delete me (slaves old))))
    (when master
      (unless (memq me (slaves master))
        (setf (slaves master) (nconc (slaves master) (list me))))
      (setf (slot-value me 'master) master)
      (setf (masterOffset me) nil))))

;; deactivate clocks when they are being disposed

(define-handler make-object-gcable :hidden (Clock)
  (setf (running me) nil)
  (call-next-method))

;;; ______________
;;; The System Clock: what makes everyone tick.
;;; ______________

(new Clock :objectname "SystemClock" :project sk8 :undisposable t)

(define-handler new (SystemClock &rest ignore)
  (declare (ignore ignore))
  (error "The SystemClock cannot be instantiated."))

(define-handler maybe-tick :private (systemClock)
                (setf (currentTime me) (+ (currentTime me) (timePerTick me)))
                (tick me))

(define-sk8-function tickEventClock nil ()
  (when sk8dev::*quickTime-callBacks-to-activate*
    (sk8dev::activate-quicktime-callbacks))
  (when (running systemClock)
    (maybe-tick systemClock))
  (when sk8dev::*active-movie-controllers*
    (sk8dev::movie-controllers-idle))
  (when sk8dev::*active-movies-without-controllers*
    (sk8dev::movies-task))
  (when sk8dev::*objects-that-want-lowLevelIdle*
    (sk8dev::dispatch-lowLevelIdle)))



#|
	Change History (most recent last):
	1	6/25/93	Seitz	Clock object
	2	6/26/93	kleiman	fixed typos which crashed build
	4	7/1/93	Seitz	
	10	8/13/93	kleiman	update Clock
	11	8/13/93	kleiman	changed default masterOffset and start-offset to true.
	12	9/22/93	kleiman	removed indiscriminate uses of slot-value
	13	10/1/93	hernan	:properties enhancement
	14	11/19/93	kleiman	ss::*event-clock* -> mf::*event-clock*
	15	1/6/94	hernan	Replacing all the old clock mess with a very simple
				new thing that seems to work.
	16	1/10/94	sidney	
	18	1/17/94	hernan	Creating a function that can be called by the user
				(from SK8Script) to tick the system clock. This
				would be called from a tight loop.
	20	2/25/94	dy	tickEventClock services QuickTime now
	21	2/28/94	hernan	Commenting out dispose.
	22	3/3/94	kleiman	private properties declared via makeprivateproperty
	23	3/14/94	kleiman	makePrivateProperty -> (setf (private ...
	24	4/1/94	Hernan	Adding a preserve of realTimeClock that sets the
				previousRealTime to 0. This guarantees that when
				we come up the real-time is > than the saved one
				and the clock ticks when it has to.
	25	6/7/94	Hernan	Putting the right initial value in realTimeClock so
				that the system does not crash when you try to 
				use it.
	26	6/17/94	kleiman	Old Clock renamed SK8Clock which inherits from
				an abstract new Clock
	27	7/8/94	Hernan	1172745: The ticksPerSecond of realTimeClock
				should be set after the setter is defined so that
				the increment is initialized to the right thing.
	28	7/26/94	Hernan	1176810: the set running method of realTimeClock
				calls next method after doing the required set up
				so that the master is correctly activated.
	29 	 9/28/94	Hernan  	clock -> AbstractClock and SK8Clock -> Clock.
	30 	 9/29/94	dy      	new AbstractClock instead of new Clock
	31 	 9/30/94	Hernan  	You cannot instantiate the systemClock.
	32 	10/ 6/94	It      	touched to force recompile
	33 	10/25/94	Hernan  	Setf master has to deal with the case when the
							master is set to false.
	34 	11/ 7/94	Hernan  	Fixed the realtimeClock bug. The previousRealTime
							has to be initialized to 0 at load time.
	35 	12/19/94	Hernan  	the if statement in maybe-tick of clock is upside
							down!
	36 	 3/13/95	dy      	tickEventClock now watches for *quickTime-callBacks-to-activate*
	37 	 4/18/95	Hernan  	1240797: the currentTime of SecondsClock = seconds of Now.
	2  	 6/23/95	sidney  	stop a clock that is being destroyed (e.g., when its project is closed)
	3  	 8/ 2/95	Hernan  	Adding support for lowLevelIdle.
	4  	 8/17/95	sidney  	can't use constant 'now' as local variable
~~	2  	 4/10/96	Hernan  	define-system-handler->define-handler.
	4  	 2/27/97	Hernan  	Putting define-sk8-function back in the code.
|# ;(do not edit past this line!!)
