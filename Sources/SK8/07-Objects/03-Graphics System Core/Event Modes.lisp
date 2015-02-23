(in-package :SK8Development)

(provide "EVENTMODE")

;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories



#| Modification History

03-19-93 ruben activeMode, forceExitCurrentMode -> forceExitActiveMode
03-10-93 ruben *current-mode* global and lastMode property obsoleted, modes now collected in a stack
               key removed from mode keywords
01-22-93 ruben id -> objectName

|#

;;; *modes-stack* -- a stack of processes for EventModes in current use first is the current one
;;; each process has a global in its environment pointing to the actual eventmode object
;;; Not all eventmodes have their own processes. *modes-stack* may contain eventmode objects
;;; in that case.
;;;

(defvar *modes-stack* nil)

(new object
     :objectName "EventMode"
     :undisposable t
     :project sk8
     :properties '(callingProcess inUseFlag))

;; prototype stub - most eventModes will do nothing here
(define-handler eventTick (eventMode)
  (declare (ignore me))
  )

;; prototype stubs for event handlers for eventmodes
;; Define a handler for each of the events that an eventmode may handle. If the mode wants
;; to allow normal processing of the event, return nil and it will be passed through.
;; If the mode handles the event itself, return T to indicate not to pass it on.

(define-handler handleIdle (eventMode)
  (declare (ignore me))
  nil)

(define-handler handleMouseDown (eventMode)
  (declare (ignore me))
  nil)

(define-handler handleMouseUp (eventMode)
  (declare (ignore me))
  nil)

(define-handler handleKeyDown (eventMode)
  (declare (ignore me))
  nil)

(define-handler handleKeyUp (eventMode)
  (declare (ignore me))
  nil)

(define-handler handleAutoKey (eventMode)
  (declare (ignore me))
  nil)

(define-handler handleUpdate (eventMode)
  (declare (ignore me))
  nil)

(define-handler handleActivate (eventMode)
  (declare (ignore me))
  nil)

(define-handler handleOther (eventMode)
  (declare (ignore me))
  nil)


(define-sk8-function activeMode nil ()
  (car *modes-stack*))

(define-sk8-function CurrentEventModes nil ()
  (copy-list *modes-stack*))

(define-sk8-function previousEventMode nil ()
  (cadr *modes-stack*))

(define-handler enterMode (eventMode)
  (when (inUseFlag me)
    (error "Internal error: Trying to re-enter eventMode ~s" me))
  (let ((oldMode (activeMode)))
    (when oldMode
      (suspend oldMode)))
  (setf (inUseFlag me) t)
  (push me *modes-stack*)
  (activate me))

;; no checking that this is the current mode?
;; I don't know what this is supposed to do, but since most of the calls are to dummy stubs,
;; I'll ignore it for now as long as everything works
(define-handler exitMode (eventMode)
  (unless (eq (activeMode) me)
    (error "Internal error: Exit of event mode ~s out of synch with Enter of event mode ~s" me (activeMode)))
  (deactivate me)
  (when (eq (activeMode) me)
    (setf *modes-stack* (cdr *modes-stack*)))
  (setf (inUseFlag me) nil
        (callingProcess me) nil)
  (let ((newMode (activeMode)))
    (when newMode
      (resume newMode))))

(define-sk8-function forceExitCurrentMode nil ()
  (let ((curmode (activeMode)))
    (when curmode (exitMode curmode))))

(setf (symbol-function 'forceExitActiveMode)
      (symbol-function 'forceExitCurrentMode))

(define-handler resume (eventMode)
  t)

(define-handler suspend (eventMode)
  t)

(define-handler activate (eventMode)
  t)

(define-handler deactivate (eventMode)
  t)

(define-sk8-function CurrentEventKey nil ()
  (gs:getEventKey))

;;; ______________________
;;; Modal states
;;; ______________________


#|
entering a modal state requires
  1) there must be something (such as a dialog) to handle events and exit the mode at the right time
  2) tell the event handler that we have a current mode which should receive events and what handling object to send them to
  3) put the current process in a state that allows the events to be processed
  4) have a way to return a value from the exiting modal object to the current process
If the handling object is in a different process, we can simply tell the event handler about
the mode and use process-wait to let it happen. How then, do we get the returned value?
If the handling object is in the same process, then we have to call the process loop and
somehow get it to return with a value.

Let's try this: entermodalState creates a new process for the eventMode.
The process is placed in a property of the eventMode and the eventMode is pushed on a global
to make it the currentMode. Entermodalstate sets up a catch for :exitmode and suspends
its process to wait for the modal process to signal that it is finished and return a value.
The event dispatcher checks if there is a current mode. If there is, events are dispatched
to the handlers of the current mode in the process of the mode by sending stuff to the
event queue of that process. The modal event handler exits the mode by calling exitModalState.
We have at least two possible ways of implementing exitModalState. One is to have entermodalState
set a variable in the modal process to its own process, set up a catch of :exitmode, and
suspend its process. Then exitModalState can use process-interrupt to trigger a throw. The
other way is to have a per-process global in the modal process that the calling process can
poll using process-wait. The global would contain a flag indicating that the mode is done and
a value to return. In either case, it is up to the calling process to kill the modal process
after it has received the return value.

I'll go for the interrupt model, rather than the polling model, i.e., have exitModalState
force a throw in the calling process. This is a pretty arbitrary choice and it should be
easy to do it the other way if some reason appears to do so.

Note that all of this implies that it will not work to call enterModalState while in the
MCL low level event handling process ("Initial"). If we needed to we could test for that
condition and call event-poll instead of using process-wait. Maybe once it works for other
processes, I'll add that in.

This is a code fragment that halts the current process and allows another process to resume
it by causing it to throw to a certain tag:

(catch :exitmode
  (process-enable-arrest-reason *current-process* :wait-for-modal)
)

This is the corresponding code that can be used by the other process to resume the first:

 (process-interrupt p #'(lambda() (throw :exitmode :my-value)))
 (process-disable-arrest-reason p :wait-for-modal)

[later]

It's a lot easier to use *eventhook* to check for eventMode processing and to do normal
window-event processing (e.g., call the mouse down handler) when the eventmode wants to
simply pass an event through. But even the standard window-event processing has to be
done in the same thread as the special eventmode handling, to ensure that events are
processed in the correct order. I think that this means that we might as well process the
eventMode events in just the default *window-process*. We can make this work even when
something running in that process tries to enter a modal state by having enterModalState call
the standard window-loop instead of suspending the process. That call to window-loop will
be interrupted by the throw to :exitmode caused by exitModalState. This will work as long
as the MCL event process does not call enterModalState, a reasonable restriction.


|#

(define-handler enterModalState (eventMode)
  (when (inUseFlag me)
    (error "Internal error: Trying to re-enter eventMode ~s" me))
  (setf (callingProcess me) *current-process*)
  (let (value)
    (unwind-protect 
      (progn
        (setf value
              (with-event-processing-enabled
                (let-globally ((mf::*events-on* t)) ;; we have to be able to process events to get out of here
                    (enterMode me) ;; tell event processor to start sending events to mode's process
                    (catch :exitmode
                      (catch :abort
                        (restart-case
                          (cond ((or (eq *current-process* gs::*window-process*)
                                     (eq *current-process* gs::*sk8-system-process*))
                                 (gs::windowloop))
                                ((eq *current-process* ccl::*event-processor*)
                                 (loop
                                   (process-wait "Event-poll" #'ccl::event-available-p)
                                   (event-dispatch)))
                                (t (process-enable-arrest-reason *current-process* :wait-for-modal) ;; mode will turn us back on and cause a throw
                                   ))
                          (abort () :cancel)
                          (abort-break () :cancel))
                    )))))
        (when (eq value :cancel)
          (throw :cancel :cancel)))
      (exitMode me)
      (setf (callingProcess me) nil))
    value))

(define-sk8-function exitModalState nil (&optional value)
  (declare (dynamic-extent value))
  (let ((curmode (SK8:ActiveMode)))
    (when curmode
      (let ((p (callingProcess curmode)))
        (when p
          (process-interrupt p #'(lambda() (throw :exitmode value)))
          (process-disable-arrest-reason p :wait-for-modal)
          (process-allow-schedule))))))

;; now we can enable the event hook for all this
(gs::turn-on-checking-for-eventmode)

(new SK8:Condition :objectname "AbortCondition" :project SK8)

;; use exitmodalstate to implement abortevent so it ends up running in the correct process
(defun abortevent ()
  (let ((curmode (SK8:ActiveMode)))
    (when curmode
      (let ((p (callingProcess curmode)))
        (if p ;; this means we are in a modal state, so exit it, otherwise just exit the mode
          (exitmodalstate :cancel)
          (exitmode curmode)))))
  (sk8-signal sk8::AbortCondition)
  (abort) ;; if we make it here, throw an abort
  )

;; modify effect of command-. when in sk8 to properly get out of sk8 modal states and not to pop up lisp dialogs
(defun process-for-interactive-abort ()
  (let* ((w (front-window :include-windoids t))
         (p (if w (gs::sk8-window-process w))))
    (or (if (or (and (eq w (caar *modal-dialog-on-top*))
                     (setq p (cdar *modal-dialog-on-top*)))
                (and p 
                     (or (eq p *current-process*)
                         (and (not (ccl::process-exhausted-p p))
                              (neq 0 (symbol-value-in-process 'ccl::*break-level* p))))))
          p)
        (if (and SS::!*lisp-dev-mode* (not (sk8:activemode)))
          (ccl::process-to-abort "Abort Process")
          gs::*sk8-system-process*))))

(let ((*warn-if-redefine* nil)
      (*warn-if-redefine-kernel* nil))
  (defun ccl::interactive-abort ()
    (let ((p (process-for-interactive-abort)))
      (if p (process-interrupt p
                               #'(lambda ()
                                   (unless ccl::*inhibit-abort*
                                     (if ccl::*in-read-loop* 
                                       (abort-break)
                                       (abortevent))
                                     )))))))

#|
	Change History (most recent last):
	2	6/28/93	rod	EventTick used to call (tick).  Now it does nothing.
	9	9/28/93	kleiman	added resume/suspend protocol
	11	2/12/94	kleiman	name changes
	13	2/25/94	rod	Added a few public functions to make these
				things usable.
	14	3/21/94	rod	Removing currentEventWindow as it already exists as currentWindow
	15	3/24/94	Hernan	Adding a predicate to return the previous event
				mode in the stack.
	16	6/29/94	Hernan	1171622: deactivate should be called AFTER the
				mode stack is popped. Also made activate be
				called before the new mode is pushed into the
				stack. This is to avoid calls to eventTick interfering
				with any required set up work.
	17 	 9/ 1/94	chip    	publish-project-symbol --> publish-global/function-symbol (radar #1183935)
	18 	 2/ 1/95	rod     	Adding a copy-list to make currenteventmodes 
							return a usable value.
	19 	 4/ 7/95	sidney  	enable events tha we want enabled during modal state so, for example, user can click OK in a dialog
	2  	 6/18/95	sidney  	exit/enter modalstate changed to work with processes in MCL3.0 (first approximation)
	3  	 8/ 1/95	sidney  	a little more tweaking to exit/entermodalstate for processes
	4  	12/15/95	sidney  	no more sk8eventabort condition
	5  	12/22/95	sidney  	define something for SK8::AbortEVent now that there is no sk8eventabort condition
	2  	 4/10/96	Hernan  	define-sk8-function->defun.
	3  	 4/19/96	Hernan  	Putting define-sk8-function back in the code.
	4  	 5/ 7/96	sidney  	Changes for new object system
	5  	 9/30/96	sidney  	changes to put sk8 windows in separate threads
	6  	10/ 8/96	sidney  	make sure abortevent does the right thing in the right thread
	7  	11/14/96	sidney  	added *sk8-system-process*, make sure modal stuff doesn't block it
	8  	12/19/96	sidney  	added a new sk8 condition named Abort that is signaled by abortevent, so user can create handlers for it
	9  	 1/30/97	Brian Roddy	Changing name of "Abort" object to "AbortCondition
	10 	 2/13/97	Brian Roddy	
	11 	 2/27/97	Hernan  	
|# ;(do not edit past this line!!)
