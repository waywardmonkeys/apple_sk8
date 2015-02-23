(in-package :graphics-system)

;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories



#| Modification History

01-22-93 ruben id -> objectName
12-08-92 hernan changed *event-time* and *event-object* to
                eventTime and eventActor (both now in SK8).
08-28-92 hernan removed vars already defined in mf/kernel/globals.
07-29-92 ice   added white abd black
07-26-92 ruben moved exports to exports.lisp; added *live* definition
06-09-92 ruben added *null-ptr*
05-27-92 ruben *uncopiable-properties*
05-12-92 ruben d29 conversion
03-24-92 ruben added blank window wdef global; added hidemenubar globals
08-21-91 ruben obsoleted *color-table*
05-24-91 ruben added mf::*visible-ui*
02-25-91 ICE converted to MCL 2.0.

|#

;;; _______________________________ 
;;; process stuff. 
;;; _______________________________ 

;;; The drawing lock. This lock is used by anything in the system that
;;; needs to draw. Drawing is considered a shared resource. 

(defparameter *drawing-lock* (make-lock))

(defmacro with-drawing-lock (&body body)
  `(with-lock-grabbed (*drawing-lock*)
     (progn ,@body)))

;;; executes the body in a new process only when it can get the drawing lock.

(defmacro with-queued-drawing (&body body)
  `(process-run-function '(:background-p t) 
                         #'(lambda () 
                             (with-drawing-lock ,@body))))

;;; _______________________________ 
;;; _______________________________ 

(defvar *unsettable-secret-property-message* 
  "Unsettable secret innards")

;;; Holds a list of the depths of all the monitors in the system.
;;; This variable is set every time we suspend SK8.

(defvar *monitor-depths* nil)

;;; This variable specifies whether the "draw only the dirty actors" optimization
;;; is performed when the window draws itself. It is only used during the draw loop.
;;; It is defined by sk8-draw-window and declared special. That way, this var is on the stack
;;; and not globally defined, allowing for reentrant graphics. 

;;; For global state we added a slot to *sk8-window* called "drawOnlyDirty".

(defvar *fast-draw*) ;; not bound. Incorrect to use outside the context of draw loop.


(defvar *force-click* nil)  ;; forces a mouseUp to be a click.

;;______________________________________________________________________
;; GLOBALS
;; We need these globals to accurately track window status. 

(defvar *event-window*)

;;; This is the current window or windoid: the last thing that was clicked on.

(defparameter *currentTla* cl-user::*top-listener*)     ;; The window (or windoid) that was last selected.

;;; Stores the EditText as soon as it is created.

(defvar *EditText-the-object* nil)

;;; *LIVE*
;;; Drags and resizes are done with full pixmap blitting rather than with outlines
;;;

(defvar sk8::*live* nil) ;; OJO! Set to nil for now since window resizing is too slow!

;; *EVENT-LOCATION*
;; The location of the mouse in global coordinates when the last event happened

(defvar *event-location* 0)

;; *EVENT-X* and *EVENT-Y*
;; The fixed-point analogues to *event-location*

(defvar *event-x* #%0)
(defvar *event-y* #%0)

;;; Event variables.

(defvar *eventTime* 0)
(defvar *eventActor* nil)
(defvar *eventH* 0)
(defvar *eventV* 0)

(defun sk8::eventTime () *eventTime*)

(defun sk8::eventActor () *eventActor*)

(defun sk8::eventH () *eventH*)

(defun sk8::eventV () *eventV*)

(defvar *selecting-clos-window* nil)

;; *LAST-EVENT-LOCATION*
;; The position of the mouse the last time through the event loop

;; not used anywhere (defvar *last-event-location* 0)

;; *OBJECT-UNDER-MOUSE*
;; A list whose car is the object under the mouse (target for all those mouseWithin messages!) during the
;; last event dispatch and whose cadr is whether that object's card was in author mode.  Note that this can be an object, a 
;; can be an object, a connector, or a card!

(defvar *object-under-mouse* (cons nil nil))

;; *LAST-CLICK-TIME*
;; The time in ticks of the last click event

(defvar *last-click-time* 0)
(defvar *last-click-location* 0)
(defvar *last-click-object* nil)

(defvar *last-mouseDown-time* 0)
(defvar *last-mouseDown-location* 0)
(defvar *last-mouseDown-object* nil)

;;; Should all windows on the screen be locked?

(defvar *lock-screen* nil)

(defvar *graphics-debug* nil)

(defvar *stage* nil)

;; SK8 Internal Variables  (don't use these in your code!)

;; *SK8-WINDOW-WDEF-HANDLE*
;; A handle to the :SK8-WINDOW WDEF resource

(defparameter *SK8-window-wdef-handle*
  (with-open-res-file (nil (sk8:physicalname (or (sk8::file sk8::sk8) sk8::SK8ApplicationFile)))
    (let ((theHandle (T_Get1NamedResource "WDEF" "sedWDEF")))
      (when (handlep theHandle)
        (T_DetachResource theHandle)
        theHandle))))

#+ppc-target
(defparameter *blank-window-wdef-handle*
  (with-open-res-file (nil (sk8:physicalname (or (sk8::file sk8::sk8) sk8::SK8ApplicationFile)))
    (let ((theHandle (T_Get1NamedResource "WDEF" "BlankWDEF")))
      (when (handlep theHandle)
        (T_detachResource theHandle)
        theHandle))))

;;; Movies

;;; Has QuickTime's #_EnterMovies been called?

(defvar *movies-initialized* nil)

;;; lists of things to be serviced from do-event

(unless (boundp '*quicktime-callbacks-to-activate*)
  (defvar *quicktime-callbacks-to-activate* nil)) ;? should be a population
(unless (boundp '*active-movie-controllers*)
  (defvar *active-movie-controllers* nil))
(unless (boundp '*active-movies-without-controllers*)
  (defvar *active-movies-without-controllers* nil))
(unless (boundp '*movies-with-bogus-file*)
  (defvar *movies-with-bogus-file* nil))

;;; A general-purpose null pointer

(defvar *null-ptr* (%null-ptr))

(define-sk8-var sk8::eventsStopForMenus 
  :initial-Value nil)

#|
	Change History (most recent last):
	2	11/1/93	hernan	New files are messing up things...
	3	11/5/93	kleiman	reset-event-system-globals will ensure that there
				are no disposed objects in event system globals
	4	11/12/93	kleiman	moved reset-event-system-globals  function to
				misc utilities file in 2-d
	5	11/19/93	hernan	Added *oldWindow*, a variable to hold the window
				that had been active before the current one. This
				is useful for knowing what to activate when a
				window goes away.
	6	1/31/94	hernan	Adding a variable to record the depth of all 
				monitors when SK8 is suspended. This will let us
				know whether the depth of monitors has changed
				and we have to update actors on the stage.
	7	2/4/94	dy	Corrected the comment for *movies-are-playing-p*
	8	2/18/94	dy	changed name of *movies-are-playing-p* to *active-movies-without-controllers* and added *active-movie-controllers* global
	9	2/18/94	dy	*movies-initialize* belongs in sk8dev package
	10	2/28/94	hernan	Now using GCable mac things.
	11	3/1/94	kleiman	obsolete *last-object-id*
	12	3/1/94	sidney	handle some pointers correctly
	13	3/1/94	kleiman	package problems
	14	3/14/94	sidney	make hash table for discarded objects weak so parents can gc if possible
	15	5/2/94	dy	clean up warnings on *active-movie-controllers* etc.
	16	5/2/94	dy	clean up warnings on *active-movie-controllers* etc.
	17	7/1/94	Hernan	1168757: added a variable to remember the state
				of the *events-on* variable when suspend happens.
	18	7/6/94	Hernan	1169269: replacing the eventX variables with
				functions.
	19	7/18/94	Hernan	Moving definition of *events-on* and 
				*events-were-on* to 02-system.
	20 	 8/31/94	Hernan  	Added *currentTla*.
	21 	 9/ 2/94	Hernan  	1183700: adding a variable to remember the 
							EditText object.
	22 	 9/27/94	Hernan  	Adding the *oldTla*, a variable to remember the
							window or windoid that was the currenttla before
							the current window was activated.
	23 	12/ 9/94	Hernan  	Adding *unsettable-secret-property-message*.
	24 	 1/ 4/95	sidney  	remove remaining traces of obsoleted lib functionality
	25 	 2/10/95	dy      	tweaks to invocations of macros that reduce to make-terminable-macptr
	26 	 2/10/95	sidney  	initialize wdef like it's done in restore-sk8 in case that's important
	27 	 3/11/95	dy      	introduce *quicktime-callbacks-to-reschedule*
	28 	 3/11/95	dy      	quickTime-callbacks-to-reschedule -> xxx-activate
	29 	 3/11/95	dy      	introduce *movies-with-bogus-files*
	30 	 4/24/95	Hernan  	Removing unused window variables.
	2  	 6/23/95	Hernan  	1252857: adding an eventWindow function.
	3  	 7/12/95	Hernan  	Adding the drawing lock and two macros to make use of it.
						The macros are exported to sk8dev.
	4  	 8/ 9/95	Brian   	exporting event window.
	2  	 4/16/96	Hernan  	Getting rid of *temp-region*, *sk8-penState* and *original-dirty-region*.
	3  	 4/18/96	Hernan  	Getting rid of *clip* and *fast-draw*.
	4  	 7/23/96	Hernan  	Adding *blank-window-wdef-handle*.
	5  	 9/30/96	sidney  	changes to put sk8 windows in separate threads
	6  	10/ 7/96	sidney  	disposing-clos-window no longer used
	7  	 2/27/97	Hernan  	
|# ;(do not edit past this line!!)
