;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories


;;;


;02-04-93 adam    added *rehide-sk8-menubar?* flag;  fixed logic of calls to mf::hideMenuBar and
;              mf::showMenuBar;  properly maintain state of mf::*mBarVisible*

;;; Dispatches Command-Key character to appropiate SK8 object
;;;
;;;   (1) If the currentCommandKey of the SK8 project is an object, then
;;;       send KEY event to that object with the command key
;;;   (2) If there's no currentCommandKey, then send the KEY event to
;;;       the currentKey of the topmost toplevel actor.
;;;   (3) If none of the above holds, then don't send the KEY event at all.
;;;

;;; Normal MCL command key dispatching may be resumed by setting the
;;; global *sk8-cmd-key-enabled* to nil.


;;; Dispatches resume and suspend events to the SYSTEM object
;;; whenever *sk8-suspend-resume-enabled* is non-nil. When system
;;; resumes, it checks whether menubar is supposed to be hidden and,
;;; if so, makes sure it is.
;;;



(in-package :ccl)

;; define some constants that are not available in the default MCL environment
(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (boundp '$MButDwnEvt)
    (defconstant $MButDwnEvt 1))
  (unless (boundp '$inMenubar)
    (defconstant $inMenubar 1))
  (unless (boundp '$inSysWindow)
    (defconstant $inSysWindow 2))
  (unless (boundp '$diskInsertEvt)
    (defconstant $diskInsertEvt 7))
  (unless (boundp '$ActivateEvt)
    (defconstant $ActivateEvt 8))
  (unless (boundp '$UpdatEvt)
    (defconstant $UpdatEvt 6))
  (unless (boundp '$KeyDwnEvt)
    (defconstant $KeyDwnEvt 3))
  (unless (boundp '$AutoKeyEvt)
    (defconstant $AutoKeyEvt 5))
  (unless (boundp '$nullEvt)
    (defconstant $nullEvt 0))
  (unless (boundp '$app4Evt)
    (defconstant $app4Evt 15))
  (unless (boundp '$kHighLevelEvent)
    (defconstant $kHighLevelEvent 23)))

(defvar *sk8-cmd-key-enabled* t)
(defvar *sk8-suspend-resume-enabled* t)

(defvar *rehide-sk8-menubar?* nil)

(defvar mf::*mBarVisible* t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Low level idle stuff.

;;; This should subsume the quickTime *active-movie-controllers* thing.

(defvar sk8dev::*objects-that-want-lowlevelidle* nil)

(defun sk8dev::dispatch-lowLevelIdle ()
    )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; QuickTime stuff

(defvar sk8dev::*active-movie-controllers* nil)

(defun sk8dev::is-movie-controller-event (event)
  (declare (ignore event))
  nil
  )

(defvar sk8dev::*active-movies-without-controllers* nil)

(defun sk8dev::movies-task ()
  )

(defvar sk8dev::*quicktime-callbacks-to-activate* nil)

(defun sk8dev::activate-quicktime-callbacks ()
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Clock stuff

(defvar mf::*clocks-running* nil)

(unless (fboundp 'sk8::tickEventClock)
   ;; This will be redefined later in the 08-clock.lisp sources
  (defun sk8::tickEventClock ()
    ))

;;; MCL3.0
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; redefine do-event in terms of existing MCL do-event function
;; This attempts to minimize the assumptions we have to make about the internals of do-event
;; to ease the switch to MCL3.0. The older version of this copied and modified all of the
;; MCL do-event code

(unless (fboundp 'sk8::OriginalMCLdo-event)
  (setf (symbol-function 'sk8::OriginalMCLdo-event) #'do-event))

(defun sk8dev::new-do-event ()
  (declare (resident)
           (special sk8::system *current-event*
                    sk8dev::*quickTime-callBacks-to-activate*
                    sk8dev::*active-movie-controllers*
                    *sk8-cmd-key-enabled*
                    *rehide-sk8-menubar?*
                    mf::*mBarVisible*
                    *sk8-suspend-resume-enabled*
                    mf::*clocks-running*
                    sk8dev::*objects-that-want-lowlevelidle*
                    sk8dev::*active-movies-without-controllers*))
  (let ((event *current-event*)
        ecode)
    (unless (sk8dev::is-movie-controller-event event)
      ;; QT stuff. This is a fix to a QT bug and should not be generalized to allow
      ;; other (non QT things) to do antything at this point. - Hernan.
      (when sk8dev::*quickTime-callBacks-to-activate*
        (sk8dev::activate-quicktime-callbacks))
      ;; Note that the movie no longer gets first crack at all events. - Hernan.
      (setq ecode (rref event eventrecord.what)) ;; or (%get-word event) ??
      ;; check for the special cases for SK8
      (cond ((and (or (eq ecode $KeyDwnEvt) (eq ecode $AutoKeyEvt))
                  (menukey-modifiers-p (rref event eventrecord.modifiers)))
             (let* ((system (and *sk8-cmd-key-enabled* sk8::system))
                    (key (and system (sk8::currentCommandKey system))))
               (if key
                 ;; Send command key to appropriate SK8 object:
                 (let ((char (logand #xff (event-keystroke
                                           (rref event eventrecord.message)
                                           (rref event eventrecord.modifiers)))))
                   (update-menus)
                   (sk8::commandKeyEvent key (code-char char)))
                 ;; do normal MCL key processing
                 (sk8::OriginalMCLdo-event))))
            ((and (eq ecode $app4Evt)
                  (eq 1 (%get-byte event $evtMessage))) ;suspend or resume event
             (if (%ilogbitp 0 (%get-byte event $evtMessage-b))
               (progn
                 (when *rehide-sk8-menubar?*
                   (without-interrupts
                    (setq mf::*mBarVisible* t)
                    (gs:hideMenuBar)
                    (setq *rehide-sk8-menubar?* nil)))
                 (when *sk8-suspend-resume-enabled* (SK8::RESUME SK8::SYSTEM))
                 (sk8::OriginalMCLdo-event))
               (progn
                 (when *sk8-suspend-resume-enabled*
                   (SK8::SUSPEND SK8::SYSTEM))
                 (sk8::OriginalMCLdo-event)
                 (when (not mf::*mBarVisible*)
                   (without-interrupts
                    (gs:showMenuBar)
                    (setq mf::*mBarVisible* nil)
                    (setq *rehide-sk8-menubar?* t))))))
            (t (sk8::OriginalMCLdo-event))))
    ;; Ticking the clock.
    (when mf::*clocks-running*
      (sk8::tickEventClock))
    ;; Deal with low level idles.
    (when sk8dev::*objects-that-want-lowLevelIdle*
      (sk8dev::dispatch-lowLevelIdle))
    ;; Deal with movies. We should get rid of this as soon as QT moves to lowLevelIdle. - Hernan.
    (when sk8dev::*active-movies-without-controllers*
      (sk8dev::movies-task))))

(without-interrupts
 (let ((*warn-if-redefine-kernel* nil)
       (*warn-if-redefine* nil))
   (defun do-event ()
     (sk8dev::new-do-event))))

#|
	Change History (most recent last):
	2	8/31/93	kleiman	defined mf::*mbarvisible*
	3	2/10/94	kleiman	took out movies loop from window-event
				and put it here for smoother movie playing
	4	2/11/94	sidney	defined mf::*movies-are-playing-p*
	5	2/11/94	sidney	whoops!
	6	2/18/94	dy	changed name of movies-are-playing-p to active-movies-without-controllers and added event handling for movie controllers
	7	2/18/94	dy	forgot some 's
	1	2/21/94	sidney	move this file to a new subproject
	2	3/31/94	Hernan	Adding clock ticking to do-event.
	3	3/31/94	Hernan	Ooops! TickEventClock should be in sk8 not sk8dev.
	4  	12/14/94	dy      	code reading led to some cleanup: rref instead of %get-word
	5  	 3/11/95	dy      	introduce *quicktime-callbacks-to-reschedule* in do-event.  (Sorry, it has to be done.)
	6  	 3/11/95	dy      	quickTime-callbacks-to-reschedule -> xxx-activate
	3  	 8/ 2/95	Hernan  	Added machinery for dispatching the lowLevelIdle event.
	2  	 5/ 7/96	Hernan  	hidemenubar and showmenubar are in gs.
	4  	 9/20/96	Hernan  	Giving quicktime controllers first crack at all events.
	5  	 9/30/96	sidney  	set up sk8 window event process in separate thread
	6  	10/ 7/96	Brian   	Removed bogus fbound checks that Dave had so
						he could select all and reeval the file.
	7  	 2/27/97	Hernan  	
|# ;(do not edit past this line!!)