(in-package :ccl)

;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories



;;; The point of this patch is to make Lisp windows behave properly when SK8 Windoids are being used.
;;; The problem used to be that Lisp windows refused to deactivate when a windoid activated. The
;;; problem was traced down to reselect-windows. THE FIX is to ONLY activate the window if it is
;;; the *currentTla* (our own variable which holds the last clos-window clicked on). Hernan.

(defparameter gs::*currentTla* nil)

;;; MCL3.0
;; This is pretty gross. Is there a better way of doing this? Here we repeat the entire
;; definition of the internal function reselect-windows, just so we can add a condtion at
;; the very end so that only a window blessed by SK8 as the *currentTLA* will be selected
;; This is supposed to fix a problem in which CLOS windows would not deactivate when a
;; SK8 windoid was selected.

(eval-when (:compile-toplevel :execute)
  (let ((*warn-if-redefine-kernel* nil)
        (*warn-if-define* nil))
    (defmacro do-wptrs (wptr &body body)
      (let ((next-wptr (gensym)))
        `(with-macptrs ((,wptr (%get-ptr (%int-to-ptr (require-trap-constant #$WindowList))))
                        ,next-wptr)
           (do () ((%null-ptr-p ,wptr))
             (%setf-macptr ,next-wptr (rref ,wptr windowrecord.nextwindow))
             ,@body
             (%setf-macptr ,wptr ,next-wptr))))))
  )

(let ((*warn-if-redefine-kernel* nil)
      (*warn-if-define* nil))
  (defun reselect-windows ()
    (let ((selected *selected-window*)
          last-windoid
          found-non-windoid?
          (da-before-selected? :maybe)
          (windoid-count 0))
      (if (typep selected 'da-window)
        (setq selected nil))
      (do-wptrs wptr
        (when (rref wptr windowrecord.visible)
          (let ((wob (window-object wptr)))
            (cond ((or (null wob) (not (typep (wptr wob) 'macptr)))
                   (hilite-wptr wptr nil))
                  ((windoid-p wob)
                   (if found-non-windoid?
                     (if last-windoid
                       (window-send-behind wptr (wptr last-windoid) t)
                       (window-bring-to-front wob wptr)))
                   (setq last-windoid wob)
                   (unless (window-active-p wob)
                     (view-activate-event-handler wob))
                   (setq windoid-count (%i+ windoid-count 1)))
                  (t
                   (setq found-non-windoid? t)
                   (if (typep wob 'da-window) 
                     (if (eq :maybe da-before-selected?)
                       (setq da-before-selected? t))
                     (if (and (eq wob (or selected (setq selected wob)))
                              (eq :maybe da-before-selected?))
                       (setq da-before-selected? nil))))))))
      (setq *windoid-count* windoid-count
            *last-windoid* last-windoid)
      (when *selected-window*           ; maybe nobody is selected
        (setq *selected-window* selected)
        (when (eq t da-before-selected?)
          (if last-windoid
            (window-send-behind (wptr selected) (wptr last-windoid) t)
            (window-bring-to-front selected)))
        (unless (window-active-p selected)
          (when (and gs::*currentTla* (eq selected gs::*currentTla*))
            (view-activate-event-handler selected)))))))

;; suppress modification of window type when grow-icon-p is specified as nil
;; MCL's version modifies it. Hernan doesn't like that in SK8
;; This recoding for MCL3.0 uses an :around method to avoid including the entire
;; internal function here

(let ((*warn-if-redefine-kernel* nil)
      (*warn-if-define* nil))
  (defmethod window-make-parts :around ((window window)
                                           &key (view-position (view-default-position window))
                                           (view-size (view-default-size window))
                                           (window-type :document-with-zoom)
                                           procid
                                           (window-title "Untitled")
                                           (close-box-p t)
                                           (color-p nil)
                                           (grow-icon-p nil gip?))
    (if (and (not procid)
             gip?
             (not grow-icon-p)
             (not (wptr window))
             (eq window-type :document-with-grow))
      (call-next-method window
                        :view-position view-position
                        :view-size view-size
                        :window-type window-type
                        :procid procid
                        :window-title window-title
                        :close-box-p close-box-p
                        :color-p color-p)
      (call-next-method))))

#|
	Change History (most recent last):
	1	2/21/94	sidney	move this file to a new subproject
	2  	 8/31/94	Hernan  	*currentWindow* -> *currentTla*.
	3  	 2/13/95	Hernan  	Patched window-make-parts to stop changing the
							window-style according to the value of 
							window-grow-icon-p. Now when grow-icon-p we
							leave the window-style alone.
	3  	 9/19/95	Hernan  	Ooops. Window-make-parts was calling next method
						without supplying the window argument. Obviously this
						did not happen very much...
	2  	 5/ 2/96	Hernan  	Please, just one *currentTla* (and it is in gs).
	4  	 2/27/97	Hernan  	
|# ;(do not edit past this line!!)
