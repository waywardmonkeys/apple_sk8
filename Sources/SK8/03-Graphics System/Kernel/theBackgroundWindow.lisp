;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories

(in-package :graphics-system)


(defclass *bkg-window* (window)
  ()
  (:default-initargs 
    :color-p t
    :grow-icon-p nil
    :window-do-first-click t))

;;; This variable exists to avoid calling inheritsFrom from within the wdef (since
;;; it slows it down somewhat).

(defvar *bkgnd-color-type* :rgb)

(defun paint-background (wptr)
  (with-port (get-wmgr-port)
    (let ((theColor (sk8::fillcolor sk8::stage)))
      (cond ((eq *bkgnd-color-type* :rgb)
             (#_penNormal)
             (with-rgb (rgb (sk8::mcl-color thecolor))
               (#_RGBForeColor rgb))
             (#_paintRgn (rref wptr :windowRecord.strucRgn))
             (with-rgb (rgb (sk8::mcl-color sk8::black))
               (#_RGBForeColor rgb)))
            (t ;; Either :bwPat or :colorPat.
             (let ((theMedia (sk8::media theColor))
                   theHandle)
               (when theMedia
                 (setf theHandle (sk8::loadMedia theMedia)))
               (if theHandle
                 ;; Draw it with the right trap!
                 (if (eq *bkgnd-color-type* :bwPat)
                   (#_fillRgn (rref wptr :windowRecord.strucRgn) theHandle)
                   (#_fillCRgn (rref wptr :windowRecord.strucRgn) theHandle))
                 ;; Just draw black.
                 (progn
                   (with-rgb (rgb (sk8::mcl-color sk8::black))
                     (#_RGBForeColor rgb))
                   (#_paintRgn (rref wptr :windowRecord.strucRgn)))))
             )))))

(defpascal bkgnd-window-wdef (:word variation :ptr wptr :word message :long parameter :long)
  (declare (ignore variation parameter))
  (without-interrupts
   (let ((result 0))
     (case message
       (#.#$wDraw
        (paint-background wptr))
       (#.#$wHit
        (setf result #$wInContent))
       (#.#$wCalcRgns
        (sk8-multival-bind (ll tt rr bb) (sk8::boundsRect sk8::stage :physical t)
          (rlet ((r :rect :left ll :top tt :right rr :bottom bb))
            (#_RectRgn (rref wptr :windowRecord.strucrgn) r)
            (#_setEmptyRgn (rref wptr windowRecord.contRgn))))))
     result)))

(defvar *bkgnd-window-wdef-handle* (ccl::make-wdef-handle bkgnd-window-wdef))

(defvar *bkgnd-layer-change-allowed* nil)
(defvar *bkgnd-selection-allowed* nil)

(defmacro changing-background-layer (&body body)
  `(prog2 (setf *bkgnd-layer-change-allowed* t
                *bkgnd-selection-allowed* t)
          ,@body
          (setf *bkgnd-layer-change-allowed* nil
                *bkgnd-selection-allowed* nil)))

;;; Fix definition of install-sk8-wdef to use *shaped-window-wdef-handle* for ":blank" windows.

(defun install-background ()
  (sk8-multival-bind (ll tt rr bb) (SK8::boundsRect SK8::Stage :physical t)
    (let ((theWindow (make-instance '*bkg-window*
                       :view-size (make-point (- rr ll) (- bb tt))
                       :view-position (make-point ll tt)
                       :window-show nil
                       :color-p t)))
      ;; Install wdef.
      (unless (%null-ptr-p *bkgnd-window-wdef-handle*)
        (rset (wptr theWindow) :window.windowDefProc *bkgnd-window-wdef-handle*))
      ;; Put it on the stage.
      (changing-background-layer
        (set-window-layer theWindow 9999)
        (when ccl::*foreground* 
          (window-show theWindow)))
      theWindow)))

(defun deinstall-background ()
  (let ((theBackground (findBackground)))
    (cond ((not (typep theBackground '*bkg-window*))
           (error "The last window was not the background window!!!"))
          (t (window-close theBackground)))))

(defun redraw-background (&optional region)
  (let ((theBackground (findBackground)))
    (when theBackground
      (with-port (wptr theBackground)
        (#_paintOne 
         (wptr theBackground) 
         (or region (rref (wptr theBackground) :windowRecord.strucRgn)))))))

;;; This is very ugly but fixes the bug by forcing the stuff under our background
;;; window to redraw itself. You can omit this method and then you will get this bug
;;; very sporadically. The bug is that when the background window is hidden one or
;;; more finder windows get covered with the pattern that was rendering the Stage. The
;;; portions of those finder windows that coincide with other SK8 windows that were not
;;; hidden (eg. the messageBox) are fine! Very weird. The problem only happens in the
;;; context of processing a suspend request from do-event. Even then, it only happens
;;; rarely. I could reproduce it by switching to the finder (using the Application Menu)
;;; right after a project was loaded.

(defmethod window-hide ((w *bkg-window*))
  (let+ ((tempRgn (:region))
         (wptr (wptr w)))
    (#_copyRgn (rref wptr :windowRecord.strucRgn) tempRgn)
    (call-next-method)
    (#_paintBehind wptr tempRgn)
    (#_calcVisBehind wptr tempRgn)
    ))

;;; Redefining methods to not do anything...

(defmethod window-select ((w *bkg-window*))
  (when *bkgnd-selection-allowed* 
    (call-next-method)))

(defmethod set-window-layer ((w *bkg-window*) new-layer &optional include-invisibles)
  (declare (ignore-if-unused new-layer include-invisibles))
  (when *bkgnd-layer-change-allowed*
    (call-next-method))
  )

;;; LISP CODE cannot run during a GC. Thus, WDEF code cannot be invoked at window
;;; destroy time. Thus, this code from MCL clears the defproc when we are getting
;;; rid of the window. 

(defmethod window-close :before ((w *bkg-window*))
  (window-hide w)
  (let ((wptr (wptr w)))
    (when (and wptr (not (%null-ptr-p wptr)))
      ;; clear the defproc. 
      (rset wptr
            :windowrecord.WindowDefProc
            (rref ccl::%temp-port% :windowrecord.WindowDefProc)))))

;;; Why does this cons 40 bytes?

(defun findbackground ()
  (map-windows
   #'(lambda (w) (return-from findBackground w))
   :include-invisibles t
   :class '*bkg-window*))


;;; this can't be correct. Maybe we can make *bkg-window* inherit from *sk8-window* and then
;;; the modes will be automatically handled correctly?
#|
(defmethod view-click-event-handler ((w *bkg-window*) where)
  (let ((*event-window* w)
        (*event-location* where)
        (*eventH* (point-h where))
        (*eventV* (point-v where)))
    (if (currentmode)
      (sk8::handlemousedown (currentmode))
      (sk8::mousedown sk8::stage))
    ))
|#

#|
	Change History (most recent last):
	1	9/20/93	hernan	New file. Implements the background window that
				covers the finder with the desktop pattern.
	2	12/21/93	sidney	Changes so files can be compiled
	3	1/31/94	hernan	Making the background window's WDEF deal with
				the fillcolor of the Stage.
	4	1/31/94	hernan	Making a function that repaints the whole stage.
	5	2/12/94	kleiman	name changes
	6	3/21/94	Hernan	Adding a window-close method that makes sure
				the window's pixmap is not the desktop's pixmap.
	7	3/31/94	Hernan	Rewrote findBackground to only use up 40 bytes
				(instead of 72).
	8	3/31/94	Hernan	Deinstall-background now searches for the
				background instead of looking at the last window.
	9	4/8/94	Hernan	redraw-background takes an optional dirty region.
	10	4/22/94	Hernan	Fixed than annoying flicker on the Stage that is
				somehow caused by the halo doing stuff. The fix
				is to penNormal before drawing the stage.
	11	6/14/94	Hernan	Covering the Stage during builds.
	12	6/14/94	dy	Cannot cover the Stage so early in the build.
	13	6/14/94	Hernan	Setting *bkgnd-color-type* to :rgb.
	14 	10/ 3/94	Hernan  	machandle -> loadMedia.
	15 	12/13/94	Hernan  	Letting the fillcolor of the Stage be a pattern again.
	16 	 1/11/95	Hernan  	The background window is only shown if SK8 is in
							the foreground.
	17 	 1/27/95	Hernan  	1188155: creating a window-hide method to make
							sure the stuff behind our background window is
							redrawn when the window goes away. This is a 
							common problem right after a project has been 
							closed.
	18 	 1/29/95	sidney  	force recompile because let+ macro has changed for (:region)
	19 	 2/ 8/95	Hernan  	1215546: install-background no longer sets the
							strucRgn when the window is created.
	20 	 4/ 3/95	Hernan  	install-background makes sure the background window
							shows up behind every other window in the system. This
							is accomplished by changing the window-layer before we
							show the window.
	21 	 6/12/96	sidney  	backround-wdef must return an integer in MCL 3.9
	3  	 9/30/96	sidney  	bind event globals to support mouse down handler when clicking on stage
	4  	10/ 7/96	Hernan  	Lisp code cannot run during GC. Remove the wdefproc from
						the window before closing it.
	5  	 2/27/97	Hernan  	
|# ;(do not edit past this line!!)
