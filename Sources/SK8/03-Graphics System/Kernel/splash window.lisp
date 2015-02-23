;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories

(in-package :graphics-system)

(defclass *splash-window* (window)
  ((theHandle)
   (drewItOnce))
  (:default-initargs
    :color-p t
    :grow-icon-p nil
    :window-do-first-click t))

;;; These functions return the boundsRect of the main monitor.

(defun main-monitor-bounds ()
  (let* ((data (get-main-monitor-data))
         (location (ccl::screen-position data))
         (size (ccl::screen-size data)))
    (sk8-multivals 
     (point-h location) (point-v location)
     (+ (point-h location) (point-h size)) (+ (point-v location) (point-v size)))))

(defun get-main-monitor-data ()
  (let ((data (#_getdevicelist)))
    (loop (when (%null-ptr-p data) (return nil))
          (when (and (#_testdeviceattribute data 13) ; ensure screen
                     (#_testdeviceattribute data 11) ; main monitor
                     )
            (return data))
          (setq data (require-trap #_getnextdevice data)))))

(defun draw-splash (wptr)
  (sk8-multival-bind (ll tt rr bb) (main-monitor-bounds)
    (let* ((theWindow (window-object wptr))
           (size (view-size theWindow))
           (width (- rr ll))
           (height (- bb tt))
           (centerH (ash width -1))
           (centerV (ash height -1)))
      (unless (slot-value theWindow 'drewItOnce)
        ;; Draw the first time!
        (with-port (get-wmgr-port)
          (rlet ((r :rect :topleft 0 :bottomright size))
            (#_foreColor #$blackColor)
            (#_paintRect r)
            ;; Now draw the pict at the center.
            (let ((theHandle (slot-value theWindow 'theHandle))
                  (h 355)
                  (v 355)
                  top left)
              (when (handlep theHandle)
                (setf top (- centerV (ash v -1)))
                (setf left (- centerH (ash h -1)))
                (rset r :rect.top top)
                (rset r :rect.left left)
                (rset r :rect.bottom (+ top v))
                (rset r :rect.right (+ left h))
                (#_drawPicture theHandle r)))
            ))
        ;; Now set the drewItOnce to true.
        (setf (slot-value theWindow 'drewItOnce) t)
        ))))

(defpascal splash-window-wdef (:word variation :ptr wptr :word message :long parameter :long)
  (declare (ignore variation parameter))
  (without-interrupts
   (let ((result 0))
     (case message
       (#.#$wDraw
        (draw-splash wptr))
       (#.#$wHit
        (setf result #$wInContent))
       (#.#$wCalcRgns
        (sk8-multival-bind (ll tt rr bb) (sk8::boundsRect sk8::stage :physical t)
                           (rlet ((r :rect :left ll :top tt :right rr :bottom bb))
                             (#_RectRgn (rref wptr :windowRecord.strucrgn) r)
                             (#_setEmptyRgn (rref wptr windowRecord.contRgn))))))
     result)))

(defvar *splash-window-wdef-handle* (ccl::make-wdef-handle splash-window-wdef))

(defun get-splash-pict ()
  (let ((theFile (sk8::file sk8::sk8)))
    (unless (and theFile (probe-file (sk8::physicalName theFile)))
      (setf theFile sk8::SK8ApplicationFile))
    (sk8::macGetResourceHandleFromIdForDrawing theFile "PICT" 0)))
                       
(defun install-splash ()
  (sk8dev::restore-file-environment)
  (sk8-multival-bind (ll tt rr bb) (sk8::boundsRect sk8::stage)
    (let ((theWindow (make-instance '*splash-window*
                       :view-size (make-point (- rr ll) (- bb tt))
                       :view-position (make-point ll tt)
                       :window-show nil
                       :window-type :single-edge-box
                       :color-p t)))
      ;; Make sure the window thinks it can be drawn.
      (setf (slot-value theWindow 'drewItOnce) nil)
      ;; Install wdef.
      (setf *splash-window-wdef-handle* (ccl::make-wdef-handle splash-window-wdef))
      (unless (%null-ptr-p *splash-window-wdef-handle*)
        (rset (wptr theWindow) :window.windowDefProc *splash-window-wdef-handle*))
      ;; get the pict and save it.
      (setf (slot-value theWindow 'theHandle) (get-splash-pict))
      (window-show theWindow)
      theWindow)))

(defun findsplash ()
  (map-windows
   #'(lambda (w) (return-from findSplash w))
   :include-invisibles t
   :class '*splash-window*))

(defun deinstall-splash ()
  (let ((theSplash (findSplash)))
    (cond ((not (typep theSplash '*splash-window*))
           (error "Did not find splash window."))
          (t 
           (setf (slot-value theSplash 'drewItOnce) nil)
           (window-close theSplash)))))

;;; LISP CODE cannot run during a GC. Thus, WDEF code cannot be invoked at window
;;; destroy time. Thus, this code from MCL clears the defproc when we are getting
;;; rid of the window. 

(defmethod window-close :before ((w *splash-window*))
  (window-hide w)
  (let ((wptr (wptr w)))
    (when (and wptr (not (%null-ptr-p wptr)))
      ;; clear the defproc. 
      (rset wptr
            :windowrecord.WindowDefProc
            (rref ccl::%temp-port% :windowrecord.WindowDefProc)))))

#|

;;; If you are sick of the splash screen use this one (and build again).

(defmacro with-splash-screen (&body body)
  `(progn 
     (when *stage-covered-when-app-saved*
       ;; If the stage was covered when we saved, cover it!
       (setf *bkgnd-window-wdef-handle* (ccl::make-wdef-handle bkgnd-window-wdef))
       (setf (sk8::covered sk8::stage) t))
     ,@body))

|#

;;; The splash window does not appear if the UI is hidden or the application
;;; is not in the foreground.

(defmacro with-splash-screen (&body body)
  (let* ((usingSplash (gensym)))
    `(if ccl::*foreground*
       (without-interrupts
        (let ((,usingSplash nil))
          (unwind-protect
            (progn 
              (when (and ccl::*foreground* (not *no-ui-windows*))
                (hidemenubar)
                (install-splash)
                (setf ,usingSplash t)
                (when *top-listener*
                  (set-window-layer *top-listener* 0))
                )
              ,@body)
            (when ,usingSplash
              (deinstall-splash)
              (showmenubar)
              ))))
       (progn ,@body))))

#|
	Change History (most recent last):
	1		5/4/94	Hernan	New file. Implements the SK8 Splash screen.
	2		5/4/94	Hernan	Need to restore the file environment before trying
							to get the pict back.
	3		5/4/94	sidney	Fixing stupid typo.
	4		6/14/94	Hernan	Now it works.
	5		6/14/94	Hernan	Restoring the splash wdef.
	6		6/15/94	Hernan	Making the picture show up in the center of the
							main monitor of the Stage. All this need to be
							done using traps since nothing is restored.
	7		7/18/94	Hernan	1173175: added a property to the window that rememebers
							that it was drawn once already. (Kudge!!!)
	8		8/5/94	Hernan	Chaning draw-splash to conform to the size of the
							new SK8 pict.
	9  	 9/28/94	Hernan  	1189308: if the user does not want the UI windows
							then the SK8 splash pict does not show up.
	10 	 9/30/94	Hernan  	Made the Stage not go black if the user does not
							want the UI windows.
	11 	10/ 3/94	Hernan  	Resource methods will all start with "mac".
	12 	11/16/94	sidney  	look for splash screen in SK8 Resources folder
	13 	 1/11/95	Hernan  	The splash window only comes up when SK8 is in the
							foreground.
	14 	 2/10/95	sidney  	look for splash screen in SK8 project file or application if file doesn't exist
	15 	 2/15/95	Hernan  	Making the splash window show up in the right
							place.
	16 	 3/ 3/95	Hernan  	with-splash-screen does a without-interrupts
							right away to stop the user from switching out of
							SK8 when the splash has already been shown.
	2  	 5/ 3/96	Hernan  	Putting the original with-splash-screen back in.
	4  	10/ 7/96	Hernan  	Lisp code cannot run during GC. Remove the wdefproc from
						the window before closing it.
	5  	 2/27/97	Hernan  	
|# ;(do not edit past this line!!)
