(in-package :sk8dev)

;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories



;;; not tested
(defun_X T_LockPixels nil (pixmap &key falseIfError)
  "Returns True on Success.
If failure and falseIfError is non-nil, then returns nil.
See IM-Imaging p. 6-32"
  (if (#_lockPixels pixmap)
    t
    (if falseIfError
      nil
      (error "Can't lock pixels"))))


(defun_X T_GetGWorld nil ()
  "Returns 2 values: CGrafPortPtr and GDeviceHandle.
See IM-Imaging p. 6-28"
  (rlet ((thePort (:pointer :CGrafPort))
         (dev    (:handle  :GDevice  )))
    (#_GetGWorld thePort dev)
    (values_X
      (%get-ptr thePort)
      (%get-ptr dev))))

(defun_X T_NewGWorldGC nil (boundsrect &key
                                            (depth 0)
                                            (ctable   TC_nil)
                                            (agdevice TC_nil)
                                            (flags 0)
                                            noGC)
  "Returns GWorld GCHandle.
See IM-Imaging p. 6-28"
  ;; Fix the depth so that the gWorld is made with the depth of the best
  ;; monitor in the system (since 0 will make it with the depth of the main monitor).
  (when (zerop depth)
    (setf depth (sk8::bestcolordepth sk8::system)))
  (rlet ((offscreenGWorld :CGrafport))
    (checking-Toolbox-Error (:OSErr)
                            (#_NewGWorld offscreengworld depth boundsrect ctable agdevice flags))
    (if noGC
      (%get-ptr offscreenGWorld)
      (makeGCOSPtr (%get-ptr offscreenGWorld) #_DisposeGWorld))))


;;; Untested
(defmacro with-locked-pixels-safe ((gworld-pixmap &key nil-if-cant-lock) &body body)
  (let ((wasLocked (gensym)))
    `(let ((,wasLocked (%boolean-to-lisp (logand
                                          (require-trap #_GetPixelsState ,gworld-pixmap) ; Trap can't signal an error
                                          (require-trap-constant #$pixelsLocked)))))
       (unless ,wasLocked
         (T_LockPixels ,gworld-pixmap :falseIfError ,nil-if-cant-lock))
       (unwind-protect
         (progn ,@body)
         (unless ,wasLocked
           (#_unlockPixels ,gworld-pixmap)); no error checking because by now we can be sure this won't misbehave
         ))))

(defmacro with-locked-pixels-force ((gworld-pixmap &key nil-if-cant-lock) &body body)
  `(progn
     (T_LockPixels ,gworld-pixmap :falseIfError ,nil-if-cant-lock)
     (unwind-protect
       (progn ,@body)
       (require-trap #_unlockPixels ,gworld-pixmap); no error checking because by now we can be sure this won't misbehave
       )))

#|
;;; 152 instructions
(defun xxxx (pm)
  (with-locked-pixels-safe (pm)
    (foo)
    (bar)))

;;; 84 instructions
(defun xxxx (pm)
  (with-locked-pixels-force (pm)
    (foo)
    (bar)))
|#

#|
	Change History (most recent last):
	1	2/25/94	dy	
	2	2/28/94	dy	add T_NewGCWorldGC
	3	3/1/94	hernan	makeGCOSPtr -> T_makeGCOSPTr.
	4	3/3/94	kleiman	best-depth-available -> bestColorDepth
	5	3/31/94	rod	Adding a function that make a non gcable gWorld.
	6	6/24/94	sidney	check for error making new gworld before we crash and burn
	7	6/25/94	sidney	even better, remove newGWorld and always use GCable macptr version
	8	6/25/94	sidney	revert to previous version until we figure out why it doesn't work
	9  	 9/ 7/94	dy      	add :noGC arg to T_NewGWorldGC
	10 	11/28/94	dy      	gc macro mods; new T_LockPixels, with-locked-pixels-xxx macros
	11 	12/19/94	dy      	Use new %boolean-to-lisp macro
	12 	 2/10/95	dy      	tweaks to invocations of macros that reduce to make-terminable-macptr
	13 	 3/17/95	till    	uncomment with-locked-pixels
	2  	 2/27/97	Hernan  	Removing case preserving reader macro.
|# ;(do not edit past this line!!)
