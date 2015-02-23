(in-package :graphics-system)

;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories



#| Modification History

01-26-93 ruben broke down weird usingresourcefile macro into two macros:
               usingresourcefilename and usingresourcefilerefnum
01-22-93 ruben id -> objectName, require-trap in usingresourcefile
01-14-93 ruben usingresourcefile
10-22-92 hernan update-gWorld works!!! (I had to record this historic moment...)
06-13-92 ruben obsoleted drag-anti-aliased-string
08-21-91 ruben changed lock-gworld, unlock-gworld and dispose-gWorld to macros
               no longer requires interface files; cleaned up some code
05-23-91 ruben changed all routines to use stack instead of heap whenever possible
02-15-91 ICE Created it

|#

;;; _______________________________ 
;;; Records
;;; _______________________________ 

;;; *** For some reason, this record type won't autoload.

(defrecord Event
  (what integer)
  (message longint)
  (when longint)
  (where point)
  (modifiers integer))

;;; *** For some reason, this record type won't autoload.

(defrecord window 
  (port grafport)
  (windowKind integer)
  (visible boolean)
  (hilited boolean)
  (goAwayFlag boolean)
  (spareFlag boolean)
  (strucRgn (handle region))
  (contRgn (handle region))
  (updateRgn (handle region))
  (windowDefProc handle)
  (dataHandle handle)
  (titleHandle handle)
  (titleWidth integer)
  (controlList (handle control))
  (nextWindow (pointer window))
  (windowPic handle)
  (refCon longint))

(ccl::def-mactype :wstatedataptr (find-mactype :pointer))
(ccl::def-mactype :wstatedatahandle (find-mactype :handle))
(defrecord (WStateData :handle) 
  (userState :rect)            ; user state
  (stdState :rect)             ; standard state
  )

;;; _______________________________ 
;;; THEN, THE CONSTANTS, DEFVARS, AND DEFPARAMETERS
;;; _______________________________ 

;; *HELP-MESSAGE-RECORD*
;; This holds a single help manager record that SK8 recycles for all of its
;; help balloon functions.

(defparameter *help-message-record* 
  (newRecordGCPtr :hmmessageRecord :hmmHelptype 1))

;;; _______________________________ 
;;; NOW, THE MACRO DEFINITIONS
;;; _______________________________ 

;;; Allows acces to records inside handles. Make sure you pass the right thing in the
;;; storage argument (eg. if the thing you want to get is a pointer, say :storage :pointer"

(defmacro hrref (handleToRecord accessor &key storage)
  (let ((varName (gensym)))
    (if storage
    `(with-dereferenced-handles ((,varName ,handleToRecord))
         (rref ,varName ,accessor :storage ,storage))
    `(with-dereferenced-handles ((,varName ,handleToRecord))
       (rref ,varName ,accessor)))))

;;; WINDOW-RECTANGLE
;;; window
;;; Returns the rectangle of window
;;; window => a CLOS window object

(defmacro window-rectangle (window)
  `(rref (wptr ,window) :cgrafport.portrect))

;;; Returns the origin of the port

(defmacro port-origin (port)
  `(rref (rref ,port :cGrafport.portRect :storage :pointer) :rect.topLeft))

;;; WINDOW-PIMXAP
;;; window
;;; Returns the pixmap of window
;;; window => a CLOS window object

(defmacro window-pixmap (window)
  `(rref (wptr ,window) :cgrafport.portpixmap))

(defmacro dispose-gWorld (offscreenGWorld)
  `(require-trap #_DisposeGWorld ,offscreenGWorld))

(defmacro lock-GWorld (offscreenGWorld)
  `(require-trap #_LockPixels (require-trap #_GetGWorldPixMap ,offscreenGWorld)))

(defmacro unLock-GWorld (offscreenGWorld)
  `(require-trap #_UnlockPixels (require-trap #_GetGWorldPixMap ,offscreenGWorld)))

;;; GWORLD-RECTANGLE
;;; gWorld
;;; Returns the rectangle of gWorld
;;; gWorld => a pointer to a gWorld

(defmacro gworld-rectangle (gworld)
  `(hrref (gworld-pixmap ,gWorld) :pixmap.bounds :storage :pointer))

;;; WITH-OFFSCREEN-GWORLD 
;;; gWorld 
;;; Executes the body fo the macro with the current gWorld set to gWorld.
;;; gWorld => a pointer to an offscreen gWorld

(defmacro with-offScreen-gWorld ((gworld) &body body)
  (let ((oldCport (gensym "cgrafport-"))
        (oldGdevice (gensym "gDevice-")))
    `(multiple-value-bind (,oldCPort ,oldGDevice)
                          (get-gWorld)
       (lock-gworld ,gWorld)
       (set-gWorld ,gworld)
       (unwind-protect 
         ,@body
         (unlock-gworld ,gWorld)
         (set-gWorld ,oldCPort ,oldGDevice)))))

(defmacro with-gWorld ((gworld) &body body)
  (let ((oldCport (gensym "cgrafport-"))
        (oldGdevice (gensym "gDevice-")))
    `(multiple-value-bind (,oldCPort ,oldGDevice)
                          (get-gWorld)
       (require-trap #_setgWorld ,gWorld (%null-ptr))
       (unwind-protect 
         ,@body
         (require-trap #_setgWorld ,oldCPort ,oldGDevice)))))

(defmacro with-gworld-rectangle ((gworld-rect-var gworld) &body body)
  (let ((gwp (gensym)))
    `(rlet ((,gworld-rect-var :rect))
       (with-macptrs ((,gwp (require-trap #_GetGWorldPixMap ,gWorld)))
         (copy-record (%incf-ptr (%setf-macptr ,gwp (%get-ptr ,gwp)) (get-field-offset :pixmap.bounds))
                      :rect ,gworld-rect-var)
         ,@body))))

;;; WINDOW-TO-GWORLD
;;; window gWorld
;;; Blits the contents of window into gWorld
;;; window => a CLOS window object
;;; gWorld => a gWorld pointer
;;; *** DON'T CALL THIS WHEN THE DRAW-LOOP IS DRAWING ONTO THE GWORLD, SINCE THIS UNLOCKS
;;;     THE GWORLD WHEN IT'S DONE BLITTING -- WHICH IS EVIL FOR THE REMAINDER OF THE DRAW-LOOP

(defmacro window-to-gWorld (window gworld &optional dither blit-region)
  `(with-port ,gWorld
     (require-trap #_setGWorld ,gWorld (%null-ptr))
     (require-trap #_backColor (require-trap-constant #$WhiteColor))
     (require-trap #_foreColor (require-trap-constant #$blackColor))
     (lock-gworld ,gWorld)
     (unwind-protect
       (with-gWorld-rectangle (gWorld-rect ,gWorld)
         (Copy-Bits (window-pixmap ,window)
                    (gworld-pixmap ,gWorld)
                    (window-rectangle ,window)
                    gWorld-rect
                    (if ,dither 64 8) 
                    (if ,blit-region
                      ,blit-region
                      (rref (wptr ,window) :cGrafPort.visRgn))))
       (unlock-gworld ,gWorld))))

;;; SK8::VISUAL-EFFECT-BLIT
;;; Defined here as a stub. Redefined in VisualEffects.lisp
;;; In this version visual effects do not exist, thus the function just
;;; calls gWorld-to-window again, but without a special effect argument. 

(defun sk8::visual-effect-blit (visual-effect gworld clos-window dither blit-region
                                                  &optional speed)
  (declare (ignore visual-effect blit-region speed))
  (gWorld-to-window gWorld clos-window dither))

;;; GWORLD-TO-WINDOW
;;; gWorld window
;;; Blits the contents of gWorld into window.
;;; gWorld => a gWorld pointer
;;; window => a CLOS window object

(defmacro gWorld-to-window (gworld clos-window &optional dither blit-region visual-effect effect-speed)
  `(with-port (get-wmgr-port)
     (lock-gworld ,gWorld)
     (if ,visual-effect
       (unwind-protect
         (sk8::visual-effect-blit ,visual-effect ,gworld ,clos-window ,dither ,blit-region ,effect-speed)
         (unlock-gworld ,gWorld))
       (unwind-protect
         (with-gWorld-rectangle (gWorld-rect ,gWorld)
           (with-dereferenced-handles ((window-pixmap-ptr (window-pixmap ,clos-window)))
             ;; Open the clip! (If this is not done, the copying has no effect when the window is at
             ;; topleft {0,0}).
             (rlet ((r :rect :top -30000 :left -3000 :right 3000 :bottom 3000))
               (require-trap #_clipRect r))
             (require-trap #_CopyBits
                           (%get-ptr (require-trap #_getGWorldPixmap ,gWorld))
                           window-pixmap-ptr
                           gWorld-rect
                           gWorld-rect
                           8
                           (if ,blit-region
                             ,blit-region
                             (rref (wptr ,clos-window) :cGrafPort.visRgn)))))
         (unlock-gworld ,gWorld)))))

;;; Some people will want to call gWorld-to-Window making sure the origin of the
;;; gWorld is 0. This is what they would use.

(defmacro with-zero-origin (thePort &body body)
  (let ((savedOrigin (gensym)))
    `(let ((,savedOrigin (port-origin ,thePort)))
       (if (zerop ,savedOrigin)
         (progn ,@body)
         (with-port ,thePort
           (prog2 (require-trap #_setOrigin :long 0)
                  (progn ,@body)
                  (require-trap #_setOrigin :long ,savedOrigin)))))))

;;; When copying bits into gWorld make sure the port is set to the
;;; destination gWorld!!!

(defmacro gWorld-to-gWorld (sourceGWorld destGWorld &optional mode)
  `(with-port ,destGWorld
     (require-trap #_setGWorld ,destGWorld (%null-ptr))
     (unwind-protect
       (progn 
         (lock-gWorld ,sourceGWorld)
         (lock-gWorld ,destGWorld)
         (with-gWorld-rectangle (source-rect ,sourceGWorld)
           (with-gWorld-rectangle (dest-rect ,destGWorld)
             (copy-Bits (gworld-pixmap ,sourcegWorld)
                        (gworld-pixmap ,destgWorld)
                        source-rect
                        dest-rect
                        ,mode))))
       (unlock-gWorld ,sourceGWorld)
       (unlock-gWorld ,destGWorld))))

;;; Fills in the rect (which is assumed to be a qd rect) with the region's rgnbbox.

(defun %region-into-rect (region rect)
  (with-pointers ((pointer region))
    (when (not (handle-locked-p region))
      (error "Illegal attempt to get a pointer to a ~s within an unlocked handle" :rect))
    (%setf-macptr rect (%inc-ptr pointer 2))))

;;; _______________________________ 
;;; Balloon stuff.
;;; _______________________________ 

;;; BALLOONS?
;;; Returns T if balloon help is on; nil otherwise.

(defmacro Balloons? ()
  `(require-trap #_hmgetballoons))

;;; ISBALLOON?
;;; Returns T if a balloon is currently showing; nil otherwise.

(defmacro IsBalloon? ()
  `(require-trap #_hmIsBalloon))

;;; REMOVE-BALLOON
;;; Removes a balloon if one is showing; otherwise, does nothing.

(defmacro Remove-Balloon ()
  `(require-trap #_hmRemoveBalloon))

(defun Show-Balloon (text where &key (save-bits t) (rect (%null-ptr)))
  (rset *help-message-record* :hmmessageRecord.hmmString text)
  (without-interrupts
   (#_hmRemoveBalloon)
   (#_HMShowBalloon 
    *help-message-record* where rect (%null-ptr) 0 0 (%lisp-to-boolean save-bits))))

;;; _______________________________ 
;;; NEXT, THE FUNCTIONS
;;; _______________________________ 

;;; MODE-ARG
;;; thing
;;; Given either a pen mode keyword or number, returns the appropriate
;;; Quickdraw penmode number or reports and error.
;;; thing => either an integer between 0 and 64 or a QuickDraw pen mode keyword.
;;;          The available keywords can be found in the global *pen-modes*

(defun mode-arg (thing)
  (or (when (null thing) 8)
      (and (fixnump thing) (<= 0 thing 64) thing)
      (position thing *pen-modes*)
      (error "Unknown pen mode: ~a" thing)))

;;; COPY-BITS
;;; source-bitmap dest-bitmap source-rect dest-rect &optional mode mask-region
;;; Copies bits from source-bitmap to dest-bitmap

(defun copy-bits (source-bitmap dest-bitmap source-rect dest-rect
                                  &optional (mode 0) (mask-region (%null-ptr)))
  (with-pointers ((sb source-bitmap)
                  (db dest-bitmap))
    (#_CopyBits sb db source-rect dest-rect (mode-arg mode) mask-region)))

(defun copy-mask (source-bitmap maskBits dest-bitmap 
                                  source-rect mask-rect dest-rect)
  (with-pointers ((sb source-bitmap)
                  (mb maskBits)
                  (db dest-bitmap))
    (#_CopyMask sb mb db source-rect mask-rect dest-rect)))

;;; GET-WMGR-PORT
;;; returns a pointer to the window manager's color grafport

(defvar *wmgr-port* nil)

(defun get-wmgr-port-internal ()
  (%stack-block ((port 4))
    (#_GetCWMgrPort port)
    (%get-ptr port)))

(ccl::def-ccl-pointers *sk8-wmgr-port-restore* ()
  (setf *wmgr-port* (get-wmgr-port-internal)))

(defun get-wmgr-port ()
  *wmgr-port*)
                               
(defun text-font (fontName)
  (let ((fontCode fontName))
    (if (stringp fontName)
      (with-pstrs ((name fontName))
        (%stack-block ((id 2))
          (#_getfnum name id)
          (setq fontCode (%get-word id)))))
    (#_textfont fontCode)))
      
(defun get-gworld ()
  (%stack-block ((v1 4)
                 (v2 4))
    (#_GetGWorld v1 v2)
    (values (%get-ptr v1)
            (%get-ptr v2))))

(defun set-gWorld (port &optional (gdh (%null-ptr)))
  (#_SetGWorld port gdh))

(defun gWorld-device (offscreenGWorld)
  (#_GetGWorldDevice offscreenGWorld))

(defun gWorld-pixmap (offscreenGWorld)
  (#_GetGWorldPixMap offscreenGWorld))

;;; Note: update gWorld RETURNS the updated gWorld. So, you have to rebind it to your variable
;;;      before using it (use it just like you use new-gWorld).

;;; MCL3.0  - definition of $ClipPix is a little different - it is now the number, not the exponent

(defun update-gWorld (gWorld rect &optional (depth 0))
  ;; UpdateGWorld (VAR offscreenGWorld: GWorldPtr; pixelDepth:                        
  ;; INTEGER; boundsRect: Rect; cTable: CTabHandle; aGDevice: GDHandle; 
  ;; flags: GWorldFlags)
  (rlet ((offScreenGWorld :pointer))
    (setf (%get-ptr offScreenGWorld) gWorld)
    (#_updateGWorld offScreenGWorld depth rect (%null-ptr) (%null-ptr) #$clipPix)
    (%get-ptr offScreenGWorld)))

#|
	Change History (most recent last):
	2	10/21/93	kleiman	simplified resource file routines
	3	11/1/93	kleiman	OpenResources
	4	11/5/93	hernan	Added frienly note the window-to-gWorld (read it!).
	5	12/7/93	hernan	Making sure that new-gWorld uses the right depth
				when 0 gets passed. (The gWorld is made with the
				greatest depth available to all the monitors in use).
	6	2/18/94	sidney	Use open-resource-file that is defined in resources.lisp in library
	7	2/28/94	hernan	New-gWorld gets replaced by T_newGworldGC
				which makes a GCable pointer instead.
	8	3/1/94	sidney	handle some pointers correctly
	9	3/1/94	kleiman	package thing
	10	3/3/94	kleiman	best-depth-available -> bestColorDepth
	11	3/10/94	Hernan	Adding gWorld-to-gWorld to copy bits accross
				gWorlds.
	12	3/11/94	Hernan	Adding a function to swap the pixmap in a
				gWorld.
	13	3/28/94	Hernan	Fixing window-to-gWorld to correctly set the
				port before blitting.
	14	3/29/94	sidney	Fixed gWorld-to-gWorld to set the port correcly.
				(Hernan).
	15	3/30/94	Hernan	Before calling copy-bits set the fore and back
				colors to black and white. This seems to fix the
				problem in which fields do not show up.
	16	3/31/94	Brian	Wrapping #$WhiteColor in require-trap-constant
				so it doesn't get stuck in the TRAPS package and
				crash restore.
	17	4/19/94	Hernan	Low level hacks to get rid of consing in gWorld-
				to-window.
	18	4/19/94	Hernan	We still have to come back and stop gWorld-to-
				window from consing those f*$%$% 16 bytes!
	19	4/20/94	Hernan	Adding a function that gets a rect out of a region
				without consing.
	20	4/29/94	Hernan	Since we do not allow dithering, set the mode to 
				8 by default.
	21	4/29/94	rod	Accounting for the possibility that thing is nil in
				mode-arg.
	22	7/14/94	Hernan	1171670: opening the clip before doing gworld-to-
				window. This fixes the weirdness at topleft {0,0} problem.
	23 	10/31/94	sidney  	The resource functions that were defined here are now obsolete
	24 	11/ 4/94	Hernan  	Added a macro to return the origin of any port as
							a long integer.
	25 	12/19/94	dy      	Use new %lisp-to-boolean macro
	26 	 2/ 3/95	Hernan  	Defining hrref which calls rref after dereferencing
							the original handle it was given.
	27 	 2/10/95	dy      	tweaks to invocations of macros that reduce to make-terminable-macptr
	28 	 5/ 1/95	Hernan  	Adding a macro that does its body with the origin of the
							port given set to 0.
	2  	10/ 9/96	Hernan  	Removing functions noone calls.
	3  	 2/27/97	Hernan  	
|# ;(do not edit past this line!!)
