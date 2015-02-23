;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories


;;;


(in-package :SK8Development)




#| Modification History

03-15-93 ruben pointerState and Mouse
02-22-93 ruben size takes &key
01-22-93 ruben id -> objectName

|#


#| To Do

Ensure that insert/mount event is handled by SK8
So far, we account only for the load-time environment.

|#

(export '(ControlKeyDown CAPSKeyDown commandKeyDown optionKeyDown shiftKeyDown))

;;;;;;;;;;;;;
;;; DEVICE
;;;;;;;;;;;;;

 
(new object :objectName "Device" :undisposable t
     :properties '((name :value "Unknown"))
     :project sk8)

(define-handler writeObject (Device str rereadably)
  (call-next-method))

#|

;; generic printer for descendant of Device that indicates what type of device it is

  (unless (maybeWriteObjectName me str)
    (when rereadably
      (dolist (c (knownchildren Device))
        (when (inheritsfrom me c)
          (write-string "the "str)
          (write-string (objectname c) str)
          (write-string " " str)
          (return nil))))
    (writeObject (name me) str t)))

|#

;;;;;;;;;;;;;
;;; MONITOR
;;;;;;;;;;;;;
              
;;; Monitors are created on disk insert events and at system load time
;;; Monitors are deleted on disk eject events and at image save time

(new Device :objectName "Monitor" :undisposable t
     :properties '(data)
     :project sk8)

(define-handler restore (Monitor)
  (when (eq me Monitor)
    (dolist (c (knownchildren me)) (make-object-gcable c))
    (let (new)
      (let ((data (require-trap #_getdevicelist)))
        (loop (when (%null-ptr-p data) (return t))
              (when (require-trap #_testdeviceattribute data 13) ; ensure screen
                (setf new (new Monitor :data data :project sk8))
                ;; It is a mistery why the previous line does not achieve this.
                (setf (data new) data)
                (push new (slot-value me 'sk8::knownChildren))) ; MUST be locatable
              (setq data (require-trap #_getnextdevice data)))))))

(define-handler preserve (Monitor)
  (unless (eq me Monitor)
    (make-object-gcable me)))
 
;;; Returns true if this is the main monitor of the system
;;;  The sense of "main" is defined by the native operating system
;;;
(define-handler mainMonitor (Monitor)
  (unless (eq me Monitor)
    (require-trap #_testdeviceattribute (data me) 11)))

;;; Returns the maximum number of bits per pixel supported by this monitor
;;;
(define-handler colorDepth (Monitor)
  (unless (eq me Monitor)
    (ccl::screen-bits (data me))))

(define-handler (setf colorDepth) (depth Monitor)
  (unless (eq me Monitor)
    (unless (memq depth '(1 2 4 8 16 32)) 
      (sk8-error PropertyTypeMismatchError
                 :object        depth
                 :expectedType  '(1 2 4 8 16 32)
                 :ownerObject   Monitor
                 :propertyName 'colorDepth
                 ))
    ;; Set the slot!
    ; (setf (slot-value me 'colorDepth) depth)
    (let ((data (data me))
          (color (if (or (= depth 1)
                         (= depth 0))
                   0
                   1)))
      (when (and (handlep data)
                 (require-trap #_hasdepth data depth 0 color))
        (require-trap #_setdepth data depth 0 color)
        (update-windows-for-new-depth) ;; Update all windows that need to be updated
        depth))))

(define-handler bestColorDepth (System)
  (let ((result 1)
        newDepth)
    (dolist (c (monitors system) result)
      (when (> (setf newDepth (colorDepth c)) result)
        (setf result newDepth)))))

(define-handler color (Monitor)
  (unless (eq me Monitor)
    (ccl::screen-color-p (data me))))

(define-handler location (Monitor &key)
  (unless (eq me Monitor)
    (let ((location (ccl::screen-position (data me))))
      (SK8-multivals (point-h location) (point-v location)))))

(define-handler active (Monitor)
  (unless (eq me Monitor)
    (ccl::screen-active-p (data me))))

(define-handler size (Monitor &key)
  (unless (eq me Monitor)
    (let ((size (ccl::screen-size (data me))))
      (SK8-multivals (point-h size) (point-v size)))))

;;; boundsRect -- Returns the bounds rect of the monitor
;;;

(define-handler boundsRect (monitor &key)
  (unless (eq me Monitor)
    (let ((location (ccl::screen-position (data me)))
          (size (ccl::screen-size (data me))))
      (SK8-multivals 
       (point-h location) (point-v location)
       (+ (point-h location) (point-h size)) (+ (point-v location) (point-v size))))))

; fat chance
(define-system-handler (setf boundsRect) (bounds Monitor)
  (declare (ignore bounds)))

(restore monitor) ; for boot session

#|
;;;;;;;;;;;;;
;;; PRINTER
;;;;;;;;;;;;;

(new device :objectName "Printer" :project sk8  :undisposable t)
|#

;;;;;;;;;;;;;
;;; StorageDevice
;;;;;;;;;;;;;


(new device :objectName "StorageDevice" :project sk8  :undisposable t)

(define-handler restore (StorageDevice)
  (when (eq me StorageDevice)
    (dolist (c (knownchildren me)) (make-object-gcable c))
    (dolist (d (mounted-volumes))
      (pushnew (new StorageDevice :name (namestring d) :project sk8)
               (slot-value me 'knownchildren)))
    (setf (slot-value me 'knownchildren)
          (nreverse (slot-value me 'knownchildren)))))

(define-handler preserve (storagedevice)
  (unless (eq me StorageDevice)
    (make-object-gcable me)))

(define-handler flush (StorageDevice)
  (flush-volume (name me)))

(define-handler number (StorageDevice)
  (volume-number (name me)))

(define-handler drive (StorageDevice)
  (drive-number (name me)))

(define-handler ejected (StorageDevice)
  (disk-ejected-p (name me)))

(define-handler unmount (StorageDevice)
  (eject&unmount-disk (name me))
  (make-object-gcable me))

(restore StorageDevice)

#| obsoleted 3/3/94 until we have animation in place; audioCD support not in feature list
;;;;;;;;;;;;;;;;;;;
;;; MultimediaDevice
;;;;;;;;;;;;;;;;;;;

(new StorageDevice :project sk8 :undisposable t
     :otherparents Orchestrator :objectname "MediaPlayerDevice")

(new MediaPlayerDevice :project sk8 :undisposable t
     :objectname "AudioCD")

(define-handler goTo (AudioCD targetTrack)
  (declare (ignore targetTrack)))

(define-handler currentTrack (AudioCD)
  )

(define-handler (setf currentTrack) (currentTrack AudioCD)
  (declare (ignore currentTrack))
  )

(define-handler currentTrackNumber (AudioCD)
  )

(define-handler (setf currentTrackNumber) (trackNumber AudioCD)
  (declare (ignore trackNumber))
  )

(define-handler currentTrackDuration (AudioCD)
  )

(define-handler currentCD (AudioCD)
  )

(define-handler (setf currentCD) (currentCD AudioCD)
  (declare (ignore currentCD))
  )
|#

;;;;;;;;;;;;;
;;; KEYBOARD
;;;;;;;;;;;;;

(new device :objectName "Keyboard" :project sk8  :undisposable t)

(define-system-handler restore (Keyboard)
  (when (eq me keyboard)
    (dolist (c (knownchildren me)) (make-object-gcable c))
    (pushnew (new Keyboard :project sk8
                  :name (cdr (assq (ccl::gestalt "kbd ") ccl::*known-gestalt-keyboard-types*)))
             (slot-value me 'knownchildren))))

(define-handler preserve (keyboard)
  (unless (eq me keyboard)
    (make-object-gcable me)))

;;; keysDown -- returns either nil or a collection of keys which are currently depressed
;;;           This does not return the modifier keys: use handlers like shiftKeyDown
;;;           to get modifier key status.
;;;
;;; AS OF NOW THIS RETURNS ONLY A LIST OF VIRTUAL KEYCODES.  SHOULD CALL
;;; THE KEYTRANS TRAP TO CONVERT THESE TO CHARACTERS.

(define-system-handler keysDown (Keyboard)
  (let (keys
        ;;newkeys
        ;;(kchr (#_getscriptvariable smsystemscript smscriptkeys))
        )
    (getkeys)
    (dotimes (i 128) 
      (if (multiple-value-bind (thebyte thebit) (truncate i 8)
            (logbitp theBit (%get-byte mf::*keymap* theByte)))
        (push i keys))
      )
    ;;;(dolist (i keys)
    ;;;  (#_keytrans kchr i returnval)
    ;;;  (push (convert_bits_23-16_to_a_char returnval) newkeys))
    keys))

;;; It might be the case that this function is called at load time before
;;; keymap is restored. In that case we return nil.

(define-sk8-function getKeys nil ()
  (when mf::*keymap*
    (#_GetKeys mf::*keymap*)))

(defmacro %getBit (ptr bit)
  (multiple-value-bind (thebyte thebit) (truncate bit 8)
    `(logbitp ,theBit (%get-byte ,ptr ,theByte))))

(define-sk8-function shiftKeyDown nil ()
  (when mf::*keymap*
    (getKeys)
    (%getbit mf::*keymap* 56)))

(define-sk8-function optionKeyDown nil ()
  (when mf::*keymap*
    (getKeys)
    (%getbit mf::*keymap* 58)))

(define-sk8-function commandKeyDown nil ()
  (when mf::*keymap*
    (getKeys)
    (%getbit mf::*keymap* 55)))

(define-sk8-function CAPSKeyDown nil ()
  (when mf::*keymap*
    (getKeys)
    (%getbit mf::*keymap* 57)))

(define-sk8-function ControlKeyDown nil ()
  (when mf::*keymap*
    (getKeys)
    (%getbit mf::*keymap* 59)))

(restore keyboard)

;;;;;;;;;;;
;;; POINTER
;;;;;;;;;;;

(new device :objectName "Pointer" :project sk8  :undisposable t)

(define-system-handler pointerState (Pointer)
  (if (mouse-down-p)
    'down
    'up))

;;(mf::publish-project-symbol 'down mf::*SK8-package*)
;;(mf::publish-project-symbol 'up mf::*SK8-package*)


(new Pointer :objectName "Mouse" :project sk8 :undisposable t)

(define-system-handler up (Mouse)
  (not (mouse-down-p)))

(define-system-handler down (Mouse)
  (mouse-down-p))

(define-system-handler location (Mouse)
  (mouseloc stage))

(define-system-handler h (Mouse)
  (hMouse stage))

(define-system-handler v (Mouse)
  (vMouse stage))

#|
;;;;;;;;;;;
;;; MODEM
;;;;;;;;;;;

(new device :objectname "Modem" :project sk8  :undisposable t)
|#

#|
;;;;;;;;;;;
;;; AUDIO
;;;;;;;;;;;

(new device :objectName "SoundChannel" :project sk8  :undisposable t)
|#


#|
	Change History (most recent last):
	2	6/15/93	kleiman	online documentation
	3	6/16/93	chip	deleted many cases of extra arg in call to doc registry!!!
	4	6/25/93	kleiman	depth -> colorDepth
	5	7/1/93	kleiman	MediaPlayerDevice, AudioCD
	13	8/27/93	chip	fixed arglist (for congruency) of goTo (of AudioCD)
	14	8/31/93	Brian Roddy	Adding Tillman's printObject functions for device
				and storagedevice
	15	10/15/93	kleiman	add check for mainMonitor
	16	10/15/93	hernan	Making sure the monitor's data field is set.
	17	11/30/93	kleiman	export getKeys and other functions
	18	12/7/93	hernan	Adding function that returns the best depth
				available in all the monitors.
	19	12/21/93	kleiman	take out documentation strings
	21	1/31/94	hernan	Adding a function that updates all windows on the
				stage to make use of a new colorDepth.
	22	1/31/94	kleiman	Restore the monitors at the end of the file!!!
	23	2/14/94	sidney	rename children to knownchildren
	24	2/21/94	hernan	The Final Renaming for Alpha! (yeah, right...)
	25	2/28/94	chip	StorageDevice now prints itself using the object-literal syntax; took out printObject of Device, since devices shouldn't print specially by default
	26	2/28/94	chip	Minor fix to StorageDevice's printObject
	27	3/3/94	kleiman	restore/Monitor: new monitors inserted into the
				knownChildren list + other code review fixed
	28	3/6/94	chip	print... --> write...
	29	3/8/94	kleiman	OBSOLETE DISPOSE!
	30	6/17/94	rod	Adding Location, H, and V handlers to the mouse.
	31	6/19/94	sidney	1169273 nice print format for all device objects
	32	6/29/94	chip	changed restypes to strings (from keywords) since symbols now have arbitrary case
	33	8/3/94	till	Restore of StorageDevice, change (directory "*:")
				to (mounted-volumes).  The former just doesn't
				work reliable wrt timed out desktop thingies.
	34	8/3/94	till	Moved mounted-volumes and drive-name-soft
				to here from 04-files for loading order reasons.
	35 	 9/16/94	rod     	
	36 	12/ 7/94	Hernan  	Replacing error with sk8-error where possible.
	37 	 3/ 3/95	Hernan  	Made every user of keymap not die when the
							keymap is nil (like at restore time).
	38 	 4/12/95	rod     	Removing undefined devices.
	2  	 8/17/95	sidney  	can't use 'pointer' as local symbol
	2  	 5/ 7/96	sidney  	Changes for new object system
	2  	 2/27/97	Hernan  	
|# ;(do not edit past this line!!)
