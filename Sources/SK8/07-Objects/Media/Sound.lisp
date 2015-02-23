;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories



(in-package :sk8dev)

(provide "SOUND")

(require "REALTIMECLOCK" "objects;Clocks:RealTimeClock")

;;; Requires RealTimeClock. (silly though...)

;;; _______________________________ 
;;; Sounds
;;; _______________________________ 

(new RealTimeClock :objectname "Sound" :project sk8 :properties '((media :value nil)
                                                                  (channel :value nil)))

(define-handler play (sound &key (synchronously nil))
  (play (media me) :synchronously synchronously)
  (started me)
  t)

(define-handler start (sound &key (synchronously nil))
  (play me :synchronously synchronously))

(define-handler stop (Sound &key)
  (stop (media me))
  (stopped me))

(define-handler running (Sound)
  (and (media me)
       (running (media me))))

(define-handler (setf running) (theval Sound)
  (if theval
    (unless (running me) (start me))
    (when (running me) (stop me))
    ))

;;; Built-in Sounds

(new Sound :objectname "ModalSound" :project sk8)
(setf (media ModalSound) ModalSoundRsrc)

(new Sound :objectname "ModelessSound" :project sk8)
(setf (media ModelessSound) ModelessSoundRsrc)

(new Sound :objectname "ZoomdownSound" :project sk8)
(setf (media ZoomdownSound) ZoomdownSoundRsrc)

(new Sound :objectname "ZoomupSound" :project sk8)
(setf (media ZoomupSound) ZoomupSoundRsrc)

#|
	Change History (most recent last):
	8	2/21/94	hernan	The Final Renaming for Alpha! (yeah, right...)
	9	2/28/94	hernan	Now using GCable mac things.
	10	3/7/94	Hernan	It seems that the sndPtr cannot be gcable... Not
				using GCable stuff for it anymore. This should not
				be a problem since we dispose the sound channels
				by hand.
	11	6/17/94	kleiman	Sound is now directly a child of RealTimeClock
	12 	10/ 3/94	Hernan  	machandle -> loadMedia.
	13 	11/ 4/94	Hernan  	I cannot remember what I did to this...
	14 	 1/17/95	till    	trapwrapper for LoadResource
	15 	 3/ 2/95	rod     	fixing spelling error in start of sound
	16 	 3/ 6/95	rod     	Various fixes to sounds.  Made both setter and
							getter of the running property work.  Fixed stop
							of soundrsrc to clear channel so it can be gc-ed.
	17 	 3/17/95	till    	trap-wrapping, newptr in play
	3  	 2/27/97	Hernan  	define-system-handler->define-handler.
|# ;(do not edit past this line!!)
