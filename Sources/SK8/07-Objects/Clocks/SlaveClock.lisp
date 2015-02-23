;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories

(in-package :sk8dev)

(provide "SLAVECLOCK")

(require "CLOCK" "objects;Clocks:Clock")

;;; ______________
;;; Slave Clocks: tick according to the number of times its master ticks.
;;; ______________

(new Clock :objectname "SlaveClock" :project sk8 :undisposable t
     :properties '((masterTicks :value 0)
                   (masterTicksPerTick :value 10)))

(define-handler maybe-tick :private (slaveClock)
                (setf (masterTicks me) (1+ (masterTicks me)))
                (when (zerop (mod (masterTicks me) (masterTicksPerTick me)))
                  (tick me)))


#|
	Change History (most recent last):
	2  	 2/27/97	Hernan  	
|# ;(do not edit past this line!!)
