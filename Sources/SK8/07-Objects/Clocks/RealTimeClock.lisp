;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories

(in-package :sk8dev)

(provide "REALTIMECLOCK")

(require "CLOCK" "objects;Clocks:Clock")

;;; ______________
;;; Real time clock: ticks a certain number of times per second.
;;; ______________

(new Clock :objectname "RealTimeClock" :project sk8 :undisposable t
     :properties '((ticksPerSecond :value nil)
                   (increment :value nil)
                   (previousRealTime :value 0)))

(define-handler initializeFromStore (realTimeClock)
  (call-next-method)
  (setf (previousRealTime me) 0))

(define-handler (setf ticksPerSecond) (num realTimeClock)
  (prog1 
    (setf (slot-value me 'ticksPerSecond) num)
    (setf (increment me) (/ internal-time-units-per-second num))))

(setf (ticksPerSecond realTimeClock) 1)

(define-handler maybe-tick :private (realTimeClock)
                (let ((prev (previousRealTime me))
                      (nnow (get-internal-real-time)))
                  (when (> (- nnow prev) (increment me))
                    (setf (previousRealTime me) nnow) 
                    (tick me))))

;;; By default will be slaved to the system clock. (To get best performance!)

(define-handler (setf running) (boolean realTimeClock)
  (if boolean
    (setf (master me) systemClock)
    (setf (master me) nil))
  (call-next-method))

;;; Set to 0 so that the next time we come up we pass the tick text
;;; (the real time might be > on the new machine than the saved time).

(define-handler preserve (realTimeClock)
  (call-next-method)
  (setf (previousRealTime me) 0))

#|
	Change History (most recent last):
	2  	 2/27/97	Hernan  	
|# ;(do not edit past this line!!)
