;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories

(in-package :sk8dev)

(provide "MINUTESCLOCK")

(require "SLAVECLOCK" "objects;Clocks:SlaveClock")
(require "DATETIME" "objects;Clocks:DateTime")

(new slaveClock :objectname "MinutesClock" :undisposable t 
     :project sk8
     :masterTicksPerTick 60
     :master secondsClock)

(define-handler currentTime (MinutesClock)
  (minute Now))


#|
	Change History (most recent last):
	2  	 2/27/97	Hernan  	
|# ;(do not edit past this line!!)
