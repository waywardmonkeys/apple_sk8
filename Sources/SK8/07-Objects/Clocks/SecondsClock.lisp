;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories

(in-package :sk8dev)

(provide "SECONDSCLOCK")

(require "REALTIMECLOCK" "objects;Clocks:RealTimeClock")
(require "DATETIME" "objects;Clocks:DateTime")

;;; Requires Date&Time.

(new realTimeClock :objectname "SecondsClock" :project sk8 :undisposable t
     :ticksPerSecond 1)

(define-handler currentTime (SecondsClock)
  (seconds Now))


#|
	Change History (most recent last):
	2  	 2/27/97	Hernan  	
|# ;(do not edit past this line!!)
