;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories

(in-package :sk8dev)

(provide "ANIMATEDCURSOR")

(require "REALTIMECLOCK" "objects;Clocks:RealTimeClock")
(require "COLORCURSOR" "objects;Media:ColorCursor")

;;; ___________________________
;;; Animated Cursors!
;;; ___________________________

;;; Every time it ticks, this object sets the cursor to the next
;;; cursor in its media (a list of cursorRSRCs).

(new realTimeclock :objectname "AnimatedCursor" :project sk8
     :properties '(lastCursor media))

(setf (lastCursor AnimatedCursor) 1)
(setf (ticksPerSecond AnimatedCursor) 5)

(define-handler tick (AnimatedCursor)
  (if (neq me (cursor stage))
    ;; If the cursor has been changed, stop ticking.
    (stop me)
    ;; Keep cycling through.
    (let* ((cursors (media me))
           (lastIndex (or (lastCursor me) 0))
           (newIndex (if (>= lastIndex (length cursors)) 1 (1+ lastIndex)))
           (newCursor (nth newIndex cursors)))
      (setf (lastCursor me) newIndex)
      (set-cursor-internal newCursor))))

(define-handler stop (animatedCursor)
  (call-next-method)
  (setf (cursor stage) standardCursor))

(define-handler update (AnimatedCursor)
  (let (theCursor)
    (setf theCursor (nth (or (lastCursor me) 1) (media me)))
    (when theCursor
      (update theCursor))))

(define-handler install (AnimatedCursor)
  ;; If the cursor is animated. Start the clock!
  (set-cursor-internal (nth (or (lastCursor me) 1) (media me)))
  (start me)
  me)

;;; The SK8 animated clock cursor!

(new cursorRSRC :objectname "Cursor12OClock" :resourceId 15600 :project SK8)
(new cursorRSRC :objectname "Cursor1OClock" :resourceId 15601 :project SK8)
(new cursorRSRC :objectname "Cursor3OClock" :resourceId 15602 :project SK8)
(new cursorRSRC :objectname "Cursor4OClock" :resourceId 15603 :project SK8)
(new cursorRSRC :objectname "Cursor6OClock" :resourceId 15604 :project SK8)
(new cursorRSRC :objectname "Cursor7OClock" :resourceId 15605 :project SK8)
(new cursorRSRC :objectname "Cursor9OClock" :resourceId 15606 :project SK8)
(new cursorRSRC :objectname "Cursor10OClock" :resourceId 15607 :project SK8)

(new AnimatedCursor :objectname "AnimatedClock" :project SK8)

(setf (ticksPerSecond AnimatedClock) 8)
(setf (media AnimatedClock) 
      (reverse (list Cursor10OClock Cursor9OClock Cursor7OClock
                     Cursor6OClock Cursor4OClock Cursor3OClock
                     Cursor1OClock Cursor12OClock)))

;;; The SK8 animated error cursor!

(new ColorCursorRSRC :objectName "Error1Cursor" :resourceid 128 :project SK8)
(new ColorCursorRSRC :objectName "Error2Cursor" :resourceid 129 :project SK8)
(new ColorCursorRSRC :objectName "Error3Cursor" :resourceid 130 :project SK8)
(new ColorCursorRSRC :objectName "Error4Cursor" :resourceid 131 :project SK8)
(new ColorCursorRSRC :objectName "Error5Cursor" :resourceid 132 :project SK8)
(new ColorCursorRSRC :objectName "Error6Cursor" :resourceid 133 :project SK8)
(new ColorCursorRSRC :objectName "Error7Cursor" :resourceid 134 :project SK8)

(new AnimatedCursor :objectname "AnimatedErrorCursor" :project SK8)

(setf (ticksPerSecond AnimatedErrorCursor) 30)
(setf (media AnimatedErrorCursor)
      (list Error1Cursor Error2Cursor Error3Cursor Error4Cursor
            Error5Cursor Error6Cursor Error7Cursor
            Error7Cursor Error6Cursor Error5Cursor Error4Cursor
            Error3Cursor Error2Cursor Error1Cursor))



#|
	Change History (most recent last):
	2  	 2/27/97	Hernan  	
|# ;(do not edit past this line!!)
