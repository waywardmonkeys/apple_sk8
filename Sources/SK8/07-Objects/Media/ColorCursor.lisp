;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories

(in-package :sk8dev)

(provide "COLORCURSOR")

;;; ___________________________
;;; Color Cursors!
;;; ___________________________

(new Media :objectname "ColorCursorRSRC" :project sk8)

(define-handler restype (colorCursorRSRC) 
  "crsr")

;;; This function deals with setting the color cursor.

(defun colorCursorHook ()
  (without-interrupts
   (let ((theCursor (cursor Stage)))
     (when theCursor
       (update theCursor)))))

(define-handler update (ColorCursorRSRC)
  (let (theHandle)
    (setf theHandle (loadMedia me))
    (when (handlep theHandle)
      (#_setCCursor theHandle))))

(define-handler cursorHook (ColorCursorRSRC)
  'colorCursorHook)

(define-handler install (ColorCursorRSRC)
  (set-cursor-internal me)
  me)

(define-handler displaySample (ColorCursorRSRC theActor)
  (declare (ignore theActor))
  (setf (cursor Stage) me))


#|
	Change History (most recent last):
	2  	 2/27/97	Hernan  	
|# ;(do not edit past this line!!)
