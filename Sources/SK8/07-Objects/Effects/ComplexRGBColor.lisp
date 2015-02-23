;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories

(in-package :sk8dev)

(provide "COMPLEXRGBCOLOR")

(require "RENDERER")

;;_____________________________________________________________________________________
;; ComplexRGBColor = RGB + penmode + backcolor + opcolor + pattern.

;;;  Added pen pattern here to make it the general renderer it should be.
;;;  This now supports all standard quickdraw parameters...  -Brian


(new RGBColor
     :objectName "ComplexRGBColor"
     :undisposable t
     :prototype t
     :properties '((backRed :value 65535)
                   (backGreen :value 65535)
                   (backBlue :value 65535)
                   (opRed :value 0)
                   (opGreen :value 0)
                   (opBlue :value 0)
                   (penMode :value srccopy)
                   (media :value nil))
     :project sk8)

(define-handler (setf backRed) (newValue ComplexRGBColor)
  (setf (slot-value me 'backRed) (gs:f.round newValue)))

(define-handler (setf backGreen) (newValue ComplexRGBColor)
  (setf (slot-value me 'backGreen) (gs:f.round newValue)))

(define-handler (setf backBlue) (newValue ComplexRGBColor)
  (setf (slot-value me 'backBlue) (gs:f.round newValue)))

(define-handler (setf opRed) (newValue ComplexRGBColor)
  (setf (slot-value me 'opRed) (gs:f.round newValue)))

(define-handler (setf opGreen) (newValue ComplexRGBColor)
  (setf (slot-value me 'opGreen) (gs:f.round newValue)))

(define-handler (setf opBlue) (newValue ComplexRGBColor)
  (setf (slot-value me 'opBlue) (gs:f.round newValue)))

(define-handler (setf media) (theval ComplexRGBColor)
  (if (or (not theval) (inheritsfrom theval bwpattern))
    (sk8::setValue 'media me theval)
    (sk8-error PropertyTypeMismatchError
               :object        theVal
               :expectedType  BWPattern
               :ownerObject   ComplexRGBColor
               :propertyName 'media
               )))

(define-handler render (ComplexRGBColor theActor region thePaper)
  (declare (ignore theActor thePaper))
  (gs:let+ ((theMedia (media me))
            (theHandle (and themedia (loadMedia theMedia)))
            (opColor (:rgbColor :red (opRed me)
                                :green (opGreen me)
                                :blue (opBlue me)))
            (foreColor (:rgbColor :red (foreRed me)
                                  :green (foreGreen me)
                                  :blue (foreBlue me)))
            (backColor (:rgbColor :red (backRed me)
                                  :green (backGreen me)
                                  :blue (backBlue me))))
    (gs:with-composed-clip region
      (#_OpColor opColor)
      (#_PenMode (mode-arg (penMode me)))
      (#_RGBBackColor backColor)
      (#_RGBForeColor foreColor)
      (if thehandle (with-pointers ((pntr thehandle))
                      (#_PenPat pntr)))
      (#_PaintRgn region)
      (#_PenNormal)
      ;; whatever you do, don't remove this next line.  It will blow
      ;; away the entire graphic system (in 1 bit mode).
      ;; We are not joking.
      ;; Don't do it!
      ;; OJO!!
      (#_BackColor #$whiteColor)
      )
    ))

(define-handler translucent (ComplexRGBColor)
  (when (memq (mode-arg (penMode me))
              (list #$Invert #$SrcOr #$srcXor #$PatXor
                    #$srcBic #$notSrcOr #$notPatXor #$notPatBic
                    #$blend #$addPin #$addOver #$adMin
                    #$AdMax #$subPin #$subOver 50))
    t))

;;; This case also needs to be defined for COMPLEXRGBColor!!!

(define-handler render-string (ComplexRGBColor theActor str x y theFont size style thePaper)
  ;; we can ignore the other info because it is already set in the current port!
  (gs:let+ ((region (:region))
            (rect (:rect))
            (bRect (gs:recompute-physicalBoundsRect theActor)))
    (#_SetRect rect (gs:rect-left bRect) (gs:rect-top bRect) (gs:rect-right bRect) (gs:rect-bottom bRect))
    (if (gs:string-region rect region str x y (fontData theFont) size style)
      (render me theActor region thePaper)
      ;; we failed!  Do the normal
      (render-string black theActor str x y theFont size style thePaper))))

;;; _______________________________ 
;;; Instances...
;;; _______________________________ 

(new ComplexRGBColor :objectName "Slightlydarker"
     :FORERED 10000 :FOREGREEN 10000 :FOREBLUE 10000
     :OPRED 0 :OPGREEN 0 :OPBLUE 0
     :project sk8)
(SETF (PENMODE SLIGHTLYDARKER) 'SUBPIN)

(new ComplexRGBColor 
     :objectName "Darker" 
     :FORERED 20000 :FOREGREEN 20000 :FOREBLUE 20000
     :OPRED 0 :OPGREEN 0 :OPBLUE 0
     :project sk8)
(SETF (PENMODE DARKER) 'SUBPIN)

(new ComplexRGBColor 
     :objectName "Muchdarker"
     :FORERED 30000 :FOREGREEN 30000 :FOREBLUE 30000
     :OPRED 0 :OPGREEN 0 :OPBLUE 0
     :project sk8)
(SETF (PENMODE MUCHDARKER) 'SUBPIN)

(new ComplexRGBColor 
     :objectName "Slightlylighter" 
     :FORERED 10000 :FOREGREEN 10000 :FOREBLUE 10000
     :OPRED 65535 :OPGREEN 65535 :OPBLUE 65535
     :project sk8)
(SETF (PENMODE SLIGHTLYLIGHTER) 'ADDPIN)

(new ComplexRGBColor 
     :objectName "Lighter" 
     :FORERED 20000 :FOREGREEN 20000 :FOREBLUE 20000
     :OPRED 65535 :OPGREEN 65535 :OPBLUE 65535
     :project sk8)
(SETF (PENMODE LIGHTER) 'ADDPIN)

(new ComplexRGBColor 
     :objectName "Muchlighter" 
     :FORERED 30000 :FOREGREEN 30000 :FOREBLUE 30000
     :OPRED 65535 :OPGREEN 65535 :OPBLUE 65535
     :project sk8)
(SETF (PENMODE MUCHLIGHTER) 'ADDPIN)

(new ComplexRGBColor 
     :objectName "Translucent" 
     :FORERED 65535 :FOREGREEN 65535 :FOREBLUE 65535 
     :OPRED 32768 :OPGREEN 32768 :OPBLUE 32768
     :project sk8)
(SETF (PENMODE TRANSLUCENT) 'BLEND)

(new ComplexRGBColor :objectName "Blendyellow" 
     :foreRed 65535 :foreGreen 65535 :foreBlue 0
     :opRed 32768 :opGreen 32768 :opBlue 32768
     :penMode 'blend
     :project sk8)

;; more intricate new colors

(new ComplexRGBColor :objectName "Invert" :project sk8)
(setf (penMode invert) 'PatXor
      (foreRed invert) 0
      (foreBlue invert) 0
      (foreGreen invert) 0)

(define-handler translucent (invert)
  t)

#|
	Change History (most recent last):
	4  	 2/27/97	Hernan  	define-system-handler->define-handler.
						3   5/ 2/96Hernan  Needs to recompile...
|# ;(do not edit past this line!!)
