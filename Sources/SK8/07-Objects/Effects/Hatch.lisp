;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories

(in-package :sk8dev)

(provide "HATCH")

(require "RENDERER")

;;;______________________________________________________________________
;;; Hatch

;;; This is a simplified version of what it used to be. You can only use rgbs for the
;;; forecolor, backcolor and opcolor. (The other fancy features from before were not
;;; being used.)

(new Renderer
     :objectName "Hatch"
     :undisposable t
     :prototype t
     :properties '((spacing :value 12)
                   (startAt :value nil)
                   (penSize :value (1 1))
                   (hatchType :value horizontal)
                   foreGroundRenderer
                   backgroundRenderer)
     :project sk8)

(setf (foreGroundRenderer hatch) white)
(setf (backgroundRenderer hatch) black)

(define-handler (setf penSize) (newValue Hatch)
  (if (and (every #'numberp newValue) (= 2 (length newValue)))
    ;; Round and set the slot.
    (progn
      (setf (car newValue) (gs:f.round (car newValue)))
      (setf (cadr newValue) (gs:f.round (cadr newValue)))
      (setf (slot-value me 'penSize) newValue))
    (sk8-error GeneralProgrammaticError
               :strings '("The pensize should be a list of 2 integers."))))

(define-handler (setf hatchType) (newValue hatch)
  (if (memq newValue '(vertical horizontal both))
    (setf (slot-value me 'hatchType) newValue)
    (sk8-error PropertyTypeMismatchError
               :object        newValue
               :expectedType  '(vertical horizontal both)
               :ownerObject   Hatch
               :propertyName 'hatchType
               )))

(define-handler (setf backgroundRenderer) (theval hatch)
  (when (and (inheritsfrom theval renderer)
             (neq theval me))
    (sk8::setValue 'backgroundRenderer me theval)))

(define-handler render (hatch theActor region thePaper)
  (gs:let+ ((trgn (:region))
            (currentClip (:region)))
    (#_getClip currentClip)
    (#_SectRgn currentClip region trgn)
    ;; get the records
    (gs:let+ ((foreGroundRenderer (foreGroundRenderer me))
              (backgroundRenderer (backgroundRenderer me))
              (hatchSpacing (spacing me))
              (qd-rect (:rect))
              (qd-rect2 (:rect))
              (penSize (penSize me))
              startVal numLines left top uleft uright utop ubottom)
      (gs:region-into-rect region qd-rect)
      (setf left (rref qd-rect :rect.left)
            top (rref qd-rect :rect.top))
      (gs:region-into-rect trgn qd-rect2)
      (setf uleft (rref qd-rect2 :rect.left)
            uright (rref qd-rect2 :rect.right)
            utop (rref qd-rect2 :rect.top)
            ubottom (rref qd-rect2 :rect.bottom))
      ;; Paint the background!
      (when backgroundRenderer
        (render backgroundRenderer theActor region thePaper))
      ;; Set the pen attributes.
      (#_penNormal)
      ;; Set the color to the front renderer.
      (with-rgb (theColor (mcl-color foreGroundRenderer))
        (#_RGBForeColor theColor))
      (#_PenSize (car penSize) (cadr penSize))
      ;; Draw the lines!
      (gs:with-clipped-region trgn
        (when (or (eq (hatchtype me) 'horizontal)
                  (eq (hatchtype me) 'both))
          (setq startVal (+ top (or (startAt me) hatchSpacing)
                            (* hatchSpacing
                               (1- (truncate (- uTop top) hatchSpacing))))
                numLines (1+ (truncate (- ubottom startVal) hatchSpacing)))
          (doTimes (i numLines)
            (#_MoveTo uleft startVal)
            (#_LineTo uright startVal)
            (incf startVal hatchSpacing)))
        (when (or (eq (hatchtype me) 'vertical)
                  (eq (hatchtype me) 'both))
          (setq startVal (+ left (or (startAt me) hatchSpacing)
                            (* hatchSpacing
                               (1- (truncate (- uLeft left) hatchSpacing))))
                numLines (1+ (truncate (- uright startVal) hatchSpacing)))
          (doTimes (i numLines)
            (#_MoveTo startVal utop)
            (#_LineTo startVal ubottom)
            (incf startVal hatchSpacing)))))))

(define-handler translucent (hatch)
  (if (backgroundRenderer me)
    (translucent (backgroundRenderer me))
    nil))

;;; _______________________________ 
;;; Hatches!
;;; _______________________________ 

(new HATCH :objectName "Linedpaper"
     :foreGroundRenderer blue :backgroundRenderer white :spacing 16
     :project sk8)

(let ((strangeBarsForeColor (new RGBColor :foreRed 40000 :foreGreen 40000 :foreBlue 40000
                                 :project sk8)))
  (new HATCH :objectName "Strangebars"
       :foreGroundRenderer strangeBarsForeColor :backgroundRenderer muchdarker
       ;; :penmode 33
       :pensize '(4 4) :spacing 8 :startat 0
       :project sk8))

(new HATCH :objectName "Prisonbars" 
     :foreGroundRenderer black
     :backgroundRenderer Transparent
     :pensize '(16 1) :spacing 32 :startat 0 :hatchtype 'vertical
     :project sk8)

(new HATCH :objectName "Greenlines"
     :foreGroundRenderer green :backgroundRenderer translucent :spacing 2
     :hatchtype 'horizontal
     :project sk8)

(new HATCH :objectName "Gridpaper" 
     :foreGroundRenderer green :backgroundRenderer greenLines :spacing 12
     :hatchtype 'vertical
     :project sk8)


#|
	Change History (most recent last):
	4  	 2/27/97	Hernan  	define-system-handler->define-handler.
						3   4/18/96Hernan  Getting rid of *clip* and *fast-draw*.
|# ;(do not edit past this line!!)
