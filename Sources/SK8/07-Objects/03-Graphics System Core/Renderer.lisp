(in-package :SK8Development)

;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories



(provide "RENDERER")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; SK8 COLOR SYSTEM
;;;   Routines for creating & manipulating SK8Colors

;;;
;;; Colors would be objects if Macintoshes were Crays...
;;;  BUT NOW, THEY ARE!!!

#| Modification History

02-22-93 ruben inheritsFrom? -> inheritsFrom
01-22-93 ruben id -> objectName
11-13-92 hernan sk8Color becomes renderer. This should load after media!!!
07-29-92 ice   converted to use SK8Color objects instead of those stupid structs.
07-26-92 ruben moved exports to exports.lisp
05-30-92 ruben netwalk -> inheritsFrom?
05-12-92 ruben d29 conversion; SK8-COLOR property renamed COLOR (conflicted with sk8-color function)
01-19-92 ruben converted to macframes II
10-02-91 adam  introduced the *sk8-colors* list of SK8COLORINFO structs describing all currently defined sk8-colors,
               the GET-SK8-COLORS function for examining the list, and the DEFINE-SK8-COLOR macro for defining new sk8-colors.
08-22-91 ruben load patches
08-21-91 ruben created color file

|#

;;; _______________________________________________________________
;;; RENDERING UTILITIES -- check for memory error and call render-an-error if things
;;;                    go wrong.

;;; The Memory Error Renderer... From now on, gray-pattern means
;;;                        memory error!!!

(defun real-render-an-error (region)
  (#_PenPat *gray-pattern*)
  (#_PaintRgn region))

;;; OJO: redefined in the renderers file when the errorColor is created!!!

(defun render-an-error (region)
  (if (and (boundp 'ErrorColorRSRC) (mf::object-class-p ErrorColorRSRC))
    (render-icon-media ErrorColorRSRC region)
    (real-render-an-error region)))

(define-sk8-function renderAnError nil (theRenderer theActor region)
  (declare (ignore theRenderer theActor))
  (render-an-error region))

(define-sk8-function renderAMediaError nil (theMedia theRenderer theActor region)
  (declare (ignore theMedia theRenderer theActor))
  (render-an-error region))

;;____________________________________________________________________
;; Renderer

(new Object :objectname "Effect" :project sk8 :undisposable t)

(new effect 
     :objectName "Renderer"
     :undisposable t
     :prototype t
     :project sk8)

(define-handler localVirtualProperties (Renderer)
  (when (eq me renderer) '(dynamic)))

(define-handler render (Renderer theActor region thePaper)
  (declare (ignore theActor region thePaper)))

(define-handler stageRelative (Renderer)
  nil)

;;; Default returns black!

(define-handler MCL-color :private (Renderer)
  0)

(define-handler translucent (renderer)
  nil)

;;____________________________________________________________________
;; TRANSPARENT RENDERER

(new Renderer 
     :objectName "Transparent" 
     :project sk8)

(define-handler translucent (transparent)
  t)

(define-handler render (Transparent theActor region thePaper)
  (declare (ignore theActor region thePaper))
  )

;;____________________________________________________________________
;; RGBColor

(new Renderer
     :objectName "RGBColor"
     :undisposable t
     :prototype t
     :properties '((foreRed :value 0)
                   (foreGreen :value 0)
                   (foreBlue :value 0))
     :project sk8)

(define-handler (setf foreRed) (newValue RGBColor)
  (setf (slot-value me 'foreRed) (gs:f.round newValue)))

(define-handler (setf foreGreen) (newValue RGBColor)
  (setf (slot-value me 'foreGreen) (gs:f.round newValue)))

(define-handler (setf foreBlue) (newValue RGBColor)
  (setf (slot-value me 'foreBlue) (gs:f.round newValue)))

(define-handler render (RGBColor theActor region thePaper)
  (declare (ignore theActor thePaper))
  (rlet ((foreColor :rgbColor :red (foreRed me)
                    :green (foreGreen me)
                    :blue (foreBlue me)))
    (#_RGBForeColor foreColor)
    (#_PaintRgn region)
    (#_ForeColor #$BlackColor)))

(define-handler MCL-color :private (RGBColor)
  (make-color (foreRed me)
              (foreGreen me)
              (foreBlue me)))

(new RGBColor :objectName "White" :foreRed 65535 :foreGreen 65535 :foreBlue 65535 :project sk8)
(new RGBColor :objectName "Black" :foreRed 0 :foreGreen 0 :foreBlue 0 :project sk8)


;;;;-----------------------------------------------------------------------

(new Renderer
     :objectName "Highlighted"
     :undisposable t
     :prototype t
     :project sk8)

(define-handler render (Highlighted theActor region thePaper)
  (declare (ignore theActor thePaper))
  (gs:with-clipped-region region
    (#_PenMode gs:$hilite)
    (#_PaintRgn region)
    (#_PenMode #$srcCopy)
    (#_PenNormal)
    ))

(define-handler translucent (Highlighted)
  t)

;;; Does it need the DynamicActorMixin
(define-handler dynamic :private (Renderer)
  nil)

;;; This call is the Renderer's opportunity to decide whether the given renderer should be used
;;; or whether a child should be substituted for it.  Whichever is decided, it is returned.

(define-handler setFillColorActor (Renderer theActor)
  (declare (ignore theActor))
  me)

;;; This call is the Renderer's opportunity to decide whether the given renderer should be used
;;; or whether a child should be substituted for it.  Whichever is decided, it is returned.

(define-handler setFrameColorActor (Renderer theActor)
  (declare (ignore theActor))
  me)

;;; This call is the Renderer's opportunity to decide whether the given renderer should be used
;;; or whether a child should be substituted for it.  Whichever is decided, it is returned.

(define-handler setTextColorActor (Renderer theActor)
  (declare (ignore theActor))
  me)


#|
	Change History (most recent last):
	8	9/20/93	hernan	Got rid of obsoleted functions...
	9	10/1/93	hernan	=====================================
				= Sad but true: the node is gone!!! =
				=====================================
	10	10/13/93	hernan	The renderer hierarchy has been revisited!
	11	10/15/93	hernan	Changing render-an-error.
	12	10/15/93	hernan	Making sure the bevelRenderer prototype can render.
	13	10/15/93	hernan	Making black and white the colors of the bevelRenderer.
	13	10/15/93	hernan	Second try!
	14	10/20/93	hernan	Making a new default render function for image'
				renderers that just renders an error.
	15	12/1/93	hernan	Removing unused variables.
	16	12/1/93	hernan	Making hach work.
	17	12/17/93	rod	Added a media property to complexRGBColor to
				specify BWPatterns to use as a penpattern.
	18	12/21/93	sidney	Changes so files can be compiled
	19	12/22/93	rod	backRenderer->BackgroundRenderer
				Obsoleted ComplexPatternRenderer
				backgroundColor->BackgroundRenderer
	20	12/27/93	rod	
	21	12/27/93	rod	
	22	1/11/94	hernan	self -> me
	23	1/14/94	hernan	Cannot remember.
	24	2/21/94	hernan	The Final Renaming for Alpha! (yeah, right...)
	25	2/25/94	hernan	Using symbols instead of keywords for options!!!
	26	3/6/94	rod	
	26	3/6/94	rod	
	27	3/21/94	kleiman	setvalue -> sk8::setValue
	28	4/12/94	rod	Fixed highlighted renderer
	29	4/13/94	Hernan	SketchRenderer does not clip to the right region.
	30	4/20/94	Hernan	Replacing calls to get-record-field with calls to
				region-into-rect (which does not cons).
	31	5/2/94	Hernan	render-an-imageRenderer-error -> renderAnError.
	32	5/2/94	Hernan	Adding renderAMediaError.
	33	7/15/94	dy	setfillColorActor (etc.) of Renderer
	34	7/18/94	dy	I guess dynamic should be a private localproperty
	35 	 8/12/94	Hernan  	SketchRenderer needs to clip to the region it is
							given to avoid lines going all over the place.
	36 	 8/23/94	Hernan  	1180500: removing stupid quote.
	37 	 8/31/94	Hernan  	Penmodes are now stored as symbols.
	38 	 9/16/94	Hernan  	Render of complexRGBColor now clips.
	39 	 9/19/94	dy      	introduce "sharable of Renderer" virtual property
	40 	 9/19/94	dy      	remove "sharable of Renderer" virtual property
	41 	10/ 3/94	Hernan  	machandle -> loadMedia.
	42 	10/17/94	dy      	setFillColorActor etc. introduced instead of set fillColorActor
	43 	11/28/94	dy      	MacSystemRGBColor stuff, commented out
	44 	12/ 7/94	Hernan  	Replacing error with sk8-error where possible.
	45 	 1/25/95	dy      	Put (#_ForeColor #$BlackColor) at the end of render of RGBColor
	2  	 8/17/95	sidney  	add check because of change in build procedure
	2  	 4/10/96	Hernan  	define-system-handler->define-handler.
	3  	 4/19/96	Hernan  	Putting define-sk8-function back in the code.
	4  	 5/13/96	Brian   	Fixing LocalVirtualProperties
	5  	 2/27/97	Hernan  	
|# ;(do not edit past this line!!)
