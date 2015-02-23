(in-package :SK8Development)

(provide "MASKEDACTOR")

(require "ACTOR")

;; (setcurrentProject sk8)

;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories



#| Modification History.

01-22-93 ruben id -> objectName
11-13-92 ruben icon -> PixMapActor
09-20-92 hernan changing this to work with MF VI

|#

(new actor :objectname "MaskedActor" :undisposable t :prototype t :project sk8
     :properties '((media :inherit) (translucent :inherit :value t)))

(setf (private MaskedActor) nil) ; PUBLIC API

;;; The optimization is turned off to deal with icons in which the mask
;;; does not cover the whole icon.  (Radar#: 1191266)

(gs:offsetsRegions! (gs:node-flags MaskedActor) 0)

(setFrameSize MaskedActor 0 0)

(define-handler setFrameSize (MaskedActor h v &key physical)
  (declare (ignore h v physical))
  (sk8-error GeneralProgrammaticError
             :strings '("Masked actors have no frame.")))

(defun colorMaskedActorDraw (theactor thePort draw-region)
  (declare (ignore-if-unused thePort draw-region))
  (gs:let+ ((theMaskedActor (media theactor))
            (qd-rect (:rect :SK8Rect (gs:recompute-physicalBoundsRect theActor)))
            (handle (if theMaskedActor
                      (loadMedia theMaskedActor))))
    (gs:recompute-fill-Region theActor (gs:node-flags theActor))
    (unless (translucent theactor)
      (render (gs:node-fillColor theActor) theActor (gs:node-fillRegion theActor) thePort))
    (if (handlep handle)
      (gs:with-composed-clip (gs:node-fillRegion theActor)
        (#_PlotCIcon qd-rect handle))
      ;; If the masked actor has no media, render an error in its bounds.
      (render-an-error (gs:node-boundsRegion theActor)))
    (when (gs:hilited? (gs:node-flags theActor))
      (#_InvertRect qd-rect))))

(define-handler (setf media) (theResource MaskedActor)
  (if (inheritsFrom theResource iconRSRC)
    (sk8-multival-bind (width height) (size theResource)
      (setvalue 'media me theResource)
      (setSize me width height :physical t)
      (unless (gs:hasWindow? (gs:node-flags me))
        (gs:making-self-dirty (me t t t t (gs:node-boundsRegion me) :bounds t)
          )))
    (sk8-error PropertyTypeMismatchError
               :object        theResource
               :expectedType  IconRSRC
               :ownerObject   MaskedActor
               :propertyName 'media
               )))

(define-handler (setf translucent) (value MaskedActor)
  (gs:making-self-dirty (me t t t t (gs:node-boundsRegion me) :bounds t)
    (setvalue 'translucent me  value)))

(setf (gs:node-drawFunction MaskedActor) 'colorMaskedActorDraw)

(define-handler makeFillRegion (MaskedActor)
  (let ((flags (gs:node-flags me)))
    (gs:recompute-bounds-region me flags)
    (#_CopyRgn (gs:node-boundsRegion me) (gs:node-fillRegion me))
    (gs:fillDirty! (gs:node-flags me) 0)))

(define-handler makeBoundsRegion (MaskedActor)
  (gs:let+ ((qd-rect (:rect :SK8Rect (gs:recompute-physicalBoundsRect me)))
            (theMedia (media me))
            (dummyRect (:rect))
            (handle (if theMedia (loadMedia theMedia)))
            bMap theMask)
    (if (and (handlep handle) (translucent me))
      (unwind-protect
        (progn
          (setq bmap (gs:make-bitmap qd-rect))
          (sk8-multival-bind (width height) (size theMedia)
            (#_SetRect dummyRect -18000 -18000 (+ -18000 width) (+ -18000 height)))
          (with-port (gs:get-wmgr-port)
            (#_PlotCIcon dummyRect Handle)
            (#_HLock Handle)
            (setq themask (gs:hrref handle :cicon.iconmask :storage :pointer))
            (gs:copy-bits themask
                                bMap
                                (rref themask :bitmap.bounds)
                                qd-rect))
          (#_BitMapToRegion (gs:node-boundsRegion me) bmap))
        (#_hUnLock handle)
        (#_disposPtr bmap))
      (#_RectRgn (gs:node-boundsRegion me) qd-rect))
    (gs:boundsDirty! (gs:node-flags me) 0)))

;; COLOR MaskedActorS HAVE NO FRAME!!

(define-handler makeFrameRegion (MaskedActor)
  ;; Not required since the region will be empty when created.
  ;; (#_SetEmptyRgn (gs:node-frameRegion me))
  (gs:frameDirty! (gs:node-flags me) 0))

(setSize maskedActor 32 32 :physical t)

;;; _______________________________ 
;;; Changing parents.
;;; _______________________________ 

(define-handler AddedMeAsParent (MaskedActor child oldParent)
  (declare (ignore-if-unused child oldparent))
  ;; Make sure it becomes an actor.
  (call-next-method)
  ;; Make sure the offsetsRegion optimization is turned off.
  (gs:offsetsRegions! (gs:node-flags child) 0))

(define-handler removingMeAsParent (MaskedActor child newParents)
  (declare (ignore-if-unused child newParents))
  ;; Make sure the offsetsRegion optimization is turned on (default for actors).
  (gs:offsetsRegions! (gs:node-flags child) 1)
  ;; Make sure it becomes an actor.
  (call-next-method))



#|
	Change History (most recent last):
	2	6/15/93	Hernan	Got rid bizarre stuff from the good old days (such
				as a frameColor that renders over the icon in the
				fillRegion (???)).
	3	6/15/93	Hernan	Second try to check in...
	4	6/25/93	Hernan	The Great Renaming of 93.
	10	10/1/93	hernan	=====================================
				= Sad but true: the node is gone!!! =
				=====================================
	11	10/5/93	hernan	with-fast-slots -> with-fast-node-slots.
	12	1/11/94	hernan	self -> me
	13	2/21/94	hernan	The Final Renaming for Alpha! (yeah, right...)
	14	2/28/94	hernan	Now using GCable stuff.
	15	3/3/94	kleiman	properties -> addproperty
	16	3/3/94	kleiman	private properties declared via makeprivateproperty
	17	4/5/94	kleiman	approved for public API
	18	4/28/94	Hernan	Disposing the bitmap after we get the mask in
				makeFillRegion. And more! Made set media 
				check that the media be set to an iconRSRC and
				to adjust the size of the actor to the size of the 
				resource.
	19	6/10/94	Hernan	Modified the draw method to render an error 
				when the masked actor has no media.
	20 	 9/23/94	rod     	Fixing bug with setf media.  It was hardcoded to
							set the inheritable property as a list of two values
							rather than using SetValue.
	21 	 9/23/94	rod     	Yet another hardcoded slot set, in translucent.
	22 	10/ 3/94	Hernan  	machandle -> loadMedia.
	23 	10/ 7/94	Hernan  	1191266: turned off the region optimization for maskedActors.
	24 	12/ 7/94	Hernan  	Replacing error by sk8-error where possible.
	25 	12/19/94	Hernan  	Setting the media should refresh the window's
							region when the masked actor is a window.
	26 	 1/24/95	Hernan  	Using with-fast-node-slots where appropriate.
	27 	 2/ 3/95	Hernan  	get-record-field -> rref or hrref.
	28 	 3/16/95	till    	trap wrappers for hlock, hunlock in makeBoundsRegion
							                                          should probably use with-lock, but I can't figure
							                                          out how right now.
	29 	 3/17/95	till    	damn, I was completely wrong about the hlock stuff
	3  	 2/27/97	Hernan  	define-system-handler->define-handler.
|# ;(do not edit past this line!!)
