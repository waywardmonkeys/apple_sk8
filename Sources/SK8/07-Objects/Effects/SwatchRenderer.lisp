;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories


;;;
(in-package :sk8dev)

(provide "SWATCHRENDERER")

(require "IMAGERENDERER")
(require "PICKER" "objects;Pickers:Picker") ;; for draw-text-not-wrapped

;;;Remaining Issues:
;;; - what size should we capture picture at.  should we calculate a nice one
;;;       as a default?
;;; -Should we frame it in some color?
;;; -Should we not scale the object larger than it's original size?

(new imageRenderer :objectname "SwatchRenderer" :project sk8
     :properties '(text size actor))

(define-handler capturePicture (SwatchRenderer otherActor &key size)
  (sk8-multival-bind (hh vv) (size otheractor)
    (if (and size (numberp size))
      (setf size (min size (max hh vv))))
    (if (> hh vv) 
      (setf vv (/ vv hh)
            hh 1)
      (setf hh (/ hh vv)
            vv 1))
    (if (and size (numberp size))
      (call-next-method me otheractor :size (list (ceiling (* size hh)) (ceiling (* size vv))))
      (call-next-method))
    (setf (size me) (list hh vv))
    (setf (text me) (objectstring otherActor :project (project otherActor)))
    (setf (actor me) otherActor)
    ))

(define-handler render (SwatchRenderer theactor region thePaper)
  (rlet ((fi :fontInfo))
    (#_GetFontInfo fi)
    (gs:let+ ((ourrect (gs:hrref region :region.rgnbbox :storage :pointer))
           (ll (rref ourrect :rect.left))
           (tt (rref ourrect :rect.top))
           (rr (rref ourrect :rect.right))
           (bb (rref ourrect :rect.bottom))
           ;; (txt (text me))
           (thestring (if (string= (text theactor) "") 
                        (text me)
                        nil))
           (line-height (if thestring
                          (+ 3 (rref fi :fontInfo.ascent) (rref fi :fontInfo.descent) (rref fi :fontInfo.leading))
                          0))
           (vSpace (- bb tt line-height 6))
           (hSpace (- rr ll 6))
           (pixmap? (inheritsFrom (media me) pixelmap))
           (siz (if pixmap? (size me) '(1 1)))
           (hh (or (car siz) 1))
           (vv (or (cadr siz) 1))
           (hcenter (+ (round hspace 2) ll 3))
           (vcenter (+ (round vspace 2) tt 3))
           (ourrgn (:region))
           (ourrect (:rect))
           )
      (render (backgroundrenderer me) theactor region thePaper)
      (if pixmap?
        (progn
          (if (= hh 1)
            (if (> vspace (* hspace vv))
              (setf hspace hspace
                    vspace (* hspace vv))
              (setf hspace (/ vspace vv)
                    vspace vspace)
              )
            (if (> hspace (* vspace hh))
              (setf vspace vspace
                    hspace (* vspace hh))
              (setf vspace (/ hspace hh)
                    hspace hspace)
              ))
          (rset ourrect :rect.left (- hcenter (truncate hspace 2)))
          (rset ourrect :rect.top (- vcenter (truncate vspace 2)))
          (rset ourrect :rect.right (+ hcenter (ceiling hspace 2)))
          (rset ourrect :rect.bottom (+ vcenter (ceiling vspace 2))))
        (progn
          (rset ourrect :rect.left (+ ll 2))
          (rset ourrect :rect.top (+ tt 2))
          (rset ourrect :rect.right (- rr 2))
          (rset ourrect :rect.bottom (- bb line-height 2)))
        )
      (#_rectrgn ourrgn ourrect)
      (multiple-value-bind (val1 err?) 
                           (ignore-errors 
                            (if (media me)
                              (if pixmap?
                                (renderStretched (media me) me theActor ourrgn thePaper)
                                (render (media me) theactor ourrgn thePaper))
                              (render (backgroundrenderer me) theactor ourrgn thePaper)))
        (declare (ignore val1))
        (if err? (render-an-error ourrgn)))
      (when thestring
        (rset ourrect :rect.left (+ ll 1))
        (rset ourrect :rect.top (- bb line-height 1))
        (rset ourrect :rect.right (- rr 1)) ;;hhmmmmm
        (rset ourrect :rect.bottom bb)
        (gs:set-text-state theactor)
        (with-rgb (rgb (mcl-color (textColor theactor)))
          (#_RGBForeColor rgb))
        (with-rgb (rgb (mcl-color (backgroundrenderer me)))
          (#_RGBBackColor rgb))
        (draw-text-not-wrapped thestring ourrect)
        (gs:restore-text-state))   ;;; This shouldn't be necessary, but I'm desperate to find the font problem...
      )
    ))

(define-handler (setf media) (theMedia SwatchRenderer)
  (sk8::setValue 'media me theMedia)
  )

(define-handler media (SwatchRenderer)
  (sk8::getValue 'media me)
  )

#|
(new SwatchRenderer :objectname "test1" :project sk8)
(setf (backgroundrenderer test1) ui::uidarktexture)
(setf (text test1) nil)
(setf (text test1) "hello")
(setf (fillcolor foo::bri) test1)
(setf (textsize foo::bri) 9)
(setf (textfont foo::bri) geneva)
(setf (textcolor foo::bri) white)

(capturepicture test1 ui::messagebox)
(capturepicture test1 ui::rectangle)
(capturepicture test1 ui::propertycontrolpanel)
(capturepicture test1 ui::standarddrawpalette)
|#
#|
	Change History (most recent last):
	1	1/24/94	rod	
	2	1/24/94	rod	
	3	1/24/94	rod	
	4	1/31/94	rod	
	6	2/28/94	rod	The Final Renaming for Alpha! (yeah, right...)
	7	3/4/94	kleiman	addproperty avoided wherever possible
	8	3/21/94	kleiman	setvalue -> sk8::setValue
	9	3/27/94	Brian	making default swatch size scale if a single 
				number is passed to capturePicture with size.
	10	3/28/94	rod	Making sure size is always at least 1
	11	3/29/94	rod	Trying to restore the pen to get rid of our font
				problem...
	12	5/4/94	Hernan	Calls renderStretched instead of other obsolete function.
	13	5/6/94	rod	
	14	7/15/94	Hernan	Render method of swatch renderer no longer
				passes a rect to renderStretched.
	15	7/21/94	rod	Changing how renderers are shown in swatches.
	16 	10/ 3/94	Hernan  	Adding the paper argument to render. (The paper
							is the Port at this point).
	17 	 2/ 3/95	Hernan  	get-record-field -> rref or hrref.
	18 	 3/20/95	Hernan  	In render of swatchRenderer pixmap? should be
							set to whether the object inheritsFrom pixmap.
	19 	 4/11/95	rod     	Dealing with failed snapshots.
	2  	 2/27/97	Hernan  	
|# ;(do not edit past this line!!)
