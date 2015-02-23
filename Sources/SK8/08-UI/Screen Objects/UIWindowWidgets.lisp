;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories


;;;

(in-package :UIDevelopment)

(SK8-declare-syms :UI :public ; Updated 12-07-95   1:30 pm
                  UI::ARROWTODRAW UI::FRAMESCORE UI::NONCROSSAUTHORCOLOR
                  UI::PROJECTBUTTONIN UI::QUERYSTRING UI::UIBIGSPLITTERCOLOR UI::UIBIGSPLITTERCOLOR1
                  UI::UIBIGSPLITTERCOLOR2 UI::UIBIGSPLITTERCOLOR3 UI::UIHOLPICKER UI::UILIGHTESTGRAY
                  UI::UILITTLESPLITTER UI::UILITTLESPLITTERCOLOR UI::UILITTLESPLITTERCOLOR1 UI::UILITTLESPLITTERCOLOR2
                  UI::UILITTLESPLITTERCOLOR3 UI::UIMENUOUT UI::UIMENURENDERER UI::UISPLITTER UI::UITEXTLISTPICKER)



;;; Functions_________________________________________

(defun okProjects (proj)
  (if proj
    (cons proj (requiredprojects proj))
    nil))


;; ui versions of common interface elements


;;;;;;;;;;;;;;;---------------------------------------------------------------------------------------------------
;;;;;;;;;;;;;;;---------------------------------------------------------------------------------------------------
;;;;;;;;;;;;;;;  the Renderers and Draw Functions....
;;;;;;;;;;;;;;;---------------------------------------------------------------------------------------------------
;;;;;;;;;;;;;;;---------------------------------------------------------------------------------------------------

(defmacro UIFrameIt (color ll tt rr bb)
  `(gs:let+ ((qd-rect (:rect)))
     (with-rgb (rgb ,color)
       (#_RGBForeColor rgb))
     (rset qd-rect :rect.left ,ll)
     (rset qd-rect :rect.top ,tt)
     (rset qd-rect :rect.right ,rr)
     (rset qd-rect :rect.bottom ,bb)
     (#_frameRect qd-rect)
     )
  )

(defmacro UIPaintIt (color ll tt rr bb)
  `(gs:let+ ((qd-rect (:rect))
          (rgn (:region)))
     (with-rgb (rgb ,color)
       (#_RGBForeColor rgb))
     (rset qd-rect :rect.left ,ll)
     (rset qd-rect :rect.top ,tt)
     (rset qd-rect :rect.right ,rr)
     (rset qd-rect :rect.bottom ,bb)
     (#_RectRgn rgn qd-rect)
     (#_PaintRgn rgn)
     )
  )

(defmacro UILDraw (color hh1 vv1 hh2 vv2 hh3 vv3)
  `(gs:let+ ()
     (with-rgb (rgb ,color)
       (#_RGBForeColor rgb))
     (#_MoveTo ,hh1 ,vv1)
     (#_LineTo ,hh2 ,vv2)
     (#_LineTo ,hh3 ,vv3)
     ))

(defmacro UIArrowDraw (color left right offset top)
  `(gs:let+ ((xx (- ,right ,left)))
     (setf xx (min xx 13))
     (decf xx 1)
     (setf xx (- ,right xx))
     (decf xx ,offset)
     (with-rgb (rgb ,color)
       (#_RGBForeColor rgb))
     (#_MoveTo (+ xx 0) (+ ,top 0) )
     (#_LineTo (+ xx 8) (+ ,top 0) )
     (#_MoveTo (+ xx 1) (+ ,top 1) )
     (#_LineTo (+ xx 7) (+ ,top 1) )
     (#_MoveTo (+ xx 2) (+ ,top 2) )
     (#_LineTo (+ xx 6) (+ ,top 2) )
     (#_MoveTo (+ xx 3) (+ ,top 3) )
     (#_LineTo (+ xx 5) (+ ,top 3) )
     (#_MoveTo (+ xx 4) (+ ,top 4) )
     (#_LineTo (+ xx 4) (+ ,top 4) )
     ))


;;;;---------------------------------------------------------------------------------------------------------------------------------

(new rgbcolor :objectname "uilightestgray" :project ui)
(setf (forered uilightestgray) 61166)
(setf (foreblue uilightestgray) 61166)
(setf (foregreen uilightestgray) 61166)

;;;;---------------------------------------------------------------------------------------------------------------------------------

;;
(new uirenderer :objectname "FrameScore" :project ui)

(define-handler render (FrameScore theActor region thePaper)
  (declare (ignore region))
  (gs:recompute-frame-region theActor (gs:node-flags theactor))
  (gs:let+ (frame-rect-left 
         frame-rect-top
         frame-rect-right
         frame-rect-bottom
         (frame-rgn (gs:node-frameregion theActor))
         (outerwhite (make-color 61166 61166 61166))
         (edges (make-color 48059 48059 48059))
         (darkrim (make-color 30583 30583 30583))
         (frame-rect (gs:hrref frame-rgn :region.rgnbbox :storage :pointer))
         )
    (setq frame-rect-left (rref frame-rect :rect.left))
    (setq frame-rect-top (rref frame-rect :rect.top))
    (setq frame-rect-right (rref frame-rect :rect.right))
    (if (< frame-rect-left 0) (decf frame-rect-right frame-rect-left))
    (setq frame-rect-bottom (rref frame-rect :rect.bottom))
    (if (< frame-rect-top 0) (decf frame-rect-bottom frame-rect-top))
    
    (render uimiddle theActor frame-rgn thePaper)
    (#_penNormal)
    (#_penSize 1 1)
    
    
    (uiframeit edges (+ frame-rect-left 0) (+ frame-rect-top 0) (- frame-rect-right 0) (- frame-rect-bottom 0))
    (uiframeit darkrim (+ frame-rect-left 0) (+ frame-rect-top 0) (- frame-rect-right 1) (- frame-rect-bottom 1))
    (uiframeit outerwhite (+ frame-rect-left 1) (+ frame-rect-top 1) (- frame-rect-right 0) (- frame-rect-bottom 0))
    (#_PenNormal)))

(new uirenderer :objectname "SimpleFrame" :project ui)

(define-handler render (SimpleFrame theActor region thePaper)
  (declare (ignore region thePaper))
  (gs:recompute-frame-region theActor (gs:node-flags theactor))
  (gs:let+ (frame-rect-left 
         frame-rect-top
         frame-rect-right
         frame-rect-bottom
         (frame-rgn (gs:node-fillregion theActor))
         (edges (make-color 0 0 0))
         (frame-rect (gs:hrref frame-rgn :region.rgnbbox :storage :pointer))
         )
    (setq frame-rect-left (rref frame-rect :rect.left))
    (setq frame-rect-top (rref frame-rect :rect.top))
    (setq frame-rect-right (rref frame-rect :rect.right))
    (if (< frame-rect-left 0) (decf frame-rect-right frame-rect-left))
    (setq frame-rect-bottom (rref frame-rect :rect.bottom))
    (if (< frame-rect-top 0) (decf frame-rect-bottom frame-rect-top))
    (uiframeit edges frame-rect-left frame-rect-top frame-rect-right frame-rect-bottom)
    (#_PenNormal)))

;;;;;;;;;;;;;;;---------------------------------------------------------------------------------------------------
;;;;;;;;;;;;;;;Table Picker stuff...
;;;;;;;;;;;;;;;---------------------------------------------------------------------------------------------------

(defun ScoredFastTablePicker-draw (theTablePicker thePort draw-rgn)    
  (gs:let+ ((flags (gs:node-flags theTablePicker))
            (TableHOffset (TableHOffset theTablePicker))
            (TableVOffset (TableVOffset theTablePicker))
            (columnWidths (columnWidths theTablePicker))
            (rowHeights (rowHeights theTablePicker))
            (columnSpacing (columnSpacing theTablePicker))
            (rowSpacing (rowSpacing theTablePicker))
            (HalfColSpace (round columnSpacing 2))
            (HalfRowSpace (round rowSpacing 2))
            (HighlightSelection (HighlightSelection theTablePicker))
            (cols (columns theTablePicker))
            (rows (rows theTablePicker))
            (startCol (max 0 (round (horizontalScroll theTablePicker))))
            (startRow (max 0 (round (verticalScroll theTablePicker))))
            (theSelection (selectionArray theTablePicker))
            (theImages (imageArray theTablePicker))
            (theTextStyles (textstyles theTablePicker))
            (thetextcolors (textcolors theTablePicker))
            (qd-rect (:rect))
            fill-rect-left fill-rect-top fill-rect-right fill-rect-bottom
            theImage
            (DirtyRect (gs:hrref draw-rgn :region.rgnbbox :storage :pointer))
            (fill-rgn (gs:node-fillRegion theTablePicker))
            (box-rgn (:region))
            (clip-rgn (:region))
            default-text-style
            (default-text-color (sk8dev::mcl-color (textcolor theTablePicker)))
            (default-text-color2 (sk8dev::mcl-color white))
            (default-line-color   (if *MacStyleInterface*  
                                     (make-color 0 0 0)
                                     (make-color 48059 48059 48059)))
            (fill-rect (gs:hrref fill-rgn :region.rgnbbox :storage :pointer))
            (theItems (items theTablePicker))
            (plist (gs:node-properties theTablePicker))
            (hLineSize (rowLinesSize theTablePicker))
            (vLineSize (columnLinesSize theTablePicker)))
    (#_TextFont (fontData (getf plist :text-font ChicagoFont)))
    (setf default-text-style (getf plist :text-style 0))
    (#_TextFace default-text-style)
    (#_TextSize (getf plist :text-size 0))
    (gs:with-clipped-region draw-rgn
      ;; Render the fill and the frame
      (gs:recompute-fill-region theTablePicker flags)
      (gs:recompute-frame-region theTablePicker flags)
      (render (gs:node-fillcolor theTablePicker) theTablePicker draw-rgn thePort) ;;fill-rgn)
      (render (gs:node-frameColor theTablePicker) theTablePicker (gs:node-frameRegion theTablePicker) thePort)
      (with-rgb (rgb default-text-color)
        (#_RGBForeColor rgb))
      (with-rgb (rgb (sk8dev::mcl-color (gs:node-fillcolor theTablePicker)))
        (#_RGBBackColor rgb))
      
      (setq fill-rect-left (rref fill-rect :rect.left))                       ;;(gs:f.round hOffset)))
      (setq fill-rect-top (rref fill-rect :rect.top))                         ;;(gs:f.round vOffset)))
      (setq fill-rect-right (rref fill-rect :rect.right))                     ;;(gs:f.round hOffset)))
      (if (< fill-rect-left 0) (decf fill-rect-right fill-rect-left))
      (setq fill-rect-bottom (rref fill-rect :rect.bottom))                   ;;(gs:f.round vOffset)))
      (if (< fill-rect-top 0) (decf fill-rect-bottom fill-rect-top))
      
      (#_PenNormal)
      
      (when theItems
        (incf fill-rect-top TableVOffset)
        (incf fill-rect-left TableHOffset)
        
        ;; Adjust the bounds
        (let (tmp)
          (setq tmp (+ fill-rect-top (* (- rows startrow) rowspacing) (apply #'+ (nthcdr startRow rowheights))))
          (when (> fill-rect-bottom tmp)
            (setq fill-rect-bottom tmp))
          (setq tmp (+ fill-rect-left (* (- cols startcol) columnspacing) (apply #'+ (nthcdr startCol columnwidths))))
          (when (> fill-rect-right tmp)
            (setq fill-rect-right tmp)))
        
        ;; Draw the top left edge
        (#_sectRgn fill-rgn draw-rgn clip-rgn)
        (unless (and (eql 0 hLineSize) (eql 0 vLineSize))
          (#_PenNormal)
          (#_SetClip clip-rgn)
          (when default-line-color
            (with-rgb (rgb default-line-color)
              (#_RGBForeColor rgb))
            
            (do ((cur-Row startRow (1+ cur-Row))
                 (top fill-rect-top bottom)
                 (curhh (nthcdr startRow rowheights) (cdr curhh))
                 (bottom fill-rect-top bottom))
                ((or (> cur-row rows) 
                     (>= bottom fill-rect-bottom)))
              (setf bottom (+ top rowspacing (car curhh)))
              (when (>= cur-Row 0)
                (#_MoveTo (1- fill-rect-left) bottom)
                (#_LineTo fill-rect-right bottom))
              )
            (do ((cur-Col startCol (1+ cur-Col))
                 (left fill-rect-left right)
                 (curvv (nthcdr startcol columnwidths) (cdr curvv))
                 (right fill-rect-left right))
                ((or (> cur-col cols) (>= right fill-rect-right)))
              (setf right (+ left columnspacing (car curvv)))
              (when (>= cur-Col 0)
                (#_MoveTo right (1- fill-rect-top))
                (#_LineTo right fill-rect-bottom))
              )
            (unless *MacStyleInterface*  
              
              (with-rgb (rgb default-text-color2)
                (#_RGBForeColor rgb))
              (do ((cur-Row startRow (1+ cur-Row))
                   (top fill-rect-top bottom)
                   (curhh (nthcdr startRow rowheights) (cdr curhh))
                   (bottom fill-rect-top bottom))
                  ((or (> cur-row rows) 
                       (>= bottom fill-rect-bottom)))
                (setf bottom (+ top rowspacing (car curhh)))
                (when (>= cur-Row 0)
                  (#_MoveTo fill-rect-left (1+ bottom))
                  (#_LineTo fill-rect-right (1+ bottom)))
                )
              (do ((cur-Col startCol (1+ cur-Col))
                   (left fill-rect-left right)
                   (curvv (nthcdr startcol columnwidths) (cdr curvv))
                   (right fill-rect-left right))
                  ((or (> cur-col cols) (>= right fill-rect-right)))
                (setf right (+ left columnspacing (car curvv)))
                (when (>= cur-Col 0)
                  (#_MoveTo (1+ right) fill-rect-top)
                  (#_LineTo (1+ right) fill-rect-bottom))
                )))
          
          (#_PenSize :long #@(1 1)))
        (#_penNormal)
        ;; Draw the cells and the lines.
        (do ((cur-Row startRow (1+ cur-Row))
             (top fill-rect-top bottom)
             (curhh (nthcdr startrow rowheights) (cdr curhh))
             (bottom 0 bottom))
            ((or (>= cur-row rows) (>= top fill-rect-bottom)))
          (setf bottom (+ top RowSpacing (car curhh)))
          (when (>= cur-Row 0)
            (gs:with-hilitecolor theport (highlightcolor theTablePicker)
              (do ((cur-Col startCol (1+ cur-Col))
                   (left fill-rect-left right)
                   (curvv (nthcdr startCol columnwidths) (cdr curvv))
                   (right 0 right))
                  ((or (>= cur-col cols) (>= left fill-rect-right)))
                (setf right (+ left columnspacing (car curvv)))
                (when (>= cur-Col 0)
                  (setf theImage (aref theImages cur-Col cur-Row ))
                  ;; draw-text-not-wrapped only cares about left and top
                  (when theImage
                    (if (stringp theImage)
                      (progn
                        (rset qd-rect :rect.left (+ left halfColSpace))
                        (rset qd-rect :rect.top (+ top halfRowSpace))
                        (rset qd-rect :rect.right (- right halfColSpace))
                        (rset qd-rect :rect.bottom (- bottom halfRowSpace))
                        (when (gs:rects-intersect DirtyRect qd-rect)
                          (#_RectRgn box-rgn qd-rect)
                          (#_SectRgn clip-rgn box-rgn box-rgn)
                          (#_SetClip box-rgn)
                          (if (and thetextstyles (aref thetextstyles cur-Col cur-Row)) 
                            (#_TextFace (gs:get-style-number (aref thetextstyles cur-Col cur-Row)))
                            (#_TextFace default-text-style))
                          (if (and thetextcolors (aref thetextcolors cur-Col cur-Row)) 
                            (with-rgb (rgb (sk8dev::mcl-color (aref thetextcolors cur-Col cur-Row)))
                              (#_RGBForeColor rgb))
                            (with-rgb (rgb default-text-color)
                              (#_RGBForeColor rgb)))
                          (rset qd-rect :rect.right (+ (rref qd-rect :rect.right) 3))
                          (sk8dev::draw-text-not-wrapped theimage qd-rect)
                          ))
                      (progn
                        (rset qd-rect :rect.left (+ left hLineSize))
                        (rset qd-rect :rect.top (+ top vLineSize))
                        (rset qd-rect :rect.right (- right 0))
                        (rset qd-rect :rect.bottom (- bottom 0))
                        (when (gs:rects-intersect DirtyRect qd-rect)
                          (#_RectRgn box-rgn qd-rect)
                          (#_SectRgn clip-rgn box-rgn box-rgn)
                          (#_SetClip box-rgn)
                          (render theImage theTablePicker box-rgn thePort)
                          (with-rgb (rgb (sk8dev::mcl-color (textColor theTablePicker)))
                            (#_RGBForeColor rgb))
                          (with-rgb (rgb (sk8dev::mcl-color (gs:node-fillcolor theTablePicker)))
                            (#_RGBBackColor rgb))
                          (#_PenNormal))))
                    (when (aref theSelection cur-Col cur-Row)
                      (rset qd-rect :rect.right (1+ right))
                      (rset qd-rect :rect.bottom (1+ bottom))
                      (rset qd-rect :rect.left left)
                      (rset qd-rect :rect.top top)
                      (#_RectRgn box-rgn qd-rect)
                      (#_SectRgn clip-rgn box-rgn box-rgn)
                      (#_SetClip draw-rgn)
                      (when HighlightSelection
                        (sk8dev::Set-Pen-For-Hilite)
                        (#_PaintRect qd-rect)
                        (#_PenMode #$srcCopy)
                        (#_PenSize 2 2))
                      ))
                  )))))
        (#_PenNormal)
        ;; Draw the contents if any!
        (unless (gs:dlist-empty? (gs:node-contents theTablePicker))
          (gs:with-clipped-region clip-rgn
            (funcall (gs:node-contentsDrawFunction theTablePicker) theTablePicker thePort clip-rgn fill-rgn t)))
        ;; invert if necessary.
        (when (and (gs:hilited? flags) (gs:inverted? flags))
          (#_InvertRect fill-rect))))))

;;;;;;;;;;;;;;;-_______________________________________________________________________________________________________________________
;;;;;;;;;;;;;;;-_______________________________________________________________________________________________________________________
;;;;;;;;;;;;;;; Draw function for pickers....

(define-handler LineHeight :private (Picker)
                (sk8dev::get-lineHeight me))
(define-handler LineHeight :private (IconTextPicker)
                (- (sk8dev::calc-IconTextPicker-LineHeight me) (linespacing me)))

(define-handler draw-function-to-use :private (Picker)
                (gs:node-drawfunction Picker))
(define-handler draw-function-to-use :private (IconTextPicker)
                (gs:node-drawfunction IconTextPicker))
(define-handler draw-function-to-use :private (styledPicker)
                (gs:node-drawfunction styledPicker))
(define-handler draw-function-to-use :private (tablepicker)
                (gs:node-drawfunction tablepicker))

(defun UIPicker-Draw (thePicker thePort draw-rgn)
  (funcall (draw-function-to-use thePicker) thePicker thePort draw-rgn)
  (unless *MacStyleInterface*
    (gs:let+ ((linespace (linespacing thePicker))
              fill-rect-left fill-rect-top fill-rect-right fill-rect-bottom
              (fill-rgn (gs:node-fillRegion thePicker))
              (rows (length (items thepicker)))
              (line-height (LineHeight thePicker))
              (score1 (make-color 65535 65535 65535))
              (score2 (make-color 48059 48059 48059))
              (selecteditems (selection* thePicker))
              (fill-rect (gs:hrref fill-rgn :region.rgnbbox :storage :pointer))
              )
      (gs:with-clipped-region draw-rgn
        ;; Render the fill and the frame
        
        (setq fill-rect-left (rref fill-rect :rect.left))                       ;;(gs:f.round hOffset)))
        (setq fill-rect-top (rref fill-rect :rect.top))                         ;;(gs:f.round vOffset)))
        (setq fill-rect-right (rref fill-rect :rect.right))                     ;;(gs:f.round hOffset)))
        (if (< fill-rect-left 0) (decf fill-rect-right fill-rect-left))
        (setq fill-rect-bottom (rref fill-rect :rect.bottom))                   ;;(gs:f.round vOffset)))
        (if (< fill-rect-top 0) (decf fill-rect-bottom fill-rect-top))
        
        (#_PenSize 1 1)
        (with-rgb (rgb1 score1)
          (with-rgb (rgb2 score2)
            (do ((cur-Row (verticalscroll thepicker) (1+ cur-Row))
                 (top fill-rect-top bottom)
                 (bottom fill-rect-top bottom))
                ((or (>= cur-row rows) 
                     (>= bottom fill-rect-bottom)))
              (setf bottom (+ top linespace line-height))
              (when (>= cur-Row 0)
                (#_RGBForeColor rgb2)
                (#_MoveTo (1- fill-rect-left) bottom)
                (#_LineTo fill-rect-right bottom)
                (unless (memq (1+ cur-row) selecteditems)
                  (#_RGBForeColor rgb1)
                  (#_MoveTo (1- fill-rect-left) (1+ bottom))
                  (#_LineTo fill-rect-right (1+ bottom)))
                )
              )))
        (#_PenNormal)))))

(defun UIMultilinePicker-Draw (thePicker thePort draw-rgn)
  (funcall (gs:node-drawfunction multilinepicker) thePicker thePort draw-rgn)
  (unless *MacStyleInterface*
    (gs:let+ ((linespace (linespacing thePicker))
              fill-rect-left fill-rect-top fill-rect-right fill-rect-bottom
              (fill-rgn (gs:node-fillRegion thePicker))
              (rows (length (items thepicker)))
              (lineinfo (lineinfo thepicker))
              (score1 (make-color 65535 65535 65535))
              (score2 (make-color 48059 48059 48059))
              (selecteditems (selection* thePicker))
              (fill-rect (gs:hrref fill-rgn :region.rgnbbox :storage :pointer))
              )
      (gs:with-clipped-region draw-rgn
        ;; Render the fill and the frame
        
        (setq fill-rect-left (rref fill-rect :rect.left))                       ;;(gs:f.round hOffset)))
        (setq fill-rect-top (rref fill-rect :rect.top))                         ;;(gs:f.round vOffset)))
        (setq fill-rect-right (rref fill-rect :rect.right))                     ;;(gs:f.round hOffset)))
        (if (< fill-rect-left 0) (decf fill-rect-right fill-rect-left))
        (setq fill-rect-bottom (rref fill-rect :rect.bottom))                   ;;(gs:f.round vOffset)))
        (if (< fill-rect-top 0) (decf fill-rect-bottom fill-rect-top))
        
        (#_PenSize 1 1)
        (with-rgb (rgb1 score1)
          (with-rgb (rgb2 score2)
            (do ((cur-Row (verticalscroll thepicker) (1+ cur-Row))
                 (top fill-rect-top bottom)
                 (bottom fill-rect-top bottom))
                ((or (>= cur-row rows) 
                     (>= bottom fill-rect-bottom)))
              (setf bottom (+ top linespace (car (nth cur-row lineinfo))))
              (when (>= cur-Row 0)
                (#_RGBForeColor rgb2)
                (#_MoveTo (1- fill-rect-left) bottom)
                (#_LineTo fill-rect-right bottom)
                (unless (memq (1+ cur-row) selecteditems)
                  (#_RGBForeColor rgb1)
                  (#_MoveTo (1- fill-rect-left) (1+ bottom))
                  (#_LineTo fill-rect-right (1+ bottom)))
                )
              )))
        (#_PenNormal)))))

;;;;;;;;;;;;;;;_______________________________________________________________________________________________________________________
;;;;;;;;;;;;;;;----------------------SCROLLER!---------------------------------

;;;Vertical


(new imageRenderer :objectName "UIScrollerBody" :project ui)
(setf (backgroundRenderer UIScrollerBody) nil)
(setf (media UIScrollerBody) (new iconrsrc :project ui :resourceId 10005 :file (file ui)))

(define-handler render (UIScrollerBody theActor region thePaper)
  (if *MacStyleInterface*
    (progn
      (render ScrollerBodyColor theactor region thePaper)
      (render simpleframe theactor region thePaper))
    (call-next-method)))

(new imageRenderer :objectName "UIScrollerThumb" :project ui)
(setf (backgroundRenderer UIScrollerThumb) nil)
(setf (media UIScrollerThumb) (new iconrsrc :project ui :resourceId 10006 :file (file ui)))

(define-handler render (UIScrollerThumb theActor region thePaper)
  (if (or *MacStyleInterface* (neq (project theactor) ui))
    (progn
      (render white theactor region thePaper)
      (render simpleframe theactor region thePaper))
    (gs:let+ ((color1 (make-color 56797 56797 56797))
           (color2 (make-color 30583 30583 30583))
           (color3 (make-color 43690 43690 43690))
           (ourRect (:rect))
           (ourRegion (:region))
           (qd-rect (gs:hrref region :region.rgnbbox :storage :pointer))
           left
           top
           right
           bottom
           fillRegion
           height
           )
      (gs:recompute-fill-region theActor (gs:node-flags theactor))
      (setf fillRegion (gs:node-fillRegion theActor)
            qd-rect (gs:hrref fillRegion :region.rgnbbox :storage :pointer)
            left (rref qd-rect :rect.left)
            top (rref qd-rect :rect.top)
            right (rref qd-rect :rect.right)
            bottom (rref qd-rect :rect.bottom)
            height (mod (- bottom top 5) 3))
      (decf bottom)
      (decf right)
      (UIPaintIt color3 left top right bottom)
      (rset ourRect :rect.left (+ left 1))
      (rset ourRect :rect.top (+ top 2 (if (> height 1) 1 0)))
      (rset ourRect :rect.right (- right 1))
      (rset ourRect :rect.bottom (- bottom 2 (if (> height 0) 1 0)))
      (#_rectRgn ourRegion ourRect)
      (rendertiled (media me) me theActor ourRegion thePaper)
      (UILDraw color2 left bottom  right bottom  right top)
      (UILDraw color1 left bottom  left top  right top)
      (#_pennormal)
      )))
(new imageRenderer :objectName "UIUpArrow1" :project ui)
(setf (backgroundRenderer UIUpArrow1) black)
(setf (media UIUpArrow1) (new IconRSRC :project ui :resourceId 10001 :file (file ui)))

(define-handler render (UIUpArrow1 theActor region thePaper)
  (if *MacStyleInterface*
    (render ScrollerUpRenderer theactor region thePaper)
    (call-next-method)))

(new imageRenderer :objectName "UIUpArrow2" :project ui)
(setf (backgroundRenderer UIUpArrow2) black)
(setf (media UIUpArrow2) (new IconRSRC :project ui :resourceId 10002 :file (file ui)))

(define-handler render (UIUpArrow2 theActor region thePaper)
  (if *MacStyleInterface*
    (render scrollerUpPressedRenderer theactor region thePaper)
    (call-next-method)))

(new imageRenderer :objectName "UIDownArrow1" :project ui)
(setf (backgroundRenderer UIDownArrow1) black)
(setf (media UIDownArrow1) (new IconRSRC :project ui :resourceId 10003 :file (file ui)))

(define-handler render (UIDownArrow1 theActor region thePaper)
  (if *MacStyleInterface*
    (render ScrollerDownRenderer theactor region thePaper)
    (call-next-method)))

(new imageRenderer :objectName "UIDownArrow2" :project ui)
(setf (backgroundRenderer UIDownArrow2) black)
(setf (media UIDownArrow2) (new IconRSRC :project ui :resourceId 10004 :file (file ui)))

(define-handler render (UIDownArrow2 theActor region thePaper)
  (if *MacStyleInterface*
    (render scrollerDownPressedRenderer theactor region thePaper)
    (call-next-method)))

(setf (renderstyle UIScrollerBody) 'Rendertiled)
(setf (renderstyle UIUpArrow1) 'Renderunstretched)
(setf (renderstyle UIUpArrow2) 'Renderunstretched)
(setf (renderstyle UIDownArrow1) 'Renderunstretched)
(setf (renderstyle UIDownArrow2) 'Renderunstretched)

(new scroller :objectname "uiVerticalScroller" :project ui)
(setf (minimumThumbSize uiVerticalScroller) 9)
(setf (fillcolor (scrollerBody uiVerticalScroller)) UIScrollerBody)
(setf (fillcolor (thumb uiVerticalScroller)) UIScrollerThumb)
(setf (framesize (thumb uiVerticalScroller)) '(0 0))
(setf (framesize (scrollerBody uiVerticalScroller)) '(0 0))
(setf (framesize (uparrow uiVerticalScroller)) '(0 0))
(setf (framesize (downarrow uiVerticalScroller)) '(0 0))
(setf (fillcolor (uparrow uiVerticalScroller)) UIUpArrow1)
(setf (fillcolor (downarrow uiVerticalScroller)) UIDownArrow1)

(define-handler arrowPressedRenderer (uiVerticalScroller &optional (thearrow 'down))
  (case thearrow
    (down UIDownArrow2)
    (up UIUpArrow2)
    (left scrollerLeftPressedRenderer)
    (right scrollerRightPressedRenderer)))
(define-handler bestSize ((uparrow uiVerticalScroller))
  (declare (special left top right bottom))
  (setBoundsRect me left top right (+ top 16) :physical t))
(define-handler bestSize ((downarrow uiVerticalScroller))
  (declare (special left top right bottom ))
  (setBoundsRect me left (- bottom 16) right bottom :physical t))
(define-handler bestSize ((scrollerBody uiVerticalScroller))
  (declare (special left top right bottom ))
  (setBoundsRect me left (+ top 16) right (- bottom 16)
                 :physical t))

(define-handler bestSize ((thumb uiVerticalScroller))
  (declare (special left top right bottom ))
  (call-next-method)
  (setBoundsRect me (if *MacStyleInterface* 0 1) 0 0 0
                 :relative t :physical t))





;;;Horizontal


(new imageRenderer :objectName "UIScrollerBodyHorizontal":project ui)
(setf (backgroundRenderer UIScrollerBodyHorizontal) nil)
(setf (media UIScrollerBodyHorizontal) (new iconrsrc :project ui :resourceId 11005 :file (file ui)))

(define-handler render (UIScrollerBodyHorizontal theActor region thePaper)
  (if *MacStyleInterface*
    (progn
      (render ScrollerBodyColor theactor region thePaper)
      (render simpleframe theactor region thePaper))
    (call-next-method)))

(new imageRenderer :objectName "UIScrollerThumbHorizontal":project ui)
(setf (backgroundRenderer UIScrollerThumbHorizontal) nil)
(setf (media UIScrollerThumbHorizontal) (new iconrsrc :project ui :resourceId 11006 :file (file ui)))

(define-handler render (UIScrollerThumbHorizontal theActor region thePaper)
  (if (or *MacStyleInterface* (neq (project theactor) ui))
    (render white theactor region thePaper)
    (gs:let+ ((color1 (make-color 56797 56797 56797))
           (color2 (make-color 30583 30583 30583))
           (color3 (make-color 43690 43690 43690))
           (ourRect (:rect))
           (ourRegion (:region))
           (qd-rect (gs:hrref region :region.rgnbbox :storage :pointer))
           left
           top
           right
           bottom
           fillRegion
           height
           )
      (gs:recompute-fill-region theActor (gs:node-flags theactor))
      (setf fillRegion (gs:node-fillRegion theActor)
            qd-rect (gs:hrref fillRegion :region.rgnbbox :storage :pointer)
            left (rref qd-rect :rect.left)
            top (rref qd-rect :rect.top)
            right (rref qd-rect :rect.right)
            bottom (rref qd-rect :rect.bottom)
            height (mod (- right left 5) 3))
      (decf bottom)
      (decf right)
      (UIPaintIt color3 left top right bottom)
      (rset ourRect :rect.left (+ left 2 (if (> height 1) 1 0)))
      (rset ourRect :rect.top (+ top 1))
      (rset ourRect :rect.right (- right 2 (if (> height 0) 1 0)))
      (rset ourRect :rect.bottom (- bottom 1))
      (#_rectRgn ourRegion ourRect)
      (rendertiled (media me) me theActor ourRegion thePaper)
      (UILDraw color2 left bottom  right bottom  right top)
      (UILDraw color1 left bottom  left top  right top)
      (#_pennormal)
      )))

(new imageRenderer :objectName "UIRightArrow1":project ui)
(setf (backgroundRenderer UIRightArrow1) black)
(setf (media UIRightArrow1) (new IconRSRC :project ui :resourceId 11003 :file (file ui)))

(define-handler render (UIRightArrow1 theActor region thePaper)
  (if *MacStyleInterface*
    (render ScrollerLeftRenderer theactor region thePaper)
    (call-next-method)))

(new imageRenderer :objectName "UIRightArrow2":project ui)
(setf (backgroundRenderer UIRightArrow2) black)
(setf (media UIRightArrow2) (new IconRSRC :project ui :resourceId 11004 :file (file ui)))

(define-handler render (UIRightArrow2 theActor region thePaper)
  (if *MacStyleInterface*
    (render ScrollerLeftPressedRenderer theactor region thePaper)
    (call-next-method)))

(new imageRenderer :objectName "UILeftArrow1":project ui)
(setf (backgroundRenderer UILeftArrow1) black)
(setf (media UILeftArrow1) (new IconRSRC :project ui :resourceId 11001 :file (file ui)))

(define-handler render (UILeftArrow1 theActor region thePaper)
  (if *MacStyleInterface*
    (render ScrollerRightRenderer theactor region thePaper)
    (call-next-method)))

(new imageRenderer :objectName "UILeftArrow2":project ui)
(setf (backgroundRenderer UILeftArrow2) black)
(setf (media UILeftArrow2) (new IconRSRC :project ui :resourceId 11002 :file (file ui)))

(define-handler render (UILeftArrow2 theActor region thePaper)
  (if *MacStyleInterface*
    (render ScrollerRightPressedRenderer theactor region thePaper)
    (call-next-method)))

(setf (renderstyle UIScrollerBodyHorizontal) 'Rendertiled)
(setf (renderstyle UIRightArrow1) 'Renderunstretched)
(setf (renderstyle UIRightArrow2) 'Renderunstretched)
(setf (renderstyle UILeftArrow1) 'Renderunstretched)
(setf (renderstyle UILeftArrow2) 'Renderunstretched)

(new scroller :objectname "uiHorizontalScroller":project ui)
(setf (minimumThumbSize uiHorizontalScroller) 9)
(setf (fillcolor (scrollerBody uiHorizontalScroller)) UIScrollerBodyHorizontal)
(setf (fillcolor (thumb uiHorizontalScroller)) UIScrollerThumbHorizontal)
(setf (framesize (thumb uiHorizontalScroller)) '(0 0))
(setf (framesize (scrollerBody uiHorizontalScroller)) '(0 0))
(setf (framesize (uparrow uiHorizontalScroller)) '(0 0))
(setf (framesize (downarrow uiHorizontalScroller)) '(0 0))
(setf (fillcolor (uparrow uiHorizontalScroller)) UIRightArrow1)
(setf (fillcolor (downarrow uiHorizontalScroller)) UILeftArrow1)

(define-handler arrowPressedRenderer (uiHorizontalScroller &optional (thearrow 'down))
  (case thearrow
    (left UIRightArrow2)
    (right UILeftArrow2)
    (up scrollerLeftPressedRenderer)
    (down scrollerRightPressedRenderer)))
(define-handler bestSize ((uparrow uiHorizontalScroller))
  (declare (special left top right bottom))
  (setBoundsRect me left top (+ left 16) bottom :physical t))
(define-handler bestSize ((downarrow uiHorizontalScroller))
  (declare (special left top right bottom ))
  (setBoundsRect me (- right 16) top right bottom :physical t))
(define-handler bestSize ((scrollerBody uiHorizontalScroller))
  (declare (special left top right bottom ))
  (setBoundsRect me (+ left 16) top (- right 16) bottom
                 :physical t))

(define-handler bestSize ((thumb uiHorizontalScroller))
  (declare (special left top right bottom ))
  (call-next-method)
  (setBoundsRect me 0 (if *MacStyleInterface* 0 1) 0 0
                 :relative t :physical t))


#|
(new uiHorizontalScroller :objectname "hahaha" :project ui)
(setf (container hahaha) nil)
(setboundsrect hahaha 11 131 200 145)

|#







;;;;;;;;;;;---------------------Shadowed Renderer

(new UIRenderer :objectname "ShadowedRenderer" :project ui)
(addproperty ShadowedRenderer 'renderedges)
(new rgbcolor :objectname "ShadowWhite" :project ui)
(setf (forered ShadowWhite) 56797)
(setf (foregreen ShadowWhite) 56797)
(setf (foreblue ShadowWhite) 56797)
(setf (backgroundRenderer ShadowedRenderer) ShadowWhite)

(define-handler render (ShadowedRenderer theActor region thePaper)
  (if *MacStyleInterface*
    (progn
      (render white theactor region thePaper)
      (render simpleframe theactor region thePaper))
    (progn
      (gs:recompute-fill-region theActor (gs:node-flags theactor))
      (gs:let+ (fill-rect-left 
             fill-rect-top
             fill-rect-right
             fill-rect-bottom
             (fill-rgn (gs:node-fillRegion theActor))
             (dlc1 (make-color 4369 4369 4369))
             (dlc2 (make-color 21845 21845 21845))
             (dlc3 (make-color 30583 30583 30583))
             (dlc4 (make-color 48059 48059 48059))
             (fill-rect (gs:hrref fill-rgn :region.rgnbbox :storage :pointer))
             )
        (setq fill-rect-left (rref fill-rect :rect.left))
        (setq fill-rect-top (rref fill-rect :rect.top))
        (setq fill-rect-right (rref fill-rect :rect.right))
        (if (< fill-rect-left 0) (decf fill-rect-right fill-rect-left))
        (setq fill-rect-bottom (rref fill-rect :rect.bottom))
        (if (< fill-rect-top 0) (decf fill-rect-bottom fill-rect-top))
        
        (render (backgroundrenderer me) theActor fill-rgn thePaper)
        (#_penNormal)
        (#_penSize 1 1)
        
        (decf fill-rect-right)
        #|
        (decf fill-rect-bottom)
        
        (with-rgb (rgb (sk8dev::mcl-color uilightcolor))
          (#_RGBForeColor rgb))
        (#_MoveTo fill-rect-left fill-rect-bottom)
        (#_LineTo fill-rect-right fill-rect-bottom)
        |#
        (decf fill-rect-bottom)
        (when (renderedges me)
          (decf fill-rect-bottom))
        
        (with-rgb (rgb dlc4)
          (#_RGBForeColor rgb))
        ;(#_MoveTo (+ fill-rect-left 4) fill-rect-bottom)
        (#_MoveTo (+ fill-rect-left 3) fill-rect-bottom)
        (#_LineTo (+ fill-rect-left 3) (+ fill-rect-top 3))
        (#_LineTo fill-rect-right (+ fill-rect-top 3))
        (#_LineTo fill-rect-right (+ fill-rect-top 4))
        (#_MoveTo (+ fill-rect-left 4) (+ fill-rect-top 4))
        (#_LineTo (+ fill-rect-left 4) (+ fill-rect-top 4))
        
        (with-rgb (rgb dlc3)
          (#_RGBForeColor rgb))
        ;(#_MoveTo (+ fill-rect-left 3) fill-rect-bottom)
        (#_MoveTo (+ fill-rect-left 2) fill-rect-bottom)
        (#_LineTo (+ fill-rect-left 2) (+ fill-rect-top 2))
        (#_LineTo fill-rect-right (+ fill-rect-top 2))
        (#_LineTo fill-rect-right (+ fill-rect-top 3))
        (#_MoveTo (+ fill-rect-left 3) (+ fill-rect-top 3))
        (#_LineTo (+ fill-rect-left 3) (+ fill-rect-top 3))
        
        (with-rgb (rgb dlc2)
          (#_RGBForeColor rgb))
        ;(#_MoveTo (+ fill-rect-left 2) fill-rect-bottom)
        (#_MoveTo (+ fill-rect-left 1) fill-rect-bottom)
        (#_LineTo (+ fill-rect-left 1) (+ fill-rect-top 1))
        (#_LineTo fill-rect-right (+ fill-rect-top 1))
        (#_LineTo fill-rect-right (+ fill-rect-top 2))
        (#_MoveTo (+ fill-rect-left 2) (+ fill-rect-top 2))
        (#_LineTo (+ fill-rect-left 2) (+ fill-rect-top 2))
        
        (with-rgb (rgb dlc1)
          (#_RGBForeColor rgb))
        (#_MoveTo (+ fill-rect-left 1) fill-rect-bottom)
        (#_LineTo (+ fill-rect-left 0) fill-rect-bottom)
        (#_LineTo (+ fill-rect-left 0) (+ fill-rect-top 0))
        (#_LineTo (- fill-rect-right 1) (+ fill-rect-top 0))
        (#_LineTo (- fill-rect-right 1) (+ fill-rect-top 1))
        (#_MoveTo (+ fill-rect-left 1) (+ fill-rect-top 1))
        (#_LineTo (+ fill-rect-left 1) (+ fill-rect-top 1))
        
        (with-rgb (rgb dlc2)
          (#_RGBForeColor rgb))
        (#_MoveTo (+ fill-rect-left 0) (+ fill-rect-top 1))
        (#_LineTo (+ fill-rect-left 0) (+ fill-rect-top 1))
        (#_MoveTo (+ fill-rect-left 1) (+ fill-rect-top 0))
        (#_LineTo (+ fill-rect-left 1) (+ fill-rect-top 0))
        (#_MoveTo (- fill-rect-right 1) (+ fill-rect-top 0))
        (#_LineTo (- fill-rect-right 1) (+ fill-rect-top 0))
        (with-rgb (rgb dlc3)
          (#_RGBForeColor rgb))
        (#_MoveTo (+ fill-rect-left 0) (+ fill-rect-top 0))
        (#_LineTo (+ fill-rect-left 0) (+ fill-rect-top 0))
        (#_MoveTo (+ fill-rect-left 0) (+ fill-rect-bottom 0))
        (#_LineTo (+ fill-rect-left 0) (+ fill-rect-bottom 0))
        (with-rgb (rgb dlc4)
          (#_RGBForeColor rgb))
        (#_MoveTo (+ fill-rect-left 0) (+ fill-rect-right 0))
        (#_LineTo (+ fill-rect-left 0) (+ fill-rect-right 0))
        
        (when (renderedges me)
          (#_forecolor #$whitecolor)
          (#_MoveTo (+ fill-rect-left 5) (- fill-rect-bottom 0))
          (#_LineTo (- fill-rect-right 1) (- fill-rect-bottom 0))
          (#_LineTo (- fill-rect-right 1) (- fill-rect-bottom 1))
          (#_LineTo (- fill-rect-right 0) (- fill-rect-bottom 1))
          (#_LineTo (- fill-rect-right 0) (+ fill-rect-top 5))
          )
        
        (#_forecolor #$blackcolor)
        (#_backcolor #$whitecolor)
        (#_pennormal)
        ))))


(new shadowedrenderer :objectname "ActivatedShadowRenderer" :project ui)
(setf (backgroundrenderer ActivatedShadowRenderer) white)

;;(setf (renderedges shadowedrenderer) t)

;;;;;;;;;;;---------------------UI Menu Color

(new UIRenderer :objectname "UIMenuColor" :project ui)

(new rgbcolor :objectname "UIDefaultMenuRGB" :project ui)
(setf (forered UIDefaultMenuRGB) 39321)
(setf (foregreen UIDefaultMenuRGB) 39321)
(setf (foreblue UIDefaultMenuRGB) 39321)
(setf (backgroundRenderer UIMenuColor) UIDefaultMenuRGB)

(define-handler render (UIMenuColor theActor region thePaper)
  (if *MacStyleInterface*
    (progn
      (render white theactor region thePaper)
      ;;(render simpleframe theactor region)
      (when (is-a (container theactor) sk8::menubar)
        (render simpleframe (container theactor) region thePaper)))
    (progn
      (gs:recompute-fill-region theActor (gs:node-flags theactor))
      (gs:let+ (fill-rect-left 
             fill-rect-top
             fill-rect-right
             fill-rect-bottom
             (fill-rgn (gs:node-fillRegion theActor))
             (cornerGray (make-color 52428 52428 52428)) ; light gray for corners
             (midGrayInnerLine (make-color 21845 21845 21845))
             (BottomLine (make-color 52428 52428 52428))  ;; bottom line
             (darkmiddleline (make-color 8738 8738 8738)) 
             (leftlightline (make-color 48059 48059 48059))
             (rightlightline (make-color 30583 30583 30583))
             (fill-rect (gs:hrref fill-rgn :region.rgnbbox :storage :pointer))
             )
        (setq fill-rect-left (rref fill-rect :rect.left))
        (setq fill-rect-top (rref fill-rect :rect.top))
        (setq fill-rect-right (rref fill-rect :rect.right))
        (if (< fill-rect-left 0) (decf fill-rect-right fill-rect-left))
        (setq fill-rect-bottom (rref fill-rect :rect.bottom))
        (if (< fill-rect-top 0) (decf fill-rect-bottom fill-rect-top))
        
        (render (backgroundrenderer me) theActor fill-rgn thePaper)
        (#_penNormal)
        (#_penSize 1 1)
        
        (decf fill-rect-bottom)
        (decf fill-rect-right)
        
        (with-rgb (rgb rightlightline)
          (#_RGBForeColor rgb))
        (#_MoveTo (- fill-rect-right 3) (- fill-rect-bottom 3))
        (#_LineTo (- fill-rect-right 2) (- fill-rect-bottom 3))
        (#_LineTo (- fill-rect-right 2) fill-rect-top)
        
        (with-rgb (rgb leftlightline)
          (#_RGBForeColor rgb))
        (#_MoveTo (+ fill-rect-left 1) (- fill-rect-bottom 4))
        (#_LineTo (+ fill-rect-left 1) fill-rect-top)
        
        (with-rgb (rgb midGrayInnerLine)
          (#_RGBForeColor rgb))
        (#_MoveTo (+ fill-rect-left 2) (- fill-rect-bottom 3))
        (#_LineTo (+ fill-rect-left 0) (- fill-rect-bottom 3))
        (#_LineTo (+ fill-rect-left 0) fill-rect-top)
        (#_MoveTo (- fill-rect-right 2) (- fill-rect-bottom 3))
        (#_LineTo (- fill-rect-right 1) (- fill-rect-bottom 3))
        (#_LineTo (- fill-rect-right 1) fill-rect-top)
        
        (with-rgb (rgb BottomLine)
          (#_RGBForeColor rgb))
        (#_MoveTo (+ fill-rect-left 0) (- fill-rect-bottom 2))
        (#_LineTo (- fill-rect-right 1) (- fill-rect-bottom 2))
        (#_MoveTo (+ fill-rect-left 0) (- fill-rect-bottom 1))
        (#_LineTo (- fill-rect-right 1) (- fill-rect-bottom 1))
        (#_MoveTo (+ fill-rect-left 0) (- fill-rect-bottom 0))
        (#_LineTo (- fill-rect-right 1) (- fill-rect-bottom 0))
        
        
        (with-rgb (rgb darkmiddleline)
          (#_RGBForeColor rgb))
        (#_MoveTo (+ fill-rect-right 0) (- fill-rect-bottom 3))
        (#_LineTo (- fill-rect-right 0) fill-rect-top)
        (#_MoveTo (+ fill-rect-left 1) (- fill-rect-bottom 2))
        (#_LineTo (- fill-rect-right 1) (- fill-rect-bottom 2))
        (#_MoveTo (+ fill-rect-left 3) (- fill-rect-bottom 1))
        (#_LineTo (- fill-rect-right 3) (- fill-rect-bottom 1))
        
        (with-rgb (rgb midGrayInnerLine)
          (#_RGBForeColor rgb))
        (#_MoveTo (+ fill-rect-left 3) (- fill-rect-bottom 2))
        (#_LineTo (- fill-rect-right 3) (- fill-rect-bottom 2))
        
        (with-rgb (rgb cornerGray)
          (#_RGBForeColor rgb))
        (#_MoveTo (+ fill-rect-left 0) (- fill-rect-bottom 1))
        (#_LineTo (+ fill-rect-left 0) (- fill-rect-bottom 0))
        (#_LineTo (+ fill-rect-left 1) (- fill-rect-bottom 0))
        (#_MoveTo (- fill-rect-right 0) (- fill-rect-bottom 2))
        (#_LineTo (- fill-rect-right 0) (- fill-rect-bottom 0))
        (#_LineTo (- fill-rect-right 1) (- fill-rect-bottom 0))
        
        (#_forecolor #$blackcolor)
        (#_backcolor #$whitecolor)
        (#_pennormal)
        ))))


;;;;;;;;;;;------------------------------------------
;;;;;;;;;;;---------------------UIButton
;;;;;;;;;;;------------------------------------------


(new imageRenderer :objectName "UIButtonOutColor" :project ui)
(setf (backgroundRenderer UIButtonOutColor) black)
(setf (media UIButtonOutColor) (new iconrsrc :project ui :resourceId 6001 :file (file ui)))
(setf (renderstyle UIButtonOutColor) 'Renderstretched)
(new imageRenderer :objectName "UIButtonInColor" :project ui)
(setf (backgroundRenderer UIButtonInColor) black)
(setf (media UIButtonInColor) (new iconrsrc :project ui :resourceId 6002 :file (file ui)))
(setf (renderstyle UIButtonInColor) 'Renderstretched)
(new imageRenderer :objectName "UIButtonDisabledColor" :project ui)
(setf (backgroundRenderer UIButtonDisabledColor) black)
(setf (media UIButtonDisabledColor) (new iconrsrc :project ui :resourceId 6003 :file (file ui)))
(setf (renderstyle UIButtonDisabledColor) 'Renderstretched)

(new rectangle :objectname "UIButton" :project ui)
(addproperty uibutton 'enabled)
(setf (textFont UIButton) EspySansFont)
(setf (textSize UIButton) 9)
(setf (textLocation UIButton) 'center)
(setf (framesize UIButton) '(0 0))
(setf (size UIButton) '(54 21))
(setf (mouseSensitivity UIButton) 'opaque)
(setf (doubleClickStyle UIButton) 'clickOnly)
(setf (autoHighlight UIButton) t)

(define-handler (setf enabled) (theval UIButton)
  (withactorlocked (me)
    (sk8::setvalue 'enabled me theval)
    (if theval
      (progn
        (setf (textcolor me) black)
        (setf (Highlight me) nil))
      (progn
        (setf (texthoffset me) -1)
        (setf (textvoffset me) -1)
        (setf (textcolor me) gray)
        (setf (fillcolor me) UIButtonDisabledColor))
      )))

(define-handler mousedown (UIButton)
  (when (enabled me)
    (call-next-method)))
(define-handler mouseup (UIButton)
  (when (enabled me)
    (call-next-method)))

(define-handler Highlight (UIButton)
  (call-next-method))

(define-handler (Setf Highlight) (value UIButton)
  (withactorlocked (me)
    (setf (inverts me) nil)
    (call-next-method)
    (withActorLocked (me)
      (if value
        (progn
          (setf (texthoffset me) 0)
          (setf (textvoffset me) 0)
          (setf (fillcolor me) UIButtonInColor))
        (progn
          (setf (texthoffset me) -2)
          (setf (textvoffset me) -2)
          (setf (fillcolor me) UIButtonOutColor))))))
(setf (enabled UIButton) t)

(define-handler addedMeAsParent (UIButton Child OldParents)
  (declare (ignore oldparents))
  (let ((tt (text child)))
    (setf (highlight Child) nil)
    (setf (enabled Child) t)
    (setf (autohighlight Child) t)
    (setf (framesize Child) '(0 0))
    (setf (fillcolor Child) (fillcolor UIButton))
    (copyTextProperties child UIButton)
    (call-next-method)
    (setf (text child) tt)
    ))




;;;;;;;;;;;------------------------------------------------------------------------------------
;;;;;;;;;;;---------------------Project Overviewer Button In
;;;;;;;;;;;------------------------------------------------------------------------------------


(new uirenderer :objectName "ProjectButtonIn" :project ui)

(define-handler render (ProjectButtonIn theActor region thePaper)
  (gs:recompute-fill-region theActor (gs:node-flags theactor))
  (gs:let+ (fill-rect-left 
         fill-rect-top
         fill-rect-right
         fill-rect-bottom
         (fill-rgn (gs:node-fillRegion theActor))
         (darkness (make-color 0 0 0))
         (middle (make-color 21823 21823 21823))
         (loweredge (make-color 39321 39321 39321))
         (upperrim (make-color 34734 34734 34734))
         (fill-rect (gs:hrref fill-rgn :region.rgnbbox :storage :pointer))
         )
    (setq fill-rect-left (rref fill-rect :rect.left))
    (setq fill-rect-top (rref fill-rect :rect.top))
    (setq fill-rect-right (rref fill-rect :rect.right))
    (if (< fill-rect-left 0) (decf fill-rect-right fill-rect-left))
    (setq fill-rect-bottom (rref fill-rect :rect.bottom))
    (if (< fill-rect-top 0) (decf fill-rect-bottom fill-rect-top))
    
    (#_penNormal)
    (#_penSize 1 1)
    
    (decf fill-rect-bottom)
    (decf fill-rect-right)
    (render UIMiddle theActor region thePaper)
    (uipaintit middle (+ 1 fill-rect-left) (+ 1 fill-rect-top) (- fill-rect-right 1) fill-rect-bottom)
    (UILDraw upperrim fill-rect-left fill-rect-bottom  fill-rect-left fill-rect-top  (- fill-rect-right 1) fill-rect-top)
    (with-rgb (rgb upperrim)
      (#_RGBForeColor rgb))
    (#_moveto (- fill-rect-right 1) fill-rect-top)
    (#_lineto (- fill-rect-right 1) fill-rect-bottom)
    (UILDraw darkness 
             (+ 1 fill-rect-left) (+ 1 fill-rect-bottom)
             (+ 1 fill-rect-left) (+ 1 fill-rect-top)
             (- fill-rect-right 2) (+ 1 fill-rect-top)
             )
    (with-rgb (rgb loweredge)
      (#_RGBForeColor rgb))
    (#_moveto (+ 3 fill-rect-left) fill-rect-bottom)
    (#_lineto (- fill-rect-right 2) fill-rect-bottom)
    
    ))



;;;;;;;;;;;------------------------------------------
;;;;;;;;;;;---------------------UIPopup
;;;;;;;;;;;------------------------------------------

(new uirenderer :objectname "UIMenuRenderer" :project ui)
(addproperty uiMenuRenderer 'ArrowToDraw :initialvalue t)

(new UIMenuRenderer :objectName "UIMenuOut" :project ui)
(setf (backgroundRenderer UIMenuOut) uimiddle)
(setf (arrowtodraw uimenuout) t)

(define-handler render (UIMenuOut theActor region thePaper)
  (declare (ignore region))
  (gs:recompute-fill-region theActor (gs:node-flags theactor))
  (gs:let+ (fill-rect-left fill-rect-top fill-rect-right fill-rect-bottom
                        (fill-rgn (gs:node-fillRegion theActor))
                        (outeredge (make-color 39321 39321 39321))
                        (blackish (make-color 0 0 0))
                        (darkrect (make-color 17476 17476 17476))
                        (midrim (if *MacStyleInterface* 
                                      (make-color 65535 65535 65535)
                                      (make-color 30583 30583 30583))
                                )
                        (lightedge (if *MacStyleInterface* 
                                      (make-color 65535 65535 65535)
                                      (make-color 48059 48059 48059)))
                        (whitish (if *MacStyleInterface* 
                                      (make-color 65535 65535 65535)
                                      (make-color 61166 61166 61166)))
                        (centerpart (if *MacStyleInterface* 
                                      (make-color 65535 65535 65535)
                                      (make-color 39321 39321 39321)))
                        (fill-rect (gs:hrref fill-rgn :region.rgnbbox :storage :pointer))
                        )
    (setq fill-rect-left (rref fill-rect :rect.left))
    (setq fill-rect-top (rref fill-rect :rect.top))
    (setq fill-rect-right (rref fill-rect :rect.right))
    (if (< fill-rect-left 0) (decf fill-rect-right fill-rect-left))
    (setq fill-rect-bottom (rref fill-rect :rect.bottom))
    (if (< fill-rect-top 0) (decf fill-rect-bottom fill-rect-top))
    
    (render uimiddle theActor fill-rgn thePaper)
    (#_penNormal)
    (#_penSize 1 1)
    
    (uiframeit outeredge (+ fill-rect-left 3) (+ fill-rect-top 2) fill-rect-right fill-rect-bottom)
    (uiframeit blackish (+ fill-rect-left 2) (+ fill-rect-top 1) (- fill-rect-right 1) (- fill-rect-bottom 1))
    (uiframeit darkrect (+ fill-rect-left 0) (+ fill-rect-top 0) (- fill-rect-right 2) (- fill-rect-bottom 2))
    (uiPaintIt centerpart (+ fill-rect-left 3) (+ fill-rect-top 3) (- fill-rect-right 4) (- fill-rect-bottom 4))
    (when (ArrowToDraw me) (uiarrowdraw blackish fill-rect-left fill-rect-right 3 (+ fill-rect-top 6)))
    
    (decf fill-rect-bottom)
    (decf fill-rect-right)
    
    (UILDraw midrim (+ fill-rect-left 2) (- fill-rect-bottom 3)
             (- fill-rect-right 3) (- fill-rect-bottom 3)
             (- fill-rect-right 3) (+ fill-rect-top 2)
             )
    (UILDraw lightedge 
             (+ fill-rect-left 2) (- fill-rect-bottom 4)
             (+ fill-rect-left 2) (+ fill-rect-top 2)
             (- fill-rect-right 4) (+ fill-rect-top 2)
             )
    (UILDraw whitish 
             (+ fill-rect-left 1) (- fill-rect-bottom 3)
             (+ fill-rect-left 1) (+ fill-rect-top 1)
             (- fill-rect-right 3) (+ fill-rect-top 1)
             )
    (#_penNormal)))



(new UIMenuRenderer :objectName "UIMenuIn" :project ui)
(setf (backgroundRenderer UIMenuIn) darkgray)

(define-handler render (UIMenuIn theActor region thePaper)
  (declare (ignore region))
  (gs:recompute-fill-region theActor (gs:node-flags theactor))
  (gs:let+ (fill-rect-left 
         fill-rect-top
         fill-rect-right
         fill-rect-bottom
         (fill-rgn (gs:node-fillRegion theActor))
         (darkness (make-color 0 0 0))
         (midrim (make-color 17476 17476 17476))
         (lightedge (make-color 65535 65535 65535))
         (outeredge (make-color 30583 30583 30583))
         (fill-rect (gs:hrref fill-rgn :region.rgnbbox :storage :pointer))
         )
    (setq fill-rect-left (rref fill-rect :rect.left))
    (setq fill-rect-top (rref fill-rect :rect.top))
    (setq fill-rect-right (rref fill-rect :rect.right))
    (if (< fill-rect-left 0) (decf fill-rect-right fill-rect-left))
    (setq fill-rect-bottom (rref fill-rect :rect.bottom))
    (if (< fill-rect-top 0) (decf fill-rect-bottom fill-rect-top))
    
    (render uimiddle theActor fill-rgn thePaper)
    (#_penNormal)
    (#_penSize 1 1)
    
    (decf fill-rect-bottom)
    (decf fill-rect-right)
    
    (uiFrameit outeredge (+ fill-rect-left 3) (+ fill-rect-top 2) (- fill-rect-right 0) (- fill-rect-bottom 0))
    (uiFrameit darkness (+ fill-rect-left 2) (+ fill-rect-top 1) (- fill-rect-right 1) (- fill-rect-bottom 1))
    (uiFrameit midrim (+ fill-rect-left 0) (+ fill-rect-top 0) (- fill-rect-right 2) (- fill-rect-bottom 2))
    (uiPaintIt darkness (+ fill-rect-left 1) (+ fill-rect-top 1) (- fill-rect-right 3) (- fill-rect-bottom 3))
    
    (when (ArrowToDraw me) (uiarrowdraw lightedge fill-rect-left fill-rect-right 3 (+ fill-rect-top 6)))    
    ))

(new UIMenuRenderer :objectName "UIMenuDisabled" :project ui)
(setf (backgroundRenderer UIMenuDisabled) gray)

(define-handler render (UIMenuDisabled theActor region thePaper)
  (declare (ignore region))
  (gs:recompute-fill-region theActor (gs:node-flags theactor))
  (gs:let+ (fill-rect-left 
         fill-rect-top
         fill-rect-right
         fill-rect-bottom
         (fill-rgn (gs:node-fillRegion theActor))
         (outerwhite (make-color 61166 61166 61166))
         (darkrim (make-color 30583 30583 30583))
         (innerlight (make-color 56797 56797 56797))  ;; bottom line
         (centerpart (make-color 48059 48059 48059))  ;; bottom line
         (fill-rect (gs:hrref fill-rgn :region.rgnbbox :storage :pointer))
         )
    (setq fill-rect-left (rref fill-rect :rect.left))
    (setq fill-rect-top (rref fill-rect :rect.top))
    (setq fill-rect-right (rref fill-rect :rect.right))
    (if (< fill-rect-left 0) (decf fill-rect-right fill-rect-left))
    (setq fill-rect-bottom (rref fill-rect :rect.bottom))
    (if (< fill-rect-top 0) (decf fill-rect-bottom fill-rect-top))
    
    (render uimiddle theActor fill-rgn thePaper)
    (#_penNormal)
    (#_penSize 1 1)
    
    (decf fill-rect-bottom)
    (decf fill-rect-right)
    
    (uiframeit outerwhite (+ fill-rect-left 2) (+ fill-rect-top 1) (- fill-rect-right 0) (- fill-rect-bottom 0))
    (uiframeit darkrim (+ fill-rect-left 0) (+ fill-rect-top 1) (- fill-rect-right 1) (- fill-rect-bottom 1))
    (uiPaintIt centerpart (+ fill-rect-left 2) (+ fill-rect-top 3) (- fill-rect-right 2) (- fill-rect-bottom 2))
    
    (when (arrowtodraw me) (uiarrowdraw darkrim fill-rect-left fill-rect-right 1 (+ fill-rect-top 6)))
    
    
    (decf fill-rect-bottom)
    (decf fill-rect-right)
    
    (UILDraw innerlight 
             (+ fill-rect-left 1) (- fill-rect-bottom 2)
             (+ fill-rect-left 1) (+ fill-rect-top 2)
             (- fill-rect-right 2) (+ fill-rect-top 2))    
    (#_PenNormal)))




(new menu :objectname "UIPopup" :project ui)
(addparent UIPopup UIButton)
(setf (framesize UIPopup) '(0 0))
(setf (enabled UIPopup) t)
(setf (fillcolor UIPopup) UIMenuOut)
(setf (textFont UIPopup) EspySansFont)
(setf (textSize UIPopup) 9)
(setf (textlocation UIPopup) 'centerleft)

(define-handler mousedown (UIPopup)
  (when (enabled me)
    (setf (highlight me) t)
    (call-next-method)
    (setf (highlight me) nil)))


(define-handler (setf enabled)  (theval UIPopup)
  (withactorlocked (me)
    (call-next-method)
    (if theval
      (setf (highlight me) nil)
      (progn
        (setf (texthoffset me) 8)        
        (setf (fillcolor me) UIMenuDisabled))
      )))

(define-handler (Setf Highlight) (value UIPopup)
  (withactorlocked (me)
    (call-next-method)
    (setf (texthoffset me) 5)
    (setf (textvoffset me) -1)
    (if value
      (progn
        (setf (textcolor me) white)
        (setf (fillcolor me) UIMenuin))
      (progn
        (setf (textcolor me) black)
        (setf (fillcolor me) UIMenuOut)))))


(define-handler (setf menuType) (newType UIPopup)
  (call-next-method)
  (unless (eq newType 'menu)
    (setf (gs:node-drawFunction me) 'gs:sk8-subactors-draw)
    (setf (texthoffset me) 5)
    ))

(setf (menutype uipopup) 'popup)
(setf (Highlight uipopup) nil)

(define-handler menuTextSize (UIPopup)
  (setf (texthoffset me) 5)
  (setf (textlocation me) 'centerleft)
  (if (and (text me) (not (equal (text me) "")))
    (if (eq (menuType me) 'popup)
      ;; if it is a popup then we leave room for the arrow plus the text
      (sk8-multival-bind (ww hh) (ActorTextSize me)
        (sk8-multivals (+ ww 20) (+ hh 6)))
      ;; if it's not a popup then just leave a little horizontal spacing...
      (sk8-multival-bind (ww hh) (ActorTextSize me)
        (sk8-multivals (+ ww 5) (1+ hh)))) 
    ;; Otherwise, we leave enough room for just the arrow!
    (sk8-multivals 28 (height me))))

(define-handler addedMeAsParent (UIPopup Child OldParents)
  (declare (ignore oldparents))
  (let ((tt (text child)))
    (call-next-method)
    (setf (highlight Child) nil)
    (setf (menuType Child) 'popup)
    (setf (enabled Child) t)
    (setf (textlocation Child) 'centerleft)
    (setf (fillcolor Child) uimenuout)
    (setf (autohighlight Child) nil)
    (copyTextProperties child uipopup)
    (setf (text child) tt)
    ))

(define-handler initialize (UIPopup original isNew initArgs)
  (declare (ignore original isNew initArgs))
  (let ((tt (text me)))
    (setf (highlight me) nil)
    (setf (menuType me) 'popup)
    (setf (enabled me) t)
    (copyTextProperties me uipopup)
    (call-next-method)
    (setf (autohighlight me) nil)
    (setf (text me) tt)
    ))

;;;;;;;;;;;------------------------------------------------------------------------------------
;;;;;;;;;;;---------------------UI Radio Buttons and Check Boxes
;;;;;;;;;;;------------------------------------------------------------------------------------

(new imageRenderer :objectName "UICheckBoxOff" :project ui)
(setf (backgroundRenderer UICheckBoxOff) uimiddle)
(setf (media UICheckBoxOff) (new iconrsrc :project ui :resourceId 8001 :file (file ui)))
(setf (renderstyle UICheckBoxOff) 'Renderunstretched)
(new imageRenderer :objectName "UICheckBoxOn" :project ui)
(setf (backgroundRenderer UICheckBoxOn) uimiddle)
(setf (media UICheckBoxOn) (new iconrsrc :project ui :resourceId 8002 :file (file ui)))
(setf (renderstyle UICheckBoxOn) 'Renderunstretched)
(new imageRenderer :objectName "UICheckBoxIn" :project ui)
(setf (backgroundRenderer UICheckBoxIn) uimiddle)
(setf (media UICheckBoxIn) (new iconrsrc :project ui :resourceId 8003 :file (file ui)))
(setf (renderstyle UICheckBoxIn) 'Renderunstretched)
(new imageRenderer :objectName "UICheckBoxDisabled" :project ui)
(setf (backgroundRenderer UICheckBoxDisabled) uimiddle)
(setf (media UICheckBoxDisabled) (new iconrsrc :project ui :resourceId 8004 :file (file ui)))
(setf (renderstyle UICheckBoxDisabled) 'Renderunstretched)


(new checkbox :objectname "UICheckBox" :project ui)
(addproperty UICheckBox 'enabled)
(setf (container (car (contents UICheckBox))) nil)
(new rectangle :objectname "UICheckBoxCheck" :project ui)
(setf (container UICheckBoxCheck) UICheckBox)
(setf (textFont UICheckBox) EspySansFont)
(setf (textSize UICheckBox) 9)
(setf (fillcolor UICheckBox) uimiddle)

(define-handler AddedMeAsParent (UICheckBox child oldparents)
  (withActorLocked (me)
    (let ((xx (text (car (contents child)))))
      (setf (container (car (contents child))) nil)
      (new UICheckBoxCheck :container child :project ui :text xx)
      (setf (fillcolor child) (fillcolor me))
      (setf (framesize child) (framesize me))
      (copytextproperties me uiCheckBox)
      (setf (enabled child) t)
      (forceRedraw child))))

(define-handler resized (UICheckBox)
  (if (not (string= (text me) ""))
    (setBoundsRect (car (contents me)) 1 1 14 14)
    (setBoundsRect (car (contents me)) 0 0 13 13)))

(define-handler (setf highlight) (boolean UICheckBox)
  (when (enabled me)
    (withactorlocked (me)
      (call-next-method)
      (setFramesize (car (contents me)) 0 0)
      (if boolean
        (setf (fillcolor (car (contents me))) UICheckBoxIn)
        (unless (up mouse)
          (setf (fillcolor (car (contents me))) (if (checked me)
                                             UICheckBoxOn
                                             UICheckBoxOff
                                             )))))))

(define-handler (setf checked) (boolean UICheckBox &key (update t))
  (when (enabled me)
    (withactorlocked (me)
      (call-next-method)
      (setf (fillcolor (car (contents me))) (if boolean
                                         UICheckBoxOn
                                         UICheckBoxOff
                                         ))
      boolean)))

(define-handler (setf enabled) (boolean UICheckBox)
  (withactorlocked (me)
    (sk8::setvalue 'enabled me boolean)
    (if boolean
      (progn
        (setf (fillcolor (car (contents me))) (if (checked me)
                                           UICheckBoxOn
                                           UICheckBoxOff
                                           ))
        (setf (textcolor me) black))
      (progn
        (setf (fillcolor (car (contents me))) UICheckBoxDisabled)
        (setf (textcolor me) gray))
      )))

(setf (enabled uicheckbox) t)
(setf (highlight uicheckbox) nil)


;;;;;__________________________________________________________________________________________________


(new imageRenderer :objectName "UIRadioButtonOff" :project ui)
(setf (backgroundRenderer UIRadioButtonOff) uimiddle)
(setf (media UIRadioButtonOff) (new iconrsrc :project ui :resourceId 9001 :file (file ui)))
(setf (renderstyle UIRadioButtonOff) 'Renderunstretched)
(new imageRenderer :objectName "UIRadioButtonOn" :project ui)
(setf (backgroundRenderer UIRadioButtonOn) uimiddle)
(setf (media UIRadioButtonOn) (new iconrsrc :project ui :resourceId 9002 :file (file ui)))
(setf (renderstyle UIRadioButtonOn) 'Renderunstretched)
(new imageRenderer :objectName "UIRadioButtonIn" :project ui)
(setf (backgroundRenderer UIRadioButtonIn) uimiddle)
(setf (media UIRadioButtonIn) (new iconrsrc :project ui :resourceId 9003 :file (file ui)))
(setf (renderstyle UIRadioButtonIn) 'Renderunstretched)
(new imageRenderer :objectName "UIRadioButtonDisabled" :project ui)
(setf (backgroundRenderer UIRadioButtonDisabled) uimiddle)
(setf (media UIRadioButtonDisabled) (new iconrsrc :project ui :resourceId 9004 :file (file ui)))
(setf (renderstyle UIRadioButtonDisabled) 'Renderunstretched)


(new RadioButton :objectname "UIRadioButton" :project ui)
(addproperty UIRadioButton 'enabled)
(addproperty UIRadioButton 'checked)
(setf (textFont UIRadioButton) EspySansFont)
(setf (textSize UIRadioButton) 9)
(hide (car (contents (car (contents UIRadioButton)))))
(setf (fillcolor UIRadioButton) uimiddle)

(define-handler (setf highlight) (boolean UIRadioButton)
  (when (enabled me)
    (withactorlocked (me)
      (call-next-method)
      (setframesize (car (contents me)) 0 0)
      (if boolean
        (setf (fillcolor (car (contents me))) UIRadioButtonIn)
        (unless (up mouse)
          (setf (fillcolor (car (contents me))) (if (checked me)
                                             UIRadioButtonOn
                                             UIRadioButtonOff
                                             )))
        )
      )))

(define-handler (setf checked) (boolean UIRadioButton)
  (declare (ignore-if-unused boolean))
  (when (enabled me)
    (call-next-method)))

(define-handler checked (UIRadioButton)
  (sk8::getvalue 'checked me))


(define-handler toggleState (UIRadioButton)
  (withactorlocked (me)
    (sk8::setvalue 'checked me (not (checked me)))
    (setf (fillcolor (car (contents me))) (if (checked me)
                                       UIRadioButtonOn
                                       UIRadioButtonOff
                                       ))
    ))

(define-handler (setf enabled) (boolean UIRadioButton)
  (withactorlocked (me)
    (sk8::setvalue 'enabled me boolean)
    (if boolean
      (progn
        (setf (fillcolor (car (contents me))) (if (checked me)
                                           UIRadioButtonOn
                                           UIRadioButtonOff
                                           ))
        (setf (textcolor me) black))
      (progn
        (setf (fillcolor (car (contents me))) UIRadioButtonDisabled)
        (setf (textcolor me) gray))
      )))

(setf (enabled uiRadioButton) t)
(setf (highlight uiRadioButton) nil)





#|

(define-handler (setf Highlight) (boolean uiZoomBox)
  (if boolean
    (setf (fillColor me) uihiliteBoxRenderer)
    (setf (fillColor me) uiZoomBoxRenderer)))

(define-handler Highlight (uiZoomBox)
  (eq (fillColor me) uihiliteBoxRenderer))

(define-handler click (uiZoomBox)
  (let ((sq (squashed me))
        (tla (sk8::window me)))
    (if sq 
      (progn
        (setf (squashed me) nil)
        (setf (minimumsize tla) (second sq))
        (SK8-multival-bind (left top right bottom) (boundsRect tla :physical t)
          (setf (boundsRect tla) (list left top right (+ top (first sq))))))
      (let ((offset 20))
        (setf (squashed me) (list (height tla) (minimumsize tla)))
        (when (neq (sk8::window me) projectoverviewer)
          (dolist (i (contents tla)) (if (Is-A i uiMenuBarActor) (setf offset 40))))
        (setf (minimumsize tla) (list (car (minimumsize tla)) offset))
        (SK8-multival-bind (left top right bottom) (boundsRect tla :physical t)
          (setf (boundsRect tla) (list left top right (+ top offset))))
        (dolist (i (contents tla)) (if (Is-A i uiResizeBox ) (setLocation i 0 100))))
      )))
|#


;;____________________________________________
;; menubar

(new SK8::Menubar :objectName "uiMenuBarActor" :project ui)

(setf (fillcolor uimenubaractor) transparent)
(setframesize uimenubaractor 0 0)

(define-handler commandkeyevent (uiMenubarActor thechar)
  (if (or (eq me ProjectBuilderMenubar) (sk8::checkMenuBarForCmdKey me theChar))
    ;; This call-next-method also calls checkMenuBarForCmdKey, which causes the work to be done twice
    ;; (especially the 'update' for all the menus!)
    (call-next-method)
    (commandkeyevent ProjectBuilderMenubar thechar)))

(define-handler bestSize (uiMenubaractor)
  (declare (special hSize vSize))
  (setBoundsRect me *BarLeft* *WindowTop* hSize (+ 20 *WindowTop*)))

;;___________________________________________
;; menu

(new menu :objectName "uiMenuActor" :project ui)

(setf (inverts uiMenuActor) nil)
(setf (menufillcolor uimenuactor) uilightestgray)

(define-handler (setf Highlight) (boolean uiMenuActor)
  ;; Calling next method records the state as highlighted (or not).
  (call-next-method)
  (if boolean
    (withactorlocked (me)
      (setf (fillcolor me) black)
      (setf (textcolor me) white))
    (withactorlocked (me)
      (setf (fillColor me) UIMenuColor)
      (setf (textcolor me) black)))
  )

;;; This one makes sure the uiMenuActors do not get inverted on highlight.

(define-handler addedMeAsParent (uiMenuActor child oldparents)
  (unless (some #'(lambda (x) (Is-A x uiMenuActor)) oldparents)
    (call-next-method)
    (setf (inverts child) nil)))

(define-handler menuEnter (uiMenuActor)
  (let ((curBoundsRect (boundsrect me))
        (ok nil))
    (unwind-protect 
      (progn
        (setf (highlight me) t)
        (lock me)
        (setboundsrect me 0 0 0 -3 :relative t)
        (call-next-method)
        (setf (boundsrect me) curBoundsRect)
        (setf (highlight me) nil)
        (unlock me)
        (setf ok t))
      (unless ok
        (setf (boundsrect me) curBoundsRect)
        (setf (highlight me) nil)
        (unlock me)))
    ))

;;---------------------------------------------------------------------------------------------------------
;; splitters

(new imageRenderer :objectName "UIBigSplitterColor1" :project ui)
(setf (backgroundRenderer UIBigSplitterColor1) nil)
(setf (media UIBigSplitterColor1) (new iconrsrc :project ui :resourceId 12001 :file (file ui)))
(new imageRenderer :objectName "UIBigSplitterColor2" :project ui)
(setf (backgroundRenderer UIBigSplitterColor2) nil)
(setf (media UIBigSplitterColor2) (new iconrsrc :project ui :resourceId 12002 :file (file ui)))
(new imageRenderer :objectName "UIBigSplitterColor3" :project ui)
(setf (backgroundRenderer UIBigSplitterColor3) nil)
(setf (media UIBigSplitterColor3) (new iconrsrc :project ui :resourceId 12003 :file (file ui)))
(setf (renderstyle UIBigSplitterColor1) 'Renderunstretched)
(setf (renderstyle UIBigSplitterColor2) 'Rendertiled)
(setf (renderstyle UIBigSplitterColor3) 'Renderunstretched)

(new UIRenderer :objectName "UIBigSplitterColor" :project ui)

(define-handler render (UIBigSplitterColor theActor region thePaper)
  (if *MacStyleInterface*
    (progn
      (render graytone90 theactor region thePaper)
      (render simpleframe theactor region thePaper))
    (progn
      (gs:recompute-fill-region theActor (gs:node-flags theactor))
      (gs:let+ (fill-rect-left fill-rect-top fill-rect-right fill-rect-bottom
                            (fill-rgn (gs:node-fillRegion theActor))
                            (fill-rect (gs:hrref fill-rgn :region.rgnbbox :storage :pointer))
                            (temprect (:rect))
                            (temprgn (:region))
                            )
        (setq fill-rect-left (rref fill-rect :rect.left))
        (setq fill-rect-top (rref fill-rect :rect.top))
        (setq fill-rect-right (rref fill-rect :rect.right))
        (if (< fill-rect-left 0) (decf fill-rect-right fill-rect-left))
        (setq fill-rect-bottom (rref fill-rect :rect.bottom))
        (if (< fill-rect-top 0) (decf fill-rect-bottom fill-rect-top))
        
        (set-qd-rect temprect fill-rect-left fill-rect-top fill-rect-right fill-rect-bottom)
        (#_rectrgn temprgn temprect)
        (render UIBigSplitterColor2 theActor temprgn thePaper)
        (set-qd-rect temprect fill-rect-left fill-rect-top (+ fill-rect-left 16) fill-rect-bottom)
        (#_rectrgn temprgn temprect)
        (render UIBigSplitterColor1 theActor temprgn thePaper)
        (set-qd-rect temprect (- fill-rect-right 32) fill-rect-top (- fill-rect-right 0) fill-rect-bottom)
        (#_rectrgn temprgn temprect)
        (render UIBigSplitterColor3 theActor temprgn thePaper)
        
        (#_penNormal)
        (#_penSize 1 1)))))

(new splitter :objectName "uiSplitter" :project ui)
(setf (textfont uiSplitter) EspySansBoldFont)
(setf (textsize uiSplitter) 9)
(setf (texthoffset uiSplitter) 5)
(setf (textlocation uiSplitter) 'centerleft)
(setf (fillcolor uiSplitter) UIBigSplitterColor)
(setf (framesize uiSplitter) '(0 0))
(define-handler mousedown (uiSplitter)
  (sk8-multival-bind (ll tt rr bb) (boundsRect (sk8::window me) :physical t)
    (drag me :live nil :constrainingrect (list (+ ll 5) (+ tt (absolutetop me))  (- rr 5) (+ tt (absoluteBottom me))))))

(new imageRenderer :objectName "UILittleSplitterColor1" :project ui)
(setf (backgroundRenderer UILittleSplitterColor1) nil)
(setf (media UILittleSplitterColor1) (new iconrsrc :project ui :resourceId 13001 :file (file ui)))
(new imageRenderer :objectName "UILittleSplitterColor2" :project ui)
(setf (backgroundRenderer UILittleSplitterColor2) nil)
(setf (media UILittleSplitterColor2) (new iconrsrc :project ui :resourceId 13002 :file (file ui)))
(new imageRenderer :objectName "UILittleSplitterColor3" :project ui)
(setf (backgroundRenderer UILittleSplitterColor3) nil)
(setf (media UILittleSplitterColor3) (new iconrsrc :project ui :resourceId 13003 :file (file ui)))
(setf (renderstyle UILittleSplitterColor1) 'Renderunstretched)
(setf (renderstyle UILittleSplitterColor2) 'Rendertiled)
(setf (renderstyle UILittleSplitterColor3) 'Renderunstretched)

(new UIRenderer :objectName "UILittleSplitterColor" :project ui)

(define-handler render (UILittleSplitterColor theActor region thePaper)
  (if *MacStyleInterface*
    (progn
      (render graytone90 theactor region thePaper)
      (render simpleframe theactor region thePaper))
    (progn
      (gs:recompute-fill-region theActor (gs:node-flags theactor))
      (gs:let+ (fill-rect-left fill-rect-top fill-rect-right fill-rect-bottom
                            (fill-rgn (gs:node-fillRegion theActor))
                            (fill-rect (gs:hrref fill-rgn :region.rgnbbox :storage :pointer))
                            (temprect (:rect))
                            (temprgn (:region))
                            )
        (setq fill-rect-left (rref fill-rect :rect.left))
        (setq fill-rect-top (rref fill-rect :rect.top))
        (setq fill-rect-right (rref fill-rect :rect.right))
        (if (< fill-rect-left 0) (decf fill-rect-right fill-rect-left))
        (setq fill-rect-bottom (rref fill-rect :rect.bottom))
        (if (< fill-rect-top 0) (decf fill-rect-bottom fill-rect-top))
        
        (set-qd-rect temprect fill-rect-left fill-rect-top fill-rect-right fill-rect-bottom)
        (#_rectrgn temprgn temprect)
        (render UILittleSplitterColor2 theActor temprgn thePaper)
        (set-qd-rect temprect fill-rect-left fill-rect-top (+ fill-rect-left 16) fill-rect-bottom)
        (#_rectrgn temprgn temprect)
        (render UILittleSplitterColor1 theActor temprgn thePaper)
        (set-qd-rect temprect (- fill-rect-right 32) fill-rect-top (- fill-rect-right 0) fill-rect-bottom)
        (#_rectrgn temprgn temprect)
        (render UILittleSplitterColor3 theActor temprgn thePaper)
        
        (#_penNormal)
        (#_penSize 1 1)))))

(new uisplitter :objectName "uiLittleSplitter" :project ui)
(setf (fillcolor uiLittleSplitter) UILittleSplitterColor)

(define-handler addedMeAsParent (uisplitter Child OldParents)
  (declare (ignore oldparents))
  (let ((tt (text child)))
    (copyTextProperties child uisplitter)
    (setf (fillcolor child) (fillcolor me))
    (call-next-method)
    (setf (text child) tt)
    ))

;;---------------------------------------------------------------------------------------------------------
;; Text List

(new textlist :objectname "UITextList" :project ui)
(setf (textfont (titlebar UITextList)) EspySansBoldFont)
(setf (textsize (titlebar UITextList)) 9)
(setf (texthoffset (titlebar UITextList)) 5)
(setf (textlocation (titlebar UITextList)) 'centerleft)
(setf (fillcolor (titlebar UITextList)) uimiddle)
(setf (framesize (titlebar UITextList)) '(0 0))
(setf (fillcolor (picker UITextList)) shadowWhite)
(setf (textfont (picker UITextList)) defaultUIFont)
(setf (textsize (picker UITextList)) defaultUIFontSize)
(setf (framecolor (picker UITextList)) black)
(setf (framesize UITextList) '(0 0))

(new rectangle :objectname "UTLInsetRect" :project ui)
(setf (container UTLInsetRect) UITextList)
(setf (framesize UTLInsetRect) '(0 0))
(tagpart UITextList UTLInsetRect 'insetRect)
(setf (fillcolor (insetRect UITextList)) shadowedrenderer)

(new uiVerticalScroller :objectname "UITextListScroller" :project ui)

(setf (container (vscroller UITextList)) nil)
(setf (container UITextListScroller) UITextList)
(setf (vscroller UITextList) UITextListScroller)
(setf (partnervscroller (picker UITextList)) UITextListScroller)

(define-handler resized (UITextList)
  (let ((theTitlebar (titleBar me))
        hsize vsize)
    (SK8-multival-setf (hSize vSize) (size me))
    (setBoundsRect (titlebar me) 0 0 (- hsize 0) 17)
    (setBoundsRect (vscroller me) (- hSize (if *MacStyleInterface* 16 13)) 
                   (if (visible theTitlebar)
                     17
                     0) hSize vsize)
    (setBoundsRect (insetRect me) 0 (if (visible theTitlebar)
                                      17
                                      0)
                   (- hSize (if *MacStyleInterface* 15 12)) (- vsize 0))
    (setBoundsRect (picker me) 
                   (if *MacStyleInterface* 1 4) 
                   (if (visible theTitlebar)
                     (if *MacStyleInterface* 18 21)
                     (if *MacStyleInterface* 1 4))
                   (- hSize (if *MacStyleInterface* 16 12)) 
                   (- vsize 1))
    ))

(sendtoback UTLInsetRect)
(resized uitextlist)

(define-handler doubleclick ((picker UITextList))
  (let ((si (selecteditems me)))
    (setf si (delete-if #'stringp si))
    (when si
      (uiedit si))
    ))
(define-handler targetProject ((picker UITextList)) (targetproject ui))
(define-handler (setf Highlight) (theval (picker UITextList))
  (setf (inverts me) nil)
  (if theval
    (setf (fillcolor me) white)
    (setf (fillcolor me) shadowwhite)
    )
  (call-next-method))

(define-handler (setf pickerPrototype) (pickerproto UITextList)
  (when (or (Is-A pickerproto editText)
            (Is-A pickerproto picker)
            (Is-A pickerproto tablepicker))
    (withactorlocked (me)
      ;; Unhook and remove the existing picker...
      (setf (partnervScroller (picker me)) nil)
      (setf (container (picker me)) nil)
      ;; Add and hook up a new picker...
      (setf (picker me) (new pickerProto :project (project me)))
      (if (inheritsFrom (picker me) EditText)
        (setf (fillcolor (picker me)) shadowWhite)
        (setf (fillcolor (picker me)) shadowedrenderer))
      (setf (container (picker me)) me)
      (resized me)
      (setf (partnervScroller (picker me)) (vScroller me))
      (when (is-a pickerproto browsercomponent)
        (eval `(define-handler targetProject (,(picker me)) (targetproject ui)))
        (eval `(define-handler (setf Highlight) (theval ,(picker me))
                 (setf (inverts me) nil)
                 (if theval
                   (setf (fillcolor me) white)
                   (setf (fillcolor me) shadowwhite)
                   )
                 (call-next-method)))
        )
      )))

(new styledpicker :objectname "UITextListPicker" :project ui)
(addparent UITextListPicker MixinForObjectPickers)
(setf (autotab UITextListPicker) t)
(setf (alphabeticalDisplay UITextListPicker) nil)
(setf (selectionStyle UITextListPicker) 'discontiguous)

(setf (pickerprototype UITextList) UITextListPicker)

(define-handler makeBoundsRegion (UITextList)
  (gs:let+ ((physRect (gs:recompute-physicalBoundsRect me))
            (ll (gs:rect-left physRect))
            (tt (gs:rect-top physRect))
            (rr (gs:rect-right physRect))
            (bb (gs:rect-bottom physRect))
            (r (:rect))
            (temp (:region))
            (bounds (gs:node-boundsRegion me))
            (flags (gs:node-flags me)))
    ;; Rect 1.
    (set-qd-rect r ll tt rr bb)
    (#_rectRgn bounds r)
    ;; Rect 2.
    (when (visible (titlebar me))
      (set-qd-rect r ll tt (+ ll 1)  (+ tt 17))
      (#_rectRgn temp r)
      (#_Diffrgn bounds temp bounds))
    ;; Done.
    (gs:boundsDirty! flags 0)))

;;;;;;;;;----------Now a UITextList for corners...

(new uitextlist :objectname "UITextListForCorners" :project ui)
(define-handler resized (UITextListForCorners)
  (let (hsize vsize)
    (SK8-multival-setf (hSize vSize) (size me))
    (setBoundsRect (titlebar me) 0 0 (- hsize 0) 17)
    (setBoundsRect (vscroller me) (- hSize (if *MacStyleInterface* 16 13)) 
                   (if (visible (titlebar me))
                     17
                     0) 
                   hSize 
                   (- vSize (if *MacStyleInterface* 16 12)))
    (setBoundsRect (insetRect me) 
                   0 
                   (if (visible (titlebar me))
                     17
                     0)
                   (- hSize (if *MacStyleInterface* 15 12))
                   (- vsize 0))
    (setBoundsRect (picker me) 
                   (if *MacStyleInterface* 1 4)
                   (if (visible (titlebar me))
                     (if *MacStyleInterface* 18 21)
                     (if *MacStyleInterface* 1 4))
                   (- hSize (if (inheritsFrom (picker me) EditText)
                              ;; EditText should not overlap the scroller. 
                              16
                              (if *MacStyleInterface* 16 12)))
                   (- vsize 1))
    ))

(define-handler makeBoundsRegion (UITextListForCorners)
  (gs:let+ ((physRect (gs:recompute-physicalBoundsRect me))
            (ll (gs:rect-left physRect))
            (tt (gs:rect-top physRect))
            (rr (gs:rect-right physRect))
            (bb (gs:rect-bottom physRect))
            (r (:rect))
            (temp (:region))
            (bounds (gs:node-boundsRegion me))
            (flags (gs:node-flags me)))
    ;; Rect 1.
    (set-qd-rect r ll tt (- rr (if *MacStyleInterface* 16 14)) bb)
    (#_rectRgn bounds r)
    ;; Rect 2.
    (set-qd-rect r ll tt rr (- bb (if *MacStyleInterface* 16 12)))
    (#_rectRgn temp r)
    (#_unionRgn bounds temp bounds)
    (when (visible (titlebar me))
      (set-qd-rect r ll tt (+ ll 1)  (+ tt 17))
      (#_rectRgn temp r)
      (#_Diffrgn bounds temp bounds))
    ;; Done.
    (gs:boundsDirty! flags 0)))


#|

(new uisimplewindow :container stage :objectname "hahaha" :project ui)
(setf (resizer hahaha) t)
(new UITextList :container hahaha :objectname "bing" :project ui)
(resized bing)
(setf (fillcolor (insetrect bing)) shadowedrenderer)
(setf (fillcolor (picker bing)) shadowwhite)
(sendtoback (insetrect bing))
(setf (pickerprototype bing) propertysheet)
(select bing)
(uicolorize bing)
(uicolorize (picker bing))
|#


;;---------------------------------------------------------------------------------------------------------
;; Labels

(new label :objectName "uiBigLabel" :project ui)
(setf (textlocation uiBigLabel) 'centerleft)
(setf (texthoffset uiBigLabel) 5) 
(setf (textfont uiBigLabel) EspySansBoldFont) 
(setf (textstyle uiBigLabel) '(plain))
(setf (textcolor uiBigLabel) black)
(setf (framesize uiBigLabel) '(0 0))
(setf (textsize uiBigLabel) 9)
(setf (fillcolor uibiglabel) uimiddle)
(setf (mousesensitivity uibiglabel) 'transparent)

(new uiBigLabel :objectName "uiSmallLabel" :project ui)
(setf (textsize uiSmallLabel) 9) 



;;---------------------------------------------------------------------------------------------------------
;;---------------------------------------------------------------------------------------------------------
;; UICOLORIZE for global setup

(define-handler uiColorize (UISimpleWindow)
  (setf (windowstyle me) 'blank) ;; HEW
  (setf (dofirstclick me) t)
  (setf (textsize me) (textsize uisimplewindow))
  (setf (textlocation me) (textlocation uisimplewindow))
  (setf (textfont me) EspySansBoldFont)
  (setf (textSize me) 10)
  (setf (textstyle me) '(Plain))
  (setf (texthoffset me) 20)
  (setf (textVoffset me) (if (sk8::menubar me) 1 0))
  (setf (fillcolor me) UIWindowColor)
  (setf (framesize me) '(0 0)))
(define-handler uiColorize (SK8::Menubar)
  (mapcar #'uicolorize (menus me))
  (when (container me) (bringToFront me))
  (setf (framesize me) '(0 0))
  (setf (fillColor me) transparent))
(define-handler uiColorize (menu)
  (setf (framesize me) '(0 0))
  (setf (textsize me) 9)
  (setf (textvoffset me) -2)
  (setf (textcolor me) black)
  (setf (textfont me) EspySansFont)
  (setf (fillColor me) UIMenuColor))
(define-handler uiColorize (tablepicker)
  (setf (gs:node-drawfunction me) 'ScoredFastTablePicker-draw)
  (setf (fillcolor me) shadowwhite)
  (setf (highlightcolor me) uilightcolor)
  )
(define-handler uiColorize (colorpicker)
  ;;(setf (highlightcolor me) uilightcolor)
  )
(define-handler uicolorize (twobynbrowserpicker)
  (call-next-method)
  (setf (rowspacing me) 0)
  (setframesize me 0 0)
  )
(define-handler uicolorize (valueeditorpicker)
  (call-next-method)
  (setf (rowspacing me) 0)
  (setframesize me 0 0)
  )
(define-handler uiColorize (textlist)
  (setf (framesize (picker me)) '(0 0))
  (setf (textcolor (titlebar me)) black)
  )
(define-handler uiColorize (picker)
  (setf (fillcolor me) shadowwhite)
  (setf (highlightcolor me) uilightcolor)
  (if (is-a (container me) contentsviewer)
    (setf (linespacing me) 6)
    (setf (linespacing me) 0))
  (setf (gs:node-drawfunction me) 'UIPicker-Draw)
  )
(define-handler uiColorize (multilinepicker)
  (setf (fillcolor me) shadowwhite)
  (setf (highlightcolor me) uilightcolor)
  (setf (linespacing me) 6)
  (setf (gs:node-drawfunction me) 'uimultilinepicker-draw)
)

(define-handler uiColorize (edittext)
  (setf (fillcolor me) shadowwhite)
  (setf (highlightcolor me) uilightcolor)
  )
(define-handler uiColorize (dialogboxbutton)
  (setf (textsize me) (textsize dialogboxbutton))
  (setf (textfont me) (textfont dialogboxbutton))
  (setf (text me) (text me))
  )

(define-handler uiColorize (uilittlesplitter)
  (setf (fillcolor me) (fillcolor uilittlesplitter))
  )

;; uicolorize of SimpleRendererEditor was defined here, but had to move it because the
;;  object is not yet defined in the build process

(define-handler uicolorize (ui)
  (dolist (i (objects ui))
    (when (is-a i actor) 
      (unless (or (eq (sk8::window i) HaloPreferencesDialog)
                  (eq (container i) WindowPreferencesDialog)
                  (eq (container i) mbResourceChooser)
                  (eq (textfont i) EspySansBoldFont))
        (setf (textfont i) EspySansFont))
      (if (or 
           (Is-A i UISimpleWindow)
           (Is-A i picker)
           (Is-A i edittext)
           (Is-A i tablepicker)
           (Is-A i dialogboxbutton)
           (Is-A i SimpleRendererEditor)
           (is-a i uilittlesplitter)
           (Is-A i textlist)
           (Is-A i sk8::menubar))
        (uicolorize i)))
    (if (is-a i browserpaper)
      (hide (highlighterhalo i)))
    )
  (setf (fillcolor projectbuildermenubar) lightgray)
  (setf (highlightcolor drawpalettetools) transparent)
  (setf (textfont MEMenuTitle) ChicagoFont)
  (setf (textfont MEMenuProxy) ChicagoFont)
  (dolist (i (sk8::windows ui))
    (resized i))
  )

#|
  (dolist (i (objects ui))
      (if (or 
           (Is-A i UISimpleWindow)
           (Is-A i picker)
           (Is-A i edittext)
           (Is-A i tablepicker)
           (Is-A i dialogboxbutton)
           (Is-A i SimpleRendererEditor)
           (is-a i uilittlesplitter)
           (Is-A i textlist)
           (Is-A i sk8::menubar))
        (uicolorize i)))
(uicolorize ui)
|#


;;;;__________________________________________________________
(new uitextlist :objectname "UIHOL" :project ui)
(setf (pickerPrototype UIHOL) HierarchicalObjectPicker)
(setf (alphabeticalDisplay (picker UIHOL)) t)
(setf (swatchesshown (picker UIHOL)) t)
(setf (iconsize (picker UIHOL)) '(30 30))
(setf (text (picker UIHOL)) " ")
(setf (objectname (picker UIHOL)) "UIHOLPicker")
(define-handler GenerateItems (UIHOLPicker ItemList)
  (logoutswatches me (mapcar #'swatch (items me)))
  (let (newlist 
        (SwatchesShown (SwatchesShown me))
        (relation (relation me))
        (p (project me)))
    (dolist (i ItemList)
      (push (new hierarchicalpickeritem 
                 :project p 
                 :value i
                 :state (if (and relation (memq relation (properties i)) 
                                 (funcall relation i))
                          'closed 
                          nil)
                 :swatch (if swatchesshown (generateswatch i me) nil)) 
            newlist)
      )
    (nreverse newlist)))
(define-handler targetProject (UIHOLPicker) (targetproject ui))
(define-handler (setf Highlight) (theval UIHOLPicker)
  (setf (inverts me) nil)
  (if theval
    (setf (fillcolor me) white)
    (setf (fillcolor me) shadowwhite)
    )
  (call-next-method))


;;;-------------------------------------------------------------------------------------------------------
(new tablepicker :objectname "SwatchPicker" :project ui)
(addparent SwatchPicker  mixinForObjectPickers)
(setf (textfont SwatchPicker) "Geneva")
(setf (textsize SwatchPicker) 9)

(define-handler targetProject (SwatchPicker) (targetproject ui))

(define-handler (setf inputobjects)  (theobjs SwatchPicker)
  (sk8dev::withLockedCursor animatedClock
    (let ((its (imagearray me)))
      (call-next-method (generateitems me theobjs) me)
      (logoutimages me its)
      )))

(define-handler generateitems (SwatchPicker theobjs)
  theobjs)

(define-handler logoutimages (SwatchPicker theImages)
  (let ((columns (car (array-dimensions theImages)))
         (rows (cadr (array-dimensions theImages)))
         )
    (dotimes (r rows)
      (dotimes (c columns)
        (logoutswatches me (aref theImages c r))))
    ))

(define-handler SetupImages (SwatchPicker)
  (let ((theImages (imageArray me))
        (theItems (items me)))
    (sk8::setValue 'imageArray me (new array :dimensions (dimensions me)))
    (setf theImages (imageArray me))
    (dotimes (r (rows me))
      (dotimes (c (columns me))
        (setf (aref theImages c r)
              (if theitems
                (generateswatch (aref theItems c r) me)
                ""))))
    theImages))

(new ColorPicker :otherparents swatchpicker :objectname "SwatchColorPicker" :project ui)

(setf (HighlightSelection SwatchColorPicker) nil)

;;;-----Now the standard widget for the object editor and handler editor....
(new queryfield :objectname "StandardQueryField" :project ui)
(setf (framecolor (TextField StandardQueryField)) darkgray)
(setf (framesize (TextField StandardQueryField)) '(0 0))
(setf (framesize StandardQueryField) '(0 0))
(setf (fillcolor StandardQueryField) uimiddle)
(setf (mousesensitivity StandardQueryField) 'transparent)
(define-handler targetProject ((textfield StandardQueryField)) (targetproject ui))
(define-handler targetProject (StandardQueryField) (targetproject ui))

(new shadowedrenderer :objectname "shadowedrendererwithedges" :project ui) 
(setf (renderedges shadowedrendererwithedges) t)


(define-handler (setf Highlight) (theval StandardQueryField)
  (setf (inverts me) nil)
  (if theval
    (progn
      (setselection (textfield me) 0 -1)
      (setf (fillcolor (textfield me)) white)
      (activatetext (textfield me)))
    (progn
      (setf (fillcolor (textfield me)) shadowwhite)
      (deactivatetext (textfield me)))
    )
  (call-next-method))

(addparent (historymenu StandardQueryField) uipopup)
(setf (enabled (historymenu StandardQueryField)) t)
(setf (menuType (historymenu StandardQueryField)) 'popup)

(new colorPicker :objectname "SIFHandle" :project ui) 
(setf (container SIFHandle) StandardQueryField)
(setf (framesize SIFHandle) '(1 1))
(setf (text SIFHandle) " ") ;;; so the swatches don't show text!!!
(setf (HighlightSelection SIFHandle) nil)
(setf (HighlightColor SIFHandle) transparent)
(setf (framecolor SIFHandle) framescore)
(setf (fillcolor SIFHandle) shadowwhite)
(setf (tablehoffset SIFHandle) 1)
(setf (tablevoffset SIFHandle) 1)
(tagpart StandardQueryField SIFHandle 'Handle)

(new rectangle :objectname "SIFInsetrect" :project ui)
(setframesize SIFInsetrect 0 0)
(setf (container SIFInsetrect) StandardQueryField)
(sendtoback SIFInsetrect)
(setf (fillcolor SIFInsetrect) shadowedrendererwithedges)
(tagpart StandardQueryField SIFInsetrect 'insetrect)

(define-handler (setf OutputObjects) (theval StandardQueryField)
  (when (visible (handle me))
    (withactorlocked (me)
      (let* ((Hand (Handle me)))
        (sk8dev::map-items Hand #'(lambda (i c r) (declare (ignore c r)) (logoutswatches hand i))
                           nil nil)
        (setf (items Hand) (mapcar #'(lambda (x) (generateswatch x Hand)) theval))
        (doaswatch))))  ;;; A little bit of a hack...
  (call-next-method)
  )

(define-handler mouseDown (SIFHandle)
  (let ((sels (OutputObjects (container me))))
    (when sels
      (if (and (listp sels) (eql (length sels) 1)) (setf sels (car sels)))
      (setf (boundsrect ObjectDataRect :physical t) (boundsrect (container me) :physical t))
      (setf (object ObjectDataRect) sels)
      (setf (componentfrom ObjectDataRect) (container me))
      (withcursor standardcursor
        (drag ObjectDataRect :live nil :onstage t :draggingMouseWithinEvents t :draggedOverEvents t :dropevents t))
      )))

(define-handler resized (StandardQueryField)
  (call-next-method)
  (if (eq (KeyTarget (sk8::window me)) (textfield me))
    (browserHighlight me))
  (sk8-multival-bind (hh vv) (size me)
    (setBoundsRect (Handle me) 0 0 vv vv)
    (setBoundsRect (Insetrect me) (if (visible (handle me)) vv 0) 0 (if (visible (historymenu me)) (- hh 20) hh) vv)
    (setBoundsRect (TextField me) 
                   (if (visible (handle me)) (+ vv (if *MacStyleInterface* 1 4)) (if *MacStyleInterface* 1 4))
                   (if *MacStyleInterface* 1 4)
                   (if (visible (historymenu me)) (- hh 20 (if *MacStyleInterface* 1 2)) (- hh (if *MacStyleInterface* 1 2)))
                   (- vv (if *MacStyleInterface* 1 2)))
    (setBoundsRect (HistoryMenu me) (- hh 20) 0 hh 18)))


(define-handler (setf InputObjects) (curInput StandardQueryField)
  (sk8dev::withLockedCursor animatedClock
    (if curinput
      (progn
        (tickeventclock)
        (setf (outputobjects me) (if (listp curInput) 
                                   curInput
                                   (list curInput)))
        (tickeventclock)
        (addItem (historymenu me) (if (and (listp curInput) (= (length curinput) 1))
                                   (car curInput)
                                   curInput))
        )
      (progn
        (setf (text (textfield me)) "")
        (setf (outputobjects me) nil))
      )))



(new StandardQueryField :objectname "StandardInputField" :project ui)
(addproperty (textfield StandardInputField) 'querystring)
(define-handler deactivateText ((textfield StandardInputField))
  (let ((oo (outputobjects (container me))))
     (cond 
      ;((not oo)
      ; (setf (text me) ""))
      (oo
       (setf (text me) (if (every #'objectname oo)
                         (objectstring (if (= (length oo) 1) (car oo) oo) :project (targetproject ui))
                         (querystring me))))
      )
     (call-next-method)
     (BrowserUnHighlight (container me))))
(define-handler DoBookKeeping (StandardInputField &key (warnings nil) (history t))
  (call-next-method me :warnings warnings :history nil)
  (let ((oo (OutputObjects me)))
    (cond 
     ((not oo)
      (setf (text (textfield me)) ""))
     ((and oo  (every #'objectname oo))
      (setf (text (textfield me)) (objectstring (if (= (length oo) 1) (car oo) oo) :project (targetproject ui))))
     )
    (setf (querystring (textfield me)) (text (textfield me)))
    (forceredraw (textfield me)) ;;;*** tell adam 
    (when (and history oo)
      (addItem (historymenu me) (if (eql 1 (length oo)) (car oo) oo)))
    oo))
(define-handler (setf InputObjects) (curInput StandardInputField)
  (when curinput
    (setf (text (textfield me)) 
          (objectstring (if (= (length curInput) 1) (car curInput) curInput) :project (targetproject ui)))
    )
  (call-next-method)
  (setf (querystring (textfield me)) (text (textfield me)))
  )






#|
(new swatchpicker :objectname "test" :project ui)
(setf (container test) stage)
(setf (inputobjects test) (children actor))
(setf (inputobjects test) (contents stage))
(setf (inputobjects test) nil)
|#


#|
	Change History (most recent last):
	2	5/24/93	Hernan	Changed menus to conform to new menus!
	3	5/24/93	Hernan	Among other things, fixed menuEnter of 
				uiMenuActor to conform to the new menus.
	4	5/25/93	Hernan	The computeSize method of menuBarActor now
				gives us the menubar look we have grown to love.
	5	5/28/93	Hernan	Changed the way menus deal with colors.
	6	5/28/93	hernan	uiMenuActor's menufillcolor and menutextcolor 
				are initialized to graytone40 and white.
	18	10/8/93	hernan	Removed references to menuname.
	19	10/8/93	hernan	Removed menuEnter and makeFitToText of 
				uimenuActor to use the default handlers. Also
				got rid of special computeSize for the uimenubar.
	22	10/25/93	rod	Added uicolorize checkbox
	41	1/14/94	rod	removed cursors...
	51	2/21/94	hernan	The Final Renaming for Alpha! (yeah, right...)
	52	2/21/94	hernan	windows -> sk8::windows.
	53	2/21/94	hernan	window -> sk8::window.
	57	2/25/94	hernan	Using symbols instead of keywords for options!!!
	58	2/26/94	rod	hilighter->highlighter
	60	2/28/94	rod	Now defines targetProject on all the 
				appropriate browsercomponents in the ui
				via uicolorize
	61	3/2/94	Hernan	Porting to Fred 3.0
	63	3/4/94	rod	addparent avoided where possible
	64	3/4/94	Brian	Put in addParent again as it stops in the build
	66	3/9/94	Hernan	alphabetical -> alphabeticalDisplay
	67	3/9/94	rod	Doing Project Switching and Reference Clearing.
	69	3/11/94	rod	Fixed swatch bugs.
	70	3/15/94	rod	Fixing Drag and Drop
	72	3/17/94	rod	zoom of projectoverviewer needs a special case.
	73	3/21/94	Hernan	Fixing uiColorize of menu to set the menuTextColor
				to nil since the textColor property is enough (the
				textcolor and the menuTextColor are the same).
	74	3/21/94	kleiman	setvalue -> sk8::setValue
	75	3/26/94	rod	Minimum size stuff.
	76	3/30/94	rod	WatchCursor stuff.
	77	3/31/94	rod	
	78	3/31/94	Hernan	Making the uiMenuActor highlight itself properly.
	79	3/31/94	Hernan	Making uiColorize set the inverts of the menus to
				false. Wrote addedMeAsParent to set the inverts
				to false when something becomes a uimenu.
	80	4/1/94	chip	took out extraneous stuff from uiMenubarActor's commandKeyEvent
	81	4/1/94	rod	
	82	4/11/94	rod	Removing reference to simple window.  No longer
				needed in the redesign of the UI.
	83	4/11/94	rod	made error-handling a bit safer
	89	4/12/94	rod	Adding a selection color.  By default it is just the
				highlightrenderer
	98	4/13/94	Hernan	Avoiding use of contents when possible.
	100	4/13/94	rod	
	101	4/13/94	sidney	Add parameters to addedMeAsParent and removingMeAsParent to allow handlers to avoid work when an ancestor is added back in after a remove
	102	4/15/94	rod	
	103	4/15/94	rod	Fixing HighlightColor for all pickers...
	105	4/18/94	Hernan	Redefining (setf checked) to call next method
				when the UIRadiobutton is enabled.
	106	4/18/94	rod	
	107	4/18/94	rod	Fixing targetproject bug.
	108	4/18/94	rod	
	109	4/20/94	rod	
	110	4/20/94	rod	
	111	4/22/94	Hernan	I do not remember what I did.
	121	5/6/94	rod	
	124	5/26/94	rod	
	125	5/26/94	rod	
	126	6/1/94	rod	New layout
	127	6/1/94	rod	
	128	6/1/94	Brian	
	129	6/3/94	rod	
	130	6/3/94	rod	
	131	6/13/94	rod	Fixed some property initialization problems in 
				addedMeAsParent for UI buttons and checkboxes.
	132	6/14/94	rod	Fixing bug with menus.  Making sure that their
				autohighlight is always set to false.
	133	6/14/94	rod	Fixing scroller thumbs to accurately follow the
				original design.
	134	6/14/94	rod	Oops, fixing typo.
	135	6/14/94	rod	Fixing clipping problems with scroller thumbs.
	136	6/14/94	rod	Making ui labels have be mouse transparent.
	137	6/15/94	rod	1168599
	138	6/17/94	rod	Allowing drag and drop to work on Global Variables.
	139	6/23/94	rod	
	140	6/29/94	rod	Highlighting and Horizontal Scroller fixes.
	142	7/11/94	rod
	143	7/13/94	rod	Getting rid of UIColorize.
	144	7/17/94	rod	
	145	7/17/94	rod	Fixing standard textlist.
	146	7/17/94	rod	Momentary insanity.
	147	7/18/94	rod	Fixing a targetproject problem.
	148	7/18/94	rod	1165082
	149	 8/23/94	rod     	
	150	 9/ 1/94	rod     	Fixing fonts.
	151	10/ 3/94	Hernan  	Adding the paper argument to render. (The paper
							is the Port at this point).
	152	10/ 4/94	rod     	Lazy Swatch stuff...
	153	10/10/94	rod     	Fixing keyword args...
	154	10/13/94	chip    	(setf pickerPrototype) now unhooks the scroller from the old picker
	155	10/19/94	rod     	Graphic fixes.
	156	10/20/94	rod     	Fixing color underline in shadow renderer.
	157	10/20/94	rod     	Removing print statement
	158	10/20/94	rod     	
	159	10/26/94	rod     	
	160	11/ 1/94	rod     	adjusting textlistforcorners for new ppats.
	161	12/ 5/94	sidney  	move definition of uicolor of SimpleRendererEditor to after the object is created, so this can build
	162	 2/ 3/95	Hernan  	WithLockedCursor is now correct. I check this file
							out and back in to force it to be recompiled.
	163	 2/ 3/95	Hernan  	get-record-field -> rref or hrref.
	164	 2/16/95	sidney  	readable argument names for initialize handler
	165	 2/17/95	Hernan  	EditText cannot be rendered with non RGBColors.
							Thus we need to change set pickerPrototype to
							not set the color to shadowedRenderer when the
							picker descends from EditText.
	166	 4/19/95	rod     	Fixing MacStyleInterface.
	2  	 8/14/95	Brian   	
	3  	12/ 8/95	Brian   	
	4  	12/11/95	Brian   	
	5  	 2/14/96	Brian   	changing font vars.
	2  	 4/10/96	Hernan  	define-system-handler->define-handler.
	4  	 4/16/96	brian   	removing (nthitem call.
	5  	 5/ 8/96	Hernan  	Fixed resized of uiTextListForCorners so that the picker and
						the scroller do not overlap.
	6  	 5/ 9/96	Hernan  	Keeping the change I Just did for EditTexts only.
	7  	 5/23/96	Brian   	
	8  	 8/ 8/96	Hernan  	body -> scrollerBody.
	9  	10/11/96	Brian   	removing crossauthorable.
	10 	10/14/96	Brian   	Fixing that annoying menu resize problem.
	11 	 2/27/97	Hernan  	
|# ;(do not edit past this line!!)
