;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories


;;;

(in-package :UIDevelopment)


;;_________________________________________________________________
;; SELECTION
;; We have grow and shrink by rectangle, and grow and shrink by lasso.

;; SelectRectGrow
(new IconRSRC :objectName "uiCiconSelectRectGrow" :project ui
          :ResourceID 100 :file (file ui))

(new imageRenderer :objectName "uiColorSelectRectGrow" :project ui
          :media uiCiconSelectRectGrow)

;; SelectRectShrink
(new IconRSRC :objectName "uiCiconSelectRectShrink" :project ui
          :ResourceID 101 :file (file ui))

(new imageRenderer :objectName "uiColorSelectRectShrink"  :project ui
          :media uiCiconSelectRectShrink)

;; SelectLassoGrow
(new IconRSRC :objectName "uiCiconSelectLassoGrow"  :project ui
          :ResourceID 102 :file (file ui))

(new imageRenderer :objectName "uiColorSelectLassoGrow"  :project ui
          :media uiCiconSelectLassoGrow)

;; SelectLassoShrink
(new IconRSRC :objectName "uiCiconSelectLassoShrink"  :project ui
          :ResourceID 103 :file (file ui))

(new imageRenderer :objectName "uiColorSelectLassoShrink"  :project ui
          :media uiCiconSelectLassoShrink)

;;_________________________________________________________________
;; RECT
;; Three versions:  CornerToCorner, CenterToCorner, SideToSide

;; Corner to Corner
(new IconRSRC :objectName "uiCiconRectCornertoCorner"  :project ui
          :ResourceID 1300 :file (file ui))

(new imageRenderer :objectName "uiColorRectCornerToCorner"  :project ui
          :media uiCiconRectCornerToCorner)

;; Center to Corner
(new IconRSRC :objectName "uiCiconRectCentertoCorner"  :project ui
          :ResourceID 1301 :file (file ui))

(new imageRenderer :objectName "uiColorRectCenterToCorner"  :project ui
          :media uiCiconRectCenterToCorner)

;;_________________________________________________________________
;; OVAL
;; Four versions:  CornerToCorner, CenterToCorner, CenterRadius, 3Points

;; corner to corner
(new IconRSRC :objectName "uiCiconOvalCornerToCorner"  :project ui
          :ResourceID 1400 :file (file ui))

(new imageRenderer :objectName "uiColorOvalCornerToCorner"  :project ui
          :media uiCiconOvalCornerToCorner)

;; center to corner
(new IconRSRC :objectName "uiCiconOvalCenterToCorner"  :project ui
          :ResourceID 1401 :file (file ui))

(new imageRenderer :objectName "uiColorOvalCenterToCorner"  :project ui
          :media uiCiconOvalCenterToCorner)



;;_________________________________________________________________
;; ROUND RECT
;; Three versions:  CornerToCorner, CenterToCorner, SideSideRadius

;; Corner to Corner
(new IconRSRC :objectName "uiCiconRRectCornertoCorner"  :project ui
          :ResourceID 1800 :file (file ui))

(new imageRenderer :objectName "uiColorRRectCornertoCorner"  :project ui
          :media uiCiconRRectCornertoCorner)

;; Center to Corner
(new IconRSRC :objectName "uiCiconRRectCentertoCorner"  :project ui
          :ResourceID 1801 :file (file ui))

(new imageRenderer :objectName "uiColorRRectCentertoCorner"  :project ui
          :media uiCiconRRectCentertoCorner)


;;_________________________________________________________________
;; LINE
;; four versions

;; simple line
(new IconRSRC :objectName "uiCiconSimpleLine"  :project ui
     :ResourceID 1500 :file (file ui))

(new imageRenderer :objectName "uiColorSimpleLine"  :project ui
     :media uiCiconSimpleLine)

;; connector line
(new IconRSRC :objectName "uiCiconConnectorLine"  :project ui
     :ResourceID 1502 :file (file ui))

(new imageRenderer :objectName "uiColorConnectorLine"  :project ui
     :media uiCiconConnectorLine)

;; arrowEnd line
(new IconRSRC :objectName "uiCiconArrowEndLine"  :project ui
     :ResourceID 1501 :file (file ui))

(new imageRenderer :objectName "uiColorArrowEndLine"  :project ui
     :media uiCiconArrowEndLine)

#|
;; arrows line
(new IconRSRC :objectName "uiCiconArrowsLine"  :project ui
     :ResourceID 1503 :file (file ui))

(new imageRenderer :objectName "uiColorArrowsLine"  :project ui
     :media uiCiconArrowsLine)
|#

;;_________________________________________________________________
;; POLYGON
;; Three versions:  points, spline, freehand

;; polygon
(new IconRSRC :objectName "uiCiconPolygon"  :project ui
          :ResourceID 1600 :file (file ui))

(new imageRenderer :objectName "uiColorPolygon"  :project ui
          :media uiCiconPolygon)

;; Regular Icon
(new IconRSRC :objectName "uiCiconRegularPolygon"  :project ui
          :ResourceID 1601 :file (file ui))

(new imageRenderer :objectName "uiColorRegularPolygon"  :project ui
          :media uiCiconRegularPolygon)

;; Freehand
(new IconRSRC :objectName "uiCiconFreehand"  :project ui
          :ResourceID 1602 :file (file ui))

(new imageRenderer :objectName "uiColorFreehand"  :project ui
          :media uiCiconFreehand)

;; uiColorSymegon
(new IconRSRC :objectName "uiCiconSymegon"  :project ui
     :ResourceID 1603 :file (file ui))

(new imageRenderer :objectName "uiColorSymegon"  :project ui
     :media uiCiconSymegon)


;;_________________________________________________________________
;; PLUG TOOL
;; One version

(new IconRSRC :objectName "uiCiconPlugTool"  :project ui
          :ResourceID 1900 :file (file ui))

(new imageRenderer :objectName "uiColorPlugTool"  :project ui
          :media uiCiconPlugTool)


;;_________________________________________________________________
;; PLUG
;; Two versions - plugged and unplugged.

(new IconRSRC :objectName "uiCiconPortPlugged"  :project ui
          :ResourceID 2201 :file (file ui))

(new imageRenderer :objectName "uiColorPortPlugged"  :project ui
          :media uiCiconPortPlugged)

(new IconRSRC :objectName "uiCiconPortUnPlugged"  :project ui
          :ResourceID 2200 :file (file ui))

(new imageRenderer :objectName "uiColorPortUnPlugged"  :project ui
          :media uiCiconPortUnPlugged)


;;_________________________________________________________________

;; Close box
(new IconRSRC :objectName "uiCloseBoxCICN" :project ui
          :ResourceID 136 :file (file ui))

(new imageRenderer :objectName "uiCloseBoxRenderer"  :project ui)
(setf (media uiCloseBoxRenderer) uiCloseBoxCICN)     

;; ZoomBox
(new IconRSRC :objectName "uiZoomBoxCICN" :project ui
          :ResourceID 137 :file (file ui))

(new imageRenderer :objectName "uiZoomBoxRenderer"  :project ui)
(setf (media uiZoomBoxRenderer) uiZoomBoxCICN)

;; Hilite 
(new IconRSRC :objectName "uiHiliteBoxCICN" :project ui
          :ResourceID 138 :file (file ui))

(new imageRenderer :objectName "uiHiliteBoxRenderer"  :project ui)
(setf (media uiHiliteBoxRenderer) uiHiliteBoxCICN)



;;_________________________________________________________________
;; Halo colors

(new complexRGBColor :objectName "DarkWarningColor" :project ui
     :foreRed 40000 
     :foreGreen 40000 
     :foreBlue 0 
     :backRed 100 
     :backGreen 100 
     :backBlue 100)

(setf (Media DarkWarningColor) bwPattern43)

(new complexRGBColor :objectName "WarningColor" :project ui
     :foreRed 65535 
     :foreGreen 65535 
     :foreBlue 0 
     :backRed 0 
     :backGreen 0 
     :backBlue 0)

(setf (Media WarningColor) bwPattern43)



#|
	Change History (most recent last):
	1		10/1/93	rod	
	2		10/13/93	hernan	Redrawing the renderer hierarchy.
	3		10/13/93	rod	prepared for the new renderers (i hope)
	4		10/13/93	hernan	quickdrawColor -> imageRenderer.
	5		10/15/93	rod	
	6		10/15/93	hernan	complexImageRenderer -> complexPatternRenderer.
	7		10/15/93	rod	
	8		10/15/93	rod	
	9		11/1/93	hernan	macpathname -> file.
	10		12/22/93	rod	
	11		2/21/94	hernan	The Final Renaming for Alpha! (yeah, right...)
	12		3/30/94	rod	
	13		3/30/94	rod	moving ArrowIcons to sk8
	14		4/11/94	Brian	
	15 	10/ 6/94	It      	got rid of obsolete :tiled arg in creation of uiScrollerThumbRenderer
	16 	 2/16/95	rod     	Removing unused media.
	17 	 2/16/95	rod     	replacing a resource.
	18 	 4/11/95	rod     	Removing old bogus renderer
	2  	 2/27/97	Hernan  	
|# ;(do not edit past this line!!)
