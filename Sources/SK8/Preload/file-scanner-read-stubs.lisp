;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories


;;;

(in-package :CCL)

;;; These things need to be defined for the source-file scanner because they're referenced while reading the sources


;;; The globals...
;;;
(dolist (globalVar '(
                     SK8::LinearSequence
                     SK8::LinePath
                     SK8::DefaultPathClock
                     SK8::Yellow
                     SK8::Blue
                     SK8::Black
                     SK8::Gray
                     SK8::White
                     SK8::Lightgray
                     SK8::Darkgray
                     SK8::Muchdarker
                     SK8::Translucent
                     SK8::Greenlines
                     SK8::BlackPatternRSRC
                     SK8::GrayPatternRSRC
                     SK8::TbDisplayItem
                     SK8::Goldmarblepict
                     SK8::Granitepict
                     SK8::Pinkmarblepict
                     SK8::Darkoakpict
                     SK8::Goldoakpict
                     SK8::Red
                     SK8::Lightred
                     SK8::FinderClosedArrowCIcon
                     SK8::FinderInBetweenArrowCIcon
                     SK8::FinderOpenArrowCIcon
                     UI::UI
                     UI::uiScrollerThumbPICT
                     UI::uiCiconHand
                     UI::uiCiconSelectRectGrow
                     UI::uiCiconSelectRectShrink
                     UI::uiCiconSelectLassoGrow
                     UI::uiCiconSelectLassoShrink
                     UI::uiCiconTrace
                     UI::uiCiconPin
                     UI::uiCiconLineGroove
                     UI::uiCiconPolyGroove
                     UI::uiCiconAutoGroove
                     UI::uiCiconSocket
                     UI::uiCiconGrid
                     UI::uiCiconGlue
                     UI::uiCiconBox
                     UI::uiCiconSpring
                     UI::uiCiconReplicator
                     UI::uiCiconViewer
                     UI::uiCiconDataField
                     UI::uiCiconRectCornerToCorner
                     UI::uiCiconRectCenterToCorner
                     UI::uiCiconRectSideToSide
                     UI::uiCiconOvalCornerToCorner
                     UI::uiCiconOvalCenterToCorner
                     UI::uiCiconOvalCenterRadius
                     UI::uiCiconOval3Points
                     UI::uiCiconRRectCornertoCorner
                     UI::uiCiconRRectCentertoCorner
                     UI::uiCiconRRectSideSideRadius
                     UI::uiCiconSimpleLine
                     UI::uiCiconConnectorLine
                     UI::uiCiconArrowEndLine
                     UI::uiCiconArrowsLine
                     UI::uiCiconPolygon
                     UI::uiCiconRegularPolygon
                     UI::uiCiconFreehand
                     UI::uiCiconSymegon
                     UI::uiCiconEditTextCornerToCorner
                     UI::uiCiconEditTextCenterToCorner
                     UI::uiCiconTextField
                     UI::uiCiconLabel
                     UI::uiCiconPlugTool
                     UI::uiCiconTagTool
                     UI::uiCiconTextTool
                     UI::uiCiconNameTool
                     UI::uiCiconAliasTool
                     UI::uiCiconPortPlugged
                     UI::uiCiconPortUnPlugged
                     ))
  (unless (boundp globalVar) (setf (symbol-value globalVar) nil)))


;;; The functions...
;;;
(dolist (functionName '(
                        SK8::now
                        SK8::namestring
                        SK8::truename
                        UI::file
                        ))
  (unless (fboundp functionName) (setf (symbol-function functionName) #'false)))

#|
	Change History (most recent last):
	1	7/28/93	chip	new file
	2	8/27/93	chip	
	3	8/30/93	chip	
	4	9/23/93	kleiman	
	5	10/29/93	hernan	
	6	11/1/93	kleiman	added more color stubs
	7	3/30/94	rod	Moving arrow icons to sk8
	2  	 2/27/97	Hernan  	
|# ;(do not edit past this line!!)
