(in-package :SK8Development)

;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories




;; STANDARD RESOURCES
;; These are the standard resources that SK8 provides.

#| Modification History

01-22-93 ruben id -> objectName

|#

;;;_________________________________________________________________
;;; Cursors!

(new cursorRSRC :objectname "CursorVSplitter" :resourceId 141 :project sk8)
(new cursorRSRC :objectname "CursorHSplitter" :resourceId 140 :project sk8)

(new cursorRSRC :objectname "CursorX" :resourceId 700 :project sk8)
(new cursorRSRC :objectname "CursorMenu" :resourceId 800 :project sk8)
(new cursorRSRC :objectname "CursorPointing" :resourceId 150 :project sk8)
(new cursorRSRC :objectname "CursorPressing" :resourceId 151 :project sk8)
(new cursorRSRC :objectname "CursorOpenHand" :resourceId 152 :project sk8)
(new cursorRSRC :objectname "CursorCloseHand" :resourceId 153 :project sk8)
(new cursorRSRC :objectname "CursorSelectRect" :resourceId 200 :project sk8)
(new cursorRSRC :objectname "CursorSelectLasso" :resourceId 300 :project sk8)
(new cursorRSRC :objectname "CursorSelectLassoDeep" :resourceId 301 :project sk8)
(new cursorRSRC :objectname "CursorPencil" :resourceId 400 :project sk8)
(new cursorRSRC :objectname "CursorCrosshair" :resourceId 500 :project sk8)
(new cursorRSRC :objectname "CursorCrosshairDeep" :resourceId 501 :project sk8)
(new cursorRSRC :objectname "CursorSelectionCrosshair" :resourceId 502 :project sk8)
(new cursorRSRC :objectname "CursorTextString" :resourceId 1200 :project sk8)
(new cursorRSRC :objectname "CursorHelp" :resourceId 1201 :project sk8)

;;;_________________________________________________________________
;;; ICONS
;;; These are the standard Icons.

(new IconRSRC :objectname "TrashEmptyIcon" :project sk8)
(setf (resourceId trashEmptyicon) 15100)

(new ImageRenderer :objectname "TrashEmptyRenderer" :project sk8)
(setf (Media trashEmptyRenderer) trashEmptyicon)


(new IconRSRC :objectname "TrashFullIcon" :project sk8)
(setf (resourceId trashFullicon) 15101)

(new imageRenderer :objectname "TrashFullRenderer" :project sk8)
(setf (Media trashFullRenderer) trashFullicon)

;;; The Scroller arrow renderers.

(new IconRSRC :objectname "ScrollerUpIcon" :project sk8)
(setf (resourceId scrollerUpicon) 110)
(new imageRenderer :objectname "ScrollerUpRenderer" :project sk8)
(setf (Media scrollerUpRenderer) scrollerUpicon)
(setf (backgroundRenderer scrollerUpRenderer) nil)

(new IconRSRC :objectname "ScrollerDownIcon" :project sk8)
(setf (resourceId scrollerDownIcon) 111)
(new imageRenderer :objectname "ScrollerDownRenderer" :project sk8)
(setf (Media scrollerDownRenderer) scrollerDownIcon)
(setf (backgroundRenderer scrollerDownRenderer) nil)

(new IconRSRC :objectname "ScrollerLeftIcon" :project sk8)
(setf (resourceId scrollerLeftIcon) 112)
(new imageRenderer :objectname "ScrollerLeftRenderer" :project sk8)
(setf (Media scrollerLeftRenderer) scrollerLeftIcon)
(setf (backgroundRenderer scrollerLeftRenderer) nil)

(new IconRSRC :objectname "ScrollerRightIcon" :project sk8)
(setf (resourceId scrollerRightIcon) 113)
(new imageRenderer :objectname "ScrollerRightRenderer" :project sk8)
(setf (Media scrollerRightRenderer) scrollerRightIcon)
(setf (backgroundRenderer scrollerRightRenderer) nil)

;;; Same guys when pressed...

(new IconRSRC :objectname "ScrollerUpPressedIcon" :project sk8)
(setf (resourceId ScrollerUpPressedIcon) 15130)
(new imageRenderer :objectname "scrollerUpPressedRenderer" :project sk8)
(setf (Media scrollerUpPressedRenderer) ScrollerUpPressedIcon)
(setf (backgroundRenderer scrollerUpPressedRenderer) nil)

(new IconRSRC :objectname "ScrollerDownPressedIcon" :project sk8)
(setf (resourceId ScrollerDownPressedIcon) 15131)
(new imageRenderer :objectname "ScrollerDownPressedRenderer" :project sk8)
(setf (Media scrollerdownpressedrenderer) ScrollerDownPressedIcon)
(setf (backgroundRenderer scrollerDownPressedRenderer) nil)

(new IconRSRC :objectname "ScrollerLeftPressedIcon" :project sk8)
(setf (resourceId ScrollerLeftPressedIcon) 15132) ;; ???
(new imageRenderer :objectname "ScrollerLeftPressedRenderer" :project sk8)
(setf (Media scrollerLeftPressedRenderer) ScrollerLeftPressedIcon)
(setf (backgroundRenderer scrollerLeftPressedRenderer) nil)

(new IconRSRC :objectname "ScrollerRightPressedIcon" :project sk8)
(setf (resourceId ScrollerRightPressedIcon) 15133) ;; ???
(new imageRenderer :objectname "ScrollerRightPressedRenderer" :project sk8)
(setf (Media scrollerRightPressedRenderer) ScrollerRightPressedIcon)
(setf (backgroundRenderer scrollerRightPressedRenderer) nil)

(new IconRSRC :objectname "ScrollerHThumbIcon" :project sk8)
(setf (resourceId scrollerHThumbIcon) 114)
(new imageRenderer :objectname "ScrollerHThumbRenderer" :project sk8)
(setf (Media scrollerHThumbRenderer) scrollerHThumbIcon)
(setf (backgroundRenderer scrollerHThumbRenderer) nil)

(new IconRSRC :objectname "ScrollerVThumbIcon" :project sk8)
(setf (resourceId scrollerVThumbIcon) 115)
(new imageRenderer :objectname "ScrollerVThumbRenderer" :project sk8)
(setf (Media scrollerVThumbRenderer) scrollerVThumbIcon)
(setf (backgroundRenderer scrollerVThumbRenderer) nil)

(new IconRSRC :objectname "CloseBoxIcon" :project sk8)
(setf (resourceId closeBoxIcon) 120)
(new imageRenderer :objectname "CloseBoxRenderer" :project sk8)
(setf (Media closeBoxRenderer) closeBoxIcon)
(setf (backgroundRenderer CloseBoxRenderer) nil)

(new IconRSRC :objectname "ZoomBoxIcon" :project sk8)
(setf (resourceId zoomBoxIcon) 121)
(new imageRenderer :objectname "ZoomBoxRenderer" :project sk8)
(setf (Media zoomBoxRenderer) zoomBoxIcon)
(setf (backgroundRenderer zoomBoxRenderer) nil)

(new IconRSRC :objectname "ResizeBoxIcon" :project sk8)
(setf (resourceId resizeBoxIcon) 122)
(new imageRenderer :objectname "ResizeBoxRenderer" :project sk8)
(setf (Media resizeBoxRenderer) resizeBoxIcon)
(setf (backgroundRenderer resizeBoxRenderer) nil)

(new IconRSRC :objectname "HighlightIcon" :project sk8)
(setf (resourceId HighlightIcon) 123)
(new imageRenderer :objectname "HighlightRenderer" :project sk8)
(setf (Media HighlightRenderer) HighlightIcon)
(setf (backgroundRenderer highlightRenderer) nil)

(new IconRSRC :objectname "MenuBoxIcon" :project sk8)
(setf (resourceId menuBoxIcon) 124)
(new imageRenderer :objectname "MenuBoxRenderer" :project sk8)
(setf (Media menuBoxRenderer) menuBoxIcon)
(setf (backgroundRenderer MenuBoxRenderer) nil)

;;; Making the file dialog's icons. Now in the SK8 project File!  And now in the application file once a standalone is built!

(let ((path (or (file sk8) SK8ApplicationFile))
      theRSRC)
  (mapc #'(lambda (aName anId)
            (setf theRSRC (new iconRSRC :objectName (string-trim '(#\space) (concatenate 'string aName "RSRC"))
                               :project sk8
                               :file path :resourceId anId))
            (new imageRenderer :objectname (concatenate 'string aName "Renderer")
                 :project sk8 :media theRSRC :renderStyle 'RenderUnstretched))
        '("Document" "Folder" "Floppy" "OpenFolder" "Application" "HardDisk" "GrayFolder"
          "Trash" "Desktop" "FileServer")
        '(15300 301 302 303 304 305 306 307 308 309)))

;;; More file dialog icons: resource types!

(let ((path (or (file sk8) SK8ApplicationFile))
      theRSRC)
  (mapc #'(lambda (aName anId)
            (setf theRSRC (new iconRSRC :project sk8 :file path :resourceId anId))
            (new imageRenderer :objectname (concatenate 'string aName "Renderer")
                 :project sk8 :media theRSRC :renderStyle 'Renderunstretched))
        '("cicn" "curs" "ppat" "pict" "snd" "pat")
        '(310 311 312 313 314 315)))

(new colorPattern :objectname "ScrollerPattern" :project sk8)
(setf (resourceId scrollerPattern) 1000)
(new imageRenderer :objectname "ScrollerBodyColor" :project sk8)
(setf (media scrollerBodyColor) scrollerPattern)
(setf (backgroundRenderer ScrollerBodyColor) nil)

(new colorPattern :objectname "DeskTopPattern" :project sk8)
(setf (resourceId deskTopPattern) 16)

;;; Returns a copy of the deskTopPattern since we do not want to dispose the real thing.
;;; (It is used by the window manager and crashes the whole system when it is gone.)

(define-handler loadMedia (DeskTopPattern)
  (let ((handle (mediaData me)))
    (if (and (handlep handle) (not (ccl::%null-ptr-p handle)))
      handle
      (let ((oldpat (#_getPixPat 16))
            (newHandle (T_NewPixpatGC)))
        (#_copyPixPat oldPat newHandle)
        (setf (mediaData me) newHandle)
        newHandle))))

(new imageRenderer :objectname "DeskTopColor" :project sk8)
(setf (media deskTopColor) deskTopPattern)

(let ((resfile (or (file sk8) SK8ApplicationFile)))
  (new BWPattern :objectname "BWPATTERN1" :project sk8 :file resfile :resourceid 129) 
  (new BWPattern :objectname "BWPATTERN2" :project sk8 :file resfile :resourceid 130) 
  (new BWPattern :objectname "BWPATTERN3" :project sk8 :file resfile :resourceid 131) 
  (new BWPattern :objectname "BWPATTERN4" :project sk8 :file resfile :resourceid 132) 
  (new BWPattern :objectname "BWPATTERN5" :project sk8 :file resfile :resourceid 133) 
  (new BWPattern :objectname "BWPATTERN6" :project sk8 :file resfile :resourceid 134) 
  (new BWPattern :objectname "BWPATTERN7" :project sk8 :file resfile :resourceid 135) 
  (new BWPattern :objectname "BWPATTERN8" :project sk8 :file resfile :resourceid 136) 
  (new BWPattern :objectname "BWPATTERN9" :project sk8 :file resfile :resourceid 137) 
  (new BWPattern :objectname "BWPATTERN10" :project sk8 :file resfile :resourceid 138) 
  (new BWPattern :objectname "BWPATTERN11" :project sk8 :file resfile :resourceid 139) 
  (new BWPattern :objectname "BWPATTERN12" :project sk8 :file resfile :resourceid 140) 
  (new BWPattern :objectname "BWPATTERN13" :project sk8 :file resfile :resourceid 141) 
  (new BWPattern :objectname "BWPATTERN14" :project sk8 :file resfile :resourceid 142) 
  (new BWPattern :objectname "BWPATTERN15" :project sk8 :file resfile :resourceid 143) 
  (new BWPattern :objectname "BWPATTERN16" :project sk8 :file resfile :resourceid 144) 
  (new BWPattern :objectname "BWPATTERN17" :project sk8 :file resfile :resourceid 145) 
  (new BWPattern :objectname "BWPATTERN18" :project sk8 :file resfile :resourceid 146) 
  (new BWPattern :objectname "BWPATTERN19" :project sk8 :file resfile :resourceid 147) 
  (new BWPattern :objectname "BWPATTERN20" :project sk8 :file resfile :resourceid 148) 
  (new BWPattern :objectname "BWPATTERN21" :project sk8 :file resfile :resourceid 149) 
  (new BWPattern :objectname "BWPATTERN22" :project sk8 :file resfile :resourceid 150) 
  (new BWPattern :objectname "BWPATTERN23" :project sk8 :file resfile :resourceid 151) 
  (new BWPattern :objectname "BWPATTERN24" :project sk8 :file resfile :resourceid 152) 
  (new BWPattern :objectname "BWPATTERN25" :project sk8 :file resfile :resourceid 153) 
  (new BWPattern :objectname "BWPATTERN26" :project sk8 :file resfile :resourceid 154) 
  (new BWPattern :objectname "BWPATTERN27" :project sk8 :file resfile :resourceid 155) 
  (new BWPattern :objectname "BWPATTERN28" :project sk8 :file resfile :resourceid 156) 
  (new BWPattern :objectname "BWPATTERN29" :project sk8 :file resfile :resourceid 157) 
  (new BWPattern :objectname "BWPATTERN30" :project sk8 :file resfile :resourceid 158) 
  (new BWPattern :objectname "BWPATTERN31" :project sk8 :file resfile :resourceid 159) 
  (new BWPattern :objectname "BWPATTERN32" :project sk8 :file resfile :resourceid 160) 
  (new BWPattern :objectname "BWPATTERN33" :project sk8 :file resfile :resourceid 161) 
  (new BWPattern :objectname "BWPATTERN34" :project sk8 :file resfile :resourceid 162) 
  (new BWPattern :objectname "BWPATTERN35" :project sk8 :file resfile :resourceid 163) 
  (new BWPattern :objectname "BWPATTERN36" :project sk8 :file resfile :resourceid 164) 
  (new BWPattern :objectname "BWPATTERN37" :project sk8 :file resfile :resourceid 165) 
  (new BWPattern :objectname "BWPATTERN38" :project sk8 :file resfile :resourceid 166) 
  (new BWPattern :objectname "BWPATTERN39" :project sk8 :file resfile :resourceid 167) 
  (new BWPattern :objectname "BWPATTERN40" :project sk8 :file resfile :resourceid 168) 
  (new BWPattern :objectname "BWPATTERN41" :project sk8 :file resfile :resourceid 169) 
  (new BWPattern :objectname "BWPATTERN42" :project sk8 :file resfile :resourceid 170) 
  (new BWPattern :objectname "BWPATTERN43" :project sk8 :file resfile :resourceid 171) 
  (new BWPattern :objectname "BWPATTERN44" :project sk8 :file resfile :resourceid 172)
  )

;; resources
(new ColorPattern :objectName "LightBoardRSRC" :project SK8 :resourceId 2000)
(new ColorPattern :objectName "SmallPurpleWeaveRSRC" :project SK8 :resourceId 9001)
(new ColorPattern :objectName "CrumpledPaperRSRC" :project SK8 :resourceId 9002)
(new ColorPattern :objectName "FloorTileRSRC" :project SK8 :resourceId 3000)
(new ColorPattern :objectName "PurpleMarbleRSRC" :project SK8 :resourceId 4000)
(new ColorPattern :objectName "NonSkidMetalRSRC" :project SK8 :resourceId 5000)
(new ColorPattern :objectName "GrayGridRSRC" :project SK8 :resourceId 6000)
(new ColorPattern :objectName "WoodFloorRSRC" :project SK8 :resourceId 7000)
(new ColorPattern :objectName "MuddyRSRC" :project SK8 :resourceId 8000)
(new ColorPattern :objectName "RancidRSRC" :project SK8 :resourceId 8001)
(new ColorPattern :objectName "BluePlaidRSRC" :project SK8 :resourceId 8002)
(new ColorPattern :objectName "WelcomeMatRSRC" :project SK8 :resourceId 8003)
(new ColorPattern :objectName "GraySquaresRSRC" :project SK8 :resourceId 8004)
(new ColorPattern :objectName "BlueCrossesRSRC" :project SK8 :resourceId 8005)
(new ColorPattern :objectName "GraySplotchRSRC" :project SK8 :resourceId 8006)
(new ColorPattern :objectName "RedCrossesRSRC" :project SK8 :resourceId 8007)
(new ColorPattern :objectName "GrayMistRSRC" :project SK8 :resourceId 8008)
(new ColorPattern :objectName "WalnutSwirlsRSRC" :project SK8 :resourceId 8009)
(new ColorPattern :objectName "WalnutWavesRSRC" :project SK8 :resourceId 8010)
(new ColorPattern :objectName "SmoothedGrayBumpsRSRC" :project SK8 :resourceId 8011)
(new ColorPattern :objectName "GrayBumpsRSRC" :project SK8 :resourceId 8012)
(new ColorPattern :objectName "GrayCragsRSRC" :project SK8 :resourceId 8013)
(new ColorPattern :objectName "GreenCragsRSRC" :project SK8 :resourceId 8014)
(new ColorPattern :objectName "BlueSquaresRSRC" :project SK8 :resourceId 8015)
(new ColorPattern :objectName "GreenLinesRSRC" :project SK8 :resourceId 8016)
(new ColorPattern :objectName "WhiteGridRSRC" :project SK8 :resourceId 8017)
(new ColorPattern :objectName "WhiteDesktopPatternRSRC" :project SK8 :resourceId 9000)

;; colors

(new ImageRenderer :objectName "LightBoard" :project SK8 :backgroundRenderer nil)
(setf (Media LightBoard) LightBoardRSRC)
(new ImageRenderer :objectName "SmallPurpleWeave" :project SK8 :backgroundRenderer nil)
(setf (Media SmallPurpleWeave) SmallPurpleWeaveRSRC)
(new ImageRenderer :objectName "CrumpledPaper" :project SK8 :backgroundRenderer nil)
(setf (Media CrumpledPaper) CrumpledPaperRSRC)
(new ImageRenderer :objectName "FloorTile" :project SK8 :backgroundRenderer nil)
(setf (Media FloorTile) FloorTileRSRC)
(new ImageRenderer :objectName "PurpleMarble" :project SK8 :backgroundRenderer nil)
(setf (Media PurpleMarble) PurpleMarbleRSRC)
(new ImageRenderer :objectName "NonSkidMetal" :project SK8 :backgroundRenderer nil)
(setf (Media NonSkidMetal) NonSkidMetalRSRC)
(new ImageRenderer :objectName "GrayGrid" :project SK8 :backgroundRenderer nil)
(setf (Media GrayGrid) GrayGridRSRC)
(new ImageRenderer :objectName "WoodFloor" :project SK8 :backgroundRenderer nil)
(setf (Media WoodFloor) WoodFloorRSRC)
(new ImageRenderer :objectName "Muddy" :project SK8 :backgroundRenderer nil)
(setf (Media Muddy) MuddyRSRC)
(new ImageRenderer :objectName "Rancid" :project SK8 :backgroundRenderer nil)
(setf (Media Rancid) RancidRSRC)
(new ImageRenderer :objectName "BluePlaid" :project SK8 :backgroundRenderer nil)
(setf (Media BluePlaid) BluePlaidRSRC)
(new ImageRenderer :objectName "WelcomeMat" :project SK8 :backgroundRenderer nil)
(setf (Media WelcomeMat) WelcomeMatRSRC)
(new ImageRenderer :objectName "GraySquares" :project SK8 :backgroundRenderer nil)
(setf (Media GraySquares) GraySquaresRSRC)
(new ImageRenderer :objectName "BlueCrosses" :project SK8 :backgroundRenderer nil)
(setf (Media BlueCrosses) BlueCrossesRSRC)
(new ImageRenderer :objectName "GraySplotch" :project SK8 :backgroundRenderer nil)
(setf (Media GraySplotch) GraySplotchRSRC)
(new ImageRenderer :objectName "RedCrosses" :project SK8 :backgroundRenderer nil)
(setf (Media RedCrosses) RedCrossesRSRC)
(new ImageRenderer :objectName "GrayMist" :project SK8 :backgroundRenderer nil)
(setf (Media GrayMist) GrayMistRSRC)
(new ImageRenderer :objectName "WalnutSwirls" :project SK8 :backgroundRenderer nil)
(setf (Media WalnutSwirls) WalnutSwirlsRSRC)
(new ImageRenderer :objectName "WalnutWaves" :project SK8 :backgroundRenderer nil)
(setf (Media WalnutWaves) WalnutWavesRSRC)
(new ImageRenderer :objectName "SmoothedGrayBumps" :project SK8 :backgroundRenderer nil)
(setf (Media SmoothedGrayBumps) SmoothedGrayBumpsRSRC)
(new ImageRenderer :objectName "GrayBumps" :project SK8 :backgroundRenderer nil)
(setf (Media GrayBumps) GrayBumpsRSRC)
(new ImageRenderer :objectName "GrayCrags" :project SK8 :backgroundRenderer nil)
(setf (Media GrayCrags) GrayCragsRSRC)
(new ImageRenderer :objectName "GreenCrags" :project SK8 :backgroundRenderer nil)
(setf (Media GreenCrags) GreenCragsRSRC)
(new ImageRenderer :objectName "BlueSquares" :project SK8 :backgroundRenderer nil)
(setf (Media BlueSquares) BlueSquaresRSRC)
(new ImageRenderer :objectName "GreenSlashes" :project SK8 :backgroundRenderer nil)
(setf (Media GreenSlashes) GreenLinesRSRC)
(new ImageRenderer :objectName "WhiteGrid" :project SK8 :backgroundRenderer nil)
(setf (Media WhiteGrid) WhiteGridRSRC)
(new ImageRenderer :objectName "WhiteDesktopPattern" :project SK8 :backgroundRenderer nil)
(setf (Media WhiteDesktopPattern) WhiteDesktopPatternRSRC)

(queue-build-fixup
  (setf (fillcolor Stage) DeskTopColor))

#|
	Change History (most recent last):
	2	6/25/93	Hernan	The Great Renaming of 93.
	9	9/28/93	rod	removed getfromuser rgbcolor to be consolidated
	10	10/1/93	hernan	Got rid of color palettes.
	11	10/13/93	hernan	New renderer hierarchy.
	12	10/15/93	hernan	ImageRenderers consolidated!
	13	11/1/93	hernan	macpathname -> file
	14	11/5/93	hernan	Added splitter cursors.
	15	11/19/93	hernan	Added resource icons for the resourceChooser.
	16	12/27/93	rod	Added two new MultiRenderers.  This will show 
				them off to the user to make sure they see what
				they are and what they can do.
	17	2/12/94	kleiman	name changes
	18	2/21/94	hernan	The Final Renaming for Alpha! (yeah, right...)
	19	2/25/94	hernan	Using symbols instead of keywords for options!!!
	20	3/21/94	Hernan	Defined macHandle of deskTopPattern to copy the
				real pattern before using it. This is done because
				the deskTop pattern is used by the window mgr.
				and if it gets disposed terrible crashes happen.
	21	4/1/94	rod	Adding new colors.
	22	5/2/94	Hernan	RenderStyle options -> RenderOption 
				(eg. tiled -> renderTiled).
	23	7/22/94	Hernan	Clearing the background renderer of the ImageRenderers
				that do not need it.
	24 	 9/ 1/94	Hernan  	The penmode of ComplexRGBColor is now stored
							as a symbol.
	25 	 9/12/94	Hernan  	1180502: capitalizing object names.
	26 	10/ 3/94	Hernan  	handle -> mediaData.
	27 	11/ 1/94	rod     	selection crosshair added.
	28 	12/15/94	rod     	Removing bad ppats and picts.
	29 	12/16/94	Brian   	Fixing bug in new colors.
	30 	12/16/94	rod     	
	31 	12/16/94	rod     	adding set fillcolor of stage to here.
	32 	12/22/94	rod     	Ok, Ok, we'll use the user's desktop color as our 
							default stage.
	33 	 2/10/95	sidney  	changes for buildstandalone merging resources into application file
	34 	 2/17/95	rod     	removing conflicts with ui.
	2  	 2/27/97	Hernan  	
|# ;(do not edit past this line!!)
