(IN-PACKAGE :SK8Development)



;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories



#| Modification History

01-22-93 ruben id -> objectName
09-23-92 ICE  Change to conform to latest MacFrames specs - no doobies!
08-24-92 hernan transformed definitions to new MacFrames
07-29-92 ice  changed the colors to SK8color objects.
07-26-92 ruben changed to SK8 package & added color definitions (symbols) to exports.lisp
06-07-92 ruben pict-id made :private throughout
05-12-92 ruben d29 conversion

|#


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  PREDEFINED SK8 COLORS
;;;


;;;--------------------------------
;;; RGB Colors 


(new RGBColor :objectname "Lightgray" :project sk8
     :forered 50000 :foregreen 50000 :foreblue 50000)

(new RGBColor :objectname "Gray" :project sk8
     :forered 32768 :foregreen 32768 :foreblue 32768)

(new RGBColor :objectname "Darkgray" :project sk8
     :forered 16384 :foregreen 16384 :foreblue 16384)

(new RGBColor :objectname "DarkRed" :project sk8
     :forered 32768 :foregreen 0 :foreblue 0)

(new RGBColor :objectname "Red" :project sk8
     :forered 65535 :foregreen 0 :foreblue 0)

(new RGBColor :objectname "LightRed" :project sk8
     :forered 65535 :foregreen 32768 :foreblue 32768)

(new RGBColor :objectname "DarkOrange" :project sk8
     :forered 50000 :foregreen 25000 :foreblue 0)

(new RGBColor :objectname "Orange" :project sk8
     :forered 65535 :foregreen 32768 :foreblue 0)

(new RGBColor :objectname "LightOrange" :project sk8
     :forered 65535 :foregreen 44000 :foreblue 0)

(new RGBColor :objectname "DarkYellow" :project sk8
     :forered 57000 :foregreen 57000 :foreblue 0)

(new RGBColor :objectname "Yellow" :project sk8
     :forered 65535 :foregreen 65535 :foreblue 0)

(new RGBColor :objectname "LightYellow" :project sk8
     :forered 65535 :foregreen 65535 :foreblue 32768)

(new RGBColor :objectname "Darkgreen" :project sk8
     :forered 0 :foregreen 26000 :foreblue 4400)

(new RGBColor :objectname "Green" :project sk8
     :forered 8000 :foregreen 47000 :foreblue 5000)

(new RGBColor :objectname "Lightgreen" :project sk8
     :forered 15000 :foregreen 55000 :foreblue 22000)

(new RGBColor :objectname "DarkBlue" :project sk8
     :forered 0 :foregreen 0 :foreblue 32768)

(new RGBColor :objectname "Blue" :project sk8
     :forered 0 :foregreen 0 :foreblue 55000)

(new RGBColor :objectname "Lightblue" :project sk8
     :forered 0 :foregreen 44000 :foreblue 65535)

(new RGBColor :objectname "DarkPurple" :project sk8
     :forered 14500 :foregreen 0 :foreblue 34000)

(new RGBColor :objectname "Purple" :project sk8
     :forered 18000 :foregreen 0 :foreblue 42000)

(new RGBColor :objectname "LightPurple" :project sk8
     :forered 32000 :foregreen 0 :foreblue 65535)

(new RGBColor :objectname "DarkBrown" :project sk8
     :forered 25500 :foregreen 15000 :foreblue 4000)

(new RGBColor :objectname "Brown" :project sk8
     :forered 46000 :foregreen 27000 :foreblue 7000)

(new RGBColor :objectname "LightBrown" :project sk8
     :forered 57000 :foregreen 33600 :foreblue 8700)

(new RGBColor :objectname "Vomit" :project sk8
     :forered 53000 :foregreen 24000 :foreblue 17000)

(new RGBColor :objectname "Pink" :project sk8
     :forered 65535 :foregreen 2000 :foreblue 32768)

(new RGBColor :objectname "Tan" :project sk8
     :forered 37000 :foregreen 29000 :foreblue 15000)

(new RGBColor :objectname "Cyan" :project sk8
     :forered 0 :foregreen 65535 :foreblue 65535)


;;;--------------------------------
;;; Gray Tones

(new RGBColor :objectname "GrayTone05" :project sk8 :forered 3277 :foregreen 3277 :foreblue 3277)
(new RGBColor :objectname "GrayTone10" :project sk8 :forered 6554 :foregreen 6554 :foreblue 6554)
(new RGBColor :objectname "GrayTone15" :project sk8 :forered 9830 :foregreen 9830 :foreblue 9830) 
(new RGBColor :objectname "GrayTone20" :project sk8 :forered 13107 :foregreen 13107 :foreblue 13107) 
(new RGBColor :objectname "GrayTone25" :project sk8 :forered 16384 :foregreen 16384 :foreblue 16384) 
(new RGBColor :objectname "GrayTone30" :project sk8 :forered 19660 :foregreen 19660 :foreblue 19660) 
(new RGBColor :objectname "GrayTone35" :project sk8 :forered 22937 :foregreen 22937 :foreblue 22937) 
(new RGBColor :objectname "GrayTone40" :project sk8 :forered 26214 :foregreen 26214 :foreblue 26214) 
(new RGBColor :objectname "GrayTone45" :project sk8 :forered 29491 :foregreen 29491 :foreblue 29491) 
(new RGBColor :objectname "GrayTone50" :project sk8 :forered 32768 :foregreen 32768 :foreblue 32768)
(new RGBColor :objectname "GrayTone55" :project sk8 :forered 36044 :foregreen 36044 :foreblue 36044) 
(new RGBColor :objectname "GrayTone60" :project sk8 :forered 39321 :foregreen 39321 :foreblue 39321) 
(new RGBColor :objectname "GrayTone65" :project sk8 :forered 42598 :foregreen 42598 :foreblue 42598) 
(new RGBColor :objectname "GrayTone70" :project sk8 :forered 45874 :foregreen 45874 :foreblue 45874) 
(new RGBColor :objectname "GrayTone75" :project sk8 :forered 49151 :foregreen 49151 :foreblue 49151)
(new RGBColor :objectname "GrayTone80" :project sk8 :forered 52428 :foregreen 52428 :foreblue 52428) 
(new RGBColor :objectname "GrayTone85" :project sk8 :forered 55705 :foregreen 55705 :foreblue 55705) 
(new RGBColor :objectname "GrayTone90" :project sk8 :forered 58982 :foregreen 58982 :foreblue 58982) 
(new RGBColor :objectname "GrayTone95" :project sk8 :forered 62258 :foregreen 62258 :foreblue 62258) 


;;;--------------------------------
;;; Gray patterns

(new ImageRenderer :objectName "Lightgraypattern" :project sk8)
(SETF (Media LIGHTGRAYPATTERN) lightGrayPatternRSRC)
(setf (backgroundRenderer lightGrayPattern) nil)

(new ImageRenderer :objectName "Graypattern" :project sk8)
(SETF (Media GRAYPATTERN) grayPatternRSRC)
(setf (backgroundRenderer GrayPattern) nil)

(new ImageRenderer :objectName "Darkgraypattern" :project sk8)
(SETF (Media DARKGRAYPATTERN) darkGrayPatternRSRC)
(setf (backgroundRenderer darkGrayPattern) nil)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  PICT-BASED SK8 COLORS
;;;

;;; First, define the pict frames:

(new qdPicture :objectName "Sk8SplashPict" :resourceId 0 :project sk8)

;;_________________________________________________________________
;;_________________________________________________________________
;; ARROWS FOR THE HIERARCHICHAL BROWSERS

(new IconRSRC :objectName "FinderClosedArrowCIcon"  :project sk8
          :ResourceID 140 :file (or (file sk8) SK8ApplicationFile))

(new imageRenderer :objectName "FinderClosedArrow"  :project sk8
          :media FinderClosedArrowCIcon)

(new IconRSRC :objectName "FinderInBetweenArrowCIcon"  :project sk8
          :ResourceID 141 :file (or (file sk8) SK8ApplicationFile))

(new imageRenderer :objectName "FinderInBetweenArrow"  :project sk8
          :media FinderInBetweenArrowCIcon)

(new IconRSRC :objectName "FinderOpenArrowCIcon"  :project sk8
          :ResourceID 142 :file (or (file sk8) SK8ApplicationFile))

(new imageRenderer :objectName "FinderOpenArrow"  :project sk8
          :media FinderOpenArrowCIcon)

(new IconRSRC :objectName "FinderEmptyClosedArrowCIcon"  :project sk8
          :ResourceID 128 :file (or (file sk8) SK8ApplicationFile))

(new imageRenderer :objectName "FinderEmptyClosedArrow"  :project sk8
          :media FinderEmptyClosedArrowCIcon)

(new IconRSRC :objectName "FinderEmptyInBetweenArrowCIcon"  :project sk8
          :ResourceID 129 :file (or (file sk8) SK8ApplicationFile))

(new imageRenderer :objectName "FinderEmptyInBetweenArrow"  :project sk8
          :media FinderEmptyInBetweenArrowCIcon)

(new IconRSRC :objectName "FinderEmptyOpenArrowCIcon"  :project sk8
          :ResourceID 139 :file (or (file sk8) SK8ApplicationFile))

(new imageRenderer :objectName "FinderEmptyOpenArrow"  :project sk8
          :media FinderEmptyOpenArrowCIcon)

#|
	Change History (most recent last):
	2	6/25/93	Hernan	The Great Renaming of 93.
	9	10/1/93	hernan	=====================================
				= Sad but true: the node is gone!!! =
				=====================================
	10	10/13/93	hernan	The renderer hierarchy has been revisited.
	11	10/15/93	hernan	ImageRenderers consolidated!
	12	12/1/93	hernan	Removing opRgbColor from the hatch renderers.
	13	12/22/93	rod	
	14	2/21/94	hernan	The Final Renaming for Alpha! (yeah, right...)
	15	2/25/94	hernan	Using symbols instead of keywords for options!!!
	16	3/24/94	Hernan	Fixing leftover keywords in complexGradient's
				render method.
	17	3/25/94	Hernan	Fixing leftover keywords.
	18	3/30/94	rod	Adding ArrowIcons
	19	5/2/94	Hernan	RenderStyle options -> RenderOption 
				(eg. tiled -> renderTiled).
	20	5/4/94	Hernan	Making the SK8 Splash Pict look at the SK8 painting.
	21	5/10/94	rod	Removing reference to uigrays...
	22	7/22/94	Hernan	Clearing the background renderer of the ImageRenderers
				that do not need it.
	23	8/18/94	kleiman	1180502
	24 	 9/ 1/94	Hernan  	The penmode of ComplexRGBColor is now stored
							as a symbol.
	25 	 9/16/94	Hernan  	Made prisonbars's background renderer be transparent.
	26 	10/ 6/94	It      	fixed :direction arg for Bluetunnel gradient: 'rect -> 'rectangle
	27 	10/24/94	Hernan  	Why? Who did that change? The direction of 
							blueTunnel is 'rect', not 'RECTANGLE'. I am now
							changing it back to what it was.
	28 	12/15/94	rod     	Removing bad ppats and picts.
	29 	 2/10/95	sidney  	changes for buildstandalone merging resources into application file
	2  	 2/27/97	Hernan  	
|# ;(do not edit past this line!!)
