;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories

(in-package :SK8Development)

(provide "SK8ABOUTBOX")

(require "LABEL" "objects;Widgets:Label")
(require "STYLEDPICKER" "objects;Pickers:StyledPicker")
(require "REALTIMECLOCK" "objects;Clocks:RealTimeClock")

(new rectangle :objectName "SK8AboutBox" :project sk8)

(setf (fillcolor sk8aboutbox) white)
(setf (draggable sk8aboutbox) nil)
(setSize sk8aboutbox 397 275)
(setf (Location sk8AboutBox) (mainMonitorCenter))

;;; A Label that reads "SK8"

(new label :objectname "AboutBoxTitle" :project sk8)
(setf (container AboutBoxTitle) sk8aboutbox)
(setf (text AboutBoxTitle) "SK8")
(setf (textSize AboutBoxTitle) 48)
(setf (textFont AboutBoxTitle) TimesFont)
(setf (textColor AboutBoxTitle) black)
(setf (textStyle AboutBoxTitle) '(bold))
(setlocation aboutboxTitle 54 38)

;;; Put the text in a label so that it does not get redrawn.

(new label :objectname "AboutBoxVersion" :project sk8)
(setf (container AboutBoxVersion) sk8aboutbox)
(setf (text AboutBoxVersion) "1.1: Going Native")
(setf (textSize AboutBoxVersion) 24)
(setf (textFont AboutBoxVersion) TimesFont)
(setf (textColor AboutBoxVersion) black)
(setf (textStyle AboutBoxVersion) '(bold condense))
(setLocation AboutBoxVersion 200 43)
(setf (textLocation AboutBoxVersion) 'centerLeft)

;;; The Picture Holder.

(new rectangle :objectname "AboutBoxPict" :project sk8)
(setf (container aboutBoxPict) sk8AboutBox)
(setboundsRect aboutBoxPict 20 61 208 252)

(new imageRenderer :objectname "SK8Painting" :project sk8)
(setf (media sk8Painting) sk8splashpict)

(setf (fillcolor aboutBoxPict) sk8Painting)

(new styledpicker :objectName "AboutBoxText" :project sk8)
(setf (container aboutBoxText) sk8AboutBox)
(setf (mousesensitivity aboutBoxText) 'transparent)
(setf (fillcolor aboutBoxText) white)
(setFramesize aboutBoxText 0 0)
(setBoundsRect aboutBoxText 216 60 382 251)
(setf (textSize aboutBoxText) 10)
(setf (textColor aboutBoxText) Black)
(setf (alphabeticalDisplay aboutBoxText) nil)

(define-handler createtextdisplayitem (aboutboxtext theitem)
  theitem)

;;; Returns... {font,size,style,color}

(define-handler createTextItemStyle (aboutboxtext targetItem theString position)
  (declare (ignore targetItem position))
  (let* ((xx (length thestring))
         (lastchar (subseq thestring (if (= xx 0) 0 (1- xx))))
         (s '(underline))
         style color)
    (cond ((string= lastchar ":")
           (setf color red)
           (setq style s))
          (t (setf color black)
             (setf style nil)))
    (sk8-multivals nil nil style color)
    ))

(setf (items aboutBoxText) (list "" "" "" "" "" "" "" "" "" "" ""
                                 "Welcome to SK8 1.1"
                                 ""
                                 "Conception & Development:"
                                 "Hern‡n Epelman-Wang"
                                 "Brian Roddy"
                                 "Sidney Markowitz"
                                 "and"
                                 "RubŽn Kleiman"
                                 ""
                                 "Special Thanks:"
                                 "Adam Chipkin"
                                 "David Vronay"
                                 "Dave Yost"
                                 ""
                                 "The Management:"
                                 "Jim Spohrer"
                                 "Lori Leahy"
                                 "Mark Miller"
                                 "Don Norman"
                                 ""
                                 "Developer Technical Support:"
                                 "Rick Evans"
                                 "Alan Bernard"
                                 ""
                                 "Special Collaborators:"
                                 "Alan Peterson"
                                 "Stephanie Houde"
                                 "Bill St. Clair"
                                 "and the rest of the folks"
                                 "at Digitool"
                                 ""
                                 "Our Interns:"
                                 "Eric Babinet"
                                 "Orville Bailey"
                                 "Steve Blessing"
                                 "Sasha Karasik"
                                 "John Lilly"
                                 "Rodrigo Madanes"
                                 "Steve Seitz"
                                 "Royston Sellman"
                                 "Maurice Sharp"
                                 "Alberto Torres"
                                 "Marcos Vescovi"
                                 "Nathan Wilson"
                                 "Gary Young"
                                 ""
                                 "Our Friends In AppleSoft:"
                                 "Chris Espinosa"
                                 "Mike Wirth"
                                 "Ken Dickey"
                                 "Mikel Evins"
                                 "Chris Flick"
                                 "Philip McBride"
                                 "John Ulrich"
                                 ""
                                 "Collaborators:"
                                 "Allen Cypher"
                                 "David C. Smith"
                                 "Jamie Dinkelacker"
                                 "Tyde Richards"
                                 "Arthur James"
                                 "Dan Rose"
                                 "Ted Kaehler"
                                 "Michael Kass"
                                 "Pete Litwinowitz"
                                 "Ramon Felciano"
                                 "Don Tillman"
                                 ""
                                 "and the SK8 widows:"
                                 "Elizabeth Gomez"
                                 "Kristen Kadner"
                                 "Beverley Kane"
                                 "Chris Leahy"
                                 "Dianne Spohrer"
                                 ""
                                 "" "" "" "" ""
                                 "" "" "" "" ""
))

(setf (windowStyle sk8aboutbox) 'sk8Window)
(setf (selecteditems aboutBoxText) nil)

(new RealTimeClock :objectname "AboutBoxClock" :project sk8)
(setf (ticksPerSecond AboutBoxClock) 2)

(define-handler tick (AboutBoxClock)
  (if (>= (verticalscroll aboutBoxText) (- (length (items aboutboxtext)) 2))
    (withVisualEffect (sk8AboutBox (nth (random 4) 
                                        (list FanHouseUp FanHouseDown FanHouseLeft FanHouseRight))
                                   'fast)
      (setf (verticalscroll aboutBoxText) 0))
    (incf (verticalscroll aboutBoxText) 1)))

(define-handler deactivate (sk8aboutBox)
  (moveoffstage me)
  (call-next-method)
  (setf (running AboutBoxClock) nil)
  (eval-in-new-thread "Closing About Box" 
    `(setf (container ,me) nil)))

(define-handler mouseup (sk8aboutbox)
  (deactivate me))

(define-handler restore (SK8AboutBox)
  (call-next-method)
  (setf (text aboutBoxVersion) 
        ;(format nil "~a~@[p~a~]" mf::*macframes-version* mf::*macframes-patch*)
        "1.1: Going Native"
        ))

(define-handler enteringStage (SK8AboutBox)
  (setf (verticalscroll aboutBoxText) 0)
  ;; (bringtofront aboutBoxText) What was this for?
  (setf (location me) (mainmonitorCenter))
  (call-next-method)
  (setf (running AboutBoxClock) t))

(setf (aboutBox stage :itemText "About SK8...") SK8AboutBox)
  

#|
	Change History (most recent last):
	2	6/25/93	Hernan	The Great Renaming of 93.
	10	8/13/93	kleiman	restore changed version to d4
	11	10/15/93	hernan	Removing the restore method, adding location
				when attaching to the stage.
	12	12/21/93	sidney	New team member
	13	1/11/94	hernan	self -> me
	14	2/21/94	hernan	The Final Renaming for Alpha! (yeah, right...)
	15	2/25/94	hernan	Using symbols instead of keywords for options!!!
	16	2/28/94	rod	
	17	3/1/94	rod	
	18	3/3/94	Hernan	The great handler argument name renaming of 94!
	19	3/3/94	kleiman	private properties declared via makeprivateproperty
	20	3/9/94	Hernan	alphabetical -> alphabeticalDisplay
	21	3/18/94	kleiman	let it show
	23	6/10/94	kleiman	Problem 1167699
	24	6/17/94	kleiman	Problem 1169044
	25	7/18/94	Hernan	1169462: item -> targetItem
	26	7/26/94	Hernan	Making the aboutBox faster so that the clock does
				not tie up the whole system. Also making it look
				better.
	27	7/26/94	Hernan	Declaring symbols.
	28		7/27/94	Hernan	Making the aboutbox go away when you click on
					it.
	29		7/29/94	Hernan	Stupid, really stupid paren missing.
	30		8/2/94	kleiman	added new interns
	31		8/10/94	rod	Alphabetized the interns.
	32 	 8/22/94	Hernan  	Adding Jamie and Rick.
	33		8/23/94	rod	Fixing mispelled name
	34 	 8/31/94	rod     	
	35 	 9/12/94	Hernan  	1180502: capitalizing object names.
	36 	 9/26/94	It      	fixed conversion of *macframes-version* into a string!
	37 	10/ 4/94	sidney  	nitpicky thing in aboutbox deactivate
	38 	11/ 4/94	kend    	Added Ken Dickey (who?)
	39 	11/10/94	rod     	Adding the AppleSoft Team.
	40 	11/10/94	rod     	
	41 	 1/17/95	Hernan  	Corrected Philip's name.
	42 	 3/28/95	rod     	Adding quality team
	43 	 4/17/95	Hernan  	Adding Mikel...
	2  	 7/10/95	Brian Roddy	removing eval-enqueues
	3  	12/ 8/95	Brian   	
	2  	 4/10/96	Hernan  	define-system-handler->define-handler.
	3  	 5/21/96	Brian   	Cleaning up for 1.1
	4  	10/17/96	Hernan  	Making the label that says "going native" look nice.
	5  	11/12/96	sidney  	added a name argument to eval-in-new-thread
	6  	 2/27/97	Hernan  	
|# ;(do not edit past this line!!)
