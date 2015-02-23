(in-package :SK8)

;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories



(SK8-declare-syms :SK8 :public
                  
                  ;; Property "attribute" options (for 'properties' arg to new).
                  value propagatedValue private
                  
                  ;; Actor.
                  doubleClickOnly clickOnly ;; standard to overridenSyms!
                  topLeft topRight topCenter
                  centerLeft centerRight center
                  bottomLeft bottomRight bottomCenter
                  expand condense outline ;; shadow to overridenSyms!
                  underline italic bold plain
                  normal opaque transparent invisible custom
                  bounds rect frame ;; fill to overridenSyms!
                  top bottom left right
                  ;; Checkbox.
                  mixed X
                  ;; Connector.
                  projectCenter horizontalEdge closestEdge verticalEdge
                  ;; EditText.
                  vertical horizontal word character characters
                  item line para paragraph
                  ;; line.
                  start end both
                  ;; menus.
                  menu popup mouseLoc bottomRightAligned
                  ;; picker.
                  single contiguous discontiguous items partialItems
                  ;; specialPickers.
                  itemAndPosition
                  ;; rectangle.
                  blank sk8Window documentWithZoom documentWithGrow
                  document doubleEdgeBox singleEdgeBox shadowEdgeBox
                  tool movableDialog floating
                  documentNoClose documentWithGrowNoClose 
                  documentWithZoomNoClose
                  ;; roundrect.
                  auto
                  ;; ScriptEditText.
                  expr exprOrCommand command accessor
                  ;; Renderers.
                  tiled stretched unstretched shape oval 
                  ;; Pen and Mask.
                  srcCopy srcOr srcXor srcBic
                  notSrcCopy notSrcOr notSrcXor
                  notSrcBic patCopy patOr patXor
                  patBic notPatCopy notPatOr
                  notPatXor notPatBic grayishTextOr
                  blend addPin addOver 
                  subPin addMax adMax
                  subOver adMin ditherCopy
                  transparent highlight
                  ;; Visual Effects
                  fast slow
                  
                  ;; MediaComponents.
                  file resType resources resID resName name
                  ;; Table Picker.
                  multiple index indeces 
                  leftCenter rightCenter 
                  
                  ;; QuickTimeMovie
                  palindrome unknown ; values for repeating
                  standardMovieProgessFunction

                  ;; QuickTimeRenderer
                  inactive stopped poster paused playing ; values for state
                  previous ; additional state value for newStateWhenDone
                  resizeMovieToActor resizeActorToMovie ; values for resizingStyle
                  
                  ;; QuickTimeMedia types
                  video sound text
                  
                  ;; QuickTimeCallBack
                  Rate TimeJump Extremes
                  start stop
                  forward backward either
                  change lessThan greaterThan equal lessEqual greaterEqual notEqual

                  ;; QuickTimeTimeBase
                  beforeStartTime afterStartTime

                  ;; getShapeFromUser
                  points cornerToCorner centerOut regular freeHand
                  symmetrical symegon simpleLine
                  
                  ;; Files
                  input io output ; values for direction
                  rename overwrite supersede create ; values for ifExists/ifDoesNotExist
                  
                  ;; Port's portType (& broadcast, an option for the wiredTo property)
                  unknown input output inputOutput broadcast
                  
                  ;; some "pseudo" types
                  NonnegativeInteger PositiveInteger
                  
                  ;; QuickTimeRenderer maintainAspectRatio values
                  fitInside fitOutside
                  )


#|
	Change History (most recent last):
	1		2/25/94	hernan	New file. Has every symbol that is used for an
							option in SK8. These should be added here
							manually!!!
	2		2/25/94	dy		Added QuickTimePlayer symbols
	5		2/25/94	kleiman	put back #~ thingies
	6		2/25/94	kleiman	took out loop again
	7		2/28/94	hernan	Adding fast and slow for visual effects.
	8		3/3/94	chip	added 'accessor' for ScriptEditText's 'displayingActiveVersion' property
	9		3/4/94	Hernan	Adding characters.
	10		3/21/94	till	Stream keywords: output, input, io
	11		3/24/94	Hernan	Added 'shape and 'oval, both used by the gradient
							renderers.
	12		4/21/94	dy		qt sk8::poster
	13		6/7/94	Hernan	Adding auto for the roundrect.
	14		6/8/94	yost	add sk8::loop to the list
	15		6/8/94	sidney	took out loop again: moved to sk8sharedsyms
	16		6/27/94	Hernan	1170049: adding style options as symbols.
	17		6/27/94	Hernan	1170049: forgot to add one.
	18		7/6/94	sidney	remove pound twiddles
	19		7/15/94	dy		loop
	20		7/18/94	dy		QuickTimeSimplePlayer -> QuickTimeRenderer
	21		7/21/94	chip	the "types" nonnegativeInteger & positiveInteger
	22 	 8/31/94	Hernan  	Added floating option for wdef.
	23 	 9/ 1/94	dy      	add 'video' etc.
	24 	 9/29/94	chip    	added portTypes
	25 	 9/29/94	chip    	another Port option
	26 	10/ 3/94	dy      	add changedNeither changedHeight changedWidth
	27 	10/ 5/94	chip    	more File stuff
	28 	10/11/94	chip    	added property "attribute" options
	29 	10/17/94	dy      	add fitInside & fitOutside for QTRenderer
	30 	11/18/94	Hernan  	adding 'x
	31 	 3/11/95	dy      	introduce QuickTimeCallBack and QuickTimeTimeBase symbols
	32 	 3/14/95	Hernan  	Adding new windowStyles that do not have a close
							box.
	33 	 3/20/95	dy      	add standardMovieProgessFunction
	2  	 5/ 7/96	sidney  	Changes for new object system
	3  	 2/27/97	Hernan  	
|# ;(do not edit past this line!!)
