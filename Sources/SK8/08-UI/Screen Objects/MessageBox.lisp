;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories


;;;

(in-package :UIDevelopment)

(SK8-declare-syms :UI :public ; Updated  4-19-95  11:23 am
                  UI::MBLISTENERSCROLLER)

(SK8-declare-syms :UI :public ; Updated  5-08-96   2:59 pm
                  UI::ADDITEM UI::COMMANDTEXTFONT UI::MBOUTPUTTEXT UI::RESULTTEXTFONT)

;;__________________________________________________________________
;;_____________           The Core Message Box                                _______________
;;__________________________________________________________________

(MF::PUBLISH-PROJECT-SYMBOL 'SK8::MessageBox (FIND-PACKAGE :SK8))  ;; So the constant's available in SK8

(new uisimplewindow :objectName "MessageBox" :project ui
     :properties '(savedThisActivation))
(setf (minimumsize messagebox) '(150 100))
(setf (actor PBMenuMessageBox) MessageBox)
(setf (sk8::menubar messagebox) nil)
(setf (resizer messagebox) t)
(setf (zoombox messagebox) t)


(define-handler clearReferences (MessageBox &key ((:objects theobjects)))
  )

(define-handler SetUpForProject (MessageBox &key ((:project theproject)))
  (enteringstage me))

(setf (dofirstclick MessageBox) t)

(define-handler enteringstage (MessageBox)
  (unless (and (keytarget me) (neq (keytarget me) me)) 
    (setf (keytarget me) mblistener)
    )
  (setf (text me) (concatenate 'string (objectstring (targetProject ui)  :project (targetProject ui)) " Message Box"))
  )

(define-handler activate (MessageBox)
  ;; (unlock me :force t) What is this for? 
  (withActorLocked (me)
    ;; Only mess with the text when you have to.
    (let ((newText (concatenate 'string (objectstring (targetProject ui) :project (targetProject ui)) " Message Box")))
      (unless (string-equal (text me) newText)
        (setf (text me) newText)))
    (call-next-method)
    (setf (currentcommandkey system) ProjectBuilderMenubar)
    (setf (savedThisActivation me) nil)
    )
  t)


;;;  For the systemLog protocol
;;;

(define-handler insertInto (MessageBox newElement &optional dependentStates)
  (declare (ignore dependentStates))
  (addItem (picker mbText) newElement :itemType 'result)
  me)

(define-handler getAttention (MessageBox)
  (bringup me)
  (setf (keytarget me) (picker ui::mbtext))
  )

;;;  Make it SK8's systemLog !
;;;
(setf (systemLog sk8::SK8) MessageBox)

;;; _______________________________ 
;;; The mbOutputText
;;; _______________________________ 

;;; An edittext that outputs SK8Script stuff... 

(new ScriptEditText :objectname "mbOutputText" :project ui
     :properties '(commandTextFont resultTextFont))

(setf (commandTextFont mbOutputText) EspySansBoldFont)
(setf (resultTextFont mbOutputText) EspySansFont)

(setf (wrapping mbOutputText) 'word)
(setf (tablength mbOutputText) 3)

;;; This is A LOT faster (about 10 times) than the version written with
;;; the SK8 EditText API. 

(define-handler addItem (mbOutputText item &key (itemType 'command))
  (let* ((buf (fred-buffer (editData me)))
         (size (buffer-size buf))
         (font-spec (buffer-char-font-spec buf size)))
    (set-mark buf size)
    (buffer-set-font-spec buf (if (eq itemType 'command)
                                (progn (setf (car font-spec) (fontname (commandTextFont me)))
                                       font-spec)
                                (progn (setf (car font-spec) (fontname (resultTextFont me)))
                                       font-spec)))
    (buffer-insert buf (simpleobjectString item))
    (buffer-insert buf (string #\Newline))
    ;; Overhead: scroll to the end of the text to show the result. Update the scroller. 
    (setSelection me (buffer-size buf) (buffer-size buf))
    (showselection me)
    (updatePartnerScroller me (partnerScroller me 'vertical))))

;;; Gets the selected text and puts it in the mblistener. 

(define-handler doubleClick (mbOutputText)
  (sk8-multival-bind (myStart myEnd) (selection me)
    (when (eq (textFont me :start myStart :end myEnd) (commandTextFont me))
      (let* ((selection (selection me))
             (startPos (previousFontChange me (cadr selection)))
             (endPos (sk8::nextFontChange me (cadr selection))))
        (setf (selection me) (list startPos endPos))
        (sk8-multival-bind (start end) (selection mbListener)
          (when (eql start end) (setf start nil end nil))
          (setf (text mbListener :start start :end end)
                (string-trim '(#\Newline #\Space)
                             (text me :start startPos :end endPos))))))))

(define-handler enterInField (mbOutputText)
  (setf (keytarget messagebox) mbListener)
  (enterInField mblistener))

(define-handler tabInField (mbOutputText)
  (setf (keytarget messagebox) mblistener))

;;___________________________________________________________________
;; the textField

(new uitextListforcorners :objectName "mbText" :project ui)
(setf (pickerPrototype mbText) mbOutputText)
(hide (titlebar mbText))
(Setf (container mbText) messagebox)
(setf (frameColor mbText) black)
(setFrameSize mbText 0 0)
(Setf (textSize mbText) 9)
(Setf (textFont mbText) defaultUIFont)

(define-handler SelectAll (mbtext)
  (selectall (picker me))
  )

;;___________________________________________________________________
;; the listener area

(new scripteditText :objectName "mbListener" :project ui)
(Setf (container mbListener) MessageBox)
(Setf (textFont mbListener) espysansboldfont)
(Setf (textSize mbListener) 9)
(setf (textStyle mbListener) '(plain))
(setf (wrapping mbListener) 'word)
(setf (frameColor mbListener) black)
(setFrameSize mbListener 0 0)
(setf (fillcolor mbListener) (backgroundrenderer shadowedrenderer))

(define-handler targetProject (mbListener)
  (targetProject ui))

(define-handler scriptInputCompleted (mbListener translation &key originalText locals globals)
  (let (result)
    (tickeventclock)
    (setSelection me 0 -1)
    (addItem (picker mbText) (text me))
    (tickeventclock)
    ;; Evaluate the translation. 
    (setf result (evaluateScriptTranslation translation))
    ;; When there is a valid target project AND the thing we just
    ;; compiled was a function or handler, save it into the store. 
    (when (and (not (memq (targetProject ui) (list sk8 ui)))
               (or (and (symbolp result) (fboundp result))
                   (eq (type-of result) 'standard-method)))
      (PS::savehandlerVersionFromText (targetProject ui) result originalText locals globals))
    (set 'SK8::SK8SCRIPTRESULT result)
    (tickeventclock)
    (addItem (picker mbText) result :itemType 'result)
    (tickeventclock)
    ))

(define-handler keydown (mblistener theKey)
  (cond
   ((eq theKey #\tab)
    (setf (keyTarget MessageBox) (picker mbtext)))
   ((or (eq theKey #\enter) (eq theKey #\return))
    (sk8::withlockedcursor animatedclock 
      (call-next-method)))
   (t
    (call-next-method))))

(define-handler SelectAll (mblistener)
  (setSelection me 0 -1)  
  )

(define-handler (setf Highlight) (theval mblistener)
  (setf (inverts me) nil)
  (if theval
    (setf (fillcolor me) white)
    (setf (fillcolor me) shadowwhite)
    )
  (call-next-method))

(new uiverticalscroller :objectname "mbListenerScroller" :project ui)
(setf (container mbListenerScroller) messagebox)
(tagpart MessageBox mbListenerScroller 'scroller)
(setf (partnervscroller mblistener) mbListenerScroller)


;;___________________________________________________________________
;; Divider

(new uiLittleSplitter :objectname "MBSplitter" :project ui
     :properties '((proportion :value 0.25)))

(setf (container MBSplitter) MessageBox)


(tagpart MessageBox MBSplitter 'Splitter)
(define-handler drag (MBSplitter &key live otherActors dropEvents DraggedOverEvents 
                                  draggingMouseWithinEvents onStage constrainingRect)
  (declare (ignore live otherActors dropEvents DraggedOverEvents 
                   draggingMouseWithinEvents onStage constrainingRect))
  (withActorLocked (me)
    (call-next-method)
    (setf (proportion me) 
          (/ (- (top me) (absolutetop me)) (- (absoluteBottom me) (absolutetop me))))
    (resized (sk8::window me)))
  )
(define-handler bestsize (MBSplitter)
  (let* ((width (width (container me)))
         (v (+ (absoluteTop me) (* (- (absoluteBottom me) (absoluteTop me)) (Proportion me)))))
    (setBoundsRect me (if *MacStyleInterface* 0 (- *WindowLeft* 1)) v (- width (if *MacStyleInterface* 0 (- *WindowRight* 1))) (+ v 2))
    ))

(define-handler setBoundsRect (MBSplitter left top right bottom 
                                             &key physical relative justMoving)
  (let ((objed (container me))
        (abstop (absoluteTop me))
        (absbot (absoluteBottom me)))
    (unless relative
      
      (if (< top (+ 18 abstop))
        (progn
          (setf (visible mblistener) nil)
          (setf (visible ListenerHilite) nil)
          (setf (keytarget objed) (picker mbtext))
          (setf top abstop))
        (unless (visible mblistener)
          (setf (visible mblistener) t)
          (setf (visible ListenerHilite) t)
          ))
      (if (> top (- absbot 30))
        (progn
          (setf (visible mbtext) nil)
          (setf (keytarget objed) mblistener)
          (setf top (- absbot 13) ))
        (unless (visible mbtext)
          (setf (visible mbtext) t)
          ))
      
      )
    (call-next-method me left top right (+ top 13) :physical physical 
                      :relative relative :justmoving justmoving)
    ))

;;___________________________________________________________________

;;;;Special set of bestSizes to make sure 

(new SIFInsetrect :project ui :objectname "ListenerHilite")
(setf (container ListenerHilite) MessageBox)
(sendtoback ListenerHilite)
(bringtofront mbListenerScroller)

(define-handler resized (MessageBox)
  (let (hsize vsize (toppy (if *MacStyleInterface* 0 (+ *WindowTop* 15) )) topsize)
    (declare (special hsize vsize))
    (withActorLocked (me)
      (sk8-multival-setf (hsize vsize) (size me))
      (setBoundsRect ListenerHilite *WindowLeft* toppy (- hsize *WindowRight*) vsize)
      (setBoundsRect mbtext *WindowLeft* 0 (- hsize *WindowRight*) (- vsize 25))
      (setf (absoluteTop (Splitter me)) toppy)
      (setf (absoluteBottom (Splitter me)) (- vSize 20))
      (bestsize (splitter me))
      (setf topsize  (- (+ (top (splitter me)) 3) toppy))
      (setf (visible mblistenerscroller) (> topsize 35))
      (setBoundsRect ListenerHilite *WindowLeft* toppy (- hsize *WindowRight*) (+ (top (splitter me)) 3))
      (setBoundsRect mblistener 
                     (+ *WindowLeft* (if *MacStyleInterface* 1 4 )) 
                     (+ toppy (if *MacStyleInterface* 1 4 ))
                     (if (visible mblistenerscroller)
                       (- hsize *WindowRight* (if *MacStyleInterface* 17 14))
                       (- hsize *WindowRight* (if *MacStyleInterface* 1 1)))
                     (- (top (splitter me)) (if *MacStyleInterface* 1 -1)))
      (when (visible mblistenerscroller)
        (setBoundsRect mblistenerscroller (- hsize *WindowRight* (if *MacStyleInterface* 16 13)) toppy 
                       (- hsize *WindowRight*) (+ (top (splitter me)) (if *MacStyleInterface* 0 3))))

      (setBoundsRect mbtext *WindowLeft* (bottom (splitter me)) (- hsize *WindowRight*) (- vsize *WindowBottom*))
      (call-next-method))
    ))

(define-handler bestsize ((Picker mbText))
  (sk8-multival-bind (hs vs) (size mbText)
    (setBoundsRect me 0 0 (- hs 25) vs)))

(define-handler bestSize ((vscroller mbText))
  (sk8-multival-bind (hs vs) (size mbText)
    (setBoundsRect me (- hs 16) 0 hs vs)))



;;__________________________________________________________________
;;______ The message object stuff                                                             __________
;;__________________________________________________________________

(new object :objectName "InspectableLogObject" :project ui
     )
(define-handler inspectLogObject (InspectableLogObject)
  nil)


(setBoundsRect MessageBox 5 345 350 485)

(resized MessageBox)
(setf (container MessageBox) stage)



#|
	Change History (most recent last):
	2	5/11/93	Brian Roddy	
	3	9/20/93	rod	Ruben's patchs to the "compact" menu items
	45	10/8/93	hernan	Removed references to menuname.
	47	10/11/93	rod	fixed bug in declaring the curproj property
	51	10/21/93	kleiman	Changed to use new store
	64	11/12/93	kleiman	add d4 store menus
	65	11/12/93	hernan	Making updateMenu for the "stack editor" option
				work now that the sk8ewindow class is gone.
	68	11/24/93	kleiman	searchscripts dialog
	70	11/29/93	chip	new logging protocol
	73	12/3/93	kleiman	menuselct searchstore
	78	12/10/93	rod	added a property to check if the user does a save
				and then a close sequence.  Minimal project dirty
				check.  This will break if a control key is added to 
				close a project.
	79	12/13/93	sidney	Put processing for Close project menu in before and after handlers on closeproject so that it can be done however a project ends up closed, e.g., as part of Quit processing
	81	12/14/93	kleiman	Disabled color palette option for kitchen
	85	1/13/94	rod	Undo!!!
	88	1/17/94	sidney	Ask about saving project file before quitting SK8
	89	1/17/94	hernan	Adding the animatedCursor to some of the actions
				we can do from here.
	93	2/12/94	kleiman	name changes
	94	2/14/94	sidney	rename children to knownchildren
	98	2/21/94	hernan	The Final Renaming for Alpha! (yeah, right...)
	99	2/21/94	hernan	The Final Renaming for Alpha! (yeah, right...)
	100	2/21/94	hernan	close -> sk8::close.
	101	2/22/94	rod	
	102	2/22/94	rod	
	103	2/22/94	kleiman	
	104	2/25/94	hernan	Using symbols instead of keywords for options!!!
	105	2/26/94	rod	
	106	2/26/94	rod	
	107	2/28/94	rod	
	108	2/28/94	hernan	Avoiding disposing things directly.
	109	3/5/94	rod	Changing draw palette
	110	3/6/94	chip	print... --> write...; "logObject" renaming
	111	3/10/94	rod	
	112	3/10/94	Hernan	Making mbListener word wrap.
	113	3/15/94	rod	Fixing Log holding onto objects
	114	3/15/94	rod	Fixing Log holding onto objects
	115	3/16/94	rod	
	116	3/17/94	rod	various fixups...
	117	3/26/94	rod	Minimum size stuff.
	118	3/29/94	rod	
	124	4/18/94	rod	
	125	4/22/94	Hernan	Made resized do its thing with the actor locked.
	127	5/6/94	rod	Redoing drag stuff
	129	6/3/94	rod	Here begins the experimentation with a Mac look 
				to the UI.
	130	6/3/94	rod	Made mbtext so you can drag multiple from it.
	131	6/7/94	chip	made all calls to logObjectString use the correct project arg
	132	6/7/94	chip	fixed ordering glitch in extendedMouseDown of mbtext's picker
	133	6/13/94	rod	1167775: Fixing various problems with the 
				scrolling of the picker of mbtext.
	134	6/13/94	rod	
	135	6/17/94	rod	Fixing objectstring
	136	6/23/94	rod	Fixing keytarget stuff in activation.
	137	7/6/94	rod	Getting rid of UIColorize.
	138	7/21/94	rod	
	139	 8/23/94	rod     	
	140	 8/24/94	rod     	
	141	 9/ 1/94	Hernan  	espysansbold -> espysansboldfont.
	142	 9/ 1/94	Hernan  	
	143	 9/ 2/94	rod     	
	144	 9/15/94	rod     	
	145	 9/20/94	rod     	Request for special enter key in mbpicker.
	146	10/25/94	rod     	Making enter in picker replace text.
	147	 1/18/95	till    	bug 1211599, add an undo to dropped of mbListener
	148	 1/20/95	Hernan  	Conforming to new picker set selected items API.
	149	 1/23/95	rod     	Fixing minor problems in doubleclick of the
							message panel.
	150	 2/ 3/95	Hernan  	WithLockedCursor is now correct. I check this file
							out and back in to force it to be recompiled.
	151	 2/14/95	Hernan  	Speeded up messageBox activation by making
							sure we do not set the text if it is nor required.
	152	 3/13/95	sidney  	use eval-enqueue to keep from tying things up from the messagebox
	153	 3/14/95	rod     	Removing sidney's eval-enqueue as it is not
							thread safe.
	154	 4/19/95	rod     	Adding scroller to mbListener...
	2  	 1/ 9/96	Hernan  	Removing calls to set scriptType.
	3  	 2/14/96	Brian   	changing font vars.
	2  	 4/10/96	Hernan  	Removing case preserving reader macro.
	3  	 5/ 9/96	Hernan  	Replacing the multilinepicker with an editText. This makes
						the messageBox a lot faster than before.
	4  	 5/ 9/96	Hernan  	Making the picker of mbText be wrapping.
	5  	 7/23/96	Hernan  	Making the textlist part behave better when doubleclicking.
	6  	10/11/96	Brian   	
	7  	11/14/96	Brian   	Making messagebox use normal sk8 error handling
	8  	12/17/96	Brian Roddy	
	9  	12/17/96	Hernan  	scriptInputCompleted now checks to see if the result of the
						evaluation is a handler or function. If so, the result is saved
						using the store.
	10 	 2/27/97	Hernan  	
|# ;(do not edit past this line!!)
