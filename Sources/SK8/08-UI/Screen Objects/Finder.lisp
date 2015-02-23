;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories


;;;

(in-package :UIDevelopment)

(SK8-declare-syms :UI :public ; Updated  2-14-96  11:39 am
                  UI::INTERSECTIONFIND UI::PSHEET UI::SEARCHER)


(new UISimpleWindow :objectname "Searcher" :project ui
     :properties '(intersectionfind))

(define-handler clearReferences (Searcher &key ((:objects theobjects)))
  (setf (inputobjects (picker SearcherObjectList)) nil)
  (setf (outputobjects (picker SearcherObjectList)) nil)
  (setf (text (textfield  SearcherSearchField)) nil)
  (setf (outputobjects SearcherSearchField) nil)
  )

(define-handler SetUpForProject (Searcher &key ((:project theproject)))
  (withactorlocked (me)
    (setf (text me) (concatenate 'string (objectstring theproject :project theproject) " Searcher"))
    (clearReferences me)))


(setf (sk8::menubar Searcher) nil)
(setf (resizer Searcher) t)
(setf (zoombox Searcher) t)


(define-handler resized (Searcher)
  (let (hSize vSize)
    (sk8-multival-setf (hSize vSize) (size me))
    (call-next-method)
    (setBoundsRect SearcherFindLabel 9 (+ *windowtop* 8) 100 (+ *windowtop* 20))
    (setBoundsRect searchSpace 18 (+ *windowtop* 26) 98 (+ *windowtop* 46))
    (setBoundsRect SearchStyle 108 (+ *windowtop* 26) 198 (+ *windowtop* 46))
    (setBoundsRect SearcherSearchField 18 (+ *windowtop* 52) (- hsize 18) (+ *windowtop* 72))
    (setBoundsRect FindObjectFindButton (- hsize 74) (+ *windowtop* 78) (- hsize 20) (+ *windowtop* 99))
    (setBoundsRect IntersectionFindButton (- hsize 147) (+ *windowtop* 78) (- hsize 80) (+ *windowtop* 99))
    (setBoundsRect SearcherObjectList *WindowLeft* (+ *windowtop* 100) (- hsize *WindowRight*) (- vsize *windowBottom*))
    ))
;;;(resized searcher)


(define-handler EnteringStage (Searcher)
  (setf (text me) (concatenate 'string (objectstring (targetproject ui)) " Searcher"))
  (setf (keytarget me) SearcherSearchField)
  (setSelection (textfield SearcherSearchField) 0 -1))

(new uiBigLabel :objectname "SearcherFindLabel" :project ui)
(setf (container SearcherFindLabel) Searcher)
(setf (text SearcherFindLabel) "Find:")
(sendtoback SearcherFindLabel)

(new PickerMenu :objectname "SearchSpace" :project ui)
(addparent searchspace uipopup)
(setf (container searchSpace) searcher)
(setf (items searchSpace) 
      (list "Anything" "Objects" "Properties" "Handlers" "Functions" "Constants" "Globals" ))
(define-handler menuselect (searchSpace)
  (withActorLocked (me)
    (setf (textstyle (picker SearcherObjectList)) '(italic))
    (setf (text me) (selecteditem me))
    (when (and (string= (text SearchStyle) "that References")
               (or (string= (selecteditem me) "Constants") 
                   (string= (selecteditem me) "Globals")))
      (setf (selecteditem SearchStyle) "that Reference")
      (menuselect SearchStyle)
      )
    (when (and (string= (text SearchStyle) "that Reference")
               (string= (selecteditem me) "Anything"))
      (setf (selecteditem SearchStyle) "that References")
      (menuselect SearchStyle)
      )
    (when (or (and (string= (text SearchStyle) "with Script Text")
                   (not (or (string= (selecteditem me) "Anything") 
                            (string= (selecteditem me) "Handlers")
                            (string= (selecteditem me) "Functions"))))
              (and (or (string= (text SearchStyle) "that References")
                       (string= (text SearchStyle) "that Reference"))
                   (not (or (string= (selecteditem me) "Anything") 
                            (string= (selecteditem me) "Constants")
                            (string= (selecteditem me) "Globals"))))
              (and (string= (text SearchStyle) "with Value")
                   (not (string= (selecteditem me) "Properties"))))
      (setf (selecteditem SearchStyle) "with Name")
      (menuselect SearchStyle)
      )
    (setBoundsRect searchSpace 18 46 98 66)
    ))
(setf (text searchspace) (car (items searchspace)))

(new PickerMenu :objectname "SearchStyle" :project ui)
(addparent SearchStyle uipopup)
(setf (container SearchStyle) searcher)
(define-handler update (SearchStyle)
  (let ((ot (text SearchSpace)))
    (cond
     ((string= ot "Anything")
      (setf (items me) (list "with Name"  "with Script Text" "with Value" "that References")))
     ((or (string= ot "Handlers") (string= ot "Functions"))
      (setf (items me) (list "with Name"  "with Script Text")))
     ((or (string= ot "Properties") (string= ot "Constants") (string= ot "Globals"))
      (setf (items me) (list "with Name" "with Value" "that Reference")))
     (t
      (setf (items me) (list "with Name" ))
      (setf (defaultmenuitem me) 1)))
    )
  (call-next-method)
  )
(define-handler menuselect (SearchStyle)
  (withActorLocked (me)
    (setf (textstyle (picker SearcherObjectList)) '(italic))
    (setf (text me) (selecteditem me))
    (setBoundsRect SearchStyle 108 46 198 66)
    ))
(setf (text SearchStyle) "with Name")


(new standardqueryfield :container Searcher :objectName "SearcherSearchField" :project ui)
(hide (historymenu SearcherSearchField))
(hide (handle SearcherSearchField))
(define-handler resized (SearcherSearchField)
  (if (eq (KeyTarget (sk8::window me)) (textfield me))
    (browserHighlight me))
  (sk8-multival-bind (hh vv) (size me)
    (setBoundsRect (Insetrect me) 0 0 hh vv)
    (setBoundsRect (TextField me) (if *macstyleinterface* 1 4) (if *macstyleinterface* 1 4)
                   (- hh (if *macstyleinterface* 1 2)) (- vv (if *macstyleinterface* 1 3)))
    ))
(define-handler (setf items) (theval (historymenu SearcherSearchField))
  nil)
(define-handler keyDown ((textfield SearcherSearchField) thechar)
  (when (and (or (eq theChar #\return) (eq theChar #\enter))
             (neq (eventactor) FindObjectFindButton))
    (setf (highlight FindObjectFindButton) t)
    (sleep 0.2)
    (setf (highlight FindObjectFindButton) nil))

  (if (eq theChar #\Tab)
    (setf (keyTarget Searcher) (picker SearcherObjectList))
    (progn
      (setf (textstyle (picker SearcherObjectList)) '(italic))
      (if (and (or (eq theChar #\return) (eq theChar #\enter))
               (or (string= (text SearchStyle) "with Script Text")
                   (string= (text SearchStyle) "with Name"))
               )
        (setf (outputobjects (container me)) (list (text me)))
        (call-next-method)))))

(define-handler (setf outputobjects) (theval SearcherSearchField)
  (sk8dev::withLockedCursor animatedClock
    (call-next-method)
    ;;;;DO THE SEARCH
    (setf theval (car theval))
    (when (or theval (string-equal (text (textfield me)) "false"))  ;;kludge
      (withactorlocked (me)
        (let* ((proj (targetProject ui))
               (okprojs (okprojects proj))
               (txt (text SearchSpace))
               (ak (string= txt "Anything"))
               (exact nil)
               valuetosearch
               res)
          (cond
           ((string= (text SearchStyle) "with Script Text")
            (sk8-multival-bind (h f) (searchScripts proj theval 
                                                    :handlers (not (string= txt "Functions"))
                                                    :Functions (not (string= txt "Handlers"))
                                                    :active t)
              (setf res (nconc h f)))
            )
           ((or (string= (text SearchStyle) "that References")
                (string= (text SearchStyle) "that Reference")
                (string= (text SearchStyle) "with Value"))
            (setf valuetosearch (if (listp theval) (car theval) theval))
            (setf res nil)
            (when (or ak (string= txt "Properties"))
              (sk8-multival-bind (xx yy zz) (references valuetosearch :properties t :globals nil :constants nil 
                                                        :deepsearch (not (string= (text SearchStyle) "with Value"))
                                                        :projects (list proj))
                (declare (ignore xx yy))
                (setf res zz)))
            (when (or ak (string= txt "Globals") (string= txt "Constants"))
              (sk8-multival-bind (xx yy zz) (references valuetosearch :properties nil :globals (or ak (string= txt "Globals")) :constants (or ak (string= txt "Globals")) 
                                                        :deepsearch (not (string= (text SearchStyle) "with Value"))
                                                        :projects okprojs)
                (declare (ignore zz))
                (when xx (setf res (append res xx)))
                (when yy (setf res (append res yy)))))
            )
           ((string= (text SearchStyle) "with Name")
            (when (or ak (string= txt "Handlers"))
              (sk8-multival-bind (xx yy) (getHandlerNameCompletions theval :completion (if exact :exact :any)
                                                                      :handlers t :matches nil :project (targetProject ui))
                (declare (ignore xx))
                (push yy res)))
            (tickeventclock)
            (when (or ak (string= txt "Properties")) 
              (sk8-multival-bind (xx yy) (getPropertyNameCompletions theval :completion (if exact :exact :any)
                                                                       :objects t :matches t :project (targetProject ui))
                ;;;***Should be a property
                (push (mapcar #'(lambda (x y) (list x y)) xx yy)
                      res )
                ))
            (tickeventclock)
            (when (or ak (string= txt "Functions")) 
              (sk8-multival-bind (xx yy) (getFunctionNameCompletions theval :completion (if exact :exact :any)
                                                                       :functions t :matches nil :project (targetProject ui))
                (declare (ignore xx))
                (push yy res )))
            (tickeventclock)
            (when (or ak (string= txt "Globals"))
              (sk8-multival-bind (xx yy) (getGlobalNameCompletions theval :completion (if exact :exact :any)
                                                                     :project (targetProject ui))
                (declare (ignore xx))
                (push yy res )))
            (tickeventclock)
            (when (or ak (string= txt "Constants"))
              (sk8-multival-bind (xx yy) (getConstantNameCompletions theval :completion (if exact :exact :any)
                                                                       :project (targetProject ui))
                (declare (ignore xx))
                (push yy res )))
            (tickeventclock)
            (when (or ak (string= txt "Objects")) 
              (sk8-multival-bind (xx yy) (getObjectNameCompletions theval :completion (if exact :exact :any)
                                                                     :objects t :matches nil :project (targetProject ui))
                (declare (ignore xx))
                (push yy res )))
            (setf res (apply #'nconc res)))
           (t (ed-beep)))
          (tickeventclock)
          (when (intersectionfind searcher)
            (setf res (intersection res (items (picker SearcherObjectList)))))
          (setf (textstyle (picker SearcherObjectList)) '(plain))
          (unless res 
            (setf res (list "No items found."))
            (setf (textstyle (picker SearcherObjectList)) '(bold)))
          (setf (inputobjects (picker SearcherObjectList)) res)
          (setf (intersectionfind searcher) nil)
          )))))
  
(new uitextlistforcorners :objectname "SearcherObjectList" :project ui)
(setf (container SearcherObjectList) Searcher)
(setf (title SearcherObjectList) "Result:")
(setf (items SearcherObjectList) nil)
(setf (alphabeticalDisplay (picker SearcherObjectList)) t)
(define-handler keyDown ((picker SearcherObjectList) thechar)
  (cond
   ((eq theChar #\Tab)
    (setf (keyTarget Searcher) (textfield SearcherSearchField)))
   (t
    (call-next-method))))

(defun uidisplaystring (theval)
  (cond
   ((not theval)
    "")
   ((listp theval) 
    (concatenate 'string 
                 (name (car theval))
                 " of "
                 (objectstring (cadr theval) :project (targetproject ui))
                 ))
   ((symbolp theval) 
    (concatenate 'string 
                 (if (constantp theval)
                   "The constant  "
                   "The global  "
                   )
                 (objectstring theval :project (targetproject ui))
                 ))
   (t
    (objectstring theval :project (targetproject ui)))
   ))

(define-handler createTextDisplayItem ((picker SearcherObjectList) theval)
  (if (stringp theval)
    theval
    (uidisplaystring theval)
  ))

(define-handler extendedMouseDown ((picker SearcherObjectList))
  (let ((sels (car (selecteditems me)))
        draggee
        )
    (cond
     ((listp sels)
      (progn
        (setf (boundsrect PropertyDataRect :physical t) (selectedItemsBoundsRect me))
        (setf (objects PropertyDataRect) (list (cadr sels)))
        (setf (propertyname PropertyDataRect) (car sels))
        (setf (ComponentFrom PropertyDataRect) me)
        (withcursor standardcursor
          (drag PropertyDataRect :live nil :onstage t :draggingMouseWithinEvents t :draggedOverEvents t :dropevents t)
          )
        t))
     ((symbolp sels)
      (setf draggee (if (constantp sels) constantdatarect globaldatarect))
      (setf (boundsrect draggee :physical t) (selectedItemsBoundsRect me))
      (if (constantp sels)
        (setf (constants draggee) (list sels))
        (setf (globals draggee) (list sels)))
      (setf (ComponentFrom draggee) me)
      (withcursor standardcursor
        (drag draggee :live nil :onstage t :draggingMouseWithinEvents t :draggedOverEvents t :dropevents t)
        )
      t)
     ((functionp sels)
      (setf (boundsrect functionDataRect :physical t) (selectedItemsBoundsRect me))
      (setf (functions functionDataRect) (list sels))
      (setf (ComponentFrom functionDataRect) me)
      (withcursor standardcursor
        (drag functionDataRect :live nil :onstage t :draggingMouseWithinEvents t :draggedOverEvents t :dropevents t))
      t)
     ((and (stringp sels) t)
      nil)
     (t
      (call-next-method))
     )))

(define-handler doubleclick ((picker SearcherObjectList))
  (let ((sels (car (selecteditems me))))
    (if (listp sels)
      (setf (selecteditems (picker (psheet (uiedit (cadr sels))))) (list (car sels)))
      (if (symbolp sels)
        (ed-beep)  ;;;should bring up project viewer?
        (uiedit sels))
      )))

(new UIButton :objectname "FindObjectFindButton" :project ui)
(setf (container FindObjectFindButton) Searcher)
(setf (text FindObjectFindButton) "Find")

(define-handler click (FindObjectFindButton)
  (keydown (textfield SearcherSearchField) #\Return))

(new UIButton :objectname "IntersectionFindButton" :project ui)
(setf (container IntersectionFindButton) Searcher)
(setf (text IntersectionFindButton) "Intersection")
(define-handler click (IntersectionFindButton)
  (setf (intersectionfind searcher) t)
  (keydown (textfield SearcherSearchField) #\Return))

(setSize Searcher 215 280)
(resized Searcher)
(setf (minimumsize searcher) '(210 235))
(setf (top Searcher :resizing nil) 100)
(setf (left Searcher :resizing nil) 200)



#|
	Change History (most recent last):
	1	5/25/93	Brian Roddy	Happy Day-- Hello and welcome to source server
	2	5/25/93	Brian Roddy	
	3	5/25/93	Brian Roddy	
	4	5/25/93	rod	UGGGHHH Menus should/better/will for sure work
	5	6/25/93	Brian Roddy	
	9	10/11/93	rod	
	10	10/25/93	rod	BrowserComponentized this.  
				***Still needs checks for properties functions and globals.
	11	11/2/93	rod	
	15	11/30/93	rod	Removed accept button
	23	12/17/93	till	#.'s be gone: keydown, keydown.
	25	1/14/94	hernan	
	26	2/21/94	hernan	The Final Renaming for Alpha! (yeah, right...)
	27	2/23/94	rod	
	28	2/26/94	rod	hilighter->highlighter
	29	2/28/94	rod	
	30	3/3/94	rod	New Style Searcher, COMING AT YA!
	31	3/4/94	kleiman	ed-fred -> editData
	32	3/8/94	rod	
	33	3/8/94	rod	
	34	3/8/94	kleiman	typo in creating "Searcher" object corrected
	35	3/9/94	rod	Doing Project Switching and Reference Clearing.
	36	3/9/94	rod	Doing Project Switching and Reference Clearing.
	37	3/10/94	rod	
	38	3/16/94	rod	Fixing various problems...
	39	3/26/94	rod	Minimum size stuff.
	44	4/11/94	rod	Removing reference to simple window.  No longer
				needed in the redesign of the UI.
	45	4/12/94	Hernan	Avoiding use of contents when it is not necesary.
	45	4/12/94	Hernan	Second try!
	49	5/22/94	rod	Here begins the experimentation with a Mac look 
				to the UI.
	50	6/10/94	kleiman	Any Kind" -> "Anything" in search field
	51	6/12/94	rod	Getting rid of search by reference as we have
				no functions to support it.  Fixing bug where
				the search style could be script text when the
				search space is objects, etc.
	52	6/17/94	rod	Fixing objectstring
	53	6/23/94	rod	
	54	6/23/94	rod	
	55	6/29/94	chip	!legalVarName? --> legal-ident-name?
	56	7/6/94	rod	Getting rid of UIColorize.
	57 	 8/25/94	rod     	
	58 	 8/30/94	rod     	
	59 	 8/30/94	rod     	Making resized call-next-method
	60 	 9/ 6/94	rod     	
	61 	 9/29/94	rod     	Fixing completion functions.
	62 	10/31/94	rod     	
	63 	10/31/94	rod     	
	64 	11/ 1/94	rod     	removing debugging code.
	65 	12/22/94	rod     	Adding finding globals and constants by value.
	66 	 1/17/95	rod     	Adding ability to search for all references.
	67 	 2/ 1/95	rod     	Changing default location of window
	68 	 2/ 3/95	Hernan  	WithLockedCursor is now correct. I check this file
							out and back in to force it to be recompiled.
	69 	 2/ 9/95	sidney  	1206676: Searchscripts was not returning function info in format expected by caller
	70 	 2/14/95	rod     	oops, making sure that setting the location occurs
							after setting the size.
	71 	 2/20/95	rod     	Making functions constants and vars drag 
							properly.
	72 	 3/ 1/95	rod     	
	73 	 4/ 3/95	Hernan  	1235429: changing the window title of the Searcher to "Searcher
	74 	 4/12/95	rod     	Fixing searcher to be nice if it doesn't find
							anything.
	75 	 4/13/95	rod     	
	2  	 6/23/95	Hernan  	Need to process the values returned from searchscripts 
							with the two mapcar statements.
	3  	12/11/95	Brian   	Cleaning up old calls to sk8script functions.
	4  	 2/14/96	Brian   	cleaning up api to searchScripts to be SK8 compliant.
	2  	 2/27/97	Hernan  	
|# ;(do not edit past this line!!)
