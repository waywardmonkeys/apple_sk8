;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories



(in-package :uidev)

(SK8-declare-syms :SK8 :public ; Updated 10-03-95   3:09 pm
                  SK8::PREFERENCES)

;;;(loadPreferences ui websk8r)
;;;(savePreferences ui websk8r)

(defun checkForPrefsProperty (proj)
  (if (or (eq proj ui) (eq proj sk8))
    nil
    (progn 
      (unless (memq 'sk8::preferences (properties proj))
        (addproperty proj 'sk8::preferences))
      t))) 

(defun windowbounds (me)
  (sk8-multival-bind (ll tt rr bb) (boundsrect me)
    (sk8-multivals ll tt rr (if (shadesize me) (+ tt (car (ShadeSize me))) bb))))
;;;(windowbounds drawpalette)
    

(define-handler savePreferences (ui theProj)
  (when (and theProj (checkForPrefsProperty theProj))
    (setf (SK8::preferences theProj) nil)
    (dolist (i (list drawpalette objecteditor systemBrowser inheritanceViewer
                     ProjectOverviewer StageOverviewer handlertracer
                     colorpalette searcher MenuEditor
                     UIScriptEditor
                     MediaBrowser MessageBox))
      (savepreferences i theProj))
    (setf (SK8::preferences theProj) (reverse (SK8::preferences theProj)))
    (push (remove-if-not #'(lambda (x) (eq (project x) (project theProj)))
                         (uiClearedObjects))
          (SK8::preferences theProj))
    ))
(define-handler loadPreferences (ui theProj)
  (when (and theProj (checkForPrefsProperty theProj))
    (let ((x (SK8::preferences theProj)))
      (when (and x (= (length x) 15))   ;;15 is the number of things below + 1 for clears.  Just in case we modify it this will error check
        
        (mapc #'addClearedObject (pop (SK8::preferences theProj)))

        (dolist (i (list drawpalette objecteditor systemBrowser inheritanceViewer
                         ProjectOverviewer StageOverviewer handlertracer 
                         colorpalette searcher MenuEditor
                         UIScriptEditor
                         MediaBrowser MessageBox))
          (loadPreferences i theProj)
          (moveonstage i))
        (setf (SK8::preferences theProj) nil)  ;;;****CLEAR REFERENCES!!!!
        ))
    ))

(define-handler savePreferences (uisimplewindow theProj)
  (push (list (container me) (windowbounds me)) (SK8::preferences theProj) )
  )

(define-handler loadPreferences (uisimplewindow theProj)
  (let ((val (pop (SK8::preferences theProj))))
    (when (car val)
      (unless (equal (boundsrect me) (cadr val))
        (if (shadesize me) (setf (shadesize me) nil))
        (setf (boundsrect me) (cadr val)))
      (unless (eq (container me) (car val))
        (setf (container me) (car val)))
      )))

(define-handler savePreferences (objecteditor theProj)
  (let (vals)
    (dolist (i (knownchildren me))
      (when (eq (container i) stage)
        (setf vals (windowbounds i))
        (return)))
    (setf vals (list vals (remove-if-not #'(lambda (x) (and (not (listp x))
                                                            (memq (project x) (okprojects (project theProj)))
                                                            (or (eq (project x) (project theProj)) (objectname x)))
                                            ) (items (historymenu OENameField)))))
    (push vals (SK8::preferences theProj))))

(define-handler loadPreferences (objecteditor theProj)
  (let ((val (pop (SK8::preferences theProj)))
        (objeds (remove-if-not #'(lambda (x) (is-a x objecteditor)) (contents stage)))
        objed)
    (when (and val (listp val) (not (listp (car (car val)))))
      (setf (items (historymenu OENameField)) (remove-duplicates (append (cadr val) (items (historymenu OENameField)))))
      (if objeds
        (progn
          (setf objed (car objeds))
          (unless (equal (boundsrect (car objeds)) (car val))
            (if (shadesize me) (setf (shadesize me) nil))
            (setf (boundsrect (pop objeds)) (car val))))
        (setf objed (editobjects nil :boundsrect (car val))))
      (setf (inputobjects objed) (car (items (historymenu OENameField))))
      )))


(define-handler savePreferences (UIScriptEditor theProj)
  (let ((prefprojs (okprojects (project theProj))))
    (push (remove-if-not #'(lambda (x) (memq (car x) prefprojs))
                         (items (historymenu (nfield UIscripteditor))))
          (SK8::preferences theProj)))
  )

(define-handler loadPreferences (UIScriptEditor theProj)
  (setf (items (historymenu (nfield UIscripteditor))) 
        (remove-duplicates (append (pop (SK8::preferences theProj)) (items (historymenu (nfield UIscripteditor))))
                           :key #'second)))


(define-handler savePreferences (drawpalette theProj)
  (when (eq (targetproject me) (project theProj)) (storeTools (project theProj) (tools me)))
  (push (list (container me) (windowbounds me) (gettools (project theProj))) (SK8::preferences theProj) ))

(define-handler loadPreferences (drawpalette theProj)
  (let ((val (pop (SK8::preferences theProj))))
    (if (shadesize me) (setf (shadesize me) nil))
    (unless (eq (container me) (car val))
      (setf (container me) (car val)))
    (storetools (project theProj) (third val))
    (setupfor (project theProj))
    (bestsize me)
    (sk8-multival-bind (xx yy) (size me)
      (setboundsrect me (car (second val)) (cadr (second val)) (+ (car (second val)) xx) (+ (cadr (second val)) yy)))
    ))


(define-handler savePreferences (ProjectOverviewer theProj)
  (push (list (container me) (windowbounds me) 
              (remove-if-not #'(lambda (x) (or (is-a x handler)
                                               (eq (project x) (project theProj))
                                               (objectname x))) (items POObjectPile)))
        (SK8::preferences theProj) ))

(define-handler loadPreferences (ProjectOverviewer theProj)
  (let ((val (pop (SK8::preferences theProj))))
    (unless (equal (boundsrect me) (cadr val))
      (if (shadesize me) (setf (shadesize me) nil))
      (setf (boundsrect me) (cadr val)))
    (unless (eq (container me) (car val))
      (setf (container me) (car val)))
    (setf (items POObjectPile) (third val))
    (storepile (project theProj) (third val))
    ))



#|
;;;; object is in selection.lisp for compile reasons...
(setf (haloSize UIPreferences) '(8 8))
(setf (HaloOutSet UIPreferences) 2)
(setf (ShowSelectionHaloTitleBar UIPreferences) t)
(when t ;;;; should check processor here!!!
  (setf (LiveMotion UIPreferences) t))

(define-handler (setf haloSize) :after (theValue UIPreferences)
                (setHaloSize Selectionhalo (car theValue) (cadr theValue) 
                             :withOutset (halooutset uipreferences)))

(define-handler (setf HaloOutSet) :after (theValue UIPreferences)
                (setHaloSize Selectionhalo 
                             (car (haloSize uipreferences)) 
                             (cadr (haloSize uipreferences)) 
                             :withOutset theValue))

(define-handler (setf ShowSelectionHaloTitleBar) :after (theValue UIPreferences)
                (bestSize (titlebar selectionhalo)))

(new uisimplewindow :objectname "uiPreferencesWindow" :project ui)
(setf (fillcolor uiPreferencesWindow) uiDarkTexture)

(define-handler editedObjects (uiPreferencesWindow)
  (list UIPreferences))

(define-handler updateEditor (uiPreferencesWindow)
  (withActorLocked (me)
    (let ((theTitleBar (titleBar me)))
      (dolist (i (contents me))
        (unless (eq i theTitleBar)
          (readvalue i))))))

(setSize uiPreferencesWindow 182 130)
(setf (location uiPreferencesWindow) (mainmonitorcenter))

(new uiTitlebar :objectname "uiPrefsTbar" :project ui)
(setf (container uiPrefsTbar) uiPreferencesWindow)
(setf (text uiPrefsTbar) "UI Preferences")
(tagPart uiPreferencesWindow uiPrefsTbar 'TitleBar)


(define-handler activate (uiPreferencesWindow)
  (call-next-method)
  (updateEditor me)
  )

;;__________________________________________________________________
;; HaloSize

(new pe2Numbers :objectName "uiPrefsHaloSize" :project ui)
(setf (labelText uiPrefsHaloSize) "Halo Size")
(setf (container uiPrefsHaloSize) uiPreferencesWindow)
(setf (myProperty uiPrefsHaloSize) 'ui::HaloSize)
(setBoundsRect uiPrefsHaloSize 104 8 194 41)
(setSize uiPrefsHaloSize 90 33)
(setLocation uiPrefsHaloSize 55 55)

;;__________________________________________________________________
;; HaloOutSet

(new pe1Number :objectName "uiPrefsHaloOutSet" :project ui)
(setf (labelText uiPrefsHaloOutSet) "Halo Outset")
(setf (container uiPrefsHaloOutSet) uiPreferencesWindow)
(setf (myProperty uiPrefsHaloOutSet) 'ui::HaloOutSet)
(setBoundsRect uiPrefsHaloOutSet 120 158 171 191)
(setSize uiPrefsHaloOutSet 65 33)
(setLocation uiPrefsHaloOutSet 140 55)

;;__________________________________________________________________
;; Live Motion

(new pe1boolean :objectName "uiPrefsLiveMotion" :project ui)
(setf (labelText uiPrefsLiveMotion) "Live Motion")
(setf (container uiPrefsLiveMotion) uiPreferencesWindow)
(setf (myProperty uiPrefsLiveMotion) 'ui::LiveMotion)
(setSize uiPrefsLiveMotion 85 17)
(setLocation uiPrefsLiveMotion 55 90)

;;__________________________________________________________________
;; Show UI TitleBar

(new pe1boolean :objectName "uiPrefsShowHalo" :project ui)
(setf (labelText uiPrefsShowHalo) "Show Halo Title")
(setf (container uiPrefsShowHalo) uiPreferencesWindow)
(setf (myProperty uiPrefsShowHalo) 'ui::ShowSelectionHaloTitleBar)
(setSize uiPrefsShowHalo 115 17)
(setLocation uiPrefsShowHalo 70 110)




(define-handler resized (uiPreferencesWindow)
  (let (hsize vsize)
    (declare (special hsize vsize))
    (sk8-multival-bind (hh vv) (size me)
      (setf hsize hh)
      (setf vsize vv))
    (bestSize (titlebar me))
    ))

(resized uiPreferencesWindow)
;;; (setf (container uiPreferencesWindow) stage)
|#



#|
	Change History (most recent last):
	2	5/11/93	Brian Roddy	
	3	5/11/93	Brian Roddy	moved the preferences object to selection for load time
	10	2/21/94	hernan	The Final Renaming for Alpha! (yeah, right...)
	11	2/25/94	rod	Redid.  Now these guys save the window 
				positions of all ui tools with a project.
				Old stuff may be resurrected later.
	12	2/28/94	rod	
	13	3/1/94	rod	
	14	3/2/94	kleiman	object-p -> typep; obsoleted object-p
	15	3/8/94	rod	
	16	3/9/94	rod	Doing Project Switching and Reference Clearing.
	17	3/14/94	rod	
	18	3/17/94	rod	Saving histories and object pile of project 
				overviewer.
	19	3/18/94	rod	
	20	3/23/94	rod	
	21	4/11/94	Brian	
	22	4/13/94	Hernan	Avoiding use of contents when possible.
	23	5/6/94	rod	
	24	6/8/94	sidney	save and load preferences of UIScripteditor were breaking and I think I fixed it
	25	6/13/94	rod	1167878:  Now it remembers one object editor 
				and initializes it with the first item in the history menu.
	26	6/17/94	rod	Ensuring Palette shows the User Palette
				Objects.
	27	6/29/94	chip	case-saving-intern --> intern-symbol/intern-legal-varName/intern-legal-handlerName (for radar #107741)
	28 	 9/12/94	rod     	
	29 	10/19/94	rod     	Removing libraries.
	30 	12/ 5/94	rod     	saving clears...
	31 	 1/30/95	rod     	
	32 	 2/ 1/95	rod     	adding handlertracer and stackwatcher.
	33 	 2/ 1/95	rod     	Fixing preferences.  GetPreferenceObject for SK8 
							would make an object in sk8 which would screw
							everything up.
	34 	 2/14/95	sidney  	remove a debugging print statement that was left in a function
	35 	 2/16/95	rod     	moving setting clears to the prefereces...
	36 	 4/12/95	rod     	Fixing draw palette location preference.
	2  	10/ 3/95	Brian   	Making this work by a property rather than by
						an object so we can work with sub projects
	3  	12/11/95	Brian   	
	4  	 2/ 6/96	Hernan  	helpWindow no longer with us.
	5  	 2/14/96	Brian   	fixing population stuff
	6  	 2/14/96	Brian   	removing mf::docontents
	2  	 2/27/97	Hernan  	
|# ;(do not edit past this line!!)
