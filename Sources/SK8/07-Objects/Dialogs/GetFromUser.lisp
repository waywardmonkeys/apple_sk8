;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories

(in-package :sk8dev)

(provide "GETFROMUSER")

(require "DIALOGBOX" "objects;Dialogs:DialogBox")
(require "BROWSERCOMPONENTS" "objects;Browser Components:BrowserComponents")
(require "MODALDIALOGMODE" "objects;Dialogs:ModalDialogMode")

;;; _____________________________________________________________________________
;;; _____________________________________________________________________________
;;; _____________________________________________________________________________
;;; _____________________________________________________________________________
;;;                GetFromUser
;;; _____________________________________________________________________________
;;; _____________________________________________________________________________
;;; _____________________________________________________________________________
;;; _____________________________________________________________________________

(new DialogBox :objectname "GetFromUserDialogBox" :project sk8)

(new HierarchicalObjectList :objectname "GetFromUserDialogTextList" :project sk8)
(setf (container GetFromUserDialogTextList) GetFromUserDialogBox)
(setf (title GetFromUserDialogTextList) "Select from ")
(setf (relation (picker GetFromUserDialogTextList)) 'KnownChildren)

(new DialogBoxHighlightedButton :objectname "GetFromUserDialogYesButton" :project sk8)
(setf (container GetFromUserDialogYesButton) GetFromUserDialogBox)
(setf (text GetFromUserDialogYesButton) "Select")

(define-handler click (GetFromUserDialogYesButton)
  (let ((ret (if (eq (selectionstyle (picker GetFromUserDialogTextList)) 'single)
               (value (car (SelectedItems GetFromUserDialogTextList)))
               (mapcar #'value (SelectedItems GetFromUserDialogTextList)))))
    (call-next-method)
    (setf (container (container me)) nil)
    (exitModalState ret)))

(new DialogBoxCancelButton :objectname "GetFromUserDialogCancelButton" :project sk8)
(setf (container GetFromUserDialogCancelButton) GetFromUserDialogBox)

#|
(new DialogBoxButton :objectname "GetFromUserDialogNewButton" :project sk8)
(setf (container GetFromUserDialogNewButton) GetFromUserDialogBox)
(setf (text GetFromUserDialogNewButton) "New")
(define-handler click (GetFromUserDialogNewButton)
  (exitModalState (getnewfromuser (value (car (SelectedItems GetFromUserDialogTextList))))))
|#

(define-handler doubleclick ((picker GetFromUserDialogTextList))
  (click GetFromUserDialogYesButton))

(define-handler extendedmousedown ((picker GetFromUserDialogTextList))
  )

(define-handler keyDown ((picker GetFromUserDialogTextList) thechar)
  (if (or (eq theChar #\Enter) (eq theChar #\Return))
    (click GetFromUserDialogYesButton)
    (call-next-method)))

(define-handler EnteringStage (GetFromUserDialogBox)
  (call-next-method)
  (setf (keyTarget me) GetFromUserDialogTextList))

(define-handler resized (GetFromUserDialogBox)
  (withActorLocked (me)
    (call-next-method)
    (sk8-multival-bind (x y) (size me)
      (setBoundsRect GetFromUserDialogTextList 5 5 (- x 5) (- y 40))
      (setBoundsRect GetFromUserDialogYesButton (- x 88) (- y 33) (- x 10) (- y 7))
      ;;;(setBoundsRect GetFromUserDialogNewButton (- x 150) (- y 32) (- x 87) (- y 10))
      (setBoundsRect GetFromUserDialogCancelButton (- x 168) (- y 29) (- x 98) (- y 11)))))

(resized GetFromUserDialogBox)

(define-handler leavingStage (GetFromUserDialogBox)
  (call-next-method)
  (setf (items GetFromUserDialogTextList) nil))


(define-sk8-function GetFromUserDialog nil (Obj &key
                                                   (textFont "Geneva")
                                                   (textsize 10)
                                                   (width 300)
                                                   (height 250)
                                                   (h nil)
                                                   (v nil)
                                                   (title nil)
                                                   (swatches nil)
                                                   ((:project InProject) nil)
                                                   (multipleValues nil)
                                                   (cancelString "Cancel")
                                                   (buttonString "Select"))
  (withcursor watchcursor
    (unless (and h v)
      (sk8-multival-bind (hh vv) (mainMonitorcenter)
        (if (not h) (setf h hh))
        (if (not v) (setf v vv))))
    (setlocation GetFromUserDialogBox h v)
    (if multiplevalues
      (progn
        (setf (selectionstyle (picker GetFromUserDialogTextList)) 'discontiguous)
        (setf (title GetFromUserDialogTextList) "Select items:"))
      (progn
        (setf (selectionstyle (picker GetFromUserDialogTextList)) 'single)
        (setf (title GetFromUserDialogTextList) "Select an item:")))
    (when title (setf (title GetFromUserDialogTextList) title))
    (setf (filterProject (picker GetFromUserDialogTextList)) inProject)
    (setf (textsize (picker GetFromUserDialogTextList)) textsize)
    (setf (textfont (picker GetFromUserDialogTextList)) textfont)
    (setf (swatchesshown (picker GetFromUserDialogTextList)) swatches)
    (setf (iconsize (picker GetFromUserDialogTextList)) '(20 20))
    (setf (inputobjects (picker GetFromUserDialogTextList)) (list obj))
    (openitem (picker GetFromUserDialogTextList) (car (items (picker GetFromUserDialogTextList))))
    (setf (text GetFromUserDialogYesButton) buttonString)
    (setf (text GetFromUserDialogCancelButton) cancelString)
    (setSize GetFromUserDialogBox width height)
    (setf (selecteditems (picker GetFromUserDialogTextList)) (car (items (picker GetFromUserDialogTextList))))
    (modalDialog GetFromUserDialogBox)))

(defmethod getFromUser ((theThing t) &key (multipleValues nil) ((:project InProject) nil) popUpMenu relativeactor)
  (declare (ignore multipleValues inProject popupMenu relativeActor))
  (getNewFromUser theThing))

(define-handler getFromUser (Object &key (multipleValues nil) ((:project InProject) nil) popUpMenu relativeactor)
  (declare (ignore popUpMenu relativeActor))
  (unless inproject (sk8-error GeneralProgrammaticError
                               :strings '("Project argument must be specified.")
                               ))
  (if (knownchildren me)
    (GetFromUserDialog me :multipleValues multipleValues :project inproject)
    (getnewfromuser me :project inproject)))


(define-handler createSwatch ((picker GetFromUserDialogTextList) theitem)
  (cond
   ((is-a theitem renderer)
    theitem)
   ((or (and (neq theitem PixelMap) (inheritsFrom theitem PixelMap))
        (and (neq theitem IconRSRC) (inheritsFrom theitem IconRSRC))
        (and (neq theitem QDPicture) (inheritsFrom theitem QDPicture))
        (and (neq theitem BWPattern) (inheritsFrom theitem BWPattern))
        (and (neq theitem ColorPattern) (inheritsFrom theitem ColorPattern))
        )
    (new imagerenderer :media theitem :project sk8))
   (t
    white)))

(define-handler getFromUser (PixelMap &key (multipleValues nil) ((:project InProject) nil) popUpMenu relativeactor)
  (declare (ignore popUpMenu relativeActor))
  (unless inproject (sk8-error GeneralProgrammaticError
                               :strings '("Project argument must be specified.")
                               ))
  (if (knownchildren me)
    (GetFromUserDialog me :multipleValues multipleValues :project inproject :swatches t)
    (getnewfromuser me :project inproject)))

(define-handler getFromUser (IconRSRC &key (multipleValues nil) ((:project InProject) nil) popUpMenu relativeactor)
  (declare (ignore popUpMenu relativeActor))
  (unless inproject (sk8-error GeneralProgrammaticError
                               :strings '("Project argument must be specified.")
                               ))
  (if (knownchildren me)
    (GetFromUserDialog me :multipleValues multipleValues :project inproject :swatches t)
    (getnewfromuser me :project inproject)))

(define-handler getFromUser (QDPicture &key (multipleValues nil) ((:project InProject) nil) popUpMenu relativeactor)
  (declare (ignore popUpMenu relativeActor))
  (unless inproject (sk8-error GeneralProgrammaticError
                               :strings '("Project argument must be specified.")
                               ))
  (if (knownchildren me)
    (GetFromUserDialog me :multipleValues multipleValues :project inproject :swatches t)
    (getnewfromuser me :project inproject)))

(define-handler getFromUser (bwpattern &key (multipleValues nil) ((:project InProject) nil) popUpMenu relativeactor)
  (declare (ignore popUpMenu relativeActor))
  (unless inproject (sk8-error GeneralProgrammaticError
                               :strings '("Project argument must be specified.")
                               ))
  (if (knownchildren me)
    (GetFromUserDialog me :multipleValues multipleValues :project inproject :swatches t)
    (getnewfromuser me :project inproject)))

(define-handler getFromUser (ColorPattern &key (multipleValues nil) ((:project InProject) nil) popUpMenu relativeactor)
  (declare (ignore popUpMenu relativeActor))
  (unless inproject (sk8-error GeneralProgrammaticError
                               :strings '("Project argument must be specified.")
                               ))
  (if (knownchildren me)
    (GetFromUserDialog me :multipleValues multipleValues :project inproject :swatches t)
    (getnewfromuser me :project inproject)))

#|
	Change History (most recent last):
~~	2  	 4/10/96	Hernan  	define-sk8-function->defun.
	4  	 2/27/97	Hernan  	Putting define-sk8-function back in the code.
|# ;(do not edit past this line!!)
