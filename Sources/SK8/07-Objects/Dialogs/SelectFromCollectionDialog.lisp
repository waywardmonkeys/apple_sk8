;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories

(in-package :sk8dev)

(provide "SELECTFROMCOLLECTIONDIALOG")

(require "DIALOGBOX" "objects;Dialogs:DialogBox")
(require "USERDIALOGS" "functions;User Dialogs")
(require "MODALDIALOGMODE" "objects;Dialogs:ModalDialogMode")
(require "TEXTLIST" "objects;Pickers:TextList")

;;; _____________________________________________________________________________
;;;                             SelectFromCollectionDialog
;;; _____________________________________________________________________________

(new DialogBox :objectname "SelectFromCollectionDialogBox" :project sk8)

(new textlist :objectname "SelectFromCollectionDialogTextList" :project sk8)
(setf (container SelectFromCollectionDialogTextList) SelectFromCollectionDialogBox)
(setf (title SelectFromCollectionDialogTextList) "Select from ")
(setf (textsize (titlebar SelectFromCollectionDialogTextList)) 12)
(setf (textsize (picker SelectFromCollectionDialogTextList)) 12)

(define-handler doubleclick ((picker SelectFromCollectionDialogTextList))
  (click SelectFromCollectionDialogYesButton))

(define-handler keyDown ((picker SelectFromCollectionDialogTextList) thechar)
  (if (or (eq theChar #\Enter) (eq theChar #\Return))
    (click SelectFromCollectionDialogYesButton)
    (call-next-method)))

(new DialogBoxHighlightedButton :objectname "SelectFromCollectionDialogYesButton" :project sk8)
(setf (container SelectFromCollectionDialogYesButton) SelectFromCollectionDialogBox)
(setf (text SelectFromCollectionDialogYesButton) "Select")

(define-handler click (SelectFromCollectionDialogYesButton)
  (call-next-method)
  (setf (container (container me)) nil)
  (exitModalState (if (eq (selectionstyle (picker SelectFromCollectionDialogTextList)) 'single)
                    (car (SelectedItems SelectFromCollectionDialogTextList))
                    (SelectedItems SelectFromCollectionDialogTextList))))

(new DialogBoxCancelButton :objectname "SelectFromCollectionDialogCancelButton" :project sk8)
(setf (container SelectFromCollectionDialogCancelButton) SelectFromCollectionDialogBox)

(define-handler EnteringStage (SelectFromCollectionDialogBox)
  (call-next-method)
  (setf (keyTarget me) SelectFromCollectionDialogTextList))

(define-handler resized (SelectFromCollectionDialogBox)
  (withActorLocked (me)
    (call-next-method)
    (sk8-multival-bind (x y) (size me)
      (sk8::sk8-multival-bind (hact1 vact1) (actortextsize SelectFromCollectionDialogCancelButton)
        (declare (ignore vact1))
        (sk8::sk8-multival-bind (hact2 vact2) (actortextsize SelectFromCollectionDialogYesButton)
          (declare (ignore vact2))
          (let* ((y1 (- y 11))
                 (y2 (- y 29)))
            (unless (> hact1 50) (setf hact1 50))
            (unless (> hact2 50) (setf hact2 50))
            (setboundsrect SelectFromCollectionDialogTextList 10 10 (- x 10) (- y2 10))
            (setBoundsRect SelectFromCollectionDialogCancelButton (- x hact2 hact1 30) y2 (- x hact2 25) y1)
            (setBoundsRect SelectFromCollectionDialogYesButton (- x hact2 15) (- y2 4) (- x 7) (+ y1 4))))))))

(define-sk8-function SelectFromCollectionDialog nil (candidateList &key
                                                                        (textFont GenevaFont)
                                                                        (textsize 9)
                                                                        (width 300)
                                                                        (height 248)
                                                                        (h nil)
                                                                        (v nil)
                                                                        (title nil)
                                                                        (alphabeticaldisplay t)
                                                                        (multipleValues nil)
                                                                        (cancelString "Cancel")
                                                                        (buttonString "Select")
                                                                        ((:system UseSystem) nil))
  (unless (and h v)
    (sk8-multival-bind (hh vv) (mainMonitorcenter)
      (if (not h) (setf h hh))
      (if (not v) (setf v vv))))
  (if UseSystem
    (MacSelectFromCollectionDialog candidateList :multipleValues multipleValues :title title :buttonString buttonString)
    (progn
      (setlocation SelectFromCollectionDialogBox h v)
      (if multiplevalues
        (progn
          (setf (selectionstyle (picker SelectFromCollectionDialogTextList)) 'discontiguous)
          (setf (title SelectFromCollectionDialogTextList) "Select items:"))
        (progn
          (setf (selectionstyle (picker SelectFromCollectionDialogTextList)) 'single)
          (setf (title SelectFromCollectionDialogTextList) "Select an item:")))
      (setf (alphabeticaldisplay (picker SelectFromCollectionDialogTextList)) alphabeticaldisplay)
      (when title (setf (title SelectFromCollectionDialogTextList) title))
      (setf (textsize (picker SelectFromCollectionDialogTextList)) textsize)
      (setf (textfont (picker SelectFromCollectionDialogTextList)) textfont)
      (setf (textfont (titlebar SelectFromCollectionDialogTextList)) ChicagoFont)
      (setf (items SelectFromCollectionDialogTextList) candidatelist)
      (setf (text SelectFromCollectionDialogYesButton) buttonString)
      (setf (text SelectFromCollectionDialogCancelButton) cancelString)
      (setSize SelectFromCollectionDialogBox width height)
      (resized SelectFromCollectionDialogBox)
      (setf (selecteditems (picker SelectFromCollectionDialogTextList)) (car (items (picker SelectFromCollectionDialogTextList))))
      (modalDialog SelectFromCollectionDialogBox))))


#|
	Change History (most recent last):
	2  	 4/10/96	Hernan  	define-sk8-function->defun.
	4  	 2/27/97	Hernan  	Putting define-sk8-function back in the code.
|# ;(do not edit past this line!!)
