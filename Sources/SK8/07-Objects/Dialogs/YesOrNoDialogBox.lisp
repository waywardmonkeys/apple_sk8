;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories

(in-package :sk8dev)

(provide "YESORNODIALOGBOX")

(require "DIALOGBOX" "objects;Dialogs:DialogBox")
(require "USERDIALOGS" "functions;User Dialogs")

;;; _____________________________________________________________________________
;;;                             YesOrNoDialog
;;; _____________________________________________________________________________

(new DialogBox :objectname "YesOrNoDialogBox" :project sk8)
(setlocation YesOrNoDialogBox 333 205)
(setSize YesOrNoDialogBox 368 105)

(new DialogBoxEditText :objectName "YesOrNoDialogDisplayer" :project sk8)
(setf (container YesOrNoDialogDisplayer) YesOrNoDialogBox)
(setf (textSize YesOrNoDialogDisplayer) 12)
(setf (textStyle YesOrNoDialogDisplayer) '(plain))
(setf (frameSize YesOrNoDialogDisplayer) '(0 0))
(setf (mousesensitivity YesOrNoDialogDisplayer) 'transparent)
(setf (fillcolor YesOrNoDialogDisplayer) (fillcolor yesornodialogbox))
(locktext YesOrNoDialogDisplayer)

(new DialogBoxHighlightedButton :objectname "YesOrNoDialogYesButton" 
     :container YesOrNoDialogBox :text "Yes" :project sk8)
(define-handler click (YesOrNoDialogYesButton)
  (call-next-method)
  (setf (container YesOrNoDialogBox) nil)
  (exitModalState t))

(new DialogBoxButton :objectname "YesOrNoDialogNoButton" :project sk8)
(setf (container YesOrNoDialogNoButton) YesOrNoDialogBox)
(setf (text YesOrNoDialogNoButton) "No")
(define-handler click (YesOrNoDialogNoButton)
  (call-next-method)
  (setf (container YesOrNoDialogBox) nil)
  (exitModalState nil))

(new DialogBoxCancelButton :objectName "YesOrNoDialogCancelButton" :project SK8)
(setf (container YesOrNoDialogCancelButton) YesOrNoDialogBox)

(define-handler EnteringStage (YesOrNoDialogBox)
  (call-next-method)
  (setf (keyTarget me) YesOrNoDialogYesButton))

(define-handler resized (YesOrNoDialogBox)
  (withActorLocked (me)
    (call-next-method)
    (sk8-multival-bind (x y) (size me)
      (sk8::sk8-multival-bind (hact1 vact1) (actortextsize YesOrNoDialogCancelButton)
        (declare (ignore vact1))
        (sk8::sk8-multival-bind (hact2 vact2) (actortextsize YesOrNoDialogNoButton)
          (declare (ignore vact2))
          (sk8::sk8-multival-bind (hact3 vact3) (actortextsize YesOrNoDialogYesButton)
            (declare (ignore vact3))
            (let* ((y1 (- y 11))
                   (y2 (- y 29)))
              (unless (> hact1 50) (setf hact1 50))
              (unless (> hact2 50) (setf hact2 50))
              (unless (> hact3 50) (setf hact3 50))
              (sendToBack YesOrNoDialogDisplayer)
              (setBoundsRect YesOrNoDialogDisplayer 7 7 (- x 7) (- y2 5))
              (setBoundsRect YesOrNoDialogCancelButton (- x hact3 hact2 hact1 45) y2 (- x hact3 hact2 40) y1)
              (setBoundsRect YesOrNoDialogNoButton (- x hact3 hact2 30) y2 (- x hact3 25) y1)
              (setBoundsRect YesOrNoDialogYesButton (- x hact3 15) (- y2 4) (- x 7) (+ y1 3))
              )))))))

(resized YesOrNoDialogBox)

(define-sk8-function YesOrNoDialog nil (message &key
                                                   (textFont ChicagoFont)
                                                   (textsize 12)
                                                   (width 350)
                                                   (height nil)
                                                   (h nil)
                                                   (v nil)
                                                   (cancel t)
                                                   (yesText "Yes")
                                                   (noText "No")
                                                   ((:system UseSystem) nil))
  (unless (and h v)
    (sk8-multival-bind (hh vv) (mainMonitorcenter)
      (if (not h) (setf h hh))
      (if (not v) (setf v vv))))
  (if UseSystem
    (progn
      (unless height (setf height 100))
      (MacYesOrNoDialog message :yesText yesText :noText noText
                        :width width :height height :cancelText (if cancel "Cancel" nil)
                        :top (- v (round height 2)) :left (- h (round width 2))))
    (progn
      (setlocation YesOrNoDialogBox h v)
      (setf (textsize YesOrNoDialogDisplayer) textsize)
      (setf (textfont YesOrNoDialogDisplayer) textfont)
      (setf (text YesOrNoDialogNoButton) noText)
      (setf (text YesOrNoDialogYesButton) yesText)
      (setf (text YesOrNoDialogDisplayer) message)
      (unless height 
        (sk8-multival-bind (hh vv) (actortextsize YesOrNoDialogDisplayer)
          (declare (ignore hh))
          (setf height (min 340 (+ vv 45)))))
      (setSize YesOrNoDialogBox width height)
      (resized YesOrNoDialogBox)
      (if (or (equal notext "cancel") (equal notext "Cancel")) (setf cancel nil))
      (unless cancel (moveoffstage YesOrNoDialogCancelButton))
      (modalDialog YesOrNoDialogBox))))

#|
	Change History (most recent last):
	2  	 4/10/96	Hernan  	define-sk8-function->defun.
	4  	 2/27/97	Hernan  	Putting define-sk8-function back in the code.
|# ;(do not edit past this line!!)
