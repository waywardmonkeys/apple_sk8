;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories

(in-package :sk8dev)

(provide "GETANSWERFROMUSER")

(require "DIALOGBOX" "objects;Dialogs:DialogBox")
(require "HALO" "objects;Shapes:Halo")
(require "USERDIALOGS" "functions;User Dialogs")
(require "MODALDIALOGMODE" "objects;Dialogs:ModalDialogMode")

;;; _____________________________________________________________________________
;;;                             GetAnswerFromUser
;;; _____________________________________________________________________________

(new DialogBox :objectname "GetNewStringDialogBox" :project sk8
     :properties '((allowSpaces :value t) (allowEmptyStrings :value t) (allowReturns :value nil)))

(new DialogBoxDisplayEditText :project SK8 :container GetNewStringDialogBox
     :objectName "GetNewStringDialogDisplayer")
(new DialogBoxEditText :project SK8 :container GetNewStringDialogBox 
     :objectName "GetNewStringDialogGetter")
(new halo :fillcolor black :project SK8 :objectname "GetNewStringDialogHighlighter")
(setf (container GetNewStringDialogHighlighter) GetNewStringDialogBox)
(moveoffstage GetNewStringDialogHighlighter)
(define-handler activatetext (GetNewStringDialogGetter)
  (call-next-method)
  (surroundobject GetNewStringDialogHighlighter me :outset '(1 1)))

(define-handler keyDown (GetNewStringDialogGetter thechar)
  (if (and (not (allowSpaces GetNewStringDialogBox)) (eq theChar #\Space))
    (ed-beep) ;;;(messageToUser "Sorry, no spaces are allowed." :beep t)
    (if (or (eq theChar #\Enter) (and (not (allowReturns GetNewStringDialogBox)) (eq theChar #\Return)))
      (click GetNewStringDialogYesButton)
      (call-next-method)))
  (unless (allowEmptyStrings GetNewStringDialogBox)
    (if (equal (text GetNewStringDialogGetter) "")
      (setf (textcolor (car (contents GetNewStringDialogYesButton))) lightgray)
      (setf (textcolor (car (contents GetNewStringDialogYesButton))) black))))

(new DialogBoxHighlightedButton :objectname "GetNewStringDialogYesButton" :project sk8)
(setf (container GetNewStringDialogYesButton) GetNewStringDialogBox)
(setf (textColor GetNewStringDialogYesButton) black)
(setf (text GetNewStringDialogYesButton) "OK")
(define-handler autohighlight (GetNewStringDialogYesButton)
  (or (allowEmptyStrings GetNewStringDialogBox) (not (string= (text GetNewStringDialogGetter) ""))))
(define-handler click (GetNewStringDialogYesButton)
  (when (or (allowEmptyStrings GetNewStringDialogBox) (not (string= (text GetNewStringDialogGetter) "")))
    (let ((ourDIalog (container me)))
      (setf (Highlight me) t)
      (sleep 0.05)
      (setf (Highlight me) nil)
      (sleep 0.05)
      (setf (container ourDIalog) nil)
      (exitModalState (text GetNewStringDialogGetter)))))

(new DialogBoxCancelButton :objectname "GetNewStringDialogCancelButton" :project sk8)
(setf (container GetNewStringDialogCancelButton) GetNewStringDialogBox)

(define-handler EnteringStage (GetNewStringDialogBox)
  (call-next-method)
  (setf (textcolor (car (contents GetNewStringDialogYesButton))) 
        (if (or (allowEmptyStrings GetNewStringDialogBox)
                (and (text GetNewStringDialogGetter)
                     (not (string= (text GetNewStringDialogGetter) ""))))
          black lightgray))
  (setf (keyTarget me) GetNewStringDialogGetter))

(define-handler resized (GetNewStringDialogBox)
  (withActorLocked (me)
    (call-next-method)
    (sk8-multival-bind (x y) (size me)
      (sendToBack GetNewStringDialogDisplayer)
      (setBoundsRect GetNewStringDialogDisplayer 7 7 (- x 7) (- y 70))
      (setBoundsRect GetNewStringDialogGetter 7 (- y 65) (- x 7) (- y 45))
      (setBoundsRect GetNewStringDialogCancelButton (- x 168) (- y 29) (- x 98) (- y 11))
      (setBoundsRect GetNewStringDialogYesButton (- x 88) (- y 33) (- x 10) (- y 7)))))

(resized GetNewStringDialogBox)

(define-sk8-function GetAnswerFromUser nil (message &key 
                                                       (textFont ChicagoFont)
                                                       (textsize 12)
                                                       (width 300)
                                                       (height nil)
                                                       (h nil)
                                                       (v nil)
                                                       (okText "OK")
                                                       (cancelText "Cancel")
                                                       (defaultAnswer "")
                                                       (allowSpaces t)
                                                       (allowReturns nil)
                                                       (allowEmptyStrings t)
                                                       ((:system UseSystem) nil))
  (let (returnedValue)
    (unless (and h v)
      (sk8-multival-bind (hh vv) (mainMonitorcenter)
        (if (not h) (setf h hh))
        (if (not v) (setf v vv))))
    (when (and UseSystem (or allowReturns (not allowSpaces)))
      (Error "System GetAnswerFromUser Dialog does not support allowReturns or allowSpaces options.  Use the non system version."))
    (if UseSystem
      (progn
        (unless height (setf height 100))
        (MacGetAnswerFromUser message :okText okText :cancelText cancelText
                              :width width :height height :defaultAnswer defaultAnswer
                              :allowEmptyStrings allowEmptyStrings
                              :top (- v (round height 2)) :left (- h (round width 2))))
      (progn
        (setlocation GetNewStringDialogBox h v)
        (setf (textsize GetNewStringDialogDisplayer) textsize)
        (setf (textfont GetNewStringDialogDisplayer) textfont)
        (setf (text GetNewStringDialogGetter) defaultanswer)
        (setf (text GetNewStringDialogYesButton) okText)
        (setf (text GetNewStringDialogCancelButton) cancelText)
        (setf (text GetNewStringDialogDisplayer) message)
        (unless height 
          (sk8-multival-bind (hh vv) (actortextsize GetNewStringDialogDisplayer)
            (declare (ignore hh))
            (setf height (min 340 (+ vv 80)))))
        (setSize GetNewStringDialogBox width height)
        (setf (allowSpaces GetNewStringDialogBox) allowSpaces)
        (setf (allowEmptyStrings GetNewStringDialogBox) allowEmptyStrings)
        (setf (allowReturns GetNewStringDialogBox) allowReturns)
        (setf returnedValue (modalDialog GetNewStringDialogBox))
        (setf (container GetNewStringDialogBox) nil)
        (if (or (not returnedValue)
                (equal returnedValue "")) (setf returnedValue defaultAnswer))
        returnedValue))))

#|
	Change History (most recent last):
~~	2  	 4/10/96	Hernan  	define-sk8-function->defun.
	4  	 2/27/97	Hernan  	Putting define-sk8-function back in the code.
|# ;(do not edit past this line!!)
