;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories

(in-package :uidev)

(SK8-declare-syms :UI :public ; Updated  1-25-96   5:59 pm
                  UI::REPORTWARNINGS UI::SCRIPTWARNINGSWINDOW UI::WARNINGSPICKER)

;;; This window shows warnings when a Script is compiled in the script editor.
;;; Uses an icon picker to show the errors. Clicking on an error takes you there. 
;;; The items of the picker are error structs as defined in the compiler interface file. 

;;; The scripts warnings window goes away when the editor it is associated with goes away.

(new UISimpleWindow :objectname "ScriptWarningsWindow" :project ui)
(setf (width ScriptWarningsWindow) 400)
(setf (height ScriptWarningsWindow) 150)
(setf (text ScriptWarningsWindow) "Script Warnings")
(setf (resizer ScriptWarningsWindow) t)
(setf (zoombox ScriptWarningsWindow) nil)
(setf (dofirstClick ScriptWarningsWindow) t)

(addProperty ScriptWarningsWindow 'scriptEditor)

(new UITextListForCorners :objectname "WarningsPicker" :project ui)
(tagpart ScriptWarningsWindow WarningsPicker 'picker)
(setf (container WarningsPicker) ScriptWarningsWindow)
(setf (pickerPrototype WarningsPicker) multiLinePicker)

(define-handler createTextDisplayItem ((picker WarningsPicker) theItem)
  (format nil "[Line ~a] ~a"
          (lineOfError theItem)
          (descriptionOfError theItem)))

;;; takes you to the line in the editor that refers to this error...

(define-handler doubleClick ((picker WarningsPicker))
  (when (and (selectedItems me)
             (scriptEditor (sk8::window me)))
    (let ((theError (car (selectedItems me)))
          (theEditor (scriptEditor (sk8::window me))))
      ;; bring up the editor.
      (bringToFront theEditor)
      ;; select the line in question.
      (sk8dev::select-entire-line (editor theEditor) (1+ (lineOfError theError)))
      )))

(define-handler selectionCompleted ((picker WarningsPicker))
  (when (and (selectedItems me)
             (scriptEditor (sk8::window me)))
    (sk8dev::select-entire-line (editor (scriptEditor (sk8::window me)))
                                (1+ (lineOfError (car (selectedItems me)))))))

;;; Resizing the window...

(define-handler resized (ScriptWarningsWindow)
  (sk8-multival-bind (width height) (size me)
    (setBoundsRect (car (contents me)) 6 25 (- width 5) (- height 5))))
                
;;; Should only be called if there are warnings.

(define-handler reportWarnings (ScriptWarningsWindow editorWindow handlerName warnings)
  (when warnings
    (setf (scriptEditor me) editorWindow)
    (setf (title (picker me)) (format nil "for ~a:" handlerName))
    (setf (items (picker me)) warnings)
    (sk8-multival-bind (ll tt rr bb) (boundsRect editorWindow)
      (declare (ignore tt rr))
      (sk8-multival-bind (width height) (size me)      
        (setBoundsRect me ll (+ bb 5) (+ ll width) (+ bb height 5) :justMoving t)))
    (bringUp me)))

(define-handler clearWarnings (ScriptWarningsWindow)
  (setf (container me) nil)
  (setf (scriptEditor me) nil)
  (setf (items (picker me)) nil)
  (setf (title (picker me)) nil))


(resized ScriptWarningsWindow)

#|
	Change History (most recent last):
	1  	 1/17/96	Hernan  	New file. Implements a warnings window for show all the
						warnings when a handler is compiled.
	2  	 1/25/96	Hernan  	Declaring some symbols.
	3  	 1/25/96	Hernan  	Making the window show up below the editor.
	4  	 1/25/96	Hernan  	Exporting symbols.
	5  	 1/26/96	Hernan  	Fixing typo. The picker of the window should not be the
						window itself.
	2  	10/11/96	Brian   	Fixing double click to select the line + 1.
	2  	10/11/96	Brian   	
	3  	 2/27/97	Hernan  	
|# ;(do not edit past this line!!)
