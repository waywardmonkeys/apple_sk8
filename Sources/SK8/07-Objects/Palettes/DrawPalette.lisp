;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories

(in-package :sk8dev)

(provide "DRAWTOOLS")

(require "BEVELRENDERER" "objects;Effects:BevelRenderer")
(require "MESSAGETOUSER" "objects;Dialogs:MessageToUserDialogBox")
(require "GETSHAPEFROMUSER" "functions;GetShapeFromUser")
(require "SELECTIONDETECTION" "functions;Selection Detection")

;;; _______________________________ 
;;;  First the Draw Palette
;;; _______________________________ 

;;;  This guy just has a current tool and sends selected and deselected

(new rectangle :objectname "DrawToolPalette" :project SK8
     :properties '(CurrentTool))

(define-handler (setf CurrentTool) (thetool DrawToolPalette)
  (withactorlocked (me)
    (when (CurrentTool me)
      (deselected (CurrentTool me)))
    (setf (slot-value me 'currenttool) thetool)
    (when thetool
      (selected thetool))))

(setf (fillcolor DrawToolPalette) darkgray)

;;; _______________________________ 
;;;  Then the Draw Tool...
;;; _______________________________ 

(new Rectangle :objectname "DrawTool" :project sk8
     :properties '((ActorToDraw :inherit) (styleToDraw :inherit) 
                   (MultipleDraw :inherit :value t) (createOnClick :value t)))

;;(addproperty drawtool 'createOnClick :initialvalue t)

(setf (ActorToDraw DrawTool) rectangle)
(setf (styleToDraw DrawTool) 'CornerToCorner)
(setf (framesize drawtool) '(3 3))
(setf (framecolor drawtool) uirectangleoutbevel)

(define-handler mousedown (DrawTool)
  (when (inheritsfrom (container me) drawtoolpalette)
    (setf (CurrentTool (container me)) me))
  (call-next-method))

(define-handler doubleclick (DrawTool)
  (let ((ans (getnewfromuser "string" :message "Enter Actor To Draw:")))
    (when ans
      (setf ans (withproject sk8 (eval (read-from-string ans nil nil)))) ;;;; ****OJO!!!
      (if (and ans
               (typep ans 'sk8::object)
               (inheritsFrom ans actor))
        (setf (actorToDraw me) ans)))))

(define-handler selected (DrawTool)
  (setf (framecolor me) uirectangleinbevel))

(define-handler deselected (DrawTool)
  (setf (framecolor me) uirectangleoutbevel))

(define-handler DrawWith (DrawTool &key (startH nil) (startV nil) (Container nil) ((:project inproject) nil))
  ;;; If a StartH is not specified use the currentMouseLocation
  (let (newShape newbie newbieguy (TheAct (actorToDraw me)) cont clickdrawn)
    (if (or (not startH) (not startV))
      (sk8-multival-bind (hh vv) (mouseloc stage)
        (setf startH hh 
              startV vv)))
    ;;; If a container is not specified use the one the mouse is over
    (unless container
      (setf container (actorAtHVCoordinates startH startV)))
    (setf cont container)
    (unless inproject 
      (if (eq container stage)
        (setf inproject sk8)
        (setf inproject (project container))))
    (if (and (neq container stage)
             (some #'(lambda (x) (memq x (cons theact (ancestors theact)))) (cons cont (containers cont))))
      (messagetouser (concatenate 'string "You cannot draw a child of " (objectstring theact :project inproject)
                                  " into " (objectstring cont :project inproject)
                                  " because that would place it in one of it's ancestors.") :beep t)
      (progn
        (withCursor cursorCrossHair
          (setf NewShape
                (GetShapeFromUser TheAct :startH startH :startV startV 
                                  :style (styleToDraw me)
                                  :MultipleDraw t)))
        (setf clickdrawn (or (and (or (eq TheAct polygon) (inheritsFrom TheAct polygon))
                                  (not (listp (car newshape)))
                                  (<= (length newshape) 4))
                             (and (not (or (eq TheAct polygon) (inheritsFrom TheAct polygon)))
                                  (not (listp (car newshape)))
                                  (<= (- (max (first newshape) (third newshape)) (min (first newshape) (third newshape))) 2)
                                  (<= (- (max (fourth newshape) (second newshape)) (min (fourth newshape) (second newshape))) 2))))
        (when (or (not clickdrawn) (createOnClick me))
          (withCursor watchCursor
            (if (neq container stage) (lock container))
            (if (and (car newshape) (not (listp (car newshape))))
              (let (ok)
                (setq newbie (new TheAct :project inproject))
                (loop
                  (setf ok (newcontentok container newbie))
                  (if ok 
                    (return)
                    (if (or (eq container stage) (not container))
                      (return)
                      (setf container (container container))))
                  )
                (if (not ok)
                  (progn
                    (messagetouser (concatenate 'string "Sorry, you cannot draw a " (objectstring theact :project project) " into " (objectstring cont :project project)))
                    (setf newbie nil))
                  (progn
                    (if (neq container stage) (setf (container newbie) container))
                    (if clickdrawn
                      (setLocation newbie (first newshape) (second newshape) :physical t)
                      (cond
                       ((or (eq TheAct polygon) (inheritsFrom TheAct polygon))
                        (setf (points newbie :physical t) newshape))
                       ((or (eq TheAct Linesegment) (inheritsFrom TheAct Linesegment))
                        (setEndPoints newbie (car newshape) (cadr newshape) (caddr newshape) (cadddr newshape) :physical t))
                       (t
                        (setf (boundsrect newbie :physical t ) newshape))))
                    (if (eq container stage) (setf (container newbie) container))
                    )))
              (let (ok bummer)
                (dolist (i newshape)
                  (unless bummer
                    (setq newbieguy (new TheAct :project inproject))
                    (setf ok (newcontentok container newbieguy))
                    (if (not ok)
                      (progn
                        (messagetouser (concatenate 'string "Sorry, you cannot draw " (objectstring theact :project project) "s into " (objectstring container :project project)))
                        (setf bummer t))
                      (progn
                        (if (neq container stage) (setf (container newbieguy) container))
                        (setf (boundsrect newbieguy :physical t ) i)
                        (push newbieguy newbie)
                        (if (eq container stage) (setf (container newbieguy) container)))))
                  )))
            (if (neq container stage) (unlock container)))
          newbie)))))

;;; _______________________________ 
;;;  Now the Selection Tool...
;;; _______________________________ 

(new Rectangle :objectname "SelectionTool" :project sk8
     :properties '(Lasso SelectionFunction avoiding))

(setf (Lasso SelectionTool) nil)
(setf (SelectionFunction SelectionTool) #'FindSelectedActors)
(setf (framesize SelectionTool) '(3 3))
(setf (framecolor SelectionTool) uirectangleoutbevel)

(define-handler mousedown (SelectionTool)
  (when (inheritsfrom (container me) drawtoolpalette)
    (setf (CurrentTool (container me)) me)))

(define-handler selected (SelectionTool)
  (setf (framecolor me) uirectangleinbevel))

(define-handler deselected (SelectionTool)
  (setf (framecolor me) uirectangleoutbevel))

(define-handler SelectWith (SelectionTool &key (startH nil) (startV nil) (Container nil))
  ;;; If a StartH is not specified use the currentMouseLocation
  (let (OurShape res)
    (if (or (not startH) (not startV))
      (sk8-multival-bind (hh vv) (mouseloc stage)
        (setf startH hh 
              startV vv)))
    (withCursor CursorSelectionCrosshair
      (setf OurShape
            (if (Lasso me)
              (GetShapeFromUser polygon :startH startH :startV StartV 
                                :style 'FreeHand)
              (GetShapeFromUser rectangle :startH startH :startV StartV  
                                :style 'CornerToCorner))))
    (withCursor watchCursor
      (setf res (funcall (SelectionFunction me) OurShape :avoiding (avoiding me) :container container)))
    res))

#|
	Change History (most recent last):
	1	6/4/93	Brian Roddy	
	2	6/10/93	Brian Roddy	
	9	9/20/93	rod	Made it so it can draw on stage--Hernan
	10	10/15/93	rod	
	11	10/18/93	rod	
	13	1/31/94	rod	
	15	2/28/94	rod	targetproject ui
	16	3/2/94	kleiman	object-p -> typep; obsoleted object-p
	17	3/10/94	rod	
	18	3/29/94	rod	
	19	6/27/94	Hernan	1170049: style options become symbols.
	20	7/19/94	rod	Fixing drawing of linesegments.
	21	7/19/94	rod	left a print statement
	22	7/27/94	rod	
	23	8/12/94	rod	Fixed newcontentsok to recurse up.
	24 	11/ 1/94	rod     	
	25 	11/21/94	rod     	
	26 	12/13/94	rod     	a request for the jamie meister.
	27 	12/19/94	rod     	Fixing bug with multi draw on the stage
	28 	12/19/94	rod     	removing print statement.
	29 	 2/ 9/95	rod     	Checking containers to see if you are drawing
							a child into one of it's ancestors.
	30 	 2/ 9/95	rod     	
	3  	 2/27/97	Hernan  	define-system-handler->define-handler.
|# ;(do not edit past this line!!)
