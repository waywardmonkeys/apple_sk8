;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories

(in-package :sk8dev)

(provide "SLIDER")

(require "SCROLLER" "objects;Widgets:Scroller")

;;; _______________________________ 
;;; The slider object
;;; _______________________________ 

(new scroller :objectname "Slider" :project sk8)

(setf (container (uparrow slider)) nil)
(setf (container (downarrow slider)) nil)
(setf (uparrow slider) nil)
(setf (downarrow slider) nil)
(setFrameSize Slider 1 1)
(setf (minimumThumbSize slider) 10)
(setf (maximumThumbSize slider) 10)

(setf (objectname (thumb slider)) "SliderThumb")
(addparent SliderThumb roundrect)

(setf (objectname (scrollerBody slider)) "SliderBody")

(define-handler bestSize (SliderBody)
  (declare (special left top right bottom thickNess orientation))
  (if (eq orientation 'vertical)
    (setBoundsRect me 
                   (- (+ left (round (- right left) 2)) thickness) (+ top thickNess -1) 
                   (+ (+ left (round (- right left) 2)) thickness) (- bottom thickness -1)
                   :physical t)
    (setBoundsRect me 
                   (+ left thickNess -1) (- (+ top (round (- bottom top) 2)) thickness) 
                   (- right thickness -1) (+ (+ top (round (- bottom top) 2)) thickness)
                   :physical t)))

(define-handler thickness (Slider)
  4)


#| Slider test!

(new rectangle :objectname "pipo" :project sk8)
(setBoundsRect pipo 50 50 450 400)
(setf (fillcolor pipo) yellow)
(setf (container pipo) stage)

(define-handler mousedown (pipo)
  (if (eq eventactor me) (drag me)))

;;; Scroller value test.

(new slider :objectname "scro" :project sk8)
(setf (container scro) pipo)
(setLocation scro 30 30 :relative t)
(setsize scro 330 30 )

(setf (minimumValue scro) 0)
(setf (maximumValue scro) 65535)
(setf (scrollStep scro) 1)
(setf (pageStep scro) 10)
(setf (thumbview scro) 1)

(define-handler mapvalue (scro)
  (setf (text pipo) (currentValue me)))

(define-handler mapvalue (scro)
  (setf (text pipo) (round (currentValue me)))
  (setf (opred newyellow) (round (currentValue me)))
  (setf (opgreen newyellow) (round (currentValue me)))
  (setf (opblue newyellow) (round (currentValue me)))
  (lightforceredraw caca))


|#

#|
	Change History (most recent last):
	2  	 8/ 8/96	Hernan  	body -> scrollerBody.
	3  	 2/27/97	Hernan  	
|# ;(do not edit past this line!!)
