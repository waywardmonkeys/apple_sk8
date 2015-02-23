(in-package :SK8Development)

(provide "TEXTFIELD")

(require "EDITTEXT" "objects;EditText:EditText")
(require "SCROLLER" "objects;Widgets:Scroller")


;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories



#| Modification History.

05-04-93 hernan the textField is now an editText with an affixed scroller!
                AMAZINGLY simple, isn't it?
03-24-93 ruben dev't kitchen GetFromUser
01-26-93 hernan setting the partnerScroller now does all the port related work.
01-22-93 ruben id -> objectName
10-03-92 ruben :number -> num in (setf textStyle), and other argument changes.
10-02-92 ICE modified to work with latest frames...
09-20-92 hernan changing this to work with MF VI

|#

(new editText :objectName "TextField" :prototype t
     :undisposable t
     :project sk8)

(setBoundsRect textField 50 50 250 200)

(new scroller :objectNAme "TfScroller"
     :undisposable t
     :project sk8)

(define-system-handler initialize (textField original isNew initArgs)
  (declare (ignore-if-unused original isNew initArgs))
  (call-next-method)
  ;; This is necessary to let the scroller snap into place (since the setBoundsrect method
  ;; does the work).
  (sk8-multival-bind (ll tt rr bb) (boundsRect me)
    (setBoundsRect me ll tt rr bb)))

;;; We do not affix the scroller because the textField does it all in the set boundsrect
;;; method.

(setf (partnerVScroller textField :affixing nil) tfScroller)
   
;;; The textField and tfScroller travel together.

(define-system-handler (setf container) (newContainer textField)
  (call-next-method)
  (when (partnerVScroller me)
    (setf (container (partnerVScroller me)) newContainer)))

(define-handler (setf visible) (boolean textField)
  (withActorLocked (me)
    (call-next-method)
    (when (partnerVScroller me)
      (setf (visible (partnerVScroller me)) boolean))))

;;; TextField's boundsRect behaves as if the textField was included. Nothing to do for the scroller that snaps
;;; back into place when the textField is resized or moved.

(define-system-handler boundsRect (textField &key physical)
  (if (and (partnerVScroller me) (visible (partnerVScroller me)))
    (sk8-multival-bind (h v hh vv) (call-next-method)
      (sk8-multivals h v (+ hh (width (partnerVScroller me) :physical physical)) vv))
    (call-next-method)))

(define-system-handler setBoundsRect (textField ll tt rr bb &key physical relative justMoving)
  (let ((theScroller (partnerVScroller me)))
    (if (and theScroller (visible theScroller))
      (let ((theWidth (width theScroller :physical physical)))
        (if relative
          (sk8-multival-bind (left top right bottom) (boundsRect me :physical physical)
            (setBoundsRect me (+ left ll) (+ top tt) (+ right rr) (+ bottom bb) 
                           :physical physical :relative nil :justMoving justMoving))
          (withActorLocked (me)
            (call-next-method me ll tt (- rr theWidth) bb :physical physical :justMoving justMoving)
            (setBoundsRect (partnerVScroller me) (1- (- rr theWidth)) tt (1- rr) bb 
                           :physical physical :justMoving justMoving))))
      (call-next-method))))

#|
	Change History (most recent last):
	2	6/1/93	Hernan	(1) The boundsRect of the textField is the field + 
				     the scroller.
				(2) Hiding the scroller enlarges the field to fill the
				     space.
				(3) Disposing the field disposes the scroller.
				(4) Setting the container of the field sets the
				     container of the scroller.
	3	6/1/93	Hernan	Makes children of tfScroller undisposable unless
				they are disposed by the dispose method of the
				field. This should avoid independent disposal of the
				scroller.
	4	6/1/93	Hernan	Third attempt!
	5	6/1/93	Hernan	Fixing problems with moving the thing with the selection tool.
	6	6/22/93	Hernan	Made the scroller snap to the field at creation time. 
				This involves a call to setBoundsRect.
	7	6/25/93	Hernan	InitializeObject -> initialize.
	13	1/11/94	hernan	self -> me
	14	2/21/94	hernan	The Final Renaming for Alpha! (yeah, right...)
	15	2/28/94	hernan	Getting rid of dispose. This involves making the
				textField's scroller not be undisposable (no danger
				anymore since the user cannot call dispose to
				destroy it).
	16	5/4/94	rod	Making tfScroller NOT be a prototype
	17	7/6/94	Hernan	1171687: hiding/showing the scroller when the
				editText is hidden/shown.
	18 	 8/31/94	Hernan  	Monaco -> GenevaFont.
	19 	 9/12/94	Hernan  	1180502: capitalizing object names.
	20 	10/ 7/94	Hernan  	1190875: set visible does not need to call withActorLocked 
							passing the container.
	21 	 2/16/95	sidney  	readable argument names for initialize handler
	22 	 3/ 1/95	Hernan  	Removing queue-build-fixup that set the textfont
							of the textField to GenevaFont. Should not be
							required.
	2  	 6/23/95	Hernan  	textField should not assume the existance of the partnerScroller.
	2  	 2/27/97	Hernan  	
|# ;(do not edit past this line!!)
