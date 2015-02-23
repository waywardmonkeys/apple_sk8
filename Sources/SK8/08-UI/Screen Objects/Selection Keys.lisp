;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories


;;;

(in-package :UIDevelopment)

;;; Here are some helpers.

(defun find-next-sibling-that (theItem predicate)
  (unless (eq theitem stage)
    (let* ((myContainer (container theItem))
           (realContents (if (eq mycontainer stage) 
                           (gs:list-to-vector (contents stage)) ;;;OJO UGLY!!!
                           (gs:node-contents myContainer)))
           (myLoc (position theItem realContents))
           (numContents (length realContents)))
      (cond ((= numContents 1) nil)
            (t (let ((curPos (1+ myLoc))
                     curitem)
                 (loop
                   (when (= curPos numContents) (setf curPos 0))
                   (setf curItem (aref realContents curpos))
                   (when (eq curitem theItem) (return nil))
                   (when (funcall predicate curItem) (return curItem))
                   (incf curPos))))))))

(defun find-prev-sibling-that (theItem predicate)
  (unless (eq theitem stage)
    (let* ((myContainer (container theItem))
           (realContents (if (eq mycontainer stage) 
                           (gs:list-to-vector (contents stage)) ;;;OJO UGLY!!!
                           (gs:node-contents myContainer)))
           (myLoc (position theItem realContents))
           (numContents (length realContents)))
      (cond ((= numContents 1) nil)
            (t (let ((curPos (1- myLoc))
                     curitem)
                 (loop
                   (when (= curPos -1) (setf curPos (1- numContents)))
                   (setf curItem (aref realContents curpos))
                   (when (eq curitem theItem) (return nil))
                   (when (funcall predicate curItem) (return curItem))
                   (decf curPos))))))))

;; (setCurrentProject ui)
;;______________________________________________________________________
;;______________________________________________________________________
;; THE SELECTION HANDLING CODE
;; 

(define-handler keydown (selectionHalo theKey)
  (let (newItem)
    (case theKey
      (#\upArrow
       (if (option-key-p)
         ;; move 1 pixel up (or 10 if shift key down)
         (withActorLocked ((sk8::window (car (selectedItems me))))
           ;; move 'em
           (doList (theItem (selectedItems me))
             (setf (location theItem :physical t :relative t)
                   (list 0 (if (shift-key-p) -10 -1))))
           ;; move the selection
           (setf (location me :physical t :relative t)
                 (list 0 (if (shift-key-p) -10 -1))))
         ;; select container
         (progn
           (setq newItem (container (car (selectedItems me))))
           (if (eq newItem stage)
             (setq newItem nil))))
       )
      (#\DownArrow
       (if (option-key-p)
         ;; move 1 pixel down (or 10 if shift key down)
         (withActorLocked ((sk8::window (car (selectedItems me))))
           ;; move 'em
           (doList (theItem (selectedItems me))
             (setf (location theItem :physical t :relative t)
                   (list 0 (if (shift-key-p) 10 1))))
           ;; move the selection
           (setf (location me :physical t :relative t)
                 (list 0 (if (shift-key-p) 10 1))))
         ;; select first item in contents
         (setq newItem (car (contents (car (selectedItems me))))))
       )
      (#\ForwardArrow
       (if (option-key-p)
         ;; move 1 pixel to the right (or 10 if shift key down)
         (withActorLocked ((sk8::window (car (selectedItems me))))
           ;; move 'em
           (doList (theItem (selectedItems me))
             (setf (location theItem :physical t :relative t)
                   (list (if (shift-key-p) 10 1) 0)))
           ;; move the selection
           (setf (location me :physical t :relative t)
                 (list (if (shift-key-p) 10 1) 0)))
         ;; select next object
         (let* ((theItem (car (selectedItems me)))
                (theProject (project theItem)))
           (setf newItem 
                 (find-next-sibling-that theItem #'(lambda (x) (eq (project x) theProject))))))
       )
      (#\BackArrow
       ;; select previous object or move
       (if (option-key-p)
         ;; move 1 pixel to the left (or 10 if shift key down)
         (withActorLocked ((sk8::window (car (selectedItems me))))
           ;; move 'em
           (doList (theItem (selectedItems me))
             (setf (location theItem :physical t :relative t)
                   (list (if (shift-key-p) -10 -1) 0)))
           ;; move the selection
           (setf (location me :physical t :relative t)
                 (list (if (shift-key-p) -10 -1) 0)))
         ;; select previous object
         (let* ((theItem (car (selectedItems me)))
                (theProject (project theItem)))
           (setf newItem 
                 (find-prev-sibling-that theItem #'(lambda (x) (eq (project x) theProject))))))
       )
      (#\Delete
       ;; dispose the selected object(s)
       (menuselect SelectionHaloPopUpDispose))
      (#\-
       ;; send farther
       (withActorLocked ((sk8::window (car (selectedItems me))))
         (doList (theItem (selectedItems me))
           (sendFarther theItem))))
      (#\_
       ;; send to back
       (withActorLocked ((sk8::window (car (selectedItems me))))
         (doList (theItem (selectedItems me))
           (sendToBack theItem))))
      (#\=
       ;; bring closer
       (withActorLocked ((sk8::window (car (selectedItems me))))
         (doList (theItem (selectedItems me))
           (bringCloser theItem))))
      (#\+
       ;; bring to front
       (withActorLocked ((sk8::window (car (selectedItems me))))
         (doList (theItem (selectedItems me))
           (bringToFront theItem))))
      (#\Escape 
       ;; deselect
         (deselect me))
      (#\`
       ;; reselect
       (deselect me)
       (mousedown SelectTool))
      ;; 
      (#\tab
       ;; flash the container
       (let ((c (container (car (selectedItems me)))))
         (when (neq c stage)
           (setf (Highlight c) t)
           (setf (Highlight c) nil))))
      )
    (when newItem
      (select (list newItem))
      )))

#|
	Change History (most recent last):
	1	10/1/93	rod	
	2	10/8/93	rod	
	3	11/1/93	rod	
	4	11/19/93	rod	
	6	2/12/94	kleiman	name changes
~~	7	2/21/94	hernan	The Final Renaming for Alpha! (yeah, right...)
	2  	 2/27/97	Hernan  	window -> sk8::window.
						93/29/94rod
						104/13/94HernanAvoiding use of contents when necessary.
						114/13/94HernanMmm. The code to select the next and previous
						actor satisfying some predicate is repeated. I'd 
						better make some functions for it.
						124/20/94rod
						136/28/94rod1171168: check for stage in the next and prev
						sib functions.
						14  2/14/95rod     fixing down arrow.
|# ;(do not edit past this line!!)
