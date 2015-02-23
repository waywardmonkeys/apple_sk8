;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories
(in-package :ccl)

; 11/01/94 dy   Add initial-selections option to select-item-from-list

(let ((*warn-if-redefine-kernel* nil))
  (declare (special *warn-if-redefine-kernel*))
  (defun select-item-from-list (the-list &key (window-title "Select an Item")
                                            (selection-type :single)
                                            table-print-function 
                                            (action-function #'identity)
                                            (default-button-text "OK")
                                            (initial-selections '())
                                            modeless)
  "Displays the elements of a list, and returns the item chosen by the user.
If initial-selections list is non-empty, then each member of the-list which
is also in initial-selections is preselected;  however, if selection-type is
:single, then only the first of initial-selections is preselected."
  (let (dialog sdi)
    (flet ((act-on-items (item)
             (let ((s-item (car (subviews (view-container item)
                                          'sequence-dialog-item))))
               (funcall action-function 
                        (mapcar #'(lambda (c) (cell-contents s-item c))
                                (selected-cells s-item))))))
      (setq dialog
          (make-instance
           'select-dialog
           :window-type :document-with-grow
           :close-box-p (if modeless t nil)
           :window-title window-title
           :view-size #@(400 140)
           :view-position (make-point (%ilsr 1 (- *screen-width* 400))
                                      90)
           :window-show nil ;modeless
           :view-subviews
           (list* 
            (setq sdi
                  (make-instance
                   'sequence-dialog-item
                   :view-position #@(4 4)
                   :view-size #@(390 100)
                   :table-hscrollp nil
                   :table-sequence the-list
                   :table-print-function table-print-function
                   :selection-type selection-type
                   :dialog-item-action
                   #'(lambda (item)
                       (when (double-click-p)
                         (dialog-item-action
                          (default-button (view-container item)))))))
            (make-instance 
             'default-button-dialog-item
             :dialog-item-text default-button-text
             :dialog-item-enabled-p the-list
             :view-position (if modeless #@(315 114) #@(238 114))
             :view-size #@(60 20)
             :dialog-item-action
             (cond 
              ((not modeless)
               #'(lambda (item)
                   (return-from-modal-dialog (act-on-items item))))
              (t
               #'act-on-items)))
             (if (not modeless)
               (list
                (make-instance 'button-dialog-item
                               :dialog-item-text "Cancel"
                               :view-position #@(308 114)
                               :view-size #@(60 20)
                               :dialog-item-action
                               #'(lambda (item)
                                   (declare (ignore item))
                                   (return-from-modal-dialog :cancel))))
               nil))))
      (when the-list
        (let (firstSelection)
          (when initial-selections
            (mapc #'(lambda (theSelection)
                      (let ((listPosition (position theSelection the-list)))
                        (when listPosition
                          (cell-select sdi (index-to-cell sdi listPosition)) ; sdi needs to be in a window to do this
                          (if firstSelection
                            (setf firstSelection (min firstSelection listPosition))
                            (setf firstSelection listPosition)))))
                  (if (eq selection-type :single)
                    (list (first initial-selections))
                    initial-selections)))
          (if firstSelection
            (let ((height (point-h (visible-dimensions sdi)))
                  (numItems (length the-list)))
              (scroll-to-cell sdi (index-to-cell sdi (min (- numItems height 1)
                                                          firstSelection))))
            (cell-select sdi (index-to-cell sdi 0))))) ; sdi needs to be in a window to do this
    (cond (modeless ; select first then show is prettier
           (window-show dialog)
           dialog)
          (t (modal-dialog dialog)))))))
#|
	Change History (most recent last):
	1  	11/ 1/94	dy      	select-item-from-list with new initial-selections keyword arg
	2  	 2/27/97	Hernan  	
|# ;(do not edit past this line!!)
