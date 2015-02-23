;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories

(in-package :sk8dev)

(provide "PICKERMENU")

(require "MENUS")

(new menu :objectname "PickerMenu" :project sk8
     :prototype t
     :properties '((Items :value nil)
                   (SelectedItem :value nil)
                   (MaximumLength :value 10000)))
(setf (text PickerMenu) "Items")

(new menuItem :objectName "PickerMenuItem" :project sk8
     :prototype t
     :properties '((Item :value nil)))
(define-handler menuselect (PickerMenuItem)
  (setf (selecteditem (menu me)) (item me))
  (call-next-method)
  )

(define-handler menuselect (PickerMenu)
  (setf (text me) (CreateTextDisplayItem me (selecteditem me)))
  (call-next-method)
  )

;;;make mousedown ensure the dynamic items are gone so they can be garbage collected...

(define-handler mousedown (PickerMenu)
  (call-next-method)
  (dolist (i (menuitems me)) (if (is-a i DynamicPickerMenuItem) (setf (menu i) nil)))
  )

(new PickerMenuItem :objectName "DynamicPickerMenuItem" :project sk8)

(define-handler AddItem (PickerMenu newItem)
  (let* ((theViewItems (Items me))
         (maxlen (MaximumLength me))
         (newits (remove newitem theViewItems :test #'equalp)))
    (setf newits (push newItem newits))
    (setf newits (delete-duplicates newits :test #'equalp))
    (when (>= (length newits) maxlen)
      (setf (nthcdr (1- maxlen) newits) nil))
    (setf (Items me) newits)))

(new PickerMenuItem :objectname "PickerMenuItemSpacer" :project sk8 :prototype t)
(setf (text PickerMenuItemSpacer) "-")
(setf (enabled PickerMenuItemSpacer) nil)

(define-handler CreateTextDisplayItem (PickerMenu theItem)
  (if (inheritsfrom theItem string) theItem (objectstring theItem)))

(define-handler update (PickerMenu)
  (dolist (i (menuItems me)) 
    (if (inheritsFrom i DynamicPickerMenuItem) 
      (setf (menu i) nil)
      ;; (dispose i)
      ))
  (if (menuItems me)
    (setf (menu PickerMenuItemSpacer) me))
  (dolist (i (Items me))
    ;; (unless (null i)
    (let ((theItem (new DynamicPickerMenuItem :project (project me))))
      (setf (text theItem) (CreateTextDisplayItem me i))
      (setf (Item theItem) i)
      (setf (menu theItem) me))))


#|
	Change History (most recent last):
	2  	 2/27/97	Hernan  	
|# ;(do not edit past this line!!)
