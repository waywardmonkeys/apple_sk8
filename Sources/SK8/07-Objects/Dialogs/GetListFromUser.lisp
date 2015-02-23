;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories

(in-package :sk8dev)

(provide "GETLISTFROMUSER")

(require "SELECTFROMCOLLECTIONDIALOG" "objects;Dialogs:SelectFromCollectionDialog")
(require "MENUS")

(new menu :objectname "GetFromUserPopUp" :project sk8)
(new menuitem :objectname "GetFromUsermenuitem" :project sk8
     :properties '(returnvalue))

(define-handler menuselect (GetFromUsermenuitem)
  (or (returnvalue me) :oknil))

(define-handler getFromUser (list &key (multipleValues nil) popUpMenu 
                                    relativeactor ((:project InProject) nil))
  (let ((len (length me)) val)
    (if (and (or popUpMenu (and (< len 15) (not multiplevalues))))  ;; (down mouse)) took this out...
      (progn
        (dolist (i me) 
          (new GetFromUsermenuitem :project sk8 :text (objectstring i) :menu GetFromUserPopUp :returnvalue i))
        (setf val 
              (if relativeactor
                (selectfrommenu getfromuserpopup :relativeactor relativeactor 
                                :position 'bottomrightaligned)
                (selectFromMenu GetFromUserPopUp :h (eventh) :v (eventv))))
        ;; (mapcar #'dispose (menuitems GetFromUserPopUp))
        (dolist (i (menuitems getfromuserpopup))
          (setf (menu i) nil))
        (if val (if (eq val :oknil) nil val) (abortevent))
        )
      (selectfromcollectiondialog me :multipleValues multipleValues))))

#|
	Change History (most recent last):
	2  	 2/27/97	Hernan  	
|# ;(do not edit past this line!!)
