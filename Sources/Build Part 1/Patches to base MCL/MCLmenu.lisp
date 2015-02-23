;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories
(in-package :ccl)

;;; The purpose of this patch is to fix the MCL bug that menu items whose text is "-xxx"
;;; show up disabled in the menubar.

;;; This function goes into the pstr and replaces all occurances of oldChar.

(defun pstr-char-replace (pstr oldChar newChar)
  (let ((strLen (%get-byte pstr))
        (counter 1)
        (oldCode (char-code oldChar))
        (newCode (char-code newChar)))
    (dotimes (i strLen)
      (when (eq (%get-byte pstr counter) oldCode)
        (%put-byte pstr newCode counter))
      (incf counter))))

;;; These functions are modified to replace all occurances of #\- with #\Ð
;;; thus fooling the menu manager and stoping it from disabling menuitems
;;; when their text is something like "--UI". 

(defun install-menu-item (mh item n)
  (let ((theTitle (slot-value item 'title)))
    (with-pstrs ((tp (or theTitle ""))
                 (xp "xxx"))
      ;; If the title is not "-" go through the string changing dashes into
      ;; little dashes.
      (unless (= 1 (length theTitle))
        (pstr-char-replace tp #\- #\Ð))
      ;    (#_appendMenu mh  xp)
      (#_InsMenuItem mh xp (%i- n 1))
      (if (%izerop (%get-byte tp))
        (%put-word tp #x0120)
        (if (and (not (eql 1 (%get-byte tp)))
                 (eq (char-code #\-) (%get-byte tp 1)))
          (%put-byte tp (char-code #\Ð) 1)))
      (#_SetItem mh n tp)
      ))
  ;Looks like a job for a generic function...
  (cond ((typep item 'menu)
         (init-menu-id item)
         (#_SetItemCmd mh n #\escape)
         (#_SetItemMark mh n (%code-char (slot-value item 'menu-id))))
        ((typep item 'menu-item)
         (if (slot-value item 'command-key)
           (set-command-key item (slot-value item 'command-key)))
         (if (slot-value item 'checkedp)
           (set-menu-item-check-mark item (slot-value item 'checkedp)))
         (if (neq (slot-value item 'style) :plain)
           (set-menu-item-style item (slot-value item 'style)))))
  (unless (slot-value item 'enabledp)
    (menu-item-disable item)))

(defmethod set-menu-item-title ((item menu-element) new-title &aux owner)
  (setf (slot-value item 'title) (setq new-title (ensure-simple-string new-title)))
  (when (setq owner (slot-value item 'owner))
    (if (equalp new-title "-")
      (progn 
        (if (slot-value owner 'menu-handle) (with-menu-detached owner))
        (menu-item-disable item))
      (let ((num (menu-item-number item)))
        (if (slot-value owner 'menu-handle)
          (with-pstrs ((tp new-title))
            (unless (= 1 (length new-title))
              (pstr-char-replace tp #\- #\Ð))
            (when (%izerop (%get-byte tp)) 
              (%put-word tp #x0120))
            (#_SetItem (slot-value owner 'menu-handle) num tp)
            )))))
  new-title)


#|
	Change History (most recent last):
	1	3/29/94	Hernan	New file. Fixes problem with menu items whose
				text is "-xxx".
	2	3/29/94	sidney	only hack the first character of menus that begin with "-" and are longer than one character
	3	3/30/94	Hernan	Now we are doing it by replacing the dashes in
				the mac version of the string (the one that goes
				to the handle).
	2  	 2/27/97	Hernan  	
|# ;(do not edit past this line!!)
