;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories


;;;

(in-package :ccl)

#| Modification History

|#

;; We enhance the menu and menu-item CLOS classes
;; so that they may work with SK8 menu and menuItem objects.


;; We add a new slot, mf::my-frame, which is a back pointer
;; to the menu and menuItem SK8 objects.


(let ((*warn-if-redefine-kernel* nil))
  (defclass menu (menu-element)
    ((item-list :initform nil)
     (menu-id :initform nil :reader menu-id)
     (menu-handle :initform nil :reader menu-handle)
     (mf::my-frame :initform nil :accessor mf::my-frame)))
  
  ;;MCL3.0 
  (defclass menu-item (menu-element)      ; really "simple-menu-element"
    ((checkedp)
     (command-key)
     (menu-item-action :accessor menu-item-action-function)
     (menu-item-icon-num :reader menu-item-icon-num
                         :writer (setf menu-item-icon-num-slot)
                         :initform nil
                         :initarg :icon-num)
     (mf::my-frame :initform nil :accessor mf::my-frame)))
  )
#|
	Change History (most recent last):
	1	2/21/94	sidney	move this file to a new subproject
	2  	 2/27/97	Hernan  	
|# ;(do not edit past this line!!)
