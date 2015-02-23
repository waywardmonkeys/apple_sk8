;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories

(in-package :SK8Development)

;;; Sneaky dev mode functionality for SimpleMenubar and ProjectMenuBar. 

(new menuItem :objectName "SneakyDevModeMenuItem" :project sk8)
(setf (text sk8::SneakyDevModeMenuItem) "Sneaky Dev-Mode")
(define-handler menuSelect (sk8::SneakyDevModeMenuItem)
  (dev-mode :force? t))

(define-handler Update (FileMenu)
  (call-next-method)
  (if (and (fboundp 'sk8:dev-mode) (optionkeydown) (commandkeydown))
    (progn 
      (setf (menu sk8::SneakyDevModeMenuItem) me)
      (bringtofront sk8::SneakyDevModeMenuItem))
    (setf (menu sk8::SneakyDevModeMenuItem) nil)))

(new menuItem :objectName "PMDEVMODE" :project ui)
(setf (text ui::PMDEVMODE) "Sneaky Dev-Mode")
(define-handler menuSelect (ui::PMDEVMODE)
  (dev-mode :force? t))

(define-handler Update (UI:PBMenuProject)
  (call-next-method)
  (if (and (fboundp 'sk8:dev-mode) (optionkeydown) (commandkeydown))
    (progn 
      (setf (menu ui::PMDEVMODE) me)
      (bringtofront ui::PMDEVMODE))
    (setf (menu ui::PMDEVMODE) nil)))

#|
	Change History (most recent last):
	2  	 2/27/97	sidney  	New file added to make MCL listener only available in developer build
	3  	 2/27/97	Hernan  	
|# ;(do not edit past this line!!)
