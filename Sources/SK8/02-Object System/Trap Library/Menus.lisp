(in-package :SK8DEV)

;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories




(defun_X T_NewMenuGC nil (menuID menuTitle)
  "Returns :Menu GCHandle.
See IM-Mac Toolbox Essentials p. 105"
  (makeGCOSPtr
   (with-pstrs ((menuTitlePstr menuTitle))
     (checking-Toolbox-Error (:pointer)
                             (#_NewMenu menuID menuTitlePstr)))
   #_DisposeMenu))

#|
	Change History (most recent last):
	1	2/28/94	dy	
	2  	11/28/94	dy      	gc macro mods
	3  	 2/10/95	dy      	tweaks to invocations of macros that reduce to make-terminable-macptr
	2  	 2/27/97	Hernan  	Removing case preserving reader macro.
|# ;(do not edit past this line!!)
