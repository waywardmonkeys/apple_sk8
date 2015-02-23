;;;; The System 7.0 icon plotting utilities.
;;;; These are only the ones from the technote.

(in-package :traps)

;; IconTransformType values
(defconstant $ttNone #x0)
(defconstant $ttDisabled #x1)
(defconstant $ttOffline #x2)
(defconstant $ttOpen #x3)
(defconstant $ttSelected #x4000)
#|
(defconstant $ttSelectedDisabled #x4001)
(defconstant $ttSelectedOffline $4002)
(defconstant $ttSelectedOpen $4003)
|#

#|
;; ttLabels
(defconstant $ttLabel0 $0000)
(defconstant $ttLabel1 $0100)
(defconstant $ttLabel2 $0200)
(defconstant $ttLabel3 $0300)
(defconstant $ttLabel4 $0400)
(defconstant $ttLabel5 $0500)
(defconstant $ttLabel6 $0600)
(defconstant $ttLabel7 $0700)
|#

(deftrap "_PlotIconID" ((rect :Rect) (align :integer) (transform :integer) (resId :integer))
              (:stack :oserr)
   (:trap #xABC9 :d0 #x0500 rect align transform resID))

(deftrap "_PlotCIconHandle" ((rect :Rect) (align :integer) (transform :integer) (cIcon :CIconHandle))
              (:stack :oserr)
   (:trap #xABC9 :d0 #x051F rect align transform cIcon))

#|
#$kCustomIconResource
#$genericExtensionIconResource
#$genericDocumentIconResource
#$genericApplicationIconResource
#$genericDeskAccessoryIconResource
#$genericFolderIconResource
#$systemFolderIconResource
#$appleMenuFolderIconResource
#$startupFolderIconResource
#$controlPanelFolderIconResource
#$printMonitorFolderIconResource
#$preferencesFolderIconResource
#$extensionsFolderIconResource
#$floppyIconResource
#$trashIconResource
#$fullTrashIconResource
|#
#|
	Change History (most recent last):
	1	4/7/92	ows	
	1	4/7/92	ows	
	1	4/7/92	ows	
	2  	 6/28/96	sidney  	deftrap needs a string now
|# ;(do not edit past this line!!)
