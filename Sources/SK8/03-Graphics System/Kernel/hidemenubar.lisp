;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories


;;;


;;;
;;; Hide/Show the Macintosh Menubar
;;;

(in-package :graphics-system)

#| Modification History

02-04-93 adam  added save-gray-region -- computes the saved gray region instead of simply copying what's there (to be safe)
01-22-93 adam  rewrote: access handle-based records more carefully & no longer allocate regions repeatedly
01-22-93 ruben id -> objectName
09-11-92 ruben moved from 2-d-kernel to mf-kernel; added draw argument to showmenubar
05-12-92 ruben d29 conversion

|#

;; *** THIS IS JUST TO BOOTSTRAP THE PATCH!!!
(SETQ mf::*SAVEGRAYREGION* NIL)


(defun save-menubar-height ()
  (when (eql 0 (setq mf::*savembarheight* (%get-word (%int-to-ptr #$mbarheight))))
    ;; Use the normal default if the global is already hosed for whatever reason
    (setq mf::*savembarheight* 20)))


(defun save-gray-region ()
  (unless (handlep mf::*savegrayregion*)
    (setq mf::*savegrayregion* (T_NewRgnGC))
    (let+ ((mbarrect (:rect))
           (menuBarRgn (:region))
           (screenbounds (:rect))
           (grayrgn (%get-ptr (%int-to-ptr #$grayrgn))))
      (declare (dynamic-extent grayrgn))
      (#_CopyRgn grayrgn mf::*savegrayregion*)
      
      ;; This is much safer than setting screenbounds to (get-record-field (#_GetMainDevice) :GDevice :gdrect)
      (with-dereferenced-handles ((gdev (#_GetMainDevice)))
        (copy-record (rref gdev :GDevice.gdrect :storage :pointer) :rect screenbounds))
      
      (copy-record screenbounds :rect mbarrect)
      (rset mbarrect :rect.bottom (+ (rref mbarrect :rect.top) mf::*savembarheight*))
      (#_RectRgn menuBarRgn mbarrect)
      
      ;; Remove the menubar's area from the desktop region
      (#_DiffRgn mf::*savegrayregion* menuBarRgn mf::*savegrayregion*))))



;;; hidemenubar -- hides the Macintosh menubar
;;;   force - unconditionally hide it if force is non-nil
;;;
(defun HideMenuBar (&optional force)
  (when (or force mf::*mBarVisible*)
    (let+ ((mbarrect (:rect))
           (menuBarRgn (:region))
           (screenbounds (:rect))
           (grayrgn (%get-ptr (%int-to-ptr #$grayrgn)))
           (frontWindow (#_FrontWindow)))
      (declare (dynamic-extent grayrgn frontWindow))

      (unless (and (fixnump mf::*savembarheight*) (neq mf::*savembarheight* 0)) (save-menubar-height))
      (unless (handlep mf::*savegrayregion*) (save-gray-region))
      
      (setq mf::*mBarVisible* nil)

      ;; This is much safer than setting screenbounds to (get-record-field (#_GetMainDevice) :GDevice :gdrect)
      (with-dereferenced-handles ((gdev (#_GetMainDevice)))
        (copy-record (rref gdev :GDevice.gdrect :storage :pointer) :rect screenbounds))
      
      ;; Set menubar height to zero
      (%put-word (%int-to-ptr #$mbarheight) 0)

      (copy-record screenbounds :rect mbarrect)
      (rset mbarrect :rect.bottom (+ (rref mbarrect :rect.top) mf::*savembarheight*))
      (#_RectRgn menuBarRgn mbarrect)
      
      ;; Expand the desktop region to include the menubar's area
      (#_UnionRgn grayrgn menuBarRgn grayrgn)

      ;; Paint the newly added area of the desktop region
      (#_PaintOne (%null-ptr) menuBarRgn)

      ;; Make sure any windows intersecting the menBarRgn paint themselves & update their visRgns as necessary
      (#_PaintOne frontWindow menuBarRgn)
      (#_PaintBehind frontWindow menuBarRgn)
      (#_CalcVis frontWindow)
      (#_CalcVisBehind frontWindow menuBarRgn))))



;;; showmenubar -- shows the Macintosh menubar.
;;;   force - unconditionally show it if force is non-nil
;;;   draw - draw it if draw is non-nil
;;;
(defun ShowMenuBar (&optional (force nil) (draw t))
  (when (or force (not mf::*mBarVisible*))
    (let+ ((grayrgn (%get-ptr (%int-to-ptr #$grayrgn)))
           (frontWindow (#_FrontWindow)))
      (declare (dynamic-extent grayrgn frontWindow))
      
      (unless (and (fixnump mf::*savembarheight*) (neq mf::*savembarheight* 0)) (save-menubar-height))
      (unless (handlep mf::*savegrayregion*) (save-gray-region))
      
      (setq mf::*mBarVisible* t)
      
      ;; Set menubar height to the saved height
      (%put-word (%int-to-ptr #$mbarheight) mf::*savembarheight*)
      
      ;; Restore desktop region to the saved region
      (#_CopyRgn mf::*savegrayregion* grayrgn)

      ;; (with-port (cl-user::get-wmgr-port) (#_SetClip grayrgn))  -- necessary?
      ;; (#_HiliteMenu 0)  -- necessary?

      (when draw (#_DrawMenuBar))

      ;; Make sure any windows intersecting the menBarRgn update their visRgns as necessary
      (#_CalcVis frontWindow)
      (#_CalcVisBehind frontWindow grayrgn))))

#|
	Change History (most recent last):
	2	2/28/94	hernan	Now using GCable mac things.
	3	3/1/94	sidney	T_NewRgnGC has to be SK8::T_NewRgnGC here
	2  	 2/27/97	Hernan  	
|# ;(do not edit past this line!!)
