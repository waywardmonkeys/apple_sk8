
;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories




(in-package :graphics-system)

#| Modification History

02-20-93 hernan the boundsRegion is offseted by the position of the window in shaped-window-wdef.
02-19-93 adam  Brand new! (from old salvaged pieces)

|#

;;; NOTE: the shaped-window replaces the blank-window (which is now obsolete) -- "sk8-blank-windows.lisp" should no longer load.



;;; Routine defining a shaped-window whose structure and content regions are both simply the boundsRegion of the 
;;; window's toplevelActor
;;;
;;; IMPORTANT NOTE:  This routine expects the node-boundsRegion to either be valid or computable!!!  I.e. you must ensure that 
;;;                the actor's internal geometry information is up to date before calling any WindowManager routine that 
;;;                will cause the window's shape to be recomputed (e.g. MoveWindow, SizeWindow, ZoomWindow, etc?).

#-ppc-target
(defpascal shaped-window-wdef (:word variation :ptr wptr :word message :long parameter :long)
  (declare (ignore variation))
  (without-interrupts
   (let ((result 0))
     (case message
       ;; NOTE: Totally ignores the #$wDraw message 
       ;;       SK8's graphics engine paints the entire structure/content region!
       (#.#$wHit
        (setq result (if (#_PtInRgn parameter (rref wptr windowRecord.contRgn))
                       #$wInContent
                       #$wNoHit)))
       (#.#$wCalcRgns
        (let+ ((clos-window (window-object wptr))
               (tla-actor (slot-value clos-window 'my-actor-object))
               (tla-flags (node-flags tla-actor))
               (boundsRegion (node-boundsRegion tla-actor))
               (temp-rgn (:region)) 
               (pos (view-position clos-window)))
          (when (boundsDirty? tla-flags) 
            (onlyLocationChanged! tla-flags 0)
            (update-bounds-region tla-actor tla-flags)
            ;; (SK8::makeBoundsRegion tla-actor)
            )
          ;; Offset the region by the location of the window! HERNAN
          (#_copyRgn boundsRegion temp-rgn)
          (#_offsetRgn temp-rgn (point-h pos) (point-v pos))
          ;; Now use the new region for the window's data!
          (#_CopyRgn temp-rgn (rref wptr windowRecord.contRgn))
          (#_CopyRgn temp-rgn (rref wptr windowRecord.strucRgn))
          ;; (setf (rref wptr windowrecord.contrgn) (content-region clos-window)) 
          ;; (setf (rref wptr windowrecord.strucrgn) (cl-user::structure-region clos-window)) 
          )))
     result)))

;;; In the PPC version this is used to do the work that the WDEF does in the 68K version.

#+ppc-target
(defun wdef-calc-regions (wptr)
  (let+ ((clos-window (window-object wptr))
         (tla-actor (slot-value clos-window 'my-actor-object))
         (tla-flags (node-flags tla-actor))
         (boundsRegion (node-boundsRegion tla-actor))
         (temp-rgn (:region)) 
         (pos (view-position clos-window)))
    (when (boundsDirty? tla-flags) 
      (onlyLocationChanged! tla-flags 0)
      (update-bounds-region tla-actor tla-flags)
      ;; (SK8::makeBoundsRegion tla-actor)
      )
    ;; Offset the region by the location of the window! HERNAN
    (#_copyRgn boundsRegion temp-rgn)
    (#_offsetRgn temp-rgn (point-h pos) (point-v pos))
    ;; Now use the new region for the window's data!
    (#_CopyRgn temp-rgn (rref wptr windowRecord.contRgn))
    (#_CopyRgn temp-rgn (rref wptr windowRecord.strucRgn))
    ))

;;; Make wdef for shaped-window

#-ppc-target
(setf *blank-window-wdef-handle* (ccl::make-wdef-handle shaped-window-wdef))

;;; Fix definition of install-sk8-wdef to use *shaped-window-wdef-handle* for "blank" windows.

(defun install-sk8-wdef (wptr style)
  (ecase style
    (sk8::sk8Window
     (unless (%null-ptr-p *SK8-window-wdef-handle*)
       (rset wptr :window.windowDefProc *SK8-window-wdef-handle*)))
    (sk8::blank
     (unless (%null-ptr-p *blank-window-wdef-handle*)
       (rset wptr :window.windowDefProc *blank-window-wdef-handle*)))
    (sk8::floating
     (unless (%null-ptr-p ccl::*windoid-wdef-handle*)
       (rset wptr :window.windowDefProc ccl::*windoid-wdef-handle*)))))

#|
	Change History (most recent last):
	2	10/1/93	hernan	=====================================
				= Sad but true: the node is gone!!! =
				=====================================
	3	12/21/93	sidney	Changes so files can be compiled
	4	2/25/94	hernan	Using symbols instead of keywords for options!!!
	5	7/20/94	Hernan	Totally ignoring the sk8Window wdef since it
				crashes the system on preserve. This is just a 
				temporary thing until we figure out what is 
				wrong with it.
	6	7/20/94	Hernan	No need for the temporary change anymore.
	7  	 8/31/94	Hernan  	Added 'floating' wdef.
	8  	 9/28/94	Hernan  	Exploring...
	9  	 2/10/95	sidney  	changes for buildstandalone merging resources into application file
	2  	 7/23/96	Hernan  	The shaped window wdef is only in effect when compiling
						for the 68K.
	3  	 7/25/96	Hernan  	I do not think I changed anything here...
	4  	 2/27/97	Hernan  	
|# ;(do not edit past this line!!)
