;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories



(in-package :sk8dev)

(SK8-declare-syms :SK8 :public ; Updated  9-11-96   5:46 pm
                  SK8::CURRENTERRORMESSAGE SK8::EDITORSCRIPTEDITTEXT SK8::INDICATESCRIPTCOMPILEWARNINGS)

(provide "EDITORSCRIPTEDITTEXT")

(require "SCRIPTEDITTEXT" "objects;EditText:ScriptEditText")
(require "YESORNODIALOGBOX" "objects;Dialogs:YesOrNoDialogBox")
(require "HANDLERSTORE" "sk8;06-SK8Script:Store:RAM Store")

;;; REQUIRES THE COMPILER. 

;;; This file implements ScriptEditorEditText, a ScriptEditText that also knows 
;;; how to deal with handlers/functions, the store, breakpoints and debugging.

(defconstant $breakpoint-pane-width 20)

;;; EditorScriptEditText -- the EditorScriptEditText actor. Note that it contains a slot that holds its clip region at all
;;;           times. This allows the field to draw itself properly outside the draw loop.

(new ScriptEditText :objectName "EditorScriptEditText" :project SK8
     :properties '(;; Handler indentification.
                   (inputHandler :value nil)             ;; may be nil if the handler hasn't yet been activated
                   (inputHandlerID :value nil)
                   ;; Breakpoints.
                   (breakPointsVisible :value t)         ;; Whether the breakpoints strip is visible.
                   (linesWithBreakpoints :value nil)     ;; A list of line numbers that have breakpoints.
                   ;; Variables info.
                   (globalVariables :value nil)          ;; variables recognized as global in the script.
                   (localVariables :value nil)           ;; variables recognized as local in the script.
                             ;; Store.
                   (versionDisplayed :value nil)         ;; the version of the handler currently displayed.
                   (availableVersions :value nil)        ;; all the versions available of this handler.
                   (displayingActiveVersion :value nil)  ;; are we displaying the active version?
                   (savedModCount :value 0)              ;; Saved number of modifications to the buffer.
                   (versionmodified :value nil)
                   ;; Debugging.
                   (runningMode :value nil)
                   (runningDepth :value nil)
                   (runningLine :value nil)
                   (currentErrorMessage :value nil)
                   ))

(setf (private EditorScriptEditText :property 'savedModCount) t)
(setf (autotab editorScriptEditText) nil)

(setf (wantsMouseWithin EditorScriptEditText) t)

;;; Need to redefine this because initialize of EditText sets the draw-function to editText-draw.
;;; Some day, remove all these silly settings from the initialize methods.

(define-handler initialize (EditorScriptEditText original isnew args)
  (declare (ignore-if-unused original isnew args))
  (call-next-method)
  (setf (drawFunction me) 'EditorScriptEditText-draw))

;;; _______________________________ 
;;; Utilities
;;; _______________________________ 

(define-handler dirty (EditorScriptEditText)
  (let ((edFred (editData me)))
    (not (eq (savedModCount me) (buffer-modcnt (fred-buffer edFred))))))

(define-handler (setf dirty) (boolean EditorScriptEditText &key (modification t))
  (let ((edFred (editData me)))
    (cond
     (boolean
      (if modification
        (unless (dirty me) (just-modified-scriptText-for-1st-time me))
        (setf (savedModCount me) t))
      t)
     (t
      (setf (savedModCount me) (buffer-modcnt (fred-buffer edFred)))
      nil))))

(defun just-modified-scriptText-for-1st-time (self)
  (setf (savedModCount self) t)
  (with-focused-view nil (textModified self)))

(define-handler textModified (ScriptEditText)
  )

(define-handler update (EditorScriptEditText &key indentation)
  (when indentation
    ;; Reindent and update dirty lines in the whole buffer
    (indent-everything (editData me))
    ;; Only redraw if necessary
    (forceRedraw me)))

(defun select-entire-line (self lineNum &key deselect)
  (decf lineNum)
  (when (>= lineNum 0)
    (let* ((edFred (editData self))
           (buf (fred-buffer edFred)))
      (cond
       (deselect
        (multiple-value-bind (s e) (selection-range edFred)
          (if (and (eql s (buffer-line-start buf 0 lineNum))
                   (eql e (buffer-line-start buf 0 (1+ lineNum))))
            (setSelection self s s)
            ;; The caller relies on this update happening, even if the selection isn't changing
            (forceRedraw self :justBreakpoints t))))
       (t
        (let* ((currentFirstLine (buffer-line buf))
               (linesShown (CCL::FREC-FULL-LINES (frec edFred))))
          (unless (<= currentFirstLine lineNum (+ currentFirstLine linesShown -1))
            (set-mark buf (buffer-line-start buf 0 (max 0 (- lineNum (truncate linesShown 2))))))
          (setSelection self 
                        (buffer-line-start buf 0 lineNum) 
                        (buffer-line-start buf 0 (1+ lineNum)))
          (showSelection self)))))))

;;; _______________________________ 
;;; Drawing.
;;; _______________________________ 

(defun draw-runningLine-arrow (top &optional greyed)
  (when greyed (#_PenPat *gray-pattern*))
  (#_MoveTo -5 (+ top 10))
  (#_Line 0 -9)
  (#_Move 1 1)
  (#_Line 0 7)
  (#_Move 1 -1)
  (#_Line 0 -5)
  (#_Move 1 1)
  (#_Line 0 3)
  (#_ForeColor #$whiteColor)
  (#_Move 1 0)
  (#_Line 0 -3)
  (#_ForeColor #$blackColor)
  (#_Move 0 1)
  (#_Line 0 1)
  (when greyed (#_PenPat *black-pattern*)))

(defun draw-runtimeError-marker (top)
  (let ((center -12))
    (rlet ((r :rect :left -20 :top top :right -6 :bottom (+ top 11)))
      (#_EraseRect r))
    
    (#_ForeColor #$yellowColor)
    (#_MoveTo center (+ top 1))
    (#_Line 0 2)
    (#_Move -1 0)
    (#_Line 3 0)
    (#_Move 0 1)
    (#_Line -3 0)
    (#_Move 4 1)
    (#_Line -5 0)
    (#_Move 0 1)
    (#_Line 5 0)
    (#_Move 1 1)
    (#_Line -7 0)
    (#_Move 0 1)
    (#_Line 7 0)
    (#_Move 1 1)
    (#_Line -9 0)
    
    (#_ForeColor #$blackColor)
    (#_MoveTo center top)
    (#_Line 5 10)
    (#_Line -10 0)
    (#_Line 5 -10)
    
    (#_MoveTo center (+ top 3))
    (#_Line 0 2)
    (#_Move 0 2)
    (#_Line 0 1)))

(defun erase-runtimeError-marker (top)
  (rlet ((r :rect :left -20 :top top :right -6 :bottom (+ top 11)))
    (#_EraseRect r)))

;;; rewritten to stop using that nasty line-info data structure.

(defun redraw-breakPoints-strip (fredActor &optional (ed-fred (editData fredActor)) (frec (frec ed-fred)))
  (gs:let+ ((startLineNum (buffer-line (fred-buffer ed-fred) (ccl::fr.wposm frec)))
         (lineNum startLineNum)
         (numLinesDisplayed (max 0 (ccl::fr.numlines frec)))
         (numLinesLeft numLinesDisplayed)
         (relevantBreakpointInfo (line->breakpointinfohead fredActor lineNum))
         (runningLine (runningLine fredActor))
         ;; OJO! No single line height anymore!!! Work here!
         (lineHeight (frec-lineheight frec))
         (fieldHeight (point-v (ccl::fr.size frec)))
         (eraserRect (:rect))
         (breakPointRect (:rect))
         (conditionDotRect (:rect))
         (activeVersion? (displayingActiveVersion fredActor))
         breakOn?)
    (declare (fixnum startLineNum lineNum numLinesDisplayed numLinesLeft runningLine lineHeight))
    (with-rgb (rgb (MCL-color (frameColor fredActor))) (#_RGBForeColor rgb))
    (#_SetRect eraserRect (- $breakpoint-pane-width) 0 -1 lineHeight)
    (#_SetRect breakPointRect (- 4 $breakpoint-pane-width) 1 -6 (1- lineHeight))
    (copy-record breakPointRect :rect conditionDotRect)
    (#_InsetRect :ptr conditionDotRect :long #@(4 4))
    ;; Draw breakpoints for all the lines currently displayed
    ;; If the current version is not the active version, draw the breakpoints only in outline and in gray.
    (if (not activeVersion?) (with-rgb (rgb (MCL-color gray)) (#_RGBForeColor rgb)))
    (loop
      (when (eql 0 numLinesLeft) (return))
      (#_EraseRect eraserRect)
      (when (eq lineNum (caar relevantBreakPointInfo))
        (setq breakOn? (cdar relevantBreakPointInfo))
        (if (and breakOn? activeVersion?) 
          (#_PaintRoundRect breakPointRect 5 5) 
          (#_FrameRoundRect breakPointRect 5 5))
        (when nil ;; (lineBreakinfo-conditionVisible? info) No conditional bkpts yet.
          (rset conditionDotRect :rect.top (+ (rref breakPointRect :rect.top) 4))
          (rset conditionDotRect :rect.bottom (- (rref breakPointRect :rect.bottom) 4))
          (if breakOn? (#_EraseRect conditionDotRect) (#_PaintRect conditionDotRect)))
        (setf relevantBreakPointInfo (cdr relevantBreakPointInfo)))
      (#_OffsetRect eraserRect 0 lineHeight)
      (#_OffsetRect breakPointRect 0 lineHeight)
      (incf lineNum)
      (decf numLinesLeft))
    ;; Erase the remainder
    (with-rgb (rgb (MCL-color (frameColor fredActor))) (#_RGBForeColor rgb))
    (rset eraserRect :rect.bottom fieldHeight)
    (#_EraseRect eraserRect)    
    ;; Draw the border line
    (#_MoveTo :long #@(-1 0))
    (#_Line 0 fieldHeight)
    ;; If code's currently running and running line's showing, draw the arrow (& maybe the error marker)
    (when (and runningLine (< -1 (decf runningLine (1+ startLineNum)) numLinesDisplayed))
      (let ((runningLineTop (+ 0 (* lineHeight runningLine))))
        (when (runtimeError fredActor) (draw-runtimeError-marker runningLineTop))
        (draw-runningLine-arrow runningLineTop (neq (runningDepth fredActor) :innermost))))))

;;; Do the frec-set-size within a port, or else!

(defun script-synch-scriptEditText-to-fillRegion (theField)
  (let ((f (editData theField))
        (breakpointsVis? (breakPointsVisible theField))
        newsize topleft frec)
    (when f
      (setq frec (frec f))
      (gs:recompute-fill-region theField (gs:node-flags theField))
      (with-dereferenced-handles ((rgnPtr (gs:node-fillRegion theField)))
        (%setf-macptr rgnPtr (rref rgnPtr :region.rgnBBox :storage :pointer))
        (setq newsize (make-point
                       (- (the fixnum (rref rgnPtr :rect.right)) (the fixnum (rref rgnPtr :rect.left))
                          (if breakpointsVis? $breakpoint-pane-width 0))
                       (- (the fixnum (rref rgnPtr :rect.bottom)) (the fixnum (rref rgnPtr :rect.top)))))
        (setq topleft (if breakpointsVis?
                        (add-points (rref rgnPtr :rect.topleft) (make-point $breakpoint-pane-width 0))
                        (rref rgnPtr :rect.topleft))))
      (unless (eql (view-position f) topleft)
        (setf (slot-value f 'view-position) topleft))
      (with-fred-port f
        (unless (eql (CCL::FREC-SIZE frec) newsize)
          (update-bwin frec)
          (setf (slot-value f 'view-size) newsize)
          (CCL::FREC-SET-SIZE frec newsize))))))

;;; Does not call next method because the synch function is different.

(define-handler makeFillRegion (EditorScriptEditText)
  (gs:let+ ((rect (:rect :SK8Rect (gs:recompute-physicalBoundsRect me))))
    (sk8-multival-bind (frameX frameY) (framesize me :physical t)
      (#_InsetRect rect frameX frameY)
      (#_RectRgn (gs:node-fillRegion me) rect)
      (gs:fillDirty! (gs:node-flags me) 0))
    ;; Make sure the field is ALWAYS in synch with the fillRegion.
    (script-synch-scriptEditText-to-fillRegion me)))

;;; 1.0
;;; FORCEREDRAW -- forces the field to redraw itself through the draw loop. Now uses the owned region.
;;;               The idea is to draw the field into the gWorld and then copy it back to the window.
;;;               All this happened OUTSIDE of the draw loop.
;;;
;;; NOTE that the field (using the owned region) needs to respect the locking protocol for windows. This
;;; means that no direct drawing should take place when the window is locked. Thus, we only draw
;;; directly when the lock level of the window is below 2.

(defvar *enable-ownedregion-check* nil)

(define-handler forceRedraw (EditorScriptEditText &key forceRedraw updateScrollers justBreakpoints)
  (let* ((f (editData me))
         (frec (CCL::frec-arg (frec f)))
         (savedModcnt (slot-value f 'CCL::file-modcnt)))
    (cond
     ;; If it's locked, we need to mark it as being dirty so it redraws when it's unlocked
     ((locked me) (lightForceRedraw me))
     ;; If it is a window, it should draw itself with all its actors!
     ((gs:hasWindow? (gs:node-flags me))
      (gs:making-self-dirty (me nil t t t (gs:node-boundsRegion me) :bounds nil)
        (gs:dirty-subnodes* me)))
     ;; It's not locked; update the screen only if it's deeply visible
     ((deeplyVisible me)
      (let* ((clos-window (gs:node-window me))
             (lockLevel (when clos-window (slot-value clos-window 'gs:locked)))
             (gworld (when clos-window (slot-value clos-window 'gs:gWorld))))
        ;; Scream if region empty!
        ;; (WHEN (AND CLOS-WINDOW (WINDOW-SHOWN-P CLOS-WINDOW)
        ;;            (ownedRegion me) (#_EMPTYRGN (ownedRegion me)))
        ;;   (WITH-FOCUSED-VIEW NIL (FORMAT T "~&'ownedRegion' OF ~S IS EMPTY!!!~%" (OBJECTSTRING me))))
        (cond
         (lockLevel (silly-field-force-update me))
         (clos-window
          (let ((wptr (wptr clos-window)))
            ;; Do the update on the window
            (with-fred-drawing (me nil)
              (unless justBreakpoints
                (if forceRedraw
                  (CCL::frec-draw-contents frec t)
                  (CCL::frec-update frec)))
              (when (breakpointsVisible me) (redraw-breakPoints-strip me f frec)))
            ;; When there's a gworld, copy the new bits back to the window. 
            (when (and gworld wptr)
              (gs:let+ ((visBlitRgn (:region)))
                ;; Only blit the parts of the drawing that are visible (not obscured)
                (#_SectRgn (ownedRegion me) (rref wptr :CGrafport.visRgn) visBlitRgn)
                ;; Make sure the gWorld's origin is 0!
                (gs:with-zero-origin 
                 gWorld
                 (gs:gWorld-to-window gworld clos-window nil visBlitRgn))))
            ))))))
    ;;(when (and (neq savedModcnt t) (neq savedModcnt (buffer-modcnt (ccl::fr.wposm frec))))
    ;; It's been dirtied since the last update
    ;; (just-modified-scriptText-for-1st-time me))
    (when updateScrollers
      (updatePartnerScroller me (partnerVScroller me))
      (updatePartnerScroller me (partnerHScroller me)))))

;;; 1.0
;;; EditorScriptEditText-DRAW -- this is the draw function that EditorScriptEditText actors use to draw themselves. By the time this
;;;                      is called in the draw loop, the clip region of the field has been calculated. Thus, we save
;;;                      it in the slot ownedRegion of the actor. We call draw-field to draw the innards of the field.

(defun EditorScriptEditText-draw (theActor destPort draw-region)
    (let ((flags (gs:node-flags theActor)))
      (gs:recompute-frame-region theActor flags)
      (gs:recompute-fill-region theActor flags)
      (gs:let+ ((f (editData theActor))
             (frec (CCL::frec-arg (frec f)))
             ;; (clos-window (gs:node-window theActor))
             (fillRegion (gs:node-fillRegion theActor))
             ;; (effective-blit-rgn (:region))
             (effective-rgn (:region))
             (ownedRegion (ownedRegion theActor)))
        ;; Rendering the frame...
        (render (gs:node-frameColor theActor) theActor (gs:node-frameRegion theActor) destPort)
        ;; Render the contents...
        (#_SectRgn fillRegion draw-region effective-rgn)
        (gs:with-clipped-region effective-rgn
          (funcall (gs:node-contentsDrawFunction theActor) theActor destPort effective-rgn fillRegion t))
        ;; Computing and Saving the clip region of the actor!
        (when (#_EmptyRgn ownedRegion)
          (#_CopyRgn effective-rgn ownedRegion))
        ;; Even though we can trim the draw region when the node's already been seen dirty, we must blit the
        ;; ENTIRE effective region back to the gworld, so save that region in effective-blit-rgn before trimming.
        ;; BUT now that we draw into gWorlds, is this needed?
        ;; (#_CopyRgn effective-rgn effective-blit-rgn)
        ;; (unless (and clos-window (gs:nodeSeenDirtyByWindow? flags))
        ;;  (#_SectRgn effective-rgn gs:*original-dirty-region* effective-rgn))
        (unless (locked theActor)
          (with-fred-drawing (theActor nil f destport)
            ;; If bwin is out of whack, make sure it is updated.
            (update-bwin frec)
            (CCL::frec-draw-contents frec)
            (when (breakPointsVisible theActor) (redraw-breakPoints-strip theActor f frec))))
        )))


(setf (drawFunction EditorScriptEditText) 'EditorScriptEditText-draw)


;;; _______________________________ 
;;; Event Handling.
;;; _______________________________ 

(define-handler displayMessage (EditorScriptEditText messageString)
  (messageToUser messageString))

(define-handler mouseDownInErrorMarker (EditorScriptEditText)
  (when (currentErrorMessage me)
    (displayMessage me (currentErrorMessage me))))

(define-handler mouseDownInBreakPoint (EditorScriptEditText lineNum)
  (if (displayingActiveVersion me)
    (unless (maybe-complain-about-accessor-method me)
      (let* ((breakPointInfo (line->breakpointinfo me lineNum))
             (newBkptStatus (not (cdr breakPointInfo))))
        (when breakPointInfo
          ;; Toggle the breakpoint's status.
          (setf (cdr breakPointInfo) newBkptStatus)
          ;; Redraw (just the breakpoints)
          (forceRedraw me :justBreakpoints t)
          ;; When this is the active version, update the actual breakpoint. Only
          ;; works when we do not need to remcompile to add a breakpoint.
          (when (activeVersion me :error nil)
            (if newBkptStatus
              (setBreakpoint (inputHandlerId me) lineNum)
              (clearBreakpoint (inputHandlerId me) lineNum))))))
    (messagetouser "Breakpoints can only be set in the active version" :beep t)))

(defun mouseDown-in-breakPoints-strip (fredActor)
  (sk8-multival-bind (fh fv) (framesize fredactor :physical t)
    (declare (ignore fh))
    (gs:let+ ((ed-fred (editData fredActor))
              (frec (frec ed-fred))
              (lineNum (buffer-line (ccl::fr.wposm frec)))
              (lineHeight (frec-lineheight frec))
              (fieldTop (+ (top fredactor :physical t) fv)) ;; (frameheight (point-v (view-position ed-fred)))
              (fieldHeight (point-v (ccl::fr.size frec)))
              (clickedLine (+ lineNum (the fixnum (truncate (- (eventv) fieldTop) lineHeight))))
              (runtimeError (runtimeError fredActor)))
      (declare (fixnum lineNum lineHeight fieldTop fieldHeight clickedLine))
      (when (<= fieldTop (eventv) (+ fieldTop fieldHeight)) ; make sure we're not in the frame region
        (cond
         ;; Runtime error? Take it as a click on error marker.
         ((and runtimeError (eql clickedLine (1- (runningLine fredActor))))
          (mouseDownInErrorMarker fredActor))
         ;; Not runtime. If there is a breakpoint here proceed.
         ((line->breakPointInfo fredActor clickedLine)
          (mouseDownInBreakPoint fredActor clickedLine)))))))

;;; Returns whether the mouse was over the breakpoints strip. 
;;; Does not use *event-x* or *event-y*. 

(defun mouseoverbreakpoints (theEditText)
  (let ((mouseh (eventh))
        (mousev (eventv)))
    (sk8-multival-bind (ll tt rr bb) (boundsrect theEditText :physical t)
      (declare (ignore rr))
      (sk8-multival-bind (fh fv) (framesize theEditText :physical t)
        (and (>= (- bb fv) mousev (+ tt fv))
             (>= (+ ll fh $breakpoint-pane-width) mouseh (+ ll fh)))))))

;;; 1.0
;;; MOUSEDOWN -- the mouseDown method for fields. Sets the current key of its window to
;;;             itself and calls handle-mousedown to deal with selecting text and
;;;             placing the cursor.

(define-handler mouseDown (EditorScriptEditText)
  ;; Make sure this actor is the keytarget of its window. This is required
  ;; to make the field highlight in that nice UI way.
  (let ((topLevelActor (sk8::window me)))
    (unless (eq (keyTarget topLevelActor) me)
      (setf (keyTarget topLevelActor) me)))
  (when (dirty me)
    ;; On click, if we were in another line in the field, make sure this line
    ;; is indented and the breakpoint info is up to date. 
    (indent-this-line (editData me))
    (update-breakpoint-this-line me)
    (update-saved-modcount me))
  (cond
   ;; Case 1: mousedown in breakpoints strip. 
   ((and (breakPointsVisible me) (mouseoverbreakpoints me))
    (mouseDown-in-breakPoints-strip me))
   ;; Case 2: field locked or indirect mouseDown call. Pass the event along.
   ((or (locked me) (lockedText me) (neq me (eventActor)))
    (let ((container (container me))) (when container (mouseDown container))))
   ;; Normal case: do the Fred thing. 
   (t
    (let ((clos-window (gs:node-window me)))
      (when clos-window
        (let* ((topLevelActor (slot-value clos-window 'gs:my-actor-object))
               (f (editData me))
               (frec (frec f))
               (curLine (buffer-line (ccl::fr.wposm frec)))
               (curHscroll (ccl::fr.hscroll frec))
               (breakpointsVisible (breakPointsVisible me))
               (*in-scriptEditText-mouseDown* me)
               (vScroller (partnerVScroller me))
               (hScroller (partnerHScroller me)))
          (declare (special *in-scriptEditText-mouseDown*))
          (unless (eq (keyTarget topLevelActor) me)
            (setf (keyTarget topLevelActor) me))
          (flet ((update-breakPoints-and-scrollers ()
                   (unless (eql curLine (setq curLine (buffer-line (ccl::fr.wposm frec))))
                     (when breakpointsVisible (redraw-breakPoints-strip me f frec))
                     (with-focused-view nil (updatepartnerScroller me vScroller)))
                   (unless (eql curHscroll (setq curHscroll (ccl::fr.hscroll frec)))
                     (with-focused-view nil (updatepartnerScroller me hScroller)))))
            (declare (dynamic-extent #'update-breakPoints-and-scrollers))
            (fred-click me clos-window #'update-breakPoints-and-scrollers))
          ;; Ok. Now copy the resulting state of the field onto the gWorld.
          (editText-to-gWorld me)
          ;; (update-dirty-lines me)
          (set-fred-last-command f nil)
          ;; Restore the event vars and dispatch a mouseUp.
          (gs:update-event-vars)
          (mouseUp me)
          ))))))

(define-handler mouseWithin (EditorScriptEditText)
  (unless (lockedtext me)
    (if (and (breakPointsVisible me) (< gs:*event-x* (point-h (view-position (editData me)))))
      (unless (eq (cursor stage) standardCursor)
        (setf (cursor stage) standardCursor))
      (unless (eq (cursor stage) ibeamCursor)
        (setf (cursor stage) ibeamcursor)))))

;;; jump a line: whenever the up or down arrow keys are pressed or when the
;;;           left arrow is pressed at the start of a line or
;;;           right arrow is pressed at the end of a line. 

(defun about-to-jump-lines? (theField key)
    (or (char= key #\UpArrow)
        (char= key #\DownArrow)
        (let ((buf (fred-buffer (editData theField))))
          (and (char= key #\BackArrow) (eq (buffer-position buf) (buffer-line-start buf)))
          (and (char= key #\ForwardArrow) (eq (buffer-position buf) (buffer-line-end buf))))))

(defmacro arrow-key-p (theKey)
  `(<= 28 (char-code ,theKey) 31))

;;; Indentation needs to be refreshed when:  
;;; (1) we jump a line and the buffer has been modified since the last
;;;     time we indented. 
;;; (2) a paste operation happens.

;;; Breakpoints need to be refreshed when:
;;; (1) we leave a line and the buffer has changed.
;;; (2) the delete key is pressed and a newline is consumed.
;;; (3) the newline key is pressed. 
;;; (4) cut and paste operations, but that happens somewhere else. 

(defmacro range-selected? (theField)
  `(sk8-multival-bind (start end) (selection ,theField)
     (not (eql start end))))

;;; Handles the delete key. 


(define-handler keydown (EditorScriptEditText key)
  (if (or (eq key #\Escape) (eq key #\Help))
    (identifierHelp me (if (breakpointsVisible me) $breakpoint-pane-width 0))
    (if (or (fieldModified? me) (not (arrow-key-p key)))
      (let* ((buf (fred-buffer (editData me)))
             (about-to-jump-lines? (about-to-jump-lines? me key))
             (fieldModified (or (fieldModified? me) (not (arrow-key-p key))))
             (simpleDeleteLine? (and (char= key #\Delete) (eq (buffer-position buf) (buffer-line-start buf))))
             (uptoDate? nil))
        ;; Do all the breakpoint update required. 
        (cond ((and about-to-jump-lines? fieldModified)
               (indent-this-line (editData me))
               (update-breakpoint-this-line me)
               (setf upToDate? t))
              (simpleDeleteLine? 
               (update-breakpoint-remove-line me)
               (setf upToDate? t)))
        ;; Actually process the character. 
        (call-next-method)
        ;; If the field has been modified, we need to update the breakpoints and
        ;; possibly whether we are showing the active version...
        (when (fieldModified? me)
          (when (displayingActiveVersion me)
            (setf (versionDisplayed me) :new)))
        ;; If the breakpoints and indentation are up to date, save now's modcount.
        (when upToDate? 
          (update-saved-modcount me)))
      (call-next-method))))

;;; Changes the breakpoint info when adding a character. 
;;; Is it a newline? Then there is stuff to do. 

(define-handler lowLevelInsert (EditorScriptEditText theString position)
  (declare (ignore position))
  (unless (stringp theString)
    (cond ((char= theString #\Newline)
           ;; process the newline. 
           (indent-this-line (editData me))
           (update-breakpoint-this-line me)
           (update-breakpoint-new-line me))
          (t nil))
    )
  (when (displayingActiveVersion me)
    (setf (versionDisplayed me) :new)))

;;; Changes the breakpoint info when we are deleting a line. 

(define-handler lowLevelDelete (EditorScriptEditText start end)
  (let ((buf (fred-buffer (editData me))))
    (unless (= (buffer-line buf start) (buffer-line buf end))
      (update-breakpoints-clipboard-operation me :cut start end))
    (when (displayingActiveVersion me)
        (setf (versionDisplayed me) :new))))

;;; This is redefined to bypass the ScriptEditText method that checks for a complete script.

(define-handler returnInField (EditorScriptEditText)
  (unless (locked me)
    (handleKey me #\Return)))

(define-handler enterInField (EditorScriptEditText)
  (unless (locked me)
    (handleKey me #\Enter)))
    
(define-handler deactivateText (EditorScriptEditText)
  ;; Do all the breakpoint update required. 
  (when (fieldModified? me)
    (indent-this-line (editData me))
    (update-breakpoint-this-line me))
  (call-next-method))




;;; ____________________________________________________________________________________________________________________________ 
;;; ____________________________________________________________________________________________________________________________ 
;;; ____________________________________________________________________________________________________________________________ 
;;; _______________________________ 
;;; Breakpoints.
;;; _______________________________ 

;;; This handler takes the breakpoint information stored in the handler and synchs the editor to it.
;;;; It ensures that both are fully in synch.

(define-handler setupBreakpoints (EditorScriptEditText)
  (let* ((handlerId (inputhandlerId me))
         (breakpoints (and handlerId (listBreakpoints handlerid))))
    (when breakpoints
      (withactorlocked (me)
        (clearBreakpoints me)
        (dolist (i breakpoints)
          (set-breakpoint-status me i t)
          (setBreakpoint handlerId i))
        (forceRedraw me :justBreakpoints t)
        ))))


;;;This clears all breakpoint info in both the editor and the handler.  Used whenever we move to a new version of the handler.
(define-handler clearBreakpoints (EditorScriptEditText)
  (let ((handlerId (inputhandlerId me))
        (breakPointsInfo (linesWithBreakpoints me)))
    (when handlerId
      (withactorlocked (me)
        (refreshBreakPointInformation me)
        (clearAllBreakpoints handlerId)
        (dolist (c breakPointsInfo)
          (when (breakpointOn? c)
            (setf (cdr c) nil)))
        (refreshBreakPointInformation me)
        (forceRedraw me :justBreakpoints t)))))



#| (setupBreakpoints ui::bing)
(clearBreakpoints ui::bing)
;;; Breakpoints API:

set breakpoint on line X.
clear breakpoint on line X.
clear all breakpoints.
set breakpoints to list.

|#

;;; The linesWithBreakpoints data structure is a list of dotted pair of the form
;;; (lineNumber . breakpointSet). The sole presence of an item means that the line
;;; it represents can potentially have a breakpoint associated to it. 

;;; The list has to be sorted by line numbers to allow for one linear search only
;;; when redrawing the breakpoints strip.

(defun line->breakPointInfo (field lineNumber)
  (assoc lineNumber (linesWithBreakpoints field)))

;;; This one can be called before drawing the whole breakpoint strip.
;;; Returns the head of the breakpoints data structure. A loop can then
;;; be clever an do a merge like procedure to avoid doing the linear search
;;; at each step.

(defun line->breakPointInfoHead (field lineNumber)
  (member lineNumber (linesWithBreakpoints field) :key #'car
          :test #'(lambda (candidateItem itemInList) (>= itemInList candidateItem))))

(defun breakpointOn? (breakPointInfo)
  (cdr breakPointInfo))

(defun anyBreakPointsSet? (field)
  (some #'(lambda (x) (breakpointOn? x)) (linesWithBreakPoints field)))

;;; Modifying the breakpoints info list... Everything happens in place. The list
;;; has to be kept sorted at all times. 

;;; 1. add a line number. Creates a new breakpoint info structure and splices it in
;;;    the right position in the list.

(defun add-breakpoint-line (field lineNumber)
  (let* ((newItem (list (cons lineNumber nil)))
         (breakPointsData (linesWithBreakPoints field))
         (afterPosition (position-if #'(lambda (x) (>= x lineNumber)) breakPointsData :key #'car))
         afterItem beforeItem)
    (cond
     ((null afterPosition)
      ;; This line is the last one yet. Add it at the end.
      (setf (linesWithBreakPoints field) (nconc breakPointsData newItem)))
     ((zerop afterPosition)
      ;; This line is the first one. Add it at the front.
      (setf (linesWithBreakPoints field) (cons (car newItem) breakPointsData)))
     (t 
      ;; normal case: do list surgery.
      (setf beforeItem (nthCdr (1- afterPosition) breakPointsData))
      (setf afterItem (cdr beforeItem))
      ;; splice in new item.
      (setf (cdr beforeItem) newItem)
      (setf (cdr newItem) afterItem)))))

;;; 2. modify number from a point by x. 

(defun change-line-number-by (field changeFrom delta)
  (let* ((breakPointsData (linesWithBreakPoints field))
         (afterPosition (position-if #'(lambda (x) (>= x changeFrom)) breakPointsData :key #'car)))
    (when afterPosition
      (dolist (c (nthCdr afterPosition breakpointsData))
        (setf (car c) (+ (car c) delta))))))

;;; 3. remove a line.

(defun remove-breakpoint-line (field lineNumber)
  (let* ((breakPointsData (linesWithBreakPoints field))
         (found? (position lineNumber breakPointsData :key #'car))
         afterItem beforeItem)
    (when found?
      (cond
       ((zerop found?)
        (setf (linesWithBreakPoints field) (cdr breakPointsData)))
       ((eql found? (1- (length breakPointsData)))
        (setf (linesWithBreakPoints field) (butlast breakpointsData)))
       (t
        ;; normal case: do list surgery.
        (setf beforeItem (nthCdr (1- found?) breakPointsData))
        (setf afterItem (cddr beforeItem))
        ;; Remove the item.
        (setf (cdr beforeItem) afterItem))))))

;;; 4. Turn on/off a breakpoint.

(defun breakpoint-status (field lineNumber)
  (cdr (line->breakpointinfo field linenumber)))

(defun set-breakpoint-status (field lineNumber newStatus)
  (let ((breakpointInfo (line->breakpointinfo field linenumber)))
    (when breakpointinfo
      (setf (cdr breakpointInfo) newStatus))))

;;; Setting this property forces a redraw.

(define-handler (setf breakPointsVisible) (boolean EditorScriptEditText)
  (let ((newState (if boolean t nil))
        (oldState (breakPointsVisible me)))
    (when (neq newState oldState)
      (setf (slot-value me 'breakPointsVisible) newState)
      (script-synch-scriptEditText-to-fillRegion me)
      (forceRedraw me :forceRedraw t))
    newState))

;;; Returns t if the line specified can have breakpoints.
;;; every line can have breakpoints except:
;;; (1) comments.
;;; (2) end bracket lines.
;;; (3) continuation lines.
;;; Any other cases?

(defun breakPointLine? (theBuffer lineStartPos)
  (let ((lineStartToken (first-word theBuffer lineStartPos)))
    (and (not (zerop (length lineStartToken)))
         (not (or (continuation-line? theBuffer lineStartPos)
                  (and lineStartToken
                       (or
                        (string-equal lineStartToken "global")
                        (string-equal lineStartToken "local")
                        (string-equal lineStartToken "end")
                        (string-equal lineStartToken "on")
                        (string-equal (subseq lineStartToken 0 (min 2 (length lineStartToken))) "--"))))))))

;;; This handler nukes the old breakpoints info data structure and scans every line in the
;;; editor to build a new one. It is assumed that no breakpoints are set in the new version.

(define-handler refreshBreakpointInformation (EditorScriptEditText)
  (let* ((theBuffer (fred-buffer (editData me)))
         (startPos 0)
         (lineCount 0)
         result)
    (do-buffer-lines (startPos theBuffer)
      (when (breakPointLine? theBuffer startPos)
        (setf result (nconc result (list (list lineCount)))))
      (incf lineCount))
    ;; Refresh the info.
    (setf (linesWithBreakPoints me) result)
    ;; Force redraw of the breakpoints.
    (forceRedraw me :justBreakpoints t)
    ))

;;; When do breakpoints need to be updated? 
;;; 1. When a new template is added.
;;; 2. When a new line is added.
;;; 3. When text is deleted.
;;; 4. When we finish editing the current line this can happen in the following ways:
;;;    * deactivating the field.
;;;    * using the arrow keys to move to another line.
;;;    * pressing the tab character.

;;; HERE ARE THE EASY CASES, TO BE CALLED ON A LINE BY LINE BASIS...

;;; This one is called when return is pressed to add a new line. 

(defun update-breakpoint-new-line (theField)
  (let* ((theBuffer (fred-buffer (editData theField)))
         (thisLineNumber (buffer-line theBuffer (buffer-position theBuffer))))
    (change-line-number-by theField (1+ thisLineNumber) 1))
  (forceRedraw theField :justBreakpoints t))

;;; This is called to possibly change the status of a line from not having a breakpoint
;;; to having one. The linesWithBreakpoints data structure is all set to change this line...
;;; Common call: when the tab key is pressed.

(defun update-breakpoint-this-line (theField)
  (let* ((theBuffer (fred-buffer (editData theField)))
         (thisLineNumber (buffer-line theBuffer (buffer-position theBuffer))))
    (if (breakPointLine? theBuffer (buffer-line-start theBuffer (buffer-position theBuffer)))
      ;; If this line did not already have a breakpointinfo pair associated to it, add one.
      (unless (line->breakpointInfo theField thisLineNumber)
        (add-breakpoint-line theField thisLineNumber))
      ;; If this line did have a breakpointinfo pair associated to it, remove it.
      (when (line->breakpointInfo theField thisLineNumber)
        (remove-breakpoint-line theField thisLineNumber))))
  (forceRedraw theField :justBreakpoints t))

;;; This one is called when delete has been keyed and BEFORE newline character has been deleted. 

(defun update-breakpoint-remove-line (theField)
  (let* ((theBuffer (fred-buffer (editData theField)))
         (thisLineNumber (buffer-line theBuffer (buffer-position theBuffer))))
    ;; If this line has a breakpoint assocated to it, remove it.
    (when (line->breakpointInfo theField thisLineNumber)
      (remove-breakpoint-line theField thisLineNumber))
    ;; Then subtract 1 from the indexes of the other breakpoints. 
    (change-line-number-by theField thisLineNumber -1))
  (forceRedraw theField :justBreakpoints t))

;;; HERE IS THE HARD CASE, HAPPENS ON EVERYTHING AT CUT/PASTE TIME.

;;; Updates the breakpoint info. Here are the argument specs:
;;; operationType: :cut or :paste.
;;; startPos: the startPosition of the selection affected in the buffer.
;;; endPos: if the operation is cut, this is the endPosition of the selection. Otherwise, 
;;;         it is the text that has been inserted.
;;; Note: this function is called before the CUT and after the PASTE.

(defun update-breakpoints-clipboard-operation (theField operationType startPos endPos)
  (if (eq operationType :cut)
    ;; Figure out how many lines are going away. Remove their breakpoints from the
    ;; editor data structure and decrement succeeding breakpoints by the number of lines.
    (let* ((theBuffer (fred-buffer (editData theField)))
           (startLine (buffer-line theBuffer startPos))
           (endLine (buffer-line theBuffer endPos))
           (totalLinesChanged (- endLine startLine)))
      (unless (zerop totalLinesChanged)
        (dotimes (i totalLinesChanged)
          (when (<= startPos
                    (buffer-line-start theBuffer 0 (+ startLine i))
                    (buffer-line-end theBuffer 0 (+ startLine i))
                    endPos)
            (remove-breakpoint-line theField (+ startLine i))))
        (change-line-number-by theField (1+ startLine) (- totalLinesChanged))))
    ;; Figure out how many lines have been added. Increment breakpoints from the start of the selection by
    ;; this number. Finally, check each new line for breakpointness. 
    (let* ((theBuffer (fred-buffer (editData theField)))
           (startLine (buffer-line theBuffer startPos))
           (totalLinesChanged (count #\Newline endPos)))
      (unless (zerop totalLinesChanged)
        (change-line-number-by theField (1+ startLine) totalLinesChanged)
        (dotimes (i totalLinesChanged)
          (when (breakpointLine? theBuffer (buffer-line-start theBuffer 0 (+ startLine i)))
            (add-breakpoint-line theField (+ startLine i 1)))))))
  (forceRedraw theField :justBreakpoints t))


;;; ____________________________________________________________________________________________________________________________ 
;;; ____________________________________________________________________________________________________________________________ 
;;; ____________________________________________________________________________________________________________________________ 
;;; _______________________________ 
;;; Clipboard
;;; _______________________________ 

(define-handler cutSelectionToClipboard (EditorScriptEditText)
  ;; Update the breakpoints.
  (if (anyBreakpointsSet? me)
    (progn (multiple-value-bind (start end) (selection-range (editData me))
             (when (or (not (eql start end))
                       (find #\Newline (buffer-substring (fred-buffer (editData me)) start end)))
               (update-breakpoints-clipboard-operation me :cut start end)))
           (call-next-method))
    (progn
      (call-next-method)
      (refreshBreakPointInformation me)))
  (when (displayingActiveVersion me)
    (setf (versionDisplayed me) :new))
  )

(define-handler pasteClipboardToSelection (EditorScriptEditText)
  ;; Do the paste. 
  (call-next-method)
  (when (displayingActiveVersion me)
    (setf (versionDisplayed me) :new))
  ;; update the breakpoints.
  (if (anyBreakpointsSet? me)
    (let ((newText (car (getFromClipboard SK8Clipboard String sk8))))
      (if (find #\Newline newText)
        (update-breakpoints-clipboard-operation me :paste (buffer-position (fred-buffer (editData me))) newText)
        (update-breakpoint-this-line me)))
    (refreshBreakPointInformation me)))
  


;;; ____________________________________________________________________________________________________________________________ 
;;; ____________________________________________________________________________________________________________________________ 
;;; ____________________________________________________________________________________________________________________________ 
;;; _______________________________ 
;;; Debugging
;;; _______________________________ 

(define-handler running (EditorScriptEditText)
  (if (runningMode me) t nil))

(define-handler runningStopped (EditorScriptEditText)
  (neq (runningMode me) :tracing))

(define-handler runtimeError (EditorScriptEditText)
  (let ((mode (runningMode me)))
    (when (eq mode :error) mode)))  ;;(when (typep mode 'error) mode)???? used to be this.


;;; Called when the handler is running and either tracing, or stopped at a breakpoint or error
(defun update-script-running-mode (self depth mode lineNum)
  (declare (fixnum lineNum))
  (incf lineNum)
  (let ((oldLineNum (runningLine self)))
    (setf (runningDepth self) depth
          (runningMode self) mode
          (runningLine self) lineNum
          )
    (unless (breakpointsVisible self) (setf (breakpointsVisible self) t))
    (unless (displayingActiveVersion self) (setf (displayingActiveVersion self) t))
    (cond
     ;; It's beginning to run
     ((null oldLineNum)
      (unless (active self) (activateText self))
      (select-entire-line self lineNum)
      (prepareForRunningMode self t))
     ;; It's continuing to run, but it's on the same line as before (it's probably changing "depth")
     ((eql lineNum oldLineNum)
      (forceRedraw self :justBreakpoints t))
     ;; It's continuing to run, and it's moved to a new line
     (t
      (select-entire-line self lineNum)
      (handlerStepped self)))))


;; Checks if we are running this handler and if so, enters running mode...
(defun maybe-enter-script-running-mode (self)
  (if (handlerRunning (inputhandlerID self))
    (update-script-running-mode self nil nil (handlerRunning (inputhandlerID self)))
    ))


;;; Called when we were just showing the running active version of a handler but now we're not
(defun switch-out-of-running-mode (self)
  (let ((oldRunningLine (runningLine self)))
    (when oldRunningLine (select-entire-line self oldRunningLine :deselect t))
    (setf (runningmode self) nil)
    (setf (runningline self) nil)
    (prepareForRunningMode self nil)
    (forceredraw self :forceredraw t)))

;;; ____________________________________________________________________________________________________________________________ 
;;; ____________________________________________________________________________________________________________________________ 
;;; ____________________________________________________________________________________________________________________________ 

;;; _______________________________ 
;;; Compiling and error reporting
;;; _______________________________ 

(defun maybe-complain-about-accessor-method (me) ; a ScriptEditText
  (when (eq 'accessor (versionDisplayed me))
    (displayMessage me 
                    "Sorry; canÕt change the debugging settings of a simple property accessor handler")))

;;; This is called after compiling, when there are some warnings. The warnings
;;; are passed in using the scriptError structs as described in the Compiler Interface file. 
;;; It runs through the editor marking the offending code by coloring. 

(define-handler indicateScriptCompileWarnings (EditorScriptEditText warnings)
  (let* ((fred (editData me))
         (buf (fred-buffer fred))
         curwarningtext
         offset charPos)
    (dolist (aWarning warnings)
      (setf offset (buffer-line-start buf 0 (lineOfError aWarning)))
      (setf charPos (characterPositionOfError aWarning))
      (when charpos
        (when (= (car charPos) (cadr charPos))
          (setf (cadr charPos) (1+ (car charPos))))
        (setf curwarningtext (descriptionOfError aWarning))
        (buffer-set-font-spec buf `(:bold 
                                    ;;:underline
                                    (:color ,(mcl-color (cond
                                                         ((and (stringp curwarningtext)
                                                               (> (length curwarningtext) 18)
                                                               (string= (subseq curwarningtext 0 18) "Undefined function"))
                                                          sk8::blue)
                                                         ((and (stringp curwarningtext)
                                                               (> (length curwarningtext) 8)
                                                               (string= (subseq curwarningtext 0 8) "Variable"))
                                                          sk8::orange)
                                                         (t
                                                          *script-syntax-error-color*)))))
                              (+ offset (car charPos))
                              (+ 1 offset (cadr charPos)))))
    ))

;;; The question of how to give the error messages remains... Options are:
;;; 1. balloon help on something painted red.
;;; 2. a message pane in the editor (does not work for things that are not the editor).
;;; 3. send to log? 
;;; 4. a message window independent of the editor... 

;;; Given that there are quite a few errors, it seems that #4 is the best option. Something
;;; like the CodeWarrior version. Clicking on an error takes you to the line where it happens
;;; in the editor.  The function above is redefined by the SK8ScriptEditor to show the warnings
;;; in the warning window. 




;;; ____________________________________________________________________________________________________________________________ 
;;; ____________________________________________________________________________________________________________________________ 
;;; ____________________________________________________________________________________________________________________________ 
;;; _______________________________ 
;;; Store
;;; _______________________________ 

#|

;;; Store API: 

are we showing the current version?
what other versions are available?
activate/save the current version.
delete the current version, going back to the previous one if any.

|#

(define-handler targetProject (EditorScriptEditText)
  (let ((handlerID (inputHandlerID me)))
    (if handlerID
      (first handlerID)
      SK8)))

;;; These are used to be able to know when the field is being changed. 

(defun update-saved-modcount (theField)
  (setf (versionmodified theField) t)
  (setf (savedModCount theField) (buffer-modcnt (fred-buffer (editData theField)))))

(defun fieldModified? (theField)
  (> (buffer-modcnt (fred-buffer (editData theField))) (savedModCount theField)))




;;; STORE SIDE OF LIFE
;;; _______________________________ 

;;;  NOTE:  the things in this list are guaranteed to be visible, on stage, and showing their active version -- they are NOT guaranteed
;;;        to be deeplyVisible, so that property should be checked (if it's important) when getting a field from this list.

(defvar *ScriptEditTexts-onStage-&-showingActive* nil)

;;;  Both maybe-add-to-*ScriptEditTexts-onStage-&-showingActive* and remove-from-*ScriptEditTexts-onStage-&-showingActive*
;;;  are called by prepareForStage, (setf scriptType), (setf visible), (setf versionDisplayed), (setf inputHandlerID)
;;;  
;;;  Also, maybe-add-to-*ScriptEditTexts-onStage-&-showingActive* is called by saveVersionDisplayed
;;;

(defun maybe-add-to-*ScriptEditTexts-onStage-&-showingActive* (me &optional justEnteringStage?)
  (let ((shouldBeInList? (and (gs:visible? (gs:node-flags me))
                              (gs:node-window me)
                              (displayingActiveVersion me)
                              (or justEnteringStage? (window-shown-p (gs:node-window me)))))
        (isInList? (memq me *ScriptEditTexts-onStage-&-showingActive*)))
    (if shouldBeInList?
      (unless isInList? (push me *ScriptEditTexts-onStage-&-showingActive*))
      (when isInList? (setq *ScriptEditTexts-onStage-&-showingActive*
                            (delete me *ScriptEditTexts-onStage-&-showingActive* :test #'eq))))))

(defun remove-from-*ScriptEditTexts-onStage-&-showingActive* (me) ; a ScriptEditText
  (setq *ScriptEditTexts-onStage-&-showingActive*
        (delete me *ScriptEditTexts-onStage-&-showingActive* :test #'eq)))

;;; Yes, someday we'll get fix it at the callers.

(defun purge-disposed-*ScriptEditTexts-onStage-&-showingActive* ()
  *ScriptEditTexts-onStage-&-showingActive*)

;;; _______________________________ 
;;;
;;; _______________________________ 

(defun setup-scriptEditor-handler-template (me) ; an EditorScriptEditText
  (destructuring-bind (&optional handlerName handlerObject handlerQualifier) (cdr (inputHandlerID me))
    (let* ((edFred (editData me))
           (buf (fred-buffer edFred)))
      (multiple-value-bind (decl-line end-line) 
                           (handlerIDstring handlerName handlerObject handlerQualifier :declaration t)
        (buffer-delete buf 0 t)
        (buffer-insert buf end-line 0)
        (buffer-insert buf #.(coerce '(#\Newline #\Tab #\Newline) 'string) 0)
        (buffer-insert buf decl-line 0)
        (set-mark buf (buffer-line-end buf 0 1))
        (setf (ccl::fr.hscroll (frec edFred)) 0)
        
        ;; Indent the lines and show the breakpoints.
        (refreshBreakPointInformation me)
        
        ;; Reset the undo history and modification history
        (setf (first (ccl::fred-history edFred)) nil)  ; *** is there a better way to do this???
        (setf (dirty me) nil)
        (forceRedraw me :updateScrollers t)))))

(defun setup-scriptEditor-handler-template-for-accessor (me) ; an EditorScriptEditText
  (destructuring-bind (&optional handlerName handlerObject handlerQualifier) (cdr (inputHandlerID me))
    (let* ((edFred (editData me))
           (buf (fred-buffer edFred)))
      (multiple-value-bind (decl-line end-line) 
                           (handlerIDstring handlerName handlerObject handlerQualifier :declaration t)
        (buffer-delete buf 0 t)
        (buffer-insert buf end-line 0)
        (buffer-insert buf #.(format nil "~%~c-- Do the actual property access:~%~cdo inherited~%" #\Tab #\Tab) 0)
        (buffer-insert buf decl-line 0)
        (set-mark buf (1+ (buffer-line-start buf 0 1)))
        (setf (ccl::fr.hscroll (frec edFred)) 0)
        
        ;; Build the breakpoints/indentation info
        (refreshBreakPointInformation me)
        
        ;; Reset the undo history and modification history
        (setf (first (ccl::fred-history edFred)) nil)  ; *** is there a better way to do this???
        (setf (dirty me) nil)
        ;; Indicate that the active version is displayed but that it's an unmodified accessor 
        ;; (so debugging options don't work)
        (setf (slot-value me 'displayingActiveVersion) T)
        (setf (slot-value me 'versionDisplayed) 'accessor)
        
        (forceRedraw me :updateScrollers t)
        
        ;; If we aren't already running, maybe enter running mode
        (unless (running me) (maybe-enter-script-running-mode me))))))

;;;  If the source was not found, returns NIL
;;;  If the source was found and inserted, returns the extra info as a keyword arglist
;;;

(defun setup-scriptEditor-handler-from-store (self &key for-new-handler-id) ; an EditorScriptEditText
  (let ((version (versionDisplayed self))) ; NOTE that if version is NIL, PS::getXXXinfo should return the latest
    (destructuring-bind (&optional proj handlerName handlerObject handlerQualifier) (inputHandlerID self)
      ;; Ask the store for the desired source
      (multiple-value-bind (version private locked script globals locals handlers
                                    breakpoints watchingState watchExpressions returnType
                                    indentationInfo editorInfo documentation)
                           (if handlerObject
                             (PS::getHandlerInfo handlerName handlerObject handlerQualifier :version version)
                             (PS::getFunctionInfo proj handlerName :version version))
        (declare (ignore handlers indentationInfo breakpoints)) ; since the latest info is either in the active lfun, or in indentationInfo if this isn't the active version
        (cond
         ;; Didn't get the source, but maybe there's an accessor method (in which case we "fake up" sources for it)
         ((and (not version) handlerObject)
          (let ((h (mf::find-local-handler handlerName handlerObject :qualifier handlerQualifier)))
            (when (and h (mf::accessor-method-p h))
              
              ;; Put the T into the versionDisplayed slot since accessors don't really have a version
              (setf (slot-value self 'versionDisplayed) T)
              
              (setup-scriptEditor-handler-template-for-accessor self)
              
              ;; If we're loading in a different handler than what we had, update the availableVersions property
              (when for-new-handler-id
                (setf (slot-value self 'availableVersions)
                      (if handlerObject
                        (PS::handlerVersions handlerName handlerObject handlerQualifier)
                        (PS::functionVersions proj handlerName))))
              
              ;; Finally, return the extra info (i.e. that which doesn't pertain to the ScriptEditText itself) as an arglist
              ;*** GOSH, MAYBE WE COULD COME UP WITH A FEW MEANINGFUL VALUES HERE (LIKE AT LEAST PRIVATE & LOCKED)
              (list :private nil :locked nil :watchingState nil :watchExpressions nil
                    :returnType nil :editorInfo nil :documentation nil))))
         
         ;; Got the source...
         (version
          (let* ((edFred (editData self))
                 (buf (fred-buffer edFred))
                 (lfun (!newgetLfun proj handlerName handlerObject handlerQualifier))
                 (isActiveVersion? (and lfun (eql (PS::lfunVersion lfun) version)))
                 ;;(warningsAlreadyIndicated? nil)
                 )
            
            ;; Put the actual version number into the versionDisplayed slot
            (setf (slot-value self 'versionDisplayed) version)
            
            ;; Set the locals and globals lists
            (setf (globalVariables self) globals ;; (mapcar #'car globals)
                  (localVariables self) locals) ;; (mapcar #'car locals))
            
            ;; Insert the text
            (buffer-delete buf 0 t)
            (buffer-insert buf script 0)
            
            ;; Update and reindent the entire handler, adding necessary warning indications as we go
            (indent-everything edFred)
            ;;(setq warningsAlreadyIndicated? t)
            
            ;; Reset the undo history
            (setf (first (ccl::fred-history edFred)) nil)  ; *** is there a better way to do this???
            
            ;; Unless we already indicated the warnings,
            ;; indicate them now (now that the indentation info is properly in place)
            ;;*** commented out for hernan to look at later (unless warningsAlreadyIndicated? (refresh-var-and-handler-warnings self :all-warnings isActiveVersion?))
            
            ;; Reset the modification flag
            (setf (dirty self) nil)
            
            (cond
             ;; This is the ACTIVE VERSION of the handler...
             (isActiveVersion?
              ;; Note that fact for future (fast) access
              (setf (slot-value self 'displayingActiveVersion) t)
              (maybe-add-to-*ScriptEditTexts-onStage-&-showingActive* self)
              ;; If we aren't already running, maybe enter running mode
              (unless (running self) (maybe-enter-script-running-mode self))
              )
             
             ;; This is NOT the active version...
             (t
              ;; Note that fact for future (fast) access
              (setf (slot-value self 'displayingActiveVersion) nil)
              (remove-from-*ScriptEditTexts-onStage-&-showingActive* self)
              ;; No longer displaying the running version, so switch out of running mode
              (when (running self) (switch-out-of-running-mode self))))
            
            ;; If we're loading in a different handler than what we had, update the availableVersions property
            (when for-new-handler-id
              (setf (slot-value self 'availableVersions)
                    (if handlerObject
                      (PS::handlerVersions handlerName handlerObject handlerQualifier)
                      (PS::functionVersions proj handlerName))))
            

            ;;; Clear all breakpoint info
            (if isActiveVersion?
              (setupBreakpoints self)
              (refreshBreakPointInformation self))

            (forceRedraw self :forceredraw t :updateScrollers t)
            
            ;; Finally, return the extra info (i.e. that which doesn't pertain to the ScriptEditText itself) as an arglist
            (list :private private :locked locked :watchingState watchingState :watchExpressions watchExpressions
                  :returnType returnType :editorInfo editorInfo :documentation documentation))))))))

(defun get-handler-watchExpressions (me) ; a ScriptEditText
  (let ((version (versionDisplayed me))) ; NOTE that if version is NIL, PS::getXXXinfo should return the latest
    (destructuring-bind (&optional proj handlerName handlerObject handlerQualifier) (inputHandlerID me)
      ;; Ask the store for the desired info
      (multiple-value-bind (version private locked script globals locals handlers
                                    breakpoints watchingState watchExpressions returnType
                                    indentationInfo editorInfo documentation)
                           (if handlerObject
                             (PS::getHandlerInfo handlerName handlerObject handlerQualifier :version version)
                             (PS::getFunctionInfo proj handlerName :version version))
        (declare (ignore version private locked script globals locals handlers
                         breakpoints watchingState returnType indentationInfo editorInfo documentation))
        watchExpressions))))

(defun save-handler-watchexpressions (me watchExpressions)
  (let ((version (versionDisplayed me)))
    (destructuring-bind (&optional proj handlerName handlerObject handlerQualifier) (inputHandlerID me)
      (if handlerObject
        (PS::saveHandlerVersion proj handlerName handlerObject handlerQualifier
                                ; private locked script translation wrappedLfun
                                ; globals locals handlers breakpoints watchingState
                                :preserve :preserve :preserve :preserve :preserve
                                :preserve :preserve :preserve :preserve :preserve
                                watchExpressions
                                ; returnType indentationInfo editorInfo documentation active
                                :preserve :preserve :preserve :preserve :preserve
                                version)
        
        (PS::saveFunctionVersion proj handlerName
                                 ; private locked script translation wrappedLfun
                                 ; globals locals handlers breakpoints watchingState
                                 :preserve :preserve :preserve :preserve :preserve
                                 :preserve :preserve :preserve :preserve :preserve
                                 watchExpressions
                                 ; returnType indentationInfo editorInfo documentation active
                                 :preserve :preserve :preserve :preserve :preserve
                                 version)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-handler handlerVersionSaved (EditorScriptEditText version &key active)
  (declare (ignore version active)))

(define-handler saveVersionDisplayed (EditorScriptEditText &key translation
                                                               (private :preserve)
                                                               (locked :preserve)
                                                               (watchingState nil)
                                                               (watchExpressions :preserve)
                                                               (returnType :preserve)
                                                               (documentation :preserve)
                                                               (editorInfo :preserve))
  (when (versionDisplayed me)
    ;; Update any dirty lines in the field before we proceed
    (update me)
    ;; If an accessor handler's "fake" source is being shown, don't do anything
    (unless (eq 'accessor (versionDisplayed me))
      (destructuring-bind (&optional proj handlerName handlerObject handlerQualifier) (inputHandlerID me)
        (let* ((version (versionDisplayed me))
               (edFred (editData me))
               (script (buffer-substring (fred-buffer edFred) 0 t))
               (globals (globalVariables me)) (locals (localVariables me)) 
               (handlers nil);;;these three are computed at compile time...
               (breakpoints (copy-list (linesWithBreakpoints me)))
               (lfun (!newgetLfun proj handlerName handlerObject handlerQualifier))
               (active (when (and lfun translation) t)))
          ;; If the watchingState wasn't supplied, try to get its value from the currently active version
          (unless watchingState
            (if active
              (setq watchingState nil)
              (setq watchingState :preserve)))
          ;; deal with possibility that the displayed version was never set
          (unless version   ;; if the displayed version was never set, we have to before saving it
            (setq version (get-universal-time))
            ;; Set the actual slot value
            (setf (slot-value me 'versionDisplayed) version)
            ;; Note that fact that this is NOT the active version
            (setf (slot-value me 'displayingActiveVersion) nil
                  active nil)
            (remove-from-*ScriptEditTexts-onStage-&-showingActive* me)
            ;; If it was in running mode before, it's certainly not now so switch out of running mode
            (when (running me) (switch-out-of-running-mode me)))
          ;; Save the handler!
          (if handlerObject
            (PS::saveHandlerVersion proj handlerName handlerObject handlerQualifier
                                    private locked script translation lfun
                                    globals locals handlers breakpoints watchingState
                                    watchExpressions returnType nil editorInfo
                                    documentation active version)
            (PS::saveFunctionVersion proj handlerName
                                     private locked script translation lfun
                                     globals locals handlers breakpoints watchingState
                                     watchExpressions returnType nil editorInfo
                                     documentation active version))
          
          ;; Reset the field's undo history and modification flag
          (setf (first (ccl::fred-history edFred)) nil)  ; *** is there a better way to do this???
          (setf (dirty me) nil)
          
          ;; If this version has just been made active, note that fact for future (fast) access
          (when active
            (setf (slot-value me 'displayingActiveVersion) t)
            (maybe-add-to-*ScriptEditTexts-onStage-&-showingActive* me))
          
          ;; Notify the field that the version's been saved (and possibly activated)
          (handlerVersionSaved me version :active active))))))

;;;  returns T if the version was successfully activated, NIL otherwise
;;;

(define-handler activateVersionDisplayed (EditorScriptEditText &key
                                                                      (private :preserve)
                                                                      (locked :preserve)
                                                                      (watchingState :preserve)
                                                                      (watchExpressions :preserve)
                                                                      (returnType :preserve)
                                                                      (documentation :preserve)
                                                                      (editorInfo :preserve))
  (declare (ignore watchingState))
  ;; Update any dirty lines and reindent the whole field before we proceed
  (update me :indentation t)
  ;; And make sure the variable lists accurately reflect the current enviornment
  ;; (maybe-refresh-scriptEditor-varlists me)
  ;; If an accessor handler's "fake" source is being shown, don't do anything
  (unless (eq 'accessor (versionDisplayed me))
    (destructuring-bind (&optional proj handlerName handlerObject handlerQualifier) (inputHandlerID me)
      (let* ((edFred (editData me))
             (edFredStream (slot-value edFred 'buf-stream))
             (endpoint-buf-marks (CCL::BUFFER-STREAM-STATE edFredStream))
             failure? errors locals globals)
        (declare (ignore-if-unused handlersInfo trueGlobalsInfo trueLocalsInfo freeVars)) ;*** RIGHT?
        (unless nil ; malformedHandler?
          ;; Make sure the script corresponds to the handler we're expecting!
          (set-mark (first endpoint-buf-marks) 0)
          (set-mark (second endpoint-buf-marks) t)
          (sk8-multival-bind (newHandlerName newHandlerObject) (handlerIDFromScript proj (text me))
            (let ((errorMessage
                     (cond
                      ((neq newHandlerName handlerName)           "the handler name is different")
                      ((eq newHandlerObject 'sk8::me)             nil)  ;;unnamed object so can't be different..
                      ((and newHandlerObject (not handlerObject)) "itÕs a handler but should be a function")
                      ((and (not newHandlerObject) handlerObject) "itÕs a function but should be a handler")
                      ((neq newHandlerObject handlerObject)       "the object is different"))))
                (when errorMessage
                  (setq errors (list (make-scriptError 
                                      :lineNumber 1
                                      :errorMessage 
                                      (concatenate 'string
                                                   "Handler definition doesnÕt match this editorÕs intended handler; "
                                                   errorMessage)))))))
          ;; Attempt to translate the handler definition
          (unless errors
            (set-mark (first endpoint-buf-marks) 0)
            (set-mark (second endpoint-buf-marks) t)
            (SK8-multival-setf (failure? errors locals globals)
                               (compileScriptHandler proj (text me) handlerObject))))
        
        (restore-scriptEditText-fontAttributes me 0 t :doesnt-shrink-lineheight t :update nil)
        
        (cond
         ;; Syntax error. Just one. Indicate it directly. 
         (failure? (ed-beep)
                   (indicateScriptSyntaxError me errors :record t))
         ;; Successful translation
         (t
          ;; Are there any warnings? Report them!
          (when errors
            (indicateScriptCompileWarnings me errors))
          ;; Having compiled, refresh the locals and globals. 
          (setf (globalVariables me) globals
                (localVariables me) locals)

          ;; Clear all the breakpoints  because we are starting a new version
          (clearBreakpoints me)

          ;; Store the version
          (saveVersionDisplayed me
                                :translation t ;; Does not seem to be a need to save this.
                                :private private
                                :locked locked
                                :watchingState 0
                                :watchExpressions watchExpressions
                                :returnType returnType
                                :documentation documentation
                                :editorInfo editorInfo)
          ;; Let the editor know everything is in synch.
          (setf (dirty me) nil)
          (forceredraw me :forceredraw t)
          t))))))


(define-handler deleteVersionDisplayed (EditorScriptEditText &key (warning t))
  (let ((active? (displayingActiveVersion me)))
    (when (or (null warning)
              (SK8::YesOrNoDialog
               (if active?
                 "This is the active version of the handler; are you sure you want to delete it?"
                 "Are you sure you want to delete this version of the handler?")
               :width 330 :yesText "Delete"))
      (destructuring-bind (&optional proj handlerName handlerObject handlerQualifier) (inputHandlerID me)
        (let* ((version (versionDisplayed me))
               (versions (availableVersions me))
               ;;(pos (or (position version versions :test #'eql) 0))
               )
          ;; *** Kinda nonsensical, but the Macframes kernel doesn't yet deal too well with symbols in the SETF package.
          (when (and (symbolp handlerName) (!setHandlerName? handlerName))
            (setq handlerName (list 'SETF (let ((*readtable* CCL::%initial-readtable%))
                                            (read-from-string (symbol-name handlerName))))))
          (when active?
            (if handlerObject
              (removeHandler (inputhandler me))
              (removeFunction (inputhandler me))))
          (unless (eq 'accessor version)
            (if handlerObject
              (PS::purgeHandler handlerName handlerObject handlerQualifier :version version)
              (PS::purgeFunction proj handlerName :version version)))
          (setf versions (delete version versions :test #'eql))
          (setf (slot-value me 'availableVersions) versions)
          (setvalue 'versionDisplayed me nil)
          (if (> (length versions) 0)
            (set-versionDisplayed* me (first (last versions)))
            (set-versionDisplayed* me :new :for-new-handler-id (inputhandlerid me)))
          )))))

(define-handler deleteEarlierVersions (EditorScriptEditText &key (warning t))
  (let* ((versionDisplayed (versionDisplayed me))
         (versions (availableVersions me))
         (activeVersion (activeVersion me))
         (includesActive? (< activeVersion versionDisplayed)))
    ;*** show this in the warning message?
    ;(string-left-trim CL-USER::*whitespace-charbag* (CL-USER::get-time-string :time versionDisplayed))
    (when (or (null warning)
              (SK8::YesOrNoDialog
               (if includesActive?
                 "Are you sure you want to delete all earlier versions of the handler, including the active version?"
                 "Are you sure you want to delete all earlier versions of the handler?")
               :width 360 :yesText "Delete"))
      (destructuring-bind (&optional proj handlerName handlerObject handlerQualifier) (inputHandlerID me)
        ;; *** Kinda nonsensical, but the Macframes kernel doesn't yet deal too well with symbols in the SETF package.
        (when (and (symbolp handlerName) (!setHandlerName? handlerName))
          (setq handlerName (list 'SETF (let ((*readtable* CCL::%initial-readtable%))
                                          (read-from-string (symbol-name handlerName))))))
        (when includesActive?
          (if handlerObject
              (removeHandler (inputhandler me))
              (removeFunction (inputhandler me))))
        (dolist (v versions)
          (when (< v versionDisplayed)
            (if handlerObject
              (PS::purgeHandler handlerName handlerObject handlerQualifier :version v)
              (PS::purgeFunction proj handlerName :version v))))
        (setf (slot-value me 'availableVersions) (delete versionDisplayed versions :test #'>))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-handler activeVersion (EditorScriptEditText &key error)
  (let* ((lfun (apply #'!newgetLfun (inputHandlerID me)))
         (activeVersion (when lfun (PS::lfunVersion lfun))))
    (unless activeVersion
      (when error (error "Handler hasn't yet been activated!")))
    activeVersion))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun set-versionDisplayed* (self version &key for-new-handler-id)
  (let ((oldVersion (versionDisplayed self))
        (handlerID (inputHandlerID self)))
    
    ;;If there is no handlerID then we are done
    (when (and (null handlerID) (null for-new-handler-id))
      (return-from set-versionDisplayed* nil))
    
    ;;if the version hasn't changed then we're done.
    (when (and (not (null version))
               (eql version oldVersion))
      (return-from set-versionDisplayed* t))
    
    
    ;; Notify the field that we're leaving the version displayed
    (leavingVersionDisplayed self)
    (if (and (or (fieldModified? self) (versionmodified self))
             (not (displayingactiveversion self)))
      (saveVersionDisplayed self))
    
    
    ;;If changing IDs now we can update it safely...
    (if for-new-handler-id
      (setf (slot-value self 'inputhandlerid) for-new-handler-id))
    
    ;; If the active version is requested, get the active version id (be leniant if it's for a new handler id)
    (when (null version)
      (setq version (or (activeVersion self :error (not for-new-handler-id))
                        :latest)))
    
    (if (eq version :new)
      ;; START A NEW VERSION...
      (progn
        (setq version (get-universal-time))
        ;; Set the actual slot value
        (setf (slot-value self 'versionDisplayed) version)
        ;; Note that fact that this is NOT the active version
        (setf (slot-value self 'displayingActiveVersion) nil)
        (remove-from-*ScriptEditTexts-onStage-&-showingActive* self)
        ;; If it was in running mode before, it's certainly not now so switch out of running mode
        (when (running self) (switch-out-of-running-mode self))
        (cond
         ;; This is for a new handler id; set up the template text and notify field that it's a new handler
         (for-new-handler-id
          (setup-scriptEditor-handler-template self)
          (setf (slot-value self 'availableVersions) (list version)) ; initialize the availableVersions slot
          (prepareForNewHandler self handlerID :justCreated t))
         ;; It's just a new version; leave whatever text we've got and notify field that it's a new version
         (t
          (push version (slot-value self 'availableVersions)) ; maintain the availableVersions slot
          (saveVersionDisplayed self)
          (prepareForNewHandlerVersion self version :oldVersion oldVersion :justCreated t)))
        t)
      
      ;; TRY TO GET AN EXISTING VERSION...
      (progn
        (when (eq version :latest) (setq version nil)) ;#####*** FIGURE OUT HOW TO GET THE RIGHT VERSION!
        
        ;; Set the actual slot value, and try to get existing source out of the store
        (setf (slot-value self 'versionDisplayed) version)
        (let ((extraInfoArgs (setup-scriptEditor-handler-from-store self :for-new-handler-id (if for-new-handler-id t nil))))
          ;; If we got it, notify field that it's a new handler or a new version, and return T
          (when extraInfoArgs
            (forceRedraw self)
            (if for-new-handler-id
              (apply #'prepareForNewHandler self for-new-handler-id extraInfoArgs)
              (apply #'prepareForNewHandlerVersion self (versionDisplayed self) :oldVersion oldVersion extraInfoArgs))
            (refreshBreakpointInformation self)
            (if (displayingactiveversion self) (setupBreakpoints self))
            (setf (versionmodified self) nil)
            t))))
    ))

;;; _______________________________ 
;;;
;;; _______________________________ 

(define-handler leavingVersionDisplayed (EditorScriptEditText)
  )

(define-handler prepareForNewHandler (EditorScriptEditText handlerID &key
                                                               justCreated private locked
                                                               watchingState watchExpressions
                                                               returnType documentation editorInfo)
  (declare (ignore justCreated handlerID private locked watchingState watchExpressions
                   returnType documentation editorInfo))
  )


;;; If justCreated is true, the rest of the keyword args are meaningless

(define-handler prepareForNewHandlerVersion (EditorScriptEditText version &key
                                                                       oldVersion justCreated private locked
                                                                       watchingState watchExpressions
                                                                       returnType documentation editorInfo)
  (declare (ignore oldVersion version justCreated private locked watchingState
                   watchExpressions returnType documentation editorInfo))
  ;;  save the current version before continuing
  (saveVersionDisplayed me)
  )

;;; _______________________________ 
;;;
;;; _______________________________ 

;;; Returns FALSE if the active version isn't up!

(define-handler inputHandler (EditorScriptEditText)
  (destructuring-bind (&optional handlerName handlerObject handlerQualifier) (cdr (inputHandlerID me))
    (if handlerObject
      (mf::find-local-handler handlerName handlerObject :qualifier handlerQualifier)
      (fboundp handlerName))))

(define-handler (setf inputHandler) (handlerObj EditorScriptEditText &key (error t))
  (when (symbolp handlerObj) (setq handlerObj (symbol-function handlerObj)))
  (cond
   ((functionp handlerObj)
    (when (typep handlerObj 'standard-generic-function)
      (SK8-error TypeMismatchError :object handlerObj :expectedType Handler)))
   ((typep handlerObj 'standard-method)
    (setq handlerObj (method-function handlerObj)))
   (t
    (SK8-error TypeMismatchError :object handlerObj :expectedType Handler)))
  (multiple-value-bind (hProj hName hObject hQualifier)
                       (!minimalExactLfunInfo handlerObj)
    (if (when (and hProj hName)
          (sk8::setValue 'versionDisplayed me nil) ; request the active version
          (setf (inputHandlerID me :creation nil) (list* hProj hName (when hObject (list hObject hQualifier)))))
      (inputHandler me)
      (when error (error "CanÕt find sources for the given handler object")))))

(define-handler (setf inputHandlerID) (handlerID EditorScriptEditText &key (creation t))
  (cond
   ((equal handlerID (inputHandlerID me)) handlerID)
   (t
    ;; Make sure we've got a valid handlerID list
    (destructuring-bind (&optional proj handlerName handlerObject handlerQualifier) handlerID
      (declare (ignore handlerObject))
      (unless (is-a proj Project) (SK8-error TypeMismatchError :object proj :expectedType Project))
      (unless (symbolp handlerName) (SK8-error TypeMismatchError :object handlerName :expectedType Symbol))
      (unless (memq handlerQualifier '(nil :before :after))
        (SK8-error TypeMismatchError :object handlerQualifier :expectedType '(nil :before :after))))
    (cond
     ((set-versionDisplayed* me nil :for-new-handler-id handlerID)
      handlerID)
     (creation
      (set-versionDisplayed* me :new :for-new-handler-id handlerID)
      handlerID)
     (t
      nil)))))

(define-handler (setf versionDisplayed) (version EditorScriptEditText)
  (let ((handlerID (inputHandlerID me))
        (oldVersion (versionDisplayed me)))
    (when handlerID
      (unless (set-versionDisplayed* me version)
        (setf (slot-value me 'versionDisplayed) oldVersion)
        (error "There is no version ~s of handler ~a in project ~a" version
               (handlerIDstring (second handlerID) (third handlerID) (fourth handlerID))
               (objectString (first handlerID)))))
    (forceredraw me :forceredraw t)
    (versionDisplayed me)))


(define-handler (setf displayingActiveVersion) (boolean EditorScriptEditText)
  (when boolean
    (unless (displayingActiveVersion me) (setf (versionDisplayed me) nil))
    t))


#|
	Change History (most recent last):
	1	11/8/93	chip	new file
	6	11/10/93	chip	got activateVersionDisplayed & saveVersionDisplayed working acceptably
	7	11/12/93	chip	now automatically saves dirtied version when switching to another version
	8	11/15/93	chip	now setup-scriptEditor-handler-template leaves the field NOT dirty when it's done
	10	11/15/93	chip	forced the display to update when a new version is loaded from the store
	11	11/17/93	chip	fixed editor-breakptInfo->internal-conditionExprs to work with editor's latest breakpoint spec
	13	11/22/93	chip	took out checks for wood weirdness
	14	11/23/93	chip	activateVersionDisplayed now forces the variable lists to update if necessary
	15	11/29/93	chip	made refernces to inputHandlerID property safer
	16	11/30/93	chip	added the inputHandler virtual property
	17	11/30/93	chip	(setf inputHandler) now responds consistently for all cases in which the sources aren't found;  added :creation option (defaults to t) for (setf inputHandlerID);  the two "setup" functions now force an update of the field when done
	19	12/10/93	chip	updated setup-scriptEditor-handler-template for new handlerIDstring API
	20	12/13/93	chip	setup-scriptEditor-handler-template no longer calls (setf versionDisplayed), since that's who calls setup-scriptEditor-handler-template
	21	12/13/93	chip	setup-scriptEditor-handler-template now resets the hscroll of the field
	22	1/11/94	hernan	self -> me
	23	1/14/94	hernan	Labeling properties and handlers private.
	24	2/16/94	chip	updated activateVersionDisplayed for new API to translateScriptHandler;  fixed saveVersionDisplayed to deal with handlersInfo arg
	25	2/25/94	chip	updated setup-scriptEditor-handler-template for new handlerIDstring API
	26	2/28/94	chip	fixed how activateVersionDisplayed transfers editor's breakpoint info to the handler's
	27	3/2/94	Hernan	Porting to Fred 3.0
	28	3/3/94	chip	added setup-scriptEditor-handler-template-for-accessor; made the field set its 'versionDisplayed' to 'accessor' when it's showing "fake" accessor source
	29	3/4/94	chip	fixed typo in activateVersionDisplayed; added deleteVersionDisplayed & deleteEarlierVersions
	31	3/4/94	chip	minor changes in breakPointVector munging
	34	3/25/94	chip	maybe-add-to-*ScriptEditTexts-onStage-&-showingActive* now can work for fields just entering the Stage
	35	3/31/94	chip	saveVersionDisplayed now saves the correct handlersInfo
	36	4/18/94	chip	updated for new scriptErrorPosition api
	37	4/21/94	chip	saveVersionDisplayed now computes & saves the complete local & global var info; consequently, setup-scriptEditor-handler-from-store extracts simple var lists from the alists when it loads the handler; also, now correctly restores var & handler warnings when loading/activating
	38	4/25/94	chip	editor-breakptInfo->internal-breakptInfo/conditionExprs now only operate on items that are valid in the given breakptVector
	39	5/27/94	chip	activateVersionDisplayed now passes the handlerObject along to translateScriptHandler (to support defining handlers on unnamed objects!)
	40	6/3/94	rod	(setf inputHandlerID) now properly clears the versionDisplayed slot before changing handlers
	41	6/6/94	chip	setup-scriptEditor-handler-from-store now correctly updates the scrollers after having inserted the handler text
	42	6/7/94	chip	fixed call to editor-breakptInfo->internal-conditionExprs to use locals list (NOT locals a-list), in activateVersionDisplayed
	43	6/10/94	chip	added leavingVersionDisplayed & save-handler-watchExpressions
	44	6/18/94	chip	activateVersionDisplayed now calls translateScriptHandler with 'warnings' inhibited (since the editor shows warnings in itself)
	45	7/11/94	chip	activateVersionDisplayed now ensures the script matches the expected handlerID (radar #1173174)
	46	7/12/94	chip	activateVersionDisplayed now passes the handlerObject to handlerID-from-script -- radar #1173962
	47 	 9/ 6/94	chip    	removed uses of obsolete SS compiler policy
	48 	 9/22/94	sidney  	part of the big cleanup/speedup of saving/loading to/from the store
	49 	 9/26/94	chip    	!bitset/!bitclear --> bitset/bitclear
	50 	12/26/94	sidney  	1207083: delete current version of handler fencepost error when version is the oldest
	51 	 1/27/95	till    	bug1211915, fix set-versionDisplayed* (who named this thing?) 
							to copy the lineInfo vector.
	52 	 4/ 3/95	sidney  	set a version number before saving a modified handler script edit buffer
	53 	 4/ 5/95	Hernan  	changed setup-scriptEditor-handler-from-store to rebuild
							the indentationInfo structure when (1) it is not there or
							(2) its number of lines are not right.
	1  	 1/ 9/96	Hernan  	New file. Implements most of the functionality of the old
						ScriptEditText. This functionality is only required for the 
						editor. Thus we created EditorScriptEditText with the full
						functionality. ScriptEditText is much simpler now.
	2  	 1/10/96	Hernan  	Rolling back handler indentification slots to what they were
						in the original ScriptEditText. This will simplify integration
						with the current SK8.
	3  	 1/10/96	Hernan  	Adding running and other handlers from the old ScriptEditText.
	4  	 1/15/96	Hernan  	Folded in missing functions from the old ScriptEditText.
	5  	 1/17/96	Hernan  	Removing duplicate code. Also fleshing out the function
						that shows compiler warnings.
	6  	 1/19/96	Hernan  	Adding breakpoint functionality.
	2  	 1/25/96	Hernan  	returnInField does not check for a complete script.
	3  	 1/25/96	Hernan  	Making the field activate on mouseDown if it is not active.
	4  	 1/25/96	Hernan  	enterInField does not evaluate the field.
	5  	 1/29/96	Hernan  	Fixed the breakpoint update code to work with the new
						lowLevelInsert and lowLevelDelete handlers.
	6  	 2/ 7/96	Hernan  	In keydown, now calling the right thing to signal modification
						of the field.
	7  	 2/ 8/96	sidney  	translateScriptCommandOrExpr subsumes older separate functions
	8  	 2/ 9/96	Hernan  	Should not try to set a breakpoint when there is no function
						available.
	9  	 2/12/96	Hernan  	In mousedown, set the keytarget of the window to the
						field (if it is not already the key target).
	10 	 2/12/96	Hernan  	Made the breakpoint and indentation info update itself
						when we click out of a line in the field.
	2  	12/15/95	sidney  	remove breakpoint related code
	3  	 1/10/96	Hernan  	Removing all references to the lineinfo data structure.
						Preparing this file to load with the new editText.
	4  	 1/15/96	Hernan  	Removing more references to the line-info vector.
	5  	 1/17/96	Hernan  	Splicing in the new compiler API. Activate-version-displayed
						uses the new code.
	6  	 1/19/96	sidney  	removing/rewriting references to old sk8script compiler
	7  	 1/19/96	Hernan  	Saving the breakpoints data structure to the store.
	8  	 1/19/96	Hernan  	After successful compilation, activateVersionDisplayed
						activates all the breakpoints that are set on the handler.
	2  	 1/25/96	Hernan  	activateVersionDisplayed needs to call the new function
						handlerIdFromScript instead of one that does not exist anymore.
	3  	 1/25/96	Hernan  	Showing warnings.
	4  	 2/ 7/96	Hernan  	Using !newGetLfun instead of !getLfun.
	4  	 2/ 7/96	sidney  	comment out call to refresh-var-and-handler-warnings (obsolete function?)
	7  	 2/ 9/96	sidney  	!newGetLfun is in a different package
	2  	 4/10/96	Hernan  	define-system-handler->define-handler.
	3  	 5/ 2/96	Hernan  	Touching the file so that it recompiles.
	4  	 5/13/96	Brian   	Fixed handlerIdFromScript
	5  	 6/11/96	Hernan  	Needs to recompile for changes to with-fred-drawing.
	6  	 7/29/96	Hernan  	in setup-editor-from-store, globals and locals are lists of symbols only.
	12 	10/10/96	Brian   	Various breakpoint updating and version saving
						problems fixed.  Still waiting on dealing with undo
						and versions. ***
	13 	10/11/96	Brian   	adding warnings.
	14 	10/11/96	Brian   	updating breakpoints when active version is
						displayed by set-handlerversion*
	15 	10/11/96	Brian   	
	16 	10/11/96	Brian   	No breakpoints on lines that begin with "on
	17 	10/11/96	Hernan  	Changing breakpoint hit testing functions to stop using
						*event-x* and *event-y* variables which are not updated
						correctly by the new threaded event system. Wrote the
						code using eventh() and eventv() which are better behaved.
	18 	10/11/96	Brian   	Updating to new version from active version
						if a low level event happens.
	19 	10/11/96	Brian   	no breakpoints at globals and locals.
	20 	10/14/96	Brian   	Fixing up versioning.
	21 	10/14/96	Brian   	
	22 	10/14/96	Brian   	
	23 	10/14/96	Brian   	
	24 	11/12/96	Brian   	Added robustness check for switch out of 
						running mode.
	25 	12/17/96	Hernan  	Fixing set-versionDisplayed*. Silly typo.
	26 	 2/27/97	Hernan  	
|# ;(do not edit past this line!!)
