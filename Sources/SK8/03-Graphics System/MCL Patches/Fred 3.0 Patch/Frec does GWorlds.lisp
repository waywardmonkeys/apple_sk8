;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories


;;;

(in-package :ccl)

;;; This file is a patch to frec. It contains all the function that we patch
;;; in frec in order to let frec image into gWorlds. The changes do the following:
;;; (1) add an update region to frec.
;;; (2) use the update region when frec is not imaging into a window. For this
;;;     we replace traps like #_invalRgn by macros that do the right thing 
;;;     depending on what frec is imaging into.
;;; (3) Needed to change the args of %scroll-screen-rect. Now takes the frec arg also.

;;; Someday this patch could be folded into MCL...
;;; Every place that is modified in the original function is labeled HERNAN.
;;; Everything in this file is from l1-edfrec.lisp unless stated otherwise.

;;; _______________________________ 
;;; Adding the updateRgn. FREDENV.lisp
;;; _______________________________ 

(let ((ccl::*warn-if-redefine-kernel* nil))
  (makunbound '$fred-record-length))

(ccl::def-accessors (fred-record) ccl::%svref
  nil                                   ; 'fred-record
  (fr.buffer fr.cursor)                 ; The buffer displayed in this frec
  fr.curascent                          ; ascent of cursor line
  fr.curdescent                         ; descent of cursor line
  ; no fr.position. Assumed to be #@(0 0). View system used for other locs.
  fr.size                               ; Our size (point)
  fr.wposm                              ; Desired window position (mark)
  fr.selmarks                           ; Desired selection rgns, list of (b . z) marks
  fr.lead                               ; Additional distance between lines (pixels)
  fr.tabcount                           ; Number of spaces per tab (max 128)
  fr.hscroll                            ; Desired amount of horizontal scroll (pixels)
  fr.margin                             ; Size of left margin (and right, if wrapping)
  fr.plist                              ; plist, for the user
  fr.bticks                             ; Tick count at last blink
  fr.cticks                             ; Tick count at last click
  fr.cposn                              ; Position at last click
  fr.bpoint                             ; Screen position of blinking char
  fr.bpos                               ; Buffer position of blinking char
  fr.curpoint                           ; Screen position of cursor at last display
  fr.curcpos                            ; Buffer position of cursor at last display
  fr.leading                            ; nil, t, float, or fixnum
  fr.hpos                               ; margin-hscroll at last display
  fr.linevec                            ; vector of screen line lengths at last display
  fr.numlines                           ; number of lines in linevec
  fr.bwin                               ; pos of first char in window at last display
  fr.zwin                               ; pos after last char in window at last display
  ; .. actually distance from last char to end of buf
  ; .. meaningful if bmod and zmod are before it
  ; .. what if bmod before and zmod after? meaningless  
  fr.bmod                               ; unchanged top area since last display
  fr.zmod                               ; unchanged bottom area since last display
  fr.selrgn                             ; selection at last display (mac region)
  fr.selposns                           ; positions of selections at last redisplay
  fr.flags-slot                         ; assorted flags
  fr.bpchar                             ; blinking char
  fr.bp-ff                              ; blinking char font&face
  fr.bp-ms                              ; blinking char mode&size
  fr.vpos                               ; Vertical position at fr.zwin
  fr.owner                              ; A view
  fr.lineascents                        ; vector of line ascents at last display
  fr.linedescents                       ; vector of line descents at last display
  fr.lineheights                        ; vector of (+ ascent descent leading)
  fr.linewidths                         ; vector of line widths
  fr.keyscript                          ; keyboard script last time we looked
  fr.truezwin				; pos after last char in window at last display
  ; meaningful if bmod and zmod are after it
  ; if bmod before and zmod after also meaningless
  ; if both zwins meaningless - dont do some case or other
  ;; HERNAN -> Adding the update region.
  fr.updateRgn                          ; the update region for this frec
  $fred-record-length
  )

;;; Note: this was the pre-MCL3.1 version of the function
#+ignore
(defmacro cons-fred-record ()
  `(let ((frec (ccl::%make-uvector $fred-record-length ccl::$v_istruct)))
     (setf (ccl::%svref frec 0) 'fred-record)
     frec))

(defmacro cons-fred-record ()
  `(%istruct 'fred-record ,@(make-list (1- $fred-record-length) :initial-element nil)))


;;; _______________________________ 
;;; Allowing Fred to go into gWorlds.
;;; _______________________________ 

;;; These functions allow the system to work in the old way and with the new
;;; update regions.

;;; Adds the region to the update region of frec.

(defun %frec-invalRgn (frec rgn)
  (if (handlep (fr.updateRgn frec))
    (#_unionRgn rgn (fr.updateRgn frec) (fr.updateRgn frec))
    (#_invalRgn rgn)))

;;; Reomves the region from the update region of the frec.

(defun %frec-validRgn (frec rgn)
  (if (handlep (fr.updateRgn frec))
    (#_diffRgn (fr.updateRgn frec) rgn (fr.updateRgn frec))
    (#_validRgn rgn)))

;;; Returns the region this frec uses as its update.

(defun %frec-updateRgn (frec)
  (if (handlep (fr.updateRgn frec))
    (fr.updateRgn frec)
    (ccl::pref (ccl::%getport) :windowRecord.updateRgn)))

;;; _______________________________ 
;;; Making frec with the update rgn.
;;; _______________________________ 

(defun make-frec (cursor owner &optional (size (view-size owner)))
  (let* ((curpos (buffer-position cursor))
         (frec (cons-fred-record)))
    (setf (fr.cursor frec) cursor
          (fr.owner frec) owner
          (fr.size frec) size
          (fr.wposm frec) (make-mark cursor 0 t)
          (fr.selmarks frec) (list (cons (make-mark cursor curpos)
                                         (make-mark cursor curpos t)))
          (fr.lead frec) 0
          (fr.flags frec) 0
          (fr.tabcount frec) (buffer-tabcount cursor)
          (fr.wrap-p frec)(buffer-wrap-p cursor)
          (fr.word-wrap-p frec)(buffer-word-wrap-p cursor)
          ;(fr.justification frec)(buffer-justification cursor)
          ;(fr.line-right-p frec)(buffer-line-right-p cursor)  ;???
          (fr.hscroll frec) 0
          (fr.margin frec) 3
          (fr.plist frec) nil
          (fr.bticks frec) (#_TickCount)
          (fr.cticks frec) -1
          (fr.cposn frec) -1
          (fr.bpoint frec) -1
          (fr.bpos frec) nil
          (fr.curpoint frec) -1
          (fr.curcpos frec) 0
          (fr.hpos frec) 3  ; should be same as margin
          (fr.linevec frec) nil
          (fr.numlines frec) 0
          (fr.bwin frec) 0
          (fr.zwin frec) 0
          (fr.truezwin frec) 0
          (fr.bmod frec) 0
          (fr.zmod frec) 0
          (fr.selrgn frec) (%null-ptr)
          (fr.selposns frec) (list (cons curpos curpos))
          
          (fr.bpchar frec) nil
          (fr.bp-ff frec) 0
          (fr.bp-ms frec) 0
          (fr.vpos frec) 0
          ;; HERNAN -> creating the update region.
          (fr.updateRgn frec) nil)
    (setf (fr.caret-on-p frec) nil)
    (%set-frec-justification frec (buffer-justification cursor))
    (setf (fr.numlines frec) 0)
    (let ((line-count (estimate-line-count frec)))
      (declare (fixnum line-count))
      (setf (fr.linevec frec) (make-array line-count :initial-element 0))
      (setf (fr.lineascents frec) (make-array line-count :initial-element 0))
      (setf (fr.linedescents frec) (make-array line-count :initial-element 0))
      (setf (fr.lineheights frec) (make-array line-count :initial-element 0))
      (setf (fr.linewidths frec) (make-array line-count :initial-element 0)))
    (when (setf (fr.line-right-p frec) (not (eql 0 (ccl::get-sys-just))))
      (setf (fr.right-justified-p frec) t))
    (setf (fr.leading frec) nil)       ; no leading
    (if (and owner (wptr owner))
      (reinit-frec frec owner))     ; Push onto *frec-list*, allocate selrgn
    (ccl::use-buffer cursor)                 ; increment buffer reference count
    frec))

;;; _______________________________ 
;;; Using the update rgn...
;;; _______________________________ 

(defun %frec-update-internal (frec no-drawing)  
  (progn ; with-frec (frec frec)
    (handler-bind ((error 
                    #'(lambda (c) 
                        (push c *error-log*)
                        (validate-view (fr.owner frec))  ; good luck
                        (return-from %frec-update-internal nil))))
      (let ((bmod (fr.bmod frec))
            (blinkers-on (fr.caret-on-p frec))
            h-scrolled)
        (declare (fixnum bmod))
        (declare (ignore-if-unused h-scrolled))
        (unless (eql #xffffff bmod)
          ; set buf-changed-p bit if the buffer was modified
          (setf (fr.buf-changed-p frec) t)
          (let ((curcpos (fr.curcpos frec)))
            (declare (fixnum curcpos))
            (when (>= curcpos bmod)
              ; set curs-changed-p bit if the mod was before the cursor
              (setf (fr.curs-changed-p frec) t)
              (when (and (eql curcpos bmod) ccl::*foreground*)
                (let* ((owner (fr.owner frec))
                       (window (and owner (view-window owner))))
                  (when (or (null window) (window-active-p window))
                    ; hide cursor if change was at cursor
                    (#_ObscureCursor)))))))
        (unwind-protect
          (progn
            (setf (fr.nodrawing-p frec) no-drawing)
            (%frec-turn-off-blinkers frec)
            ;Set sel-valid-bit if shape unchanged and all positions were before bmod. 
            (when (eq (fr.framed-sel-p frec) (fr.frame-sel-p frec))
              (dolist (sel (fr.selposns frec)
                           (setf (fr.sel-valid-p frec) t))
                (declare (list sel))
                (when (or (<= bmod (car sel)) (<= bmod (cdr sel)))
                  (return))))
            ;Update hpos setting per hscroll
            (let ((new-hscroll (- (fr.margin frec) (fr.hscroll frec))))
              (declare (fixnum new-hscroll))
              (unless (eql new-hscroll (fr.hpos frec))
                (setq h-scrolled t)
                (%update-hscroll frec)))
            ;update lines
            ; some call to with-frec above here clobbers this so put it back!
            ; maybe frec-turn-off-blinkers was only culprit
            (setf (fr.nodrawing-p frec) no-drawing)
            (%update-lines-maybe frec bmod)
            
            ;Update cursor
            ;*** need to add split cursor here.
            (let* ((buffer (fr.cursor frec))
                   (new-curcpos (%buffer-position buffer))
                   (cursor-bol-p (fr.cursor-bol-p frec))
                   (curcpos (fr.curcpos frec)))
              (declare (fixnum new-curcpos))
              (unless (and (not (fr.curs-changed-p frec))
                           (eql curcpos new-curcpos)
                           (if (null (fr.curpoint frec))
                             (not (fr.changed-p frec))
                             (not (eql (fr.curpoint frec) -1))))
                (dbmsg "~& Curpoint, old=~A"
                       (if (fr.curpoint frec) (point-string (fr.curpoint frec))))
                (unless (eql (fr.curcpos frec) new-curcpos)
                  (setf (fr.curs-changed-p frec) t)
                  (if (fr.cursor-bol-p-valid frec)
                    (setf (fr.cursor-bol-p-valid frec) nil)
                    (when (if (eql new-curcpos (1- curcpos))
                            (eql new-curcpos (nth-value 1 (frec-screen-line-num frec new-curcpos)))
                            (eql curcpos (nth-value 1 (frec-screen-line-num frec curcpos))))
                      (setq cursor-bol-p (setf (fr.cursor-bol-p frec) t)))))
                (setf (fr.changed-p frec) t)
                (setf (fr.curpoint frec) -1)
                (multiple-value-bind (curpoint cursor-line)
                                     (%screen-point frec new-curcpos cursor-bol-p)
                  (when curpoint
                    (setf (fr.curpoint frec)
                          (make-point (1- (point-h curpoint)) (point-v curpoint)))
                    (let ((pos (if (eql 0 new-curcpos) 0 (1- new-curcpos)))
                          max-ascent max-descent)
                      (multiple-value-bind (ff ms) 
                                           (if (>= pos (buffer-size buffer))
                                             (ccl::buffer-font-codes buffer)
                                             (ccl::buffer-char-font-codes buffer pos))
                        (multiple-value-bind (ascent descent) (font-codes-info ff ms)
                          (if cursor-line
                            (setq max-ascent (linevec-ref (fr.lineascents frec) cursor-line)
                                  max-descent (linevec-ref (fr.linedescents frec) cursor-line))
                            (multiple-value-setq (max-ascent max-descent)
                              (multiple-value-bind (ff ms) (ccl::buffer-font-codes buffer)
                                (font-codes-info ff ms))))
                          (setf (fr.curascent frec) (min ascent max-ascent)
                                (fr.curdescent frec) (min descent max-descent)))
                        (setf (fr.cursor-italic-p frec) (italic-ff-code-p ff))))))
                (setf (fr.curcpos frec) new-curcpos)
                (dbmsg " new=~A" (if (fr.curpoint frec)
                                   (point-string (fr.curpoint frec))))))
            ;Update bpchar
            (let (pos
                  (buffer (fr.cursor frec))
                  point)
              (unless (or (not (fr.changed-p frec))
                          (and (not (fr.buf-changed-p frec))
                               (not (fr.curs-changed-p frec))
                               (setq pos (fr.bpos frec))
                               (or (eql pos -1) (not (eql (fr.bpoint frec) -1)))))
                (when (null pos)
                  (let ((owner (fr.owner frec)))
                    (setq pos (and owner (fred-blink-position owner))))
                  (setf (fr.bpos frec) pos))
                (if (and pos 
                         (<= 0 (the fixnum pos))
                         (< pos (buffer-size buffer)))                         
                  (multiple-value-bind (ff ms) (ccl::buffer-char-font-codes buffer pos)
                    (if (setq point (%screen-char-point frec pos (ff-script ff)))
                      (progn
                        (setf (fr.bpoint frec) point)
                        (setf (fr.bpchar frec) (buffer-char buffer pos))
                        (setf (fr.bp-ff frec) ff)
                        (setf (fr.bp-ms frec) (make-point (point-h ms) #$SrcXor)))
                      (setf (fr.bpoint frec) -1)))                      
                  (setf (fr.bpoint frec) -1))))
            ;Update selection
            (unless (let* ((posns (fr.selposns frec))
                           (marks (fr.selmarks frec))
                           (valid (fr.sel-valid-p frec)))
                      (declare (cons posns) (list marks))
                      (loop
                        (let* ((posn (car posns))
                               (mark (car marks))
                               (car-pos (%buffer-position (car mark)))
                               (cdr-pos (%buffer-position (cdr mark))))
                          (declare (type cons posn mark)
                                   (fixnum car-pos cdr-pos))
                          ; I changed this. The old code made no sense to me.
                          (unless (and (eql (car posn) car-pos)
                                       (eql (cdr posn) cdr-pos))
                            (setf (car posn) car-pos
                                  (cdr posn) cdr-pos
                                  valid nil)))
                        (if (null (cdr posns))
                          (if (null (setq marks (cdr marks)))
                            (return valid)
                            (dolist (mark marks (return nil))
                              (declare (list mark))
                              (setf (cdr posns) (cons (%buffer-position (car mark))
                                                      (%buffer-position (cdr mark))))
                              (setq posns (cdr posns))))
                          (if (null (setq marks (cdr marks)))
                            (return (setf (cdr posns) nil))
                            (setq posns (cdr posns))))))
              (dbmsg "~& recompute sel")
              (setf (fr.changed-p frec) t
                    (fr.framed-sel-p frec) (fr.frame-sel-p frec))
              ; way up there about 10 pages ago we may or may not have turned it on -        
              (rotatef *sel-region* (fr.selrgn frec))
              (%screen-selection-region frec)
              (let ((rgn *sel-region*))
                (#_XorRgn rgn (fr.selrgn frec) rgn)
                (if nil ;(fr.nodrawing-p frec)
                  ;; HERNAN. used invalidate-region.
                  (%frec-invalRgn frec rgn)
                  (progn
                    (#_LMSetHiliteMode (ccl::bitclr 7 (the fixnum (#_LMGetHiliteMode))))
                    (#_InvertRgn rgn))))
              ))
          
          (when blinkers-on
            (screen-caret-on frec))
          (setf (fr.nodrawing-p frec) nil))
        ;If we got an update region, draw that... Is this right??? never does anything useful??
        ; dont do it - makes things ugly when e.g. meta-. opens a window 
        ; and then scrolls it. Let window-update-event-handler deal with redraw.
        ; But do it when horizontal scroll so we see new stuff now
        ; doing it now is also better when v-scroll with windoid atop
        ; but still worse when meta-. - so make meta-. a special case
        (when (and (not *gonna-change-pos-and-sel*) (not no-drawing))  ; ugh
          (with-macptrs ((updateRgn (%frec-updateRgn frec))) ;; Get the right updateRgn. HERNAN
            (let ((topleft (href updateRgn :region.rgnbbox.topleft))
                  (botright (href updateRgn :region.rgnbbox.botright)))
              (when (and (< (point-h topleft) (point-h botright))
                         (< (point-v topleft) (point-v botright)))
                (let ((offset (grafport-global-origin)))
                  (%frec-draw-contents-internal 
                   frec (add-points topleft offset) (add-points botright offset)))))))))))

(defun %hscroll-screen (frec new-hpos &aux (h (- new-hpos (fr.hpos frec))))
  (setf (fr.changed-p frec) t)
  (setf (fr.hpos frec) new-hpos)
  (rlet ((rect :rect :topleft #@(0 0) :botright (fr.size frec)))
    ;; HERNAN -> No invalRgn utility for gWorlds.
    ;; (#_InvalRgn (%scroll-screen-rect rect h 0))
    (%frec-invalRgn frec (%scroll-screen-rect frec rect h 0))
    (setf (fr.sel-valid-p frec) nil)
    (let ((selrgn (fr.selrgn frec))
          (rgn ccl::*temp-rgn*)
          (offset (make-point h 0)))
      (#_OffsetRgn selrgn h 0)
      (#_GetClip rgn)
      (#_SectRgn selrgn rgn selrgn)
      (unless (eql (fr.curpoint frec) #@(-1 -1))  ; <<
          (setf (fr.curpoint frec) (add-points (fr.curpoint frec) offset)))
      (setf (fr.bpoint frec) (add-points (fr.bpoint frec) offset)))))

;;; added the frec argument!
;Scroll the rectangle, bringing the update region along.
;Leaves the clobbered region in *temp-rgn*

(defun %scroll-screen-rect (frec rect h v)
  (let ((new-update-rgn ccl::*temp-rgn*)
        (rgn ccl::*temp-rgn-2*)
        ;(rgn-3 *sel-region*)
        (global-origin (grafport-global-origin)))
    ;; HERNAN.
    ;; (#_CopyRgn (pref (ccl::%getport) :windowRecord.updateRgn) new-update-rgn)
    (#_CopyRgn (%frec-updateRgn frec) new-update-rgn)
    (#_OffsetRgn :ptr new-update-rgn :long global-origin)
    (#_RectRgn rgn rect)
    (#_SectRgn rgn new-update-rgn new-update-rgn)
    ; add invisible part of rect to update region
    (#_rectrgn rgn rect)    
    (#_diffrgn rgn (pref (ccl::%getport) :grafport.visrgn) rgn)
    (#_unionrgn new-update-rgn rgn new-update-rgn)
    ;Add the clipped part of rect to the update region
    (#_rectrgn rgn rect)
    (#_diffrgn rgn (pref (%getport) :grafport.cliprgn) rgn)
    (#_unionrgn new-update-rgn rgn new-update-rgn)
    ;"Scroll" the update region
    (#_rectrgn rgn rect)
    (#_sectrgn rgn (pref (%getport) :grafport.cliprgn) rgn)
    ;; HERNAN
    ;; (#_ValidRgn rgn)
    (%frec-validRgn frec rgn)
    (#_offsetrgn new-update-rgn h v)
    (#_sectrgn new-update-rgn rgn new-update-rgn)
    ;; HERNAN -> No invalRgn utility for gWorlds.
    ;; (#_InvalRgn new-update-rgn)
    (%frec-invalRgn frec new-update-rgn)
    ;Finally scroll the text
    (#_ScrollRect rect h v new-update-rgn)
    new-update-rgn))

(eval-when (:compile-toplevel :execute)
  ; make this more flexible?? adjustable helps no body
  (defmacro with-font-run-vectors (&body body)
    ;; Changed a let to a let*. This seems to allow stack allocation of
    ;; the arrays under MCL 2.0. HERNAN
    ;; Is this required under MCL 3.0?
    `(let* ((*font-run-positions* (make-array $max-font-changes-per-line))
            (*font-run-endpoints* (make-array $max-font-changes-per-line))
            (*font-run-ordering* (make-array $max-font-changes-per-line)))
       (declare (dynamic-extent *font-run-positions* *font-run-endpoints*
                                *font-run-ordering*))
       ,@body))
  )

(defun scroll-screen-vertically (frec old-vstart new-vstart new-vend)
  (setq old-vstart (require-type old-vstart 'fixnum))
  (setq new-vstart (require-type new-vstart 'fixnum))
  (setq new-vend (require-type new-vend 'fixnum))
  (locally (declare (fixnum old-vstart new-vstart new-vend))
    (let* ((old-vend (+ old-vstart (- new-vend new-vstart)))
           (v (- new-vstart old-vstart))
           (start 0)
           (end 0))
      (declare (fixnum old-vend v start end))
      (if (<= new-vstart old-vstart)
        (setq start new-vstart end old-vend)
        (setq start old-vstart end new-vend))
      (when (or (fr.nodrawing-p frec) (eql new-vstart new-vend))
        (return-from scroll-screen-vertically
          (clear-screen-band frec start end)))
      (rlet ((rect :rect
                   :top start
                   :bottom end
                   :left 0
                   :right (point-h (fr.size frec))))
        ;; HERNAN: provide frec arg to %scroll-screen-rect.
        (%scroll-screen-rect frec rect 0 v) 
        (%scroll-sel-region rect (fr.selrgn frec) 0 v)
        (setf (fr.sel-valid-p frec) nil)
        (macrolet ((scroll-point (accessor)
                     `(let ((point ,accessor)
                            point-v)
                        (declare (fixnum point-v)) ; really?
                        (if (and point 
                                 (and (> (setq point-v (point-v point)) 0)
                                      (>= point-v start)
                                      (<= point-v end)))
                          (progn
                            (incf point-v v)
                            (setf ,accessor
                                  (if (and (>= point-v start)
                                           (<= point-v end))
                                    (make-point (point-h point) point-v)                                  
                                    #@(-1 -1))))
                          (setf ,accessor #@(-1 -1))))))          
          (scroll-point (fr.bpoint frec))
          #|
          ; this failed when the cursor (er.. caret) was only half visible at bottom of frec
          ; NOT right yet - may scroll too many times
          ; want to know if curpos is within the scope of this scroll
          (let* ((b-size (buffer-size (fr.cursor frec)))
                 (curpos (fr.curcpos frec))
                 (force (and curpos
                             (>= curpos (fr.bwin frec))
                             (<= curpos (1+ (- b-size (fr.zwin frec)))))))
            (scroll-point (fr.curpoint frec) force))))))
            |#
          ; just say we dont know where caret went and let callers caller (frec-update-internal) fix it
          (setf (fr.curpoint frec) #@(-1 -1))))))
  nil)

(defun clear-screen-band (frec start-y end-y &optional (left 0) right)
  (progn ;WITH-BACK-COLOR *WHITE-COLOR*  ; this certainly should be elsewhere - fix cut from fdi
    #-bccl (frec-arg frec)
    (setq start-y (require-type start-y 'fixnum)
          end-y (require-type end-y 'fixnum))
    (locally (declare (fixnum start-y end-y))
      (unless (>= start-y end-y)
        (multiple-value-bind (top-h top-v bot-h bot-v) (screen-caret-corners frec)
          (declare (ignore bot-h))
          (when (and top-h
                     (line-segments-overlap-p top-v bot-v start-y end-y))          
            (setf (fr.curpoint frec) #@(-1 -1))))
        (let ((bpoint-v (point-v (fr.bpoint frec))))
          (declare (fixnum bpoint-v))
          (when (and (<= start-y bpoint-v) (<= bpoint-v end-y))          
            (setf (fr.bpoint frec) #@(-1 -1))))
        (let ((size-h (point-h (fr.size frec))))
          (unless right (setq right size-h))
          (rlet ((rect :rect
                       :top start-y
                       :left left
                       :bottom end-y
                       :right right))
            (setf (fr.sel-valid-p frec) nil)
            (let ((selrgn (require-type (fr.selrgn frec) 'macptr))
                  (rgn ccl::*temp-rgn*)
                  (rect-rgn ccl::*temp-rgn-2*))
              (declare (type macptr selrgn rgn rect-rgn))
              (#_RectRgn rect-rgn rect)
              (#_GetClip rgn)
              (#_SectRgn rect-rgn rgn rect-rgn)
              (if (fr.nodrawing-p frec)
                ;; HERNAN -> No invalRgn utility for gWorlds.
                ;; (#_InvalRgn rect-rgn)
                (unless (fr.printing-p frec)
                  (%frec-invalRgn frec rect-rgn))
                (progn
                  ;; HERNAN
                  ;; (#_ValidRgn rect-rgn)
                  (%frec-validRgn frec rect-rgn)
                  (#_EraseRect rect)
                  (#_DiffRgn selrgn rect-rgn selrgn)))
              nil)))))))

(defun %inval-char-rect (frec char point &optional truetype-p)
  (let ((h (point-h point))
        (v (point-v point))
        (rgn ccl::*temp-rgn*)
        max-y min-y)
    (ccl::with-truetype-flags truetype-p
      (multiple-value-bind (a d w l) (font-info)
        (declare (ignore l))
        (if truetype-p
          (%stack-block ((theString 1))
            (setf (%get-byte theString) (char-code char))
            (multiple-value-setq (max-y min-y)
              (ccl::string-max-and-min-y theString :start 0 :end 1)))
          (setq max-y a min-y (- d)))
        (#_SetRectRgn rgn h (- v max-y) (+ h w) (- v min-y))
        ;; HERNAN -> No invalRgn utility for gWorlds.
        ;; (#_InvalRgn rgn)
        (%frec-invalRgn frec rgn)
        ))))

(defun %bwd-screen-line-start (frec pos count)
  (when (> count 0) (setq count 0))
  (loop
    (when (or (>= count -128) (eql 0 pos))
      (return))
    (setq pos (%bwd-screen-line-start frec pos -128)
          count (+ count 128)))
  (when (eql 0 pos)
    (return-from %bwd-screen-line-start
      (values pos
              (when (eql 0 (fr.bwin frec)) 0))))
  (multiple-value-bind (line line-pos) (frec-screen-line-num frec pos)
    (when line
      (setq pos line-pos)
      (let ((linevec (fr.linevec frec)))
        (loop
          (when (eql 0 count)
            (return-from %bwd-screen-line-start (values pos line)))
          (incf count)
          (when (eql 0 line)
            (return))
          (decf line)
          (decf pos (linevec-ref linevec line))))))
  (let ((bwin (fr.bwin frec))
        (zwin (- (buffer-size (fr.cursor frec)) (fr.zwin frec)))
        (buf (fr.cursor frec)))
    (when (< pos bwin) (setq zwin 0))
    (unless (fr.wrap-p frec)
      (loop
        (setq pos (ccl::buffer-backward-find-char buf #\newline pos))
        (if pos
          (incf pos)
          (setq pos 0))
        (when (or (eql 0 count) (eql 0 pos))
          (return-from %bwd-screen-line-start pos))
        (incf count)
        (when (<= pos zwin)
          ; Can revert to using fr.linevec
          (when (eql pos zwin)
            (decf pos)
            (incf count))
          (return-from %bwd-screen-line-start
            (%bwd-screen-line-start frec pos count)))))
    ; Line wrapped and off-screen. We've got some work to do.
    ; This works by moving back to the previous newline, then
    ; calling %compute-screen-line until we get to the
    ; current pos, and cacheing the line starts in a stack-consed buffer.
    ; Because of the comparison of count to -128 at the top of the
    ; function, we know that this buffer needs to have only 128 entries.
    ; Really, we need a pre-linevec cache in the frec so that we
    ; don't need to do this every time the user scrolls up.
    ; This code will be responsible for warming up that cache, and
    ; frec-update will need to scroll or invalidate it.
    ; For now, we do this computation every time which will make
    ; scrolling up in a large wrapped paragraph very slow.
    ;; HERNAN -> turned let into let* to have the arrays allocated on the stack.
    ;; Is this necessary under MCL 3.0?
    (let* ((linestarts (make-array 130))
           (line-nums (make-array 130))
           (line 0)                      ; index into linestarts
           (linestarts-full? nil)
           ;(linevec (fr.linevec frec))
           (count (- count))
           newline-pos line-pos line-length
           line-num 
           ;linevec-line-pos
           )
      (declare (dynamic-extent linestarts line-nums))
      (declare (ignore-if-unused line-num))  ; donkey poo
      (loop                             ; for each #\return before pos
        (when (and (> pos 0) (eql (buffer-char buf (1- pos)) #\newline))
          (decf pos))
        (setq newline-pos (ccl::buffer-backward-find-char buf #\newline pos))
        (if newline-pos
          (incf newline-pos)
          (setq newline-pos 0))
        (when (and (eq frec para-frec) (eql newline-pos para-start) (<= pos para-end))
          (return-from %bwd-screen-line-start (find-pcache-line frec pos count)))
        (setf (linevec-ref linestarts 0) newline-pos
              line-pos newline-pos
              line-num nil)
        (loop                           ; for each line between newline-pos & pos
          ; this is broken - dont do it. breaks when split pane of listener at least
          (if nil ;(and (< line-pos zwin) (>= line-pos bwin))
            ; Moved into the on-screen region.
            ; Can save some calls to %compute-screen-line
            (progn
              (unless line-num
                (multiple-value-setq (line-num linevec-line-pos)
                  (frec-screen-line-num frec line-pos))
                (unless (and line-num (eql line-pos linevec-line-pos))
                  (error "Inconsistency. ~s=~s, ~s­~s"
                         'line-num line-num line-pos linevec-line-pos)))
              (setq line-length (linevec-ref linevec line-num))
              (setf (linevec-ref line-nums line) line-num)
              (incf line-num))
            (progn
              (setq line-num nil)
              (setq line-length (%compute-screen-line frec line-pos))
              (setf (linevec-ref line-nums line) nil)))
          (incf line-pos line-length)
          (when (>= line-pos pos) (return))
          (when (eql 0 line-length)
            (error "End of buffer before reaching pos"))
          (incf line)
          (when (eql line 128)
            (setq linestarts-full? t
                  line 0))
          (setf (linevec-ref linestarts line) line-pos)
          (when (eql line-pos pos)
            (return)))
        (when (or linestarts-full? (>= line count))
          (SETF (LINEVEC-REF LINESTARTS (1+ LINE)) LINE-POS)
          (when (> line 2)
            (setf para-frec frec)
            (setf para-start newline-pos)
            (setf para-end line-pos)
            (SETF PARA-LINES (+ 2 line))
            (dotimes (i (+ 2 line))
              (setf (aref para-linevec i) (aref lineSTARTS i))))
          (setq line (mod (- line count) 128))
          (return-from %bwd-screen-line-start
            (values (linevec-ref linestarts line)
                    (linevec-ref line-nums line))))
        (decf count (1+ line))
        (setq line 0
              pos newline-pos)))))

(defun %frec-draw-contents-internal (frec &optional (topleft #@(0 0) tp) (botright #@(32767 32767)))
  (let ((blinkers-on (fr.caret-on-p frec)))
    #-bccl (frec-arg frec)
    (when (not tp)
      (when t ;(eq frec my-frec)
        ; intersection of clip and visrgn
        (multiple-value-setq (topleft botright) (grafport-visible-corners))))
    (unwind-protect
      (progn
        (%frec-turn-off-blinkers frec)
        (let* ((size (fr.size frec))
               (top (max (point-v topleft) 0))
               (left (max (point-h topleft) 0))
               (bottom (min (point-v botright) (point-v size)))
               (right (min (point-h botright) (point-h size))))
          (when  (and (< top bottom) (< left right))
            ; need to normalize topleft & botright to frec dimensions
            (with-macptrs (rgn)
              (unwind-protect
                (let ((rgn2 ccl::*temp-rgn*))            
                  (%setf-macptr rgn (#_NewRgn))
                  (#_GetClip rgn)  ; some of this can go away now - but its cheap
                  (#_SetRectRgn rgn2 left top right bottom)
                  (#_SectRgn rgn rgn2 rgn2)
                  (#_SetClip rgn2)
                  (#_EraseRgn rgn2)
                  ;; HERNAN.
                  ;; (#_ValidRgn rgn2)
                  (%frec-validRgn frec rgn2)
                  (let ((linevec (fr.linevec frec))
                        (lineheights (fr.lineheights frec))
                        (line 0)
                        (vpos 0)
                        (pos (fr.bwin frec))
                        (top (point-v topleft))
                        (bottom (point-v botright))
                        next-vpos
                        (numlines (fr.numlines frec))
                        start-line start-vpos end-line)
                    (loop
                      (when (>= line numlines) (return))
                      (setq next-vpos (+ vpos (linevec-ref lineheights line)))
                      (when (> next-vpos top)
                        (setq start-line line
                              start-vpos vpos
                              vpos next-vpos)
                        (incf line)
                        (return))
                      (incf pos (linevec-ref linevec line))
                      (setq vpos next-vpos)
                      (incf line))
                    (when start-line
                      (loop
                        (when (>= line numlines)
                          (setq end-line (1- numlines))
                          (return))
                        (when (>= vpos bottom)
                          (setq end-line line)
                          (return))
                        (incf vpos (linevec-ref lineheights line))
                        (incf line))                
                      (%redraw-screen-lines frec pos start-line end-line start-vpos)
                      (#_LMSetHiliteMode (ccl::bitclr 7 (the fixnum (#_LMGetHiliteMode))))
                      (#_InvertRgn (fr.selrgn frec))                
                      nil)))
                (unless (%null-ptr-p rgn)
                  (#_SetClip rgn)
                  (#_DisposeRgn rgn)))))))
      (when blinkers-on 
        (screen-caret-on frec)))))

#|
	Change History (most recent last):
	1  	 6/ 7/95	sidney  	The only Fred patches now needed for MCL 3.0
	2  	 6/26/95	Hernan  	Folded in changes from MCL3.0b5.
	3  	 8/14/95	sidney  	fold in 3.0p2 change to %hscroll-screen
	2  	 4/ 8/96	Hernan  	string -> theString
	3  	 4/18/96	Hernan  	Folded in changes from MCL-PPC 3.9f1c2.
	4  	 4/18/96	Hernan  	Need to recompile cons-fred-record because it uses
						$fred-record-length which has been changed by the form
						above it.
	5  	 4/25/96	Hernan  	Added to code needed to run on the PPC. When the time
						comes we just remove the comment.
	7  	10/22/96	sidney  	code is not PPC specific, just MCL version specific. Fixed the conditionalization so it works with MCL 3.1 too
	8  	 2/27/97	Hernan  	
|# ;(do not edit past this line!!)
