;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories



(in-package :SK8Development)

(provide "EDITTEXT")
(require "RECTANGLE")

;;;______________________________________________________
;;; EditText. Now implemented using Fred 3.0!!!
;;;______________________________________________________

;;; EditText -- the EditText actor. Note that it contains a slot that holds its clip region at all
;;;          times. This allows the field to draw itself properly outside the draw loop.

(new Rectangle :objectName "EditText" 
     :project SK8 :prototype t :undisposable t
     :properties '(editData
                   (lockTextLevel :inherit)
                   affixedScrollers
                   highlightcolor
                   ;;; *** Get rid of these properties!!! :
                   partnerHScroller
                   partnerVScroller))

(setf (localCreationRelations EditText) '(SK8::[affixedScrollers*]
                                          partnerHScroller
                                          partnerVScroller))

(setf gs:*EditText-the-object* EditText)

(setf (highlightcolor edittext) highlighted)
(setf (private EditText) nil) ; in public api

(setf (private editText :property 'editData) t)
(setf (private editText :property 'lockTextLevel) t)

(incrementOwnsRegionCount editText)
(setf (autotab editText) t)

;;; *** This is a kludge that works but generates garbage! call-with-tmp-string is supposed to stack allocate the string
;;; *** but here we are only calling ccl::buffer-substring which allocates the string on the heap!

(define-handler call-with-temp-string :hidden (EditText fcn &optional fromState toState)
                ;; [No way to build a string "in place", since buffer contents are stored NON-contiguously!
                ;;  but CAN do it by building a temp string and copying into it.]
                (let* ((buf (fred-buffer (editData me)))
                       (bufSize (buffer-size buf)))
                  (if fromState
                    (cond ((> fromState bufSize)
                           (return-from call-with-temp-string (funcall fcn "")))
                          ((< fromState 0)
                           (setq fromState 0)))
                    (setq fromState 0))
                  (if toState
                    (cond ((> toState bufSize)
                           (setq toState bufSize))
                          ((< toState 0)
                           (setq toState 0)))
                    (setq toState bufSize))
                  (funcall fcn (ccl::buffer-substring buf fromState toState))))

;;;______________________________________________________
;;; Macros.
;;;______________________________________________________

;;; Also needs to do it when the fr.selposns is out of whack.

(defmacro update-bwin (frec)
  (let ((bufSize (gensym)))
    `(let ((,bufSize (buffer-size (ccl::fr.buffer ,frec))))
       (when (or (> (ccl::fr.bwin ,frec) ,bufSize)
                 (and (> (caar (ccl::fr.selposns ,frec)) ,bufSize)
                      ;; Oh well... mark selposns dirty.
                      (progn (rplacd (car (ccl::fr.selposns ,frec)) #x1000000)
                             t)))
         (ccl::frec-update ,frec)))))

;;; Returns the port into which the field should draw itself.
;;; A gWorld if one is available.
;;; Enhanced to return the gWorld, or wptr and false if you wanted one of these and there is none.

(defmacro sk8-fred-port (field &optional (portType nil))
  (let* ((theWindow (gensym))
         (theGWorld (gensym)))
    (case portType
      (:wptr `(let ((,theWindow (view-container ,field)))
                (when ,theWindow (wptr ,theWindow))))
      (:gWorld `(let ((,theWindow (view-container ,field)))
                  (when ,theWindow
                    (slot-value ,theWindow 'gs:gWorld))))
      (otherwise
       `(let ((,theWindow (view-container ,field))
              ,theGWorld)
          (when ,theWindow
            (if (setf ,theGWorld (slot-value ,theWindow 'gs:gWorld))
              ,theGWorld
              (wptr ,theWindow))))))))

;;; Executes body setting the port to the fred's port if there is one or
;;; a temporary port if not.

(defmacro with-fred-port (fred &body body)
  (let ((thePort (gensym)))
    `(let ((,thePort (sk8-fred-port ,fred)))
       (if ,thePort
         (with-port ,thePort
           ,@body)
         (gs:with-temporary-port 
           ,@body)))))

;;;______________________________________________________
;;; SK8-Fred. Its functions and methods.
;;;______________________________________________________

(progn
  (defun ed-fred-default-key-handler (fred)
    (let ((code (char-code *CURRENT-CHARACTER*)))
      (when (or (and (>= code 32) (neq code #.(char-code #\Rubout)))
                (eq code 13)  ;; Newline
                (eq code 9)) ;; Tab
        (ed-self-insert fred))))
  
  (defun ed-fred-insert-continuation (fred)
    (multiple-value-bind (s e) (CCL::frec-get-sel (frec fred))
      (ed-fred-replace-range-with-undo fred #.(coerce '(#\Â #\Newline) 'string) s e)))
  
  ;;*** COULD MAKE ARROW-KEYS, DELETE KEY, DEL KEY (& MAYBE CLICKS?) GROUP THE
  ;;     LINE'S INDENTATION-TABS WITH THE LINE'S FIRST CHARACTER!!!
  ;;   + GROUP CONTINUATION-CHAR WITH ITS NEWLINE CHAR !!!
  
  ;; These four are defined so that ed-delete-with-undo is properly called for our purposes
  (defun ed-fred-rubout-char (fred)
    (let ((append-p (eq *LAST-COMMAND* :kill)))
      (multiple-value-bind (s e) (CCL::frec-get-sel (frec fred))
        (if (eql s e)
          (unless (eql 0 s) (ed-delete-with-undo fred (1- s) s t t append-p))
          (ed-delete-with-undo fred s e t t append-p))))
    (setf (slot-value fred 'CCL::last-command) :kill))
  
  (defun ed-fred-delete-char (fred)
    (let ((append-p (eq *LAST-COMMAND* :kill)))
      (multiple-value-bind (s e) (CCL::frec-get-sel (frec fred))
        (if (eql s e)
          (unless (eql e (buffer-size (fred-buffer fred))) (ed-delete-with-undo fred e (1+ e) t nil append-p))
          (ed-delete-with-undo fred s e t nil append-p))))
    (setf (slot-value fred 'CCL::last-command) :kill))
  
  (defun ed-fred-rubout-word (fred)
    (multiple-value-bind (s e fwd) (CCL::FRED-SELECTION-FOR-WORD-COMMAND fred nil)
      (when s (ed-delete-with-undo fred s e t (null fwd) (eq *LAST-COMMAND* :kill))))
    (setf (slot-value fred 'CCL::last-command) :kill))
  
  (defun ed-fred-delete-word (fred)
    (multiple-value-bind (s e fwd) (CCL::FRED-SELECTION-FOR-WORD-COMMAND fred t)
      (when s (ed-delete-with-undo fred s e t (null fwd) (eq *LAST-COMMAND* :kill))))
    (setf (slot-value fred 'CCL::last-command) :kill))
  
  (defvar *SK8-ed-fred-comtab* (make-comtab 'ed-fred-default-key-handler))
  (dolist (spec
           '((#\Delete                             ed-fred-rubout-char)
             ((:control #\Delete)                  ed-fred-rubout-word)
             ((:meta #\Delete)                     (:control #\Delete))
             
             (#\Rubout                             ed-fred-delete-char)
             ((:control #\Rubout)                  ed-fred-delete-word)
             ((:meta #\Rubout)                     (:control #\Rubout))
             
             (#\UpArrow                            ed-previous-line)
             ((:shift #\UpArrow)                   ed-select-previous-line)
             (#\DownArrow                          ed-next-line)
             ((:shift #\DownArrow)                 ed-select-next-line)
             (#\BackArrow                          ed-backward-char)
             ((:shift #\BackArrow)                 ed-backward-select-char)
             (#\ForwardArrow                       ed-forward-char)
             ((:shift #\ForwardArrow)              ed-forward-select-char)
             
             ((:control #\BackArrow)               ed-backward-word)
             ((:meta #\BackArrow)                  (:control #\BackArrow))
             ((:shift :control #\BackArrow)        ed-backward-select-word)
             ((:shift :meta #\BackArrow)           (:shift :control #\BackArrow))
             
             ((:control #\ForwardArrow)            ed-forward-word)
             ((:meta #\ForwardArrow)               (:control #\ForwardArrow))
             ((:shift :control #\ForwardArrow)     ed-forward-select-word)
             ((:shift :meta #\ForwardArrow)        (:shift :control #\ForwardArrow))
             
             ((:control #\a)                       ed-beginning-of-line)
             ((:control #\e)                       ed-end-of-line)
             
             ))
    (comtab-set-key *SK8-ed-fred-comtab* (first spec) (second spec)))
  
  (let (ch subst ctrl?)
    (dolist (subst-pair '((#\^f . #\ForwardArrow)
                          (#\^b . #\BackArrow)
                          (#\^n . #\DownArrow)
                          (#\^p . #\UpArrow)))
      (setq ch (car subst-pair) subst (cdr subst-pair))
      (if (char< ch #\Space)
        (setq ch `(:control ,(code-char (+ (char-code ch) #.(char-code #\@))))
              ctrl? t)
        (setq ctrl? nil))
      (comtab-set-key *SK8-ed-fred-comtab* ch subst)
      
      (cond
       (ctrl?
        (comtab-set-key *SK8-ed-fred-comtab* `(:shift ,@ch) `(:shift ,subst))
        (comtab-set-key *SK8-ed-fred-comtab* `(:meta ,@ch) `(:meta ,subst)))
       (t
        (comtab-set-key *SK8-ed-fred-comtab* `(:control ,ch) `(:control ,subst))
        (comtab-set-key *SK8-ed-fred-comtab* `(:shift :control ,ch) `(:shift :control ,subst))
        (comtab-set-key *SK8-ed-fred-comtab* `(:meta ,ch) `(:meta ,subst))
        (comtab-set-key *SK8-ed-fred-comtab* `(:shift :meta ,ch) `(:shift :meta ,subst)))))))

;;; More low level stuff needed for setting the text.

(defvar *ed-fred-undoing-p* nil)

;ED-INSERT-WITH-UNDO (called by ed-self-insert, ed-insert-char, paste, ed-replace-with-undo)
;ED-DELETE-WITH-UNDO (called by ed-self-insert, ed-insert-char, paste, cut, clear, ed-replace-with-undo)

;; Promote ed-insert-with-undo into a generic!

(let ((*warn-if-redefine-kernel* nil))
  (unless (fboundp 'default-ed-insert-with-undo)
    (setf (symbol-function 'default-ed-insert-with-undo) #'ed-insert-with-undo)
    (fmakunbound 'ed-insert-with-undo))
  (defmethod ed-insert-with-undo (view str &optional position append-p theFont)
    (default-ed-insert-with-undo view str position append-p thefont)))

(defun ed-fred-replace-range-with-undo (fred str start end)
  (let ((frec (frec fred))
        (buf (fred-buffer fred))
        (undo-cons nil)
        screenLineStart)
    (unless (eql start end)
      (setq undo-cons (ed-delete-with-undo fred start end nil)))
    (with-fred-port fred 
      (setf screenLineStart (CCL::frec-screen-line-start frec (ccl::fr.wposm frec))))
    ;; *** WHAT'S THIS FOR???
    (when (and (<= screenLineStart (buffer-position buf))
               (< (buffer-position buf)
                  (buffer-position (ccl::fr.wposm frec))))
      (set-mark (ccl::fr.wposm frec) buf))
    (when str
      (ed-insert-with-undo fred str start (or undo-cons (eq *last-command* ':self-insert))))
    (CCL::set-fred-undo-stuff fred :self-insert "Typing")
    str))

;;; Returns the screen line number of pos.

(defun sk8-frec-screen-line-num (frec pos)
  (unless (ccl::fr.wrap-p frec)
    (return-from sk8-frec-screen-line-num (buffer-line (ccl::fr.cursor frec) pos)))
  (let* ((lineNum 0)
         (lineStart 0)
         (size (buffer-size (ccl::fr.cursor frec)))
         (bwin (ccl::fr.bwin frec))
         new-bwin
         (zwin (- size (ccl::fr.zwin frec))))
    (loop 
      (when (and (<= bwin lineStart) (<= lineStart zwin)) 
        (incf lineNum
              (let ((theline 0)
                    (linevec (ccl::fr.linevec frec))
                    (numlines (ccl::fr.numlines frec)))
                (loop
                  (when (>= theline numlines)      ; shouldn't happen
                    (return theLine))
                  (setq new-bwin (+ bwin (ccl::linevec-ref linevec theline)))
                  (when (> new-bwin pos)
                    (return-from sk8-frec-screen-line-num (values (+ theline lineNum) bwin)))
                  (setq bwin new-bwin)
                  (incf theline)
                  (setf lineStart bwin)))))
      (with-fred-port (ccl::fr.owner frec)
        (incf lineStart (ccl::%compute-screen-line frec lineStart)))
      (when (> lineStart pos)
        (return-from sk8-frec-screen-line-num (1- lineNum)))
      (when (>= lineStart size)
        (return-from sk8-frec-screen-line-num (1- lineNum)))
      (incf lineNum))))

(defun sk8-frec-line-start (frec lineNum)
  (unless (ccl::fr.wrap-p frec)
    (return-from sk8-frec-line-start (buffer-line-start (ccl::fr.cursor frec) 0 lineNum))
    (with-fred-port (ccl::fr.owner frec) 
      (ccl::frec-screen-line-start frec 0 lineNum))))

;;; Returns the height and width in pixels the field should have to show all its text. Does not
;;; include margins or the frame. 

#|

;;This one is optimized but fails in some cases and since we need a build...fuck it.
(defun sk8-frec-size (frec)
  (let* ((lineStart 0)
         (size (buffer-size (ccl::fr.cursor frec)))
         (bwin (ccl::fr.bwin frec))
         new-bwin
         (zwin (- size (ccl::fr.zwin frec)))
         (height 0)
         (width 0))
    (loop 
      ;; Use cached values if any.
      (when (and (<= bwin lineStart) (<= lineStart zwin)) 
        (let ((theline 0)
              (linevec (ccl::fr.linevec frec))
              (lineHeights (ccl::fr.lineheights frec))
              (lineWidths (ccl::fr.linewidths frec))
              (numlines (ccl::fr.numlines frec)))
          (loop
            (when (>= theline numlines)      ; shouldn't happen
              (return))
            ;; New width and height...
            (incf height (aref lineHeights theLine))
            (setf width (max width (aref lineWidths theLine)))
            (setq new-bwin (+ bwin (ccl::linevec-ref linevec theline)))
            (when (> new-bwin size)
              (return-from sk8-frec-size (values width height)))
            (setq bwin new-bwin)
            (incf theline)
            (setf lineStart bwin))))
      ;; No cached vals? Oh well... This is where things get expensive.
      (multiple-value-bind (len asc desc lineHeight lineWidth)
                           (with-fred-port (ccl::fr.owner frec)
                             (ccl::%compute-screen-line frec lineStart))
        (declare (ignore asc desc))
        (incf height lineHeight)
        (setf width (max width lineWidth))
        (incf lineStart len))
      (when (>= lineStart size)
        (return-from sk8-frec-size (values width height))))))
|#

;;; This one is a lot simple and it works, but since it does not make use of the cached
;;; values, it is a lot slower.

(defun sk8-frec-size (frec)
  (let ((size (buffer-size (ccl::fr.cursor frec)))
        (height 0)
        (width 0))
    (loop
      (when (<= size 0) (return))
      (setf size (ccl::%bwd-screen-line-start frec size 1))
      (multiple-value-bind (len asc desc lineHeight lineWidth)
                           (ccl::%compute-screen-line frec size)
        (declare (ignore len asc desc))
        (incf height lineHeight)
        (setf width (max width lineWidth)))
      (decf size))
    (values width height)))

;;;MCL3.0 - fred-mixin now has to be mixed with a simple-view to be used
(defclass SK8-fred (fred-mixin simple-view)
  ((actor :initarg :actor)
   (buf-stream)))

;;; Needed to redefine this because MCL's version only works when the fred is
;;; on the Stage.

(defmethod (setf fred-tabcount) (value (w sk8-fred))
  (setf (ccl::fr.tabcount (frec w)) value))

;;; Taken straight out of the MCL sources. Note that we do not call next method.

(defmethod view-key-event-handler ((w sk8-fred) *current-character*)
  (let* ((*current-keystroke* (if (boundp '*current-event*)
                                (event-keystroke (rref *current-event* eventrecord.Message)
                                                 (rref *current-event* eventrecord.Modifiers))
                                (char-code *current-character*)))
         ;; If the character was entered using a direct call to keydown, make this work!
         ;; If the *current-event* is not bound, just use the character as the key stroke.
         ;         (*processing-events* nil)
         (hook *fred-keystroke-hook*)
         (theTab (or hook (fred-shadowing-comtab w) (slot-value w 'comtab))))
    (declare (special *current-keystroke* *processing-events*))
    ;; If *current-keystroke* is 0 (null char), and *current-character* has something
    ;; in it, it means that the keydown event was generated programmatically, and thus 
    ;; we should use the character supplied. 
    (when (zerop *current-keystroke*) 
      (setf *current-keystroke* (char-code *current-character*)))
    (if (and hook (or (functionp hook) (symbolp hook)))
      (funcall hook w)
      (run-fred-command w (keystroke-function w *current-keystroke* theTab)))
    (when (not (menu-enabled-p *edit-menu*))(menu-update *edit-menu*))))

(defmethod ed-fred-replace-range ((fred sk8-fred) str start end)
  (unless (eql start end) 
    (buffer-delete (fred-buffer fred) start end))
  (when str
    (buffer-insert (fred-buffer fred) str start)
    str))

;;; This is redefined so that view-clip-region does not die on us.

(defmethod view-container ((f sk8-fred))
  (gs:node-window (slot-value f 'actor)))

(defmethod view-mouse-position ((f sk8-fred))
  (sk8-multival-bind (h v) (mouseLoc Stage)
    (let ((theActor (slot-value f 'actor)))
      (sk8-multival-bind (ll tt rr bb) (boundsRect theActor :physical t)
        (declare (ignore rr bb))
        (sk8-multival-bind (fh fv) (framesize theActor :physical t)
          (make-point (- h (+ ll fh))
                      (- v (+ tt fv))))))))

;;; ____________________________________________________________

;;; These methods are redefined to avoid calling #_invalRgn or #_validRgn when the
;;; fred is drawing into a gWorld.

(defmethod validate-region ((f sk8-fred) rgn)
  (ccl::%frec-validRgn (frec f) rgn))

(defmethod invalidate-region ((f sk8-fred) region &optional erase-p)
  (declare (ignore erase-p))
  (ccl::%frec-invalRgn (frec f) region))

(defmethod set-view-scroll-position ((f sk8-fred) h &optional v scroll-visibly)
  (declare (ignore h v scroll-visibly))
  )

(defmethod mini-buffer-update ((f sk8-fred))
  )

;;; ____________________________________________________________

(defun fill-in-buffer-text-info (buf lines styles)
  ;; First the lines.
  (dolist (oneLine (butlast lines))
    (buffer-insert buf oneLine)
    (buffer-insert buf (string #\Newline)))
  (buffer-insert buf (car (last lines)))
  ;; Now the styles.
  (dolist (stylePair styles)
    (buffer-set-font-spec buf (cadr stylePair) (car stylePair)))
  ;; If the buffer is empty or the change affects everything, change
  ;; the buffer empty font to the font desired.
  (when (= 1 (length styles))
    (multiple-value-bind (ff ms) (font-codes (cadr (car styles)))
      (ccl::set-buffer-empty-font-codes buf ff ms))))

(defmethod new-fred ((original sk8-fred) &key newSize newActor newWptr textLines styles)
  (let* ((fredActor (or newActor (slot-value original 'actor)))
         (wptr (or newWptr (wptr original)))
         (size (or newSize (view-size original)))
         (thefont (view-font original))
         (ed-fred (make-instance (class-name (class-of original))
                    :actor fredActor
                    :comtab *SK8-ed-fred-comtab*
                    :save-buffer-p t  ; ??? NIL ??? #####
                    :copy-styles-p nil
                    :buffer-chunk-size 1024
                    :view-size size
                    :view-font thefont
                    :history-length 1
                    :text-edit-sel-p t)))
    (setf (wptr ed-fred) wptr)
    (CCL::FREC-DEACTIVATE (frec ed-fred))
    ;; (setf (ccl::fr.numlines (frec ed-fred)) 1) ;!!!
    (setf (fred-tabcount ed-fred) (fred-tabcount original))
    ;; *** NOTE: the make-instance does a USE-BUFFER on the underlying buffer -- so we have to make sure to
    ;;            UNUSE-BUFFER when we're done with this object (can be done through stream-close on buf-stream)
    (setf (slot-value ed-fred 'buf-stream)
          (make-instance 'CCL::buffer-stream :buffer (make-mark (fred-buffer ed-fred) 0)))
    ;; Make sure the frec is inited.
    ;; (ccl::reinit-frec (frec ed-fred) ed-fred)
    ;; Make an update region for frec. This line HAS to be at the end!
    (setf (ccl::fr.selRgn (frec ed-fred)) (ccl::%new-rgn))
    (setf (ccl::fr.updateRgn (frec ed-fred)) (ccl::%new-rgn))
    ;; Add the textStyles and lines.
    (when (and styles textLines)
      (fill-in-buffer-text-info (fred-buffer ed-fred) textLines styles))
    ed-fred))

;;; Bootsstraping the first sk8-fred!

(setf (editData editText) (make-instance 'sk8-fred
                            :actor EditText
                            :comtab *SK8-ed-fred-comtab*
                            :save-buffer-p t
                            :copy-styles-p nil
                            :buffer-chunk-size 1024
                            :view-font '("Geneva" 9 :plain)))

(setf (wptr (editData editText)) nil)
(setf (slot-value (editData editText) 'actor) editText)
(set-view-size (editData editText) #@(10 10))

(buffer-set-font-spec (fred-buffer (editData editText)) '("Geneva" 9 :plain))
(multiple-value-bind (ff ms ignored1 ignored2) (font-codes '("Geneva" 9 :plain))
  (declare (ignore ignored1 ignored2))
  (ccl::set-buffer-empty-font-codes (fred-buffer (editData editText)) ff ms))

(setf (boundsRect editText) (boundsRect editText))               ; get the innards' size in synch

;;; At the point where this is called, the new buffer and the old buffer have the
;;; same text. 

(defun copy-font-information (old-buffer new-buffer)
  ;;; (1) Copy the empty buffer font.
  (multiple-value-bind (ff ms) (ccl::buffer-empty-font-codes old-buffer)
    (ccl::set-buffer-empty-font-codes new-buffer ff ms))
  ;;; (2) Copy all the other fonts.
  (let ((curPos 0))
    (loop
      (multiple-value-bind (changePosition changesDone changesToGo) 
                           (buffer-next-font-change old-buffer curPos)
        (declare (ignore changesDone changesToGo))
        (unless changePosition
          (buffer-set-font-spec new-buffer (buffer-char-font-spec old-buffer curPos) curPos)
          (return-from copy-font-information nil))
        (buffer-set-font-spec new-buffer (buffer-char-font-spec old-buffer curPos) curPos)
        (setf curPos changePosition)))))

(defun copy-ed-fred (old-ed-fred &key newSize newActor newWptr)
  (let* ((ed-fred (new-fred old-ed-fred :newSize newSize :newActor newActor :newWptr newWptr))
         (new-buffer (fred-buffer ed-fred))
         (old-buffer (fred-buffer old-ed-fred)))
    ;; Copy the text.
    (buffer-insert new-buffer (buffer-substring old-buffer (buffer-size old-buffer) 0))
    ;; Fix font information.
    (copy-font-information old-buffer new-buffer)
    ed-fred))

(defmethod window-show-cursor ((field SK8-fred) &optional position scrolling)
  ;; This is called during view-key-event-handler, but we do nothing within MCL's view system --
  ;; we perform this function in our key event handlers
  (declare (ignore position scrolling)))

;;; If this variable is true, the method above always focuses on the window.
;;; Otherwise it uses the gWorld if one is available.

(defvar *keep-fred-focused-on-window* nil)

(defmacro focusing-fred-on-its-window (&body body)
  `(unwind-protect
     (progn 
       (setf *keep-fred-focused-on-window* t)
       ,@body)
     (setf *keep-fred-focused-on-window* nil)))

#|

(defun print-origins (theField)
  (let* ((clos-window (gs:node-window theField))
         (gWorldOrg (cl-user::port-origin (slot-value clos-window 'gs:gWorld)))
         (wptrOrg (cl-user::port-origin (wptr clos-window))))
  (format t "~%The gWorld: {~d,~d} and the wptr: {~d,~d}."
          (point-h gWorldOrg) (point-v gWorldOrg)
          (point-h wptrOrg) (point-v wptrOrg))))

(defmacro with-zero-origin (thePort &body body)
  (let ((savedOrigin (gensym)))
    `(let ((,savedOrigin (cl-user::port-origin ,thePort)))
       (#_setOrigin

|#

;;; Here I am trying to lock out the SK8 view system.
;;; This should only be called within with-fred-drawing in which case everything is fine...

(defmethod ccl::call-with-focused-view ((field sk8-fred) function &optional font-view)
  (declare (ignore font-view))
  (ed-beep)
  (funcall function field))

(defmethod ccl::call-with-focused-view :around ((field sk8-fred) function &optional font-view)
  (declare (ignore font-view function field))
  )

;;; This is not directly used by our drawing code -- we define it so our origin gets correctly restored when our
;;; WITH-FRED-DRAWING body is interrupted and then resumed by MCL's view system
;;; Needs to focus on the gWorld (not the wptr).

(defmethod focus-view ((field SK8-fred) &optional font-view)
  (without-interrupts
   ;; reset the origin of the last wptr (this is a concession to gwviews).
   ;; assumption is that they are all originally 0 based. 
   ;; doesn't hurt for windows.
   (#_SetOrigin :long #@(0 0))
   (let ((thePort (if *keep-fred-focused-on-window*
                    (wptr (gs:node-window (slot-value field 'actor)))
                    (sk8-fred-port field))))
     (if thePort 
       (progn 
         (#_SetPort thePort)
         ;; This line was here in Adam's original. In MCL 3.0 it messes up the origin of
         ;; the port. This happens when it gets called before the window has come up.
              ;; BUT if we take this out we have problems at activateText time
              ;; and also when we type a new expression into the Query field or messagebox...
              ;; Hernan.
         (#_SetOrigin :long (subtract-points #@(0 0) (slot-value field 'view-position)))
         (setq CCL::*CURRENT-FONT-VIEW* font-view)
         (setq *CURRENT-VIEW* field))
       (focus-view nil font-view)))))

;;; This makes upArrow and downArrow work when wrapping is on.

(defun ed-next-line-pos-wrapped (w &optional select n)
  (when (null n) (setq n (fred-prefix-numeric-value w)))
  (let ((c (fred-buffer w))
        (frec (frec w)))
    (when (null select) (collapse-selection w (>= n 0)))
    (with-fred-port w
      (let* ((line-pos (ccl::frec-screen-line-start frec (buffer-position c) 0))
             (new-line-pos (ccl::frec-screen-line-start frec line-pos n))
             (goal-pos (+ new-line-pos
                          (if (eq *last-command* 'vertical-motion)
                            (slot-value w 'ccl::goal-column)
                            (setf (slot-value w 'ccl::goal-column)
                                  (- (buffer-position c) line-pos)))))
             (end-pos (max 0 (1- (ccl::frec-screen-line-start frec line-pos (1+ n))))))
        (when (< end-pos goal-pos) 
          (setq goal-pos end-pos))
        (set-fred-last-command w 'vertical-motion)
        ;; KLUDGE!!! Make the cursor go to the end of the text.
        (when (= goal-pos (1- (buffer-size c))) 
          (setf goal-pos (buffer-size c)))
        ;; Return.
        (values goal-pos (>= n 0))))))

(defmethod ed-next-line ((field sk8-fred) &optional n)
  (if (ccl::fr.wrap-p (frec field))
    (set-mark (fred-buffer field) (ed-next-line-pos-wrapped field nil n)) 
    (call-next-method)))

#|

;;; No longer used. 

;;; This is modified so that the new font is NOT merged with the previous one. The change is to 
;;; avoid using the old font codes when getting the new font codes.

(defun sk8-buffer-replace-font-spec (buf old-spec new-spec)
  "replaces the old spec with the new one."
  (multiple-value-bind (old-ff old-ms) (font-codes old-spec)
    (multiple-value-bind (new-ff new-ms) (font-codes new-spec) ;;  old-ff old-ms
      (buffer-replace-font-codes buf old-ff old-ms new-ff new-ms))))

|#

;;______________________________________________________
;;; EDITTEXT and its handlers.
;;______________________________________________________

;;; Fillcolor and textColor can only be RGBColors.

(define-handler (setf fillcolor) (newValue EditText)
  (unless (inheritsFrom newValue RGBColor)
    (sk8-error PropertyTypeMismatchError
               :object        me
               :expectedType  RGBColor
               :ownerObject   EditText
               :propertyName 'fillcolor
               ))
  (call-next-method))

(define-handler (setf textColor) (newValue EditText)
  (unless (inheritsFrom newValue RGBColor)
    (sk8-error PropertyTypeMismatchError
               :object        me
               :expectedType  RGBColor
               :ownerObject   EditText
               :propertyName 'textColor
               ))
  (call-next-method))

;;; The field's state is whatever is on the window. Blit it back to the gWorld.
;;; This is a bit strange and is only required for the mouseDown handler of EditText.

(defun EditText-to-gWorld (theField)
  (let ((clos-window (gs:node-window theField)))
    (when clos-window
      (let ((gWorld (slot-value clos-window 'gs:gWorld)))
        (when gWorld
          (gs:let+ ((visBlitRgn (:region))
                    (wptr (wptr clos-window)))
            ;; Only blit the parts of the drawing that are visible (not obscured)
            (#_SectRgn (ownedRegion theField) (rref wptr :CGrafport.visRgn) visBlitRgn)
            (gs:window-to-gWorld clos-window gworld nil visBlitRgn)))))))

(defun editText-to-window (theField &optional extraClipRgn)
  (let* ((clos-window (gs:node-window theField))
         (gWorld (when clos-window (slot-value clos-window 'gs:gWorld))))
    (when gworld
      (gs:let+ ((visBlitRgn (:region)))
        ;; Only blit the parts of the drawing that are visible (not obscured)
        (#_SectRgn (ownedRegion theField) (rref (wptr clos-window) :CGrafport.visRgn) visBlitRgn)
        (when extraClipRgn
          (#_sectRgn visBlitRgn extraClipRgn visBlitRgn))
        (gs:gWorld-to-window gworld clos-window nil visBlitRgn)))))

(defun editText-cursor-to-window (theField)
  (gs:let+ ((fred (editData theField))
         (topLeft (view-position fred)) ;; use view-position to deal with ScriptEditText w/breakpoints.
         (ll (point-h topLeft))
         (tt (point-v topLeft))
         (frec (frec fred))
         (cursorRgn (:region)))
    (multiple-value-bind (top-h top-v bot-h bot-v) (ccl::screen-caret-corners frec)
      (when top-h
        (#_setRectRgn cursorRgn (1- (+ ll top-h)) (1- (+ tt top-v)) (+ ll bot-h 1) (+ tt bot-v 1))
        (editText-to-window theField cursorRgn)))))

;;; initialize --  creates a new ScriptEditText actor. This involves the creation of the field structure.
;;;           Since this actor owns its region, it is important to create its region and install
;;;           it in the object. Note that the draw function of fields is set here to scriptEditText-draw.

(define-handler initialize (editText original isNew args)
  (declare (ignore args isNew))
  (call-next-method)
  ;; Since we will look at slots in original, it better be set to the
  ;; edittext parent. 
  (setf original (originalancestor me original EditText))
  (SK8-multival-bind (width height) (gs:rect-size (gs:node-physicalBoundsRect me))
    (SK8-multival-bind (frame-width frame-height) (framesize me :physical t)
      (gs:fdecf width (gs:fdouble frame-width))
      (gs:fdecf height (gs:fdouble frame-height)))
    (let* ((old-ed-fred (editData original))
           (newWptr (when (gs:node-window me) (wptr (gs:node-window me))))
           (f (if old-ed-fred
                (copy-ed-fred old-ed-fred :newSize (gs:SK8Coords-to-point width height) :newActor me :newWptr newWptr)
                (new-fred old-ed-fred :newSize (gs:SK8Coords-to-point width height) :newActor me :newWptr newWptr))))
      ;; Install the field, and the owned region.
      (setf (editData me) f)
      (setf (ownedRegion me) (T_NewRgnGC))
      (setf (gs:node-drawFunction me) 'editText-draw)
      ;; Inherit wrapping and justification!
      (setf (wrapping me) (wrapping original))
      (setf (justification me) (justification original))
      (setf (tabLength me) (tabLength original))
      ;; There are some bizarre cases in which this is required to avoid getting
      ;; garbage in fields. (Happens when the field's text is locked from the start).
      ; (update-ed-fred-if-necessary f)
      ;; (setf (dirty me) (dirty original))
      )))

;;; This is a method of sk8-fred! This is so that scriptEditText can redefine it with minimum pains.

(defmethod synch-scriptEditText-to-fillRegion ((f sk8-Fred))
  (let ((theField (slot-value f 'actor))
        newsize topleft frec)
    (when f
      (setq frec (frec f))
      (gs:recompute-fill-region theField (gs:node-flags theField))
      (with-dereferenced-handles ((rgnPtr (gs:node-fillRegion theField)))
        (%setf-macptr rgnPtr (rref rgnPtr :region.rgnBBox :storage :pointer))
        (setq newsize (make-point
                       (- (the fixnum (rref rgnPtr :rect.right)) (the fixnum (rref rgnPtr :rect.left)))
                       (- (the fixnum (rref rgnPtr :rect.bottom)) (the fixnum (rref rgnPtr :rect.top)))))
        (setq topleft (rref rgnPtr :rect.topleft)))
      (unless (eql (view-position f) topleft)
        (setf (slot-value f 'view-position) topleft))
      (with-fred-port f
        (unless (eql (CCL::FREC-SIZE frec) newsize)
          ;; In bwin is out of whack, fix it.
          (update-bwin frec)
          (setf (slot-value f 'view-size) newsize)
          (CCL::FREC-SET-SIZE frec newsize))))))

(define-handler makeFillRegion (editText)
  (gs:let+ ((rect (:rect :SK8Rect (gs:recompute-physicalBoundsRect me))))
    (sk8-multival-bind (frameX frameY) (framesize me :physical t)
      (#_InsetRect rect frameX frameY)
      (#_RectRgn (gs:node-fillRegion me) rect)
      (gs:fillDirty! (gs:node-flags me) 0))
    ;; Make sure the field is ALWAYS in synch with the fillRegion.
    (synch-scriptEditText-to-fillRegion (editData me))))

#|

;;; THIS IS ADAM's ORIGINAL WHICH MESSES UP WHEN THE
;;; ORIGIN OF THE PORT WAS NOT ORIGINALLY 0.
;;; Focus on the gWorld, not the wptr.

(defmacro with-fred-drawing ((fredActor &optional fred wptr) &body body)
  (let* ((theFredActor (gensym))
         (theFredClosInst (gensym))
         (thisPort (gensym))
         (savedClip (gensym))
         (fredOrigin (gensym))
         (fredClip (gensym)))
    `(without-interrupts
      (gs:let+ ((,theFredActor ,fredActor)
             (,theFredClosInst ,(or fred `(editData ,theFredActor)))
             (,thisPort (or ,wptr (sk8-fred-port ,theFredClosInst)))
             (,savedClip (:region))
             (,savedOrigin 0)
             (,fredOrigin 0)
             (,fredClip (:region))
             ;; We bind *CURRENT-VIEW* so our origin gets correctly restored (via our focus-view method)
             ;; when our WITH-FRED-DRAWING body is interrupted and then resumed by MCL's view system
             (*CURRENT-VIEW* ,theFredClosInst))
        (when ,thisPort
          (setf ,fredOrigin (subtract-points 0 (slot-value ,theFredClosInst 'view-position)))
          (require-trap #_CopyRgn (ownedRegion ,theFredActor) ,fredClip)
          (require-trap #_OffsetRgn :ptr ,fredClip :long ,fredOrigin)
          (with-port ,thisPort
            (gs:with-hilitecolor ,thisPort (highlightcolor ,thefredactor)
              (require-trap #_GetClip ,savedClip)
              (require-trap #_SetOrigin :long ,fredOrigin)
              (require-trap #_SetClip ,fredClip)
              (with-RGB (backColor (MCL-color (fillColor ,theFredActor))) 
                (require-trap #_RGBBackColor backColor))
              (with-RGB (foreColor (MCL-color (textColor ,theFredActor))) 
                (require-trap #_RGBForeColor foreColor))
              ,@body
              (require-trap #_SetPort ,thisPort) ; just in case it was switched in the body
              (require-trap #_SetOrigin :long 0)
              (require-trap #_SetClip ,savedClip))))))))

|#

;;; _______________________________ 
;;; WITH-FRED-DRAWING
;;; _______________________________ 

;;; Why go through all the mess of writing such a hideous looking thing? Well, the fact
;;; that we added gWorld imaging capabilities to Fred is NOT ENOUGH to allow Fred to image
;;; into a port because frec draws itself assuming its topleft is at (0 0). All this macro
;;; does is to mess with the origin of the port we are drawing into so that when we call the
;;; frec drawing procedures, frec is in the right place. This has two parts: setting the origin
;;; and the correct clip (to account for other SK8 actors) and then resetting them to their
;;; original values. 

;;; NOTE TO THE BRAVE:
;;; A lot of weird stuff goes on in this macro. Do not mess with the stuff we do about the
;;; origin and the clipping or you'll regret it. If you do want to mess with stuff, make sure
;;; to always keep a copy of the version that worked somewhere. Also make sure you understand
;;; what you are doing before you change this. 

;;; If you are drawing into your window, make sure the port is still around at the end of the
;;; body.

;;; This variable record the port and the object that with-fred-drawing is dealing with.

(defparameter *fred-drawing-state* nil)

(defmacro with-fred-drawing ((fredActor portType &optional fred wptr) &body body)
  (let* ((theFredActor (gensym))
         (theFredClosInst (gensym))
         (thisPort (gensym))
         (savedClip (gensym))
         (savedOrigin (gensym))
         (fredOrigin (gensym))
         (fredClip (gensym))
         (newPort (gensym))
         (viewPosition (gensym)))
    `(without-event-processing
       (locally
         (declare (special *fred-drawing-state*))
         (let* ((,theFredActor ,fredActor)
                (,theFredClosInst ,(or fred `(editData ,theFredActor)))
                (,thisPort (or ,wptr (sk8-fred-port ,theFredClosInst ,portType))))
           (if (eq ,thisPort (car *fred-drawing-state*))
             ;; Nothing to do, everything is set up just right.
             (when (eq ,theFredActor (cdr *fred-drawing-state*))
               (progn ,@body)) ;; Note that the case when the port is the same and the actor isn't is ignored.
             ;; ports not the same? Do all the work.
             (gs:let+ ((*fred-drawing-state* (cons ,thisPort ,theFredActor))
                       (,viewPosition (slot-value ,theFredClosInst 'view-Position))
                       (,savedClip (:region))
                       (,fredClip (:region))
                       ;; We bind *CURRENT-VIEW* so our origin gets correctly restored (via our focus-view method)
                       ;; when our WITH-FRED-DRAWING body is interrupted and then resumed by MCL's view system
                       (*CURRENT-VIEW* ,theFredClosInst)
                       ,savedOrigin ,fredOrigin ,newPort)
               (declare (special *fred-drawing-state*))
               ;; Do the drawing.
               (when ,thisPort
                 (with-port ,thisPort
                   (setf ,savedOrigin (gs::port-origin ,thisPort))
                   (setf ,fredOrigin (subtract-points ,savedOrigin ,viewPosition))
                   (require-trap #_GetClip ,savedClip)
                   ;; Compute the clip. 
                   (if (eq ,thisPort (sk8-fred-port ,theFredClosInst ,portType))
                     ;; If drawing onto its window, use the ownedRegion. 
                     (require-trap #_CopyRgn (ownedRegion ,theFredActor) ,fredClip)
                     ;; Otherwise, use the intersection between the clip of the current port and the fillRegion.
                     (progn
                       (gs::recompute-fill-region ,theFredActor (gs::node-flags ,theFredActor))
                       (require-trap #_SectRgn ,savedClip (gs::node-fillRegion ,theFredActor) ,fredClip)))
                   (require-trap #_OffsetRgn :ptr ,fredClip :long (subtract-points 0 ,viewPosition))
                   (gs:with-hilitecolor ,thisPort (highlightcolor ,thefredactor)
                     (unwind-protect
                       (progn
                         (require-trap #_SetClip ,fredClip)
                         (require-trap #_SetOrigin :long ,fredOrigin)
                         (with-RGB (backColor (MCL-color (fillColor ,theFredActor))) 
                           (require-trap #_RGBBackColor backColor))
                         (with-RGB (foreColor (MCL-color (textColor ,theFredActor))) 
                           (require-trap #_RGBForeColor foreColor))
                         ,@body)
                       ;; If drawing into my window, get the port again, just in case
                       ;; it was disposed by whetever perverse thing was done in the body.
                       (if ,wptr
                         (setf ,newPort ,wptr)
                         (setf ,newPort (sk8-fred-port ,theFredClosInst ,portType)))
                       (when (and ,newPort (eq ,newPort ,thisPort))
                         ;; Only do this if the current port is the port whose origin we have changed.
                         ;; (just in case it was switched in the body)
                         (require-trap #_SetPort ,thisPort) 
                         (require-trap #_SetOrigin :long ,savedOrigin)
                         (require-trap #_SetClip ,savedClip)))))))))))))

#|

;;; this one is useful to debug any origin problems.

(defun port-origin (&optional thePort)
  (gs:let+ ((portPtr (:pointer)))
    (unless thePort
      (#_getPort portPtr)
      (setf thePort (%get-ptr portPtr)))
    (sk8-multivals (rref thePort :cGrafPort.portRect.left)
                   (rref thePort :cGrafPort.portRect.top))))

|#

(defun silly-field-force-update (theactor)
  (gs:making-self-dirty (theActor nil nil nil nil (gs:node-boundsRegion theActor) :bounds nil)
    t))

;;; These have to do with updating scroller on idle only.

(defun dirtyScrollers! (theActor)
  (gs:objectDirty! (gs:node-flags theActor) 1))

(defun cleanScrollers! (theActor)
  (gs:objectDirty! (gs:node-flags theActor) 0))

(defun scrollersDirty? (theActor)
  (gs:objectDirty? (gs:node-flags theActor)))

;;; editText-FORCE-UPDATE -- forces the field to redraw itself through the draw loop. Now uses the owned region.
;;;                       The idea is to draw the field into the gWorld and then copy it back to the window.
;;;                       All this happened OUTSIDE of the draw loop.
;;;
;;; NOTE that the field (using the owned region) needs to respect the locking protocol for windows. This
;;; means that no direct drawing should take place when the window is locked. Thus, we only draw
;;; directly when the lock level of the window is below 2.

;;; Now draws into the gWorld and blits into the window.

(define-handler forceRedraw (editText &key forceRedraw updateScrollers)
  (let* ((f (editData me))
         (frec (CCL::frec-arg (frec f))))
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
              (if forceRedraw
                (CCL::frec-draw-contents frec t)
                (CCL::frec-update frec)))
            ;; When there's a gworld, copy the new bits back to the window.
            (when (and gworld wptr)
              (gs:let+ ((visBlitRgn (:region)))
                ;; Only blit the parts of the drawing that are visible (not obscured)
                (#_SectRgn (ownedRegion me) (rref wptr :CGrafport.visRgn) visBlitRgn)
                (gs:with-zero-origin 
                 gWorld
                 (gs:gWorld-to-window gworld clos-window nil visBlitRgn))))))))))
    (when updateScrollers
      (updatePartnerScroller me (partnerVScroller me))
      (updatePartnerScroller me (partnerHScroller me)))))

;;; Keeps the low level field in synch with changes to the boundsRect of the ScriptEditText.
;;;

(define-handler update-field-rect (editText &optional physical stretch)
  (declare (ignore physical stretch))
  (synch-scriptEditText-to-fillRegion me))

;;; 1.0
;;; editText-DRAW -- this is the draw function that ScriptEditText actors use to draw themselves. By the time this
;;;                      is called in the draw loop, the clip region of the field has been calculated. Thus, we save
;;;                      it in the slot ownedRegion of the actor. We call draw-field to draw the innards of the field.

;;; Will draw into the gWorld and then transfer to the window.

(defun editText-draw (theActor destPort draw-region)
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
      ;;   (#_SectRgn effective-rgn gs:*original-dirty-region* effective-rgn))
      (unless (locked theActor)
        (with-fred-drawing (theActor nil f destport)
          ;; If bwin is out of whack, make sure it is updated.
          (update-bwin frec)
          (CCL::frec-draw-contents frec t)
          ))
      )))

(define-handler enteringStage (editText)
  (let* ((f (editData me))
         (frec (frec f)))
    (with-fred-drawing (me nil)
      ;; If bwin is out of whack, make sure it is updated.
      (update-bwin frec))
    (setf (wptr (editData me)) (wptr (gs:node-window me)))
    (call-next-method)))

;;______________________________________________________
;;; 1. Key Handling.
;;______________________________________________________

(define-handler handleKey (EditText key)
  (without-interrupts 
   (gs:with-drawing-lock
     (let* ((clos-window (gs:node-window me))
            (f (editData me)))
       (cond
        ((null clos-window)
         (with-focused-view nil
           (view-key-event-handler f key)))
        (t
         (with-fred-drawing (me nil)
           (view-key-event-handler f key))
         ;; No idea why, but if the CCL::FREC-SHOW-CURSOR happens in the same with-fred-drawing
         ;; as the buffer-insert (in set-line-indentation), then the drawing origin is off!
         (with-fred-drawing (me nil) 
           (CCL::FREC-SHOW-CURSOR (frec f)))
         ;; This gets the changes back into the window. 
         (editText-to-Window me)
         ;; And update the scrollers.
         (updatePartnerScroller me (partnerVScroller me))
         (updatePartnerScroller me (partnerHScroller me))
         ))))))

(defun editText-pageDown (me)
  (let* ((frec (frec (editData me)))
         (lines (ccl::fr.linevec frec))
         (numLines (ccl::fr.numlines frec))
         (startPos (ccl::fr.bwin frec)))
    (dotimes (i numLines)
      (incf startPos (aref lines i)))
    (setf (verticalScroll me) (lineNumber me startPos))
    (setSelection me startPos startPos)
    ))

;;; Subtract numLines from the position of the starting line.

(defun editText-pageUp (me)
  (let* ((frec (frec (editData me)))
         (numLines (ccl::fr.numlines frec))
         (startPos (ccl::fr.bwin frec))
         (targetLine (max (- (lineNumber me startPos) numLines) 0)))
    ;; scroll to the target line.
    (setf (verticalScroll me) targetLine)
    ;; Set the selection to the first char visible.
    (setSelection me (ccl::fr.bwin frec) (ccl::fr.bwin frec))))
         
;;; KEYDOWN -- calls handle-key to process the key.

(define-handler keyDown (editText key)
  (unless (lockedText me)
    (require-type key Character)
    (case key
      (#\NewLine (returnInField me))
      (#\Enter   (enterInField me))
      (#\Tab     (tabInField me))
      (#\Home    
       (setSelection me 0 0)
       (showSelection me))
      (#\End
       (let ((bufsize (buffer-size (fred-buffer (editData me)))))
         (setSelection me bufSize bufSize)
         (showSelection me)))
      (#\PageDown (editText-pageDown me))
      (#\PageUp (editText-pageUp me))
      (otherwise
       (unless (locked me) (handleKey me key))))))

(define-handler returnInField (editText)
  (unless (locked me)
    (handleKey me #\Return)))

(define-handler enterInField (editText)
  (unless (locked me)
    (handleKey me #\Enter)))

(define-handler tabInField (editText)
  (if (and (autotab me) (not (option-key-p)))
    (tabToNextActor me :direction (if (shift-key-p) 'backward 'forward))
    (handleKey me #\Tab)))

(define-handler autoKey (editText key)
  (keyDown me key))

;;______________________________________________________
;;; 2. Selection Handling and Programatic Text.
;;______________________________________________________

;;; SELECTION -- returns the boundaries of the selection of the field.

(define-handler selection (editText &key)
  (multiple-value-bind (start end) (CCL::frec-get-sel (frec (editData me)))
    (if (<= start end)
      (SK8-multivals start end)
      (SK8-multivals end start))))

;;; SETSELECTION -- sets the selected text of me to the text between start-index and
;;;               end-index.

(define-handler setSelection (editText start end)
  (let* ((frec (frec (editData me)))
         (buf (fred-buffer (editData me)))
         (bufSize (buffer-size buf)))
    (when (and (numberp start) (or (< start 0) (>= start bufSize))) (setq start t))
    (when (and (numberp end) (or (< end 0) (>= end bufSize))) (setq end t))
    (with-fred-drawing (me nilF)
      (CCL::frec-set-sel frec start end)
      (CCL::frec-update frec))
    (forceRedraw me :forceRedraw t)))

(define-handler (setf selection) (newSelection EditText)
  (definitely-recover-list newSelection)
  (sk8-destructuring-bind (start end) newSelection
    (setSelection me start end)))

;;; Returns the selected text.

(define-handler selectedItems (editText)
  (sk8-multival-bind (start end) (selection me)
    (text me :start start :end end)))

;;; Replaces the selected text.

(define-handler (setf selectedItems) (newItems editText)
  (sk8-multival-bind (start end) (selection me)
    (setf (text me :start start :end end) newItems)))

(define-handler SelectAll (editText) (setSelection me 0 -1))

(define-handler showSelection (editText &key index)
  (let ((frec (frec (editData me))))
    (multiple-value-bind (s e) (CCL::frec-get-sel frec)
      (with-fred-drawing (me nil)
        (CCL::frec-show-cursor frec (or index (min s e))))
      (forceRedraw me))))

(define-handler text (editText &key start end)
  (buffer-substring (fred-buffer (editData me)) (or start 0) (or end t)))

(define-handler (setf text) (theText editText &key start end undo)
  (let ((f (editData me)))
    (unless start (setq start 0))
    (unless end (setq end (buffer-size (fred-buffer f))))
    (if undo
      (ed-fred-replace-range-with-undo f theText start end)
      (ed-fred-replace-range f theText start end))
    (forceRedraw me :updateScrollers t)
    theText))

;;; _______________________________ 
;;; Font Manipulation
;;; _______________________________ 

;;; This function is called to change all font related things in Fred. If start and end are
;;; not provided, the empty buffer font is affected too. 

(defun set-editText-font-spec (edittextt font-spec start end)
  (let* ((f (editData edittextt))
         (buf (fred-buffer f))
         (bufSize (buffer-size buf))
         (affects-whole-buffer? 
          (or (and (null start) (null end))
              (and (zerop start) (and end (= end bufSize))))))
    ;; If the buffer is empty or the change affects everything, change
    ;; the buffer empty font to the font desired.
    (when (or (zerop bufsize) (null start) (null end))
      (multiple-value-bind (ff ms) (font-codes font-spec)
        (ccl::set-buffer-empty-font-codes buf ff ms)))
    ;; Make start and end be positions.
    (unless start (setf start 0))
    (unless end (setf end bufSize))
    ;; Now change the font in the range specified. 
    (buffer-set-font-spec buf font-spec start end)
    ;; For some reason, the buffer code does not recognize the case when the
    ;; font of the whole buffer is being changed. In this case we can get rid of
    ;; all the old font information: the font list and the fontruns vector.
    (when affects-whole-buffer?
      (let ((theBuffer (ccl::mark.buffer buf))
            (newFlist (make-array 2))
            (newFontruns (make-array 1 :element-type '(signed-byte 32)
                                     :initial-element 0)))
        (multiple-value-bind (ff ms) (font-codes font-spec)
          (setf (aref newflist 0) ff
                (aref newflist 1) ms))
        (setf (ccl::bf.flist theBuffer) newflist)
        (setf (ccl::bf.fontruns theBuffer) newFontruns)
        (ccl::bf-efont theBuffer ccl::%no-font-index)
        (ccl::bf-cfont theBuffer 0)))
    ;; And redraw.
    (forceRedraw edittextt :updateScrollers t)
    font-spec))

;;;  The aspectNumber is one of the following:   0 = font, 1 = size, 2 = style
;;;

(defun get-editText-style-aspect (self aspectNumber start end)
  (declare (ignore end))
  (let ((buf (fred-buffer (editData self))))
    (multiple-value-bind (ff ms) (if (eql 0 (buffer-size buf))
                                   (CCL::buffer-font-codes buf)
                                   (CCL::buffer-char-font-codes buf (or start 0)))
      (nth-value aspectNumber (CCL::font-values ff ms)))))

;;; Returns 4 values: font, size, mode and styles.
;;; Leaves the parsing of the fontSpec to MCL's function.

(defun merge-with-current-font-spec (theEditText value component start)
  (let* ((theBuffer (fred-buffer (editData theEditText)))
         (theSpec (buffer-char-font-spec theBuffer (or start (buffer-position theBuffer)))))
    (multiple-value-bind (ff ms) (font-codes theSpec)
      (multiple-value-bind (fontName size mode styles colorIndex) (ccl::font-values ff ms)
        (case component
          (:name (append (list value size mode) 
                         (if (listp styles) styles (list styles))
                         (list (list :color-index colorIndex))))
          (:size (append (list fontName value mode)
                         (if (listp styles) styles (list styles))
                         (list (list :color-index colorIndex))))
          (:mode (append (list fontName size value)
                         (if (listp styles) styles (list styles))
                         (list (list :color-index colorIndex))))
          (:style (append (list fontName size mode)
                          (if (listp value) value (list value))
                          (list (list :color-index colorIndex))))
          (:color (append (list fontName size mode)
                          (if (listp styles) styles (list styles))
                          (list (list :color value)))))))))
                       
(define-handler textFont (editText &key start end)
  (let* ((theName (get-editText-style-aspect me 0 start end))
         (theSymbol (read-from-string theName)))
    (if (and (boundp theSymbol)
             (inheritsFrom (symbol-value theSymbol) Font))
      (symbol-value theSymbol)
      (find theName (knownChildren Font) :key #'fontName :test #'string-equal))))

(define-handler (setf textFont) (thefont editText &key start end)
  (unless (stringp theFont) (setf theFont (fontName theFont)))
  (set-edittext-font-spec 
   me (merge-with-current-font-spec me theFont :name start) start end)
  theFont)

(define-handler previousFontChange (EditText position)
  (buffer-previous-font-change (fred-buffer (EditData me)) position))

(define-handler nextFontChange (EditText position)
  (buffer-next-font-change (fred-buffer (EditData me)) position))

(define-handler textSize (editText &key start end)
  (let ((theSize (get-editText-style-aspect me 1 start end)))
    (if (= thesize 0)
      12
      theSize)))

(define-handler (setf textSize) (size editText &key start end)
  (set-edittext-font-spec me (merge-with-current-font-spec me size :size start) start end)
  size)

(define-handler textStyle (editText &key start end)
  (let ((styles (get-editText-style-aspect me 3 start end)))
    (unless (listp styles)
      (setf styles (list styles)))
    (map-into styles #'(lambda (aStyle) (intern (symbol-name aStyle) :sk8)) styles)))

;;; Turns everything in styleList into the corresponding keyword without consing!!!

(defun parse-stylelist (styleList)
  (map-Into styleList #'ccl::make-keyword styleList))

;;; This one has to cons because we want to return to the user the styles, and the
;;; internal representation is different.

;;; This one has to cons because we want to return to the user the styles, and the
;;; internal representation is different.

(define-handler (setf textStyle) (styleList editText &key start end)
  (let ((newStyles (parse-styleList (copy-list styleList)))
        dummyStyles)
    (withActorLocked (me)
      (unless (equal styleList '(:plain))
        ;; PUAJ! Since Fred always merges with the current font, need to set the
        ;; style to plain before doing anything.
        (setf dummyStyles (merge-with-current-font-spec me '(:plain) :style start))
        (set-edittext-font-spec me dummyStyles start end)
        ;; Now build a new font spec from the values.
        (setf newStyles (merge-with-current-font-spec me newStyles :style start))
        (set-edittext-font-spec me newStyles start end)))
    styleList))

;;______________________________________________________
;;; 3. Customization.
;;______________________________________________________

(define-handler wrapping (editText)
  (let ((myFred (editData me)))
    (cond ((ccl::fred-word-wrap-p myFred) 'word)
          ((ccl::fred-wrap-p myFred) 'character)
          (t nil))))

(define-handler (setf wrapping) (wrapType editText)
  (let ((myFred (editData me)))
    (case wrapType
      (word 
       (setf (ccl::fred-word-wrap-p myFred) t)
       (forceRedraw me :forceRedraw t))
      (character 
       (setf (ccl::fred-word-wrap-p myFred) nil)
       (setf (ccl::fred-wrap-p myFred) t)
       (forceRedraw me :forceRedraw t))
      (otherwise 
       (if (null wrapType)
         (progn (setf (ccl::fred-word-wrap-p myFred) nil)
                (forceRedraw me :forceRedraw t))
         (sk8-error PropertyTypeMismatchError
                    :object        wrapType
                    :expectedType  '(nil word character)
                    :ownerObject   EditText
                    :propertyName 'wrapping
                    ))))
    wrapType))
  
(define-handler justification (editText)
  (case (ccl::fred-justification (editData me))
    (:left 'left)
    (:right 'right)
    (:center 'center)))

(define-handler (setf justification) (justificationOption editText)
  (let ((fred (editData me)))
    (case justificationOption
      (left (setf (ccl::fred-justification fred) :left)
            (forceRedraw me :forceRedraw t))
      (right (setf (ccl::fred-justification fred) :right)
             (forceRedraw me :forceRedraw t))
      (center (setf (ccl::fred-justification fred) :center)
              (forceRedraw me :forceRedraw t))
      (otherwise (sk8-error PropertyTypeMismatchError
                            :object        justificationOption
                            :expectedType  '(left right center)
                            :ownerObject   EditText
                            :propertyName 'justification
                            )))
    justificationOption))

(define-handler tabLength (editText)
  (fred-tabcount (editData me)))

(define-handler (setf tabLength) (size editText)
  (setf (fred-tabcount (editData me)) size))

(define-handler leftMargin (editText)
  (ccl::fr.margin (frec (editData me))))

(define-handler (setf leftMargin) (newValue editText)
  (unless (integerp newValue)
    (sk8-error typeMismatchError :object newValue :expectedType integer)) 
  (setf (ccl::fr.margin (frec (editData me))) newValue)
  (forceRedraw me :forceRedraw t))

;;______________________________________________________
;;; 4. Locking and Activation.
;;______________________________________________________

;;; We do it this way because the user expects a boolean to be returned.

(define-handler lockedText (editText)
  (let ((level (lockTextLevel me)))
    (if level
      t
      nil)))
        
;;; 1.0
;;; DEACTIVATETEXT -- called when an actor ceases to be the current key of its window. The effect is to
;;;                 make the cursor dissappear and make the selected text, if any, show
;;;                 up as an outline.

;;; EditText should only get idle events when active!!!

(define-handler deactivateText (editText)
  (when (gs:node-window me)
    (setf (wantsIdle me) nil)
    (with-fred-drawing (me nil)
      (CCL::FREC-DEACTIVATE (frec (editData me))))
    (forceRedraw me :forceRedraw t)))

;;; 1.0
;;; ACTIVATETEXT -- called when self becomes the current key. It makes the cursor show up and it
;;;               highlights the selected text if any.

(define-handler activateText (editText)
  (unless (lockedText me)
    (setf (wantsIdle me) t)
    (with-fred-drawing (me nil)
      (CCL::FREC-ACTIVATE (frec (editData me))))
    (forceRedraw me)))

;;; ACTIVE -- returns t if field is active, else nil

(define-handler active (editText)
  (not (ccl::fr.frame-sel-p (frec (editData me)))))

;;; UNLOCKText -- decreases the lock level of the field by 1. When the level reaches 0, the field is
;;;          really unlocked and graphical changes show in the window.

(define-handler unlockText (editText &key force)
  (let ((lockVal (locktextlevel me)))
    (when lockVal
      (if force 
        (setf lockval nil)
        (progn (decf lockVal)
               (when (zerop lockVal)
                 (setq lockVal nil))))
      (setf (locktextlevel me) lockVal)
      (unless lockVal
        (forceRedraw me :forceRedraw t :updateScrollers t)))
    lockVal))

(define-handler lockText (editText)
  (let ((lockVal (locktextlevel me)))
    (if lockVal
      (incf lockVal)
      (progn
        (setq lockVal 1)
        (deactivateText me))
      )
    (setf (locktextlevel me) lockVal)))

(define-handler lockedText (editText)
  (when (lockTextLevel me)
    t))

;;______________________________________________________
;;; 5. Clipboard.
;;______________________________________________________

;; the hidden keyword is to make the argument list compatible with the undo methods already defined in the editor by MCL`
(define-handler undo :hidden (editText)
  (undo (editData me))
  (forceRedraw me :updateScrollers t))

(define-handler cutSelectionToClipboard (editText)
  (let ((f (editData me)))
    (multiple-value-bind (s e) (CCL::frec-get-sel (frec f))
      (unless (eql s e)
        (addToClipboard (buffer-substring (fred-buffer f) s e) SK8Clipboard :copy nil)
        (ed-fred-replace-range-with-undo f nil s e)
        (forceRedraw me :updateScrollers t)))))

(define-handler copySelectionToClipboard (editText)
  (let ((f (editData me)))
    (multiple-value-bind (s e) (CCL::frec-get-sel (frec f))
      (unless (eql s e)
        (addToClipboard (buffer-substring (fred-buffer f) s e) SK8Clipboard)))))

(define-handler pasteClipboardToSelection (editText)
  (let ((newText (car (getFromClipboard SK8Clipboard String sk8))))
    (when newText
      (let ((f (editData me)))
        (multiple-value-bind (s e) (CCL::frec-get-sel (frec f))
          (ed-fred-replace-range-with-undo f newText s e)
          (forceRedraw me :updateScrollers t))))))

(define-handler clearSelection (editText) 
  (let ((f (editData me)))
    (multiple-value-bind (s e) (CCL::frec-get-sel (frec f))
      (unless (eql s e)
        (ed-fred-replace-range-with-undo f nil s e)
        (forceRedraw me :updateScrollers t)))))

;;______________________________________________________
;;; 6. Informational.
;;______________________________________________________

(define-handler size (editText &key physical how)
  (case how
    (characters (buffer-size (fred-buffer (editData me))))
    (lines (lineNumber me (buffer-size (fred-buffer (editData me)))))
    (otherwise (call-next-method me :physical physical))))

;;; Important to note that this handler returns the number of lines of text currently
;;; visible!!! (It does not tell you how many lines fit since you cannot know when there
;;; are visible fonts and lineheight vary). Vector ends with a 0 or a false!

(define-handler visibleLines (editText)
  (let ((result 0))
    (dovector (i (ccl::fr.linevec (frec (editData me))))
      (when (or (eq i 0) (null i))
        (return))
      (incf result))
    result))

;;; Given a position in the text, returns the real line number.

(define-handler lineNumber (editText position)
  ;; When this field is not on a window, we will need to call
  ;; frec-update to be able to return things that make sense here.
  (unless (gs:node-window me)
    (ccl::frec-update (frec (editData me))))
  (if (wrapping me)
    (multiple-value-bind (lineNum lineStartPos)
                         (sk8-frec-screen-line-num (frec (editData me)) position)
      (declare (ignore lineStartPos))
      linenum)
    (buffer-line (fred-buffer (editData me)) position)))

;;; translates physical coords h and v into Fred coords. 

(defun physical-to-fred (field h v)
  (sk8-multival-bind (logh logv) (physicalToLogical field h v)
    ;; and we have to round and subtract the frameSize.
    (sk8-multival-bind (frameH frameV) (frameSize field)
      (make-point (gs:f.round (- logH frameH)) (gs:f.round (- logV frameV))))))

(defun fred-mouse-pos (field h v)
  (let* ((edFred (editData field))
         (mousePos (physical-to-fred field h v)))
    (fred-point-position edFred mousePos)))

(define-handler wordUnderMouse (editText)
  (let* ((edFred (editData me))
         (buf (fred-buffer edFred))
         (mousePos (focusing-fred-on-its-window
                     (view-mouse-position edFred)))
         (charPos (fred-point-position edFred mousePos)))
    (multiple-value-bind (start end) (buffer-word-bounds buf charPos)
      (buffer-substring buf start end))))

(define-handler pointOnWhichPart (editText h v &key (part 'bounds))
  (case part
    (sk8::character (fred-mouse-pos me h v))
    (line (let ((thePos (fred-mouse-pos me h v)))
            (lineNumber me thePos)))
    (otherwise (call-next-method))))

;;; Returns the size the field should have to show all its text. If the field is not wrapping,
;;; returns a width different from the current one.

(define-handler actorTextSize (EditText &key theText)
  (declare (ignore theText))
  (let ((hDisplacement 4)  ; These vars add a little white space.
        (vDisplacement 4))
    (multiple-value-bind (newWidth newHeight)
                         (sk8-frec-size (frec (editData me)))
      (sk8-multival-bind (fh fv) (frameSize me :physical t)
        (if (wrapping me)
          (sk8-multivals (width me :physical t)
                         (+ newHeight (* 2 fv) vDisplacement))
          (sk8-multivals (+ newWidth (* 2 fh) hDisplacement)
                         (+ newHeight (* 2 fv) vDisplacement)))))))


;;______________________________________________________
;;; 7. MouseHandling.
;;______________________________________________________

(defmacro without-frec-drawing (frec &body body)
  `(unwind-protect
     (progn
       (setf (ccl::fr.nodrawing-p ,frec) t)
       ,@body)
     (setf (ccl::fr.nodrawing-p ,frec) nil)))

;;; IDLE -- Pass idle events down to the field-code so that it can flash the caret.

(define-handler idle (editText)
  ;; Do normal idle behavior
  (let* ((clos-window (gs:node-window me))
         (f (editData me)))
    ;; Update bwin if needed.
    (with-fred-port f
      (update-bwin (frec f)))
    (when (scrollersDirty? me)
      (updatePartnerScroller me (partnerVScroller me) :force t)
      (updatePartnerScroller me (partnerHScroller me) :force t))
    (when (and clos-window (not (locked me)) (deeplyVisible me) 
               (null (lockLevel (sk8::window me))))
      ;; If the updateRgn is not empty, we might need to update the field.
      (let ((frec (frec f)))
        (when (and (ccl::fr.updateRgn frec) (not (#_emptyRgn (ccl::fr.updateRgn frec))))
          (forceRedraw me :updateScrollers nil :forceRedraw t)
          ;; If forceRedraw does not clear the updateRgn, we do it ourselves to
          ;; avoid doing this all the time. 
          (#_setEmptyRgn (ccl::fr.updateRgn frec))))
      ;; Do normal idle thing.
      (gs:recompute-fill-region me (gs:node-flags me))
      (with-fred-drawing (me nil)
        (CCL::frec-idle (CCL::frec-arg (frec f))))
      (editText-cursor-to-window me)
      )))

(defun fred-click (me clos-window actionProc)
  (declare (ignore clos-window))
  ;; Updating the cursor seems to fix frec's state so that
  ;; it draws correctly in the space alloted to it in the window.
  ;; We do not know why this is. 
  (setf (cursor Stage) ibeamCursor)
  ;; All the live selection is done on the window. 
  (let* ((f (editData me))
         (frec (frec f))
         (frec-old-hScroll (ccl::fr.hScroll frec)))
    (focusing-fred-on-its-window
      (with-fred-drawing (me :wptr)
        (CCL::frec-click frec 
                         (subtract-points gs:*event-location* (view-position f))
                         #'(lambda ()
                             (if (eql (ccl::fr.hScroll frec) frec-old-hScroll)
                               (ccl::frec-update frec)
                               (progn
                                 (setf frec-old-hScroll (ccl::fr.hScroll frec))
                                 (ccl::frec-draw-contents frec)))
                             ;; (funcall actionProc)
                             ))))
    (funcall actionProc)))

;;; MOUSEDOWN -- the mouseDown method for fields. Sets the current key of its window to
;;;             itself and calls handle-mousedown to deal with selecting text and
;;;             placing the cursor.

(define-handler mouseDown (editText)
  (cond
   ((or (locked me) (lockedText me) (neq me (eventActor)))
    ;; propagate to container.
    (let ((container (container me))) 
      (when container (mouseDown container))))
   (t
    (let ((clos-window (gs:node-window me)))
      ;; propagate to container.
      (let ((container (container me))) 
        (when container (mouseDown container)))
      (when clos-window
        (let* ((topLevelActor (slot-value clos-window 'gs:my-actor-object))
               (f (editData me))
               (frec (frec f))
               (curLine (buffer-line (ccl::fr.wposm frec)))
               (curHscroll (ccl::fr.hscroll frec))
               (*in-scriptEditText-mouseDown* me)
               (hScroller (partnerHScroller me))
               (vScroller (partnerVScroller me)))
          (declare (special *in-scriptEditText-mouseDown*))
          (unless (eq (keyTarget topLevelActor) me)
            (setf (keyTarget topLevelActor) me))
          (flet ((update-scrollers ()
                   (unless (eql curLine (setq curLine (buffer-line (ccl::fr.wposm frec))))
                     (with-focused-view nil 
                       (updatePartnerScroller me vScroller)))
                   (unless (eql curHscroll (setq curHscroll (ccl::fr.hscroll frec)))
                     (with-focused-view nil 
                       (updatePartnerScroller me hScroller)))))
            (declare (dynamic-extent #'update-scrollers))
            ;; All the live selection is done on the window. 
            (fred-click me clos-window #'update-Scrollers))
          ;; Ok. Now copy the resulting state of the field onto the gWorld.
          (editText-to-gWorld me)
          (set-fred-last-command f nil))
        ;; Restore the event vars and dispatch a mouseUp.
        (gs:update-event-vars)
        (mouseUp me)
        )))))

;;; 1.0
;;; MOUSEENTER -- changes the cursor when the field is entered. Note that the cursor does not change if
;;;              the field is locked.

(define-handler mouseEnter (editText)
  (unless (lockedtext me)
    (setf (cursor stage) ibeamCursor)))

;;; 1.0
;;; MOUSELEAVE -- changes the cursor when the field is exited.

(define-handler mouseLeave (editText)
  (setf (cursor stage) standardCursor))

;;______________________________________________________
;;; 8. Scrolling.
;;______________________________________________________

;;; Returns line numbers by default. I do not understand why we need the
;;; other option...

(define-handler verticalScroll (editText &key (byLine t))
  (let ((frec (frec (editData me))))
    (cond (byLine (lineNumber me (ccl::fr.bwin frec)))
          (t      (buffer-position (ccl::fr.wposm frec))))))

;;; Only accepts line numbers for scrolling.

(define-handler (setf verticalScroll) (newValue editText)
  (let ((frec (frec (editData me)))
        (needUpdate? nil)
        newScroll)
    (setq newValue (round newValue))
    ;; Vertical Scroll is done by changing the position of the mark.
    (update-bwin frec)
    (setq newScroll 
          (with-fred-port (ccl::fr.owner frec)
            (CCL::frec-screen-line-start frec 0 newValue)))
    (unless (eql newScroll (buffer-position (ccl::fr.wposm frec)))
      (setq needUpdate? t)
      (set-mark (ccl::fr.wposm frec) newScroll))
    (when needUpdate? (forceRedraw me))
    newScroll))

(define-handler horizontalScroll (editText)
  (ccl::fr.hscroll (frec (editData me))))

(define-handler (setf horizontalScroll) (newValue editText)
  (let ((frec (frec (editData me)))
        (needUpdate? nil))
    (setq newValue (round newValue))
    ;; Horizontal. Done by changing fr.hscroll.
    (unless (and (fixnump newValue) (<= 0 (the fixnum newValue) 32767))
      (setf newValue 0))
    (unless (eql newValue (ccl::fr.hscroll frec))
      (setq needUpdate? t)
      (setf (ccl::fr.hscroll frec) newValue))
    (when needUpdate?
      (forceRedraw me :forceRedraw t))
    newValue))

(define-handler partnerScroller (editText orientation)
  (if (eq orientation 'vertical)
    (partnerVScroller me)
    (partnerHScroller me)))

(define-handler (setf partnerScroller) (theScroller editText &key (affixing t) (orientation 'vertical))
  (let* ((myScrollerProperty (if (eq orientation 'vertical)  'partnerVScroller 'partnerHScroller))
         (wiredProperty      (if (eq orientation 'vertical)  'verticalScroll   'horizontalScroll))
         (oldScroller (funcall myScrollerProperty me)))
    ;; Unaffix the old one if necessary.
    (when (and oldScroller (memq oldScroller (affixedScrollers me)))
      (setf (affixedScrollers me) (remove oldScroller (affixedScrollers me))))
    ;; Do the main work.
    (set-partnerScroller me wiredProperty myScrollerProperty theScroller)
    ;; Affix the new one if necessary.
    (when (and theScroller affixing)
      (setf (affixedScrollers me) (cons theScroller (affixedScrollers me)))
      (resized me))
    theScroller))

(define-handler (setf partnerVscroller) (theScroller editText &key (affixing t))
  (setf (partnerScroller me :orientation 'vertical :affixing affixing) theScroller))

(define-handler (setf partnerHscroller) (theScroller editText &key (affixing t))
  (setf (partnerScroller me :orientation 'horizontal :affixing affixing) theScroller))

;;; Note that this is TERRIBLY SLOW when wrapping is on!!! Maybe we should update the
;;; scrollers on idle... How about that? Problem is that we have to compute the REAL
;;; line number using frec and it is very slow!

(define-handler updatePartnerScroller (editText theScroller &key force)
  ;; If the window is not visible yet, mark scroller dirty because update-bwin
  ;; will yield incorrect results. 
  (if (and (gs:node-window me) 
           (window-shown-p (gs:node-window me))
           (or force (not (wrapping me))))
    (when theScroller
      (let* ((f (editData me))
             (frec (frec f))
             (orientation (orientation theScroller))
             (val (currentValue theScroller))
             (thumb (thumbView theScroller))
             (max (maximumValue theScroller))
             (min (minimumValue theScroller))
             (scrollStep (scrollStep theScroller))
             (pageStep (pageStep theScroller))
             (needsRescaling? nil)
             (needsUpdating? nil))
        (update-bwin frec)
        (cond
         ;; Vertical: scrolling by line.
         ((eq orientation 'vertical) 
          (let* (;; Important: if fred is not on a window, fr.bwin has nonsense in it!
                 (firstVisibleCharacter (if (wptr f) (ccl::fr.bwin frec) 0)) 
                 (topLine (sk8-frec-screen-line-num frec firstVisibleCharacter))
                 (view (visibleLines me))
                 (screenLines (sk8-frec-screen-line-num frec (buffer-size (fred-buffer f)))))
            (while-scrolling theScroller
              ;; Minimum.
              (unless (eq min 0) 
                (setf (minimumValue theScroller) 0)
                (setf needsRescaling? t))
              ;; ScrollStep.
              (unless (eq scrollStep 1) (setf (scrollStep theScroller) 1))
              ;; Current Value.
              (unless (eq val topLine) 
                (setf needsUpdating? t)
                (setf (currentValue theScroller) topLine))
              ;; Thumb view.
              (unless (eq thumb view) 
                (setf (thumbview theScroller) view)
                (setf needsRescaling? t))
              ;; Page step.
              (unless (eq pageStep view) (setf (pageStep theScroller) view))
              ;; Maximum.
              (unless (eq max screenLines) 
                (setf (maximumValue theScroller) screenLines)
                (setf needsRescaling? t)))))
         ;; Horizontal: scrolling by pixel.
         (t
          (let* ((hscroll (ccl::fr.hscroll frec))
                 (view (the fixnum (point-h (ccl::fr.size frec))))
                 (halfWidth (ash view -1)))
            (while-scrolling theScroller
              (unless (eql min 0)                    
                (setf needsRescaling? t  (minimumValue theScroller) 0))
              (unless (eql max 1023)                 
                (setf needsRescaling? t  (maximumValue theScroller) 1023))  ; don't ask -- that's what MCL uses
              (unless (eql val hscroll)              
                (setf needsUpdating? t   (currentValue theScroller) hscroll))
              (unless (eql scrollStep 6)             
                (setf (scrollStep theScroller) 6))
              (unless (eql thumb view)
                (setf (thumbview theScroller) view)
                (setf needsRescaling? t))
              (unless (eql pageStep halfWidth)       
                (setf (pageStep theScroller) halfWidth))))))
        (if needsRescaling? 
          (rescaleThumb theScroller)
          (when needsUpdating? (update theScroller)))
        (cleanScrollers! me)))
    (dirtyScrollers! me)))

;;; _______________________________ 
;;; Color for the Field!!!
;;; _______________________________ 

;;; The property holds a key called :colors which holds in a list, the form is an alist
;;; whose key is the color index assigned to the color. 

(defun colors (theField)
  (getf (gs:node-properties theField) :colors))

;;; Returns the color with the specified mcl-color in the colors of the field.

(defun findColor (theField theIndex)
  (let ((theColors (colors theField)))
    (cadr (assoc theIndex theColors))))

(defun buffer-char-font-index (buffer &optional pos)
  (let* ((fontSpec (buffer-char-font-spec Buffer (or pos (buffer-position buffer))))
         (colorInfo (car (last fontSpec))))
    (if (and (listp colorInfo) (eq (car colorInfo) :color-index))
      (cadr colorInfo)
      0)))

;;; Ask Fred about the color and then look it up in the colors.

(define-handler textColor (EditText &key start end)
  (declare (ignore end))
  ;; Now that we have the index we look it up in the colors of the field.
  (or (findColor me (buffer-char-font-index (fred-buffer (editData me)) (or start 0))) 
      Black))

;;; If start and end are not supplied, the colors list is bashed.

(define-handler (setf textColor) (newValue EditText &key start end)
  ;; Make sure it is an RGB.
  (unless (inheritsFrom newValue RGBColor)
    (sk8-error PropertyTypeMismatchError
               :object        newValue
               :expectedType  RGBColor
               :ownerObject   EditText
               :propertyName 'textColor
               ))
  ;; If start and end are not provided, nuke the colors list.
  (unless (and start end)
    (setf (getf (gs:node-properties me) :colors) nil))
  ;; Make the real, FRED change.
  (set-edittext-font-spec 
   me (merge-with-current-font-spec me (mcl-color newValue) :color start) start end)
  (let ((theIndex (buffer-char-font-index (fred-buffer (editData me)) start)))
    ;; If the color is not found in the colors, add it in.
    (unless (findColor me theIndex)
      (setf (getf (gs:node-properties me) :colors)
            (pushnew (list theIndex newValue) (getf (gs:node-properties me) :colors)))))
  ;; Force a redraw.
  (lightForceRedraw me))
  
;;; _______________________________ 
;;; The End! Settings...
;;; _______________________________ 

(setf (wrapping editText) 'word)

;;; _______________________________ 
;;; Preserve and restore.
;;; _______________________________ 

(define-handler preserve (editText)
  ;; Skip all the editText's nonsense
  (preserve-actor me)
  (setf (wptr (editData me)) nil)
  (setf (ccl::fr.updateRgn (frec (editData me))) nil))

(define-handler restore (editText)
  ;; Skip all the editText's nonsense
  (restore-actor me)
  (let ((clos-window (gs:node-window me))
        (ed-fred (editData me)))
    (setf (ccl::fr.selrgn (frec ed-fred)) (ccl::%new-rgn))
    (setf (wptr ed-fred) (when clos-window (wptr clos-window)))
    (setf (ccl::fr.updateRgn (frec ed-fred)) (ccl::%new-rgn))
    (ccl::frec-delay-cursor-off (frec ed-fred) t)))

(define-handler localVirtualProperties (editText) 
  (when (eq me EditText)
    '(justification horizontalscroll wrapping selection verticalScroll)))

;;; _______________________________ 
;;; InitializeFromStore of EditText
;;; _______________________________ 

;;; Migration Notes:
;;; ---------------

;;; In Beta 3 we introduce MCL 3.0's version of Fred which adds color. We had 
;;; previously added color ourselves by changing the representation of the fontlist in the buffer.
;;; The differences between Beta2's representation and Beta3's representation are:

;;; (1) In B2: a font is represented by a trio of numbers: the ff, the ms and the color.
;;;    In B3: a font is represented by a pair of numbers: the ff and the ms. The color is stored with them in some spare bits.

;;; (2) In B2: we store SK8 Colors in the :colors part of the properties slot. 
;;;         The format of this list is {color1, color2, ... , colorn}
;;;    In B3: the :colors part of the properties slot contains an association list.
;;;         The format is {{index1, color1}, {index2, color2}, ... , {indexn, colorn}}

;;; So, at initializeFromStore time, we decide whether the field we are looking at is of the old type.
;;; If so we remove all color information in both the font list and the color list. For each EditText to whom this happens
;;; a message is printed on the MessageBox. 
;;; ---------------

;;; This function returns t if every color in our color list is present as a third item
;;; in the font list of the buffer.

(defun all-colors-represented? (colors flist)
  (let ((thirdItems nil))
    ;; collect the third items.
    (dotimes (i (length flist))
      (when (zerop (mod (1+ i) 3))
        (push (aref flist i) thirdItems)))
    (dolist (c colors t)
      (unless (memq (mcl-color c) thirdItems)
        (return nil)))))

;;; How to know that we have an old EditText? Check out the comment below.

(defun remove-old-color-spec-if-needed (theEditText)
  (let* ((mark (ccl::mark.buffer (fred-buffer (editData theEditText))))
         (flist (ccl::bf.flist mark))
         (colors (colors theEditText))
         survivingFontCodes)
    ;; We have an old version when:
    ;; (1) the number of things in the font list is odd, OR
    ;; (2) the format of the :colors list is (color1, color2,..., colorn), OR
    ;; (3) every item in the colors list is present as a third item in the list.
    (when (or (oddp (length flist))
              (and colors                                       ;; there are colors.
                   (not (listp (car colors)))                   ;; they are in the old format
                   (zerop (mod (length flist) 3))               ;; flist has 3s
                   (all-colors-represented? colors flist)))    
      ;; We have an old version. Fix the bf.flist by removing every third item.
      (dotimes (i (length flist))
        (unless (zerop (mod (1+ i) 3))
          (setf survivingFontCodes (append survivingFontCodes (list (aref flist i))))))
      ;; Now create a new vector and add it to the buffer.
      (setf (ccl::bf.flist mark) (apply 'ccl::vector survivingFontCodes))
      t
      )))

;;; Modified to call remove-old-color-spec-if-needed and clear old color information.

(define-handler InitializeFromStore (editText)
  (let ((ed-fred (editData me)))
    ;; Make sure the buffer flist is of the right form. 
    (when (remove-old-color-spec-if-needed me)
      ;; Notify the user (only if they had messed with the textColor).
      (when (colors me)
        (sendToLog (format nil "Migrating from Beta 2 to Beta 3, ~a might loose some of its textColor information." me)))
      (remf (gs:node-properties me) :colors))
    ;; Deal with the regions.
    (setf (ccl::fr.selrgn (frec ed-fred)) (ccl::%new-rgn))
    (setf (ccl::fr.updateRgn (frec ed-fred)) (ccl::%new-rgn)))
  (call-next-method))

#|

;;; This is the old version that we may restart using for GM, when there is no need to 
;;; be compatible with projects stored in Beta 2.
  
;;; initializeFromStore ScriptEditText and some CLOS objects that it contains
;;; Mostly it prevents some things from crashing the store which doesn't know what to do with them

(define-handler InitializeFromStore (editText)
  (let ((ed-fred (editData me)))
    (setf (ccl::fr.selrgn (frec ed-fred)) (ccl::%new-rgn))
    (setf (ccl::fr.updateRgn (frec ed-fred)) (ccl::%new-rgn)))
  (call-next-method))

|#

(defmethod InitializeFromStore ((me sk8-fred) &key)
  (declare (ignore me)))

;;; These ones should only be defined if the store is available. 

(when (memq :store cl-user::*features-wanted*)

  (defmethod NOT-IMMEDIATELY-SETTABLE? ((prop-value sk8-fred)
                                             (SK8-info-rec CodeGenerator-SK8-object-info-class)
                                             (cg CodeGenerator))
    (let ((myact (slot-value prop-value 'actor))
          (mysize (view-size prop-value)))
      (or (not-immediately-settable? myact SK8-info-rec cg)
          (not-immediately-settable? mysize SK8-info-rec cg)
          (not-immediately-settable? (baseparent myact) SK8-info-rec cg))))
  
  (defmethod CREATE-VALUE-STRING ((me sk8-fred) (cg SK8Script-CodeGenerator))
    (let ((myact (gen-value-string (slot-value me 'actor) cg))
          (mysize (gen-value-string (view-size me) cg))
          (lines (gen-value-string (compile-text-list (fred-buffer me)) cg))
          (styles (gen-value-string (compile-styles-list (fred-buffer me)) cg)))
      (format nil "(|(sk8)new-fred| (the editData of the baseParent of ~a) with newsize ~a with newactor ~a with textLines ~a with styles ~a)"
              myact mysize myact lines styles)))

  (defmethod CREATE-VALUE-STRING ((me sk8-fred) (cg FASL-CodeGenerator))
    (let ((myact (gen-value-string (slot-value me 'actor) cg))
          (mysize (gen-value-string (view-size me) cg))
          (lines (gen-value-string (compile-text-list (fred-buffer me)) cg))
          (styles (gen-value-string (compile-styles-list (fred-buffer me)) cg)))
      
      `(sk8dev::new-fred (sk8dev::editData (baseParent ,myact)) :newsize ,mysize :newactor ,myact :textlines ,lines :styles ,styles)
      ))
)

#|
	Change History (most recent last):
	2	6/1/93	Hernan	Just trying stuff but in the end did not change anything.
	3	6/1/93	Hernan	PartnerScrollers are copied and partnered but are
				only affixed if the originals were!
	4	6/1/93	Hernan	Modified update-field-rect to use data from the
				low level rect (avoids problems with the textfield).
	5	6/10/93	Hernan	InitializeObject now updates the low-level field. 
				This was a problem for fields that are locked forever.
	6	6/11/93	Brian Roddy	
	7	6/15/93	Hernan	InitializeObject looks uses the low level rect to
				compute the size. This avoids problems with the
				textField, who is all screwed up anyway...
	8	6/21/93	Hernan	Just a little research...
	8	6/21/93	Hernan	Second try to check in!
	9	6/21/93	Hernan	SimpleText only passes mouseDown up the containement hierarchy
				when it is not locked!!!
	10	6/22/93	Hernan	unlockText now takes a force argument which
				really unlocks the field. Also, field-force-update
				does not draw if the field is locked.
	11	6/25/93	Hernan	The Great Renaming of 93.
	12	6/25/93	Hernan	InitializeObject -> initialize.
	13	6/26/93	kleiman	(ownedRegion -> (ownsRegion
	14	6/28/93	Hernan	Fixing improper renaming of ownedRegion -> ownsRegion.
	15	7/6/93	chip	
	20	8/31/93	hernan	Complete reimplementation of editText in order
				to increase efficiency and do away with all those
				zillions wptrs.
	21	9/1/93	hernan	The great integrating build for d4!!!
	22	9/3/93	hernan	Making lockText do the right thing: disallow the 
				user from changing the field by direct manipulation.
	23	9/14/93	chip	Defined update-fieldpartners on simpleText
				because it seems to get called somehow... It does
				nothing.
	24	9/22/93	hernan	Fixed update-partnerScroller to work with the
				new improved scroller.
	25	9/22/93	kleiman	initialize-internal
	26	9/28/93	rod	removed getfromuser
	27	10/1/93	hernan	=====================================
				= Sad but true: the node is gone!!! =
				=====================================
	28	10/4/93	kleiman	initialize peek
	29	10/5/93	hernan	with-fast-slots -> with-fast-node-slots.
	30	10/8/93	hernan	PrepareForStage now takes care of setting the
				field's window.
	31	11/1/93	hernan	Use withResourceFile instead of usingResourceFileName...
	32	11/3/93	hernan	Fixing pointOnWhichPart to return the right char index.
	33	11/19/93	hernan	Making editText activate itself when it is moused
				on. This is not happening when the edittext was
				deactivated but is still the keyTarget of its window.
	34	11/29/93	chip	(setf partnerScroller) now uses its orientation arg when creating the mapValue handler; initialize now uses SK8-multival-bind for gs:rect-size
	35	12/3/93	sidney	Allow save and load of projects with ScriptEditText objects
	36	12/5/93	sidney	Move definition of a handler for ScriptEditText to where it loads after the class is defined
	37	12/21/93	hernan	Making sure we have a region before we try to
				dispose it.
	38	12/21/93	hernan	Error checking trap stuff.
	39	1/7/94	hernan	Making editText use the currentCursor property
				to set the cursor of the stage.
	40	1/10/94	hernan	Fonts just became objects!!!
	41	1/10/94	hernan	Stupid typo.
	42	1/11/94	hernan	self -> me
	43	1/14/94	hernan	Now using ports to talk to scrollers!
	44	1/17/94	hernan	Separating out the part of initialize that makes
				the low level field so that it may be called from
				other places.
	45	2/12/94	kleiman	name changes
	46	2/21/94	hernan	The Final Renaming for Alpha! (yeah, right...)
	47	2/21/94	hernan	window -> sk8::window.
	48	2/25/94	hernan	Using symbols instead of keywords for options!!!
	49	2/28/94	hernan	Now using GCable mac pointers and handles!
	50	3/2/94	Hernan	Porting to Fred 3.0
	51	3/3/94	Hernan	The great handler argument name renaming of 94!
	52	3/3/94	kleiman	properties -> addproperty
	53	3/3/94	kleiman	private properties declared via makeprivateproperty
	54	3/4/94	kleiman	ed-fred -> edData
	55	3/6/94	Hernan	Making editText conform with the final API.
	56	3/6/94	chip	now also has Text as a parent
	57	3/6/94	Hernan	Making wrapping editText update scrollers on'
				idle only!
	58	3/6/94	Hernan	EditText and scrollers shall behave!!! (some day...)
	59	3/14/94	Hernan	Making the text attributes be changed properly 
				when the field has no text. Had to define our own
				version of buffer-replace-font-spec which really
				replaces the thing.
	60	3/14/94	kleiman	makePrivateProperty -> (setf (private ...
	61	3/15/94	Hernan	Fixed handlers that change the text style, font
				and size to pass a whole new font spec to the 
				lower level function. The idea is to stop MCL from
				merging fonts since that is not the SK8 way.
	62	3/18/94	Hernan	Making horizontalScroll be 0 when a negative
				number is passed.
	63	3/21/94	sidney	duplicate -> copy
	64	3/24/94	Hernan	Fixing horizontal scrolling. We forgot to update 
				the thumbview in updatePartnerScroller.
	65	3/25/94	Hernan	Fixing thumbview setting problem.
	66	3/29/94	sidney	Fixing scriptEditText-draw to correctly set the
				port before copying to the gWorld. (Hernan)
	67	3/30/94	Hernan	Nothing, just checking in so that this file will
				be recompiled in next build.
	68	3/31/94	Hernan	All calls to low level frec functions have to be
				done within a port (otherwise the current port
				is used and the current port might be the wmgr
				port!!!). This may be the cause of the font problem.
	69	3/31/94	Hernan	Fixing forceRedraw to blit the to the ownedRegion
				(not the fillRegion).
	70	4/1/94	Hernan	Before copy-bits set the foreColor and the
				backColor.
	71	4/5/94	kleiman	code review: made publi api
	72	4/15/94	Hernan	Kludged ed-next-line-pos-wrapped to put the
				cursor in the last character when at the end of
				the buffer.
	73	4/15/94	rod	Fixing HighlightColor for all pickers and edittext...
	74	4/22/94	Hernan	Fixed bug in merge-with-current-font-spec.
	75	4/26/94	Hernan	Removing extraneous print statement.
	76	6/13/94	Hernan	Removing the continuation character from the 
				comtab that editText uses.
	77	7/1/94	Hernan	1169264: adding the set selection handler.
	78	7/13/94	Hernan	The text translator's internalObject is String
				(not Text).
	79	7/13/94	Hernan	1172953: Fixing it!
	80	7/18/94	Hernan	1174339: Since Fred always merges fonts, had to
				kludge set textStyle to set the style to plain
				before changing the style to what the user really
				wants.
	81	7/22/94	chip	main parent of EditText is now EditTextCollection (radar #1171748)
	82	8/5/94	Hernan	1178348: forceRedraw should to the usual actor
				stuff when the editText is a window.
	83	8/5/94	Hernan	Got rid of silly looking references to the
				*scrolling* variable in the code.
	84 	 8/12/94	Hernan  	Changing ed-fred-default-key-handler to let
							you insert tab characters into fields.
	85 	 8/22/94	Hernan  	1181072: textFont of EditText now returns a font 
							object (instead of a string).
	86 	 8/31/94	Hernan  	scrollerOrientation -> orientation.
	87 	 9/ 2/94	Hernan  	1183700: Setting *editText-the-object* to the
							EditText as soon as it is created.
	88 	 9/13/94	till    	Bug 1183148, fix set-editText-style-aspect to 
							set the defaulfont.
	89 	 9/16/94	Hernan  	Making the textFont of EditText be Geneva.
	90 	 9/28/94	Hernan  	EditText now copies its text and draws its contents.
	91 	10/ 3/94	Hernan  	Adding the paper argument to render. (The paper
							is the Port at this point).
	92 	10/ 7/94	chip    	added localCreationRelations to EditText & fixed its initialize as needed
	93 	10/13/94	chip    	updated (setf partnerScroller) to use set-partnerScroller
	94 	10/17/94	dy      	ownedRegion is now in p-list
	95 	10/20/94	chip    	added call-with-temp-string handler
	96 	10/21/94	dy      	incr/decrOwnsRegionCount -> incr/decrementOwnsRegionCount
	97 	11/ 4/94	Hernan  	Adding changes to allow EditText to draw straight
							into the gWorld.
	98 	11/ 5/94	Hernan  	Kludged handleKey to avoid the flashing scrollbar
							problem.
	99 	11/ 4/94	Till    	copy-ed-fred change for color fred.
	100	11/ 4/94	till    	Color fred, sk8-buffer-replace-font-codes.
	101	11/ 4/94	till    	backoff of color for a moment
	102	11/ 8/94	Hernan  	Trying to avoid costly redraws while fixing the
							lingering cursor problem...
	103	11/ 9/94	Hernan  	1198239: wordUnderMouse should ask for the
							view-mouse-position with the port focused on the
							editText's window.
	104	11/10/94	Hernan  	new-fred makes the selection region as well.
	105	11/14/94	Hernan  	1199094: fixing the messagebox colorDepth crash
							by changing with-fred-drawing to resample the
							port before reseting the origin. This has to be done
							because the port might be disposed in the body
							of the macro. The API is changed to tell with-fred-
							drawing which port of the actor we are dealing with.
	106	11/16/94	chip    	just a little arg fix in call-with-temp-string
	107	11/16/94	Hernan  	Redefining view-key-event-handler of sk8-fred to
							allow entering characters via direct calls to
							keyDown.
	108	11/16/94	dy      	move view-key-event-handler after the defclass of sk8-fred
	109	12/ 5/94	rod     	fixing bug in pointonwhichpart.
	110	12/ 9/94	Hernan  	Adding support for Fred color.
	111	12/ 9/94	Hernan  	Making textColor return the color.
	112	12/13/94	Hernan  	FindColor should return nil if the color is not
							found.
	113	12/13/94	till    	typo in setf textstyle
	114	12/13/94	Hernan  	The text font of EditText should be set after the 
							Fred component is set up so that it is copied from
							there and not from the plist (which is where 
							normal actors keep their textFonts).
	115	12/21/94	Hernan  	EditText should send the mouseup event.
	116	 1/17/95	Hernan  	1212106: setf horizontalScroll forces a redraw
							with the forceRedraw argument set to true to 
							force the innards of the field to redraw themselves.
	117	 1/20/95	Hernan  	1170352: adding home, page up, etc. funcionality.
							Also fixing holes in the whole informational part of
							the API. verticalScroll and set verticalScroll will
							both use lines from now on.
	118	 1/24/95	Hernan  	Using with-fast-node-slots where appropriate.
	119	 1/27/95	Hernan  	1207549: before calling mouseUp, the event vars
							are reset to the current state.
	120	 2/ 2/95	Hernan  	1169263: setSelection now causes an expensive redraw.
							Who knows why the caret behaves in such funny (strange)
							ways...
	121	 2/ 6/95	Hernan  	1215052: Keydown makes sure the thing given to it is a 
							character.
	122	 2/ 6/95	Hernan  	Fixing the previous comment to say the right thing.
	123	 2/ 6/95	Hernan  	mouseDown passes the event along when the field
							is not the eventActor.
	124	 2/17/95	Hernan  	handleKey redefined from what used to be
							ScriptEditText method.
	125	 3/ 1/95	Hernan  	Updating source to work with new Fred 3.0 code.
	126	 3/ 1/95	Hernan  	Copying font information correctly.
	127	 3/ 3/95	Hernan  	Trying to figure out why the field reverts to 
							Monaco when we restore SK8.
	128	 3/10/95	rod     	Checking in Hernan's fix to set-edit-text-font-spec.
	129	 3/10/95	dy      	Hern‡n sez: when a font attribute is changed, and the change affects the whole buffer,
							we also need to change the buffer's efont and cfont
	130	 3/15/95	Hernan  	editText-draw now calls frec-update to deal with
							fr.bwin being out of synch. This seems to be the
							cause of the buffer-position errors in the help
							window.
	131	 3/15/95	Hernan  	enteringStage of EditText has to make sure 
							fr.bwin is correct before setting the wptr.
	132	 3/22/95	Hernan  	Changed idle to clear the update region if it has 
							not been cleared by calling forceRedraw.
	133	 3/27/95	Hernan  	1232766: mouseDown of EditText propagates the
							event.
	134	 4/ 5/95	Hernan  	set verticalScroll updates the bwin if necessary.
	135	 4/14/95	Hernan  	update-bwin also checks the value of fr.selposns and
							marks it dirty.
	136	 4/14/95	Hernan  	updatePartnerScroller updates bwin just in case...
	137	 4/19/95	Hernan  	1241141: merge-with-current-font-spec is rewritten to 
							use MCL's code to parse font-specs.
	138	 4/25/95	Hernan  	Making tabInField not tab to next actor when modifier
							keys are pressed. Also added actorTextSize of EditText 
							which tells you how big the field should be to show all its 
							text. This is version 1: the slow one.
	139	 4/25/95	Hernan  	Now implementing the fast version of sk8-frec-size. Uses
							the cached information when it can. Done!
	140	 4/26/95	sidney  	actorTextSize needed a keyword argument to match the other definitions
	141	 4/26/95	rod     	For now removing the actortextsize speedup as
							it does not work in all cases.
	142	 4/26/95	rod     	Making fred redraw after each and every 
							keystroke...for now.
	143	 4/26/95	Hernan  	Fixing the lingering caret problem from our side of the
							fence. We do all drawing into the main port (a gWorld if
							there is one). To blink the caret we have to blit to the
							window.
	144	 4/28/95	Hernan  	Changed with-fred-drawing to do nothing when the call
							is nested (with-fred-drawing should not be nested).
	145	 5/ 1/95	Hernan  	with-fred-drawing has to be able to be called in a nested
							fashion. I fixed the problem that prevented that. Each time
							we prepare to draw the field we compute the origin by
							subtracting the field's position from {0,0}. Not a problem 
							since we restore the origin at the end.
	146	 5/ 1/95	Hernan  	More fixes to with-fred-drawing. Has to deal with the case
							that the port has been disposed. The key is to resample the
							port at the end of the execution of the body.
	147	 5/ 1/95	Hernan  	ForceRedraw has to make sure the gWorld's
							origin is at 0 before blitting. Added with-zero-origin for this
							purpose.
	2  	 6/ 8/95	sidney  	MCL 3.0 changes
	3  	 6/23/95	Hernan  	1239905, and also making all editText's wantsIdle to false
						until they get activated.
	4  	 6/23/95	Hernan  	1254507
						1255882: needs to make sure frec is updated before this is called.
						Also added nextFontChange. This allows save as text to work.
	5  	 6/26/95	Hernan  	Fixing fred click weirdness introduced by 3.0b5 code.
	6  	 6/30/95	sidney  	add arguments to new-fred to support saving text styles 
						info with fred buffer
	7  	 7/ 5/95	Hernan  	Adding a view-mouse-position method for Sk8-fred.
	8  	 7/12/95	Hernan  	handleKey has to take ownership of the drawing lock.
	9  	 9/15/95	Hernan  	fill-in-buffer-text-info sets the buffer empty font, if the
						buffer only has 1 font for itself.
	10 	12/13/95	Hernan  	Need to redefine (setf fred-tabcount) of sk8-fred because
						the MCL version only works when the Fred is on the screen.
	11 	 1/26/96	sidney  	remove dependency of gs:call-with-temp-string of editText on lap code. Note that it no longer stack-allocates the temp string
	12 	 2/15/96	Brian   	Adding selectAll.
	2  	 4/10/96	Hernan  	define-system-handler->define-handler.
	4  	 4/22/96	Brian   	Fixing method congruency problems.
	5  	 5/ 2/96	Hernan  	Fixing with-fred-drawing to correctly allow EditText to 
						image when capturing its picture.
	6  	 5/ 7/96	sidney  	dont't use originalpropertyvalue. it was a no-op the way it was used anyway, and it shouldn't be defined yet when this is loaded
	7  	 5/ 8/96	Hernan  	Adding previousFontChange.
	8  	 5/ 8/96	Hernan  	fred-click only updates the scroller when it is done. This
						makes scrolling by hand a lot faster.
	9  	 6/11/96	Hernan  	with-fred-drawing has to deal with the nested case. The
						solution is to store the functions previous state. If the new
						state is the previous state, then we assume all the port and
						origin stuff is set up correctly and execute the body. If the
						port we are drawing is not the same, we do all the setup
						again.
	10 	 7/ 7/96	sidney  	changes for native PPC build
	11 	 7/26/96	Hernan  	Need to call update-cursor or else fred-click has a redraw
						error. Very embarrassing that we do not know why 
						refreshing the cursor fixes it...
	12 	 7/26/96	Hernan  	update-cursor -> setting the cursor of the Stage to ibeamCursor.
	13 	11/15/96	Hernan  	Initialize cannot assume that original is the Edittext parent.
						The EditText parent might be in the args as part of the
						otherParents list. If not, an error has to be signaled.
	14 	11/15/96	Hernan  	Abstracted out the code I added just above. The function is
						now called originalAncestor.
	15 	12/12/96	Hernan  	Fixed view-key-event-handler to use the character supplied when
						the character from the event record is null. This allows key events
						generated programmatically to work.
	16 	12/17/96	Brian Roddy	Adding robustness to setselection.
	17 	 2/27/97	Hernan  	
|# ;(do not edit past this line!!)
