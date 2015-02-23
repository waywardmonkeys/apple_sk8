;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories

(in-package :cl-user)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; COMTABS
;;;

;; To make our (sparse) comments stand out more:

(let ((*warn-if-redefine-kernel* nil)
      (*warn-if-redefine* nil))
  
  (defmethod ed-indent-for-lisp ((w fred-mixin) &optional start end)
    (let ((buf (fred-buffer w)))
      (multiple-value-bind (selStart selEnd) (selection-range w)
        (when start (setq selStart start))
        (when end (setq selEnd end))
        (let* ((lineStart (buffer-line-start buf selStart))
               firstNonWhite
               lineEnd
               (lastLineEnd (buffer-line-end buf selEnd))
               (old-buf-position (buffer-position buf))
               commentStart
               style
               defStart
               defEnd)
          (loop
            (setq lineEnd (buffer-line-end buf lineStart)
                  firstNonWhite (ccl::buffer-fwd-skip-wsp buf lineStart))
            (when (and (> lineEnd (+ 4 firstNonWhite))
                       (string-equal "(def" (buffer-substring buf (+ 4 firstNonWhite) firstNonWhite))
                       (= firstNonWhite lineStart))
              (setq defStart (+ 4 firstNonWhite))
              (loop
                (when (= defStart lineEnd)
                  (return))
                (if (position
                     (aref (buffer-substring buf (1+ defStart) defStart) 0)
                     " 	")    ;; space, tab
                  (return)
                  (incf defStart)))
              (loop
                (when (= defStart lineEnd)
                  (return))
                (if (position
                     (aref (buffer-substring buf (1+ defStart) defStart) 0)
                     " 	")    ;; space, tab
                  (incf defStart)
                  (return)))
              (setq defEnd defStart)
              (loop
                (when (= defEnd lineEnd)
                  (return))
                (if (position
                     (aref (buffer-substring buf (1+ defEnd) defEnd) 0)
                     ") 	")    ;; paren, space, tab
                  (return)
                  (incf defEnd)))
              (when (char= #\( (aref (buffer-substring buf (1+ defStart) defStart) 0))
                (incf defStart)
                (when (buffer-substring-p buf "SETF " defStart)
                  (setq defEnd (buffer-fwd-sexp buf (+ defStart 5)))))
              (when (/= defStart defEnd)
                (buffer-set-font-spec buf '(:bold "Monaco") defEnd defStart)))
            (set-mark buf lineStart)
            (when (setq commentStart (ccl::buffer-find-comment buf))
              (setq style (if (= commentStart lineStart)
                            :plain
                            :bold))
              (loop
                (when (= commentStart lineEnd)
                  (return))
                (if (position
                     (aref (buffer-substring buf (1+ commentStart) commentStart) 0)
                     "; 	")    ;; semicolon, space, tab
                  (incf commentStart)
                  (return)))
              (buffer-set-font-spec buf (list "Geneva" 9 style)
                                    commentStart
                                    lineEnd))
            (setq lineStart (buffer-line-start buf lineStart 1))
            (when (>= lineStart lastLineEnd)
              (return)))
          (set-mark buf old-buf-position))))
    ;; Now do normal thing.
    (let* ((frec (frec w))
           (c (fred-buffer w)))
      (unless (and start end) (multiple-value-setq (start end) (ccl::frec-get-sel frec)))
      (prog* ((pos (buffer-line-start c start))
              (defun (or (ccl::buffer-backward-search c (coerce #(#\Return #\() 'string) pos) 0))
              ind)
        (set-mark c end)
        loop
        (when (setq ind (ccl::lisp-indentation c defun pos))
          (buffer-delete c pos (ccl::buffer-fwd-skip-wsp c pos))
          (ccl::frec-indent-to-col frec pos (ccl::frec-hpos frec ind)))
        (setq pos (buffer-line-start c pos 1))
        (when (ccl::%i< pos (buffer-position c)) (go loop)))
      (collapse-selection w t)))
  
  )

;; Show file's full pathname on c-x c-f:
(comtab-set-key *control-x-comtab* '(:control #\f)
                #'(lambda (w)
                    (set-mini-buffer w (namestring (truename (slot-value w 'ccl::my-file-name))))))

(defun set-isearch-string-from-selection (window)
  (setf (fill-pointer ccl::*i-search-search-string*) 0)
  (ccl::dovector (ch (multiple-value-bind (selStart selEnd) (selection-range window)
                       (buffer-substring (fred-buffer window) selEnd selStart)))
                 (vector-push-extend ch ccl::*i-search-search-string*)))

(comtab-set-key *control-x-comtab* '(:control #\s)
                #'(lambda (w)
                    (set-isearch-string-from-selection w)
                    (ed-i-search-forward w)))
(comtab-set-key *control-x-comtab* '(:control #\r)
                #'(lambda (w)
                    (set-isearch-string-from-selection w)
                    (ed-i-search-reverse w)))


;; Restore the window-save functionality to a keystroke (c-x s):
(comtab-set-key *control-x-comtab* #\s 'window-save)

(defun surround-by-nice-lines (w)
  (let ((theCommentLine (format nil ";;; _______________________________ ~%"))
        (buf (fred-buffer w)))
    (multiple-value-bind (start end) (selection-range w)
      (declare (ignore end))
      (buffer-insert buf theCommentLine (buffer-line-start buf start))
      (buffer-insert buf theCommentLine (1+ (buffer-line-end buf (+ start (length theCommentLine))))))
    (set-mini-buffer w "Ahhhh...")))

(comtab-set-key *control-x-comtab* #\m 'surround-by-nice-lines)

(defun window-and-char-under-mouse ()
  (let* ((where nil)
         (window (dolist (w (windows :class 'fred-window))
                   (setq where (view-mouse-position w))
                   (when (#_PtInRgn where (rref (wptr w) :cGrafPort.visRgn))
                     (return w)))))
    (when (and window (ignore-errors (frec window)))
      (values
       window
       (ccl::frec-point-pos (frec window) where)))))

(defun window-and-sexp-under-mouse ()
  (multiple-value-bind (window char) (window-and-char-under-mouse)
    (when (and window char)
      (multiple-value-bind (s e) (buffer-current-sexp-bounds (fred-buffer window) char)
        (when (and s e)
          (values window (buffer-substring (fred-buffer window) e s) s e))))))

(defun safe-read-sexp-under-mouse ()
  (multiple-value-bind (window sexp-string s e) (window-and-sexp-under-mouse)
    (declare (ignore window s e))
    (ignore-errors (read-from-string sexp-string))))

#| 951202 DY - show-fixed-under-mouse is undefined - this is broken
(defun show-or-convert-fixed-under-mouse (window)
  (if (ccl::key-down-p 53)  ;;; 53 is the ESC key code.
    (convert-fixed-under-mouse window)
    (show-fixed-under-mouse window)))
|#

#|  ;; Shouldn't refer to MF package (since this set of utils is loaded right at start of build)!
(defun convert-fixed-under-mouse (front-window)
  (multiple-value-bind (window sexp-string s e) (window-and-sexp-under-mouse)
    (when (eq window front-window) ;; *** Maybe bring it forward if it's not frontmost
      (let ((number (ignore-errors (read-from-string sexp-string))))
        (when (fixnump number)
          (setq sexp-string (if (char= (char sexp-string 0) #\#)
                              (princ-to-string number)
                              (concatenate 'string "#%" (mf::fixed-to-string number))))
          (ed-replace-with-undo window s e sexp-string))))))


(defun show-fixed-under-mouse (window)
  (let ((number (safe-read-sexp-under-mouse)))
    (when (fixnump number)
      (let ((format-string " ~A  =  #%~A")
            (number-string (mf::fixed-to-string number)))
        (cond
         ((ignore-errors (view-mini-buffer window))
          (set-mini-buffer window format-string number number-string)
          (mini-buffer-update window))
         (t
          (format *top-listener* format-string number number-string)
          (stream-force-output *top-listener*)))))))


(comtab-set-key *comtab* '(:function #\1) 'show-or-convert-fixed-under-mouse)
|#

#| 951202 DY - what is mini-buffer-string?  this is broken
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CTRL-Click on Minibuffer types it into the window
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod view-click-event-handler :after ((window fred-window) where)
  (when (control-key-p)
    (let* ((mini-buffer-rect (ignore-errors (ccl::mini-buffer-rect window)))
           (string (when (and mini-buffer-rect (#_PtInRect where mini-buffer-rect))
                     (mini-buffer-string (view-mini-buffer window)))))
      (when string
        (ed-insert-with-undo window (concatenate 'string (string #\Space) string))
        (fred-update window)))))
|#


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Modifier key when pulling down Windows menu alphabetizes its items
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(unless (fboundp 'real-update-windows-menu)
  (setf (symbol-function 'real-update-windows-menu) #'CCL::update-windows-menu))
(let ((*warn-if-redefine-kernel* nil))
  (defun CCL::update-windows-menu (theMenu)
    (real-update-windows-menu theMenu)
    (when (any-modifier-keys-p)
      (let ((items (menu-items theMenu)))
        (apply #'remove-menu-items theMenu items)
        (setq items (sort items #'string-lessp :key #'menu-item-title))
        (apply #'add-menu-items theMenu items)))))
(setf (slot-value *windows-menu* 'CCL::update-function) #'CCL::update-windows-menu)

;;; _______________________________ 
;;; Click on something on an inspector with the shift key down and it gets
;;; sent to the listener. 
;;; _______________________________ 

(let ((ccl::*warn-if-redefine-kernel* nil))
  (defmethod view-click-event-handler ((view inspector::inspector-view) where)
    (let ((single-click-inspect (and (not (shift-key-p)) (any-modifier-keys-p))))
      (if (and (not single-click-inspect) (double-click-p))
        (inspector::inspect-selection view)
        (progn
          (inspector::select-inspector-view (view-container view) view)
          (let ((v (point-v where))
                (line-positions (inspector::line-positions view))
                temp
                new-selection)
            (when line-positions
              (setq temp (aref line-positions 0))
              (dotimes (i (1- (length line-positions)))
                (declare (fixnum i))
                (when (and (<= temp v)
                           (< v (setq temp (aref line-positions (1+ i)))))
                  (let ((selection (+ (inspector::start-line view) i)))
                    (unless (eq (inspector::cached-type-n view selection) :comment)
                      (setq new-selection selection)
                      (return)))))
              (inspector::set-selection view new-selection)
              (inspector::update (view-container view))
              (if single-click-inspect (inspector::inspect-selection view)))))))
    (when (shift-key-p)
      (ccl::funcall-in-top-listener-process 
       'ccl::toplevel-print (list (inspector::selected-object view)))
      ))
  )

#|
;; To see the keyboard-code of a key:
(loop
  (dotimes (i 128)
    (when (ccl::key-down-p i) (format t "~%~A is down" i))))


;; To dump the functional contents of a file-search result window into the listener:
(progn (mapc #'(lambda (foo) (print (cadr foo)))
             (cddr (slot-value (find-window "Files containing" 'CCL::ACTION-SEQUENCE-DIALOG)
                               'ccl::my-items))) nil)

;; To dump the string contents of a file definitions list window into the listener:
(progn (mapc #'(lambda (foo) (terpri) (princ (car foo)))
             (cddr (slot-value (find-window "Definitions in" 'CCL::MENU-OF-DEFS-DIALOG)
                               'ccl::items))) nil)

|#
#|
	Change History (most recent last):
	2	8/20/93	kleiman	added in-package at top for scanner's sake
	3	10/25/93	chip	fixed indentor to correctly bold setter definitions
	4	11/10/93	chip	Windows menu sorts with modifier-key
	5	7/14/94	Hernan	When making a function name bold, we also make
				sure the font is Monaco since it could have been
				changed if the function was previously taken as
				a comment.
	6	7/14/94	Hernan	Adding a function to add section headers for files.
	7  	12/ 9/94	till    	Color Fred again:
							changes to ed-indent-for-lisp
	8  	 3/ 1/95	Hernan  	Fixed ed-indent-for-lisp to use the right signature of buffer-set-font-spec.
	3  	 1/ 8/96	dy      	comment out two apparently unused, obviously broken functions, declare an ignored variable
	4  	 1/30/96	Hernan  	ed-indent-for-lisp no longer highlights things that start with
						"(def" and are not at the start of the line. No more var
						names in let statements highlighted.
	2  	 8/20/96	Hernan  	Needed to deal with warn-if-redefine-kernel for ed-indent-for-lisp.
	3  	 9/10/96	Hernan  	Adding my inspector utility.
	4  	 2/27/97	Hernan  	
|# ;(do not edit past this line!!)
