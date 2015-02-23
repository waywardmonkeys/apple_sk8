;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories

;;;-*- Mode: Lisp; Package: ccl -*-

;;	Change History (most recent first):
;;  3 10/26/95 gb   some macro changes (no read-time conditionalization.)
;;  2 10/23/95 akh  ppc stuff
;;  8 5/15/95  akh  just debugging macros
;;  7 5/8/95   akh  some dbmsg macros
;;  6 5/5/95   akh  add a slot to frec
;;  5 5/4/95   akh  probably no change
;;  3 4/24/95  akh  added fr.printing-p
;;  2 4/4/95   akh  change commented out deffredmethod for delegation-mixin
;;  7 3/22/95  akh  change a macro in a comment
;;  (do not edit before this line!!)

; FredEnv.lisp
;; Copyright 1989-1994 Apple Computer, Inc.
;; Copyright 1995 Digitool, Inc.

(in-package :ccl)

; Modification History
;
;  4/24/95 slh   in-package makes swapping lisp happier
; 04/07/89 gb   $sp8 -> $sp.
; 03/17/89  gz  %cons-comtab. Comtab fixes per as.
; 14-apr-89 as  interactive-arglist
; 13-apr-89 as  ed-last/next-input-line
; 10-apr-89 as  ed-delete-whitespace, ed-grab-last-input
; 8-apr-89  as  new comtab-default mechanism
; 12/16/88  gz  New buffer structure defs
; 11/20/88  gz  New fred structure defs.
; 9/10/88   gz  tb8$ -> tb$
; 8/14/88   gz  %comtab-data macro.  Use proper comtab for c-x.
;              Added m-; c-m-; and c-x ;
; 8/2/88  gb   %svref,%svset lose their "-8" suffixes.
; 5/25/88 jaj  added m-t, c-m-t
; 5/12/88 as   added several new keybindings

;  7/12/88 gz Lisp-8
; 04/02/88 gz new macptr scheme.
; 10/12/87 cfry changed meta-u to meta-U
;------------------------- Version 1.0 ------------------------------------
;8/4/87   gz  m-backspace = ed-rubout-word
;8/1/87   cfry ed-scroll-down -> ed-next-screen
;              ed-scroll-up   -> ed-previous-screen
; 7/27/87 gz  c-\ => c-/ for ed-help
; 7/26/87 gz  renamed ed-scroll-up <-> ed-scroll-down.
; 7/24/87 cfry 'ed-delete-fwd-delimiters moved to c-m-d
;              'ed-delete-bwd-delimiters moved to c-m-backspace
;               added ed-delete-word to m-d
;              c-v ed-scroll-up, m-v ed-scroll-down
; 7/12/87 gb Provide thyself.
; 6/22/87 gz more complete wsp et. al. constants.
; 6/11/87 gz added c-x c-s, c-x c-w, c-x c-v, c-x h, m-f, m-b, m-c, m-l, m-u
;            Made c-w do CUT.
; 6/2/87 gz mods for new comtab scheme. Complete CCL comtab!
; 5/18/87 gb use %(h)get-signed-word
; 3/30/87 gz mods in low-level buffer accessors


(eval-when (eval compile)
   (require'backquote))

(defconstant $ControlKey 12) ;in key event record...


;These are the linear whitespace characters in the initial readtable.
(makunbound 'wsp)
(defconstant wsp #.(let ((str (make-string 6  :element-type 'base-character)))
                      (set-schar str 0 #\Space)
                      (set-schar str 1 #\^I)
                      (set-schar str 2 #\^L)
                      (set-schar str 3 #\^@)
                      (set-schar str 4 #\^J)
                      (set-schar str 5 (%code-char #xCA))
                      str))

(makunbound 'wsp&cr)
(defconstant wsp&cr #.(let ((str (make-string 7 :element-type 'base-character)))
                        (set-schar str 0 #\Space)
                        (set-schar str 1 #\^M)
                        (set-schar str 2 #\^I)
                        (set-schar str 3 #\^L)
                        (set-schar str 4 #\^@)
                        (set-schar str 5 #\^J)
                        (set-schar str 6 (%code-char #xCA))
                        str))

;All atom-terminating chars in the initial readtable
(makunbound 'symbol-specials)
(defconstant symbol-specials #.(let ((str (make-string 16 :element-type 'base-character)))
                                 (set-schar str 0 #\Space)
                                 (set-schar str 1 #\()
                                 (set-schar str 2 #\))
                                 (set-schar str 3 #\^M)
                                 (set-schar str 4 #\')
                                 (set-schar str 5 #\`)
                                 (set-schar str 6 #\,)
                                 (set-schar str 7 #\")
                                 (set-schar str 8 #\;)
                                 (set-schar str 9 #\\)
                                 (set-schar str 10 #\|)
                                 (set-schar str 11 #\^I)
                                 (set-schar str 12 #\^L)
                                 (set-schar str 13 #\^@)
                                 (set-schar str 14 #\^J)
                                 (set-schar str 15 (%code-char #xCA))
                                 str))

(makunbound 'prefix&wsp&cr)
(defconstant prefix&wsp&cr (%str-cat "`',@" wsp&cr))

(makunbound 'atom-specials)
(defconstant atom-specials (%str-cat wsp&cr "()")) ;for bwd scan
(defconstant atom-specials-with-quote (%str-cat atom-specials "'`"))

(makunbound 'unoption-unkeymap)
(defconstant unoption-unkeymap
   (let ((s (make-string 256 :initial-element #\null :element-type 'base-character)))
     (dotimes (i 128) (declare (fixnum i)) (set-schar s i (%code-char i)))
     (set-schar s  94 #\i)  ;option-i reads in as ^
     (set-schar s 126 #\n)  ;option-n reads in as ~
    ;128 ???
     (set-schar s 129 #\A)
     (set-schar s 130 #\C)
    ;131-139 ???
     (set-schar s 140 #\a)
     (set-schar s 141 #\c)
    ;142-159 ???
     (dotimes (i 43)
       (declare (fixnum i))
       (set-schar s (%i+ i 160)
                    (schar "t*43687srg2eu=\"O5+,.ymdwPpb90z'o?1lvfxj\\\|; " i)))
    ;203-205 ???
     (dotimes (i 10) (declare (fixnum i))(set-schar s (%i+ i 206) (schar "Qq-_[{]}/V" i)))
    ;216 ???
     (dotimes (i 35)
       (declare (fixnum i))
       (set-schar s (%i+ i 217)
                    (schar "~!@#$%^&()WERTYUISDFGHJKL:ZXBNM<>hk" i)))
     ;252-255 ???
     s))

(defconstant shift-alist
  '((#\1 . #\!)
    (#\2 . #\@)
    (#\3 . #\#)
    (#\4 . #\$)
    (#\5 . #\%)
    (#\6 . #\^)
    (#\7 . #\&)
    (#\8 . #\*)
    (#\9 . #\()
    (#\0 . #\))
    (#\- . #\_)
    (#\= . #\+)
    (#\[ . #\{)
    (#\] . #\})
    (#\; . #\:)
    (#\' . #\")
    (#\, . #\<)
    (#\. . #\>)
    (#\/ . #\?)
    (#\\ . #\|)
    (#\` . #\~)))

(defmacro %cons-comtab (data default)
  `(%istruct 'comtab ,data ,default))

(def-accessors (comtab) %svref
  nil                                   ; 'comtab
  %comtab-data
  %comtab-default)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-accessors (buffer) %svref
  nil                            ; 'buffer
  bf.chunksz                     ; Size of chunks
  bf.gapchunk                    ; The chunk with start of gap.
  bf.gapstart                    ; Distance from gapchunk to gap start <= chunksz
  bf.gapend                      ; Distance from gapchunk to gap end.
  bf.gappos                      ; Buffer position of gap.
  bf.bufsiz                      ; Number of chars in buffer
  bf.bfonts                      ; Number of before-gap font changes
  bf.afonts                      ; Number of after-gap font changes
  bf.fontruns                    ; Vector of font changes
  bf.bmarks                      ; Before-gap marks (weak list)
  bf.amarks                      ; After-gap marks (weak list)
  bf.modcnt                      ; modification count
  bf.bmod                        ; Number of chars to first change
  bf.zmod                        ; Number of chars from last change to eob
  bf.plist                       ; For the user
  bf.maxasc                      ; Max ascent of any font in flist
  bf.maxdsc                      ; Max descent of any font in flist
  bf.maxwid                      ; Max char width of any font in flist
  bf.flist                       ; Buffer font table (ff/ms pairs)
  bf.fixnum                      ; Assorted flags
  bf.refcount                    ; reference count
  bf.chunkarr			 ; new - array of chunks
  bf.logsiz			 ; log2 of chunksz
  bf.undo-string
  bf.undo-redo
  bf.fred-history
)

;Byte offsets in bf.fixnum
(defconstant $bf_chartype 0)              ; 0 8 bit, 1 16 bit
(defconstant $bf_cfont 1)               ; Font to use in an empty buffer
(defconstant $bf_afont 2)               ; Font to use for next insertion
(defconstant $bf_flags 3)               ; Flags...
;Flags in bf.fixnum
(defconstant $bf_r/o-flag 0)            ; read-only buffer
(defconstant $bf_purge-fonts-flag 1)    ; enable purging of fonts if set.

(def-accessors %svref
  mark.value
  mark.buffer)

(defmacro %buf-bmod (buf)
  `(ccl::%svref (ccl::mark.buffer ,buf) ccl::bf.bmod))

(defmacro %buf-zmod (buf)
  `(ccl::%svref (ccl::mark.buffer ,buf) ccl::bf.zmod))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defconstant $portBounds.topleft 8)
(defconstant $portBounds.top 8)
(defconstant $portBounds.left 10)
(defconstant $bitmap.topleft 6)
(defconstant $bitmap.top 6)
(defconstant $bitmap.left 8)
(defconstant $portRect 16)
(defconstant $portVersion 6)            ; for color windows
(defconstant $portColorMask #xC000)     ; Mask for $portVersion
(defconstant $portPixMap 2)             ; for color windows
(defconstant $visRgn 24)
(defconstant $clipRgn 28)
(defconstant $pnLoc 48)
(defconstant $pnSize 52)
(defconstant $pnVis 66)
(defconstant $txFont 68)
(defconstant $txFace 70)
(defconstant $txMode 72)
(defconstant $txSize 74)
(defconstant $spExtra 76)
(defconstant $updateRgn 122)

(defconstant $SrcOr 1)
(defconstant $SrcXor 2)

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
  $fred-record-length
  )

(defmacro cons-fred-record ()
  `(%istruct 'fred-record ,@(make-list (1- $fred-record-length) :initial-element nil)))


(defmacro fr.flags (x)
  `(the fixnum (fr.flags-slot ,x)))

(defmacro deffrflag (name bit)
  (let ((setf-name (intern (ccl::%str-cat "SETF-" (symbol-name name)) *package*)))
    `(progn
       (defconstant ,name ,bit)
       (declaim (inline ,name ,setf-name))
       (defmacro ,name (frec) `(logbitp ,,bit (fr.flags ,frec)))
       (defmacro ,setf-name (frec value)
         (let ((frec-var (gensym))
               (value-var (gensym)))
           `(let ((,frec-var ,frec)
                  (,value-var (not (null ,value))))
              (setf (fr.flags ,frec-var)
                    (if ,value-var
                      (logior ,,(ash 1 bit) (fr.flags ,frec-var))
                      (logand ,,(lognot (ash 1 bit)) (fr.flags ,frec-var))))
              ,value-var)))
       (defsetf ,name ,setf-name))))

(deffrflag fr.bpchar-on-p  0)
(deffrflag fr.caret-on-p 1)
(deffrflag fr.frame-sel-p 2)
(deffrflag fr.wrap-p 3)
(deffrflag fr.framed-sel-p 4)
(deffrflag fr.bwin-cr-p 5)
(deffrflag fr.text-edit-sel-p 6)
(deffrflag fr.truetype-p 7)
(deffrflag fr.word-wrap-p 8)
(deffrflag fr.cursor-italic-p 9)
(deffrflag fr.zwin-return-p 10)         ; #\return at fr.zwin
(deffrflag fr.cursor-bol-p 11)          ; cursor at beginning of line.
(deffrflag fr.cursor-bol-p-valid 12)    ; set when frec-click sets fr.cursor-bol-p
                                        ; cleared by %frec-update-internal
(deffrflag fr.center-justified-p 13)
(deffrflag fr.right-justified-p 14)
(deffrflag fr.line-right-p 15)          ; right-to-left line direction

(deffrflag fr.nodrawing-p 27)
(deffrflag fr.buf-changed-p 26)
(deffrflag fr.curs-changed-p 25)
(deffrflag fr.sel-valid-p 24)
(deffrflag fr.changed-p 23)
(deffrflag fr.printing-p 22)  ; new 4/17


(defconstant $fr.flags_non-drawing-bits-mask #x1FFFFF)

(defmacro with-frec ((frec-var frec) &body body)
  (let ((thunk (gensym)))
    `(let ((,thunk #'(lambda (,frec-var) ,@body)))
       (declare (dynamic-extent ,thunk))
       (call-with-frec ,frec ,thunk))))

(defmacro linevec-ref (linevec index)
  `(ccl::%svref ,linevec ,index))

(defmacro dbmsg (&rest args) (declare (ignore args)) ())
(defmacro dbtest (&rest args) (declare (ignore args)) ())
#|
(defun *frdebug* ()
  (and *frdebug*
       (or (not (typep *frdebug* 'view))
           (eq *current-view* *frdebug*))))
(defmacro dbmsg (&rest args)
  `(when *frdebug*
     (format t ,@args)))

; I like this better
(defvar *memoize* nil)
(defvar *memo* nil)
(defmacro dbmsg (&rest args)
  `(when *memoize*
    (push (format nil ,@args) *memo*)))

; or
(defmacro dbmsg (&rest args)
  `(when (eq frec my-frec)
    (push (ignore-errors (format nil ,@args)) *memo*)))


; need to munch on args more - eval has something likethat ?

(defun arg-names-from-arglist (list)
  (let ((opt-p nil)
        (key-p nil)
        (rest-p nil)
        res
        ret-rest
        before-rest)
    (dolist (x list)
      (case x 
        (&optional (setq opt-p t))
        (&key (setq key-p t))
        (&aux (return))
        (&rest (setq rest-p t))
        (t (if rest-p
             (progn (setq rest-p nil)
                    (setq ret-rest x)
                    (setq before-rest (reverse res)))
             (if (listp x)
               (if (not (or opt-p key-p))
                 (error "barf")
                 (push (car x) res))
               (push x res))))))
    (values (nreverse res) ret-rest before-rest)))
                   
; not perfect - handles most common cases
; her job is to define the main method for fred-mixin and the delegator
; for fred-window.    
(defmacro deffredmethod (name (fred-var &rest more-args) &body body)
  (multiple-value-bind (argnames rest-p before-rest)(arg-names-from-arglist more-args)
    (let* ((pos (position '&aux more-args))
           (more-args-no-aux (if pos (butlast more-args (- (length more-args) pos))
                                 more-args)))
    `(progn
       (defmethod ,name ((,fred-var fred-mixin) ,@more-args)
         ,@body)
       ,(if (not rest-p)
          `(defmethod ,name ((,fred-var fred-delegation-mixin) ,@more-args-no-aux)                                
             (,name (fred-item ,fred-var) ,@argnames))
          `(defmethod ,name ((,fred-var fred-delegation-mixin)  ,@more-args-no-aux)
             (declare (ignore-if-unused ,@argnames))
             (declare (dynamic-extent ,rest-p))
             (apply ',name (fred-item ,fred-var) ,@before-rest ,rest-p)))))))

; currently 240 methods on fred-mixin, 60 on fred-window
; what is the cost of ~180 new but trivial methods?
(deffredmethod bar (fred y &optional z)
               (list y z))

(deffredmethod bap (fred y &rest z &key foo &aux barf))
  
|#

(provide "FREDENV")


#|
	Change History (most recent last):
	2	12/29/94	akh	merge with d13
  3   1/6/95   akh   add macro deffredfmethod
|# ;(do not edit past this line!!)

#|
	Change History (most recent last):
	1  	 3/ 1/95	Hernan  	Last version merged.
	2  	 4/11/95	Hernan  	Folded in fredenv from MCL 3.0a5.
	3  	 4/21/95	Hernan  	Folded in fredenv from MCL3.0b1.
	4  	 6/26/95	Hernan  	Folded in fredenv from MCL3.0b3.
	5  	 7/14/95	Hernan  	Folded in fredenv from MCL3.0 final.
	2  	 4/25/96	Hernan  	Folded in fredenv from MCL-PPC 3.9f2c2.
	4  	 2/27/97	Hernan  	
|# ;(do not edit past this line!!)
