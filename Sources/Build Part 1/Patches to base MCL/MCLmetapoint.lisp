;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories

;;;


(in-package :ccl)

(eval-when (load compile)
  (defconstant symbol-specials #.(let ((str (make-string 16)))
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
                                      str)))

(defun sk8-search-method-classes (w classes qualifiers pos end)
  (let () ; eventually do all classes
    (when (and (setq pos (buffer-skip-fwd-wsp&comments w pos end))
               (cond 
                ((eq qualifiers t)
                 (while (eq (buffer-char w pos) #\:)   ; skip :before, :after, :around
                   (setq pos (buffer-fwd-symbol w pos end))
                   (setq pos (buffer-skip-fwd-wsp&comments w pos end)))
                 t)
                ((null qualifiers) t)
                ((and (eq (buffer-char w pos) #\:)
                      (setq pos (buffer-delimited-substring-p w (symbol-name (car qualifiers))
                                                              (1+ pos) end)))
                 (setq pos (buffer-skip-fwd-wsp&comments w pos end))))
               (eq (buffer-char w pos) #\( )
               ;(progn (print-db "Found the open paren") t)
               (setq pos (buffer-skip-fwd-wsp&comments w (1+ pos) end)))
      (or (eq classes t)
          (dolist (class classes t)
            (cond
             ((eq class t)
              (unless (and (not (%str-member (buffer-char w pos)
                                             symbol-specials))
                           (setq pos (buffer-fwd-symbol w pos end)))                                    
                (return nil)))
             (t (let (start sk8class)
                  (unless
                    (and (setq pos (buffer-skip-fwd-wsp&comments w pos end))
                         (setq start pos)
                         (if (and (eq #\# (buffer-char w pos))
                                  (eq #\. (buffer-char w (1+ pos))))
                           (incf pos 2)
                           t)                         
                         (setq sk8class (buffer-substring w start (buffer-fwd-sexp w pos end)))
                         (incf pos)
                         (let ((*package* (buffer-getprop w 'package *package*)))
                           (setq sk8class (ignore-errors (read-from-string sk8class))))
                         (setq sk8class (if (symbolp sk8class)
                                          (when (boundp sk8class) (symbol-value sk8class))
                                          sk8class))
                         (setq sk8class (if (typep sk8class 'class) sk8class (class-of sk8class)))
                         (eq (class-name sk8class) class))
                    (return nil)))))
            (unless (setq pos (buffer-skip-fwd-wsp&comments w pos end))
              (return nil)))))))




(defun search-for-def-sub (w target type classes qualifiers pos end toplevel)
  (let ((target-length (length target))
        chr result)
    (while (and pos (< pos end))
      (setq pos (buffer-skip-fwd-wsp&comments w pos end))
      ;(print (list "after wsp" pos end))
      (when (and pos  (< pos end))
        (case (setq chr (buffer-char w pos))
          (#\#
           (if (< (+ pos 3) end)
             (case (setq chr (buffer-char w (1+ pos)))
               (#\\ (setq pos (+ pos 3)))  ; just skip the chr- dont care about #\return
               ((#\+ #\-) (setq pos (search-def-features w pos end chr)))
               (t ; this is iffy
                (setq pos (buffer-fwd-sexp w (+ pos 2) end))))
             (setq pos nil)))
          (#\(
           (let* ((expend (buffer-fwd-up-sexp w (1+ pos) end))
                  (defend (buffer-fwd-symbol w (1+ pos) end))
                  before-d-e-f)
             (cond
              ((null expend)(setq result nil))
              ((or (and (buffer-substring-p w "def" (1+ pos))
                        (setq before-d-e-f (1+ pos)))
                   (setq before-d-e-f
                         (buffer-substring-with-package-p w "def" (1+ pos) defend 3)))
               (let* ((defstart pos)
                      (after-d-e-f (+ before-d-e-f 3)))
                 (setq pos (buffer-skip-fwd-wsp&comments w defend end))
                 (when (buffer-substring-p w "#~" pos)  ;; hack so meta-. works with #~ symbols
                   (setf pos (+ pos 2)))
                 (when (and (setq pos (buffer-delimited-substring-with-package-p
                                       w target pos expend target-length))
                            (or toplevel
                                ; be fussier when not toplevel? e.g. require newline
                                t)
                            (or (null type)
                                (search-check-type w type after-d-e-f expend))
                            (or (neq type 'method)
                                ;(null (cdr classes))  ; *** roo
                                (search-method-classes w classes qualifiers pos expend)
                                (sk8-search-method-classes w classes qualifiers pos expend)
                                ))
                   
                   (setq result defstart)
                   )))
              (t ; here want to look inside top level sexps - e.g. eval-when, progn
               ; is it reasonable to go only one level? - conditional on found already?
               (when (or ;toplevel ; dunno about this
                      (do ((thing *search-top-level-forms* (cdr thing)))
                          ((null thing) nil)
                        (when (buffer-delimited-substring-p
                               w (caar thing) (1+ pos) end (cdar thing))
                          (return t))))
                 (setq result
                       (or (search-for-def-sub w target type classes qualifiers
                                               (1+ pos) expend nil)
                           result)))))
             ;(when (and pos expend (> pos expend)) (break))
             (setq pos expend)))
          (t (setq pos (buffer-fwd-sexp w pos end chr))))))
    (when (and (eq type 'structure)(not result)(neq  (schar target 0) #\())
      (setq result (search-for-def-sub w (%str-cat "(" target)
                                       type classes qualifiers pos end toplevel)))
    ;(print (list "exiting" result))(break)
    result))
#|
	Change History (most recent last):
	1	2/21/94	sidney	move this file to a new subproject
	2	6/7/94	sidney	hack so meta-. works with #~ symbols
	2  	 2/27/97	Hernan  	
|# ;(do not edit past this line!!)
