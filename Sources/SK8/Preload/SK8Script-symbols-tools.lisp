;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories


;;;

(in-package :ccl)


(export '(maybe-save-original-sym-case reintern-in-other-package intern-symbol
          legal-ident-name? intern-legal-varName intern-legal-handlerName
          pretty-symbol-name write-pretty-symbol-name true false)
        :ccl)


(defun maybe-save-original-sym-case (sym &optional (pkg (if (symbolp sym) (symbol-package sym) *package*)))
  (let* ((real-string (if (stringp sym) sym (symbol-name sym)))
         (all-lower (every #'(lambda (c) (or (not (alpha-char-p c)) (lower-case-p c))) real-string))
         (all-upper (every #'(lambda (c) (or (not (alpha-char-p c)) (upper-case-p c))) real-string))
         (up-string (if all-upper real-string (string-upcase real-string)))
         (desired-sym (if (and all-upper (symbolp sym)) sym (intern up-string pkg))))
    
    (when (and (symbolp sym) (not all-upper))
      (unintern sym pkg))
    ;; *** Shouldn't save anything when there are no alpha chars in name!!!
    ;; *** To save memory, could save :UPCASE, :DOWNCASE, :CAPITALIZE, or the original string
    (when (and (not all-lower)   ;; Added this to save memory (since downcasing is the default)
               (not (get desired-sym :original-case)))
      (setf (get desired-sym :original-case) real-string))
    desired-sym))


(defun reintern-in-other-package (sym pkg)
  (let ((newsym (intern (symbol-name sym) pkg)))
    (unless (get newsym :original-case)
      (let ((original-case (get sym :original-case)))
        (when original-case (setf (get newsym :original-case) original-case))))
    newsym))


(defun intern-symbol (str pkg)
  (let ((len (length str)))
    (declare (fixnum len))
    (if (> len 3)
      (if (and (not (eql len 4)) (string-equal str "SET " :end1 4))
        (let* ((sstr (subseq str 4))
               (sym (maybe-save-original-sym-case sstr pkg)))
          (CCL::SETF-FUNCTION-NAME sym))
        (let ((sym (maybe-save-original-sym-case str pkg)))
          (case sym
            (TRUE t)
            (FALSE nil)
            (t sym))))
      (maybe-save-original-sym-case str pkg))))


(defun legal-ident-name? (str &optional s e)
  (declare (fixnum s e))
  (unless s (setq s 0))
  (unless e (setq e (length str)))
  (unless (eql s e)
    (let ((ch (aref str s)))
      (when (or (alpha-char-p ch) (eq ch #\_))
        (loop
          (when (eql (incf s) e) (return str))
          (setq ch (aref str s))
          (unless (or (alpha-char-p ch) (digit-char-p ch) (eq ch #\_)) (return nil)))))))


(defun intern-legal-varName (str pkg &optional (error t) forceCase)
  (if (legal-ident-name? str 0 (length str))
    (let ((sym (maybe-save-original-sym-case str pkg)))
      (case sym
        (TRUE t)
        (FALSE nil)
        (t
         (when (and forceCase (not (string= str (pretty-symbol-name sym))))
           (remf (symbol-plist sym) :original-case)
           (maybe-save-original-sym-case str pkg))
         sym)))
    (when error
      (error "The string ~s is not a legal identifier for use as a variable name" str))))


(defun intern-legal-handlerName (str pkg &optional (error t) forceCase)
  (declare (ignore forceCase)) ; only works for intern-legal-varName
  (let ((len (length str)))
    (declare (fixnum len))
    (if (legal-ident-name? str 0 len)
      (let ((sym (maybe-save-original-sym-case str pkg)))
        (case sym
          (TRUE t)
          (FALSE nil)
          (t sym)))
         (or (and (> len 4)
               (cond
                ((and (string-equal str "SET " :end1 4) (legal-ident-name? str 4 len))
                 (let* ((sstr (subseq str 4))
                        (sym (maybe-save-original-sym-case sstr pkg)))
                   (CCL::SETF-FUNCTION-NAME sym)))
                ((and (> len 5) (string-equal str "WITH " :end1 5) (legal-ident-name? str 5 len))
                 (let ((sym (maybe-save-original-sym-case str pkg)))
                   sym))
                ((and (> len 9) (string-equal str " FOR MODS" :start1 (- len 9)) (legal-ident-name? str 0 (- len 9)))
                 (let ((sym (maybe-save-original-sym-case str pkg)))
                   sym))))
          (when error
            (error "The string ~s is not a legal identifier for use as a handler name" str))))))

#| defined better elsewhere
(defun pretty-symbol-name (sym)
  (cond
   ((eq sym nil) "False")
   ((eq sym t) "True")
   ;; Special-case for setter symbols
   ((eq (symbol-package sym) CCL::*SETF-PACKAGE*)
    (let ((*readtable* CCL::%INITIAL-READTABLE%))
      (setq sym (read-from-string (symbol-name sym)))
      (concatenate 'string "set " (or (get sym :original-case)
                                      (string-downcase (symbol-name sym))))))
   (t
    (or (get sym :original-case)
        (string-downcase (symbol-name sym))))))

|#

(defun write-pretty-symbol-name (sym strm)
  (cond
   ((eq sym nil) (write-string "False" strm))
   ((eq sym t) (write-string "True" strm))
   ;; Special-case for setter symbols
   ((eq (symbol-package sym) CCL::*SETF-PACKAGE*)
    (let ((*readtable* CCL::%INITIAL-READTABLE%))
      (setq sym (read-from-string (symbol-name sym))))
    (write-string "set " strm)
    (let ((name (get sym :original-case)))
      (if name
        (write-string name strm)
        (write-perverted-string (setq name (symbol-name sym))
                                strm (length name) :downcase))))
   (t
    (let ((name (or (get sym :original-case)
                    (string-downcase (symbol-name sym)))))
      (if name
        (write-string name strm)
        "This symbol no name")))))

;;; NOTE that this is not called by the pretty-printer
;;;
(defmethod print-object ((s symbol) strm)
  (declare (optimize (speed 3) (safety 0)))
  (if (or (not (boundp 'MF::*SAVING-INTO-PROJECT-STORE*)) MF::*SAVING-INTO-PROJECT-STORE*)
    (CCL::WRITE-A-SYMBOL s strm)  ; To ensure that the symbol reconstitutes correctly
    (let ((sym-pkg (symbol-package s))
          (sym-name (symbol-name s))
          (escape-bars nil)
          original)
      (declare (package sym-pkg) (simple-string sym-name))
      (when *print-escape*
        (unless (eq sym-pkg *package*)
          (cond
           ;; It's a gensym
           ((null sym-pkg)
            (princ #\# strm) (princ #\: strm))
           ;; It's a keyword
           ((eq sym-pkg (find-package :keyword))
            (princ #\: strm))
           (t
            (multiple-value-bind (sym where) (find-symbol sym-name sym-pkg)
              (cond
               ;; It's not in its own package!
               ((and s (null sym))
                (princ "#|symbol not found in home package!!|#" strm))
               ;; It's fully imported into *package*
               ((eq s (find-symbol sym-name *package*)))
               ;; It's internal
               ((eq where :internal)
                (stream-write-string strm (package-name sym-pkg) 0 (length (package-name sym-pkg)))
                (princ #\: strm) (princ #\: strm))
               ;; It's external but not imported into *package*
               (t
                (stream-write-string strm (package-name sym-pkg) 0 (length (package-name sym-pkg)))
                (princ #\: strm)))))))
        (when (dovector (c sym-name) (when (or (eq c #\Space) (eq c #\:) (eq c #\') (eq c #\)) (eq c #\() (eq c #\,) (eq c #\{)) (return t)))
          (setq escape-bars t)
          (princ #\| strm)))
      (cond
       (escape-bars
        (stream-write-string strm sym-name 0 (length sym-name)))
       ((setq original (get s :original-case))
        (stream-write-string strm original 0 (length original)))
       (t
        (CCL::WRITE-PERVERTED-STRING sym-name strm (length sym-name) :downcase)))
      (when escape-bars (princ #\| strm))
      nil)))


#|
	Change History (most recent last):
	1		6/29/94	chip	a new improved interface for the built-in symbol-case-saving (radar #107741)
	2		6/29/94	chip	added pretty-symbol-name definition here
	3		7/4/94	sidney	set symbol-name when SK8 creates a symbol for object or handler name
	4		7/5/94	chip	added forceCase optional arg to intern-legal-varName/intern-legal-handlerName
	5		7/6/94	chip	changed it all to work the old way (saves original-case in the symbol plist!)
	6		7/6/94	sidney	define a dummy  pound twiddles so we can build for now until we remove them
	7		7/7/94	chip	oops -- fixed an inadvertent ommission from intern-symbol
	8		7/8/94	Hernan	took out #~ reader-macro "stub" -- the real thing is defined in SK8-load.lisp
	9		7/11/94	chip	reinstituted the pretty-symbol behavior for the lisp printer
	10		7/14/94	chip	True & False now write themselves capitalized (since they're objects -- radar #1174468)
	11		8/4/94	sidney	remove a dependency so we can load this earlier in build, since intern-symbol is now used earlier
	12		8/4/94	sidney	fix export/import problem with True and False
	13		8/4/94	sidney	whhops - missed one
	14 	11/20/94	chip    	intern-legal-handlerName now accepts "... for mods" handler names (radar #1200774)
	15 	 1/31/95	sidney  	eliminate extra intrn/unintern in maybe-save-original-sym-case by letting it work with string instead of symbol
	2  	 5/23/96	sidney  	
	3  	 8/ 8/96	Hernan  	Something to do with write-perverted-string.
	4  	 2/27/97	Hernan  	
|# ;(do not edit past this line!!)
