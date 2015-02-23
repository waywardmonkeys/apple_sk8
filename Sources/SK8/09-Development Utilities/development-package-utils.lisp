;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories


;;;


(in-package :macframes)

(defparameter mf::*SK8-Dev-packages*
  (list (list (find-package :SK8)
              (find-package :SK8Dev)
              "SK8Development" 
     "SK8"
              #P"ccl:SK8;Preload;SK8PublicSyms.lisp"
              #P"ccl:SK8;Preload;SK8PrivateSyms.lisp"
              #P"ccl:SK8;Preload;SK8SpecialSyms.lisp")
        (list (find-package :UI)
              (find-package :UIDev)
              "UIDevelopment"
              "UI"
              #P"ccl:SK8;Preload;UIPublicSyms.lisp"  
              #P"ccl:SK8;Preload;UIPrivateSyms.lisp"
              #P"ccl:SK8;Preload;UISpecialSyms.lisp")))

; (ccl::set-fred-package ccl::*top-listener* (ccl::in-package :SK8dev))
; (defpackage SK8Development (:use :ccl :common-lisp :SK8) (:nicknames :SK8Dev))
; (defpackage UIDevelopment (:use :ccl :common-lisp :UI) (:nicknames :UIDev))

;; deal with backquotes by processing forms that contain a combination of list, list* and quote forms
;; since the scanner works ok with quoted forms that contain #. macros which are converted to NILs,
;; all we should have to do is substitute NIL for anything that would have to be evaluated and
;; end up with a form that could be quoted. E.g., `(foo bar ,baz) is equivalent to '(foo bar #.baz)
;; which is equivalent to '(foo bar nil)

(defun stripBackQuotes (form)
  (when (consp form)  ;; return nil if we don't know what to do with it. Only know what to do with lists
    (case (first form)
      (quote (second form))
      (list (mapcar #'stripBackQuotes (cdr form)))
      (list* (apply #'list* (mapcar #'stripBackQuotes (cdr form)))))))

(defun maybeStripBackQuotes (form)
  (or (stripBackQuotes form) form))

(defun processNewOrCopy (rest buf s e)
  (declare (ignore buf s e))
  (when (and (listp rest)   ;; do some minimal sanity checking before trying to parse this form
             (oddp (length rest))
             (keywordp (cadr rest)))
    (let* ((newFormKeywordArgs (cdr rest))
           (proj (getf newFormKeywordArgs :project))
           (private? (getf newFormKeywordArgs :privateObject))
           (objName (getf newFormKeywordArgs :objectName))
           (properties (getf newFormKeywordArgs :properties))
           (publicSyms nil)
           (privateSyms nil)
           (specialSyms nil))
      (when (and (symbolp proj) (or (boundp proj) (find-package proj)))
        (when (stringp objName)
          (setq objName (intern-symbol objName
                                       (if (and proj (boundp proj) (fboundp 'package))
                                         (funcall 'package (symbol-value proj))
                                         (find-package proj))))
          (if private?
            (push objName privateSyms)
            (push objName specialSyms)))
        (dolist (p (stripBackQuotes properties))
          (cond
           ((listp p)
            (if (memq :private (cdr p))
              (pushnew (first p) privateSyms :test #'eq)
              (pushnew (first p) publicSyms :test #'eq)))
           (t
            (pushnew p publicSyms :test #'eq))))
        (values publicSyms privateSyms specialSyms)))))

(defun processDeclareSyms (rest buf s e)
  (declare (ignore e))
  (let* ((pkgName (pop rest))
         (publicOrPrivate (pop rest)))
    
    (setq rest (reduce #'(lambda (current nextList)
                           (if (listp nextList)
                             (append current (cdr nextList))
                             (cons nextList current)))
                       rest
                       :initial-value nil)) ;; gracefully handle erroneous category specification
    (cond
     ((and (eql s (buffer-line-start buf s)) (every #'symbolp rest))
      (case publicOrPrivate
        (:PUBLIC rest)
        (:PRIVATE (values nil rest))
        (:SPECIAL (values nil nil rest))
        (t
         (format *error-output* "~&   WARNING: Bad type ~s. Skipping (SK8-declare-syms ~s ~s ~s...)~%"
                 publicOrPrivate pkgName publicOrPrivate (subseq (first rest) 0 (when (> (length (first rest)) 5) 5)))
         nil)))
     (t
      (format *error-output* "~&   WARNING: Non-symbol in symbol list. Skipping (SK8-declare-syms ~s ~s ~s...)~%"
              pkgName publicOrPrivate (subseq (first rest) 0 (when (> (length (first rest)) 5) 5)))
      nil))))

(defparameter *SK8-definition-types*
  `(("DEFINE-HANDLER"             1  :find-:private-and-dispatch-arg)
    ("DEFINE-SYSTEM-HANDLER"      1  :find-:private-and-dispatch-arg)
    ("DEFINE-SK8-FUNCTION"        1  2)
    ("DEFUN_X"                    1  2)
    ("DEFINE-SK8-MACRO"           1  2)
    ("DEFINE-PROJECT-FUNCTION"    2  3)
    ("DEFINE-SK8-VAR"             1  :private?)
    ("DEFINE-SK8-VAR*"           '1  :private?)
    ("DEFINE-SK8-CONSTANT"        1  :private?)
    ("DEFCONSTANT_X"              1  :private?)
    ("DEFINE-SK8-CONSTANT*"      '1  :private?)
    ("PUBLISH-PROJECT-SYMBOL"    '1  t)
    ("PUBLISH-GLOBAL-SYMBOL"     '1  t)
    ("PUBLISH-FUNCTION-SYMBOL"   '1  t)
    ("DEFINE-OPERATOR-SYNONYM"   '1  t)
    ("TAGPART"                   '3  t)
    ("ADDPROPERTY"               '2  :private)
    
    ("MAKE-CLOS-CLASS-SHADOW"     ,#'(lambda (rest buf s e)
                                        (declare (ignore buf s e))
                                        (values nil nil
                                                (if (second rest)
                                                  (intern-symbol (second rest) :SK8)
                                                  (second (first rest))))))
    
    ("NEW"                        ,#'processNewOrCopy)

    ("COPY"                       ,#'processNewOrCopy)

    ("SK8-DECLARE-SYMS"           ,#'processDeclareSyms)
    ))


;; Symbols in these forms, if in SK8 or UI, go into "SK8PrivateSyms.lisp" or "UIPrivateSyms.lisp", unless they're found among the public ones!
(defparameter *relevant-lisp-definition-types*
  '("DEFUN"
    "DEFMETHOD"
    "DEFMACRO"
    "DEFINE-COMPILER-MACRO"
    "DEFVAR"
    "DEFPARAMETER"
    "DEFCONSTANT"
    "DEFCLASS"))



;; For everyone's sanity...
(defmethod view-click-event-handler ((window listener) where)
  (call-next-method)
  (when (and (>= (point-v where) (- (point-v (view-size window)) 13))
             (<= (point-h where) 50))
    (focus-listener-package window)))

(defun focus-listener-package (window)
  (let* ((pkg (or (window-package window) (first (first MF::*SK8-Dev-packages*))))
         (dev-pkg-info (or (assq pkg MF::*SK8-Dev-packages*)
                           (first (member pkg MF::*SK8-Dev-packages* :key #'second :test #'eq))))
         (user-pkg (first dev-pkg-info))
         (dev-pkg (second dev-pkg-info)))
    (cond
     ((or (shift-key-p) (option-key-p) (command-key-p))
      (setq pkg (when dev-pkg (second (second (member pkg MF::*SK8-Dev-packages* :key #'second :test #'eq)))))
      (unless pkg (setq pkg (second (first MF::*SK8-Dev-packages*)))))
     
     (dev-pkg-info
      (setq pkg (package-name (if (eq pkg user-pkg) dev-pkg user-pkg)))))
    
    (CCL::set-package pkg)
    (set-fred-package (window-key-handler window) pkg)
    (mini-buffer-update window)))


(defun file-category (filename)
  (setq filename (namestring (truename filename)))
  (let* ((filenameLen (length filename))
         (SK8path (namestring (truename "CCL;SK8:")))
         (SK8pathLen (length SK8path))
         colonPos)
    (when (and (>= filenameLen SK8pathLen)
               (string-equal filename SK8path :end1 SK8pathLen)
               (setq colonPos (position #\: filename :start SK8pathLen :test #'eq)))
      (subseq filename SK8pathLen colonPos))))


(defvar *reusable-buffer-stream* (make-instance 'CCL::buffer-stream :buffer (make-buffer)))
(defun get-buffer-stream (buffer &optional (start 0) (end (buffer-size buffer)))
  (let* ((buf-stream *reusable-buffer-stream*)
         (endpoint-buf-marks (CCL::BUFFER-STREAM-STATE buf-stream)))
    (set-mark buffer start)
    (setf (first endpoint-buf-marks) buffer
          (second endpoint-buf-marks) (make-mark buffer end))
    buf-stream))


(defun file-read-only-p (filename)
  (let* ((truename (truename filename))
         (refnum (sk8dev::T_OpenResFile truename)))
    (unwind-protect
      (CCL::check-read-only-p truename refnum)
      (sk8dev::T_CloseResFile refnum))))


;*** USING THE CL FILE-WRITING FUNCTIONS BASHES THE FILE'S RESOURCE FORK (WHICH KILLS ITS ckid RSRC)!!!    SO WE DO IT WITH BUFFER-WRITE-FILE INSTEAD.
(defvar *temp-buffer* (make-buffer))
(defun write-SK8-symbols-file (exportsFile package symsCategoriesList private? special? &key temporary)
  (unless temporary
    (when (file-read-only-p exportsFile) (error "Can't write file ~s because it is read-only" exportsFile)))
  (let* ((pkgName (CCL::shortest-package-nickname package))
         (existingWindow (CCL::pathname-to-window exportsFile))
         (buf (cond (temporary
                     (setq temporary (fred))
                     (fred-buffer temporary))
                    (existingWindow
                     (fred-buffer existingWindow))
                    (t
                     *temp-buffer*)))
         (*print-readably* t)
         (typename (cond (private? :PRIVATE)
                         (special? :SPECIAL)
                         (t :PUBLIC)))
         column)
    (buffer-delete buf 0 (buffer-size buf))
    (let ((strm (get-buffer-stream buf))
          (indent 18))
      (format strm "~%;; Exports updated ~a~%~%(in-package :~a)~%~%~%~%" (cl-user::get-time-string) pkgName)
      (format strm "(SK8-declare-syms :~a :~a~%" pkgName typename)
      (dolist (category symsCategoriesList)
        (terpri strm)
        (dotimes (i indent) (stream-tyo strm #\Space))
        (format strm "(~s" (car category))
        (setq column 1000)
        (dolist (sym (cdr category))
          (cond
           ((> column 85)
            (terpri strm)
            (dotimes (i (1+ indent)) (stream-tyo strm #\Space))
            (setq column 0))
           (t
            (stream-tyo strm #\Space)))
          (CCL::write-pname (symbol-name sym) :upcase strm)
          (incf column (1+ (length (symbol-name sym)))))
        (unless (cdr category)
          (terpri strm)
          (dotimes (i (1+ indent)) (stream-tyo strm #\Space)))
        (stream-tyo strm #\))
        (terpri strm))
      (terpri strm)
      (dotimes (i indent) (stream-tyo strm #\Space))
      (stream-tyo strm #\)))
    
    (cond
     (temporary
      (CCL::set-fred-package temporary package)
      (fred-update temporary))
     (t
      (buffer-write-file buf exportsFile :if-exists :overwrite)
      (when existingWindow (window-set-not-modified existingWindow))))))


(defun add-to-exports-file (exportsFile symsToExport package fileCategory &key private special ensure-symbol-package ignore-old-exports temporary)
  (unless symsToExport (return-from add-to-exports-file))

  (unless temporary
    (when (file-read-only-p exportsFile) (error "Can't write file ~s because it is read-only" exportsFile))
    (when (null fileCategory)
      (error "Can't add symbols ~a to ~a without a file category (i.e. file must be in one of SK8's subfolders" symsToExport exportsFile)))
  
  (let ((symsCategoriesList nil)
        currentCategory)
    
    (unless (or ignore-old-exports temporary)
      (let ((*fetch-exports-list* t)
            (*load-verbose* nil)
            (wind (CCL::pathname-to-window exportsFile)))
        (declare (special *fetch-exports-list*))
        (when wind (window-save wind))
        (load exportsFile)
        (setq symsCategoriesList *fetch-exports-list*)))
    
    (unless (listp symsToExport) (setq symsToExport (list symsToExport)))
    (when ensure-symbol-package
      (map-into symsToExport #'(lambda (sym)
                                 (if (eq (symbol-package sym) package)
                                   sym
                                   (intern (symbol-name sym) package)))
                symsToExport))
    
    (unless (setq currentCategory (assoc fileCategory symsCategoriesList :test #'string=))
      (setq currentCategory (list fileCategory)
            symsCategoriesList (nconc symsCategoriesList (list currentCategory))))
    
    (setf (cdr currentCategory) (sort (nunion symsToExport (cdr currentCategory) :test #'eq)
                                      #'string< :key #'symbol-name))
    
    (write-SK8-symbols-file exportsfile package symsCategoriesList private special :temporary temporary)))



(defun read-from-buffer (buf &optional (start 0) (end (buffer-size buf)))
  (let ((*read-eval-suppress* t)) ; so that a #.<form> is treated as NIL
    (declare (special *read-eval-suppress*))
    (read (get-buffer-stream buf start end) nil)))


(defun safe-buffer-current-sexp (buf &optional pos)
  (let ((s (buffer-current-sexp-start buf pos)))
    (when s (read-from-buffer buf s))))


(defun buffer-package-and-bounds (buf)
  (let (pkg start end sym e temp)
    (flet ((find-form (buf pos)
             (let (start end)
               (and (setq start (buffer-skip-fwd-wsp&comments buf pos (buffer-size buf)))
                    (setq end (buffer-fwd-sexp buf start))
                    (< start end)
                    (eql (buffer-char buf start) #\()
                    (progn
                      (incf start)
                      (values (CCL::buffer-current-symbol buf start) start end)))))
           (FAIL ()
             (error "COULDN'T FIND \"IN-PACKAGE\" FORM AT TOP OF FILE")))
      

      (multiple-value-setq (sym start end) (find-form buf 0))
      (unless (string= (symbol-name sym) "IN-PACKAGE") (FAIL))
      
      (setq start (buffer-fwd-sexp buf start)
            e (when start (buffer-fwd-sexp buf start)))
      (unless e (FAIL))

      (setq temp (buffer-skip-fwd-wsp&comments buf e end))
      (unless (and temp (eq (1+ temp) end)) (FAIL))

      (setq pkg (let ((*package* CCL::*ccl-package*)) (read-from-buffer buf start e)))
      (unless pkg (FAIL))

      (setq pkg (find-package (maybeStripBackQuotes pkg)))
      
      (values pkg start e))))

; (buffer-package-and-bounds (fred-buffer (front-window)))



; (previous-comment-end (fred-buffer (target)) (buffer-size (fred-buffer (target))))

(defun previous-comment-end (buf pos)
  (if (eql 0 pos)
    0
    (loop
      (setq pos (buffer-bwd-sexp buf pos))
      (cond
       ((buffer-substring-p buf "#|" pos) (return (buffer-fwd-sexp buf pos)))
       ((eql 0 pos) (return 0))))))


; (buffer-map-occurrences (fred-buffer (target)) "#|" #'(lambda (buf s e) (print (list s e (buffer-substring buf s e)))) :include-comments t)
; (buffer-map-occurrences (fred-buffer (target)) "yo" #'(lambda (buf s e) (print (list s e (buffer-substring buf (buffer-line-start buf s) (buffer-line-end buf e))))))



(defun buffer-map-occurrences (buf targetString fcn &key include-comments valid-definition)
  (let ((len (length targetString))
        (start (buffer-size buf)))
    (unless (eql 0 start)
      (cond
       (include-comments
        (loop
          (unless (setq start (CCL::buffer-backward-search buf targetString 0 start)) (return))
          (funcall fcn buf start (+ start len))))
       (t
        (let ((sharpCommentStart start)
              sharpCommentEnd
              lineStart preDefPos defName
              commentStart
              end)
          (loop
            
            (setq sharpCommentEnd (previous-comment-end buf sharpCommentStart)
                  sharpCommentStart (buffer-bwd-sexp buf sharpCommentEnd))
            
            ;(PRINT (LIST :SEARCHING-BETWEEN SHARPCOMMENTEND 'AND START))
            
            ;; Now search backwards within the range [sharpCommentEnd, start]
            (loop
              (unless (setq start (CCL::buffer-backward-search buf targetString sharpCommentEnd start)) (return))
              (setq end (+ start len))
              (setq lineStart (buffer-line-start buf start))
              (when (or (and (not (setq commentStart (CCL::buffer-find-comment buf lineStart)))
                             (not (setq commentStart (CCL::buffer-find-comment buf (buffer-line-start buf end)))))
                        (< start commentStart))
                (when (or (not valid-definition)
                          (and (setq preDefPos (CCL::buffer-backward-find-char buf #\( lineStart start))
                               (decf preDefPos)
                               (or (< preDefPos lineStart)
                                   (not (memq (buffer-char buf preDefPos) '(#\" #\`))))
                               (setq defName (ignore-errors (safe-buffer-current-sexp buf (+ preDefPos 2))))
                               (symbolp defName)
                               (string= (symbol-name defName) targetString)))
                  (funcall fcn buf start end))))
            
            (when (eql 0 sharpCommentEnd) (return))
            (setq start sharpCommentStart))))))))


(defun buffer-nth-sexp (buf n start)
  (let ((bufsize (buffer-size buf)))
    (ignore-errors
     (loop
       (unless (setq start (buffer-skip-fwd-wsp&comments buf start bufsize)) (return))
       (decf n)
       (when (eql 0 n)
         (return (values (read-from-buffer buf start) start)))
       (setq start (buffer-fwd-sexp buf start))))))


;;;
;;; As a side-effect, calls ADD-TO-EXPORTS-FILE to add any needed exports to the appropriate export file
;;;
(defun buffer-update-dev-package-and-exports (buf fileCategory &key (update t))
  (multiple-value-bind (file-pkg s e) (buffer-package-and-bounds buf)
    (declare (ignore s e))
    (let* ((original-buf-pos (buffer-position buf))
           (dev-pkg-info (or (assq file-pkg *SK8-Dev-packages*)
                             (first (member file-pkg *SK8-Dev-packages* :key #'second :test #'eq))))
           (user-pkg (first dev-pkg-info))
           ;(dev-pkg (second dev-pkg-info))
           ;(dev-pkg-name (third dev-pkg-info))
           ;(pkg-project-name (fourth dev-pkg-info))
           ;(pkg-exports-filename (fifth dev-pkg-info))
           
           (*PACKAGE* (or user-pkg file-pkg))
           (public-syms nil)
           (private-syms nil)
           (special-syms nil))
      
      (dolist (defName *relevant-lisp-definition-types*)
        (buffer-map-occurrences buf defName
                                #'(lambda (buf start end)
                                    (declare (ignore start))
                                    (block nil
                                      (let ((name (buffer-nth-sexp buf 1 end)))
                                        (unless name
                                          (format *error-output* "~&   WARNING: Bad ~a form~%" defName)
                                          (return))
                                        (cond
                                         ((symbolp name))
                                         ((and (listp name) (symbolp (first name)) (string-equal (symbol-name (first name)) "SETF"))
                                          (setq name (second name)))
                                         (t
                                          (format *error-output* "~&   WARNING: Unexpected expr ~s in ~a form~%" name defName)
                                          (return)))
                                        (when (or user-pkg (neq file-pkg (symbol-package name)))
                                          (pushnew name private-syms :test #'eq)))))
                                :valid-definition t))
      
      
      (let (defName namePos symsFunction requireQuote? privateAndDispatchInfo findPrivateAndDispatchObj? privateIndicator)
        (dolist (SK8-def *SK8-Definition-types*)
          (setq defName (pop SK8-def)
                namePos (pop SK8-def)
                privateAndDispatchInfo (first SK8-def)
                findPrivateAndDispatchObj? (eq :FIND-:PRIVATE-AND-DISPATCH-ARG privateAndDispatchInfo)
                privateIndicator (unless findPrivateAndDispatchObj? privateAndDispatchInfo)
                symsFunction (when (functionp namePos) namePos)
                requireQuote? (stripBackQuotes namePos))
          (when requireQuote?
            (setf namePos requireQuote?
                  requireQuote? t))
          (buffer-map-occurrences buf defName
                                  (if symsFunction
                                    
                                    #'(lambda (buf start end)
                                        (setq start (CCL::buffer-backward-find-char buf #\( 0 start)
                                              end (buffer-fwd-sexp buf start))
                                        (let ((args (cdr (read-from-buffer buf start end))))
                                          (multiple-value-bind (publics privates specials)
                                                               (ignore-errors
                                                                (funcall symsFunction args buf start end))
                                            (cond
                                             ((and (null publics) (typep privates 'error))
                                              (warn ">>> Skipping ~s form with args ~s due to error:~%~%~a~%" defName args privates))
                                             (t
                                              (if (listp publics)
                                                (dolist (s publics) (pushnew s public-syms :test #'eq))
                                                (pushnew publics public-syms :test #'eq))
                                              (if (listp privates)
                                                (dolist (s privates) (pushnew s private-syms :test #'eq))
                                                (pushnew privates private-syms :test #'eq))
                                              (if (listp specials)
                                                (dolist (s specials) (pushnew s special-syms :test #'eq))
                                                (pushnew specials special-syms :test #'eq)))))))
                                    
                                    #'(lambda (buf start end)
                                        (block nil
                                          (let ((setter? nil)
                                                (private? nil)
                                                name obj)
                                            (multiple-value-setq (name start) (buffer-nth-sexp buf namePos end))
                                            (unless name
                                              (format *error-output* "~&   WARNING: Bad ~a form~%" defName)
                                              (return))
                                            (flet ((withHandNam? (nam)
                                                     (let ((nameString (when (symbolp nam) (symbol-name nam))))
                                                       (and (> (length nameString) 5) (string= "WITH " nameString :end2 5)))))
                                              (cond
                                               ((symbolp name)
                                                (if requireQuote?
                                                  (setq name nil)
                                                  (when (withHandNam? name) (setq setter? t))))
                                               ((let ((nm (stripBackQuotes name)))
                                                  (when nm (setf name nm)))
                                                (when (withHandNam? name) (setq setter? t)))
                                               ((and (listp name) (symbolp (first name)) (string-equal (symbol-name (first name)) "SETF"))
                                                (setq setter? t
                                                      name (second name)))
                                               (t
                                                (return))))
                                            
                                            (when name
                                              (cond
                                               
                                               (findPrivateAndDispatchObj?
                                                (let ((bufsize (buffer-size buf))
                                                      sexp)
                                                  (ignore-errors
                                                   (loop
                                                     (setq sexp nil)
                                                     (unless (setq start (buffer-fwd-sexp buf start)) (return))
                                                     (unless (setq start (buffer-skip-fwd-wsp&comments buf start bufsize)) (return))
                                                     (setq sexp (read-from-buffer buf start))
                                                     (if (eq :PRIVATE sexp)
                                                       (setq private? t)
                                                       (when (consp sexp) (return)))))
                                                  (if sexp
                                                    (when (symbolp (setq obj (if setter? (second sexp) (first sexp))))
                                                      ;; *** OK TO ASSUME OBJ IS PUBLIC?  MAYBE IT'S MORE CORRECT TO ASSUME IT'S PRIVATE?!
                                                      (pushnew obj public-syms :test #'eq))
                                                    (format *error-output* "~&   WARNING: Couldn't find dispatch argument in (~a ~s ...) form... it won't be declared private or public!~%"
                                                            defName name))))
                                               
                                               ((eq T privateIndicator)
                                                (setq private? nil))
                                               
                                               ((numberp privateIndicator)
                                                (setq private? (buffer-nth-sexp buf privateIndicator end)
                                                      private? (not (and (symbolp private?) (string= "NIL" (symbol-name private?))))))
                                               
                                               (t ; it's a keyword arg to look for
                                                (let* ((form (safe-buffer-current-sexp buf (CCL::buffer-backward-find-char buf #\( 0 start)))
                                                       (keywordArgs (nthcdr (1+ namePos) form)))
                                                  (when (oddp (length keywordArgs)) (setq keywordArgs (cdr keywordArgs)))
                                                  (setq private? (eq T (getf keywordArgs privateIndicator))))))
                                              (if private?
                                                (pushnew name private-syms :test #'eq)
                                                (pushnew name public-syms :test #'eq)))))))
                                  
                                  :valid-definition t)))
      
      #|
      ;; Move "ugly" symbols that were in the public list into the private list
      (setq public-syms (delete-if #'(lambda (sym)
                                       (let ((symname (symbol-name sym)))
                                         (when (or (not (alpha-char-p (char symname 0)))
                                                   (position #\- symname :test #'eq)
                                                   (position #\* symname :test #'eq))
                                           (pushnew sym private-syms :test #'eq)
                                           t)))
                                   public-syms))
      |#
      
      ;; Get rid of syms that aren't in a package of interest to us (and also syms named NIL)
      (flet ((in-interesting-package? (sym)
               (unless (string= "NIL" (symbol-name sym))
                 (let ((pkg (symbol-package sym)))
                   (cond
                    ((assq pkg *SK8-Dev-packages*)
                     t)
                    ((member pkg *SK8-Dev-packages* :key #'second :test #'eq)
                     t)
                    ((and user-pkg (or (eq pkg CCL::*common-lisp-package*) (eq pkg CCL::*ccl-package*)))
                     (find-symbol (symbol-name sym) user-pkg)))))))
        (setq public-syms (delete-if-not #'in-interesting-package? public-syms))
        (setq private-syms (delete-if-not #'in-interesting-package? private-syms))
        (setq special-syms (delete-if-not #'in-interesting-package? special-syms)))

      ;; Remove private-syms that are also in public-syms
      (setq public-syms (delete-if #'(lambda (s) (memq s special-syms)) public-syms))
      (setq private-syms (delete-if #'(lambda (s) (or (memq s public-syms) (memq s public-syms))) private-syms))
      
      ;; Finally, stick the syms into their appropriate files
      (let (user-pkg dev-pkg pkg-public-syms-filename pkg-private-syms-filename pkg-special-syms-filename
                     pkg-public-syms pkg-private-syms pkg-special-syms)
        (dolist (pkg-info *SK8-Dev-packages*)
          (setq user-pkg (first pkg-info)
                dev-pkg (second pkg-info)
                pkg-public-syms-filename (fifth pkg-info)
                pkg-private-syms-filename (sixth pkg-info)
                pkg-special-syms-filename (seventh pkg-info))
          (if (eq user-pkg (find-package :SK8))
            (setq pkg-public-syms (remove-if-not #'(lambda (pkg) (or (eq pkg user-pkg) (eq pkg dev-pkg)
                                                                     (eq pkg CCL::*common-lisp-package*)
                                                                     (eq pkg CCL::*ccl-package*)))
                                                 public-syms :key #'symbol-package)
                  pkg-private-syms (remove-if-not #'(lambda (pkg) (or (eq pkg user-pkg) (eq pkg dev-pkg)
                                                                      (eq pkg CCL::*common-lisp-package*)
                                                                      (eq pkg CCL::*ccl-package*)))
                                                  private-syms :key #'symbol-package)
                  pkg-special-syms (remove-if-not #'(lambda (pkg) (or (eq pkg user-pkg) (eq pkg dev-pkg)
                                                                      (eq pkg CCL::*common-lisp-package*)
                                                                      (eq pkg CCL::*ccl-package*)))
                                                  special-syms :key #'symbol-package))
            (setq pkg-public-syms (remove-if-not #'(lambda (pkg) (or (eq pkg user-pkg) (eq pkg dev-pkg)))
                                                 public-syms :key #'symbol-package)
                  pkg-private-syms (remove-if-not #'(lambda (pkg) (or (eq pkg user-pkg) (eq pkg dev-pkg)))
                                                  private-syms :key #'symbol-package)
                  pkg-special-syms (remove-if-not #'(lambda (pkg) (or (eq pkg user-pkg) (eq pkg dev-pkg)))
                                                  special-syms :key #'symbol-package)))
          (add-to-exports-file pkg-public-syms-filename pkg-public-syms user-pkg fileCategory
                               :ensure-symbol-package t :temporary (not update))
          (add-to-exports-file pkg-private-syms-filename pkg-private-syms user-pkg fileCategory
                               :ensure-symbol-package t :temporary (not update) :private t)
          (add-to-exports-file pkg-special-syms-filename pkg-special-syms user-pkg fileCategory
                               :ensure-symbol-package t :temporary (not update) :special t)))
      
      (set-mark buf original-buf-pos))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(comtab-set-key *control-x-comtab* '(:meta #\x) 'window-compute-symbol-declarations)
(defun window-compute-symbol-declarations (wind)
  (with-cursor *watch-cursor*
    (let* ((filename (window-filename wind))
           (fileCategory (or (when filename (file-category filename)) "No category")))
      (buffer-update-dev-package-and-exports (fred-buffer wind) fileCategory :update nil))))


;(comtab-set-key *control-x-comtab* '(:control :meta #\x) 'window-replace-dev-package-and-exports)
;(defun window-replace-dev-package-and-exports (wind)
;  (buffer-update-dev-package-and-exports (fred-buffer wind) "New" :delete-old-exports t))



#|
;; Find any existing SK8-export forms and delete them
(defun buffer-delete-old-SK8-exports (buf)
  (buffer-map-occurrences buf #.(format nil "~%;;; It's possession or use,")
                          #'(lambda (buf start end)
                              (declare (ignore end))
                              (buffer-delete buf (+ start 7) (+ start 8)))
                          :include-comments t)
  
  (let (oldExports)
    (buffer-map-occurrences buf #.(format nil "~%(SK8-export :")
                            #'(lambda (buf start end)
                                (let* ((prevLineStartPos (buffer-line-start buf start))
                                       (formStart (buffer-skip-fwd-wsp&comments buf start (buffer-size buf))))
                                  (when (and formStart
                                             (setq end (buffer-fwd-sexp buf formStart))
                                             (< formStart end))
                                    (cond
                                     ((and (buffer-substring-p buf "#|" prevLineStartPos)
                                           (buffer-substring-p buf "|#" (1+ end)))
                                      (decf start 2)
                                      (incf end 3))
                                     (t
                                      (setq oldExports (cddr (read-from-buffer buf start end)))))
                                    (loop
                                      (if (buffer-substring-p buf (string #\Newline) (1- start))
                                        (decf start)
                                        (return)))
                                    (when (buffer-substring-p buf (string #\Newline) (1+ end)) (incf end))
                                    (buffer-delete buf start end))))
                            :include-comments t)
    oldExports))
|#


(defun buffer-SK8-exports-and-bounds (buf user-pkg typename)
  (let ((search-string (format nil "(SK8-declare-syms :~a :~a"
                               (CCL::shortest-package-nickname user-pkg) typename)))
    (buffer-map-occurrences buf search-string
                            #'(lambda (buf start end)
                                (setq end (buffer-fwd-sexp buf start))
                                (let ((*package* user-pkg))
                                  (return-from buffer-SK8-exports-and-bounds
                                    (values (safe-buffer-current-sexp buf start) start end)))))
    nil))


(defun buffer-add-symbol-declarations (buf user-pkg sym-or-syms typename)
  
  (multiple-value-bind (decls-form start end) (buffer-SK8-exports-and-bounds buf user-pkg typename)
    (let ((existing-syms nil))
      (cond
       (decls-form
        (setq existing-syms (cdddr decls-form))
        (buffer-delete buf start end))
       (t
        (setq start (buffer-line-start buf (nth-value 1 (buffer-package-and-bounds buf)) 1))
        (buffer-insert buf #.(format nil "~%~%~%") start)
        (incf start)))
      
      (setq existing-syms (if (listp sym-or-syms)
                            (union sym-or-syms existing-syms :test #'equal :key #'symbol-name)
                            (adjoin sym-or-syms existing-syms :test #'equal :key #'symbol-name))
            existing-syms (sort existing-syms #'string< :key #'symbol-name))
      
      (let* ((*print-readably* t)
             (original-buf-pos (buffer-position buf))
             (buf-stream (get-buffer-stream buf start))
             (indent 18)
             (column 1000)
             (user-pkg-name (CCL::shortest-package-nickname user-pkg))
             (prefix (concatenate 'string user-pkg-name "::"))
             (prefix-length (1+ (length prefix))))
        (format buf-stream "(SK8-declare-syms :~a :~a ; Updated ~a"
                user-pkg-name typename (cl-user::get-time-string))
        (buffer-set-font-spec buf '("Geneva" :bold 9)
                              (+ 1 (CCL::buffer-forward-search buf #\;
                                                               (buffer-line-start buf (CCL::stream-position buf-stream))
                                                               (buffer-size buf)))
                              (CCL::stream-position buf-stream))
        (dolist (sym existing-syms)
          (cond
           ((> column 75)
            (terpri buf-stream)
            (dotimes (i indent) (stream-tyo buf-stream #\Space))
            (setq column 0))
           (t
            (stream-tyo buf-stream #\Space)))
          (write-string prefix buf-stream)
          (write-string (symbol-name sym) buf-stream)
          (incf column (+ prefix-length (length (symbol-name sym)))))
        
        (unless existing-syms
          (terpri buf-stream)
          (dotimes (i indent) (stream-tyo buf-stream #\Space)))
        (stream-tyo buf-stream #\))
        
        (set-mark buf original-buf-pos)))))



(defun make-SK8-user-sym-available (sym user-pkg dev-pkg)
  (setq user-pkg (require-type (find-package user-pkg) 'package)
        dev-pkg (require-type (find-package dev-pkg) 'package))
  (let ((dev-sym (find-symbol (symbol-name sym) dev-pkg)))
    (cond
     
     ((or (null dev-sym) (eq dev-sym sym))
      (import sym dev-pkg)
      t)
     
     ((eq (symbol-package dev-sym) dev-pkg)
      (when (or (boundp sym) (fboundp sym) (find-class sym nil))
        (format t "~%~%~%")
        (cerror "Unintern '~3*~a' from ~a"
                "Can't import '~a' from ~a into ~a because '~a' in ~a contains data!~
                 ~%[Continue if you want to unintern '~a' from ~a.]"
                sym (CCL::shortest-package-nickname user-pkg) (CCL::shortest-package-nickname dev-pkg)
                sym (CCL::shortest-package-nickname dev-pkg)
                sym (CCL::shortest-package-nickname dev-pkg)))
      (unintern sym dev-pkg)
      (import sym dev-pkg)
      t)
     
     (t
      (error "Can't import '~a' from ~a into ~a because ~a gets '~a' from ~a!"
             sym (CCL::shortest-package-nickname user-pkg) (CCL::shortest-package-nickname dev-pkg)
             (CCL::shortest-package-nickname dev-pkg) sym (CCL::shortest-package-nickname (symbol-package dev-sym)))))))


(defun export-SK8-user-sym (sym user-pkg)
  (setq user-pkg (require-type (find-package user-pkg) 'package))
  (tagbody
    :RETRY
    (let* ((sym-holder (list sym))
           (conflicts (CCL::check-export-conflicts sym-holder user-pkg))
           s)
      (declare (dynamic-extent sym-holder))
      (cond
       ((dolist (c conflicts)
          (setq s (fourth c))
          (when (or (boundp s) (fboundp s) (find-class s nil)) (return t)))
        (format t "~%~%~%")
        (cerror "Unintern '~2*~a' from ~a"
                "Can't export '~a' from ~a because '~a' in ~a contains data!~
                 ~%[Continue if you want to unintern '~a' from ~a.]"
                sym (CCL::shortest-package-nickname user-pkg)
                s (CCL::shortest-package-nickname (symbol-package s))
                s (CCL::shortest-package-nickname (symbol-package s)))
        (unintern s (symbol-package s))
        (go :RETRY))
       (t
        (when conflicts
          (dolist (c conflicts)
            (unintern (fourth c) (third c))
            (unless (proclaimed-special-p (fourth c)) (setf (fourth c) nil)))
          (setq conflicts (delete-if #'null conflicts :key #'fourth))
          (when conflicts
            (format t "~%~%~%")
            (warn "The symbol named '~a' has been uninterned from the package~
                   ~:[ ~a~;s ~{~a~^, ~}~]~
                   ~%so please make sure you re-eval any code that might refer to the old symbol!!!"
                  (symbol-name sym) (cdr conflicts)
                  (if (cdr conflicts) (mapcar #'third conflicts) (third (first conflicts))))))
        ;(export sym user-pkg)
        (if (fboundp 'publish-project-symbol)
          (publish-project-symbol sym user-pkg)
          (export sym user-pkg))))))
  t)


(defun window-declare-user-symbol (wind private? &aux special?)
  (let* ((buf (fred-buffer wind))
         (original-buf-pos (buffer-position buf))
         (wind-pkg (or (buffer-package-and-bounds buf) (window-package wind) *package*))
         (dev-pkg-info (or (first (member wind-pkg *SK8-Dev-packages* :key #'second :test #'eq))
                           (assq wind-pkg *SK8-Dev-packages*)))
         (user-pkg (first dev-pkg-info))
         (dev-pkg (second dev-pkg-info))
         sym)
    
    (unwind-protect
      (progn
        (let ((*PACKAGE* (or user-pkg wind-pkg)))
          (setq sym (safe-buffer-current-sexp buf original-buf-pos)))
        
        (cond
         ((and (listp sym) (eq 'SK8-DECLARE-SYMS (first sym)))
          (unless (or (window-filename wind)
                      (and (listp (fourth sym)) (stringp (first (fourth sym)))))
            (error "Don't know what file-category this anonymous window is in"))
          (setq user-pkg (find-package (second sym))
                dev-pkg-info (assq user-pkg *SK8-Dev-packages*))
          (setq private? (eq :PRIVATE (third sym)))
          (setq special? (eq :SPECIAL (third sym)))
          (let ((filename (cond (private? (sixth dev-pkg-info))
                                (special? (seventh dev-pkg-info))
                                (t (fifth dev-pkg-info)))))
            (set-mini-buffer wind "Adding these symbols to ~s..." filename)
            (if (listp (fourth sym))
              (dolist (category-and-syms (cdddr sym))
                (add-to-exports-file filename (cdr category-and-syms) user-pkg (first category-and-syms) :private private? :special special?))
              (add-to-exports-file filename (cdddr sym) user-pkg (file-category (window-filename wind)) :private private? :special special?))
            (let ((s (buffer-current-sexp-start buf original-buf-pos)))
              (buffer-insert buf (format nil ";; Symbols inserted into the appropriate preload file ~a~%" (cl-user::get-time-string)) s)
              (buffer-set-font-spec buf '("Geneva" :plain 9) (+ s 3) (buffer-line-end buf s)))
            (set-mini-buffer wind "Added to ~s." filename)))
         
         (t
          (unless (and (symbolp sym) (not (keywordp sym)) (neq sym nil) (neq sym t))
            (cond
             ((let ((sm (stripBackQuotes sym)))
                (when sm (setq sym (second sym)))))
             ((and (listp sym) (symbolp (first sym)) (string-equal (symbol-name (first sym)) "SETF"))
              (setq sym (second sym)))
             (t
              (ed-beep)
              (set-mini-buffer wind ">>> NOT A VALID SYMBOL: ~s" sym)
              (return-from window-declare-user-symbol))))
          
          (when (or (member (symbol-name sym) *SK8-Definition-types* :key #'first :test #'string=)
                    (member (symbol-name sym) *relevant-lisp-definition-types* :test #'string=))
            (set-mini-buffer wind "You don't really want to export '~a'" (symbol-name sym))
            (return-from window-declare-user-symbol))
          
          (unless user-pkg
            (setq user-pkg (symbol-package sym)
                  dev-pkg-info (or (first (member user-pkg *SK8-Dev-packages* :key #'second :test #'eq))
                                   (assq user-pkg *SK8-Dev-packages*))
                  user-pkg (first dev-pkg-info)
                  dev-pkg (second dev-pkg-info))
            (unless user-pkg
              (set-mini-buffer wind "The symbol ~s isn't in one of the user packages" sym)
              (return-from window-declare-user-symbol)))
            
          (when (if private?
                  (make-SK8-user-sym-available sym user-pkg dev-pkg)
                  (export-SK8-user-sym sym user-pkg))
            (when special? (proclaim (list 'special sym)))
            (set-mini-buffer wind (if private? "Made '~a' available in ~a's development package" "Exported '~a' from ~a package")
                             (symbol-name sym) (CCL::shortest-package-nickname user-pkg))
            (buffer-add-symbol-declarations buf user-pkg sym (cond (private? :PRIVATE)
                                                                   (special? :SPECIAL)
                                                                   (:PUBLIC)))))))
      
      (set-mark buf original-buf-pos))))


(defun window-declare-sym (wind)
  (with-cursor *watch-cursor*
    (window-declare-user-symbol wind (shift-key-p))))


(comtab-set-key *comtab* '(:meta #\x) 'window-declare-sym)



#|
	Change History (most recent last):
	1	7/6/93	chip	new file
	10	8/19/93	kleiman	added define-sk8-macro & expanded an error msg
	12	8/30/93	chip	Fixed scan-behavior for SK8-DECLARE-SYMS; added behavior for IN-PACKAGE & SETCURRENTPROJECT
	13	8/31/93	chip	added package-swith (click handler) for Listener
	14	9/3/93	chip	option-x now works on the test-scan results (in unnamed windows); also now updates any open syms window in addition to on-disk version
	15	9/17/93	chip	add-to-exports-file now correctly deals with syms files opened in fred-windows; fixed bug in window-declare-user-symbol that ignored the package of the given symbol
	16	9/17/93	chip	export-SK8-user-sym now uses publish-project-symbol instead of export
	17	9/24/93	chip	
	18	9/28/93	chip	added ADDPROPERTY to *SK8-definition-types*
	19	11/8/93	kleiman	extended the package-switch (click handler) for the Listener to cycle throught the known dev-packages when a modifier key is down
	20	11/15/93	chip	fixed listener package utility to work when listener has NO package
	21	11/17/93	chip	now uses *read-eval-suppress* to safely read forms (with the help of the "read-eval-patch"), instead of requiring the "stubs" file
	22	11/22/93	It 	declared *read-eval-suppress* special
	23	12/14/93	sidney	Moved definition of *sk8-dev-packages* to loader file so that it can be defined even when building a non-developer version
	24	12/21/93	sidney	Changes so files can be compiled
	25	1/17/94	sidney	Modify scanner to deal with backquotes
	26	1/17/94	kleiman	Fix typo in last checkin
	27	2/18/94	sidney	Duplicate new as duplicate
	28	2/20/94	sidney	Add new category of symbols: Specials are used for SK8 object names to proclaim them special
	29	2/21/94	hernan	The Final Renaming for Alpha! (yeah, right...)
	30	2/22/94	hernan	duplicate -> copy.
	31	2/25/94	sidney	Fix :public being written in specials file instead of :special sometimes
				Move defparameter back in here because it isn't used outside development versions anymore
				Remove no longer needed special searches for in-package and setcurrentpackage
				Change sk8::package to package because they end up the same and the former screws up when this is loaded before the preloads
				Remove dependency on a function in the Sk8Script compiler code so this can be loaded and run earlier
	32	2/25/94	dy	Add defun_X and devconstant_X
	33	3/3/94	sidney	Make warning more informative, not generate error on short lists
	34	3/3/94	sidney	gracefully handle sk8-declare-syms containing category strings
				ignore errors parsing individual expressions without rather than skipping entire file
	35	4/11/94	rod	made error-handling a bit safer
	36	6/29/94	chip	now writes out scanned symbols in their original case instead of upcased (for radar #107741)
	37	7/8/94	chip	write-pname is now used instead of simply writing the symbol-name in write-SK8-symbols-file
	38	8/2/94	Hernan	intern --> intern-symbol
	39 	 9/ 1/94	chip    	added PUBLISH-GLOBAL-SYMBOL & PUBLISH-FUNCTION-SYMBOL to *SK8-definition-types* (radar #1183935)
	40 	11/16/94	chip    	added DEFINE-OPERATOR-SYNONYM to definition-types
	41 	12/ 9/94	till    	Color Fred:
							changes to buffer-add-symbol-declarations
							changes to window-declare-user-symbol
	42 	 1/17/95	till    	trapwrappers for OpenResFile, CloseResFile
	43 	 3/ 1/95	Hernan  	buffer-set-font-spec lost its color argument.
	44 	 3/ 3/95	Hernan  	Defining the function that turns the listener into
							a useful entity by focusing on the sk8dev package.
	2  	 6/ 8/95	sidney  	MCL 3.0 changes
	2  	 2/27/97	Hernan  	
|# ;(do not edit past this line!!)
