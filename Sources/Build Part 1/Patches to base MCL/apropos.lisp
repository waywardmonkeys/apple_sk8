;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories
(in-package :ccl)

;;; Improved sorting of apropos results and
;;; improved formatting of apropos printout
;;; Dave Yost, Apple  94-11-13

(defun find-alpha-char (str)
  "returns the character position of the first
alphabetic character in str, or the length of str
if it contains no alphabetic characters."
  (setq str (string-arg str))
  (loop for ind from 0 below (length str)
        when (alpha-char-p (schar str ind))
        return ind
        finally return ind))
#|
(find-alpha-char "a")
(find-alpha-char "$a")
(find-alpha-char "$")
|#

(defun find-sym-alpha-part (sym)
  (let* ((str (prin1-to-string sym))
         (sortOffset (let ((sym-start (if (find #\: str)
                                      (loop for ind from (1- (length str)) downto 0
                                            when (eql #\: (char str ind))
                                            return (1+ ind))
                                      0)))
                     (+ sym-start (find-alpha-char (subseq str sym-start))))))
    (values str sortOffset sortOffset)))
#|
(find-sym-alpha-part '|a::b|)
(find-sym-alpha-part '%abc)
|#

(defun find-str-in-sym (str sym)
  (let* ((symStr (string-arg (prin1-to-string sym)))
         (sortOffset (let ((sym-start (if (find #\: str)
                                      (loop for ind from (1- (length str)) downto 0
                                            when (eql #\: (char str ind))
                                            return (1+ ind))
                                      0)))
                     (+ sym-start (find-alpha-char (subseq str sym-start)))))
         (displayOffset (let ((sym-start (if (find #\: symStr)
                                       (or (loop for ind from (1- (length symStr)) downto 0
                                             when (eql #\| (schar symStr ind))
                                             do (setf ind (loop for ind2 from (1- ind) downto 0
                                                                when (eql #\| (schar symStr ind2))
                                                                return ind2))
                                             when (eql #\: (char symStr ind))
                                             return (1+ ind))
                                           0)
                                       0)))
                      (+ sym-start (search (string-upcase str) (string-upcase (subseq symStr sym-start)))))))
    (values symStr sortOffset displayOffset)))
#|
(find-str-in-sym "c" '|a::bc|)
(find-str-in-sym "b" '%abc)
(find-str-in-sym "b" '%ABC)
(find-str-in-sym "find" ':|ABC_FIND-:PRIVATE|)
|#

;;; The following should eventually be user-selectable on the Apropos dialog
(defvar *apropos-indent-to-search-string* nil)

(defun sort-symbol-list (theList search-string)
  ;;; First precompute the stylized string form of the symbols as they will be compared
  ;;; and calculate the maximum indent
  (multiple-value-bind (tmpVector indentation)
                       (let (sortOffset
                             displayOffset
                             str)
                         (loop for x in theList
                               do (multiple-value-setq (str sortOffset displayOffset)
                                    (if search-string
                                      (find-str-in-sym search-string x)
                                      (find-sym-alpha-part           x)))
                               maximize displayOffset into indentation1
                               collect `#(,x ,(string-arg (subseq str sortOffset)) ,sortOffset ,displayOffset) into tmpList1
                               finally return (values `#(,@tmpList1) indentation1)))
    (sort tmpVector #'(lambda (symPair1 symPair2)
                       (string-lessp (aref symPair1 1) (aref symPair2 1))))
    (values tmpVector ; each element is a vector of `#(,sym sortable-string-for-sym)
            indentation)))

;;;-------------------------------------------------
;;; now for redefined versions of the standard stuff

(let ((ccl::*warn-if-redefine-kernel* nil)
      (ccl::*warn-if-redefine*        nil))
  
  (defun apropos-list-aux (theString package indent-to-search-string &aux theList)
    (setq theString (string-arg theString))
    (if package
      (do-symbols (sym package)
        (when (%apropos-substring-p theString (symbol-name sym))
          (push sym theList)))
      (do-all-symbols (sym)
        (when (%apropos-substring-p theString (symbol-name sym))
          (push sym theList))))
    (let* ((last 0)                      ; not a symbol
           (junk #'(lambda (item)
                     (declare (debugging-function-name nil))
                     (or (eq item last) (progn (setq last item) nil)))))
      (declare (dynamic-extent junk))
      (sort-symbol-list (delete-if junk theList) (if indent-to-search-string
                                                   theString
                                                   nil))))
  
  (defun apropos-string-indented (symTuple indent)
    (let ((pr-string     (prin1-to-string (aref symTuple 0)))
          (displayOffset (aref symTuple 3)))
      (format nil "~v@a~a"
              indent
              (subseq pr-string 0 displayOffset)
              (subseq pr-string displayOffset))))
  
  (defun apropos-list (theString &optional package)
    (multiple-value-bind (symVector indent) (apropos-list-aux theString package *apropos-indent-to-search-string*)
      (loop for symTuple across symVector
            collect (list (apropos-string-indented symTuple indent) (aref symTuple 0)))))
  
  (defun apropos-aux (theString symtuple indent &aux val sym)
    (setf sym (aref symtuple 0))
    (when (%apropos-substring-p theString (symbol-name sym))
      (format t "~a" (apropos-string-indented symtuple indent))
      (when (setq val (fboundp sym))
        (cond ((functionp val)
               (princ ", Def: ")
               (prin1 (type-of val)))
              ((setq val (macro-function sym))
               (princ ", Def: MACRO ")
               (prin1 (type-of val)))
              (t (princ ", Special form"))))
      (when (boundp sym)
        (princ ",  Value: ")
        (prin1 (symbol-value sym)))
      (terpri)))
  
  (defun apropos (theString &optional package)
    (multiple-value-bind (symVector indent) (apropos-list-aux theString package *apropos-indent-to-search-string*)
      (loop for symtuple across symVector
        do (apropos-aux theString symtuple indent))
      (values)))
  
  ;;;--------------------------------------------------------------------------------
  ;;; from apropos-dialog.lisp
  
  (defun apropos-cell-contents-print (contents theStream)
    (format theStream "~A" (first contents)))
  
  (defun apropos-cell-contents-symbol (table-item cell)
    (second (cell-contents table-item cell)))
  
  ;;; DY Todo:
  ;;;   Provide popup to sort by package, symbol, or first alpha in symbol
  ;;;   Provide popup to indent by package, symbol, first alpah in symbol, or search string
  ;;;   display the search string in bold
  (defun apropos-dialog (&optional (apropos-string "") &aux (d *apropos-dialog*))
    (unless d
      (setq d
            (make-instance 'apropos-dialog
              :window-title "Apropos"
              :view-position (cdr *apropos-dialog-size&position*)
              :window-type :document-with-grow
              :window-show nil
              :help-spec 14010))
      (add-subviews
       d
       (make-instance 'static-text-dialog-item
         :dialog-item-text "Substring to search for:"
         :view-position #@(3 3)
         :view-size #@(160 16)
         :help-spec 14010)
       (setf (slot-value d 'text-item)
             (make-instance 'editable-text-dialog-item
               :view-position #@(5 23)
               :view-size #@(233 17)
               :dialog-item-text apropos-string
               :help-spec 14011))
       (make-instance 'default-button-dialog-item
         :dialog-item-text "Apropos"
         :view-position #@(7 49)
         :view-size #@(60 16)
         :dialog-item-enabled-p (not (eq  0 (length apropos-string)))
         :help-spec 14012
         :dialog-item-action #'(lambda (item) 
                                 (apropos-search
                                  (view-container item))))
       (setf (slot-value d 'inspect-button)
             (make-instance 'button-dialog-item
               :dialog-item-text "Inspect"
               :view-position #@(86 49)
               :view-size #@(60 16)
               :dialog-item-enabled-p nil
               :help-spec 14013
               :dialog-item-action 
               #'(lambda (item)
                   (with-slot-values (table-item)
                                     (view-container item)
                     (inspect (apropos-cell-contents-symbol
                               table-item
                               (first-selected-cell table-item)))))))
       (setf (slot-value d 'table-item)
             (make-instance ' sequence-dialog-item
               :dialog-item-text "dummy"
               :view-position #@(0 73)
               :view-size #@(234 94)
               :help-spec 14014
               :dialog-item-action
               #'(lambda (item)
                   (without-interrupts
                    (if (double-click-p)
                      (let* ((table-item (slot-value (view-container item)
                                                     'table-item))
                             (cell (first-selected-cell table-item)))
                        (when cell
                          (inspect (apropos-cell-contents-symbol table-item cell)))))))
               :table-sequence nil
               :table-print-function #'apropos-cell-contents-print
               )))
      (set-view-size d (car *apropos-dialog-size&position*))
      (setq *apropos-dialog* d))
    (window-select d))
  )
#|
	Change History (most recent last):
	1  	11/14/94	dy      	
	2  	12/14/94	dy      	add *apropos-indent-to-search-string* functionality
	3  	12/18/94	sidney  	it helps to pass the correct arguments in a function call
	2  	 2/27/97	Hernan  	
|# ;(do not edit past this line!!)
