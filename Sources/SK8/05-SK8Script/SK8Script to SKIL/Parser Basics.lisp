;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories


;;;
(in-package :skil)


;;;;---------------------------------------------------------------------------
;;;;Some basic token functions
;;;;---------------------------------------------------------------------------

(defparameter input-tokens nil)
(defparameter comment-tokens nil)

;;______________________________________________________________________________________________

(defparameter tokenizer-current-line 0)
(defparameter tokenizer-current-line-location 0)

(defun make-sk8-token (obj startCnt EndCnt)
  (vector 'sk8-token 
          obj
          startcnt
          endcnt
          tokenizer-current-line 
          (- startcnt tokenizer-current-line-location)
          (- endcnt tokenizer-current-line-location)
          nil  ;type
          nil  ;corresponding token
          ))

(defun sk8-token-p (obj)
  (and (vectorp obj) (> (length obj) 0) (eq (aref obj 0) 'sk8-token)))

(defun sk8-token-val (obj)
  (aref obj 1))

(defun set-sk8-token-val (obj val)
  (setf (aref obj 1) val))

(defmacro sk8-token-start (obj)
  `(aref ,obj 2))

(defmacro sk8-token-end (obj)
  `(aref ,obj 3))

(defmacro sk8-token-line-number (obj)
  `(aref ,obj 4))

(defmacro sk8-token-start-line-pos (obj)
  `(aref ,obj 5))

(defmacro sk8-token-end-line-pos (obj)
  `(aref ,obj 6))

(defmacro sk8-token-type (obj)
  `(aref ,obj 7))

(defmacro sk8-token-correspondingToken (obj)
  `(aref ,obj 8))



;;;___________________________________________________________________________
;;these are used to hold the globals and locals the user declares as such
(defparameter handler-globals nil)
(defparameter handler-locals nil)
(defparameter handler-errors nil)

(defun get-handler-locals ()
  (mapcar #'(lambda (x) (transform-to-symbol (car x)))  handler-locals))
(defun get-handler-globals ()
  (mapcar #'transform-to-symbol handler-globals))

;;______________________________________________________________________________

(defparameter generate-list-of-globals-p nil)
(defparameter current-special-global-list nil)

;;this global is used for cases of translating a script without relying on lisp packages.
;;e.g. translating to java without having to load the project.
(defparameter generate-readable-code nil)

;;;___________________________________________________________________________

(defparameter cur-handler-name nil)

;;;cur-handler-object global variable is to support the existing kludge of "with targetobject"                                            
(defparameter cur-handler-object nil)

;;;___________________________________________________________________________
(defparameter current-warning-list nil)

;;;___________________________________________________________________________
(defun do-get-best-token-line-info (tokenToCheck tokWithValue)
  (when (sk8-token-p tokenToCheck)
    (when (>= (sk8-token-start-line-pos tokenToCheck) 0)

      ;;First if tokwithvalue has nothing 
      (when (and (>= (sk8-token-start-line-pos tokenToCheck) 0)
               (not (>= (sk8-token-start-line-pos tokWithValue) 0)))
        (setf (sk8-token-line-number tokWithValue) (sk8-token-line-number tokenToCheck))
        (setf (sk8-token-start-line-pos tokWithValue) (sk8-token-start-line-pos tokenToCheck)))
      (when (and (>= (sk8-token-end-line-pos tokenToCheck) 0)
               (not (>= (sk8-token-end-line-pos tokWithValue) 0)))
        (setf (sk8-token-line-number tokWithValue) (sk8-token-line-number tokenToCheck))
        (setf (sk8-token-end-line-pos tokWithValue) (sk8-token-end-line-pos tokenToCheck)))

      (when (< (sk8-token-start-line-pos tokenToCheck)
             (sk8-token-start-line-pos tokWithValue))
        (setf (sk8-token-line-number tokWithValue) (sk8-token-line-number tokenToCheck))
        (setf (sk8-token-start-line-pos tokWithValue) (sk8-token-start-line-pos tokenToCheck)))
      (when (> (sk8-token-end-line-pos tokenToCheck)
             (sk8-token-end-line-pos tokWithValue))
        (setf (sk8-token-line-number tokWithValue) (sk8-token-line-number tokenToCheck))
        (setf (sk8-token-end-line-pos tokWithValue) (sk8-token-end-line-pos tokenToCheck))))
    (let ((curval (sk8-token-val tokenToCheck)))
      (cond
       ((listp curval) 
        (mapc #'(lambda (x) (do-get-best-token-line-info x tokWithValue)) curval))
       ((sk8-token-p curval)
        (do-get-best-token-line-info curval tokWithValue))))
    ))

(defun get-best-token-line-info (tok)
  (do-get-best-token-line-info tok tok)
  tok)

(defun parse-error (errMsg &optional curToken)
  (let (linenum pos (tokenpassed curtoken))
    (setf cur-handler-name nil)
    (unless curToken (setf curToken (car input-tokens)))
    (if (and curToken (sk8-token-p curToken)) 
      (progn
        (setf curToken (get-best-token-line-info curtoken))
        (setf linenum (sk8-token-line-number curToken)
              pos (list (max 0 (sk8-token-start-line-pos curToken))
                        (max 0 (1+ (sk8-token-end-line-pos curToken)))   
                        )))
      (progn
        (when curtoken (format t "Warning a non token was passed in error ~s" curtoken))
        (setf linenum 0
              pos (list 0 0))))
    (when (and (eq (peek-token-symbol) 'EOL) (not tokenpassed)) 
      (setf pos (mapcar #'1- pos)))
    (throw :PARSING-ERROR 
           (list 'ERROR  linenum pos errMsg))))

;;;Token manipulation and comparitive function...
(defun transform-to-symbol (tok)
  (cond
   ((symbolp tok)
    tok)
   ((sk8-token-p tok)
    (sk8-token-val tok))
   ))

(defun token-equal (token sym)
  (eq (transform-to-symbol token) sym))

(defun peek-token ()
  (if input-tokens
    (car input-tokens)
    'end-of-input 
    ))

(defun peek-token-symbol ()
  (transform-to-symbol (peek-token)))

(defun consume-token ()
  (let (x)
    (setf x (peek-token))
    (setf input-tokens (cdr input-tokens))
    x))

(defun consume-token-symbol ()
  (transform-to-symbol (consume-token)))

(defun confirm-and-consume-token-symbol (val)
  (unless (eq (peek-token-symbol) val)
    (parse-error (concatenate 'string "Expected " 
                              (symbol-name val))))
  (consume-token-symbol))



#|
	Change History (most recent last):
	1  	 3/12/96	Brian   	The New Parser!!!
	2  	 3/14/96	Brian   	Removed all references to skil
	3  	 3/14/96	Brian   	Making parse error be a little smarter in figuring out
						where the error is.
	2  	 4/15/96	Brian   	Making everything happen in the SKIL package.
	3  	 4/19/96	Brian   	
	4  	 4/30/96	Brian   	
	5  	 5/13/96	Brian   	Fixing parse error for EOL
	6  	 7/19/96	Brian   	Putting definition of true and false here.
	7  	 8/ 5/96	Brian   	
	8  	 8/ 8/96	Brian   	
	9  	 8/15/96	Brian   	
	10 	 8/28/96	Brian   	
	11 	 8/28/96	Brian   	adding comments
	12 	 8/30/96	Brian   	Moving true, false,etc. to runtime:library functions.lisp
	13 	 9/ 9/96	Brian   	
	14 	 9/16/96	Brian   	
	15 	 9/16/96	Brian   	
	16 	10/10/96	Brian   	Fixing on error syntax.
	17 	10/11/96	Brian   	Adding warnings.
	18 	10/21/96	Hernan  	Moving it to the SK8Script runtime.
	19 	11/ 7/96	Brian Roddy	Fixing get-best-token-line to choose the line 
						number properly.
	20 	 2/27/97	Hernan  	
|# ;(do not edit past this line!!)
