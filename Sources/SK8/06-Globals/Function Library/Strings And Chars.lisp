;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories


;;;


(in-package :SK8Development)

(provide "STRINGSANDCHARS")

(SK8-declare-syms :SK8 :public ; Updated  6-26-95  11:34 am
                  SK8::ISLOWERCASE SK8::ISUPPERCASE SK8::STRINGEQUAL)

;; ASCII defined by SK8Script

(mf::publish-function-symbol 'character (SK8::package SK8))



(defun not-text-or-char-error (handlerName argName value)
  (SK8-error ArgumentTypeMismatchError
             :handlerName handlerName :argumentName argName
             :object value :expectedType (list Text Character)))


(define-sk8-function alphabetic nil (textOrCharacter)
  (cond
   ((characterp textOrCharacter)
    (alpha-char-p textOrCharacter))
   ((stringp textOrCharacter)
    (every #'alpha-char-p textOrCharacter))
   (t
    (let ((str (as textOrCharacter string :project sk8)))
      (unless str (not-text-or-char-error 'alphabetic 'textOrCharacter textOrCharacter))
      (every #'alpha-char-p str)))))


(define-sk8-function alphanumeric nil (textOrCharacter)
  (cond
   ((characterp textOrCharacter)
    (alphanumericp textOrCharacter))
   ((stringp textOrCharacter)
    (every #'alphanumericp textOrCharacter))
   (t
    (let ((str (as textOrCharacter string :project sk8)))
      (unless str (not-text-or-char-error 'alphabetic 'textOrCharacter textOrCharacter))
      (every #'alphanumericp str)))))


(define-sk8-function lowercase nil (textOrCharacter)
  (cond
   ((characterp textOrCharacter)
    (char-downcase textOrCharacter))
   ((stringp textOrCharacter)
    (string-downcase textOrCharacter))
   (t
    (let ((str (as textOrCharacter string :project sk8)))
      (unless str (not-text-or-char-error 'alphabetic 'textOrCharacter textOrCharacter))
      (string-downcase str)))))


(define-sk8-function uppercase nil (textOrCharacter)
  (cond
   ((characterp textOrCharacter)
    (char-upcase textOrCharacter))
   ((stringp textOrCharacter)
    (string-upcase textOrCharacter))
   (t
    (let ((str (as textOrCharacter string :project sk8)))
      (unless str (not-text-or-char-error 'alphabetic 'textOrCharacter textOrCharacter))
      (string-upcase str)))))

(define-handler isUpperCase (Character)
  (let ((cc (char-code me)))
    (or (and (>= cc 65) (<= cc 90))
        (and (>= cc 128) (<= cc 134))
        (and (>= cc 203) (<= cc 205)))))

(define-handler isLowerCase (Character)
  (let ((cc (char-code me)))
    (or (and (>= cc 97) (<= cc 122))
        (and (>= cc 135) (<= cc 159)))))

(define-sk8-function stringEqual nil (string1 string2)
  (string= string1 string2))

#|
	Change History (most recent last):
	2		6/1/93	Brian Roddy	Modified alphanumeric to take strings with both alpha and numeric
	3		6/25/93	chip	published the names of the handlers made by defmethod
	9  	 9/ 1/94	chip    	publish-project-symbol --> publish-global/function-symbol (radar #1183935)
	10 	11/ 1/94	chip    	textual functions now work with all text (radar #1170375)
	11 	 2/17/95	Hernan  	Adding isUpperCase and isLowerCase.
	2  	 6/26/95	Hernan  	Adding stringEqual, a function to compare strings cosidering
							case.
~~	2  	 4/10/96	Hernan  	define-sk8-function->defun.
	5  	 2/27/97	Hernan  	Putting define-sk8-function back in the code.
						4   5/21/96Brian   removing with-temp-string.
|# ;(do not edit past this line!!)
