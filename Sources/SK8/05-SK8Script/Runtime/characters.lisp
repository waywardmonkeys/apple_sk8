;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories


;;;


(in-package :SK8Development)


;; CHARACTER
(mf::publish-function-symbol 'character  mf::*SK8-package*)

;; ASCII
(define-sk8-function ascii nil (ch)
  (cond
   ((characterp ch)
    (return-from ascii (char-code ch)))
   ((stringp ch)
    (when (eql 1 (length ch))
      (return-from ascii (char-code (aref ch 0)))))
   (t
    (when (and (is-a ch collection) (eql 1 (SK8::length ch)))
      (return-from ascii
        (char-code (elementatvisitstate ch (initialvisitstate ch)))))))
  (error "value ~s is not a single character" ch))

(define-sk8-constant SK8::Newline #\Newline :project SK8 :register t)
(define-sk8-constant SK8::Tab #\Tab :project SK8 :register t)
(define-sk8-constant SK8::Enter #\Enter :project SK8 :register t)
(define-sk8-constant SK8::Delete #\Delete :project SK8 :register t)
(define-sk8-constant SK8::LeftArrow #\BackArrow :project SK8 :register t)
(define-sk8-constant SK8::RightArrow #\ForwardArrow :project SK8 :register t)
(define-sk8-constant SK8::UpArrow #\UpArrow :project SK8 :register t)
(define-sk8-constant SK8::DownArrow #\DownArrow :project SK8 :register t)
(define-sk8-constant SK8::Escape #\Escape :project SK8 :register t)
(define-sk8-constant SK8::Quote #\" :project SK8 :register t)

;;; These are needed by the Picker in sk8Script.

(define-sk8-constant SK8::Home #\Home :project SK8 :register t)
(define-sk8-constant SK8::EndKey #\End :project SK8 :register t)
(define-sk8-constant SK8::PageUp #\PageUp :project SK8 :register t)
(define-sk8-constant SK8::PageDown #\Page :project SK8 :register t)
(define-sk8-constant SK8::Space #\Space :project SK8 :register t)

(defparameter *character-names*
  '((#\Newline .		SK8::Newline)
    (#\Tab .			SK8::Tab)
    (#\Enter .			SK8::Enter)
    (#\Delete .			SK8::Delete)
    (#\BackArrow .		SK8::LeftArrow)
    (#\ForwardArrow .	SK8::RightArrow)
    (#\UpArrow .		SK8::UpArrow)
    (#\DownArrow .		SK8::DownArrow)
    (#\Escape .			SK8::Escape)
    (#\" .			SK8::Quote)
    (#\Space .			SK8::Space)))


#|
	Change History (most recent last):
	1  	 4/16/96	Brian   	
	2  	 4/22/96	Brian   	
	3  	 2/27/97	Hernan  	
|# ;(do not edit past this line!!)
