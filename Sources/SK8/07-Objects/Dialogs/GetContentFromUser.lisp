;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories

(in-package :sk8dev)

(provide "GETCONTENTFROMUSER")

(require "EDITTEXT" "objects;EditText:EditText")
(require "TEXTLIST" "objects;Pickers:TextList")

;;; _______________________________ 
;;; GetContentFromUser
;;; _______________________________ 

(define-handler GetText (file &key (aslistoflines t) (aslistofforms nil))
  (let (theline (curline "") res)
    (with-open-file (str (ospathname me))
      (unless (or aslistoflines aslistofforms) (setf res ""))
      (loop
        (setf theline (read-line str nil nil))
        (unless theline (return))
        (cond
         (aslistoflines 
          (push theline res))
         (aslistofforms
          (if (string= theline "")
            (progn
              (push curline res)
              (setf curline ""))
            (setf curline 
                  (if (string= curline "") theline
                      (concatenate 'string curline (string #\newline) theline))))
          )
         (t
          (setf res (concatenate 'string res (string #\newline) theline))))
        )
      (if (or aslistoflines aslistofforms) (setf res (nreverse res)))
      res
      ))
  )

(define-handler GetContentFromUser (editText) 
  (let ((pname (openfiledialog :project sk8 :macfiletype "TEXT")))
    (when pname
      (setf (text me) (GetText pname :aslistoflines nil)))))

;; needs options for getting it from various sources

(define-handler GetContentFromUser (textList &key (forms nil)) 
  (let ((pname (openfiledialog :project sk8 :macfiletype "TEXT")))
    (when pname
      (setf (items me) (GetText pname :aslistoflines (not forms) :aslistofforms forms)))))

(define-handler GetContentFromUser (picker &key (forms nil)) 
  (let ((pname (openfiledialog :project sk8 :macfiletype "TEXT")))
    (when pname
      (setf (items me) (GetText pname :aslistoflines (not forms) :aslistofforms forms)))))

(define-handler GetContentFromUser (imageRenderer) 
  (let ((pname (openfiledialog :project sk8 :macfiletype "PICT")))
    (when pname
      (setf (media me) (new qdpicture :project (project me) :file pname)))))





#|
	Change History (most recent last):
	4  	 2/27/97	Hernan  	define-system-handler->define-handler.
						3   7/17/96Brian   fixing gettext of file.
|# ;(do not edit past this line!!)
