;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories

;;;    c-m-S:            Evaluate SK8Script forms in region 
;;; c-m-sh-S:            Translate SK8Script forms in region to SKIL
;;; c-m-Command-S: Translate SK8Script forms in region to ScriptX
;;;   c-sh-L:             Insert continuation character 
;;;

(in-package :sk8dev)

(defun do-ed-evaluate-sk8script-region (window)
  (set-mini-buffer window "Evaluating...")
  (multiple-value-bind (from to) (ccl::kill-range window)
    (let ((str (buffer-substring (fred-buffer window) from to)))
      (sk8-multival-bind (fail? errors locals globals res)
                         (evaluateScriptForms (targetproject ui)
                                              str)
        (if fail?
          (bring-up-error-on-fred-window to (window-filename window) str errors)
          (progn
            ;; When there is a valid target project AND the thing we just
            ;; compiled was a function or handler, save it into the store. 
            (when (and (neq (targetProject ui) sk8)
                       (or (and (symbolp res) (fboundp res))
                           (eq (type-of res) 'standard-method)))
              (let ((targetProj (targetProject ui)))
                (PS::savehandlerVersionFromText targetProj res str locals globals)))
            (when errors 
              (format t "Warnings:~%")
              (dolist (curWarning (reverse errors))
                (format t "At Line ~a: ~a~%" 
                        (1+ (lineOfError curWarning))
                        (descriptionOfError curWarning))
                ))
            (print res)
            (set-mini-buffer window "Done"))))
      )))

(defun ed-evaluate-sk8script-region (window)
  (process-run-function "SK8Script Evaluation" #'do-ed-evaluate-sk8script-region window)
  )

(defun ed-translate-sk8script-region (window)
  (multiple-value-bind (from to) (ccl::kill-range window)
    (let ((str (buffer-substring (fred-buffer window) from to)))
      (sk8-multival-bind (fail? errors locals globals result)
                         (translateScriptCommandOrExpr (targetproject ui)
                                                       str)
        (declare (ignore globals locals))
        (if fail?
          (bring-up-error-on-fred-window to (window-filename window) str errors)
          (pprint result)))
      )))

(defun ed-insert-line-continuation (window)
  (buffer-insert (fred-buffer window) #\302))


(comtab-set-key *comtab* '(:control :meta #\s)
                #'ed-evaluate-sk8script-region
                "Eval the SK8Script code in the region")

(comtab-set-key *comtab* '(:control :meta :shift #\s)
                #'ed-translate-sk8script-region
                "Translate the SK8Script code in the region")

(comtab-set-key *comtab* '(:control :shift #\l) 'ed-insert-line-continuation)

(when (member "SCRIPTEDITTEXT" *modules* :test #'string=)
(comtab-set-key *script-fred-comtab* '(:control :meta #\s)
                #'ed-evaluate-sk8script-region
                "Eval the SK8Script code in the region")

(comtab-set-key *script-fred-comtab* '(:control :meta :shift #\s)
                #'ed-translate-sk8script-region
                "Translate the SK8Script code in the region")

(comtab-set-key *script-fred-comtab* '(:control :shift #\l) 'ed-insert-line-continuation)
)


#|
	Change History (most recent last):
	1  	 3/24/95	till    	Imported here from my hack.  Now handles multiple forms in a region.
	2  	 4/19/95	till    	packages can be a drag
	2  	 1/17/96	Hernan  	Folding in the new compiler API.
	2  	 4/ 3/96	Hernan  	Adding the nicer showing of errors on Fred windows.
	3  	 4/26/96	Brian   	bringing this up to date
	4  	 4/26/96	Brian   	moving error message to bring-up-error
	5  	 4/30/96	Brian   	
	6  	 4/30/96	Brian   	
	7  	 5/ 3/96	Brian   	
	8  	 5/ 6/96	Brian   	
	9  	 7/29/96	Hernan  	Saving handlers defined on a Fred window, into the targetProject of the UI.
	10 	11/12/96	Brian   	Making Sk8Script run in its own process.
	11 	11/26/96	Hernan  	Removing dead code.
	12 	12/19/96	Brian Roddy	Added printing of warnings to eval-sk8script-reg
	13 	12/19/96	Brian Roddy	
	14 	 2/27/97	Hernan  	
|# ;(do not edit past this line!!)
