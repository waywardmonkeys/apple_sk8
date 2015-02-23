;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories

(in-package :sk8dev)

(provide "INDIRECTTRANSLATOR")

(require "TRANSLATOR")

;;; ____________________________
;;; IndirectTranslator
;;; ____________________________

(new translator :objectname "IndirectTranslator" :project sk8
     :properties '((mediaTranslator :value nil))) ;; Translator used for the media.

(define-handler canImport (IndirectTranslator source destination)
  (let ((mediaTranslator (mediaTranslator me)))
    (when mediaTranslator
      (canImport mediaTranslator source destination))))

(define-handler sk8::import (indirectTranslator &key source destination)
  (sk8::import (mediaTranslator me) :source source :destination destination))

;;; This function is used by all the 2 step image renderer translators.

(defun make-imageRenderer-from-new-media (theMedia source destination)
  (if theMedia
    ;; Make the imageRenderer and return it.
    (new imageRenderer :project (project destination) :media destination)
    (sk8-error GeneralProgrammaticError
               :strings '("No media object created when trying to import " " into " ".")
               :objects (list source destination))))

(define-handler exportToClipboard (IndirectTranslator source destination)
  (let ((theMedia (media source))
        theTranslator)
    (when theMedia
      (setf theTranslator (findExportTranslator theMedia destination))
      (when (and theTranslator (neq theTranslator me))
        (exportToClipboard thetranslator theMedia destination)))))

#|
	Change History (most recent last):
	2  	 2/27/97	Hernan  	
|# ;(do not edit past this line!!)
