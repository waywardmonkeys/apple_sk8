;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories

(in-package :sk8dev)

(provide "COLORPATTERNTOPPATTRANSLATOR")

(require "TRANSLATOR")

;;; ColorPatternToppatTranslator
;;; _______________________

(new translator :objectName "ColorPatternToppattranslator" :project sk8
     :internalObject ColorPattern)

;;; Cannot import from file yet. Remove when an importFromFile method is written.

(define-handler canImport (ColorPatternToppattranslator source destination)
  (declare (ignore destination))
  (if (inheritsFrom source file)
    nil
    (call-next-method)))

(define-handler exportToClipboard (ColorPatternToppattranslator source destination)
  (declare (ignore destination))
  (let ((theHandle (loadMedia source)))
    (if (handlep theHandle)
      (putScrap theHandle (externalType me))
      (sk8-error GeneralProgrammaticError
                 :strings '("Could not obtain a ppat handle from " ".")
                 :objects (list source)))))

#|
	Change History (most recent last):
	2  	 2/27/97	Hernan  	
|# ;(do not edit past this line!!)
