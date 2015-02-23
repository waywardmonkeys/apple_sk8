;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories

(in-package :sk8dev)

(provide "QDPICTURETOPICTTRANSLATOR")

(require "TRANSLATOR")

;;; QDPictureToPICTTranslator
;;; _____________________  

(new translator :objectName "QDPictureToPICTtranslator" :project sk8
     :internalObject QDPicture)

;;; Asumes the file exists and is of the right macfiletype. Gets the handle and writes it to the resource file
;;; of the project. (If you want to read it from the data fork all the time, should use the translator for
;;; PictFileMedia).

(define-handler importFromFile (QDPictureToPICTtranslator source destination)
  (if (eq (slot-value destination 'file) :project)
    (let ((theHandle (pictFileToHandle source)))
      (if (and theHandle (handlep theHandle))
        ;; Everything ok. Write to project file.
        (progn (writeHandleToProjectFile (project destination) theHandle destination)
               destination)
        ;; Error: could not read the handle.
        (sk8-error GeneralProgrammaticError
                   :strings '("Could not read a PICT handle from " "'s datafork.")
                   :objects (list source))))
    ;; Want to leave data there? Just clear the resource id.
    (progn (setf (resourceid destination) nil)
           destination)))

(define-handler exportToClipboard (QDPictureToPICTtranslator source destination)
  (declare (ignore destination))
  (let ((theHandle (loadMedia source)))
    (if (handlep theHandle)
      (putScrap theHandle (externalType me))
      (sk8-error GeneralProgrammaticError
                 :strings '("Could not obtain PICT handle from " ".")
                 :objects (list source)))))


#|
	Change History (most recent last):
	2  	 2/27/97	Hernan  	
|# ;(do not edit past this line!!)
