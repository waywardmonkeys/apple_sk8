;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories

(in-package :sk8dev)

(provide "SOUNDTOSNDTRANSLATOR")

(require "SOUND" "objects;Media:Sound")
(require "INDIRECTTRANSLATOR" "objects;Import/Export:IndirecTranslator")
(require "SOUNDRSRCTOSNDTRANSLATOR" "objects;Import/Export:SoundRSRCTosndTranslator")

;;; SoundTosndTranslator
;;; _________________

(new IndirectTranslator :objectName "SoundTosndTranslator" :project sk8
     :internalObject SoundRSRC
     :finalObject Sound
     :mediaTranslator SoundRSRCTosndTranslator)

(define-handler sk8::import (SoundTosndTranslator &key source destination)
  (let ((theMedia (call-next-method)))
    (if theMedia
      (new Sound :project (project destination) :media theMedia)
      (sk8-error GeneralProgrammaticError
                 :strings '("No media object created when trying to import " " into " ".")
                 :objects (list source destination)))))

#|
	Change History (most recent last):
	2  	 2/27/97	Hernan  	
|# ;(do not edit past this line!!)
