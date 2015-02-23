(in-package :SK8Development)

(provide "RECORDSOUND")

(require "SCRAPTRAPS" "macf-trap-library;Scrap")
(require "SOUNDINPUTTRAPS" "macf-trap-library;SoundInput")

;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories



(SK8-declare-syms :SK8 :public
                  (SK8::recordSoundToClipBoard))

(SK8-declare-syms :SK8 :public
                  ;; Sound quality settings
                  sk8::good sk8::better sk8::best
                  )

(SK8-declare-syms :SK8 :private
                  SK8::SoundQualityValues
                  )

(defun checkLegalValue (value legalValues)
  (unless (member value legalValues)
    (sk8-error TypeMismatchError
               :object value
               :expectedType legalValues))
  (sk8-return-nothing))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (define-sk8-constant SoundQualityValues
    '(best better good)
    :private? t :project SK8))

(define-sk8-function recordSoundToClipBoard nil (&key
                                                     (quality 'best)
                                                     )
  (checkLegalValue quality SoundQualityValues)
  (let ((qualityNumber (case quality
                         (best   #$siBestQuality)
                         (better #$siBetterQuality)
                         (good   #$siGoodQuality))))
    (with-temporary-handle (sndHandle (T_SndRecordGC :quality qualityNumber :notGC t))
      (with-dereferenced-handles
        ((sndPtr sndHandle))
        (T_ZeroScrap)
        (T_PutScrap "snd " sndPtr (T_GetHandleSize sndHandle)))))
  (sk8-return-nothing))

#| TESTS

(recordSoundToClipBoard :quality 'bad)
(recordSoundToClipBoard :quality 'good)

|#

#| other interesting stuff

;;; non-sk8 versions

(defun record-sound-to-resource-file (resFile &key
                                           resID
                                           dialogPosition
                                           resName)
  "Records a sound and saves it in a file.
If you don't specify a resID, uses Unique1ID trap to find one.
Returns the resID.
This is a lisp version of the code
in IM Sound, 1-28 to 1-30"
  (with-open-and-use-resource-file (refNumVar resfile
                                              :if-does-not-exist :create)
    (unless resID
      (setf resID (T_Unique1ID "snd ")))
    (with-pstrs ((theName (or resName "")))
      (let ((theSndHandle (T_SndRecordGC :dialogPosition dialogPosition :notGC t)))
        ;;  add the resource to the resource fork of the file.
        (when-unwind
          (T_AddResource theSndHandle "snd " resID theName)
          (T_DisposeHandle theSndHandle)))
      ;;  update the resource file
      (T_UpdateResFile refNumVar)))
  resID)

(defun record-sound-to-scrap ()
  "Record a sound.  If the user clicks Save,
then replace the scrap with the recorded sound."
  (with-temporary-handle (sndHandle (T_SndRecordGC :notGC t))
    (with-dereferenced-handles
      ((sndPtr sndHandle))
      (T_ZeroScrap)
      (T_PutScrap "snd " sndPtr (T_GetHandleSize sndHandle))))
  (values))
#| TESTS

(record-sound-to-scrap)

|#

|#


#|
	Change History (most recent last):
	1  	11/28/95	dy      	new file
	2  	11/29/95	dy      	streamlined record-snd-to-res-file
	3  	11/29/95	dy      	fix record-snd-to-res-file
	4  	11/29/95	dy      	
	2  	 4/10/96	Hernan  	Removing case preserving reader macro.
	3  	 4/19/96	Hernan  	Putting define-sk8-function back in the code.
	4  	 4/19/96	Hernan  	Trying to stop sourceserver from adding the double squiggle every time.
	5  	 4/19/96	Hernan  	Trying to stop sourceserver from adding the double squiggle every time.
	6  	 4/22/96	Hernan  	Trying to stop sourceserver from adding the double squiggle every time.
	7  	 4/22/96	Hernan  	Trying to stop sourceserver from adding the double squiggle every time.
	8  	 4/22/96	Hernan  	Did it work?
	9  	 5/ 7/96	Hernan  	Needs to recompile...
	10 	 9/ 4/96	Hernan  	Moved the traps to the object system.
	11 	 2/27/97	Hernan  	
|# ;(do not edit past this line!!)
