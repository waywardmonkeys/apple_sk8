#+SK8
(in-package :SK8DEV)

(provide "SOUNDINPUTTRAPS")

;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories



#+SK8
(SK8-declare-syms :SK8 :private
                  SK8::T_SndRecordGC
                   )

(defun_X T_SndRecordGC :private (&key
                                      sndHandle
                                      filterProc
                                      quality
                                      dialogPosition
                                      notGC)
  "Record a sound into either a given handle or a newly-allocated
handle (which is GC-able unless notGC is non-nil).
Raises an error if anything goes wrong.
If user cancels, error is -128.
See IM-Sound, p. 3-28."
  (rlet ((theSndHandle :pointer))
    (if sndHandle
      (setf (%get-ptr theSndHandle) sndHandle)
      (setf (%get-ptr theSndHandle) (%null-ptr))) ; must be initialized - thanks to Bill St. Clair
    (checking-toolbox-error (:OSErr)
                            (#_SndRecord
                             (or filterProc     (%null-ptr))
                             (or dialogPosition (make-point 100 100))
                             (or quality        #$siGoodQuality)
                             theSndHandle))
    (or sndHandle
        (if notGC
          (%get-ptr theSndHandle)
          (handleToGCHandle (%get-ptr theSndHandle))))))

#|
	Change History (most recent last):
	1  	11/28/95	dy      	new file
	2  	 2/27/97	Hernan  	Removing case preserving reader macro.
|# ;(do not edit past this line!!)
