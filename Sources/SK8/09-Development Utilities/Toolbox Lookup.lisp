;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories

(in-package :ccl)
;
;  think-ref-lookup.lisp
;
;  This code enables you to lookup THINK Reference (TM) from Fred editor.
;  If you load this file, the lookup function ed-toolbox-reference is bound to m-r.
;
;  The original code is posted to info-mcl@cambridge.apple.com on 12/1/1993
;  by Jeffrey B Kane (jbk@world.std.com).
;  I added some faculties
;  * to launch THINK Reference (TM) if you have not loaded it yet.
;  * to get the current S expression and lookup if it is a symbol.
;  * to handle appleevent-error and display its message to mini-buffer.
;
;  And on Bill St. Clair's (bill@cambridge.apple.com) advice, I changed my code
;  to search THINK Reference (TM) with _PBDTGetAPPL. I referd to his code
;  in the file cambridge.apple.com /pub/mcl2/contrib/processes.lisp.
;
;  Special thanks for Jeffery and Bill.
;
;  Masaya UEDA		ueda@shpcsl.sharp.co.jp

(eval-when (:compile-toplevel :execute :load-toplevel)
  (require :appleevent-toolkit))

(defun %get-creator-path (creator fsspec)
  (let ((devs (directory "*:")))
    (dolist (vrefnum (sort (mapcar 'volume-number devs) #'>))
      (rlet ((pb :DTPBRec
                 :ioNamePtr (%null-ptr)
                 :ioVRefnum vrefnum))
        (when (= (#_PBDTGetPath pb) #$noErr)
          (setf (rref pb :DTPBRec.ioNamePtr)
                (%inc-ptr fsspec (get-field-offset :fsspec.name))
                (pref pb :DTPBRec.ioIndex) 0
                (pref pb :DTPBRec.ioFileCreator) creator)
          (when (= (#_PBDTGetAPPL pb) #$noErr)
            (setf (pref fsspec :fsspec.vRefnum) vrefnum
                  (pref fsspec :fsspec.parID) (pref pb :DTPBRec.ioAPPLParID))
            (return (values))))))))

#|
(defun get-creator-path (creator)
  (rlet ((fsspec :fsspec))
    (%get-creator-path creator fsspec)
    (%path-from-fsspec fsspec)))
|#

(defun launch-application (&key creator fsspec)
  (unless (= (count-if-not #'null (list creator fsspec))
             1)
        (error "invalid arguments"))
  (rlet ((theFsspec :fsspec))
    (when creator
      (%get-creator-path creator theFsspec)
      (setf fsspec theFsspec))
    (rlet ((lpb :LaunchParamBlockRec
              :launchBlockID #$extendedBlock
              :launchEPBLength #$extendedBlockLen
              :launchFileFlags 0
              :launchControlFlags (+ #$launchContinue #$launchNoFileFlags)
              :launchAppSpec fsspec
              :launchAppParameters (%null-ptr)))
    (if (= (#_LaunchApplication lpb) #$noErr)
      (values (rref lpb :LaunchParamBlockRec.launchProcessSN.highLongOfPSN)
              (rref lpb :LaunchParamBlockRec.launchProcessSN.LowLongOfPSN))))))

(defun get-application (creator)
  (let (psnhigh psnlow)
    (or (progn
          (multiple-value-setq (psnhigh psnlow) (appleevents::find-process-with-signature creator))
          psnhigh)
        (progn
          (multiple-value-setq (psnhigh psnlow) (launch-application :creator creator))
          psnhigh))
    (values psnhigh psnlow)))

(defun get-one-application (creator-list)
  (let (psnhigh psnlow)
    (dolist (creator creator-list)
      (multiple-value-setq (psnhigh psnlow) (get-application creator))
      (when psnhigh
        (return-from get-one-application (values psnhigh psnlow))))))


#|
(defun launch-application-file (filename &aux (pf (probe-file filename)))
  (if pf (rlet ((fsspec :fsspec))
           (with-pstrs ((name (mac-namestring pf)))
             (#_FSMakeFSSpec 0 0 name fsspec))
           (%launch-application fsspec))))
|#

(defparameter *toolbox-reference-app-creators* (list
                                                      "ALTV"  ;; Toolbox Assistant
                                                                                 ;; "DanR"  ;; THINK Reference
                                                      ))

(defun toolbox-reference (search-string)
  (multiple-value-bind (psnhigh psnlow) (get-one-application *toolbox-reference-app-creators*)
    (when psnhigh
      (with-aedescs (ae target reply)
        (with-pstrs ((pstring search-string))
          (appleevents::create-psn-target target psnhigh psnlow)
          ;; create an apple event
          (ae-error (#_AECreateAppleEvent
                     :|DanR|
                     :|REF |
                     target
                     #$kAutoGenerateReturnID
                     #$kAnyTransactionID
                     ae))
          ;; stuff it with our parameters
          (ae-error (#_AEPutParamPtr
                     ae
                     #$keyDirectObject
                     #$typeChar
                     (%inc-ptr pstring)
                     (%get-unsigned-byte pstring)))
          ;; send it off
          (appleevents::send-appleevent ae reply :reply-mode :wait-reply)
          t)))))

(defmethod ed-toolbox-reference ((fm fred-mixin))
  (let ((sym (ed-current-sexp fm))) 
    (when (and sym (symbolp sym))
      (let ((sn (symbol-name sym)))
        (when (or (char= #\_ (char sn 0))
                  (char= #\$ (char sn 0)))
          (setq sn (subseq sn 1)))
        (handler-case (toolbox-reference sn)
          (appleevent-error (theCondition)
                            (format (view-mini-buffer fm) "~a: ~a"
                                    sn theCondition)))))))

(defun edit-toolbox-reference (trapSymbol)
  "if symbol doesn't start with _ or $, return (values nil \"\")
Otherwise, try to look it up in Think Reference
and return t if success else (values nil error-string)"
  (let ((sn (symbol-name trapSymbol)))
    (if (or (char= #\_ (char sn 0))
            (char= #\$ (char sn 0)))
      (handler-case (toolbox-reference (subseq sn 1))
        (appleevent-error (theCondition)
                          (values nil (format nil "~a: ~a" sn theCondition))))
      (values nil ""))))

(def-fred-command (:meta #\r) ed-toolbox-reference)

#|
	Change History (most recent last):
	1  	11/ 7/94	dy      	slight mods to public domain codeto work with SK8
	2  	 1/31/95	dy      	change think-reference to toolbox-reference; Try Toolbox assistant, then THINK Reference
	2  	 2/27/97	Hernan  	
|# ;(do not edit past this line!!)
