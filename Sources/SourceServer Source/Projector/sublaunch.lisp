;sublaunch.lisp

(in-package :SourceServer)

;;; 11/05/91 gz  Convert to new traps.

;The following three defs were lifted from Leibniz Sources:Defs:Defsys.lisp
(eval-when (:execute :compile-toplevel)
  (require 'lispequ)
  (require 'sysequ)
  (defrecord LaunchStruct
    (pfName :pointer)
    (param :integer)
    (LC :unsigned-integer)                 ; extended parameters:
    (extBlockLen :longint)              ; number of bytes in extension = 6
    (fFlags :integer)                   ; Finder file info flags
    (launchFlags :unsigned-integer)        ; bit 15,14=1 for sublaunch, others reserved
    (reserved :integer))
  )

(defun sublaunch (path &optional file (stay-in-background-p nil))
  (when file (store-app-param-handle file))
  (rlet ((launch :LaunchStruct))
    (ccl::%stack-iopb (pb np)
      ;Get the Finder flags
      (ccl::%path-to-iopb path pb :errchk)
;;      (%put-ostype pb "ERIK" $ioWDProcID)
;;      (%put-ptr pb (%null-ptr) $ioFileName)
      (rset pb :WDPBRec.ioWDProcID "ERIK")
      (rset pb :WDPBRec.ioNamePtr (%null-ptr))
      (#_PBOpenWDSync :errchk pb)
      (#_PBSetVolSync :errchk pb)
;;      (%put-ptr pb np $ioFileName)
;;      (%put-long pb 0 $ioDirID)
;;      (%put-word pb 0 $ioFDirIndex)
      (rset pb :CInfoPBRec.ioNamePtr np)
      (rset pb :CInfoPBRec.ioDirID 0)
      (rset pb :CInfoPBRec.ioFDirIndex 0)
      (#_PBGetCatInfoSync :errchk pb)
      (rset launch :LaunchStruct.pfName np)
      (rset launch :LaunchStruct.param 0)
      (rset launch :LaunchStruct.LC #x4C43)
      (rset launch :LaunchStruct.extBlockLen 6)
      ;Copy flags; set bit 6 of low byte to 1 for RO access:
      (rset launch :LaunchStruct.fFlags (pref pb :CInfoPBRec.ioFLFndrInfo.fdFlags))
      (rset launch :LaunchStruct.launchFlags (logior 
                                              #$launchContinue
                                              (if stay-in-background-p #$launchDontSwitch 0)))
      (rset launch :LaunchStruct.reserved 0)
      (let ((err (#_LaunchApplication launch)))
        (when (< err 0)
          (case err                     ; Process Manager errors
            ((-600) (error "No elegible process with specified descriptor"))
            ((-601) (error "Not enough room to launch application w/special requirements"))
            ((-602) (error "Memory mode is 32-bit, but application not 32-bit clean"))
            ((-603) (error "Application made module calls in improper order"))
            ((-604) (error "Hardware configuration not correct for call"))
            ((-605) (error "Application SIZE not big enough for launch"))
            ((-606) (error "Application is background-only, and launch flags disallow this"))
            (t (ccl::%err-disp err))))))))

(defun store-app-param-handle (file)
  (with-macptrs ((ploc (%int-to-ptr $appparmhandle)))
    (let ((params (%get-ptr ploc)))
      (when (%null-ptr-p params)
        (%put-ptr ploc (setq params (#_NewHandle :errchk 0))))
      (ccl::%stack-iopb (pb np)
        (ccl::%path-to-iopb file pb :errchk)
        ;; (%put-ostype pb "ERIK" $ioWDProcID)
        ;; (%put-ptr pb (%null-ptr) $ioFileName)
        (rset pb :WDPBRec.ioWDProcID "ERIK")
        (rset pb :WDPBRec.ioNamePtr (%null-ptr))
        (#_PBOpenWDSync :errchk pb)
        (#_SetHandleSize :errchk params (+ 13 (%get-byte np)))
        (with-dereferenced-handles ((p params))
          (%put-word p 0 0)
          (%put-word p 1 2)
          ;; (%put-word p (%get-word pb $ioVRefNum) 4)
          ;;  (%put-ptr pb np $ioFileName)
          ;; (%put-long pb 0 $ioDirID)
          ;; (%put-word pb 0 $ioFDirIndex)
          (%put-word p (pref pb :CInfoPBRec.ioVRefNum) 4)
          (rset pb :CInfoPBRec.ioNamePtr np)
          (rset pb :CInfoPBRec.ioDirID 0)
          (rset pb :CInfoPBRec.ioFDirIndex 0)
          (#_PBGetCatInfoSync :errchk pb)
          ;; (%put-ostype p (%get-ostype pb $fdType) 6)
          (%put-ostype p (pref pb :CInfoPBRec.ioFLFndrInfo.fdType) 6)
          (%put-word p 0 10)
          (#_BlockMove np (%inc-ptr p 12) (1+ (%get-byte np))))))))

#|
	Change History (most recent last):
	3	4/4/93	Sidney	Use SourceServer package instead of CCL and use
				MCL 2.0 low-level system calls instead of ones
				from verison 1.3 that required loading a verison of
				sysequ.lisp that could lead to name collisions in
				the ccl package.
	4	4/4/93	Sidney	Use SourceServer package instead of CCL to
				minimize possibility of name collision by users.
				
				Switch to MCL 2.0 trap interface calls instead of
				now undocumented 1.3 calls
	3  	 7/ 1/96	sidney  	copy some changes form ther MCL3.9 version in hopes that it will make launching work
|# ;(do not edit past this line!!)
