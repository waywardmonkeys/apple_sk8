(in-package :SourceServer)

(export 'find-folder)

(eval-when (:compile-toplevel :execute)
  (require 'sysequ))

(defun path-from-hard-id (dir-id vrefnum &aux (dirlist nil))
  (with-returned-pstrs ((vname ""))
    (rlet ((qb :CInfoPBRec 
               :ioCompletion (ccl:%null-ptr)
               :ioNamePtr vname 
               :ioVRefNum vrefnum))
      (loop
        (rset qb :CInfoPBRec.ioFDirIndex -1)
        (rset qb :CinfoPBRec.ioDirID dir-id)
        (if (eql #$noErr (#_PBGetCatInfo qb))
          (push (%get-string (pref qb :CInfoPBRec.ioNamePtr)) dirlist)
          (return nil))
        (when (= dir-id 2)
          (return (make-pathname :directory (cons :absolute dirlist))))
        (setf dir-id (pref qb :CinfoPBRec.ioDrParID))))))

(defun find-folder (folder-signature &optional (createp t))
  ;;folder types are documented in Inside Mac Volume VI, The Finder Interface
  ;; This code modified from MacTCP.lisp in library folder
  (when (ccl::gestalt #$gestaltFindFolderAttr #$gestaltFindFolderPresent)
    (rlet ((vrefnumP :signed-integer)
           (diridP :signed-long))
      (let ((results (#_FindFolder 
                      #$kOnSystemDisk
                      folder-signature
                      createp  ;; booleans in trap calls are automatically converted from t/nil to $true/$false
                      vRefNumP
                      dirIDP)))
        (when (eql #$noErr 
                   results)
          (path-from-hard-id (%get-signed-long dirIDP) (%get-signed-word vrefnumP)))))))

(defun find-application-for-creator-on-vol (application-signature volnum)
  (when (ccl::gestalt #$gestaltFindFolderAttr #$gestaltFindFolderPresent)
    (with-returned-pstrs ((vname "uninitialized"))
      (rlet ((pb :DTPBRec 
                 :ioNamePtr vname 
                 :ioDTRefNum 0
                 :ioFileCreator application-signature
                 :ioIndex 0
                 :ioCompletion (ccl:%null-ptr)
                 :ioVRefNum volnum))
        (when (and (eql #$noErr
                        (#_PBDTGetPath pb))
                   (eql #$noErr (#_PBDTGetAPPL pb)))
          (merge-pathnames
           (path-from-hard-id (pref pb :DTPBRec.ioAPPLParID)
                              (pref pb :DTPBRec.ioVRefNum))
           (%get-string (pref pb :DTPBRec.ioNamePtr))))))))

(defun find-application-for-creator (application-signature)
  ;; find an application that matches a particular creator signature (which is an :os-type)
  ;; look for newest one, first on default volume, then rest of mounted volumes
  ;; return first one that's found(or nil, if none found or not HFS)
  (rlet ((pb :paramblockrec :ioCompletion (ccl:%null-ptr) :ioVolIndex -1
             :ioVRefNum 0 :ioNamePtr (ccl:%null-ptr)))
    (when (eql #$noErr (#_PBGetVInfo pb)) ;; start by getting default volume
      (do* ((ndx 1 (1+ ndx))
            (deflt (pref pb :ParamBlockRec.ioVRefNum))
            (vol nil (pref pb :ParamBlockRec.ioVRefNum))
            (path (find-application-for-creator-on-vol application-signature deflt)
                  (unless (= vol deflt)
                    (find-application-for-creator-on-vol application-signature vol))))
           ((or path
                (not (progn (rset pb :ParamBlockRec.ioVolIndex ndx)
                            (rset pb :ParamBlockRec.ioVRefNum 0)
                            (eql #$noErr (#_PBGetVInfo pb)))))
            path)))))


#|
	Change History (most recent last):
	3	4/4/93	Sidney	Use SourceServer package instead of CCL to
				minimize possibility of name collision by users.
				
				Switch to MCL 2.0 trap interface calls instead of
				now undocumented 1.3 calls
	2  	 7/ 1/96	sidney  	modernize to MCL3.9
	3  	10/21/96	sidney  	fix error in use of _findfolder that was copied from MacTCP example in library
|# ;(do not edit past this line!!)
