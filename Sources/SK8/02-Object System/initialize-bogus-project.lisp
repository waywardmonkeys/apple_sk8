;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories

(in-package :mf)

;;; The functionality required to create the project's resource file. 
;;; Requires the resources traps to be around. 

(defun move-resources (from-file to-file)
  (sk8dev::T_CreateResFile to-file)
  (flet ((checkreserror ()
           (let ((err (#_reserror)))
             (unless (= 0 err)
               (error "Resource error ~a" err)))))
    (let* ((cur-refnum (#_curresfile))
           (to-file-refnum (sk8dev::T_OpenResFile to-file))
           (from-file-refnum (sk8dev::T_OpenResFile from-file))
           succeeded)
      
      (unwind-protect
        (let* ((type-count (progn (sk8dev::T_UseResFile from-file-refnum)
                                  (#_count1types)))
               (types (let (types)
                        (dotimes (i type-count)
                          (push (unwind-protect
                                  (%stack-block  ((longPtr 4))
                                    (#_get1indtype longPtr (1+ i))
                                    (checkreserror)
                                    (%get-ostype longPtr)))
                                types))
                        (reverse types)))
               handle resourceID resourceType)
          (%stack-block ((resid 4)
                         (restype 8) ;8 = size + ch1 ch2 ch3 ch4, rounded to nearest long
                         (resname 256))
            (dolist (type types)
              (sk8dev::T_UseResFile from-file-refnum)
              (dotimes (index (#_count1resources type))
                ;; Get resource:
                (sk8dev::T_UseResFile from-file-refnum)
                (setq handle (sk8dev::T_get1indresource type (1+ index)))
                (sk8dev::T_loadresource handle)
                (#_hLock handle)
                (sk8dev::T_GetResInfo handle resid restype resname)
                (setf resourceId (%get-word resid))
                (setf resourceType (%get-ostype restype))
                (#_hUnlock handle)
                ;; Convert handle to a new resource:
                (sk8dev::T_detachResource handle)
                (sk8dev::T_UseResFile to-file-refnum)
                (#_addResource handle resourceType resourceID resname)
                (checkreserror)
                (#_setresattrs handle 0)
                (checkreserror)
                (#_changedResource handle)
                (checkreserror)
                (#_writeresource handle)
                (checkreserror)
                (setq succeeded t)
                ))))
        (when succeeded
          (sk8dev::T_updateResFile to-file-refnum)
          (sk8dev::T_useresfile to-file-refnum)
          (let ((ckid (#_Get1Resource "ckid" 128)))  ;; don't want to copy source control info
            (when (not (%null-ptr-p ckid))
              (sk8dev::T_RemoveResource ckid)
              (checkreserror)))
          (sk8dev::T_updateResFile to-file-refnum))
        ;;; remove wood (wood::flush-all-open-pheaps) ; make sure that files are not "busy"
        (sk8dev::T_CloseResFile from-file-refnum)
        ;; Leave resource fork of project file open
        ;; (sk8dev::T_closeresfile to-file-refnum)
        (sk8dev::T_UseResFile cur-refnum)))))

(defun create-project-file (project filename)
  (with-open-file (strm filename :direction :output :if-exists :error :external-format :PROJ)
    (let ((required (and (sk8::requiredproject project)
                         (sk8::sk8_id (sk8::requiredproject project)))))
      ;;; *sk8-major-version* *sk8-minor-version* *sk8-little-version* major-version minor-version store-version created-time required-proj-name
      (format strm ";;    SK8 Project file~%;; SK8 version~%~s ~s ~s~%;; Store version~%~s ~s ~s~%;; Creation time~%~s~%;;Requires project~%~s"
              mf::*sk8-major-version* mf::*sk8-minor-version* mf::*sk8-little-version* 1 0 (sk8::version project) (get-universal-time) required))))

(defun initialize-bogus-project (project)
  (let* ((filename (sk8::physicalname (sk8::file project)))
         (resname (namestring (translate-logical-pathname
                               (concatenate 'string "sk8;10-Resources:" (file-namestring filename) ".rsrc"))))
         (exists-p (probe-file filename)))
    (unless (probe-file resname)
      (error "Resource file needed for building project does not exist: ~s" resname))
    (format t "~%Building project file ~a" filename)
    ;; Delete existing:
    (when exists-p
      (let ((cur (#_curresfile)))
        (sk8dev::removeRes filename)
        (#_UseResFile cur))
      (delete-file filename))
    (create-project-file project filename) ;;; remove wood
    (move-resources resname filename)
    )
  (setf (sk8::swapfile project) nil))


#|
	Change History (most recent last):
	2  	 2/27/97	Hernan  	
|# ;(do not edit past this line!!)
