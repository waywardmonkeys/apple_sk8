;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories
;;-*- Mode: Lisp; Package: CCL -*-
;;
;; resources.lisp
;;
;; Simple resource accessors
;;

;; Rewritten starting from the version in the MCL Library folder

(in-package :ccl)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(with-open-resource-file open-resource-file with-open-res-file
            with-load-resources-off OpenResFileNums close-resource-file
            res-error use-resource-file current-resource-file using-resource-file
            get-resource get-string get-ind-string load-resource release-resource
            add-resource write-resource delete-resource remove-resource detach-resource)))


; Execute the BODY with REFNUM-VAR bound to the refnum for the resource
; file of FILE.  :IF-DOES-NOT-EXIST can be NIL, :ERROR, or :CREATE
(defmacro with-open-resource-file ((refnum-var file &key (if-does-not-exist :error))
                                       &body body)
  `(let ((,refnum-var nil))
     (unwind-protect
       (progn
         (setq ,refnum-var
               (open-resource-file ,file :if-does-not-exist ',if-does-not-exist))
         ,@body)
       (if ,refnum-var
         (close-resource-file ,refnum-var)))))

; Open the resource FILE and return it's refnum.
; if-does-not-exist can be :error, nil or :create (just like OPEN).
; If ERRORP is NIL and there is an error, return two values: NIL and
; the error code.
(defun open-resource-file (file &key (if-does-not-exist :error) (errorp t))
  (let ((real-file (probe-file file)))          ; resolve alias
    (setq real-file (mac-namestring (or real-file file)))
    (with-pstr (pf real-file)
      (let ((res (#_OpenResFile pf)))
        (declare (fixnum res))
        (when (< res 0)
          (flet ((err (code)
                   (if errorp
                     (if (eq code #$eofErr)
                       (error "Trying to read resources, but there is no resource fork in file ~s" file)
                       (signal-file-error code file))
                     (return-from open-resource-file (values nil code)))))
            (declare (dynamic-extent #'err))
            (let ((code (#_ResError)))
              (unless (or (eq code #$fnfErr)
                          (eq code #$eofErr)
                          (eq code #$resFNotFound))
                (err code))
              (case if-does-not-exist
                (:create
                 (#_CreateResFile pf)
                 (setq res (#_OpenResFile pf))
                 (when (< res 0) (err (#_ResError))))
                (:error
                 (err code))
                ((nil) (return-from ccl::open-resource-file nil))
                (t (error (%badarg if-does-not-exist '(member nil :create :error))))))))
        res))))

;; Similar to with-open-resource-file, but will not close an already open resource file
(defmacro with-open-res-file ((refnum-var file &key (if-does-not-exist :error) (errorp t))
                                 &body body)
  (let ((reslist (make-symbol "RESLIST")))
    (unless refnum-var (setf refnum-var (make-symbol "REFNUM")))
    `(let ((,reslist (OpenResFileNums))
           (,refnum-var nil))
       (unwind-protect
         (when
           (setq ,refnum-var
                 (open-resource-file ,file
                                     :if-does-not-exist ',if-does-not-exist
                                     :errorp ',errorp))
           (unless (= ,refnum-var (require-trap #_CurResFile))
             (require-trap #_UseResFile ,refnum-var))
           ,@body)
         (unless (or (null ,refnum-var) (memq ,refnum-var ,reslist))
           (close-resource-file ,refnum-var))))))

;; execute body with loading of resources turned off, making sure that it gets turned back on
(defmacro with-load-resources-off (&body body)
  `(unwind-protect
     (progn
       (require-trap #_setResLoad nil)
       ,@body)
     (require-trap #_setResLoad t)))

;; constants that are not available in an equ file, so have to be defined explicitly here
(eval-when (:compile-toplevel :execute)
  (defconstant $nextResMapHandle 16)
  (defconstant $resFileNumber 20))

;; return list of resFile numbers of all open res files
(defun OpenResFileNums ()
  (ccl:without-interrupts
   (let ((resFileNums nil))
     (ccl:with-macptrs ((resMapHandle (ccl:%get-ptr (ccl:%int-to-ptr #$TopMapHndl)))
                        (resMapPtr (ccl:%null-ptr)))
       (loop
         (when (ccl:%null-ptr-p resMapHandle) (return resFileNums))
         (ccl:%setf-macptr resMapPtr (ccl:%get-ptr resMapHandle))
         (push (ccl:%get-word resMapPtr $resFileNumber) resFileNums)
         (ccl:%setf-macptr resMapHandle (ccl:%get-ptr resMapPtr $nextResMapHandle)))))))

; Close the resource file with the given refnum
(defun close-resource-file (refnum)
  (#_CloseResFile refnum)
  (res-error))

; General error checker for resource manager traps
(defun res-error ()
  (let ((err (#_ResError)))
    (unless (eql 0 err)
      (%err-disp err))))

; Use the resource file with the given refnum
(defun use-resource-file (refnum)
  (prog1
    (#_CurResFile)
    (#_UseResFile refnum)
    (res-error)))

(defmacro using-resource-file (refnum &body body)
  (let ((old-refnum (gensym)))
    `(let (,old-refnum)
       (unwind-protect
         (progn
           (setq ,old-refnum (use-resource-file ,refnum))
           ,@body)
         (when ,old-refnum
           (use-resource-file ,old-refnum))))))

(defun current-resource-file ()
  (#_CurResFile))

; Get a resource with the given type and name-or-number.
; (string type) should be a four-character string
; name-or-number should be an integer or a string
; if used-file-only? is true, Get1Resource is used instead of GetResource.
; if load? is true (the default), load the resource as well.
; Return NIL if the resource is not found for any reason.
(defun get-resource (type name-or-number &optional 
                            used-file-only?
                            (load? t))
  (let ((res (if (integerp name-or-number)
               (if used-file-only?
                 (#_Get1Resource type name-or-number)
                 (#_GetResource type name-or-number))
               (with-pstr (ps name-or-number)
                 (if used-file-only?
                   (#_Get1NamedResource type ps)
                   (#_GetNamedResource type ps))))))
    (unless (%null-ptr-p res)
      (when load?
        (load-resource res))
      res)))

; Get the 'STR ' resource with the given NAME-OR-NUMBER
(defun get-string (name-or-number &optional used-file-only? dont-release)
  (let ((str (get-resource "STR " name-or-number used-file-only?)))
    (when str
      (unwind-protect
        (%get-string str)
        (unless dont-release (#_ReleaseResource str))))))

; get the INDEX'th string from the 'STR#' resource with the given NAME-OR-NUMBER
; Returns NIL if there is no such 'STR#' resource.
; Returns two values, NIL and the number of strings in the resource, if there
; is a matching 'STR#' resource, but the INDEX is too big.
; INDEX starts at 1 to copy the broken Mac definition.
(defun get-ind-string (name-or-number index &optional used-file-only? dont-release)
  (unless (and (fixnump index) (>= index 1))
    (report-bad-arg index '(fixnum 1 *)))
  (let ((index (1- (the fixnum index)))
        (str# (get-resource "STR#" name-or-number used-file-only? nil)))
    (declare (fixnum index))
    (when str#
      (unwind-protect
        (without-interrupts               ; don't want anyone to purge this resource
         (load-resource str#)
         (let ((count (%hget-word str#)))
           (if (<= count index)
             (values nil count)
             (let ((offset 2))
               (dotimes (i index)
                 (declare (fixnum i))
                 (setq offset (+ 1 offset (%hget-byte str# offset))))
               (%get-string str# offset)))))
        (unless dont-release (#_ReleaseResource str#))))))

; Load a resource
(defun load-resource (resource)
  (#_LoadResource resource)
  (res-error))

; Release the given resource
(defun release-resource (resource)
  (#_ReleaseResource resource)
  (res-error))

; Add resource to the currently used resource
(defun add-resource (resource type id &key name attributes)
  (with-pstr (ps (or name ""))
    (#_AddResource resource type id ps)
    (res-error)
    (when attributes
      (#_SetResAttrs resource attributes)
      (res-error))
    resource))

(defun write-resource (resource)
  (#_WriteResource resource)
  (res-error))

(defun delete-resource (type id-or-name &optional (used-file-only? t))
  (unwind-protect
    (progn
      (#_SetResLoad nil)
      (let ((resource (get-resource type id-or-name used-file-only?)))
        (when resource
          (remove-resource resource)
          (#_DisposHandle resource)
          t)))
    (#_SetResLoad t)))

; Note that this does not free the memory allocated for the resource.
(defun remove-resource (resource)
  (using-resource-file (#_HomeResFile resource)
    (#_RmveResource resource))
  (res-error))

(defun detach-resource (resource)
  (#_DetachResource resource)
  (res-error))

(provide "RESOURCES")

#|
	Change History (most recent last):
	1  	 2/10/95	sidney  	Upgraded version of the resource functions found in the library folder
	2  	 2/27/97	Hernan  	
|# ;(do not edit past this line!!)
