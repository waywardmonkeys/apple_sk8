;;
;;  read-only.lisp
;;
;;  teach fred about projector resources and read-only windows
;;
;;  Based on work by Ora Lassila (ora@hutcs.hut.fi)


;;; 11/05/91 gz  Convert to new traps.
;;  11/05/91  alms mini-buffer icons moved to listeners-and-windows
;;  31-oct-91 jaj add projector icons to mini-buffer
;;  10/09/91  alms many changes
;;  27-sep-91 jaj  added transfer-ckid
;;  15-aug-91 gz   added symbolic ckid constants
;;  15-aug-91 jaj added set-file-modify-read-only
;;  05-Jun-91 Vrhel.T to clean up comtab, add minibuffer alert for checked out files
;;  09-May-91 Andy Stadler   to better support ModifyReadOnly
;;  31-jan-91 alms Ported to MCL 2.0 and expanded

(in-package :SourceServer)

;;access to the ckid resource

(eval-when (eval compile #-debugged load)
  (defrecord (ckid :handle)
    (checkSum longint)
    (LOC longint)
    (version integer)                   ;  this definition is for VERSION 4
    (readOnly integer)                  ;  0 = readonly   nonzero = readwrite
    (branch byte)
    (modifyReadOnly boolean)            ;  T = modreadonly 
    (unused longint)
    (checkoutTime longint)
    (modDate longint)
    (pida longint)
    (pidb longint)
    (userID integer)
    (fileID integer)
    (revID integer)
    (projectlen byte))

#-ppc-target
(eval-when (:compile-toplevel :execute)
  (require :lapmacros))

;; file-modifiable-state is:  -1 = modifyreadonly,  0 = readwrite, >0 = readonly, -2 is checkedout 

(defconstant $ckid-checkedout -2)
(defconstant $ckid-modifyreadonly -1)
(defconstant $ckid-readwrite 0)
(defconstant $ckid-readonly 1)
)

(defun file-modifiable-state (filename)
  (or
  (when filename
    (cl-user::with-open-res-file
     (ref filename :if-does-not-exist nil)
     (and ref (neq ref -1)
          (with-resource (ckid "ckid" 128)
            (cond ((neq 4 (rref ckid ckid.version)) $ckid-readwrite)
                  ((neq 0 (rref ckid ckid.readOnly)) $ckid-checkedout)
                  ((rref ckid ckid.modifyReadOnly) $ckid-modifyreadonly)
                  (t $ckid-readonly))))))
                   $ckid-readwrite))

(defun set-file-modify-read-only (filename)
  (unless (probe-file filename) (error "Non-existent file: ~s" filename))
  (cl-user::with-open-res-file (ref filename :if-does-not-exist :create)
      (with-resource (ckid "ckid" 128)
        (setf (rref ckid ckid.modifyReadOnly) t)
        (setf (rref ckid ckid.checksum) (handle-checksum ckid))
        (#_ChangedResource ckid)
        (#_WriteResource ckid))))

(defun set-file-local-checked-out-p (filename value)
  (unless (probe-file filename) (error "Non-existent file: ~s" filename))
  (cl-user::with-open-res-file (ref filename :if-does-not-exist :create)
    (with-resource (ckid "ckid" 128)
      (setf (rref ckid ckid.readonly) (if value 8 0))
      (setf (rref ckid ckid.checksum) (handle-checksum ckid))
      (#_ChangedResource ckid)
      (#_WriteResource ckid))))

(defun handle-checksum (handle)
  (let ((count (- (truncate (#_GetHandleSize handle) 4) 1)))
    (with-dereferenced-handles ((p handle))
      (pointer-checksum (%incf-ptr p 4) count))))

#-ppc-target
(defun pointer-checksum (p count)
  (decf count)                          ; dbf stops when negative
  (ccl::lap-inline ()
    (:variable p count)
    ;(ccl::dc.w #_debugger)
    (ccl::move.l (ccl::varg p) ccl::a0)
    (ccl::move.l (ccl::a0 ccl::$macptr.ptr) ccl::a0)
    (ccl::move.l (ccl::varg count) ccl::da)
    (ccl::getint ccl::da)
    (ccl::clr.l ccl::dx)
    @loop
    (ccl::add.l ccl::a0@+ ccl::dx)
    (ccl::dbf ccl::da @loop)
    (ccl::move.l ccl::dx ccl::d0)
    (ccl::jsr_subprim ccl::$sp-mklong)))

#+ppc-target
(ccl::defppclapfunction pointer-checksum ((p ccl::arg_y) (count ccl::arg_z))
  (ccl::check-nargs 2)
  (ccl::macptr-ptr ccl::imm0 p)
  (ccl::la ccl::imm0 -4 ccl::imm0)
  (ccl::li ccl::imm2 0)
  (ccl::li ccl::imm3 '1)
  loop
  (ccl::sub. ccl::arg_z ccl::arg_z ccl::imm3)
  (ccl::lwzu ccl::imm1 4 ccl::imm0)
  (ccl::add ccl::imm2 ccl::imm2 ccl::imm1)
  (ccl::bgt loop)
  (ccl::box-signed-byte-32 ccl::arg_z ccl::imm2 ccl::imm0)
  (ccl::blr))

(defun remove-ckid-resource (filename)
  (if (null filename)
    0
    (cl-user::with-open-res-file (ref filename :if-does-not-exist nil)
      (unless (or (null ref) (eql ref -1))
        (loop  ;; repeat until done. SourceServer sometimes creates multiple bogus CKID resources
          (when
            (with-resource (ckid "ckid" 128)
              (#_RemoveResource ckid))
            (return t)))))))

(defun transfer-ckid (from-file to-file &aux ckid)
  (unless (probe-file from-file) (error "Non-existent file: ~s" from-file))
  (unless (probe-file to-file) (error "Non-existent file: ~s" to-file))
  (cl-user::with-open-res-file (ref from-file)
      (setf ckid (#_Get1Resource "ckid" 128))
      (when (%null-ptr-p ckid) (error "No 'ckid' resource in file: ~s" from-file))
      (#_DetachResource ckid))
  (cl-user::with-open-res-file
   (ref to-file :if-does-not-exist :create)
   (loop  ;; repeat until done. SourceServer sometimes creates multiple bogus CKID resources
     (when
       (with-resource (old "ckid" 128)
         (#_RemoveResource old))
       (return t)))
   (ccl::write-resource (add-resource ckid "ckid" 128))))


;;  new version of file-is-modifiable-p, defined in terms of file-modifiable-state

(defun file-is-modifiable-p (filename)
  (or (null filename)                   ;  empty filename, return t
      (< (file-modifiable-state filename) $ckid-readonly)))     ;  -1 or 0 are writeable

;;;
;;;  Use a hash table to attach the file-is-modifiable value to each window
;;;


(defmethod read-only-state ((f fred-window))
  (view-get f :ckid-state $ckid-readwrite))

(defmethod (setf read-only-state) (state (f fred-window))
  (view-put f :ckid-state state)
  (ccl::%buffer-set-read-only (fred-buffer f) (eql state $ckid-readonly))
  (update-fred-window-from-changed-file f)
  )

(defmethod is-read-only ((f fred-window))
  (= (read-only-state f) $ckid-readonly))

(defmethod initialize-instance :after ((f fred-window) &rest ignore)
  (declare (ignore ignore))
  (setf (read-only-state f) (file-modifiable-state (window-filename f)))
  (mini-buffer-update f))


#|
	Change History (most recent last):
	2	5/6/91	ads	1.  Supressing the "beep" when opening RO files
				2.  Fixing RO problems w/cut, paste, return
	2	5/30/91	tv 	Added :read-only to *features* to prevent multiple loads
	3	6/3/91	jcg	update to Leibniz 1.0:
				  + put in ralphdebug package (adbg)
	4	6/4/91	wrs	Cleaned up a bit, moved to PROJECTOR package, and
				turned over to TV.
	5	6/5/91	tv  	cleaned up dialog behavior, comtab errors
				
	7	6/6/91	   	making backwards compatible to MacRalph
	8	6/7/91	   	fixing MacRalph compatibility
	9	6/7/91	   	macralph stuff
	10	6/7/91	   	More MacRalph Compatibility
	11	6/10/91	   	adbg::find-or-make-buffer removed
	12	6/18/91	tv 	Lost code! re-adding remove-ckid-resource
	13	6/20/91	tv 	basic sanity test of 1.1d13
               08/05/91 gz      Use symbolic constants.
                                null -> %null-ptr-p in file-modifiable-state
	5	11/5/91	alms	Move mini-buffer-update definition.
	7	1/21/92	jaj	added set-file-local-checked-out-p
	3	4/4/93	Sidney	Use SourceServer package instead of CCL and use
				MCL 2.0 low-level system calls instead of ones
				from verison 1.3 that required loading a verison of
				sysequ.lisp that could lead to name collisions in
				the ccl package.
	4	4/5/93	sidney	Checkin good version of file that got corrupted
				somehow in the data base
	5	7/7/94	sidney	Use string for resource id instead of a keyword symbol
				            get rid of errors caused by files with no resource fork
	6	7/14/94	sidney	release heap memory when we're through with resources.
	7	7/14/94	sidney	write-resource in proper package
	8  	 9/13/94	sidney  	get correct file-modifiable-state on files with no resource fork
	9  	10/ 4/94	sidney  	remove unnecessary trap call that was causing errors in remove-ckid
	10 	 2/17/95	Hernan  	in set read-Only-State, = -> eql because the state
							can be t which is not a number.

~~	11 	10/ 4/94	sidney  	add ppc version of lap code. change _rmvrsrce to _removeresource
|# ;(do not edit past this line!!)