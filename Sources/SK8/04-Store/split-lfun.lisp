;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories

;;;-*- Mode: Lisp; Package: CCL -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; split-lfun.lisp
;; Code to split an lfun into pieces that WOOD knows how to save
;;
;; Copyright © 1992-1996 Apple Computer, Inc. All rights reserved.
;; Permission is given to use, copy, and modify this software provided
;; that Apple is given credit in all derivative works.
;; This software is provided "as is". Apple makes no warranty or
;; representation, either express or implied, with respect to this software,
;; its quality, accuracy, merchantability, or fitness for a particular
;; purpose.
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Modification History
;;
;; 03/20/96 bill  ppc-target code
;; -------------- 0.93
;; 06/30/95 bill  split-lfun now works in MCL 3.0.
;; -------------- 0.9
;; 10/13/94 gz    optional preserve-lfun-info-p arg to split-lfun
;; 10/11/94 bill  split-lfun no longer attempts to skip immediate map when
;;                there is none. Thanx to Chris DiGiano for finding this one.
;; -------------- 0.8
;; 11/09/93 bill  %patch-lfun-immediates
;; -------------- 0.6
;; -------------- 0.5
;; 03/13/92 bill  New file
;;

(in-package :ccl)

(export '(split-lfun join-lfun))

(defvar *fasl-min-version* fasl-version)
(defvar *fasl-max-version* fasl-version)

#-ppc-target
(progn

(eval-when (:compile-toplevel :execute)
  (require :lispequ)
  (require :lapmacros))

; 2 bytes of attributes + 4 bytes of bits in words:
(defconstant $lfun-overhead-words 3)

(defconstant $lfun-linkmap-offset
  #+ccl-3 3                             ; bits at end in MCL 3.0
  #-ccl-3 1)                            ; bits at beginning in MCL 2.0

; Returns a list of length 6: (imms icode linkmap bits attrib fasl-version)
; If you APPLY JOIN-LFUN to this list, you will get a copy of the lfun.
; Note that the icode & linkmap vectors are of type (array (unsigned-byte 16)).
; ccl::%make-lfun requires that they be of exactly that type.
; Code largely copied from fasl-dump-lfun-vector.
; Tested in 2.0f3c2

(defun split-lfun (lfun preserve-lfun-info-p)
  (let* ((lfunv (%lfun-vector lfun))
         (lfunv-len (uvsize lfunv))
         icode
         (imm-count (%count-immrefs lfunv))
         (imms nil)
         (linkmap (make-array (ash imm-count 1) 
                              :element-type '(signed-byte 16)
                              :initial-element $lm_longimm))
         (bits (lfun-bits lfun))
         (attrib (lfun-attributes lfun))
         (info-index (and (not preserve-lfun-info-p)
                          (%ilogbitp $lfatr-symmap-bit attrib)
                          (%i- imm-count (if (%ilogbitp $lfatr-noname-bit attrib) 1 2)))))
    (declare (fixnum lfunv-len imm-count))
    (if (logbitp $lfatr-slfunv-bit attrib)
      ; swappable lfun-vectors have an extra longword at the end
      (decf lfunv-len 2))
    ; Skip the immediate map at the end of the lfun vector.
    (when (logbitp $lfatr-immmap-bit attrib)
      (do ((i (- lfunv-len $lfun-linkmap-offset) (1- i)))
          ((< i 0) (error "Immediate map took entire lfun"))
        (decf lfunv-len)
        (let ((word (uvref lfunv i)))
          (declare (fixnum word))
          (if (or (eql 0 (logand #xff word))
                  (eql 0 (logand #xff00 word)))
            (return)))))
    (decf lfunv-len  $lfun-overhead-words)     ; skip the header.
    (setq icode (make-array lfunv-len :element-type '(signed-byte 16)))
    (do ((i 0 (1+ i))
         (j (/ (- $lfv_lfun $v_data) 2) (1+ j))
         (immno -1)
         (u-imm-count 0))
        ((>= i lfunv-len) (setq imm-count u-imm-count))
      (declare (fixnum i j))
      (if (%immref-p i lfunv)
        (multiple-value-bind (imm offset)
                             (%nth-immediate lfunv (incf immno 1))
          (when (eq immno info-index) (setq imm nil offset nil))
          (let ((first-imm (memq imm imms))
                (v-immno u-imm-count))
            (if first-imm
              (setq v-immno (length (cdr first-imm)))
              (progn
                (push imm imms)
                (incf u-imm-count)))
            (setf (aref icode i) (or offset 0))
            (setf (aref icode (1+ i)) v-immno)
            (setf (aref linkmap (+ immno immno)) 
                  (%immediate-offset lfunv immno))
            (incf i)
            (incf j)))
        (setf (aref icode i) (uvref lfunv j))))
    (list (make-array imm-count :initial-contents (nreverse imms))
          icode linkmap bits attrib fasl-version)))

; imms is a sequence of Lisp values, preferably of type (array t)
; icode is an array of opcodes, preferably of type (array (unsigned-byte 16)).
; linkmap is alternating (byte) offsets in icode and
; $lm_longimm's,  preferably of type (array (unsigned-byte 16)).
; bits is the LFUN-BITS of the function.
; attrib is its LFUN-ATTRIBUTES.

; At each linkmap referenced offset in icode, there are two (16-bit)
; words: a constant to add to the immediate (offsets a symbol to its
; value cell or function entry) and the index in IMMS for the immediate
; that goes there. This function just calls %MAKE-LFUN after coercing the
; sequences to the correct type and doing a little error checking.

; The list returned by split-lfun is taylor made to call join-lfun.
; (apply 'join-lfun (split-lfun #'split-lfun)) will get you a copy
; of #'split-lfun.

(defun join-lfun (imms icode linkmap bits attrib &optional (fver fasl-version))
  (unless (<= *fasl-min-version* fver *fasl-max-version*)
    (cerror "they're compatible. Stop bothering me with error messages."
            "LFUN saved with FASL version #x~x, ~s is now #x~x."
            fver 'fasl-version fasl-version)
    (setq *fasl-min-version* (min fver *fasl-min-version*)
          *fasl-max-version* (max fver *fasl-max-version*)))
  (symbol-macrolet ((array-type '(array (signed-byte 16))))
    (let* ((imms (if (typep imms '(array t))
                   imms
                   (coerce imms '(array t))))
           (imms-length (length imms))
           (icode (if (typep icode array-type)
                    icode
                    (coerce icode array-type)))
           (icode-bytes (* 2 (length icode)))
           (linkmap (if (typep linkmap array-type)
                      linkmap
                      (coerce icode array-type)))
           (linkmap-length (length linkmap))
           (bits (require-type bits 'fixnum))
           (attrib (require-type attrib 'fixnum)))
      (unless (evenp linkmap-length)
        (error "~s has an odd number of elements." linkmap))
      (do ((i 0 (+ i 2)))
          ((>= i linkmap-length))
        (declare (fixnum i))
        (let ((offset (aref linkmap i))
              (type (aref linkmap (the fixnum (1+ i)))))
          (declare (fixnum offset))
          (unless (eql type $lm_longimm)
            (error "Type code ~s is not ~s" type $lm_longimm))
          (unless (and (evenp offset) (< -1 offset icode-bytes))
            (error "Offset ~s odd or out of range." offset))
          (setq offset (ash offset -1))
          (let ((sym-adjust (aref icode offset))
                (imms-index (aref icode (the fixnum (1+ offset)))))
            (declare (fixnum sym-adjust imms-index))
            (unless (and (< -1 imms-index imms-length)
                         (or (eql sym-adjust 0)
                             (and (symbolp (aref imms imms-index))
                                  (or (eql sym-adjust 8)
                                      (eql sym-adjust 16)))))
              (error "Malformed immediate specifier at index ~s in ~s"
                     offset icode)))))
      (%make-lfun imms icode linkmap bits attrib))))

; WOOD needs to create an LFUN before filling in its immediates.
; This function creates an LFUN with dummy immediates and three values
; suitable for passing to %patch-lfun-immediates below (after filling in
; the immediates):
;   1) The lfun
;   2) The dummy immediates vector. FIll this in with the real immediates
;   3) A vector containing the index in the immediates vector for each immediate in the lfun
(defun join-lfun-with-dummy-immediates (icode linkmap bits attrib &optional (fver fasl-version))
  (let* ((imm-count (ash (length linkmap) -1))
         (indices (make-array imm-count))
         (max-index -1))
    (declare (fixnum imm-count))
    (dotimes (i imm-count)
      (let ((index (aref icode (1+ (ash (aref linkmap (+ i i)) -1)))))
        (when (> index max-index) (setq max-index index))
        (setf (aref indices i) index)))
    (let ((imms (make-array (1+ max-index) :initial-element '*%dummy-imm%*)))
      (values (join-lfun imms icode linkmap bits attrib fver)
              imms
              indices))))

; Patch lfun by changing its immediates to new-immediates.
; indices contains an index in new-immediates for each immediate in lfun.
(defun %patch-lfun-immediates (lfun new-immediates indices)
  (let* ((lfv (%lfun-vector lfun))
         (count (%count-immrefs lfv)))
    (declare (fixnum count))
    (setq new-immediates (require-type new-immediates 'simple-vector)
          indices (require-type indices 'simple-vector))
    (unless (eql count (length indices))
      (error "indices not a vector of the correct length"))
    (dotimes (i count)
      (let ((index (%svref indices i)))
        (multiple-value-bind (old-imm imm-offset) (%nth-immediate lfv i)
          (declare (ignore old-imm))
          (let ((lfun-offset (%immediate-offset lfv i)))
            (lap-inline (imm-offset lfun-offset (svref new-immediates index))
              (:variable lfun)
              (move.l (varg lfun) atemp0)
              (getint arg_x)              ; imm-offset
              (getint arg_y)              ; lfun-offset
              (add.l arg_x arg_z)
              (move.l arg_z (atemp0 arg_y)))))))
    (lap-inline () (jsr_subprim $sp-clrcache)))
  lfun)

)  ; end of #-ppc-target progn

#+ppc-target
(progn

(defun split-lfun (lfun &optional preserve-lfun-info-p)
  (declare (ignore preserve-lfun-info-p))
  (unless (functionp lfun)
    (setq lfun (require-type lfun 'function)))
  (let* ((code-vector (normalize-code-vector (copy-uvector (uvref lfun 0))))
         (code-vector-length (uvsize code-vector))
         (code-words (make-array (length code-vector)
                                 :element-type '(unsigned-byte 32)))
         (size (1- (uvsize lfun)))
         (imm-vector (make-array size)))
    (declare (fixnum code-vector-length size))
    (dotimes (i size)
      (setf (%svref imm-vector i) (uvref lfun (1+ i))))
    ; This conses bignums out the wazoo
    (dotimes (i code-vector-length)
      (setf (aref code-words i) (uvref code-vector i)))
    (list imm-vector code-words size fasl-version)))

(defun check-fasl-version (fver)
  (unless (<= *fasl-min-version* fver *fasl-max-version*)
    (cerror "they're compatible. Stop bothering me with error messages."
            "LFUN saved with PFSL version #x~x, ~s is now #x~x."
            fver 'fasl-version fasl-version)
    (setq *fasl-min-version* (min fver *fasl-min-version*)
          *fasl-max-version* (max fver *fasl-max-version*))))

(defun join-lfun (imm-vector code-words imms-count &optional (fver fasl-version))
  (check-fasl-version fver)
  (unless (eql imms-count (the fixnum (length imm-vector)))
    (error "imms-count mismatch"))
  (let* ((code-vector-length (length code-words))
         (code-vector (make-uvector code-vector-length ppc::subtag-code-vector))
         (lfun (make-uvector (1+ imms-count) ppc::subtag-function)))
    (setf (uvref lfun 0) code-vector)
    (dotimes (i imms-count)
      (setf (uvref lfun (1+ i)) (aref imm-vector i)))
    (dotimes (i code-vector-length)
      (setf (uvref code-vector i) (aref code-words i)))
    (without-interrupts (%make-code-executable code-vector))
    lfun))

(defun join-lfun-with-dummy-immediates (code-words imms-count &optional (fver fasl-version))
  (check-fasl-version fver)
  (let* ((code-vector-length (length code-words))
         (imms (make-array imms-count))
         (code-vector (make-uvector code-vector-length ppc::subtag-code-vector))
         (lfun (make-uvector (1+ imms-count) ppc::subtag-function)))
    (setf (uvref lfun 0) code-vector)
    (dotimes (i code-vector-length)
      (setf (uvref code-vector i) (aref code-words i)))
    (without-interrupts (%make-code-executable code-vector))
    (values lfun imms)))

(defun %patch-lfun-immediates (lfun new-immediates &optional ignore)
  (declare (ignore ignore))
  (let ((imms-count (length new-immediates)))
    (unless (eql (length lfun) (1+ imms-count))
      (error "Wrong length immediates vector"))
    (dotimes (i imms-count)
      (setf (uvref lfun (1+ i)) (aref new-immediates i)))
    lfun))

)  ; end of #+ppc-target progn


;;;    1   3/10/94  bill         1.8d247
;;;    2  10/13/94  gz           1.9d074
;;;    3  11/01/94  Derek        1.9d085 Bill's Saving Library Task
;;;    2   3/23/95  bill         1.11d010
;;;    3   8/01/95  bill         1.11d065

#|
	Change History (most recent last):
	1  	 5/20/96	sidney  	Add split-lfun from wood 0.94 to build
	2  	 2/27/97	Hernan  	
|# ;(do not edit past this line!!)
