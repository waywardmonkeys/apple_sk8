;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories

;-*- Mode: Lisp; Package: CCL -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; fasl-concatenate.lisp
;;; Copyright 1992-1994, Apple Computer, Inc
;;; Copyright 1995, Digitool, Inc
;;;
;;; Concatenate fasl files.

;;; Format of a fasl file as expected by the fasloader.
;;;
;;; #xFF00         2 bytes - File version
;;; Block Count    2 bytes - Number of blocks in the file
;;; addr[0]        4 bytes - address of 0th block
;;; length[0]      4 bytes - length of 0th block
;;; addr[1]        4 bytes - address of 1st block
;;; length[1]      4 bytes - length of 1st block
;;; ...
;;; addr[n-1]      4 bytes
;;; length[n-1]    4 bytes
;;; length[0] + length[1] + ... + length [n-1] bytes of data

;; 04/28/93 mwp Release

(in-package :ccl)

(export '(fasl-concatenate))

(defconstant $fasl-id #xff00)          ; fasl file id

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; (fasl-concatenate out-file fasl-files &key :if-exists)
;;
;; out-file     name of file in which to store the concatenation
;; fasl-files   list of names of fasl files to concatenate
;; if-exists    as for OPEN. Defaults to :error
;;
;; function result: pathname to the output file.
;; All file types default to "FASL"
;; It works to use the output of one invocation of fasl-concatenate
;; as an input of another invocation.
;;


(defun fasl-concatenate (out-file fasl-files &key (if-exists :error))
  (let ((count 0)
        (created? nil)
        (finished? nil))
    (declare (fixnum count))
    (dolist (file fasl-files)
      (unless (eq (mac-file-type file) #.(ccl::make-keyword (pathname-type *.fasl-pathname*
                                                                           :case :common)))
        (error "Not a fasl file: ~s" file))
      (with-open-file (strm file)
        (multiple-value-bind (r ra) (stream-reader strm)
          (unless (eql $fasl-id (reader-read-word r ra))
            (error "Bad fasl file ID in ~s" file))
          (incf count (reader-read-word r ra)))))
    (unwind-protect
      (with-open-file (strm out-file
                              :direction :output
                              :if-does-not-exist :create
                              :if-exists if-exists)
        ;; comment this out so file creator type defaults to the application's creator type (set-mac-file-creator out-file :ccl2)
        (set-mac-file-type out-file #.(ccl::make-keyword (pathname-type *.fasl-pathname*
                                                                        :case :common)))
        (setq created? t)
        (multiple-value-bind (w wa) (stream-writer strm)
          (let ((addr-address 4)
                (data-address (+ 4 (* count 8))))
            (writer-write-word 0 w wa)         ;  will be $fasl-id
            (writer-write-word count w wa)
            (dotimes (i (* 2 count))
              (writer-write-long 0 w wa))       ; for addresses/lengths
            (dolist (file fasl-files)
              (with-open-file (in-stream file)
                (multiple-value-bind (r ra) (stream-reader in-stream)
                  (reader-read-word r ra)    ; skip ID
                  (let* ((fasl-count (reader-read-word r ra))
                         (addrs (make-array fasl-count))
                         (sizes (make-array fasl-count))
                         addr0)
                    (declare (fixnum fasl-count)
                             (dynamic-extent addrs sizes))
                    (dotimes (i fasl-count)
                      (setf (svref addrs i) (reader-read-long r ra)
                            (svref sizes i) (reader-read-long r ra)))
                    (setq addr0 (svref addrs 0))
                    (file-position strm addr-address)
                    (dotimes (i fasl-count)
                      (writer-write-long
                       (+ data-address (- (svref addrs i) addr0))
                       w wa)
                      (writer-write-long (svref sizes i) w wa)
                      (incf addr-address 8))
                    (file-position strm data-address)
                    (dotimes (i fasl-count)
                      (file-position in-stream (svref addrs i))
                      (let ((fasl-length (svref sizes i)))
                        (dotimes (j fasl-length)
                          (funcall w wa (funcall r ra)))
                        (%fbflush (slot-value strm 'fblock))
                        (incf data-address fasl-length)))))))
            (file-length strm data-address)
            (file-position strm 0)
            (writer-write-word $fasl-id w wa)
            (setq finished? t))))
      (when (and created? (not finished?))
        (delete-file out-file))))
  out-file)


(defun writer-write-byte (byt writer writer-arg)
  (declare (fixnum byt))
  (funcall writer writer-arg (%code-char (logand #xff byt))))

(defun writer-write-word (wrd writer writer-arg)
  (declare (fixnum wrd))
  (writer-write-byte (the fixnum (ash wrd -8)) writer writer-arg)
  (writer-write-byte (the fixnum (logand #xff wrd)) writer writer-arg))

(defun writer-write-long (lng writer writer-arg)
  (writer-write-word (ash lng -16) writer writer-arg)
  (writer-write-word (logand #xffff lng) writer writer-arg))

(defun reader-read-byte (reader reader-arg)
  (char-code (the character (funcall reader reader-arg))))

(defun reader-read-word (reader reader-arg)
  (the fixnum
    (logior (the fixnum 
              (ash (the fixnum (reader-read-byte reader reader-arg))
                   8))
            (the fixnum (reader-read-byte reader reader-arg)))))

(defun reader-read-long (reader reader-arg)
  (logior (ash (reader-read-word reader reader-arg) 16)
          (reader-read-word reader reader-arg)))

; End of fasl-concatenate.lisp

#|

(let ((forms '((ed-beep)))
      newfile
      (files nil))
  (dotimes (i 2)
    (loop
      (setf newfile (concatenate 'string "ccl;" (symbol-name (gensym "temp"))))
      (unless (probe-file newfile)) (return))
    (push newfile files)
    (with-open-file (*fasdump-stream* newfile :direction :output
                                      :element-type 'base-character ; huh
                                      :if-exists :supersede
                                      :if-does-not-exist :create
                                      :external-format :fasl)
      (declare (ignore *fasdump-stream*))
      )
    (with-compilation-unit ()
      (ccl::%compile-forms
       forms
       nil
       newfile
       nil
       :overwrite t)))
  (ccl::fasl-concatenate "ccl;dumptest1.fasl" files :if-exists :supersede)
  ;(mapc #'delete-file files)
  )

(mac-file-type "ccl;dumptest1.fasl")

(load "ccl;dumptest1.fasl")

|#
#|
	Change History (most recent last):
	1  	 7/14/95	Brian Roddy	Modified version from the examples folders
	2  	 9/13/95	sidney  	use application's creator type, not hardcoded :ccl2
	2  	 5/23/96	sidney  	use the compile time MCL's file type for fasl, not hardcoded :fasl
	3  	 2/27/97	Hernan  	
|# ;(do not edit past this line!!)
