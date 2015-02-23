;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories


;;;


(in-package :CCL)

;; we used to define packages and logical hosts here, but we now can assume that it is all done in the saved MCL 2.0.1 for SK8 Build image
;; So we'll just check that we are in a development version of SK8 or the MCL for SK8 builds.

(unless (and (boundp 'cl-user::*development-p*) cl-user::*development-p*)
  (error "This scanner file must be run in a development version of the MCL image used for SK8 builds"))

(defvar MF::*SAVING-INTO-PROJECT-STORE* nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Make sure the import/export functions and the package utilities are loaded
;;;

(unless (fboundp 'CCL::pretty-symbol-name)
  (load "ccl;Build Part 1:Patches to base MCL:symbol-case-saving"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Make sure SourceServer's up and mounted, and the files are all up to date!
;;;

;;; this should never be necessary, since a development version should have SourceServer loaded already, but checking doesn't hurt
(unless (boundp 'CCL::*my-projects*)
  (format t "~%~%  Loading SourceServer...~%")
  (load "ccl;SourceServer Source:load-SourceServer")
  (setq *default-menubar* (copy-list ccl::%menubar)))

;; this is similarly true
(unless CCL::*my-projects*
  (setq SOURCESERVER::*scan-on-mount* NIL)
  (setq ccl::*user-initials* nil)
  (setq ccl::*user* "Adam Chipkin")
  (setq ccl::*my-projects* '(("Implementor's Gig:SK8 1.0 Project:" #.(namestring (truename "ccl;"))))))

(unless SOURCESERVER::*mounted-projects*
  (SOURCESERVER::reset-projects)
  (setq SOURCESERVER::*scan-on-mount* NIL)
  (format t "~%~%  Mounting SourceServer projects...~%")
  (SOURCESERVER::mount-all-projects))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Finally, the reason for this file's existence:
;;;


(defun map-all-SK8-lisp-files (fcn &optional start-at-file)
  (let ((files (directory #p"ccl;SK8:**:*.lisp"))
        err?)
    (when start-at-file
      (setq files (member start-at-file files :test #'equalp)))
    (dolist (f files)
      (when (and (command-key-p) (shift-key-p) (option-key-p) (control-key-p))
        (format t "~%STOPPED SCANNING.~%")
        (return))
      (format t "~%NOW SCANNING ~s...~%" f)
      (when (setq err? (nth-value 1 (ignore-errors (funcall fcn f))))
        (format t "~&   IGNORING FILE DUE TO ERROR:~%   ")
        ;(SK8::printObject err? *standard-output*)
        (princ err? *standard-output*)
        (terpri *standard-output*)))))



(defun remove-inherited-syms-from-ui-syms-files ()
  (let* ((SK8-pkg-info (assq #.(find-package :SK8) MF::*SK8-Dev-packages*))
         (UI-pkg-info (assq #.(find-package :UI) MF::*SK8-Dev-packages*))
         SK8-public-symbols
         SK8-special-symbols)
    
    (let ((MF::*fetch-exports-list* t)
          (*load-verbose* nil))
      (declare (special MF::*fetch-exports-list*))
      (load (fifth SK8-pkg-info))
      (setq SK8-public-symbols (reduce #'(lambda (current nextList) (nconc current (mapcar #'symbol-name (cdr nextList))))
                                       MF::*fetch-exports-list*
                                       :initial-value nil)))
    
    (let ((MF::*fetch-exports-list* t)
          (*load-verbose* nil))
      (declare (special MF::*fetch-exports-list*))
      (load (seventh SK8-pkg-info))
      (setq SK8-special-symbols (reduce #'(lambda (current nextList) (nconc current (mapcar #'symbol-name (cdr nextList))))
                                        MF::*fetch-exports-list*
                                        :initial-value nil)))
    
    (flet ((remove-from-syms-file (other-file other-pkg private? special?)
             (let ((MF::*fetch-exports-list* t)
                   (*load-verbose* nil)
                   specials-move-list
                   testfun
                   other-syms)
               ;; when checking symbols declared special, if we find it in the non-special file of the SK8 package symbols, we
               ;; need to move it to the special one so it is both published and proclaimed special
               (setf testfun (if special?
                               #'(lambda(sym)
                                   (let ((nam (symbol-name sym)))
                                     (or (member nam SK8-special-symbols :test #'string=)
                                         (when (member nam SK8-public-symbols :test #'string=)
                                           (pushnew sym specials-move-list)
                                           t))))
                               #'(lambda(sym) (let ((nam (symbol-name sym)))
                                                (or (member nam SK8-public-symbols :test #'string=)
                                                    (member nam SK8-special-symbols :test #'string=))))))
               (load other-file)
               (setq other-syms MF::*fetch-exports-list*)
               (dolist (category other-syms)
                 (setf specials-move-list nil)
                 (setf (cdr category)
                       (delete-if testfun
                                  (cdr category)))
                 (when specials-move-list
                   (mf::add-to-exports-file (seventh SK8-pkg-info) specials-move-list (first SK8-pkg-info) (car category) :special t)
                   (setf SK8-special-symbols (nconc specials-move-list SK8-special-symbols))))
               (MF::write-SK8-symbols-file other-file other-pkg other-syms private? special?)
               )))
      
      (remove-from-syms-file (fifth UI-pkg-info) (first UI-pkg-info) nil nil)
      (remove-from-syms-file (sixth UI-pkg-info) (first UI-pkg-info) t nil)
      (remove-from-syms-file (seventh UI-pkg-info) (first UI-pkg-info) nil t)
      t)))


(defun remove-redundant-private-syms-from-syms-files ()
  (flet ((remove-redundant-public-and-private-syms (pkg public-syms-file private-syms-file special-syms-file)
           (let ((MF::*fetch-exports-list* t)
                 (*load-verbose* nil)
                 public-syms
                 special-syms
                 private-syms-categories)
             (load public-syms-file)
             (setq public-syms MF::*fetch-exports-list*)
             (setq MF::*fetch-exports-list* t)
             (load special-syms-file)
             (setq special-syms (reduce #'(lambda (current nextList) (nconc current (cdr nextList)))
                                        MF::*fetch-exports-list*
                                        :initial-value nil))
             (dolist (category public-syms)
               (setf (cdr category) (delete-if #'(lambda (sym) (memq sym special-syms)) (cdr category))))
             (MF::write-SK8-symbols-file public-syms-file pkg public-syms nil nil)
             (setq public-syms (reduce #'(lambda (current nextList) (nconc current (cdr nextList)))
                                       public-syms
                                       :initial-value nil))
             (setq MF::*fetch-exports-list* t)
             (load private-syms-file)
             (setq private-syms-categories MF::*fetch-exports-list*)
             (dolist (category private-syms-categories)
               (setf (cdr category) (delete-if #'(lambda (sym) (or (memq sym public-syms) (memq sym special-syms))) (cdr category))))
             (MF::write-SK8-symbols-file private-syms-file pkg private-syms-categories t nil))))
    (dolist (pkg-info MF::*SK8-Dev-packages*)
      (remove-redundant-public-and-private-syms (first pkg-info) (fifth pkg-info) (sixth pkg-info) (seventh pkg-info)))))


(defun make-symbols-files-modifiable (&key empty-them)
  (let ((proj (SOURCESERVER::PATHNAME-PROJECT-P (fifth (first MF::*SK8-Dev-packages*)))))
    (flet ((maybe-checkout (p f)
             (unless (sourceserver::projector-file-info p f '(:local-checked-out-p)) (sourceserver::file-checkout f :no-comment t))))
      
      (dolist (pkg-info MF::*SK8-Dev-packages*)
        (maybe-checkout proj (fifth pkg-info))
        (maybe-checkout proj (sixth pkg-info))
        (maybe-checkout proj (seventh pkg-info)))
      
      (when empty-them
        ;; First empty out the existing symbols files!
        (dolist (pkg-info MF::*SK8-Dev-packages*)
          (MF::write-SK8-symbols-file (fifth pkg-info) (first pkg-info) NIL nil nil)
          (MF::write-SK8-symbols-file (sixth pkg-info) (first pkg-info) NIL t nil)
          (MF::write-SK8-symbols-file (seventh pkg-info) (first pkg-info) NIL nil t))))))



(defun checkin-symbols-files ()
  (let ((proj (SOURCESERVER::PATHNAME-PROJECT-P (fifth (first MF::*SK8-Dev-packages*)))))
    (flet ((check-it-in (p f)
             (if (SOURCESERVER::PROJECT-MOUNTED-P p)
               (SOURCESERVER::FILE-CHECKIN f :NO-COMMENT T)
               (error "Project ~s is not mounted!!!" proj))))
      
      (dolist (pkg-info MF::*SK8-Dev-packages*)
        (check-it-in proj (fifth pkg-info))
        (check-it-in proj (sixth pkg-info))
        (check-it-in proj (seventh pkg-info))))))



(defvar *scan-buffer* (make-buffer))

(defun maybe-update-exports (filename)
  (let ((buf *scan-buffer*)
        (fileDirs (last (pathname-directory filename) 2))
        old-modcnt)
    
    (cond
     ((and (string-equal (second fileDirs) "Preload")
           (string-equal (first fileDirs) "SK8"))
      (format t "~&   Ignoring the file because it's in the SK8;Preload folder~%"))
     
     (t
      (CCL::%buffer-set-read-only buf nil)
      (buffer-delete buf 0 (buffer-size buf))
      (buffer-insert-file buf filename 0)
      (CCL::%buffer-set-read-only buf nil)
      (setq old-modcnt (buffer-modcnt buf))
      
      (MF::buffer-update-dev-package-and-exports buf (MF::file-category filename))
      
      ;; If the file was changed, check in the new version!
      (unless (eql old-modcnt (buffer-modcnt buf))
        (format t "~&   Modified the file -- checking it in~%")
        (let ((proj (SOURCESERVER::PATHNAME-PROJECT-P filename)))
          (unless (SOURCESERVER::PROJECTOR-FILE-INFO proj filename '(:local-checked-out-p))
            (SOURCESERVER::file-checkout filename :no-comment t))
          (buffer-write-file buf filename :if-exists :overwrite)
          (if (SOURCESERVER::PROJECT-MOUNTED-P proj)
            (SOURCESERVER::FILE-CHECKIN filename :NO-COMMENT T)
            (error "Project ~s is not mounted!!!" proj))))))))



(defun scan-files-and-update-preload (&optional continue-from-file)
  (when continue-from-file
    (unless (eq t continue-from-file) (setq continue-from-file (truename continue-from-file))))
  
  (unless continue-from-file
    (cond
     ((y-or-n-dialog "Update all projects before scanning the sources?  (Hint: this is HIGHLY RECOMMENDED)")
      (SOURCESERVER::checkoutNewerAll))
     ((y-or-n-dialog "Let me just make SURE you know what you're doing Ñ scanning out of date files could put the sources out of whack!  So would you like to update them now?")
      (SOURCESERVER::checkoutNewerAll))))
  
  (eval-enqueue `(let ((elapsed-time (- (get-universal-time) ,(get-universal-time))))
                   (multiple-value-bind (hours nonhours)
                                        (floor elapsed-time 3600)
                     (multiple-value-bind (minutes seconds)
                                          (floor nonhours 60)
                       (format t "~%~%Scan completed in ~d hours, ~d minutes, ~d seconds.~%~%"
                               hours minutes seconds )))))

  (make-symbols-files-modifiable :empty-them (not continue-from-file))
  
  (unless (eq t continue-from-file)
    (map-all-SK8-lisp-files 'maybe-update-exports continue-from-file))
  
  (format t "~%~%NOW REMOVING REDUNDANT SYMBOLS FROM THE UI SYMBOLS FILES...~%")
  (remove-inherited-syms-from-ui-syms-files)
  
  (format t "~%~%NOW REMOVING REDUNDANT PRIVATE SYMBOL DECLARATIONS FROM THE SYMBOLS FILES...~%")
  (remove-redundant-private-syms-from-syms-files)
  
  (checkin-symbols-files)
  
  (format t "~%DONE.~%")
  t)

; (scan-files-and-update-preload #P"Melqu’ades:SK8 1.0§-Chip:SK8:Function Library:with-handlers.lisp")
; (scan-files-and-update-preload t)

(scan-files-and-update-preload)


#|
	Change History (most recent last):
	5	8/16/93	kleiman	define MF::*SAVING-INTO-PROJECT-STORE*
	6	9/23/93	kleiman	make-sk8-package no longer takes :use arg
	7	9/23/93	kleiman	fixed args in call to buffer-update-dev-package-and-exports
	8	9/23/93	kleiman	now no longer scans the "...:SK8:Preload:" folder
	9	10/4/93	chip	
	10	11/17/93	chip	now uses "read-eval-patch" instead of requiring the "stubs" file
	11	11/22/93	It 	fixed load order (the read-eval-patch loads before the scanning utils) & pathnames
	12	2/18/94	sidney	load-development-utils now defines *SK8-Dev-packages, so it has to be loaded
	13	2/20/94	sidney	separate out symbols to be proclaimed as special (e.g., SK8 objects)
	14	2/21/94	sidney	changes because things are different now
	15	2/21/94	sidney	modifyreadonly and checkin the new symbol files too
	16	2/25/94	sidney	removed loading of utilities that were moved into the MCL image we are using
	17	2/25/94	sidney	more removal to base build
	18	3/3/94	sidney	checkout files instead of making them modify readonly
	19	3/8/94	sidney	Try to keep from asking for comments
	20	6/29/94	chip	now loads symbol-case-saving file from the right directory
	2  	 2/27/97	Hernan  	
|# ;(do not edit past this line!!)
