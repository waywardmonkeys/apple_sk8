;;;; Projector-related utility functions.
;;;; These used to live in projector.lisp
;;;;

(in-package :SourceServer)

;;;
;;; merging
;;;

(defvar *user-initials*)
(defvar *user* nil)
(defvar *last-root-dir* "")
(defvar *last-merge-dir* "")

(defun directory-path-with-suffix (pathname suffix)
  (let ((directory (pathname-directory (mac-namestring pathname))))
    (make-pathname :directory
                   (append (butlast directory)
                           (list
                            (concatenate 'string (first (last directory)) suffix))))))

(defun merge-dir-list (root &optional (partial-name " Merge"))
  (mapcar #'pathname (sort (mapcar #'mac-namestring
                                   (directory (directory-path-with-suffix
                                               root
                                               (concatenate 'string partial-name "*"))
                                              :directories t))
                           #'string>)))

(defun latest-merge-dir (root &optional (partial-name " Merge"))
  (first (merge-dir-list root partial-name)))

(defun new-merge-dir (root &optional (partial-name " Merge"))
  (let* ((latest (latest-merge-dir root partial-name))
         (new-dir
          (directory-path-with-suffix
           root
           (concatenate 'string partial-name
                        (princ-to-string
                         (if latest
                           (let ((leaf (first (last (pathname-directory latest)))))
                             (1+ (read-from-string leaf nil -1
                                                   :start (1+ (position-if-not #'digit-char-p leaf :from-end t)))))
                           0))))))
;    (create-directory new-dir)
    new-dir))

(defun find-corresponding-path (path source target)
  (let* ((path-dir (pathname-directory (mac-namestring path)))
         (source-dir (pathname-directory (mac-namestring source)))
         (target-dir (pathname-directory (mac-namestring target)))
         (path-length (length path-dir))
         (source-length (length source-dir)))
    (assert (and (>= path-length source-length)
                 ;; equalp because the directory names may have different case
                 (equalp (subseq path-dir 0 source-length) (subseq source-dir 0 source-length)))
            nil "~S is not in ~S" path source)
    (make-pathname :directory (append target-dir (subseq path-dir source-length)))))

(defun move-files-to-merge-directory (paths &optional (partial-name " Merge"))
  (let ((project->dir-hash (make-hash-table :test #'eq))
        merge-dirs)
    (labels ((project->dir (project)
               (or (gethash project project->dir-hash)
                   (let* ((local-dir (project-local-dir project))
                          (parent (project-parent project))
                          (merge-dir
                           (if parent
                             (find-corresponding-path local-dir
                                                      (project-local-dir parent)
                                                      (project->dir parent))
                             (new-merge-dir local-dir partial-name))))
                     (when parent
                       (push (cons local-dir merge-dir) merge-dirs)
                       (setq *last-root-dir* local-dir
                             *last-merge-dir* merge-dir))
                     (unless (probe-file merge-dir)
                       (create-file merge-dir))
                     (setf (gethash project project->dir-hash) merge-dir)))))
      (dolist (path paths)
        (let ((new-path (merge-pathnames (project->dir (pathname-project path))
                                         path)))
          (format t "Moving ~s to ~s~&" path new-path)
          (rename-file path new-path)
          (remove-filename-from-projector-menu path))))
    merge-dirs))

;; Ought to let you choose from a list of merge directories.  Unfortunately,
;; we don't know which merge directory goes with which original.  We could write
;; a file into each merge directory that had the name of the original.  Also, we
;; could assume the toplevel project, in the case where there's only one.
(defun choose-and-merge-directories ()
  (declare (special *last-root-dir* *last-merge-dir*))
  (make-instance 'merge-directory-dialog
    :main-dir *last-root-dir*
    :merge-dir *last-merge-dir*))

;;;
;;; Comment logging
;;;

(let* ((header-key "Change History (most recent last):")
       (pre-header (format nil "~%#|~%~a" #\tab))
       (post-header (format nil "|# ;(do not edit past this line!!)"))
       (header (concatenate 'string pre-header header-key (string #\Newline) post-header (string #\Newline)))
       ;; For some reason, searching for the last-comment-indicator with a tab at the end
       ;; fails in some weird cases, leaving sourceserver free to corrupt your file. I removed
       ;; the tab from the search string. 
       (last-comment-indicator (format nil "~%~~")))

  ;;; This function returns the position of the end of the comment block.
  ;;; If not found, it returns an error instead of corrupting your file. Now isn't that nice?

(defun find-comment-end-pos (window buf)
  (let ((comment-end-pos (buffer-string-pos buf post-header :end t :from-end t)))
    (unless comment-end-pos
      (error (format nil "Could not find end of sourceserver comment block in ~a. ~
                          And, (what do you know?), instead of corrupting your file, I decided to ~
                          let you correct the problem. " window)))
    comment-end-pos))

;;; Trying to get our text to format in a nicer way. Changed the # of tabs to 7
;;; and added padding to that the user names and the versions and dates are all
;;; the same. (Hernan).

(defun add-change (window version comment check-in-p)
  (unless (boundp 'SourceServer::*user-initials*) (error "~s is not set" 'SourceServer::*user-initials*))
  (let* ((buf (fred-buffer window))
         (buf-size (buffer-size buf))
         (header-pos (buffer-string-pos buf header-key :end t :from-end t))    ; search for Change log from the end of the file
         (scroll-pos (buffer-position (fred-display-start-mark window))))
    (multiple-value-bind (selection-start selection-end) (selection-range window) ; remember selection
      (unless header-pos
        (setq header-pos buf-size)
        (buffer-insert buf header buf-size))
      (let* ((comment-end-pos (find-comment-end-pos window buf))
             (comment-start-pos (or (and check-in-p
                                         (buffer-string-pos buf last-comment-indicator 
                                                            :start header-pos 
                                                            :end comment-end-pos
                                                            :from-end t))
                                    comment-end-pos)))
        (buffer-delete buf comment-start-pos comment-end-pos)
        (set-mark buf comment-start-pos)
        (multiple-value-bind (dummy dummy dummy day month year) (get-decoded-time)
          (declare (ignore dummy))
          (if check-in-p
            (format window "~&~a" #\tab)
            (format window "~&~2~~a" #\tab))
          ;; Inser the comment header. Make all initials padded to 8 chars.
          (format window "~3a~a~2d/~2d/~2d~a~8a~a" version
                  #\tab month day (mod year 100) #\tab SourceServer::*user-initials* #\tab)
          ;; Now compute the number of tabs for the comment and add the comment.
          (let ((tabNumber 6))
            (format window "~a~%" (comment-format comment tabNumber)))
          ))
      (set-selection-range window selection-start selection-end)
      (set-mark (fred-display-start-mark window) scroll-pos)
      )))

#|

(defun add-change (window version comment check-in-p)
  (unless (boundp 'SourceServer::*user-initials*) (error "~s is not set" 'SourceServer::*user-initials*))
  (let* ((buf (fred-buffer window))
         (buf-size (buffer-size buf))
         (header-pos (buffer-string-pos buf header-key :end t :from-end t))    ; search for Change log from the end of the file
         (scroll-pos (buffer-position (fred-display-start-mark window))))
    (multiple-value-bind (selection-start selection-end) (selection-range window) ; remember selection
      (unless header-pos
        (setq header-pos buf-size)
        (buffer-insert buf header buf-size))
      (let* ((comment-end-pos (find-comment-end-pos window buf))
             (comment-start-pos (or (and check-in-p
                                         (buffer-string-pos buf last-comment-indicator 
                                                            :start header-pos 
                                                            :end comment-end-pos
                                                            :from-end t))
                                    comment-end-pos)))
        (buffer-delete buf comment-start-pos comment-end-pos)
        (set-mark buf comment-start-pos)
        (multiple-value-bind (dummy dummy dummy day month year) (get-decoded-time)
          (declare (ignore dummy))
          (if check-in-p
            (format window "~&~a" #\tab)
            (format window "~&~2~~a" #\tab))
          (format window "~a~a~d/~d/~d~a~a~a~a~%" version
                  #\tab month day (mod year 100) #\tab SourceServer::*user-initials* #\tab (comment-format comment))))
      (set-selection-range window selection-start selection-end)
      (set-mark (fred-display-start-mark window) scroll-pos))))

|#

(defun find-comment-from-file (path)
  (let* ((w (make-instance *default-editor-class* :filename path :window-show nil))
         (comment (find-comment-from-window w)))
    (window-close w)
    comment))

(defun find-comment-from-window (window)
  (let* ((buf (fred-buffer window))
         (header-pos (buffer-string-pos buf header-key :end t :from-end t)))    ; search for Change log from the end of the file
    (if header-pos
      (let* ((comment-end-pos (find-comment-end-pos window buf))
             (comment-start-pos (buffer-string-pos buf last-comment-indicator 
                                                   :start header-pos 
                                                   :end comment-end-pos
                                                   :from-end t)))
        (if comment-start-pos
          (unformat-comment (buffer-substring buf (1+ comment-start-pos) comment-end-pos))
          ""))
      "")))
)

(defun unformat-comment (comment)
  (string-trim '(#\Tab #\Space #\Return)
               (let* ((first-return (position #\return comment))
                      (start-tab (position #\tab comment :end first-return :from-end t)))
                 (delete #\tab (subseq comment (1+ start-tab))))))
                       

;;; Lets you tell it how many tabs are required so that the comment is formatted nicely.
;;; Will also remove double quotes from beginning and end (if they are there)
;;; Could add something to delete trailing <returns>

(defun comment-format (comment &optional (numtabs 4))
  (let* ((c1 (if (and (> (length comment) 0)
                      (eql (elt comment  0) #\"))
               (subseq comment 1)
               comment))
         (len-c1 (1- (length c1)))
         (c2 (if (and (> len-c1 0)
                      (eql (elt c1 len-c1 ) #\"))
               (subseq c1 0 len-c1)
               c1)))
    (string-right-trim wsp&cr 
                       (replace-char-with-string c2 
                                                 #\newline 
                                                 (let ((result (string #\Newline)))
                                                   (dotimes (i numTabs result)
                                                     (setf result (concatenate 'string result (string #\tab)))))
                                                 ))))


(defun string-push-extend (str1 str2)
  (unless (array-has-fill-pointer-p str1)
    (error "First arg must have a fill pointer."))
  (let ((len1 (length str1))
        (size1 (array-dimension str1 0))
        (len2 (length str2)))
    (when (> (+ len1 len2) size1)
      (adjust-array str1 (+ len1 len2)))
    (setf (fill-pointer str1) (+ len1 len2))
    (replace str1 str2 :start1 len1)
    str1))

;;; Change the char FROM to the string TO in the string STR
(defun replace-char-with-string (str from to)
  (let ((newstr (make-array 20 :element-type 'character :adjustable t :fill-pointer 0)))
    (map nil #'(lambda (xchar)
                 (if (eql xchar from)
                   (string-push-extend newstr to)
                   (vector-push-extend xchar newstr)))
         str)
    newstr))

(defun open-pathname-comment-log-window-p (path &optional no-comment)
  ;; second value = t if the window already existed
  ;; also makes sure only one window is open onto this file
  (let ((window (pathname-window path)))
    (if window
      (progn
        (do-pathname-windows (other path)
          (unless (eq other window)
            (window-close other)))
        (values window t))
      (if no-comment
        (when (eq (mac-file-type path) :text)
          (values (make-instance *default-editor-class* :filename path :window-show nil)
                  nil))
        (and (eq (mac-file-type path) :TEXT)
             (y-or-n-dialog (format nil "Add comment to file: ~S?" (namestring path)))
             (values (make-instance *default-editor-class* :filename path :window-show nil)
                     nil))))))

(defun inc-projector-version (version)
  (if (find-if-not #'digit-char-p version)
    (error "Can't handle branches yet.")
    (princ-to-string (1+ (read-from-string version)))))

;;;
;;; Version hacking
;;;

(defun parse-version (version &optional (errorp t))
  (let ((major-rev 0)
        (minor-rev 0)
        (dev-stage 'f)
        (bug-rev 0)
        (internal-rev 0)
        (substage nil)
        (substage-rev 0)
        (pos 0))
    (flet ((next-piece (end-test)
             (if pos
               (let ((end-pos (position-if end-test version :start pos)))
                 (let ((*package* (find-package :projector)))
                   (prog1
                     (read-from-string version t nil :start pos :end end-pos)
                     (setq pos end-pos))))))
           (bad-version (&rest args)
             (if errorp
               (apply #'error args)
               (return-from parse-version (values nil nil nil nil nil nil)))))
      (let ((temp (next-piece #'(lambda (x) (eq #\. x)))))
        (if (numberp temp) 
          (setq major-rev temp)
          (bad-version "No major revision number in version: ~s" version)))
      (if pos (incf pos))
      (let ((temp (next-piece #'(lambda (x) (not (digit-char-p x))))))
        (if (numberp temp)
          (setq minor-rev temp)
          (bad-version "No minor revision number in version: ~s" version)))
      (when (and pos (eq #\. (schar version pos)))
        ;;bug fix version
        (incf pos)
        (let ((temp (next-piece #'(lambda (x) (not (digit-char-p x))))))
          (if (numberp temp)
            (setq bug-rev temp)
            (bad-version "Invalid bug fix revision number in version: ~s" version))))
      (when pos
        (let ((temp (next-piece #'digit-char-p)))
          (if (memq temp '(f b a d))
            (setq dev-stage temp)
            (bad-version "Invalid development stage in version: ~s" version))))
      (when pos
        (let ((temp (next-piece #'(lambda (x) (not (digit-char-p x))))))
          (if (numberp temp)
            (setq internal-rev temp)
            (bad-version "Invalid internal revision number in version: ~s" version))))
      (when pos
        (setq substage (next-piece #'digit-char-p)))
      (when pos
        (let ((temp (next-piece #'(lambda (x) (not (digit-char-p x))))))
          (if (numberp temp)
            (setq substage-rev temp)
            (bad-version "Invalid substage revision number in version: ~s" version))))
      (when pos
        (bad-version "Invalid version: ~s" version)))
    (values major-rev minor-rev bug-rev dev-stage internal-rev substage substage-rev)))

#|
(parse-version "1.0d23")
(parse-version "2.0d23")
(parse-version "1.1d23")
(parse-version "1.0")
(parse-version "1.0a1")
(parse-version "1.0b4")
(parse-version "1.0.1")
(parse-version "1.1")
(parse-version "1.0.2f3")
(parse-version "71.2f3c2")
|#

(defun version-lessp (version1 version2)
  (multiple-value-bind (major-rev1 minor-rev1 bug-rev1 stage1 internal-rev1 sub-stage1 sub-stage-rev1)
                       (parse-version version1)
    (multiple-value-bind (major-rev2 minor-rev2 bug-rev2 stage2 internal-rev2 sub-stage2 sub-stage-rev2)
                         (parse-version version2)
      (or (< major-rev1 major-rev2)
          (and (= major-rev1 major-rev2)
               (or (< minor-rev1 minor-rev2)
                   (and (= minor-rev1 minor-rev2)
                        (or (< bug-rev1 bug-rev2)
                            (and (= bug-rev1 bug-rev2)
                                 (or (< (position stage1 '(d a b f))
                                        (position stage2 '(d a b f)))
                                     (and (eq stage1 stage2)
                                          (or (< internal-rev1 internal-rev2)
                                              (and (= internal-rev1 internal-rev2)
                                                   (eq sub-stage1 sub-stage2)
                                                   (< sub-stage-rev1 sub-stage-rev2))))))))))))))

#|
(version-lessp "1.0d23" "2.0d23")
(version-lessp "1.0d23" "1.1d23")
(version-lessp "1.0d23" "1.0")
(version-lessp "1.0a1" "1.0")
(version-lessp "1.0b4" "1.0")
(version-lessp "1.0b4" "1.0.1")
(version-lessp "1.0" "1.0.1")
(version-lessp "1.0.1" "1.0.2")
(version-lessp "1.0" "1.1")
(version-lessp "1.0d2" "1.0a1")
(version-lessp "1.0d2" "1.0b1")
(version-lessp "1.0a2" "1.0b1")
(version-lessp "1.0d12" "1.0d13")
(version-lessp "1.0a2" "1.0.4")
(version-lessp "1.0f3c1" "1.0f3c2")

(version-lessp "2.0d23" "1.0d23")
(version-lessp "1.1d23" "1.0d23")
(version-lessp "1.0" "1.0d23")
(version-lessp "1.0" "1.0a1")
(version-lessp "1.0" "1.0b4")
(version-lessp "1.0.1" "1.0b4")
(version-lessp "1.0.1" "1.0")
(version-lessp "1.0.2" "1.0.1")
(version-lessp "1.1" "1.0")
(version-lessp "1.0a1" "1.0d2")
(version-lessp "1.0b1" "1.0d2")
(version-lessp "1.0b1" "1.0a2")
(version-lessp "1.0d13" "1.0d12")
(version-lessp "1.0.4" "1.0a2")
(version-lessp "1.0f3c2" "1.0f3c1")

|#

(defun make-version (&key (major-rev 1)
                          (minor-rev 0)
                          (bug-rev 0)
                          (stage 'f)
                          (internal-rev 0)
                          (substage nil)
                          (substage-rev 0))
  (unless (and (integerp major-rev) 
               (integerp minor-rev)
               (integerp bug-rev)
               (memq stage '(d a b f))
               (integerp internal-rev)
               (integerp substage-rev))
    (error "Incorrect arguments"))
  (with-output-to-string (ret)
    (format ret "~a.~a" major-rev minor-rev)
    (when (not (zerop bug-rev)) (format ret ".~a" bug-rev))
    (unless (and (eq stage 'f) (eq internal-rev 0) (null substage))
      (let ((*print-case* :downcase)) (format ret "~a~a" stage internal-rev))
      (when substage
        (format ret "~a~a" substage substage-rev)))))

(defun inc-version (version)
  (multiple-value-bind (major-rev minor-rev bug-rev stage internal-rev substage substage-rev)
                       (parse-version version)
    (cond (substage (incf substage-rev))
          ((neq internal-rev 0) (incf internal-rev))
          ((neq bug-rev 0) (incf bug-rev))
          (t (incf minor-rev)))
    (make-version :major-rev major-rev :minor-rev minor-rev :bug-rev bug-rev
                  :stage stage :internal-rev internal-rev :substage substage
                  :substage-rev substage-rev)))

#|
	Change History (most recent last):
	2	3/25/92	ows	fix a braino
	3	3/25/92	ows	projector comment ate my code
	4	4/4/93	Sidney	
				Use SourceServer package instead of CCL and use
				MCL 2.0 low-level system calls instead of ones
				from verison 1.3 that required loading a verison of
				sysequ.lisp that could lead to name collisions in
				the ccl package.
	5	3/8/94	sidney	Allow not asking for comments when checking out a file
	6  	 8/12/94	Hernan  	Changing comment-format to take the number of
							tabs you want.
	7  	 9/ 7/94	sidney  	move defvar *user* from later in load
	2  	 6/23/95	Hernan  	 to 6.ourceServer package instead of CCL and use
										MCL 2.0 low-level system calls instead of ones
										from verison 1.3 that re	Use S
	3  	 4/18/96	Hernan  	find-comment-from-window is really stupid at finding the
						end of the sourceserver comment block. I am fixing it to
						be able to handle invisible characters after the comments
						block.
	4  	 4/22/96	Hernan  	Fixing open-pathname-comment-log-window-p to not
						bring up any dialogs if no-comment is true.

|# ;(do not edit past this line!!)