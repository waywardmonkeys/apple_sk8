;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Merge.lisp
;;;    Origins unknown
;;;
;;;  10/18/91 better directory merge, file merge, and merge central default positions
;;;  Modified 5-23-91 by Vrhel.T to include merge-stream-and-file, buffer-streams, etc.
;;;      This file provides the compare/merge menu item in the CCL Tools menu
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :SourceServer)

(defparameter *merge-window-1-position* #@(5 50))
(defparameter *merge-window-2-position* (make-point (truncate (- *screen-width* 10) 2) 50))
(defparameter *merge-window-1-size* (make-point (truncate (- *screen-width* 10) 2)
                                                (truncate (- *screen-height* 180) 2)))
(defparameter *merge-window-2-size* *merge-window-1-size*)
(defparameter *merge-central-position* (list :top (- *screen-height* 128)))
(defparameter *merge-directory-window-position* *window-default-position*)

(defclass merge-central (dialog)
  ((window1 :accessor window1 :initarg :window1)
   (window2 :accessor window2 :initarg :window2)
   (difs1 :accessor difs1 :initarg :difs1)
   (difs2 :accessor difs2 :initarg :difs2)
   (remaining-difs1 :accessor remaining-difs1 :initarg :remaining-difs1)
   (remaining-difs2 :accessor remaining-difs2 :initarg :remaining-difs2)
   (current-dif1 :accessor current-dif1 :initform ())
   (current-dif2 :accessor current-dif2 :initform ())
   (finish-action :reader finish-action :initarg :finish-action
                  :initform #'(lambda ())))
  (:default-initargs
    :view-size #@(359 127)
    :window-type :document
    :view-position *merge-central-position*
    :window-title "Merge Central"
    :view-subviews (let ((button-size #@(158 16)))
                     (list (make-dialog-item 'button-dialog-item
                             #@(10 14) button-size "Next Difference"
                             'goto-next-merge-difference
                             :view-nick-name 'next-button)
                           (make-dialog-item 'button-dialog-item
                             #@(10 40) button-size "Previous Difference"
                             'goto-previous-merge-difference
                             :view-nick-name 'previous-button)
                           (make-dialog-item 'button-dialog-item
                             #@(10 66) button-size "Done - Close"
                             'announce-merge-completion
                             :view-nick-name 'complete-button)
                           (make-dialog-item 'button-dialog-item
                             #@(190 92) button-size "Cancel"
                             'cancel-merge
                             :view-nick-name 'cancel-button)
                           (make-dialog-item 'button-dialog-item
                             #@(190 14) button-size ">> Replace >>"
                             'copy-to-right
                             :view-nick-name 'source-1-button)
                           (make-dialog-item 'button-dialog-item
                             #@(190 40) button-size "Forget Difference"
                             'forget-difference
                             :view-nick-name 'source-2-button)
                           (make-dialog-item 'button-dialog-item
                             #@(190 66) button-size ">> Use Both Versions >>"
                             'use-both-versions
                             :view-nick-name 'both-button)
                           (make-dialog-item 'button-dialog-item
                             #@(10 92) button-size "Done - Keep Open"
                             'announce-merge-completion-open
                             :view-nick-name 'complete-open-button)))))

(defmethod announce-merge-completion ((item button-dialog-item))
  (announce-merge-completion (view-window item)))

(defmethod announce-merge-completion-open ((item button-dialog-item))
  (announce-merge-completion-open (view-window item)))

(defmethod announce-merge-completion ((controller merge-central))
    (let* ((finish-action (finish-action controller)))
        (when finish-action
             (funcall finish-action)))
    (update-merge-preference-file controller)
    (window-close (window1 controller))
    (window-close (window2 controller))
    (window-close controller))

(defmethod announce-merge-completion-open ((controller merge-central))
    (let* ((finish-action (finish-action controller)))
        (when finish-action
             (funcall finish-action)))
    (update-merge-preference-file controller)
    (window-close controller))

(defmethod cancel-merge ((item button-dialog-item))
  (cancel-merge (view-window item)))

(defmethod cancel-merge ((controller merge-central))
  (window-close (window2 controller))
  (window-close controller)
  (cancel))

(defmethod pop-dif1 ((window merge-central))
  (when (null (remaining-difs1 window))
    (setf (remaining-difs1 window)
          (difs1 window)))
  (setf (current-dif1 window)
        (pop (remaining-difs1 window))))

(defmethod pop-dif2 ((window merge-central))
  (when (null (remaining-difs2 window))
    (setf (remaining-difs2 window)
          (difs2 window)))
  (setf (current-dif2 window)
        (pop (remaining-difs2 window))))

(defmethod goto-next-merge-difference ((item button-dialog-item))
  (goto-next-merge-difference (view-window item)))

(defmethod goto-next-merge-difference ((controller merge-central))
  (let* ((window1 (window1 controller))
         (window2 (window2 controller))
         (dif1 (pop-dif1 controller))
         (dif2 (pop-dif2 controller)))
    (if dif1
      (progn
        (unless dif2 (error "this shouldn't happen"))
        (scroll-to-dif window1 dif1)
        (scroll-to-dif window2 dif2))
      (progn
        (dialog-item-disable (view-named 'next-button controller))
        (dialog-item-disable (view-named 'previous-button controller))))))

(defmethod goto-previous-merge-difference ((item button-dialog-item))
  (goto-previous-merge-difference (view-window item)))

(defmethod goto-previous-merge-difference ((controller merge-central))
  (let* ((window1 (window1 controller))
         (window2 (window2 controller))
         (dif1 (unpop-dif1 controller))
         (dif2 (unpop-dif2 controller)))
    (when dif1
      (unless dif2 (error "this shouldn't happen"))
      (scroll-to-dif window1 dif1)
      (scroll-to-dif window2 dif2))))

(defmethod unpop-dif1 ((window merge-central))
  (let* ((all-difs1 (difs1 window))
         (current-pos (position (current-dif1 window)
                                all-difs1)))
    (setf (remaining-difs1 window)
          (nthcdr (1- (if (zerop current-pos)
                        (length all-difs1)
                        current-pos))
                  all-difs1))
    (pop-dif1 window)))

(defmethod unpop-dif2 ((window merge-central))
  (let* ((all-difs2 (difs2 window))
         (current-pos (position (current-dif2 window)
                                all-difs2)))
    (setf (remaining-difs2 window)
          (nthcdr (1- (if (zerop current-pos)
                        (length all-difs2)
                        current-pos))
                  all-difs2))
    (pop-dif2 window)))

(defmethod use-source-2 ((item button-dialog-item))  ;; use old version
  (let* ((controller (view-window item)) )
      (clear (window1 controller))
    (copy (window2 controller))
    (multiple-value-bind (start end) (selection-range (window2 controller))
        (if (not (eql start end))
    (paste (window1 controller)))) 
    (remove-current-dif controller)))

(defmethod forget-difference ((item button-dialog-item))  ;; use old version
  (let* ((controller (view-window item)) )
    (remove-current-dif controller)))

(defmethod copy-to-right ((item button-dialog-item))  ;; use old version
  (let* ((controller (view-window item)) )
      (clear (window2 controller))
    (copy (window1 controller))
    (multiple-value-bind (start end) (selection-range (window1 controller))
        (if (not (eql start end))
    (paste (window2 controller)))) 
    (remove-current-dif controller)))

(defmethod use-source-1 ((item button-dialog-item)) ;; use new version
  (remove-current-dif (view-window item)))

(defmethod use-both-versions ((item button-dialog-item))
  (let* ((controller (view-window item))
         (window2 (window2 controller))
         (buffer (fred-buffer window2)))
    (multiple-value-bind (start end) (selection-range window2)
      (let* ((start-mark (make-mark buffer start))
             (end-mark (make-mark buffer end)))
        (collapse-selection window2 t)
        (buffer-insert buffer ";;start of retained text
" start-mark)
        (buffer-insert buffer ";;start of added text
" end-mark)
        (set-mark buffer end-mark)
        (copy (window1 controller))
        (paste window2)))
    (remove-current-dif controller)))

;;; test this - foo

(defmethod remove-current-dif ((win merge-central))
  (setf (difs1 win) (delete (current-dif1 win) (difs1 win)))
  (setf (difs2 win) (delete (current-dif2 win) (difs2 win)))
  (if (difs1 win)
    (if (difs2 win)
      (goto-next-merge-difference win)
      (error "this shouldn't happen"))
    (all-difs-merged win)))

(defmethod all-difs-merged ((win merge-central))
  (dialog-item-disable (view-named 'next-button win))
  (dialog-item-disable (view-named 'previous-button win))
  (dialog-item-disable (view-named 'source-1-button win))
  (dialog-item-disable (view-named 'source-2-button win))
  (dialog-item-disable (view-named 'both-button win))
  ;unfortunately, can't set the default button in a windoid
  ;because windoids don't get keystrokes
  ;(set-default-button win (view-named 'complete-button win))
  (collapse-selection (window1 win) t)
  (fred-update (window1 win))
  (collapse-selection (window2 win) t)
  (fred-update (window2 win)))

(defun merge-files (file1 file2 &optional difs? finish-action)
  (multiple-value-bind (difs1 difs2)
                       (get-file-difs file1 file2 difs?)
    (if difs1
      (let* ((win1 (make-sized-fred-window file1
                                           *merge-window-1-position*
                                           *merge-window-1-size*))
             (win2 (make-sized-fred-window file2
                                           *merge-window-2-position*
                                           *merge-window-2-size*)))
        (labels ((markify (buffer dif-list last-mark last-line-count)
                          (let* ((cell (car dif-list))
                                 (line-count (car cell))
                                 (the-rest (cdr dif-list))
                                 (mark (make-mark buffer
                                                  (buffer-line-start buffer
                                                                     last-mark
                                                                     (- line-count last-line-count)))))
                            (setf (car cell) mark)
                            (when the-rest
                              (markify buffer the-rest mark line-count)))))
          (when difs1
            (markify (fred-buffer win1) difs1 0 0)
            (markify (fred-buffer win2) difs2 0 0))
          (goto-next-merge-difference
           (make-instance 'merge-central
             :window-title (mac-namestring (pathname-name file1))
             :window1 win1
             :window2 win2
             :difs1 difs1
             :difs2 difs2
             :remaining-difs1 difs1
             :remaining-difs2 difs2
             :finish-action finish-action))))
      (progn
        (message-dialog (format nil "The files match:~% ~a~%"
                                (file-namestring file1)))
        (when finish-action
          (funcall finish-action))))))

#+test
(merge-files (choose-file-dialog) (choose-file-dialog))

(defun make-sized-fred-window (file position size)
  (let ((w (ccl::pathname-to-window  file)))
     (unless (and w 
              (y-or-n-dialog
               (format nil "Select old window to ~s or open a new window?"
                      (mac-file-namestring file))
               :yes-text "Old"
               :no-text "New"))
      (setq w (make-instance 'fred-window  :filename file  :window-show nil)))
    (set-view-position w position)
    (set-view-size w size)
    (window-select w)
    w))


;;;;;;;;;;;;;;;
;;
;; stuff for the preferences folder
;;

(defparameter *merge-preferences-filename* "merge-files prefs")

#+test
(find-preferences-folder)

(defun update-merge-preference-file (controller)
  (let* ((pathname (merge-pathnames
                    (find-preferences-folder) *merge-preferences-filename*)))
    (with-open-file (file pathname :direction :output
                                   :if-exists :supersede
                                   :if-does-not-exist :create)
      (let ((window1 (window1 controller))
            (window2 (window2 controller)))
        (format file
                "(setq *merge-window-1-position* ~s
      *merge-window-2-position* ~s
      *merge-window-1-size* ~s
      *merge-window-2-size* ~s
      *merge-central-position* ~s
      *merge-directory-window-position* ~s)"
                (setq *merge-window-1-position* (view-position window1))
                (setq *merge-window-2-position* (view-position window2))
                (setq *merge-window-1-size* (view-size window1))
                (setq *merge-window-2-size* (view-size window2))
                (setq *merge-central-position* (view-position controller))
                (let ((win (front-window :class 'merge-directory-dialog)))
                  (if win
                    (view-position win)
                    *merge-central-position*)))))))


(defun load-merge-preferences ()
  (let ((pathname (merge-pathnames
                   (find-preferences-folder) *merge-preferences-filename*))
        sizeok-p)
    (when (probe-file pathname)
      (load pathname :verbose nil)
      ;; ensure we can resize a window if it is not sized to our liking
      (setf sizeok-p (and (< (point-v *merge-window-1-size*) *screen-height*)
                          (< (point-v *merge-window-2-size*) *screen-height*)
                          (< (if (listp *merge-central-position*)
                               (second *merge-central-position*)
                               (point-v *merge-central-position*))
                             (- *screen-height* 64)))))
    (unless sizeok-p  ;; initialize window size and positions to defaults if there are no good preferences
      (setf *merge-window-1-position* #@(5 50)
            *merge-window-2-position* (make-point (truncate (- *screen-width* 10) 2) 50)
            *merge-window-1-size* (make-point (truncate (- *screen-width* 10) 2)
                                              (truncate (- *screen-height* 180) 2))
            *merge-window-2-size* *merge-window-1-size*
            *merge-central-position* (list :top (- *screen-height* 128))
            *merge-directory-window-position* *window-default-position*))))

(load-merge-preferences)


#+sample-code
(merge-files (choose-file-dialog :directory "lamont:old-level-1:l1-boot.lisp")
             (choose-file-dialog :directory "lamont:new-level-1:l1-boot.lisp"))
#+sample-code
(merge-files "lamont:old-level-1:l1-boot.lisp"
             "lamont:new-level-1:l1-boot.lisp")

(defclass merge-directory-dialog (dialog)
  ((merge-dir :accessor merge-dir :initarg :merge-dir)
   (main-dir :accessor main-dir :initarg :main-dir))
  (:default-initargs
    :window-type :document-with-grow
    :window-title "Files To Merge"
    :view-position *merge-directory-window-position*
    :view-size #@(509 262)
    :view-subviews (list
                    (make-dialog-item 'static-text-dialog-item
                      #@(20 6) #@(67 16) "Main Dir:" nil)
                    (make-dialog-item 'static-text-dialog-item
                      #@(10 34) #@(77 16) "Merge Dir:" nil)
                    (make-dialog-item 'editable-text-dialog-item
                      #@(90 6) #@(321 17) "" nil
                      :view-nick-name 'main-dir-text)
                    (make-dialog-item 'editable-text-dialog-item
                      #@(90 34) #@(321 17) "" nil
                      :view-nick-name 'merge-dir-text)
                    (make-dialog-item 'check-box-dialog-item
                      #@(229 72) #@(168 16) "Generate Difs Window" nil
                      :view-nick-name 'difs-window?)
                    (make-dialog-item 'button-dialog-item
                      #@(16 72) #@(90 16) "List Files"
                      'generate-file-list
                      :default-button t)
                    (make-dialog-item 'button-dialog-item
                      #@(125 72) #@(90 16) "Merge File"
                      'merge-selected-files
                      :view-nick-name 'merge-button
                      :dialog-item-enabled-p nil)
                    (make-dialog-item 'sequence-dialog-item
                      #@(0 98) #@(509 150) ""
                      #'(lambda (item)
                          (when (double-click-p)
                            (merge-selected-files item)))
                      :selection-type :disjoint
                      :table-hscrollp nil
                      :table-vscrollp t
                      :table-sequence ()
                      :view-font *fred-default-font-spec*
                      :view-nick-name 'the-table))))

(defmethod initialize-instance ((w merge-directory-dialog) &rest rest)
  (declare (ignore rest))
  (call-next-method)
  (set-dialog-item-text (view-named 'merge-dir-text w)
                        (namestring (merge-dir w)))
  (set-dialog-item-text (view-named 'main-dir-text w)
                        (namestring (main-dir w))))

(defmethod generate-difs? ((w merge-directory-dialog))
  (check-box-checked-p (view-named 'difs-window? w)))

(defmethod set-view-size ((w merge-directory-dialog) h &optional v)
  (call-next-method)
  (let* ((size (make-point h v)))
    (set-view-size (view-named 'the-table w)
                   (subtract-points size #@(0 106)))))

(defmethod merge-selected-files ((d dialog-item))
  (merge-selected-files (view-window d)))

(defmethod merge-selected-files ((w merge-directory-dialog))
  (let* ((table (view-named 'the-table w))
         (difs? (generate-difs? w))
         (cell (car (last (ccl:selected-cells table)))))
    (when cell
      (let* ((short-file (cell-contents table cell))
             (from-file (pathname (concatenate 'string (mac-namestring (merge-dir w)) short-file)))
             (to-file (pathname (concatenate 'string (mac-namestring (main-dir w)) short-file))))
        (eval-enqueue `(merge-files ,from-file ,to-file
                                    ,difs?
                                    ,#'(lambda ()
                                         (finished-merging-file w short-file))))))))

;; remove a cell in a sequence-dialog without disturbing the selections
;; This is tricky because selections are apparently stored by position
;; and the absolute positions are not changed by changing the underlying sequence

(defmethod remove-element-from-selected-cells ((table sequence-dialog-item) elem)
  (let* ((contents (table-sequence table))
         (del-pos (position elem contents)))
    (when del-pos
      (let ((selld (selected-cells table))
            (changelist nil))
        (dolist (cell selld)
          (let ((pos (cell-to-index table cell)))
            (when (<= del-pos pos)
              (cell-deselect table cell)
              (unless (= del-pos pos) (push (1- pos) changelist)))))
        (dolist (pos changelist)
          (cell-select table (index-to-cell table pos)))
        (set-table-sequence table (remove elem contents))))))

(defmethod finished-merging-file ((w merge-directory-dialog) file)
  (remove-element-from-selected-cells (view-named 'the-table w) file)
  (merge-selected-files w))

(defmethod generate-file-list ((item button-dialog-item))
  (generate-file-list (view-window item)))

(defmethod generate-file-list ((w merge-directory-dialog))
  (with-cursor *watch-cursor*
    (let* ((merge-dir (mac-namestring (dialog-item-text (view-named 'merge-dir-text w))))
           (len-merge-dir (length merge-dir))
           (file-list (mapcar #'(lambda (pathname)
                                  (subseq (mac-namestring pathname) len-merge-dir))
                              (directory (merge-pathnames ":**:*.*" merge-dir))))
           (merge-button (view-named 'merge-button w)))
      (setf (merge-dir w) (dialog-item-text (view-named 'merge-dir-text w))
            (main-dir w) (dialog-item-text (view-named 'main-dir-text w)))
      (set-table-sequence (view-named 'the-table w) file-list)
      (dialog-item-enable merge-button)
      (set-default-button w merge-button))))

(defun merge-directories (merge-dir main-dir)
  (make-instance 'merge-directory-dialog
    :merge-dir merge-dir
    :main-dir main-dir))

#+test
(merge-directories "ccl:leibniz 1.0d36 update;" "ccl:leibniz;")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Now let's make it usable
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Buffer streams are not a published feature of MacL - in fact, the manual
;;;  says that they are output-only. Not true.
;;;
;;; The following function makes a buffer-stream (see file cl-streams in 
;;;  level-1) and returns it. This stream supports all the normal stream
;;;  operations, and is an IO stream.
;;;

(defun make-buffer-stream (window)
  (let ((current-buffer (fred-buffer window)))
    (set-mark current-buffer 0)
    (make-instance 'ccl::buffer-stream 
      :buffer current-buffer)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; The following body of code provides the merge functionality. At a high level, it takes
;;;   a stream and a file, and allows the user to merge the outputs. Most of the changes from 
;;;   the functions in merge-files.lisp deal with using a buffer stream and a file, and being
;;;   a little smarter about how the windows are laid out....
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; The following function does some smart window layouts, based upon the screen size of the
;;;   current monitor.
;;;
        
;;; Now this will take some explaining -
;;;
;;;   Layout-windows takes two arguments, the first a point giving the area to be used, the second
;;;     a nested list of either view-sizes or T. T is used as a placeholder to indicate that
;;;     the size for the corresponding view is to be calculated by the routine.
;;;
;;;  The nesting of the list in the argument is important, as follows:
;;;    Top-level items indicate vertical bands across the screen. Sub-lists indicate that there 
;;;     are multiple panes in the horizontal direction. At the current time, sub-sublists are not
;;;     supported (kiss). Therefore, a structure like '((T T) #@(359 95)) would indicate that
;;;     there are two be two windows, side by side, above a single window of size 359 x 95. 
;;;
;;;  The function returns a list of the same structure, where each item is a pair of points. 
;;;    The car of the pair is the position, the  cdr the size. Thus, for a two page display with
;;;     the arguments:
;;;
;;;     the return value will be:
;;;

(defun layout-windows (layout-size layout-list &optional (start-point (make-point 0 40)))
    (let* ((gap-size 2)
                (screen-height ( - (point-v layout-size) (point-v start-point)))
                (screen-width (- (point-h layout-size) (point-h start-point)))
                (height-list (mapcar #'(lambda (sublist-or-pt)
                                                            (cond ((listp sublist-or-pt)
                                                                        (or (get-max-height-of-list sublist-or-pt)
                                                                               T))
                                                                      ((typep sublist-or-pt 'fixnum)
                                                                        (point-v sublist-or-pt))
                                                                      (T 0)))
                                                    layout-list))
                (committed-height (+ (reduce #'+ (remove-if #'symbolp height-list))
                                                      (* gap-size (length height-list))))
                (final-height-list (substitute (ceiling (/ (ceiling (- screen-height committed-height))
                                                                                      (max (count 0 height-list) 1)))
                                                                   0 height-list))
                (width-list (mapcar #'(lambda  (sublist-or-pt)
                                                           (cond ((listp sublist-or-pt)
                                                                       (mapcar #'(lambda (item)
                                                                                              (cond ((typep item 'fixnum)
                                                                                                          (point-h item))
                                                                                                        (T T)))
                                                                                      sublist-or-pt))
                                                                     ((typep sublist-or-pt 'fixnum)
                                                                       (point-h sublist-or-pt))
                                                                     (T T)))
                                                   layout-list))
                (committed-width-list
                  (mapcar #'(lambda  (sublist-or-wd)
                                         (cond ((listp sublist-or-wd)
                                                     (get-total-width-of-list sublist-or-wd))
                                                   ((typep sublist-or-wd 'fixnum)
                                                     (point-h sublist-or-wd))
                                                   (T 0)))
                                 width-list))
                size-list)
        (setf size-list
                 (mapcar 
                   #'(lambda (widths height committed-width)
                           (cond ((listp widths)
                                       (mapcar #'(lambda (wd) (make-point wd height))
                                                      (substitute (truncate (/ (truncate 
                                                                                                 (- screen-width 
                                                                                                       (+ committed-width
                                                                                                             (* gap-size (length widths)))))
                                                                                               (max (count T widths) 1)))
                                                                          T widths)))
                                     ((typep widths 'fixnum)
                                       (make-point widths height))
                                     (T (make-point (- screen-width 2) height))))
                   width-list
                   final-height-list
                   committed-width-list))
        (let ((cur-ht-pos (+ (point-v start-point) gap-size))
                  (cur-wd-pos (+ (point-h start-point) gap-size)))
            (mapcar #'(lambda (list-or-pt)
                                   (setf cur-wd-pos gap-size)
                                   (prog1 
                                       (cond ((listp list-or-pt)
                                                   (prog1 (mapcar 
                                                                  #'(lambda (size-pt)
                                                                          (prog1 (cons (make-point cur-wd-pos cur-ht-pos)
                                                                                                 size-pt)
                                                                              (incf cur-wd-pos (+ (point-h size-pt) gap-size))))
                                                                  list-or-pt)
                                                       (incf cur-ht-pos (+ (point-v (car list-or-pt)) gap-size))))
                                                 (T (prog1 (cons (make-point 
                                                                               (+ cur-wd-pos 
                                                                                     (truncate (/ (- screen-width (point-h list-or-pt))
                                                                                                           2)))
                                                                               cur-ht-pos)
                                                                             list-or-pt)
                                                          (incf cur-ht-pos (+ (point-v list-or-pt) gap-size)))))))
                           size-list))))


;;; These are support functions for layout-windows
;;;


(defun get-max-height-of-list (list-of-Ts-or-hts)
    (let* ((clean-list (remove-if-not #'numberp list-of-Ts-or-hts)))
        (cond ((< (length clean-list) 2)
                    (or (car clean-list) 0))
                  (T (reduce #'max clean-list)))))

(defun get-total-width-of-list (list-of-Ts-or-wds)
    (let* ((clean-list (remove-if-not #'numberp list-of-Ts-or-wds)))
        (cond ((< (length clean-list) 2)
                    (or (car clean-list) 0))
                  (T (reduce #'+ clean-list)))))


;;; Now we need a little describer fxn for testing the layout code
;;;
(defun d-pts (x)
    (cond ((null x)
                nil)
              ((numberp x)
                (point-string x))
              (T (cons (d-pts (car x))(d-pts (cdr x))))))

;;;  Make-sized-fred-window assumes that each window corresponds to a file - here, we must
;;;   make a new version which checks for streams
;;;

(defun make-sized-fred-file-or-stream-window (file-or-stream position size &optional window)
    (let* ((w (cond ((typep file-or-stream 'ccl::buffer-stream)
                                  ;;(set-window-title window (concatenate 'string "New-> "
                                   ;;                                                                 (window-title window)))
                                  window)
                                (T (ccl::pathname-to-window file-or-stream)))))
        (cond ((typep file-or-stream 'ccl::buffer-stream))
                  (T (unless  window 
                           (setq w (make-instance 'fred-window :filename  file-or-stream :window-show nil))
                           ;;(set-window-title w (concatenate 'string "Old -> " (window-title w)))
                           )))
        (set-view-position w position)
        (set-view-size w size)
        (window-select w)
        w))

;;; The following function takes a stream and a file, and otherwise provides essentially the 
;;;   same functionality as merge-files in merge-files.lisp
;;;

(defun merge-stream-and-file (stream file difs? finish-action
                                                                   &key (compare-only-p nil) 
                                                                   (projector-revision-p nil))
    (let* ((front-window (front-window :class 'fred-window)))
        (multiple-value-bind (difs1 difs2)
                                              (get-stream-and-file-difs stream file difs? front-window)
             (if difs1
                 (let* ((small-screen-p (< *screen-width* 500))  ;; too arbitrary, should be user-config'd
                             (control-panel-size (make-point 359 127))
                             (layout-list (layout-windows 
                                                    (make-point *screen-width* *screen-height*)
                                                    (cond (small-screen-p
                                                                `(T T ,control-panel-size))   ; stacked vertical layout
                                                              (T `((T T) ,control-panel-size))))) ;; side-by-side for files
                             (win1-data (cond (small-screen-p (car layout-list))
                                                           (T (caar layout-list))))
                             (win2-data (cond (small-screen-p (cadr layout-list))
                                                           (T (cadar layout-list))))
                             (win1 (make-sized-fred-file-or-stream-window stream
                                                                                                                 (car win1-data)
                                                                                                                 (cdr win1-data)
                                                                                                                 front-window))
                             (win2 (make-sized-fred-file-or-stream-window file
                                                                                                                 (car win2-data)
                                                                                                                 (cdr win2-data))))
                     ;;(print "difs1, 2") (print difs1) (print difs2)
                     (labels ((markify (buffer dif-list last-mark last-line-count)
                                                    (let* ((cell (car dif-list))
                                                                (line-count (car cell))
                                                                (the-rest (cdr dif-list))
                                                                (mark (make-mark buffer
                                                                                                  (buffer-line-start 
                                                                                                    buffer
                                                                                                    last-mark
                                                                                                    (- line-count last-line-count)))))
                                                        ;;(format T "buffer dif last-mark last-line = ~A ~A ~A ~A~%"
                                                        ;;              buffer dif-list last-mark last-line-count)
                                                        ;;(format T "cell line-count the-rest mark = ~A ~A ~A ~A~%"
                                                        ;;             cell line-count the-rest mark )
                                                        (setf (car cell) mark)
                                                        (when the-rest
                                                             (markify buffer the-rest mark line-count)))))
                         (when difs1
                              (markify (fred-buffer win1) difs1 0 0)
                              (markify (fred-buffer win2) difs2 0 0))
                         (cond (compare-only-p
                                     (goto-next-compare-difference
                                       (make-instance (cond (compare-only-p 'compare-central)
                                                                            (projector-revision-p 'projector-merge-central)
                                                                            (T 'merge-central))
                                                                  :window-title (slot-value front-window 'ccl::object-name)
                                                                  :window1 win1
                                                                  :window2 win2
                                                                  :view-position (caar (last layout-list))
                                                                  :difs1 difs1
                                                                  :difs2 difs2
                                                                  :remaining-difs1 difs1
                                                                  :remaining-difs2 difs2
                                                                  :finish-action finish-action)))
                                   (T (goto-next-merge-difference
                                          (make-instance (cond (compare-only-p 'compare-central)
                                                                               (projector-revision-p 'projector-merge-central)
                                                                               (T 'merge-central))
                                                                     :window-title (slot-value front-window 'ccl::object-name)
                                                                     :window1 win1
                                                                     :window2 win2
                                                                     :difs1 difs1
                                                                     :difs2 difs2
                                                                     :remaining-difs1 difs1
                                                                     :remaining-difs2 difs2
                                                                     :finish-action finish-action))))))
                 (progn (message-dialog "The files match.")
                              (when finish-action
                                   (funcall finish-action))
                              )))))

;;; The next function takes a stream and a file (suitable for with-open-file)
;;;   and performs a compare upon them. Note two optional arguments -
;;;   if the difs-window is not specified, the returned values are two lists. The
;;;   input-window parameter is used to derive the name for purposes of output.
;;;
;;;  This function is a modified version of the function get-file-difs from compare.lisp
;;;
                           
(defun get-stream-and-file-difs (stream file &optional difs-window? input-window)
    (let ((buffer-stream (if difs-window? 
                                            (make-buffer-stream difs-window?))))
        (with-open-file (s2 file)
             (let ((output (make-instance 'difference-output-stream
                                                               :window difs-window?)))
                 (format t "~&Finding Differences")
                 (if buffer-stream (format buffer-stream "~&Finding Differences~%"))
                 (print-legend output 
                                         (if input-window 
                                             (slot-value input-window 'ccl::object-name)
                                             stream)
                                         file)
                 (if (eql (with-cursor *watch-cursor*
                                   (compare stream s2 output)) 0) ; this is how the routine says no diffs!
                     (and buffer-stream 
                              (progn (format buffer-stream "~%No differences! ~%")
                                           T)
                              (window-close difs-window?)))
                 (values (nreverse (first-file-difs output))
                               (nreverse (second-file-difs output)))))))

;;; (compare-active-window-and-file) ;;;test code

(defun merge-active-lisp-window-and-file ()
    (let* ((front-window (front-window :class 'fred-window))
                (buffer-stream (make-buffer-stream front-window))
                (file (choose-file-dialog :button-string "Merge" :mac-file-type :text))
                )
        (merge-stream-and-file buffer-stream file nil nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Now let's put the good stuff in a menu - for lispers, the Tools menu
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Now here's a hack - to get the items where we want them (under Search Files), we
;;;   have to remove the last 8 items and then stick them in again
;;;



(defun dissect-and-repair-menu (menu-name item-to-insert-after &rest new-items)
    (declare (special menu-item))
    (let* ((the-menu (find-menu menu-name))
                (menu-items (menu-items the-menu))
                (count 0)
                temp-store)
        (loop (cond ((string-equal (menu-item-title (nth count  menu-items))
                                                      item-to-insert-after)
                              (return count))
                            (T (incf count))))
        (setf temp-store (nthcdr count menu-items))
        (mapc #'(lambda (itm) (remove-menu-items the-menu itm))
                    temp-store)
        (eval `(add-menu-items ,the-menu ,@new-items))
        (mapcar #'(lambda (itm) (add-menu-items the-menu itm))
          temp-store)))

#-:merge-pkg

;;; MCL3.0
(eval-when (eval load)
  (dissect-and-repair-menu 
   "Tools" "Search FilesÉ"
   (MAKE-INSTANCE 'MENU-ITEM
                  :MENU-ITEM-TITLE "Compare/Merge FilesÉ"
                  :MENU-ITEM-ACTION
                  #'(LAMBDA NIL (Merge-ACTIVE-lisp-WINDOW-AND-FILE))))
  (pushnew :merge-pkg *features*))

#|
	Change History (most recent last):
	2	6/4/91	tv 	cleanup for package change
				
	4	6/14/91	tv 	adding changes for projector menu additions, removing CKID resources when copying
	7	6/25/91	tv 	fixing for ralph files
	8	8/23/91	jaj    don't try to effect projector menus in announce-merge-completion fns
      9     8/29/91	alms compare/merge only shows text files
	3	10/18/91	jaj	test checkout, in
	3	4/4/93	Sidney	Use SourceServer package instead of CCL to
				minimize possibility of name collision by users.
				
				Switch to MCL 2.0 trap interface calls instead of
				now undocumented 1.3 calls
	4	4/5/93	sidney	Add qualifier to references to buffer-stream
				class, needed because of change from ccl package
				to sourceserver package
	5	4/5/93	sidney	More stuff that is supposed to be in ccl package
	6	4/26/93	Sidney	Fix broken processing of directory names in merge dialog
				Allow multiple selections of files to merge, so they can be processed in a batch
	7	4/27/93	Sidney	Finish implementation of multiple selection in merge dir dialog
	8	5/12/94	sidney	Fix make-buffer-stream to work right with newer fred code
	9  	 3/18/95	sidney  	force merge window sizes to fit on screen in case preferences are too big
	10 	 3/19/95	sidney  	make load of preferences quiet
|# ;(do not edit past this line!!)
