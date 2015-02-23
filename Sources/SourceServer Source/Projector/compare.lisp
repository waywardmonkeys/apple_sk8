;;; compare utility 6/20/90
;;; written by Laura Bagnall Linden of TERC

;;;;;;;
;;; Change history
;;;
;;; 01/22/91 bill Add compare-files-to-buffer, keep-first-version, keep-second-version
;;; Ported to MACL2.0 by alanr Saturday September 22,1990 5:36pm
;;;
;;; 05/23/91 Vrhel.T added compare-files functionality, compare-central, etc.
;;;  
;;;;;;

(in-package :SourceServer)

(defvar *show-matched-text* t
  "True if matching text should be displayed")

;;; This is the data structure used by the comparison algorithm.  A buffered stream
;;; is a list of lines taken from a stream.  The list is used in a first-in, first-out
;;; fashion.  HEAD-PTR points to the first element of the list, and TAIL-PTR points to
;;; the last element of the list, allowing elements to efficiently be added to the
;;; end of the list.  The algorithm alternately tests each element in the buffer against 
;;; all of the elements in the other buffer.  TESTED-PTR points to the last entry in the 
;;; buffer which has been tested.  COUNT is the number of entries which have been tested,
;;; and is always equal to (1+ (- (length head-ptr) (length tested-ptr))).  Once a match
;;; has been found, MATCHED-PTR is used to point to the beginning of the match.  The 
;;; buffered-stream should always have at least one line in the buffer, unless the
;;; underlying stream is empty.

(defstruct (bstream (:print-function print-bstream))
  (head-ptr '())		;pointer to start of queue
  (tested-ptr '())		;pointer to last tested entry in queue
  (tail-ptr '())		;pointer to end of queue
  (matched-ptr '())             ;pointer to start of match
  (count 0)			;number of tested entries in queue
  stream			;stream that the queue is buffering.
  (line-number 0))              ;line-number of first line

(defun make-buffered-stream (&rest args)
  (declare (dynamic-extent args))
  (apply 'make-bstream args))

(defun print-bstream (bstream stream depth)
  (declare (ignore depth))
  (format stream "[~S;~S;~S;~S;~S]"
          (bstream-head-ptr bstream)
          (bstream-tested-ptr bstream)
          (bstream-tail-ptr bstream)
          (bstream-matched-ptr bstream)
          (bstream-count bstream)))

(defun init-buffered-stream (a-line stream)
  (if a-line
    (let ((new-pair (cons a-line nil)))
      (make-buffered-stream :head-ptr new-pair
                            :tested-ptr new-pair
                            :tail-ptr new-pair
                            :count 1
                            :stream stream))
    (make-buffered-stream :stream stream)
    ))

(defun bstream-at-eof? (bstream)
  (empty-queue? bstream))                           

(defun at-end? (bstream)
  (eq (bstream-head-ptr bstream) (bstream-tail-ptr bstream)))

;;; this is not to be used by functions external to the bstream abstraction.
(defun empty-queue? (queue)
  (null (bstream-head-ptr queue)))

(defun first-entry (bstream)
  (if (empty-queue? bstream)
      (error "first-entry called with an empty bstream" bstream) 
      (car (bstream-head-ptr bstream))))

(defun bstream-insert (a-line bstream &aux (new-pair (cons a-line nil)))
  (cond ((empty-queue? bstream) (error "this shouldn't happen"))
        (t (rplacd (bstream-tail-ptr bstream) new-pair)
           (setf (bstream-tail-ptr bstream) new-pair)
           bstream)))

(defun bump-tested-ptr (bstream)
  (if (null (cdr (bstream-tested-ptr bstream))) 
    (error "Can't increment tested pointer past end of buffer"))
  (setf (bstream-tested-ptr bstream) (cdr (bstream-tested-ptr bstream)))
  (incf (bstream-count bstream)))

;;; delete entry from bstream buffer, and if that's the last one, prefetch the next
;;; line from the stream, if there is one.
(defun delete-and-prefetch (bstream)
  (when (eq (bstream-head-ptr bstream)
            (bstream-tail-ptr bstream))
    (buffer-next-line bstream))
  (delete-queue bstream)
  )

;;; this is not to be used by functions external to the bstream abstraction.
(defun delete-queue (queue)
  (cond ((empty-queue? queue)
         (error "Delete called with an empty queue" queue))
        (t (when (and (eq (bstream-head-ptr queue) (bstream-tested-ptr queue))
                      (not (eq (bstream-head-ptr queue) (bstream-tail-ptr queue))))
             (bump-tested-ptr queue))
           (setf (bstream-head-ptr queue) (cdr (bstream-head-ptr queue)))
           (incf (bstream-line-number queue))
           (decf (bstream-count queue))
           queue)))

(defun flush-bstream (bstream output)
  (labels ((flush-queue ()
                        (cond ((empty-queue? bstream)
                               (stream-copy-until-eof (bstream-stream bstream)))
                              (t (insert-line output (first-entry bstream))
                                 (delete-queue bstream)
                                 (flush-queue))))
           (stream-copy-until-eof (stream)
                                  (unless (stream-eofp stream)
                                    (insert-line output (read-line stream))
                                    (stream-copy-until-eof stream))))
    (flush-queue)))


;;; This function is the guts of the program.  It uses the algorithm described in 
;;; MPW to compare two input streams (stream1 and stream2) and outputs the result 
;;; onto output-stream.  The input streams must support the operation READ-LINE.
;;; The output stream must support the operations START-DIFFERENCE-ONE,
;;; START-DIFFERENCE-TWO, START-MATCHED-TEXT, INSERT-LINE, and PRINT-LEGEND.  

;;; Description of algorithm (taken verbatim from MPW Compare facility)

;;; Both files are read and compared line for line.  As soon as a mismatch is found,
;;; the two mismatched lines are stored in two stacks, one for each file.  Lines are 
;;; then read alternately (starting from the next input line in file2) until a match 
;;; is found to put the files back in synchronization.  If such a match is found, 
;;; Compare writes the mismatched lines to standard output.

;;; Files are considered resynchronized when a certain number of lines in the two 
;;; stacks exactly match.  By default, the number of lines, called the grouping factor, 
;;; is defined by the formula (truncate (+ (* 2.0 (log M 10)) 2.0)) where M is the 
;;; number of lines saved in each stack so far.  This definition requires more lines 
;;; to be the same after larger mismatches.

(defun compare (stream1 stream2 output-stream)
  (declare (optimize (speed 3) (safety 0)) (inline iter))
  (let ((bstream1 (init-buffered-stream (read-line stream1 nil nil) stream1))
        (bstream2 (init-buffered-stream (read-line stream2 nil nil) stream2)))
    (labels ((iter (at-eof? diff-count)
                   (cond ((bstream-at-eof? bstream1)
                          (cond ((bstream-at-eof? bstream2) diff-count)
                                (t (unless at-eof?
                                     (start-difference-one output-stream bstream1)
                                     (start-difference-two output-stream bstream2))
                                   (insert-line output-stream (first-entry bstream2))
                                   (delete-and-prefetch bstream2)
                                   (iter t (if at-eof? diff-count (1+ diff-count))))))
                         ((bstream-at-eof? bstream2)
                          (unless at-eof?
                            (start-difference-two output-stream bstream2)
                            (start-difference-one output-stream bstream1))
                          (insert-line output-stream (first-entry bstream1))
                          (delete-and-prefetch bstream1)
                          (iter t (if at-eof? diff-count (1+ diff-count))))
                         ((string= (first-entry bstream1) (first-entry bstream2))
                          (when *show-matched-text*
                            (insert-line output-stream (first-entry bstream1)))
                          (delete-and-prefetch bstream1)
                          (delete-and-prefetch bstream2)
                          (iter nil diff-count))
                         (t (resynchronize bstream1 bstream2 output-stream)
                            (iter nil (1+ diff-count))))))
      (iter nil 0))))

(defun resynchronize (bstream1 bstream2 output)
  (cond ((match bstream1 bstream2)
         (start-difference-one output bstream1)
         (output-differences bstream1 output)
         (start-difference-two output bstream2)
         (output-differences bstream2 output)
         (start-matched-text output)
         (output-matched-text bstream1 bstream2 output)
         )
        (t (start-difference-one output bstream1)
           (flush-bstream bstream1 output)
           (start-difference-two output bstream2)
           (flush-bstream bstream2 output))
        ))

(defun output-differences (bstream output)
  (declare (optimize (speed 3) (safety 0)) (inline loop))
  (labels ((loop (l)
                 (cond ((eq l (bstream-matched-ptr bstream)) bstream)
                       (t (insert-line output (first-entry bstream))
                          (delete-queue bstream)
                          (loop (cdr l))))))
    (loop (bstream-head-ptr bstream))))

(defun output-matched-text (bstream1 bstream2 output)
  (declare (optimize (speed 3) (safety 0)) (inline loop))
  (labels ((loop ()
                 (cond ((at-end? bstream1) (buffer-next-line bstream1))
                       ((at-end? bstream2) (buffer-next-line bstream2))
                       ((string/= (first-entry bstream1) (first-entry bstream2))
                        (error "This shouldn't happen"))
                       (t (when *show-matched-text*
                            (insert-line output (first-entry bstream1)))
                          (delete-queue bstream1)
                          (delete-queue bstream2)
                          (loop)))))
    (loop)))
                       
;;; Given two buffered streams, bstream1 and bstream2, tries to match the first 
;;; untested entry in bstream1 against all the entries in bstream2.  If it succeeds,
;;; then it outputs the entries already tested as differences, otherwise, it repeats
;;; the process with the next line in bstream2.
(defun match (bstream1 bstream2)
  (declare (optimize (speed 3) (safety 0)) (inline loop))
  (let ((first-untested-entry (next-entry (bstream-tested-ptr bstream1) bstream1)))
    (labels ((loop (entry-to-compare count)
                   (cond ((null first-untested-entry) nil)
                         ((zerop count) 
                          (bump-tested-ptr bstream1)
                          (match bstream2 bstream1))
                         ((match-entry first-untested-entry bstream1
                                       entry-to-compare bstream2
                                       (group-factor (bstream-count bstream1)))
                          (setf (bstream-matched-ptr bstream1) first-untested-entry)
                          (setf (bstream-matched-ptr bstream2) entry-to-compare)
                          t)
                         (t (loop (next-entry entry-to-compare bstream2) (1- count))))))
      (loop (bstream-head-ptr bstream2) (bstream-count bstream2))
      )))

(defun group-factor (m) 
  (check-type m (integer 1 *) "a positive integer")
  (truncate (+ (* 2.0 (log M 10)) 2.0)))

(defun match-entry (ptr1 bstream1 ptr2 bstream2 depth)
  (cond ((zerop depth) t)
        ((or (null ptr1) (null ptr2)) nil)
        ((string/= (car ptr1) (car ptr2)) nil)
        (t (match-entry (next-entry ptr1 bstream1)
                        bstream1
                        (next-entry ptr2 bstream2)
                        bstream2
                        (1- depth)))
    ))

(defun next-entry (ptr bstream)
  (if (null (cdr ptr))
    (buffer-next-line bstream)
    (cdr ptr)))

(defun buffer-next-line (bstream)
  (let ((a-line (read-line (bstream-stream bstream) nil nil)))
    (when a-line
      (bstream-insert a-line bstream)
      (bstream-tail-ptr bstream))))


;;;;;;;;;;;;;;;
;;
;; output stream classes

(defclass full-text-output-stream (stream)
  ((stream :accessor stream :initarg :stream :initform *standard-output*)))

(defmethod start-difference-one ((s full-text-output-stream) bstream1)
  (declare (ignore bstream1))
  (format (stream s) "*** Start of difference in first file ***~%"))

(defmethod start-difference-two ((s full-text-output-stream) bstream2)
  (declare (ignore bstream2))
  (format (stream s) "*** Start of difference in second file ***~%"))

(defmethod start-matched-text ((s full-text-output-stream))
  (format (stream s) "*** End of differences ***~%"))

(defmethod insert-line ((s full-text-output-stream) a-line)
  (stream-write-string (stream s) a-line 0 (length a-line))
  (terpri (stream s)))

(defmethod print-legend ((s full-text-output-stream) name1 name2)
  (format (stream s) "Comparison of ~A with ~A~%" name1 name2))

(defmethod stream-close ((s full-text-output-stream))
  (close (stream s)))

(defun compare-files (file1 file2 &optional filename)
  (with-open-file (s1 file1)
    (with-open-file (s2 file2)
      (with-open-stream
        (output (make-instance 'full-text-output-stream
                               :stream (if filename 
                                         (open filename :direction :output
                                               :if-exists :append
                                               :if-does-not-exist :create)
                                         *standard-output*))) 
        (print-legend output file1 file2)
        (format t "~D differences found" (compare s1 s2 output))))))


;;;;;;;;;;
;;
;; difference-output-streams

;;each list of file-difs is an alist of the form (start-pos . line-count)

(defclass difference-output-stream (stream)
  ((first-file-difs :accessor first-file-difs :initform '())
   (second-file-difs :accessor second-file-difs :initform '())
   (current-difs-cell :accessor current-difs-cell :initform '())
   (difs-output-window :accessor difs-output-window :initform nil :initarg :window)
   (cached-legend :accessor cached-legend :initform nil)))

(defmethod start-difference-one ((s difference-output-stream) bstream1)
  (let ((new-cell (cons (bstream-line-number bstream1) 0))
        (window (check-difs-window s)))
    (push new-cell (first-file-difs s))
    (setf (current-difs-cell s) new-cell)
    (when window
      (let ((legend (cached-legend s)))
        (when legend (format window "~a" legend)
              (setf (cached-legend s) nil)))
      (format window "~&~%;;Starting Text from New Version:~%"))))

(defmethod start-difference-two ((s difference-output-stream) bstream2)
  (let ((new-cell (cons (bstream-line-number bstream2) 0))
        (window (check-difs-window s)))
    (push new-cell (second-file-difs s))
    (setf (current-difs-cell s) new-cell)
    (when window
      (format window "~&~%;;Starting Text from Old Version:~%"))))

(defmethod start-matched-text ((s difference-output-stream))
  (format t ".")
  (setf (current-difs-cell s) nil))

(defmethod insert-line ((s difference-output-stream) a-line)
  (let ((dif-holder (current-difs-cell s)))
    (when dif-holder
      (incf (cdr dif-holder)))
    (when (current-difs-cell s)
      (let ((window (check-difs-window s)))
        (when window
          (format window "~&~a" a-line))))))

(defmethod print-legend ((s difference-output-stream) name1 name2)
  (setf (cached-legend s)
        (format nil ";;New Version is ~s~%;;Old Version is ~s~%~%" name1 name2)))

(defmethod check-difs-window ((s difference-output-stream))
  (let ((win (difs-output-window s)))
    (typecase win
      (null nil)
      (window win)
      (t (setf (difs-output-window s) (make-instance *default-editor-class*
                                        :window-title (format nil
                                                              "Differences in ~s" "whoops")
                                        :scratch-p t))))))

(defun get-file-difs (file1 file2 &optional difs-window?)
  (with-open-file (s1 file1)
    (with-open-file (s2 file2)
      (let ((output (make-instance 'difference-output-stream
                      :window difs-window?)))
        (format t "~&Finding Differences")
        (print-legend output file1 file2)
        (compare s1 s2 output)
        (values (nreverse (first-file-difs output))
                (nreverse (second-file-difs output)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; here we add the two-screen compare functionality - however, this also requires the files
;;;   projector-compare-merge.lisp and merge.lisp
;;;   to be present in the environment
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *compare-window-1-position*  #@(2 40))
(defparameter *compare-window-2-position* #@(502 40))
(defparameter *compare-window-1-size* #@(500 500))
(defparameter *compare-window-2-size* #@(500 500))
(defparameter *compare-central-position* #@(340 575))
(defparameter *compare-directory-window-position* *window-default-position*)

(defclass compare-central (dialog)
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
    :view-size #@(359 95)
    :window-type :document
    :view-position *compare-central-position*
    :window-title "compare Central"
    :view-subviews (let ((button-size #@(158 16)))
                     (list (make-dialog-item 'button-dialog-item
                             #@(10 14) button-size "Next Difference"
                             'goto-next-compare-difference
                             :view-nick-name 'next-button)
                           (make-dialog-item 'button-dialog-item
                             #@(10 40) button-size "Previous Difference"
                             'goto-previous-compare-difference
                             :view-nick-name 'previous-button)
                           (make-dialog-item 'button-dialog-item
                             #@(190 27) button-size "Done - Close"
                             'announce-compare-completion
                             :view-nick-name 'complete-button)
                           (make-dialog-item 'button-dialog-item
                             #@(10 66) button-size "Cancel"
                             'cancel-compare
                             :view-nick-name 'cancel-button)
                           (make-dialog-item 'button-dialog-item
                             #@(190 53) button-size "Done - Keep Open"
                             'announce-compare-completion-open
                             :view-nick-name 'complete-open-button)))))

(defmethod announce-compare-completion ((item button-dialog-item))
  (announce-compare-completion (view-window item)))

(defmethod announce-compare-completion-open ((item button-dialog-item))
  (announce-compare-completion-open (view-window item)))

(defmethod announce-compare-completion ((controller compare-central))
  (let* ((finish-action (finish-action controller)))
    (when finish-action
      (funcall finish-action)))
  (update-compare-preference-file controller)
  (window-close (window1 controller))
  (window-close (window2 controller))
  (window-close controller))

(defmethod announce-compare-completion-open ((controller compare-central))
  (let* ((finish-action (finish-action controller)))
    (when finish-action
      (funcall finish-action)))
  (update-compare-preference-file controller)
  ;(window-close (window1 controller))
  (window-close (window2 controller))
  (window-close controller))

(defmethod cancel-compare ((item button-dialog-item))
  (cancel-compare (view-window item)))

(defmethod cancel-compare ((controller compare-central))
  (window-close (window2 controller))
  (window-close controller)
  (cancel))

(defmethod pop-dif1 ((window compare-central))
  (when (null (remaining-difs1 window))
    (setf (remaining-difs1 window)
          (difs1 window)))
  (setf (current-dif1 window)
        (pop (remaining-difs1 window))))

(defmethod pop-dif2 ((window compare-central))
  (when (null (remaining-difs2 window))
    (setf (remaining-difs2 window)
          (difs2 window)))
  (setf (current-dif2 window)
        (pop (remaining-difs2 window))))

(defmethod goto-next-compare-difference ((item button-dialog-item))
  (goto-next-compare-difference (view-window item)))

(defmethod goto-next-compare-difference ((controller compare-central))
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

(defmethod goto-previous-compare-difference ((item button-dialog-item))
  (goto-previous-compare-difference (view-window item)))

(defmethod goto-previous-compare-difference ((controller compare-central))
  (let* ((window1 (window1 controller))
         (window2 (window2 controller))
         (dif1 (unpop-dif1 controller))
         (dif2 (unpop-dif2 controller)))
    (when dif1
      (unless dif2 (error "this shouldn't happen"))
      (scroll-to-dif window1 dif1)
      (scroll-to-dif window2 dif2))))

(defmethod unpop-dif1 ((window compare-central))
  (let* ((all-difs1 (difs1 window))
         (current-pos (position (current-dif1 window)
                                all-difs1)))
    (setf (remaining-difs1 window)
          (nthcdr (1- (if (zerop current-pos)
                        (length all-difs1)
                        current-pos))
                  all-difs1))
    (pop-dif1 window)))

(defmethod unpop-dif2 ((window compare-central))
  (let* ((all-difs2 (difs2 window))
         (current-pos (position (current-dif2 window)
                                all-difs2)))
    (setf (remaining-difs2 window)
          (nthcdr (1- (if (zerop current-pos)
                        (length all-difs2)
                        current-pos))
                  all-difs2))
    (pop-dif2 window)))

(defmethod remove-current-dif ((win compare-central))
  (setf (difs1 win) (delete (current-dif1 win) (difs1 win)))
  (setf (difs2 win) (delete (current-dif2 win) (difs2 win)))
  (if (difs1 win)
    (if (difs2 win)
      (goto-next-compare-difference win)
      (error "this shouldn't happen"))
    (all-difs-compared win)))

(defmethod all-difs-compared ((win compare-central))
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

(defun scroll-to-dif (window dif)
  (let* ((buffer (fred-buffer window))
         (start (car dif)))
    (case (window-layer window)
      ((1 2) ())
      (t (window-select window)))
    (set-selection-range window
                         start
                         (buffer-line-start buffer start (cdr dif)))
    (ccl::window-show-selection window)))

;;;;;;;;;;;;;;;
;;
;; stuff for the preferences folder
;; test difference with this string

(defparameter *compare-preferences-filename* "compare-files prefs")

(defun find-preferences-folder ()
    (or (find-folder "pref") "ccl:"))

(defun update-compare-preference-file (controller)
  (let* ((pathname (merge-pathnames
                    (find-preferences-folder) *compare-preferences-filename*)))
    (with-open-file (file pathname :direction :output
                                   :if-exists :supersede
                                   :if-does-not-exist :create)
      (let ((window1 (window1 controller))
            (window2 (window2 controller)))
        (format file
                "(setq *compare-window-1-position* ~s
      *compare-window-2-position* ~s
      *compare-window-1-size* ~s
      *compare-window-2-size* ~s
      *compare-central-position* ~s
      *compare-directory-window-position* ~s)"
                (setq *compare-window-1-position* (view-position window1))
                (setq *compare-window-2-position* (view-position window2))
                (setq *compare-window-1-size* (view-size window1))
                (setq *compare-window-2-size* (view-size window2))
                (setq *compare-central-position* (view-position controller))
                (let ((win (front-window :class 'compare-directory-dialog)))
                  (if win
                    (view-position win)
                    *compare-central-position*)))))))

(defun load-compare-preferences ()
  (let* ((pathname (merge-pathnames
                    (find-preferences-folder) *compare-preferences-filename*)))
    (when (probe-file pathname)
      (load pathname :verbose nil))))

(load-compare-preferences)



(defclass compare-directory-dialog (dialog)
  ((from-dir :accessor from-dir :initarg :from-dir)
   (to-dir :accessor to-dir :initarg :to-dir))
  (:default-initargs
    :window-type :document-with-grow
    :window-title "Files To compare"
    :view-position *compare-directory-window-position*
    :view-size #@(509 284)
    :view-subviews (list
                    (make-dialog-item 'static-text-dialog-item
                      #@(10 6) #@(67 16) "Merging:" nil)
                    (make-dialog-item 'static-text-dialog-item
                      #@(10 34) #@(67 16) "Into:" nil)
                    (make-dialog-item 'static-text-dialog-item
                      #@(10 62) #@(67 16) "Archive:" nil)
                    (make-dialog-item 'editable-text-dialog-item
                      #@(80 6) #@(331 17) "" nil
                      :view-nick-name 'to-dir-text)
                    (make-dialog-item 'editable-text-dialog-item
                      #@(80 34) #@(331 17) "" nil
                      :view-nick-name 'from-dir-text)
                    (make-dialog-item 'editable-text-dialog-item
                      #@(80 62) #@(331 17) "" nil
                      :view-nick-name 'archive-dir-text)
                    (make-dialog-item 'check-box-dialog-item
                      #@(229 94) #@(168 16) "Generate Difs Window" nil
                      :view-nick-name 'difs-window?)
                    (make-dialog-item 'button-dialog-item
                      #@(16 94) #@(90 16) "List Files"
                      'generate-file-list
                      :default-button t)
                    (make-dialog-item 'button-dialog-item
                      #@(125 94) #@(90 16) "compare File"
                      'compare-selected-files
                      :view-nick-name 'compare-button
                      :dialog-item-enabled-p nil)
                    (make-dialog-item 'sequence-dialog-item
                      #@(0 120) #@(509 150) ""
                      #'(lambda (item)
                          (when (double-click-p)
                            (compare-selected-files item)))
                      :table-hscrollp nil
                      :table-vscrollp t
                      :table-sequence ()
                      :view-font *fred-default-font-spec*
                      :view-nick-name 'the-table))))

(defmethod initialize-instance ((w compare-directory-dialog) &rest rest)
  (declare (ignore rest))
  (call-next-method)
  (set-dialog-item-text (view-named 'from-dir-text w)
                        (from-dir w))
  (set-dialog-item-text (view-named 'to-dir-text w)
                        (to-dir w)))

(defmethod generate-difs? ((w compare-directory-dialog))
  (check-box-checked-p (view-named 'difs-window? w)))

(defmethod archive-dir ((w compare-directory-dialog))
  (let ((string (dialog-item-text (view-named 'archive-dir-text w))))
    (if (zerop (length string))
      nil
      string)))

(defmethod set-view-size ((w compare-directory-dialog) h &optional v)
  (call-next-method)
  (let* ((size (make-point h v)))
    (set-view-size (view-named 'the-table w)
                   (subtract-points size #@(0 106)))))

(defmethod compare-selected-files ((d dialog-item))
  (compare-selected-files (view-window d)))

(defmethod compare-selected-files ((w compare-directory-dialog))
  (let* ((table (view-named 'the-table w))
         (cell (ccl::first-selected-cell table))
         (difs? (generate-difs? w)))
    (if cell
      (let* ((short-file (cell-contents table cell))
             (from-file (merge-pathnames short-file (from-dir w)))
             (to-file (merge-pathnames short-file (to-dir w))))
        (eval-enqueue `(merge-files ,from-file ,to-file
                                    ,difs?
                                    ,#'(lambda ()
                                        (finished-merging-file w short-file)))))
      (ed-beep))))

(defmethod finished-merging-file ((w compare-directory-dialog) file)
    (let* ((from-dir (from-dir w))
                (to-dir (archive-dir w)))
        (when to-dir
             (let* ((full-file (pathname (concatenate 'string (mac-namestring to-dir) file)))
                         (to-inner-dir (make-pathname :directory
                                                                               (pathname-directory full-file))))
                 (unless (probe-file to-inner-dir) (create-file to-inner-dir))
                 (rename-file  (pathname (concatenate 'string (mac-namestring from-dir) file)) full-file
                                        :if-exists :dialog))))
    (let* ((the-table (view-named 'the-table w)))
        (set-table-sequence the-table
                                           (delete file (table-sequence the-table)))))

(defmethod generate-file-list ((item button-dialog-item))
  (generate-file-list (view-window item)))

(defmethod generate-file-list ((w compare-directory-dialog))
  (with-cursor *watch-cursor*
    (let* ((from-dir (mac-namestring (dialog-item-text (view-named 'from-dir-text w))))
                (len-from-dir (length from-dir))
                (file-list (mapcar #'(lambda (pathname)
                                                       (subseq (mac-namestring pathname) len-from-dir))
                                                (directory (merge-pathnames ":**:*.*" from-dir))))
                (compare-button (view-named 'compare-button w)))
        (setf (from-dir w) (dialog-item-text (view-named 'from-dir-text w))
                 (to-dir w) (dialog-item-text (view-named 'to-dir-text w)))
        (set-table-sequence (view-named 'the-table w) file-list)
        (dialog-item-enable compare-button)
        (set-default-button w compare-button))))

(defun compare-directories (from-dir to-dir)
  (setq from-dir (mac-namestring from-dir)
        to-dir (mac-namestring to-dir))
  (make-instance 'compare-directory-dialog
    :from-dir from-dir
    :to-dir to-dir))

#|
	Change History (most recent last):
	3	6/6/91	   	testing cancel checkout active
				
	4	6/7/91	   	changing line to some other symbol
	2	11/6/91	alms	testing projector fix.
	4	4/14/92	ows	projector test
	4	4/14/92	ows	test projector
	3	4/4/93	Sidney	Use SourceServer package instead of CCL to
				minimize possibility of name collision by users.
				
				Switch to MCL 2.0 trap interface calls instead of
				now undocumented 1.3 calls
	4	4/26/93	Sidney	Fixed broken processing of directory names in compare dialog
	5  	 3/19/95	sidney  	make load of preferences quiet
	2  	12/18/95	sidney  	workaround a bug in read-line in a way that will still work when the bug is fixed
	3  	10/10/96	Brian   	
	4  	10/21/96	sidney  	enable update of compare preferences
|# ;(do not edit past this line!!)
