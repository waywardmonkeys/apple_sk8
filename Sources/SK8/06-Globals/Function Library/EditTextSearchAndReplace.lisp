;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories


;;;

(in-package :sk8dev)

(provide "EDITTEXTSEARCHANDREPLACE")

(require "EDITTEXT" "objects;EditText:EditText")

;-----------------------------------------------------------------------------
; Search-And-Replace
; Search and replace for SK8 EditTexts
; 
;-----------------------------------------------------------------------------

;-----------------------------------------------------------------------------
; Search
;-----------------------------------------------------------------------------

(define-handler search-in-selection-only :hidden (EditText targetString
                                                               &key caseSensitive
                                                               (searchBackward nil)
                                                               (beepIfNotFound t))
                (multiple-value-bind (start end)
                                     (selection-range (editdata me))
                  (sk8::search me targetString
                           :start start :end end
                           :caseSensitive caseSensitive
                           :searchBackward searchBackward
                           :beepIfNotFound beepIfNotFound)))

(define-handler search-backward-case-sensitive :hidden (EditText target-string
                                                               &key 
                                                               start 
                                                               end
                                                               beepIfNotFound
                                                               inSelection)
                (let* ((fred-obj (editdata me))
                       (buf (fred-buffer fred-obj)))
                  (multiple-value-bind (range-start range-end)
                                       (selection-range fred-obj)
                    (unless inSelection
                      (set-selection-range fred-obj range-start range-start)
                      (setf range-end range-start))
                    (let* ((start (or start 0))
                           (end (or end range-end))
                           (string-pos (buffer-string-pos buf
                                                          target-string
                                                          :start start
                                                          :end end
                                                          :from-end t)))
                      (if string-pos
                       (let ((found-string (buffer-substring buf 
                                                             (+ string-pos (length target-string))
                                                             string-pos)))
                         (if (string= target-string found-string)
                           ;; exact match
                           (set-selection-range fred-obj string-pos
                                             (+ string-pos (length target-string)))
                           ;; strings differ in case - try again
                           (search-backward-case-sensitive me target-string
                                                          :start start
                                                          :end string-pos
                                                          :beepIfNotFound beepIfNotFound)))
                       ;; not found
                       (when beepIfNotFound
                          (ed-beep)))))))

(define-handler search-backward-case-insensitive :hidden (EditText target-string
                                                               &key 
                                                               start 
                                                               end
                                                               beepIfNotFound
                                                               inSelection)
                (let* ((fred-obj (editdata me))
                       (buf (fred-buffer fred-obj)))
                  (multiple-value-bind (range-start range-end)
                                       (selection-range fred-obj)
                    (unless inSelection
                      (set-selection-range fred-obj range-start range-start)
                      (setf range-end range-start))
                    (let* ((start (or start 0))
                           (end (or end range-end))
                           (string-pos (buffer-string-pos buf
                                                          target-string
                                                          :start start
                                                          :end end
                                                          :from-end t)))
                      (if string-pos
                        (set-selection-range fred-obj string-pos
                                             (+ string-pos (length target-string)))
                        (when beepIfNotFound
                          (ed-beep)))))))

(define-handler search-forward-case-sensitive :hidden (EditText target-string
                                                               &key 
                                                               start 
                                                               end
                                                               beepIfNotFound
                                                               inSelection)
                (let* ((fred-obj (editdata me))
                       (buf (fred-buffer fred-obj)))
                  (multiple-value-bind (range-start range-end)
                                       (selection-range fred-obj)
                    (unless inSelection
                      (set-selection-range fred-obj range-end range-end)
                      (setf range-start range-end))
                    (let* ((start (or start range-start))
                           (end (or end (buffer-size buf)))
                           (string-pos (buffer-string-pos buf
                                                          target-string
                                                          :start start
                                                          :end end)))
                      (if string-pos
                       (let ((found-string (buffer-substring buf 
                                                             (+ string-pos (length target-string))
                                                             string-pos)))
                         (if (string= target-string found-string)
                           ;; exact match
                           (set-selection-range fred-obj string-pos
                                             (+ string-pos (length target-string)))
                           ;; strings differ in case - try again
                           (search-forward-case-sensitive me target-string
                                                          :start (+ string-pos (length target-string))
                                                          :end end
                                                          :beepIfNotFound beepIfNotFound)))
                       ;; not found
                       (when beepIfNotFound
                          (ed-beep)))))))

(define-handler search-forward-case-insensitive :hidden (EditText target-string
                                                               &key 
                                                               start 
                                                               end
                                                               beepIfNotFound
                                                               inSelection)
                (let* ((fred-obj (editdata me))
                       (buf (fred-buffer fred-obj)))
                  (multiple-value-bind (range-start range-end)
                                       (selection-range fred-obj)
                    (unless inSelection
                      (set-selection-range fred-obj range-end range-end)
                      (setf range-start range-end))
                    (let* ((start (or start range-start))
                           (end (or end (buffer-size buf)))
                           (string-pos (buffer-string-pos buf
                                                          target-string
                                                          :start start
                                                          :end end)))
                      (if string-pos
                        (set-selection-range fred-obj string-pos
                                             (+ string-pos (length target-string)))
                        (when beepIfNotFound
                          (ed-beep)))))))

(define-sk8-function sk8::search nil (me searchString
                                           &key start end caseSensitive
                                           (searchBackward nil)
                                           (beepIfNotFound t)
                                           inSelection
                                           inSelectionOnly)
  (assert (stringp searchString) ()
          "SEARCH-SUBSTRING: the target string argument must be a string: ~S"
          searchString)
  ;; discriminate whether the keywords are legal
  ;; cannot combine inSelectionOnly with any of (start end inSelection)
  (if inSelectionOnly
    ;; inSelectionOnly search
    (if (or start end inSelection)
      (error "SEARCH (of EditText): inSelectionOnly cannot be used ~
              with the keywords (start end inSelection)")
      (search-in-selection-only me searchString 
                                :caseSensitive caseSensitive
                                :searchBackward searchBackward
                                :beepIfNotFound beepIfNotFound))
    ;; other searches
    (if searchBackward
      ;; backward swearch
      (if caseSensitive
        ;; case-sensitive search
        (search-backward-case-sensitive me searchString 
                                        :start start
                                        :end end
                                        :beepIfNotFound beepIfNotFound
                                        :inSelection inSelection)
        ;; case-insensitive search
        (search-backward-case-insensitive me searchString 
                                          :start start
                                          :end end
                                          :beepIfNotFound beepIfNotFound
                                          :inSelection inSelection))
      ;; forward search
      (if caseSensitive
        ;; case-sensitive search
        (search-forward-case-sensitive me searchString 
                                       :start start
                                       :end end
                                       :beepIfNotFound beepIfNotFound
                                       :inSelection inSelection)
        ;; case-insensitive search
        (search-forward-case-insensitive me searchString 
                                         :start start
                                         :end end
                                         :beepIfNotFound beepIfNotFound
                                         :inSelection inSelection)))))


;-----------------------------------------------------------------------------
; Replace
;-----------------------------------------------------------------------------

(define-handler replace-all-occurrences-of-string (EditText searchString replacementString
                                                    &key
                                                    start end caseSensitive
                                                    searchBackward
                                                    (beepIfNotFound t)
                                                    inSelectionOnly)
  (declare (ignore searchBackward))
  (let* ((fred-obj (editdata me))
         (len (length searchString)))
    (multiple-value-bind (selection-start selection-end)
                         (selection-range fred-obj)
      (let* ((start (if inSelectionOnly
                      selection-start
                      (or start 0)))
             (end (if inSelectionOnly
                    selection-end
                    (or end (buffer-size (fred-buffer fred-obj))))))
        (do ((working-start start))
            ((>= working-start end))
          (let ((pos (sk8::search me searchString 
                                :start working-start
                                :end end
                                :caseSensitive caseSensitive
                                :searchBackward nil
                                :beepIfNotFound beepIfNotFound)))
            (if pos
              (progn
                (set-selection-range fred-obj pos (+ pos len))
                ;; range of selected search text
                (multiple-value-bind (range-start range-end)
                                     (selection-range fred-obj)
                  (ed-replace-with-undo fred-obj range-start range-end  replacementString)
                  ;; range of cursor pos after replacement
                  (multiple-value-bind (post-range-start post-range-end)
                                       (selection-range fred-obj)
                    (declare (ignore post-range-start))
                    (setf working-start post-range-end)
                    ;; adjust the end for changes in the length of the text
                    ;; after replace
                    (let* ((change (- post-range-end range-end)))
                      (setf end (+ end change))))))
              ;; search string not found so give up
              (setf working-start (1+ end)))))))))

(define-handler simple-replace-search-string (EditText searchString replacementString
                                                    &key
                                                    start end caseSensitive
                                                    (searchBackward nil)
                                                    (beepIfNotFound t)
                                                    inSelection
                                                    inSelectionOnly)
  (let ((fred-obj (editdata me))
        (pos (sk8::search me searchString 
                        :start start
                        :end end
                        :caseSensitive caseSensitive
                        :searchBackward searchBackward
                        :beepIfNotFound beepIfNotFound
                        :inSelection inSelection
                        :inSelectionOnly inSelectionOnly)))
    (when pos
      (multiple-value-bind (range-start range-end)
                           (selection-range fred-obj)
        (ed-replace-with-undo fred-obj range-start range-end  replacementString)
        (when searchBackward
          (set-selection-range fred-obj range-start range-start))))))

(define-handler replace-search-string (EditText searchString replacementString
                                                    &key
                                                    start end caseSensitive
                                                    (searchBackward nil)
                                                    (beepIfNotFound t)
                                                    inSelection
                                                    inSelectionOnly
                                                    allOccurrences)
  (if allOccurrences
    (replace-all-occurrences-of-string me searchString replacementString
                                       :start start
                                       :end end
                                       :caseSensitive caseSensitive
                                       :searchBackward searchBackward
                                       :beepIfNotFound beepIfNotFound
                                       :inSelectionOnly inSelectionOnly)
    (simple-replace-search-string me searchString replacementString
                                  :start start
                                  :end end
                                  :caseSensitive caseSensitive
                                  :searchBackward searchBackward
                                  :beepIfNotFound beepIfNotFound
                                  :inSelection inSelection
                                  :inSelectionOnly inSelectionOnly)))

(define-sk8-function sk8::replace nil (me replacementString &key
                                             searchString
                                             start end caseSensitive
                                             (searchBackward nil)
                                             (beepIfNotFound t)
                                             inSelection
                                             inSelectionOnly
                                             allOccurrences)
  (let* ((fred-obj (editdata me)))
    (multiple-value-bind (range-start range-end)
                         (selection-range fred-obj)
      (if searchString
        ;; search for occurrences
        (replace-search-string me searchString replacementString
                               :start start
                               :end end
                               :caseSensitive caseSensitive
                               :searchBackward searchBackward
                               :beepIfNotFound beepIfNotFound
                               :inSelection inSelection
                               :inSelectionOnly inSelectionOnly
                               :allOccurrences allOccurrences)
        ;; simply replace the selection
        (progn
          (ed-replace-with-undo fred-obj range-start range-end  replacementString)
          (when searchBackward
            (set-selection-range fred-obj range-start range-end)))))))


#|
	Change History (most recent last):
	1  	12/21/94	me      	Created  EditText search and replace handlers
	2  	10/21/96	Hernan  	search and replace have to be functions!
	3  	 2/27/97	Hernan  	
|# ;(do not edit past this line!!)
