;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories


;;;

(in-package :SK8Development)

;;; TextChunk Protocol stuff for Streams.


(define-handler mapCharacterChunks (TextStream rangeStart rangeEnd functionOrName)
  (with-slots (str) me
    (let* ((length (file-length str))
           (end (if rangeEnd (min rangeEnd length) length)))
      (do ((i (or rangeStart 0) (1+ i)))
          ((>= i end))
        (funcall functionOrName i (1+ i))))))


(define-handler mapWordChunks (TextStream rangeStart rangeEnd functionOrName)
  (with-slots (str) me
    (let ((i (or rangeStart 0))
          starti this-char)
      (ccl::stream-position str i)
      (setq this-char (read-char str nil nil))
      (flet ((advance-stream ()
               (incf i)
               (setq this-char (read-char str nil nil)))
             (end? ()
               (or (null this-char) (when rangeEnd (>= i rangeEnd)))))
        (loop
          ;; Find the start of the word
          (loop
            (when (end?) (return-from mapWordChunks))
            (when (not (memq this-char *WORD-DELIMITERS*)) (return))
            (advance-stream))
          (setq starti i)
          ;; Find the end of the word and call the functionOrName
          (loop
            (when (end?)
              (funcall functionOrName starti i)
              (return-from mapWordChunks))
            (when (memq this-char *WORD-DELIMITERS*)
              (funcall functionOrName starti i)
              (advance-stream)
              (return))
            (advance-stream)))))))


(define-handler mapLineChunks (TextStream rangeStart rangeEnd functionOrName)
  (with-slots (str) me
    (let* ((i (or rangeStart 0))
           (starti i)
           this-char)
      (ccl::stream-position str i)
      (setq this-char (read-char str nil nil))
      (flet ((advance-stream ()
               (incf i)
               (setq this-char (read-char str nil nil)))
             (end? ()
               (or (null this-char) (when rangeEnd (>= i rangeEnd)))))
        (loop
          (loop
            (when (end?) 
              (funcall functionOrName starti rangeEnd)
              (return-from mapLineChunks))
            (when (eq this-char #\newline)
              (funcall functionOrName starti i)
              (advance-stream)
              (return))
            (advance-stream)))))))

;;; For now the same as paragraphs.
(define-handler mapParagraphChunks (TextStream rangeStart rangeEnd functionOrName)
  (mapLineChunks me rangeStart rangeEnd functionOrName))


;;; Reverse stream mapping functions are slow as all hell.
(define-handler reverseMapCharacterChunks (TextStream rangeStart rangeEnd functionOrName)
  (unless rangeStart (setq rangeStart 0))
  (with-slots (str) me
    (let ((i (or rangeEnd (1- (file-length str)))))
      (loop 
        (when (< i rangeStart) (return))
        (ccl::stream-position str i)
        (funcall functionOrName (1+ i) (+ i 2))
        (decf i)))))

(define-handler reverseMapWordChunks (TextStream rangeStart rangeEnd functionOrName)
  (unless rangeStart (setq rangeStart 0))
  (with-slots (str) me
    (let ((i (or rangeEnd (file-length str)))
          endi this-char)
      ;; gets previous character, leaves i at it's position, leaves last-char at it's value
      (flet ((get-char ()
               (decf i)
               (unless (< i rangeStart)
                 (ccl::stream-position str i)
                 (setq this-char (read-char str nil nil)))
               (print (list i this-char))))
        (loop
          ;; Find the end of the word
          (loop
            (get-char)
            (when (< i rangeStart) (return-from reverseMapWordChunks))
            (unless (memq this-char *WORD-DELIMITERS*) (return)))
          (setq endi (1+ i))
          ;; Find the start of the word and call the functionOrName
          (loop
            (when (< i rangeStart)
              (funcall functionOrName rangeStart endi)
              (return-from reverseMapWordChunks))
            (when (memq this-char *WORD-DELIMITERS*)
              (print (list (1+ i) endi))
              (funcall functionOrName (1+ i) endi)
              (return))
            (get-char)))))))


(define-handler reverseMapLineChunks (TextStream rangeStart rangeEnd functionOrName)
  (unless rangeStart (setq rangeStart 0))
  (with-slots (str) me
    (let* ((endi (or rangeEnd (file-length str)))
           (i (1- endi))
           this-char)
      (flet ((get-char ()
               (unless (< i rangeStart)
                 (ccl::stream-position str i)
                 (setq this-char (read-char str nil nil))
                 (decf i))))
        (loop
          (loop
            (when (< i rangeStart)
              (funcall functionOrName rangeStart endi)
              (return-from reverseMapLineChunks))
            (when (eq this-char #\newline)
              (funcall functionOrName (1+ i) endi)
              (setq endi i)
              (get-char)
              (return)))
          (get-char))))))


(define-handler reverseMapParagraphChunks (TextStream rangeStart rangeEnd functionOrName)
  (reverseMapLineChunks me rangeStart rangeEnd functionOrName))


(define-handler currentCharacter (TextStream state)
  (with-slots (str) me
    (ccl::stream-position str (car state))
    (read-char str nil nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;   ByteStreams, man.


(define-handler nthitem (bytestream n)
  (check-type n integer)
  (with-slots (str) me
    (ccl::stream-position str (1- n))
    (read-byte str nil nil)))

(define-handler mapByteChunks (ByteStream rangeStart rangeEnd functionOrName)
  (with-slots (str) me
    (let* ((length (file-length str))
           (end (if rangeEnd (min rangeEnd length) length)))
      (do ((i (or rangeStart 0) (1+ i)))
          ((>= i end))
        (funcall functionOrName i (1+ i))))))

(define-handler reverseMapByteChunks (ByteStream rangeStart rangeEnd functionOrName)
  (unless rangeStart (setq rangeStart 0))
  (with-slots (str) me
    (let ((i (or rangeEnd (1- (file-length str)))))
      (loop 
        (when (< i rangeStart) (return))
        (ccl::stream-position str i)
        (funcall functionOrName i (1+ i))
        (decf i)))))

(define-handler initialState (ByteStream)
  (initialStateTyped me SK8::Byte))

(define-handler nextState (ByteStream state)
  (with-slots (str) me
    (incf (car state))
    (unless (>= (car state) (file-length str))
      (incf (cdr state))
      state)))          


(define-handler initialStateTyped (ByteStream type)
  (flet ((grabber (s e)
           (return-from initialStateTyped (cons s e))))
    (declare (dynamic-extent #'grabber))
    (funcall (forwardMapper type) me nil nil #'grabber)))


(define-handler currentitem (bytestream state)
  (with-slots (str) me
    (ccl::stream-position str (car state))
    (read-byte str nil nil)))

(define-handler nthPreviousState (ByteStream n fromState)
  (nthPreviousStateTyped me SK8::Byte n fromState))

(define-handler nthPreviousStateTyped (ByteStream type n fromState)
  (flet ((mapper (s e)
           (when (not (plusp (decf n)))
             (return-from nthPreviousStateTyped (cons s e)))))
    (declare (dynamic-extent #'mapper))
    (funcall (reverseMapper type) 
             me nil (1- (if fromState (car fromState) (file-length (str me)))) #'mapper)))

#|
	Change History (most recent last):
	1  	 4/22/96	Brian   	
	2  	 2/27/97	Hernan  	
|# ;(do not edit past this line!!)
