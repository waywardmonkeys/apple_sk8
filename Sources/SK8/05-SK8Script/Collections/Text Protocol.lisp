;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories


;;;

(in-package :SK8Development)

(SK8-declare-syms :SK8 :public ; Updated  5-13-96   2:32 pm
                  SK8::BASETEXT SK8::INDIRECTCHARACTERVECTOR SK8::INDIRECTLINEVECTOR SK8::INDIRECTPARAGRAPHVECTOR
                  SK8::INDIRECTTEXTVECTOR SK8::INDIRECTWORDVECTOR SK8::LINE SK8::PARAGRAPH SK8::TEXT
                  SK8::WORD)

(new Text :objectname "Word" :project SK8)
(new Text :objectname "Line" :project SK8)
(new Text :objectname "Paragraph" :project SK8)


#|
TEXT = {STRUCTUREMUTABLEORDEREDCOLLECTION + Discrete + (text extensions)}

ABOUT TEXT

SK8's Text object relies on proxy objects to provide access to the text
data. A Text object in itself cannot provide access to its text data;
instead the user must request a proxy of one of the following types:

  - Characters
  - Words
  - Paragraphs
  - Lines

Each proxy type provides an implementation of the Text protocol that
manipulates the text data in terms of the appropriate abstraction. The
Text protocol as implemented by Text itself simply warns the user to
use these proxy objects.

Text itself extends the protocol with N handlers that facilitate use
of these proxies. The handlers are:

  - characters (textObj)
  - words (textObj)
  - paragraphs (textObj)
  - lines (textObj)
  - listInterfaces (textObj)
  - isInterface (textObj, interfaceSelector)

The proxy objects are essentially 'private' objects, no part of their
API is published except for the Text protocol. Even their names are not 
considered public, though there is no way at present to prevent a
user from learning them.

This extension of the Text protocol is called the Text Interface Protocol,
and may be thought of as an early sketch of a more general facility for
providing multiple interfaces to SK8 objects in the future.

|#


#| -------------------
   TEXT Object
   -------------------
|#

#| collect (collectionType, charSequence) => [Text]
 ----------------------------------------------------------
   Required Handler

   Creates a Text object, initialized with the characters
   provided.
|#
(defmethod collect ((me (eql Text)) &rest objects)
  (coerce objects 'string))

;;_______________________________________________________________________________
;-----------------------------------------------------------------------------
; OBJECT DEFINITIONS
;-----------------------------------------------------------------------------

;; IndirectTextVector
;---------------------------------------------------------
(new Collection :objectname "IndirectTextVector" 
     :project sk8)
(addProperty IndirectTextVector 'baseText :private t)

(define-handler text (IndirectTextVector)
  (cond
   ((stringp (baseText me))
    (baseText me))
   (t "")))

(defmethod text ((ch cl::character) &key start end)
  (declare (ignore start end))
  ch)

(define-handler text (string &key start end)
  (if (or start end)
    (subseq me start end)
    me)
  )



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Constants used by the scanners...
;;;

(defconstant *word-delimiters* '(#\Space #\Return #\Tab #\. #\, #\? #\! #\: #\;))
(defconstant *number-punctuations* '(#\. #\,))
(defconstant *paragraph-delimiter* #\Return)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  The TextChunk mapper handlers...
;;;
;;;  Notes: 
;;;
;;;   - The indices (rangeStart and rangeEnd) are 0-based end-exclusive (e.g. the 1st word of "Hi there" is represented by 0,2).
;;;   - The Collection protocol requires that these mappers return false.
;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  These TextChunk mapper handlers for Text make the default interpretation of Line be the same as Paragraph...
;;;

(define-handler mapLineChunks (Text rangeStart rangeEnd functionOrName)
  (mapParagraphChunks me rangeStart rangeEnd functionOrName))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  TextChunk mapper handlers for Strings...
;;;

(define-handler mapCharacterChunks (String rangeStart rangeEnd functionOrName)
  (declare (integer rangeStart rangeEnd))
  (unless rangeEnd (setq rangeEnd (sk8::length me)))
  (let ((i (or rangeStart 0)))
    (declare (integer i))
    (loop
      (when (sk8::>= i rangeEnd) (return))
      (funcall functionOrName i (1+ i))
      (incf i))))


(define-handler mapWordChunks (String rangeStart rangeEnd functionOrName)
  (declare (string me) (integer rangeStart rangeEnd))
  (unless rangeEnd (setq rangeEnd (sk8::length me)))
  (let ((i (or rangeStart 0))
        starti ch)
    (declare (integer i starti))
    (tagbody
      :TOP
      ;; Find the start of the word
      (loop
        (when (sk8::>= i rangeEnd) (return-from mapWordChunks))
        (when (or (not (memq (setq ch (aref me i)) *WORD-DELIMITERS*))
                  (and (memq ch *NUMBER-PUNCTUATIONS*)
                       (not (eql (1+ i) rangeEnd))
                       (digit-char-p (aref me (1+ i)))))
          (return))
        (incf i))
      :GOT-START
      (setq starti i)
      ;; Find the end of the word and call the functionOrName
      (loop
        (when (sk8::>= i rangeEnd)
          (funcall functionOrName starti rangeEnd)
          (return-from mapWordChunks))
        (when (and (memq (setq ch (aref me i)) *WORD-DELIMITERS*)
                   (not (and (memq ch *NUMBER-PUNCTUATIONS*)
                             (not (eql (1+ i) rangeEnd))
                             (digit-char-p (aref me (1+ i))))))
          (funcall functionOrName starti i)
          (return (incf i)))
        (incf i))
      (go :TOP))))


(define-handler mapParagraphChunks (String rangeStart rangeEnd functionOrName)
  (declare (string me) (integer rangeStart rangeEnd))
  (unless rangeEnd (setq rangeEnd (sk8::length me)))
  (let* ((i (or rangeStart 0))
         (starti i))
    (declare (integer i starti))
    (loop
      (loop
        (when (sk8::>= i rangeEnd)
          (funcall functionOrName starti rangeEnd)
          (return-from mapParagraphChunks))
        (when (eq (aref me i) *PARAGRAPH-DELIMITER*)
          (funcall functionOrName starti i)
          (return (setq starti (incf i))))
        (incf i)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; THIS DOESN'T ASSUME FILL-POINTERS, *BUT* IT'S MEMORY-INNEFICIENT SINCE ADJUST-ARRAY CREATES A NEW COPY EACH TIME!!! (TALK TO CAMBRIDGE?):
(defun shiftVectorEnd (vect from to)
  ;*** Optimize to work directly with the internal vector (returned by CCL::array-data-and-offset)
  (let ((len (sk8::length vect))
        (delta (- to from))
        newlen sourcei desti)
    (when (eq 0 delta) (return-from shiftVectorEnd vect))
    (cond
     ((sk8::> to from)
      (unless (eql (setq newlen (+ len delta)) (array-dimension vect 0))
        (setq vect (adjust-array vect newlen)))
      (when (array-has-fill-pointer-p vect)
        (setf (fill-pointer vect) newlen))
      (unless (eql from len)
        (setq desti (1- newlen)
              sourcei (- desti delta))
        (loop
          (setf (aref vect desti) (aref vect sourcei))
          (when (eql sourcei from) (return))
          (decf sourcei) (decf desti)))
      vect)
     (t
      (unless (eql from len)
        (setq sourcei from
              desti to)
        (loop
          (setf (aref vect desti) (aref vect sourcei))
          (when (eql (incf sourcei) len) (return))
          (incf desti)))
      (setq vect (adjust-array vect (+ len delta)))
      (when (array-has-fill-pointer-p vect)
        (setf (fill-pointer vect) newlen))
      vect))))

(defun updateTextDependentStates (subStart subEnd postShift dependentStates)
  (declare (fixnum subStart subEnd postShift))
  (let (depStatePair depState)
    (declare (fixnum depState))
    (loop
      (unless dependentStates (return))
      (when (setq depStatePair (car dependentStates))
        (setq depState (car depStatePair))
        (locally (declare (cons depStatePair))
          (cond ((sk8::>= depState subEnd)  (setf (car depStatePair) (+ depState postShift)))
                ((sk8::> depState subStart) (setf (car depStatePair) subStart)))
          (setq depState (cdr depStatePair))
          (cond ((sk8::>= depState subEnd)  (setf (cdr depStatePair) (+ depState postShift)))
                ((sk8::> depState subStart) (setf (cdr depStatePair) subStart)))))
      (setq dependentStates (cdr (the list dependentStates))))))

(defun removeStringSubsequence (str subStart subEnd dependentStates)
  (when dependentStates (updateTextDependentStates subStart subEnd (- subStart subEnd) dependentStates))
  (shiftVectorEnd str subEnd subStart))

(define-handler mapCharacters (String fromIndex toIndex functionOrName)
  (multiple-value-bind (sourceVector i) (CCL::array-data-and-offset me)
    (declare (fixnum i))
    (setq toIndex (the fixnum (+ i (the fixnum (if toIndex
                                                 (require-type toIndex 'fixnum)
                                                 (sk8::length me))))))
    (when fromIndex (incf i (the fixnum (require-type fromIndex 'fixnum))))
    (locally (declare (fixnum toIndex))
      (loop
        (when (eql i toIndex) (return))
        (funcall functionOrName (uvref (the simple-string sourceVector) i))
        (incf i)))))

(defun replaceStringSubsequence (str subStart subEnd newItems dependentStates)
  (let* ((char? (characterp newItems))
         (numNew (if char? 1 (sk8::length newItems))))
    (when dependentStates
      (updateTextDependentStates subStart subEnd (- numNew (- subEnd subStart)) dependentStates))
    (setq str (shiftVectorEnd str subEnd (+ subStart numNew)))
    (unless (eql 0 numNew)
      (if char?
        (setf (aref str subStart) newItems)
        ;*** Optimize to use internal vector (returned by CCL::array-data-and-offset)
        (flet ((inserter (ch)
                 (setf (aref str subStart) ch)  ; *** should xfer style information if possible!
                 (incf subStart)))
          (declare (dynamic-extent #'inserter))
          (mapCharacters newItems nil nil #'inserter))))
    str))




;-----------------------------------------------------------------------------
; Visitation handlers
;-----------------------------------------------------------------------------

(define-handler initialVisitState (IndirectTextVector
                                      &rest initargs)
  (declare (ignore initargs))
  (flet ((grabber (s e)
           (return-from initialVisitState (cons s e))))
    (declare (dynamic-extent #'grabber))
    (funcall (mapper me) (baseText me) nil nil #'grabber))
  )

(define-handler succeedingVisitState (IndirectTextVector 
                                          stateObj)
  (when stateObj
    (flet ((grabber (s e)
             (setf (car stateObj) s (cdr (the cons stateObj)) e)
             (return-from succeedingVisitState stateObj)))
      (declare (dynamic-extent #'grabber))
      (funcall (mapper me) (baseText me) (cdr stateObj) nil #'grabber)))
  (when stateObj
    (cons (car stateObj) (cdr stateObj))))


(define-handler elementAtVisitState (IndirectTextVector
                                         stateObj)
  (subseq (baseText me) (car stateObj) (cdr stateobj)))

(define-handler isFinalVisitState (IndirectTextVector stateObj)
  (= (length (baseText me)) (cdr stateobj)))

(define-handler setElementAtVisitState (IndirectTextVector
                                            stateObj newVal)
  (setf (baseText me) 
        (replaceStringSubsequence (baseText me) (car stateObj) (cdr (the cons stateObj)) newVal nil))
  me)

(define-handler removeVisitState (IndirectTextVector stateObj)
  (when stateObj
    (setf (baseText me) 
          (removeStringSubsequence (baseText me) 
                                   (car stateObj)
                                   (cdr (the cons stateObj)) 
                                   (list StateObj))))
  me)

(define-handler insertAtVisitState (IndirectTextVector 
                                       stateObj
                                       strOrChar
                                       &key (after nil))
  (when stateObj
    (let ((index (if after
                   (or (cdr stateObj) 0)
                   (or (car stateObj) (length me)))))
      (setf (baseText me) 
            (replaceStringSubsequence (baseText me) 
                                      index
                                      index
                                      strOrChar
                                      (list StateObj)))))
  me)



;;_____________________________________________________________________________
;;_____________________________________________________________________________

(new IndirectTextVector :objectname "IndirectCharacterVector" 
     :project sk8)
(define-handler mapper (IndirectCharacterVector)
  'mapCharacterChunks)
(define-handler elementAtVisitState (IndirectCharacterVector
                                         stateObj)
  (character (subseq (baseText me) (car stateObj) (1+ (car stateObj))))
  )


(new IndirectTextVector :objectname "IndirectWordVector" 
     :project sk8)
(define-handler mapper (IndirectWordVector)
  'mapWordChunks)

(new IndirectTextVector :objectname "IndirectParagraphVector" 
     :project sk8)
(define-handler mapper (IndirectParagraphVector)
  'mapParagraphChunks)

(new IndirectTextVector :objectname "IndirectLineVector" 
     :project sk8)
(define-handler mapper (IndirectLineVector)
  'mapLineChunks)

;;_____________________________________________________________________________
;;_____________________________________________________________________________
; Utility functions

(defmacro create-text-type-handler (name type)
  `(define-handler ,name (Text)
     (let ((resultObj (new ,type
                           :project sk8)))
       (setf (baseText resultObj) me)
       resultObj)))

(create-text-type-handler characters  IndirectCharacterVector)
(create-text-type-handler words IndirectWordVector)
(create-text-type-handler paragraphs IndirectParagraphVector)
(create-text-type-handler lines IndirectLineVector)

;-----------------------------------------------------------------------------
; Private handlers and functions
;-----------------------------------------------------------------------------

(defmacro create-writeobject-for-text-vector (type str)
  `(define-handler sk8dev::writeobject :private (,type theStream rereadably)
                   (declare (ignore rereadably))
                   (unless (maybeWriteObjectName me theStream)
                     (format theStream ,str)
                     (writeObject (baseText me) theStream t))))

(create-writeobject-for-text-vector IndirectCharacterVector "the characters ")
(create-writeobject-for-text-vector IndirectWordVector "the words ")
(create-writeobject-for-text-vector IndirectParagraphVector "the paragraphs ")
(create-writeobject-for-text-vector IndirectLineVector "the lines ")


#|
	Change History (most recent last):
	1  	 4/22/96	Brian   	
	2  	 5/ 2/96	Brian   	
	3  	 5/ 7/96	sidney  	add declaration to remove annoying warning message
	4  	 5/13/96	Brian   	
	5  	12/17/96	Brian Roddy	Fixing handling of characters.
	6  	 2/27/97	Hernan  	
|# ;(do not edit past this line!!)
