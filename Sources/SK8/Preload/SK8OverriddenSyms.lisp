;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories


(in-package :SK8Development)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  These are made separate symbols in SK8 (and "inherited" into UI);  i.e. they're new
;;;  symbols residing in SK8 and totally unconnected with those in CCL or COMMON-LISP.
;;;
;;;  IMPORTANT:  To refer to any of these SK8 symbols from one of the "development"
;;;             packages, you must explicitly qualify it with "SK8::".
;;;


(SK8-override :SK8 (:UI)
              
              COMMON-LISP:>
              COMMON-LISP:<
              COMMON-LISP:=
              COMMON-LISP:<=
              COMMON-LISP:>=
              COMMON-LISP:/

              COMMON-LISP:SLEEP

              CCL:MENUBAR
              CCL:TARGET
              CCL:WINDOWS
              CCL:WINDOW
              CCL:COPY
              CCL:UNDO
              
              COMMON-LISP:ABORT
              COMMON-LISP:APPEND
              COMMON-LISP:CHAR
              COMMON-LISP:CLOSE
              COMMON-LISP:CONDITION
              COMMON-LISP:DELETE
              COMMON-LISP:DIRECTORY
              COMMON-LISP:ERROR
              COMMON-LISP:EXPORT
              COMMON-LISP:FILL
              COMMON-LISP:FLOAT
              COMMON-LISP:FUNCTION
              COMMON-LISP:IMPORT
              COMMON-LISP:INTERSECTION
              COMMON-LISP:LENGTH
              COMMON-LISP:MOD
              COMMON-LISP:PATHNAME
              COMMON-LISP:POSITION
              COMMON-LISP:QUOTE
              COMMON-LISP:REPLACE
              COMMON-LISP:REVERSE
              COMMON-LISP:SEARCH
              COMMON-LISP:SHADOW
              COMMON-LISP:STANDARD
              COMMON-LISP:STREAM
              COMMON-LISP:UNION

              ;; QuickTime
              COMMON-LISP:STEP
              COMMON-LISP:LOOP
              COMMON-LISP:TIME
              )

#|
	Change History (most recent last):
	2	8/31/93	kleiman	added cl:pathname
	3	9/2/93	hernan	added cl:import and cl:export to be used for clipboard.
	5	9/27/93	chip	added ccl:target
	6	2/18/94	sidney	added common-lisp:function, position, length
	7	2/21/94	hernan	Adding windows to the list.
	8	2/21/94	hernan	Adding window and close to the list.
	9	2/22/94	hernan	close is in COMMON-LISP...
	10	2/22/94	hernan	Adding second.
	11	2/25/94	hernan	Added standard, fill and shadow which are used
				for user visible options.
	12	4/21/94	dy	step
	13	6/17/94	chip	added Abort
	14	7/15/94	dy	loop
	15 	10/ 5/94	chip    	added append
	16 	10/31/94	chip    	added MOD
	17 	11/16/94	chip    	added the comparator operators
	18 	11/16/94	chip    	more operators
	19 	12/21/94	me      	Add EditText search and replace handlers
	20 	 3/ 6/95	dy      	add cancel
	21 	 3/11/95	dy      	add TIME, back out CANCEL
	2  	 8/17/95	sidney  	add Stream
	3  	12/15/95	sidney  	add Condition
	2  	 4/29/96	Hernan  	Adding sleep which is provided as a sk8-function in the
						processes file.
	3  	 5/ 7/96	sidney  	various symbols
	4  	 2/27/97	Hernan  	
|# ;(do not edit past this line!!)
