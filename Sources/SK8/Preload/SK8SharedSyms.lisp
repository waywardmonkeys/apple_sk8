;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories


(in-package :SK8)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; These are "shared" from CCL to SK8, and from COMMON-LISP to SK8
;;;


(SK8-share :SK8
           
           CCL:QUIT
           
           CCL:CLEAR
           CCL:CUT
           CCL:PASTE

           CCL:LISTENER
           CCL:MENU
           CCL:VIEW
           
           CCL::BEEP
           CCL:LSH
           
           COMMON-LISP:*
           COMMON-LISP:+
           COMMON-LISP:-

           COMMON-LISP:ABS
           COMMON-LISP:ACOS
           COMMON-LISP:ACOSH
           COMMON-LISP:ASH
           COMMON-LISP:ASIN
           COMMON-LISP:ASINH
           COMMON-LISP:ATAN
           COMMON-LISP:ATANH
           COMMON-LISP:CEILING
           COMMON-LISP:COS
           COMMON-LISP:COSH
           COMMON-LISP:EXP
           COMMON-LISP:EXPT
           COMMON-LISP:FCEILING
           COMMON-LISP:FFLOOR
           COMMON-LISP:FLOOR
           COMMON-LISP:FROUND
           COMMON-LISP:FTRUNCATE
           COMMON-LISP:GCD
           COMMON-LISP:IMAGPART
           COMMON-LISP:LOG
           COMMON-LISP:MAX
           COMMON-LISP:MIN
           COMMON-LISP:PHASE
           COMMON-LISP:RANDOM
           COMMON-LISP:REALPART
           COMMON-LISP:ROUND
           COMMON-LISP:SIGNUM
           COMMON-LISP:SIN
           COMMON-LISP:SINH
           COMMON-LISP:SQRT
           COMMON-LISP:TAN
           COMMON-LISP:TANH
           COMMON-LISP:TRUNCATE
           
           COMMON-LISP:NULL
           COMMON-LISP:REST
           COMMON-LISP:SORT
           
           COMMON-LISP:ARRAY
           COMMON-LISP:BYTE
           COMMON-LISP:CHARACTER
           COMMON-LISP:COMPLEX
           COMMON-LISP:INTEGER
           COMMON-LISP:LIST
           COMMON-LISP:NUMBER
           COMMON-LISP:REAL
           COMMON-LISP:SEQUENCE
           ;;COMMON-LISP:STREAM
           COMMON-LISP:STRING   ;???
           COMMON-LISP:SYMBOL
           COMMON-LISP:VECTOR
           
           COMMON-LISP:PACKAGE
           COMMON-LISP:TYPE
                      
           COMMON-LISP:PI
           COMMON-LISP:SPACE

           ;; These are all reserved words in SK8Script, so they can't be used as
           ;; handler/property names anyway
           
           CCL:FALSE
           CCL:LOCAL
           CCL:TRUE
           COMMON-LISP:AND
           COMMON-LISP:APPLY
           COMMON-LISP:DO
           COMMON-LISP:EQUAL
           COMMON-LISP:EVERY
           COMMON-LISP:GET
           COMMON-LISP:IF
           COMMON-LISP:LAST
           COMMON-LISP:NOT
           COMMON-LISP:OR
           COMMON-LISP:REMOVE
           COMMON-LISP:RETURN
           COMMON-LISP:SET
           COMMON-LISP:SOME
           COMMON-LISP:THE
           COMMON-LISP:UNLESS
           
           COMMON-LISP:FIRST
           COMMON-LISP:SECOND
           COMMON-LISP:THIRD
           COMMON-LISP:FOURTH
           COMMON-LISP:FIFTH
           COMMON-LISP:SIXTH
           COMMON-LISP:SEVENTH
           COMMON-LISP:EIGHTH
           COMMON-LISP:NINTH
           COMMON-LISP:TENTH
           
           )


#|
	Change History (most recent last):
	2		8/24/93	chip	
	3		8/27/93	chip	
	4		8/31/93	kleiman	added cl:stream
	5		9/23/93	kleiman	added CCL:VIEW
	6		11/2/93	nil	added CCL:UNDO
	7		2/12/94	kleiman	renaming
	8		2/18/94	sidney	remove common-lisp:length common-lisp:position
	9		4/3/94	kleiman	
	10		4/3/94	kleiman	
	11		5/3/94	kleiman	Problem 1160766: QUIT made public
	12		6/8/94	sidney	Problem 1161844: LOOP used for movies
	13		6/17/94	chip	added CONDITION
	14		7/15/94	dy	remove loop
	15 	10/31/94	chip    	removed MOD
	16 	11/16/94	chip    	removed the comparator operators
	17 	11/16/94	chip    	added *, +, - since they're truly shared
	18 	11/18/94	chip    	ABS and ACOS were missing; added them
	19 	11/20/94	chip    	added realPart & imagPart
	20 	11/20/94	chip    	removed (obsolete) REM
	21 	 3/ 1/95	sidney  	
	2  	 8/17/95	sidney  	move Stream to overridden
	3  	12/15/95	sidney  	move Condition to overridden
	2  	 5/ 7/96	sidney  	Changes for new object system
	3  	 2/27/97	Hernan  	
|# ;(do not edit past this line!!)
