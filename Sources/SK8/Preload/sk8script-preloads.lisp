;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories


;;;

(in-package :SK8)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; *** NO IDEA WHY THIS IS NECESSARY!!!  THERE'S NO REFERENCE TO THIS IN THE SK8SCRIPT PACKAGE BEFORE NOW!
;;(unintern 'SS::where :SK8Script)


;; First shadowing import the language keywords that conflict with glisp symbols
;;(CCL::shadowing-import '(all call repeat)
;;                       :SK8Script)


;; Now import (and declare) all the language keywords (as taken from the DECLARERESERVEDWORDS form at the top of "grammar.lisp") from :SK8 into the :SK8Script package!
(CCL::import '(a after all am an and any anything apply are aren arent as at back before beginning behind body but by call cleanup
               constant contain contained contains defaulting div do does doesn doesnt don dont eighth eleventh else end
               ends equal equals every everything exit false fifth first for forever fourth from front get given global greater
               handler i if in inherited insert into is isn isnt it its itself last less local me middle my myself named nd
               new next ninth no not object of on one or over pass private project range rd remove repeat result return returns same
               second set seventh sixth st start starts tenth th than that the then third through thru times to true twelfth
               unless until using wait where whether while whose with without
               INITIALIZEMEM DISPOSEMEM STACK REGD0 REGD1 REGD2 REGD3 REGD4 REGD5 REGD6 REGD7 REGA0 REGA1 REGA2 REGA3 REGA4 REGA5  
               CALLSPEC OUT INOUT BYREFERENCE BYVALUE Mac68kMPWCCallSpec Mac68kPascalCallSpec Mac68kTrapCallSpec 
               FOREIGN CALLIN TYPE MEMRECORD MEMPOINTER MEMHANDLE FOREIGNMEMORY MEMARRAY TYPENAME FIELDS ITEMTYPE NUMITEMS)
             :SK8Script)

(SK8-declare-syms :SK8 :public
                  a after all am an and any anything apply are aren arent as at back before beginning behind body but by call cleanup
                  constant contain contained contains defaulting div do does doesn doesnt don dont eighth eleventh else end
                  ends equal equals every everything exit false fifth first for forever fourth from front get given global greater
                  handler i if in inherited insert into is isn isnt it its itself last less local me middle my myself named nd
                  new next ninth no not object of on one or over pass private project range rd remove repeat result return returns same
                  second set seventh sixth st start starts tenth th than that the then third through thru times to true twelfth
                  unless until using wait where whether while whose with without
                  DISPOSEMEM INITIALIZEMEM STACK REGD0 REGD1 REGD2 REGD3 REGD4 REGD5 REGD6 REGD7 
                  REGA0 REGA1 REGA2 REGA3 REGA4 REGA5  
                  CALLSPEC OUT INOUT BYREFERENCE BYVALUE Mac68kMPWCCallSpec Mac68kPascalCallSpec Mac68kTrapCallSpec 
                  FOREIGN CALLIN TYPE MEMRECORD MEMPOINTER MEMHANDLE FOREIGNMEMORY MEMARRAY TYPENAME FIELDS ITEMTYPE NUMITEMS
                  )

;; Now import (and declare) the language keywords that aren't explicit in "grammar.plisp"
(CCL::import '(item thing)
             :SK8Script)
(SK8-declare-syms :SK8 :public
                  item thing)



(CCL::import '(SK8-error)
             :SK8Script)
(SK8-declare-syms :SK8 :public
                  SK8-error)



#|
	Change History (most recent last):
	1		12/17/93	till	New file.  Just the symbols from the old preload file.
	2		2/18/94	chip	updated symbols for alpha revision of grammar
	3		3/3/94	chip	one more symbol addition
	4		3/6/94	chip	
	5		4/5/94	kend	Added Foreign Function and Data Interface support
	6		4/5/94	chip	tweaked so it can load at beginning of build
	7		4/15/94	kend	Added symbol ForeignMemory
	8		4/29/94	kend	Syntax update for initializeMem and disposeMem
	9		6/17/94	chip	added RANGE & OVER & CONDITION to syms
	10 	10/31/94	yost    	qualified SK8::mod to get the right symbol
	11 	10/31/94	yost    	now SK8::mod doesn't get imported to SK8Script package
	2  	12/15/95	sidney  	do the right thing with symbols sk8::mod and sk8::condition
	2  	 5/ 7/96	sidney  	Changes for new object system
	3  	10/18/96	sidney  	remove some no longer used symbols
	4  	 2/27/97	Hernan  	
|# ;(do not edit past this line!!)
