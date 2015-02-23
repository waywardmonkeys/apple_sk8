;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories


;;;

 
(in-package :macframes)

(eval-when (:compile-toplevel :load-toplevel :execute) ; makes SURE the exports happen!
  
  (import '(SK8::initializerArgument
            SK8::processUnusedInitializerArguments
            SK8::SK8-error
            SK8::let+
            SK8::make-pt
            SK8::make-rect
            SK8::nmapcar
            SK8::pt
            SK8::rect)
          :mf)

  ;; Symbols inserted into the appropriate preload file  8-19-93   6:10 pm
  (SK8-declare-syms :SK8 :private
                    SK8::let+
                    SK8::make-pt
                    SK8::make-rect
                    SK8::nmapcar
                    SK8::pt
                    SK8::rect)
  
  ;; Symbols inserted into the appropriate preload file  9-22-94   1:29 pm
  (SK8-declare-syms :SK8 :public
                    SK8::initializerArgument
                    SK8::processUnusedInitializerArguments
                    SK8::me
                    SK8::define-SK8-constant
                    SK8::define-SK8-var
                    SK8::define-SK8-macro
                    SK8::define-SK8-function
                    SK8::define-handler
                    SK8::define-system-handler
                    SK8::inheritsFrom
                    SK8::is-a
                    SK8::*undefined*)

  (import '(SK8::inheritsFrom?
            SK8::inheritsFrom
            SK8::is-a
            SK8::*undefined*)
          :macframes)
  
  (import '(SK8::is-a
            SK8::*undefined*)
          :SK8Script)

  
  (export '(slot-value-if-exists
            set-slot-value-if-exists
            with-repeated-slot-access
            bitset
            bitclear
            def-bit-accessors
            dolist-into
            doheads
            dovector-in-reverse
            with-initializerArguments
            special
            declare-not-global
            default-not-defined-error)
          :macframes)

  (export '(CCL::%slot-missing-marker
            )
          :ccl)
  
  )



#|
	Change History (most recent last):
	8		11/1/93	kleiman	
	10 	 9/16/94	It      	added is-a exports/imports to this file
	11 	 9/22/94	chip    	added *undefined*
	12 	 9/22/94	chip    	added necessary eval-when
	13 	 9/26/94	chip    	added a few good exports
	14 	10/ 3/94	chip    	initializerArgument stuff
	15 	10/ 4/94	chip    	more initializerArgument stuff
	16 	10/ 4/94	chip    	SK8-condition stuff
	17 	10/ 6/94	chip    	added declare-not-global
	18 	11/16/94	chip    	added default-not-defined-error
	2  	 5/ 7/96	sidney  	Changes for new object system
	3  	10/18/96	sidney  	remove some no longer used symbols
	4  	 2/27/97	Hernan  	
|# ;(do not edit past this line!!)
