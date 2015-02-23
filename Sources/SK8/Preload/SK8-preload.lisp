;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories


;;;


(sk8-build-files
 )


(SK8-build-files "sk8;preload:exporting"
                 "sk8;Preload:SK8Script-symbols-tools"
                 "SK8;Preload:SK8OverriddenSyms"
                 "SK8;Preload:SK8SharedSyms"

                 "SK8;Preload:sk8script-preloads"
                 "SK8;Preload:mf-exports"

                 "SK8;Preload:SK8PrivateSyms"
                 "SK8;Preload:SK8PublicSyms"
                 "SK8;Preload:SK8SpecialSyms"
                 "SK8;Preload:SK8OptionSyms"
                 
                 "SK8;Preload:UISharedSyms"
                 "SK8;Preload:UIPrivateSyms"
                 "SK8;Preload:UIPublicSyms"
                 "SK8;Preload:UISpecialSyms")

#|
	Change History (most recent last):
	2	7/13/93	chip	new file
	3	2/20/94	sidney	use build function, add specials files
	4	2/25/94	hernan	Added sk8OptionSyms to the list!
	5	2/25/94	sidney	move load of exporting.lisp to earlier in build, remove .lisp extensions
	6	4/5/94	chip	now loads "preload-symbols" for SK8Script syms
	7	4/5/94	sidney	overriden and shared symbols have to come first
	8  	 9/29/94	sidney  	load mf-kernel;exports here so they get done earlier in load
	9  	12/ 9/94	sidney  	read in a file of symbol case information
	10 	 1/31/95	sidney  	1180502: move loading of case-sensitive name info from here to end of build so they can't get overwritten during the build process
	2  	 4/16/96	sidney  	move the functions that are used in preload to preload to use
	3  	 2/27/97	Hernan  	
|# ;(do not edit past this line!!)
