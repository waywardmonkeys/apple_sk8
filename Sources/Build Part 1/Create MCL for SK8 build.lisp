(in-package :cl-user)

;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories


;;; This creates an MCL image with patches and with resource changes that are necessary.
;;; For convenience in using the resulting image for development work, it also defines the packages
;;; used in SK8 and loads SourceServer (eventually)

;;; This file will create the build part one, in a separate process.
;;; This is required because if not, things happen in the listener process
;;; which has to be killed when the listener goes away as we build fred...

;;; Load the old style logical pathnames. 

(load "ccl:library;logical-dir-compatibility.lisp")

;;; Main pathnames
(def-load-pointers init-logical-ccl-directory () ;; this should be done when the saved image starts up as well as now
  (def-logical-directory "ccl;" (namestring (truename "ccl:"))))

(process-run-function nil 
                      #'(lambda ()
                          (load "ccl;Build Part 1:Real Build P1 Work.lisp")))


#|
	Change History (most recent last):
	2		2/21/94	sidney	File to create an MCL to use for building SK8
	3		2/21/94	sidney	Patch a bug in the Resources library
	4		2/21/94	sidney	Add boyer-moore file search example to developer utilities
	5		2/22/94	hernan	Redefining *default-menubar* to include the
							source server menu.
	6		2/23/94	sidney	Use logical pathname for source files instead of hardcoding the build machine's truename for it
	7		2/25/94	sidney	load development-package-utils (scans for public symbols) in development build
	8		2/25/94	sidney	define preload exporting functions earlier in build
	9		2/25/94	sidney	typo
	10		2/25/94	dy		added  macptr-ptr.lisp to the build list
	11		2/25/94	kleiman	moved load of symbol-case-saving here
	12		2/25/94	kleiman	add a call to sk8dev::initialize-trap-null-ptr-values in reset-sk8-globals
	13		2/26/94	kleiman	added xor.lisp to the load list
	14		2/26/94	kleiman	removed call to initialize trap stuff -dy
	15		3/1/94	sidney	Remove many-classes patch until I can get the source and modify it!!!
	16		3/1/94	sidney	Add MCL libraries to default sourceserver projects
	17		3/1/94	sidney	Setting the user and initials to nil so that you can
							be prompted.
	18		3/2/94	sidney	Fix problem with resource code when resource does not exist
	19		3/2/94	Hernan	Now loading Fred 3.0!
	20		3/4/94	dy		(pushnew :macframes *features*)
	21		3/4/94	dy		:SK8 in *features instead of :macframes
	22		3/4/94	sidney	get logical directories to come up correctly in both lisp and sk8 images
	23		3/5/94	sidney	Remove :SK8 from feature list, pending making sure that things that are turned on by it actually work
	24		3/7/94	sidney	Add gcable classes file to load
	25		3/10/94	yost	reinstate the (pushnew :SK8 *features*)
	26		3/25/94	chip	now loads "wrapper-inspection" patch for development version
	27		3/29/94	sidney	preload stuff should not be only in developer version
							ask first, build later
	28		3/29/94	sidney	change default name for app depending on whether developer or release version
	29		3/29/94	sidney	Move appleEvent support to base MCL
	30		4/1/94	sidney	*fasl-save-local-symbols* must be t for define-handler to work correctly!
	31		4/5/94	sidney	turn on egc by default on startup if mmu support is available
	32		5/1/94	sidney	Radar bug #1157940, made uninformative error message more informative
	33		5/2/94	sidney	whoops. screwed up check for errorp in last fix.
	34		6/29/94	chip	added symbol-case-saving at start of load order
	35		6/29/94	chip	took out old "symbol-case-saving" load
	36		6/29/94	chip	changed restypes to strings (from keywords) since symbols now have arbitrary case
	37		7/6/94	chip	took out the new fangled symbol case-saving mechanism
	38		7/12/94	dy		line up file names in Listener
	39		7/20/94	dy		add pr.lisp
	40		8/4/94	sidney	movw load of mf-kernel;SK8Script-symbols-tools to earlier in build
	41 	 8/22/94	It      	change default mcl save name
	42 	 9/ 2/94	chip    	SK8'ed MCL now has modified compiler policy that doesn't constant-fold any constants holding standard-objects
	43 	 9/ 2/94	It      	fixed typo
	44 	 9/12/94	nw      	Added highlevel-event-patch to load list
	45 	 9/14/94	sidney  	only allow reindex-interfaces to be run in vanilla MCL
	46 	 9/26/94	chip    	made packages SK8Script, SK8Development, UIDevelopment, PS, and Wood use Macframes
	47 	 9/26/94	dy      	add l1-io.lisp to build
	48 	 9/26/94	chip    	oops -- fixed glitch in ":use" clause of defpackage's
	49 	10/ 3/94	dy      	add ccl-menus to the build
	50 	10/13/94	dy      	add "Patches to base MCL;encapsulate.lisp" to the build & #+DYost code for no tail recursion optimization
	51 	10/17/94	sidney  	speedup a screw case in use of hash tables that Wood was getting hit by
	52 	10/19/94	sidney  	compiler policy stuff: use the official MCL functions instead of hacking structures directly,
							change the file compilation policy, not just the interactive one,
							use the existing optimize debug 3 proclamation to give DYost his tail recursion results,
							move all of it so it doesn't effect any of the part 1 build.
	53 	10/28/94	dy      	support for building from .sk8 files
	54 	10/28/94	dy      	fix copyNewResourcesToSavedMCL to copy over small icon masks
	55 	11/14/94	dy      	add apropos fixups
	56 	11/15/94	dy      	 Dave@Yost.COM
	57 	11/16/94	dy      	
	58 	11/16/94	dy      	use new keyword arg on with-resource
	59 	11/16/94	dy      	now does its thing by stamping the revID of the master file in the fasl
	60 	11/16/94	dy      	*give-recompile-reason* sense was reversed
	61 	11/16/94	dy      	unwind-protect in get-source-info-stamp
	62 	11/18/94	sidney  	Always store recompile-required-revID field of CCLe defrecord as a number, never as nil
	63 	12/14/94	dy      	add boolean.lisp to the build
	64 	12/15/94	jol     	fix compile-sk8-file to do the right project thing
	65 	12/16/94	dy      	eval-enqueue compile-sk8-file
	66 	12/16/94	dy      	compile-sk8-file doesn't set the project;  that'll be handled as a default in compileScriptFile
	67 	 1/29/95	sidney  	when deciding to build project files, use the same methods we are using for deciding to compile files
	68 	 2/ 3/95	dy      	Add :save-definitions to *sk8-features*
	69 	 2/ 3/95	dy      	typo
	70 	 2/10/95	sidney  	provide for different from and to resource ids in when we copy resources
	71 	 2/13/95	dy      	remove xor.lisp, when-unwind.lisp, add utilities.lisp, utility-macros.lisp
	72 	 2/20/95	dy      	pr.lisp -> trace-print.lisp
	73 	 2/20/95	dy      	move encapsulate.lisp before trace-print.lisp
	74 	 2/20/95	dy      	remove utility-macros.lisp
	75 	 3/ 2/95	dy      	new with-debugging-features macro
	76 	 3/ 2/95	dy      	Moved with-debugging-features macro to Patches to Base MCL
	77 	 3/19/95	sidney  	new resources for new standalone app creator icons
	78 	 3/22/95	sidney  	increase default minimum memory partition size to 25mb
	79 	 4/24/95	dy      	Make recompile immune to changes in daylight savings time.
	80 	 4/28/95	dy      	recompile-require fix
	81 	 4/28/95	dy      	Use full path for "ccl:sk8;Recompile Required Timestamp" so it's always findable
	2  	 6/ 8/95	sidney  	MCL 3.0 changes
	3  	 8/20/96	Hernan  	When building for the PPC we need to load the old style
						logical pathnames.
	4  	 9/ 6/96	Hernan  	Checked in the logical pathnames patch into the build part 1 folder.
	5  	 9/20/96	sidney  	It's easier to load the old style logical pathnames patch if you don't try to use old style names to load it
	6  	10/17/96	sidney  	Get the logical pathname compatibility file from the MCL library folder
	7  	10/22/96	sidney  	remove conditionalization that specified ccl-4, as it is for ccl-3.1 too
	8  	 2/27/97	Hernan  	
|# ;(do not edit past this line!!)
