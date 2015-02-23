(in-package :cl-user)

;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories



(provide "STORE")

(def-logical-directory "store;" "sk8;04-Store:")

(require "HANDLERSTORE" "sk8;05-SK8Script:Store:RAM Store")

(cl-user::sk8-build-files 
 "store;Runtime Utilities"
 "store;Old Store Utilities"
 "store;Save"
 "store;Source Dumper"
 "store;Simple Queue"
 "store;Code Generator Class"
 "store;Generic CodeGen"
 "store;Fasl Concatenate"
 "store;SS/FASL CodeGen"
 "store;Project Parent Order"
 "store;Project Walker"
 "store;split-lfun"
 "store;Make-load-form"
)

#|
	Change History (most recent last):
	1	12/22/93	sidney	Create a load file for this module
	2	2/23/94	kleiman	added source-dumper.lisp
	3	3/6/94	chip	coercion & writeObject
	4	6/23/94	sidney	utilities.lisp no longer loaded
	5  	 9/26/94	kend    	Added new files for SK8Script codegen.
	6  	10/12/94	It      	Added Generic-CodeGen.lisp
	7  	 2/10/95	sidney  	changes for buildstandalone merging resources into application file
	2  	 7/ 5/95	sidney  	load new file for fasl codegen
	3  	 7/ 6/95	Brian Roddy	Making it load the consolidated version
						of save as text and fasl.
	4  	 7/14/95	Brian Roddy	adding fasl concatenate
	2  	 5/20/96	sidney  	add files for saving functions and handlers as fasl
	3  	 9/ 3/96	Hernan  	Added a runtime utilities file.
	4  	 2/27/97	Hernan  	
|# ;(do not edit past this line!!)
