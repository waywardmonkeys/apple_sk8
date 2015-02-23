;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories

; FILE          "Project-Walker.lisp"
; IMPLEMENTS     Walks project objects to drive a code generator.
; AUTHOR         Ken Dickey
; DATE           1994 July 27
; LAST UPDATE    1994 July 27
; COPYRIGHT (c)  1994 Apple Computer, Inc. ; All rights reserved
; NOTES          See also "Project-Parent-Order.lisp"
; STATUS         Alpha              

;; ALGORITHM

;----Objective
;
; The Project Walker takes a SK8 Project, a code generator, 
; and a codegen protocol, and walks all objects in the 
; designated project and its subprojects.  If any objects are not 
; cross-“authorable” they are listed in an error diagnostic and the processing 
; stops. Otherwise all project objects and objects they reference which are not 
; in the libraries are ordered by their parent-child relations (a topological 
; sort: parents before children) and the code generator called on each object.
; The results of code generation are combined with the libraries by a link step 
; to generate a title/executable/product.


;----Design Issues/Wrinkles

;Nested Projects:
;
; Nested projects are handled via modules.  The basic strategy is that each 
; project is a module which exports all its bindings.  Each contained module 
; imports all non-shadowed bindings from its parent and exports all its own 
; bindings.  This is recursive to all levels. {Optimization: leaf
; projects do not have to export any bindings}.
;

;Distinguished “main” function.
;
; SK8 has no distinguished “main” or “kick off” function. ScriptX does.  
; This function does things like [a] put up a “splash screen”, [b] start a 
; sound loop, [c] check what OS and HW resources are available and load 
; specific libraries, [d] other initialization actions {e.g. object creation 
; and set!s}.  For our purposes, the ideal strategy is to break the 
; initialization function into 2 parts [1] a kick-off function which is 
; minimal and puts up a splash screen, starts a sound loop, etc. and 
; [2] the initialization proper, which loads libraries, etc.  The reason 
; for this is that our generated code needs to create and initialize 
; title objects.  If we do this before the splash screen and sound loop, 
; the used waits too long to see something.  On the other hand, we have to 
; create and initialize our foundation objects before the initialization 
; proper can take place.   The simplest solution is to specify an optional 
; sound loop and splash screen at title generation time.

;Walk-ordering info and codegen usage pattern
;
; The code generator is driven via a simple open/use/close protocol.  In 
; order to pass object ordering information to the code generator, the
; caller creates a hash table (object -> creation-order-number).
; This allows the code generator to distinguish
; which objects at any given time have been output
; so that object slot initialization can be either by reference or must be 
; delayed to a later set!  
;
;  Open( codegen-obj, sk8-project-object, 
;        object->creation-order-table, 
;        Output-Protocol, Warning-Stream, 
;        optional-splash-screen, optional-sound-loop, 
;        Non-interactive? )  -> false if error, else true
;  // use error mechanism rather than Error-Stream 
;
;  GenCode( codegen-obj, SK8-Obj-for-which-code-is-required, 
;                        Object-creation-order-number ) 
;                        ->  false if error, else true
;  // GenCode is at least once for each project object
;
;  Close( codegen-obj, distinguished-main-function )
;                      -> false if error, else true
;  // called with distinguished-main-function = #false on error-cleanup case
;  // NB: This code should *not* close the Output-Protocol
;
;
; The above implies the following operational model for the code generator:
;
;  Open(..) 
;    Save persistent internal references to object->creation-order-table,
;      Output-Protocol, Warning-Stream, 
;      optional-splash-screen, optional-sound-loop, and Non-interactive?
;    Create (empty) stretchy vector for unnamed-refs
;
;  GenCode(..)
;    Generate code for SK8-Obj-for-which-code-is-required
;      If object is unnamed, allocate a slot in the unnamed-refs vector.
;      Remember its name or unnamed-refs vector-ref 
;            {e.g. in object->creation-order-table}
;      Gen code for instance/slots creation which init slot values which have 
;		been created and delay others {know because their creation # is
;            >= the current object counter value}
;      Gen code for handlers
;
;      Warnings to Warning-Stream
;      Code to Output-Protocol  // see “Batched Codegen Protocol”, below
;      Recoverable errors are signaled if Non-interactive? is false, else     
;        become non-recoverable
;      Non-recoverable errors are of course always signaled
;
;  Close(..)
;    Gen code for splash-screen and sound-loop display
;    Gen code for object initialization 
;    Release references created by Open(), e.g. by set!ing them to #false.
;    Gen code for distinguished-main-function
;


;Anonymous Object References
;
; It is assumed that the code generator tracks object references.  Named
; objects can be referred to directly {in init value or set!s}.  A growable
; vector of unnamed objects is kept and their indices used to refer to them.
; Code is generated to create a fixed sized vector in the target environment. 
; Generated code uses the 1 reserved vector name and an index to get the 
; value of an unnamed object.  Code is generated so that after object
; initialization stage in the target environment, the vector of unnamed
; objects is released to the collector.


;Code Generator Error Handling
;
; In this context there are 2 cases of errors [1] recoverable, 
; [2] non-recoverable.   Recoverable errors are handled directly by
; the code generator interactive with the user via popup dialogs.  
; Non-recoverable errors are signaled with suitable text to be caught 
; and displayed by the project walker.  {Need to define the error handler
; list}.  Upon catching an error, the project walker posts it to the user
; and calls the code generator’s Close function.


;Batched Codegen Protocol
;
; In dealing with code generation in a distributed environment, it is usually
; a large speed win if operations are blocked/batched together to save
; rpc/network/io overhead.  It is desirable to factor out the protocol 
; from the project walker and component code generators.  Note that both 
; text output and tethered output can return errors such as “device full” 
; and “network connection lost” which may be recoverable.  This suggests
; that a code output protocol gets first chance at handling protocol errors.  
;
; A natural strategy for batch protocol is as follows.  
; [1] establish a code generator check point. 
; [2] Gen code into the protocol-stream until the return status indicates 
; “next” or “retry” {non-recoverable errors are assumed signaled}.  
; When “next” is noted, go to step 1. 
; When “retry” is noted, back up codegen to the saved state of the 
; checkpoint--i.e. forget that any computation has happened after 
; the checkpoint and try again.  [An incremental protocol just gens
; the code into the protocol-stream as it sees each object.  There
; is no need to checkpoint an incremental protocol].
;
; Codegen protocol notes: The protocol should be stream based and return
; a status.  The protocol must indicate whether it is “batch” or “incremental”.
; The protocol takes care of retry and recovery cases, including interaction
; with the user, before bailing out by signaling a non-recoverable error.
; We need to enumerate the errors.
;
; It is assumed that the UI preferences/defaults know how to associate libraries,
; a project, a code generator, and a codegen protocol.



;----Miscellaneous Notes
;
;SK8 Object Attributes:
;  UnAuthorable - Can’t cross-author this object {e.g. MacTraps}
;  NonLiteral - Don’t copy structure, make new & copy elts {e.g. dictionary}
;  Literal - Use literal {e.g. #[1,2,3] vs make(<array>, 1,2,3)}
;  Unique  - Bind to Unique/Singleton object in target system {e.g. #false}
;  Foundation - Make ref to object in foundation/baseline/kernel library
;  Generable - Templated objects which are logically part of the foundation 	  
;              library but in reality are generated on demand {space savings}
;
; {SK8 seems to have no literals other than symbols, numbers, and characters}.


;; {@@@ STILL TO DO
;;      Menu item to specify code generator {SX default}, libs
;;      Menu item to select project, specify libs
;;      CodeGen options
;; }@@@

(in-package :SK8Dev)  ;; CL needs modules!

;; (require 'Simple-Queue 'CodeGenerator-class 'Project-Parent-Order)

(defmethod walk-project ( (sk8-project sk8::project)
                          (code-generator CodeGenerator)
                          output-protocol
                          warning-stream
                          libs-info-set
                          interactive?
                          splash-screen
                          sound-loop
                          main-function
                          )
  (multiple-value-bind (creation-order-info-table creation-order-queue)
                       (make-creation-table-and-list 
                           sk8-project
                           (Get-object-info-maker code-generator))
    (doOpen code-generator 
            sk8-project
            creation-order-info-table
            libs-info-set
            output-protocol
            warning-stream
            splash-screen
            sound-loop
            (not interactive?)
            )
    (labels (
             (process-loop (obj-and-order)
               (when obj-and-order
                 (tickeventclock)
                 (let ( (obj (sk8-object obj-and-order)) )
                   (when (and 
                          (doGenCode code-generator obj-and-order)
                          (not (empty? creation-order-queue)))
                     (process-loop (pop-first creation-order-queue)))
               ) ) )
             )

      (when (not (empty? creation-order-queue))
        (process-loop (pop-first creation-order-queue))) ; gen dem codes

      (doClose code-generator main-function)

    ) ; end labels
) )


;;                              --- E O F ---

#|
	Change History (most recent last):
	1  	 9/26/94	kend    	Walks project objects to drive a code generator.
	2  	11/ 7/94	kend    	White out several internal QuickTime objects which should never be built
	3  	11/ 7/94	kend    	Nit
	4  	11/14/94	dy      	call dontSave handler to see if an object should be saved
	5  	 1/ 4/95	sidney  	remove remaining traces of obsoleted lib functionality
	6  	 1/12/95	sidney  	changes to save as text to be more like binary store
	7  	 3/ 8/95	sidney  	allow for animated cursor during save
	2  	 8/ 9/95	Brian   	
	2  	 2/27/97	Hernan  	
|# ;(do not edit past this line!!)
