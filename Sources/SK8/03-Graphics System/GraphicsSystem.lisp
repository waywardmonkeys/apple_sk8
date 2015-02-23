;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories

(in-package :ccl)

;;; _______________________________ 
;;; Patching provide and require.
;;; _______________________________ 

(defvar *the-real-provide* (symbol-function 'provide))

;;; This is a list of the form ((pathString . provideString)...)
;;; We need it to map pathnames to provide strings so that we can use require instead of load.

(defvar *provide-path-alist* nil)

(defmacro pathString->requireString (pathString)
  `(cdr (assoc (remove-extension (sk8::relativize-pathname ,pathString))
               *provide-path-alist*
               :test #'string-equal)))

;;; Remove the .xxx at the end of a file. This is so that the fasls are loaded if available.  

(defun remove-extension (pathString)
  (subseq pathString 0 (position #\. pathstring :from-end t)))

(let ((*warn-if-redefine-kernel* nil)
      (*warn-if-redefine* nil))
  
  ;;; Just like provide, but creates an assoc list of path -> require string. 
  
  (defun provide (module)
    (prog1 
      (funcall *the-real-provide* module)
      (unless (assoc (remove-extension (sk8dev::relativize-pathname (ccl::loading-file-source-file)))
                     *provide-path-alist*
                     :test #'string-equal)
        (push (cons (remove-extension (sk8dev::relativize-pathname (ccl::loading-file-source-file))) module)
              *provide-path-alist*))))
  
  ;;; My own version of REQUIRE, to make things faster and allow SK8Script to use it.
  
  (defun require (module &optional pathname &aux (str (string module)))
    (when (null module) (report-bad-arg module '(not null)))
    (when (and (not (member str *modules* :test #'string=))
               (not (member str *loading-modules* :test #'string=))
               (or pathname
                   (setq pathname (find-module-pathnames str))
                   (progn
                     (cerror "If ~S still hasn't been provided, you will be asked to choose a file."
                             "The module ~S was required while loading ~S. No file could be found for that module."
                             str *loading-file-source-file*)
                     (unless (member str *modules* :test #'string=)
                       (catch-cancel
                         (setq pathname (choose-file-dialog :mac-file-type
                                                            (list #.(ccl::make-keyword (pathname-type *.fasl-pathname*
                                                                                                    :case :common))
                                                                  :text)))))
                     pathname)))
      (let ((*loading-modules* (cons str *loading-modules*)))
        (if (consp pathname)
          (dolist (path pathname) 
            ;; (load path)
            (cl-user::sk8-build-files path))
          ;; (load pathname)
          (cl-user::sk8-build-files pathname)
          ))
      (setq *modules* (adjoin str *modules* :test #'string=)))
    str)
  )

;;; Make it visible to SK8Script...

(import 'require :sk8)
(import 'provide :sk8)

;;; _______________________________ 
;;; The Graphics System Load File
;;; _______________________________ 

(in-package :cl-user)

;;; Create the Graphics system package.

(unless (find-package :graphics-system)
  (defpackage :graphics-system
    (:use :ccl :common-lisp)
    (:nicknames :gs)))

;;; Pathnames used in this sub system.

(def-logical-directory "graphics;" "sk8;03-Graphics System:")
(def-logical-directory "graphics-kernel;" "graphics;kernel:")
(def-logical-directory "graphics-patches;" "graphics;MCL patches:")

(def-logical-directory "objects;" "sk8;07-Objects:")
(def-logical-directory "graphics-objects;" "objects;03-Graphics System Core:")
(def-logical-directory "functions;" "sk8;06-Globals:Function Library:")

;;; Import/Export...

(sk8-build-files "graphics;Import.lisp" "graphics;Export.lisp")

;;; Load the MCL patches that the graphics system needs.

(sk8-build-files 
 "graphics-patches;MCLcmd-key"
 "graphics-patches;MCLmenus"
 "graphics-patches;MCLwindows-patch"
 "graphics-patches;Fred 3.0 Patch:load-fred-patch"
 )


(sk8-build-files 
 "macf-trap-support;Utilities")

;;; Load traps required. 

(sk8-build-files
 "macf-trap-library;misc"
 "macf-trap-library;QuickDraw"
 "macf-trap-library;QDOffScreen"
 )

;;; Load the core graphics system.

(sk8-build-files 
 "graphics-kernel;toolbox-extensions"     ;; Useful graphics routines.
 "graphics-kernel;Fixed Point Package"    ;; SK8 Fixed-point arith. package
 "graphics-kernel;globals"                ;; SK8 Constants and global variables
 "graphics-kernel;macros"                 ;; SK8 Macros
 "graphics-kernel;Let+"                   ;; The glorious Let+.
 "graphics-kernel;structures"             ;; SK8 Internal Structures
 "graphics-kernel;region-macros"          ;; Macros dealing with recomputing regions.
 "graphics-kernel;utility-functions"      ;; SK8 Miscellaneous utilities
 "graphics-kernel;CLOS-glue"              ;; SK8 CLOS glue / window management
 "graphics-kernel;pixmap-caching"         ;; Pixmap Caching Functions
 "graphics-kernel;shaped-window"          ;; SK8 shaped window wdef (used to load BLANK window here!)
 "graphics-kernel;hidemenubar"            ;; Lets you hide the menubar. 
 "graphics-kernel;theBackgroundWindow"    ;; The background window that covers the finder.
 "graphics-kernel;splash window"
 "graphics-kernel;save-Image"             ;; preserve and restore for GS.
 "graphics-kernel;public utilities"       ;; Utility functions exported. 
 "graphics-kernel;Recycle Mechanism"
 )

;;; These are patches to Object system stuff.  REMOVE REMOVE REMOVE REMOVE

;;(load "ccl;Modularization Notes")

;;; Load the core graphics system objects. 

(sk8-build-files 
 ;; Events.
 "graphics-objects;Event Modes"           ;; SK8 custom event modes
 ;; Renderers.
 "graphics-objects;Media Types"
 "graphics-objects;Renderer"              ;; SK8 color system
 "graphics-objects;ImageRenderer"         ;; SK8 renderers
 "graphics-objects;Standard Resources"    ;; SK8 media used by renderers.
 "graphics-objects;Standard Renderers"    ;; SK8 Instances of Renderers.
 ;; Actor.
 "graphics-objects;graphic"               ;; Stage and inheritance path to Actor.
 "graphics-objects;Actor"                 ;; The actors.
 "graphics-objects;ActorMixins"           ;; Bounded by contents and stuff like it.
 "graphics-objects;drag"                  ;; Drag sub system.
 "graphics-objects;dragOnStage"           ;; Dragging actors on the stage.
 ;; Shapes.
 "graphics-objects;rectangle"             ;; SK8's rectangle.
 ;; Menus, menubars and menuitems.
 "graphics-objects;menus"
 ;; Imaging.
 "graphics-objects;PenAndMask"            ;; The imaging system!
 ;; The Clipboard. 
 "graphics-objects;Clipboard"             ;; The SK8Clipboard object. 
 "graphics-objects;Translator"            ;; Just the string translator.
 )

;;; Load the dynamic linking code and record the state of the world.

(sk8-build-files 
 "graphics-kernel;Dynamic Linking"     ;; Record existing objects & define utilities.
 )
#|
	Change History (most recent last):
	2  	 4/ 8/96	Hernan  	Loading the fred patch as part of the graphics build.
	3  	 4/16/96	Brian   	
	4  	 4/19/96	sidney  	Split import and export files to avoid package problems
	5  	 5/ 7/96	Hernan  	Loading extra trap support stuff.
	6  	 5/23/96	sidney  	don't hard code the file type name FASL, because it has a different name on the PPC
	7  	 7/ 7/96	sidney  	changes for native PPC build
	8  	 9/ 4/96	Hernan  	Moved the traps to the object system folder.
	9  	 2/27/97	Hernan  	
|# ;(do not edit past this line!!)
