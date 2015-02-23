;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories

;;;-*- Mode: Lisp; Package: CL-USER -*-

(in-package :cl-user)


;; to catch left over bad stuff at compile time -- some of it is redefined when the build is ready for it
(defmacro sk8dev::special (&rest r) (declare (ignore r)) (error "Macro Special should never be called"))
(defmacro sk8::withlockedcursor (&rest r) (declare (ignore r)) (error "Macro withlockedcursor should never be called"))
(defmacro sk8::withcursor (&rest r) (declare (ignore r)) (error "Macro withcursor should never be called"))

;;; Create the Object system's package.

(unless (find-package :macframes)
  (defpackage :macframes
    (:use :ccl :common-lisp)
    (:nicknames :mf)))

(def-logical-directory "macf;" "sk8;02-Object System:")
(def-logical-directory "macf-core;" "sk8;07-Objects:02-Object System Core:")
(def-logical-directory "macf-trap-support;" "sk8;02-Object System:Trap Support:")
(def-logical-directory "macf-trap-library;" "sk8;02-Object System:Trap Library:")

;; take this out after making sure that all references to the symbls use an explicit package reference

(let ((syms '(sk8::originalObject
              sk8::makeProxyFor
              sk8::MakeObjectAProxy
              sk8::proxy
              sk8::addparent
              sk8::inheritsfrom
              sk8::is-a
              sk8::define-handler
              sk8::restore
              sk8::preserve
              sk8::Object
              sk8::me
              sk8::inheritedProperties sk8::inheritedVirtualProperties
              sk8::localProperties sk8::localVirtualProperties
              sk8::properties sk8::virtualProperties
              sk8::ancestors
              sk8::mapAncestors
              sk8::AddProperty sk8::RemoveProperty sk8::GetPropertyValue sk8::SetPropertyValue
              sk8::sk8
              sk8::project
              sk8::KNOWNCHILDREN sk8::PROJECT sk8::FILE sk8::REQUIREDPROJECT sk8::REQUIRINGPROJECTS
              sk8::VERSION sk8::PACKAGE sk8::OBJECTTABLE sk8::PREFERENCES
              sk8::NEW sk8::GETVALUE sk8::SETVALUE
              sk8::BaseParent sk8::Initialize sk8::ObjectName sk8::Parents
              sk8::SK8_flags
              sk8::SK8_id)))
  (import syms :mf)
  (export syms :mf)
  )

(sk8-build-files "macf;globals"
                 "macf;01-obj"
                 "macf;object-creation-macros"
                 "macf;02-creation-relation"
                 "macf;03-new-and-init"
                 "macf;04-bootstrap"
                 "macf;05-basic-obj-api"
                 "macf-core;proxy"
                 "macf;06-obj"
                 "macf-core;built ins"
                 "macf-core;builtin handlers"
                 "macf;new multivalue stuff"
                 ;; move all the these to the correct folders
                 "macf-trap-support;macros-traps"
                 "macf-trap-support;Trap Support;gcosptr"
                 "macf-trap-support;Trap Support;Records"
                 "macf-trap-support;osptr-sk8Object"
                 "macf-trap-library;Resources"
                 "macf-trap-library;Memory"
                 "macf-core;Project"
                 "macf-core;File"
                 "macf;initialize-bogus-project"
                 "macf-core;System"
                 "macf-core;sk8-error-system"
                 "macf-core;Devices"
                 "macf-core;specialstorage"
                 "macf-core;Ports"
                 "macf-core;Virtual Types"
                 "macf;07-creat-relat-api"
                 "macf;copy resources"
                 "macf;systemlog"
                 "macf;08-leftovers"
                 "macf;09-WriteObject"
                 )

#|
	Change History (most recent last):
	1  	 5/ 9/96	sidney  	New object system
	2  	 5/13/96	Hernan  	Adding the writeObject file.
	4  	 7/ 7/96	sidney  	private symbols no longer exported from sk8 package
	5  	 9/ 3/96	Hernan  	Can't remember. Probably no change.
	6  	 9/ 4/96	Hernan  	Getting rid of the macframes folder. Everything the object
						system loads is now in the object system.
	7  	10/17/96	sidney  	add SK8 error system
	8  	 2/27/97	Hernan  	
|# ;(do not edit past this line!!)
