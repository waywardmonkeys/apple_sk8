;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories

(in-package :sk8dev)

(provide "DIRECTIONALCONNECTOR")

(require "CONNECTOR" "objects;Widgets:Connector")
(require "ARROW" "objects;Shapes:Arrow")

;;; directionalConnector -- a connector to be used for your directional connection needs.

(new connector :objectName "DirectionalConnector" :project sk8 :undisposable t :prototype t)

(setf (endGeometry directionalConnector) 'projectCenter)
(setf (startGeometry directionalConnector) 'projectCenter)

(changeParents directionalConnector (list Connector Arrow))

(setf (endArrow directionalConnector) t)


#|
	Change History (most recent last):
	2  	 2/27/97	Hernan  	
|# ;(do not edit past this line!!)
