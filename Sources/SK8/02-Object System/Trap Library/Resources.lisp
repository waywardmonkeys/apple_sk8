(in-package :sk8dev)

;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories




(defun_X T_AddResource nil (theData theType theID name)
  "The name arg is converted to a Pascal string.
See IM-More Mac Toolbox p. 1-90"
  (with-pstrs ((pName name))
    (checking-Toolbox-Error (:void (#_ResError))
                            (#_AddResource theData theType theID pName))))

(defun_X T_WriteResource nil (theResource)
  "See IM-More Mac Toolbox p. 1-93"
    (checking-Toolbox-Error (:void (#_ResError))
                            (#_WriteResource theResource)))

(defun_X T_Unique1ID nil (resourceType)
  "See IM-More Mac Toolbox p. 1-96"
  (checking-Toolbox-Error (:value (#_ResError)) (#_Unique1ID resourceType)))

(defun_X T_DetachResource nil (theResource)
  "See IM-More Mac Toolbox p. 1-108"
    (checking-Toolbox-Error (:void (#_ResError))
                            (#_DetachResource theResource)))

(defun_X T_RemoveResource nil (theResource)
  "See IM-More Mac Toolbox p. 1-109"
    (checking-Toolbox-Error (:void (#_ResError))
                            (#_RmveResource theResource)))



(defun_X T_GetResource nil (resourceType resourceID &key notFoundOK)
  "Returns a handle to the given resource or raises an error.
If notFoundOK is true, then if the error is resNotFound or resourceTypeNotFound,
it returns two values: (nil error-result-code) instead of raising an error.
Can generate a resourceTypeNotFound error which Dave Yost has unofficially
assigned to the heretofore-unused result code -191.  (See Radar bug #1153580.)
See IM-More Mac Toolbox p. 1-74"
  (if notFoundOK
    ;; returns 2 values
    (checking-toolbox-error (:pointer
                             (#_ResError) 
                             :error-result -191
                             :ok-errors (list -191 #$resNotFound))
                            (#_GetResource resourceType resourceID))
    ;; returns 1 value
    (checking-toolbox-error (:pointer
                             (#_ResError)
                             :error-result -191)
                            (#_GetResource resourceType resourceID))))

(defun_X T_Get1Resource nil (resourceType resourceID &key notFoundOK)
  "Returns a handle to the given resource or raises an error.
If notFoundOK is true, then if the error is resNotFound or resourceTypeNotFound,
it returns two values: (nil error-result-code) instead of raising an error.
Can generate a resourceTypeNotFound error which Dave Yost has unofficially
assigned to the heretofore-unused result code -191.  (See Radar bug #1153580.)
See IM-More Mac Toolbox p. 1-74"
  (if notFoundOK
    ;; returns 2 values
    (checking-toolbox-error (:pointer
                             (#_ResError)
                             :error-result -191
                             :ok-errors (list -191 #$resNotFound))
                            (#_Get1Resource resourceType resourceID))
    ;; returns 1 value
    (checking-toolbox-error (:pointer
                             (#_ResError)
                             :error-result -191)
                            (#_Get1Resource resourceType resourceID))))

(defun_X T_Get1ResourceGC nil (resourceType resourceID
                                     &key
                                     notFoundOK
                                     terminationFunction
                                     terminateOnQuit)
  "Returns a GCHandle to the given resource or raises an error.
See documentation for T_Get1Resource for arguments and return codes.
Default terminationFunction is DisposeHandle.
Always calls detachResource to prevent the possibility of a subsequent
call returning another GCHandle pointing to the same place thus setting
up a situation in which the handle could be disposed twice."
  (if notFoundOK
    (multiple-value-bind_X
      (theRes theError)
      (T_Get1Resource resourceType resourceID :notFoundOK t)
      (values_X (if theRes
                  (progn
                    (checking-Toolbox-Error (:void (#_ResError)) (#_detachResource theRes))
                    (if terminationFunction
                      (T_handleToGCHandle theRes
                                        :terminationFunction terminationFunction
                                        :terminateOnQuit     terminateOnQuit)
                      (T_handleToGCHandle theRes)))
                  nil)
                theError))
    (let ((theRes (T_Get1Resource resourceType resourceID :notFoundOK nil)))
      (if theRes
        (progn
          (checking-Toolbox-Error (:void (#_ResError)) (#_detachResource theRes))
          (if terminationFunction
                      (T_handleToGCHandle theRes
                                        :terminationFunction terminationFunction
                                        :terminateOnQuit     terminateOnQuit)
                      (T_handleToGCHandle theRes)))
        nil))))


(defun T_OpenResFile (pathname)
  "OpenResFile, handling errors, Pascalstringing and Macifying the pathname."
  (with-pstrs ((pstr-namestring (mac-namestring pathname)))
    (checking-toolbox-error (:minus-one (#_ResError))
                            (#_OpenResFile pstr-namestring))))

(defun T_CreateResFile (pathname)
  (with-pstrs ((pstr-namestring (mac-namestring pathname)))
    (checking-toolbox-error (:void (#_ResError))
                            (#_CreateResFile pstr-namestring))))


(defun T_UseResFile (refnum)
  (checking-toolbox-error (:void (#_ResError))
                          (#_UseResFile refnum)))

(defun T_CloseResFile (frRefNum)
  "Error trap around CloseResFile"
  (checking-toolbox-error (:void (#_ResError))
                          (#_CloseREsFile frRefNum)))

;;; Removes the Res of a file, if it exists.  Not really a toolbox function.
;;; Perhaps there's a better way?
(defun RemoveRes (pathname)
  "The (#_CloseResFile (#_OpenResFile)) idiom.  With pstr-ing built in."
  (with-pstrs ((pstr-namestring (mac-namestring pathname)))
    (let ((rf (#_OpenResFile pstr-namestring)))
      (unless (eql rf -1)
        (#_CloseResFile rf)
        t))))



(defun T_GetResInfo (theresource theid thetype thename)
  (checking-toolbox-error (:void (#_ResError))
                          (#_GetResInfo theresource theid thetype thename)))

(defun T_Get1IndResource (thetype index)
  (checking-toolbox-error (:pointer (#_ResError))
                          (#_Get1IndResource thetype index)))

(defun T_ReleaseResource (theresource)
  (checking-toolbox-error (:void (#_ResError)) 
                          (#_ReleaseResource theresource)))

(defun T_Get1NamedResource (thetype name)
  "Error trap around Get1NamedResource, including pstring the name"
  (with-pstrs ((pstr-name name))
    (checking-toolbox-error (:pointer (#_ResError))
                            (#_Get1NamedResource thetype pstr-name))))

(defun T_UpdateResFile (refnum)
  (checking-toolbox-error (:void (#_ResError))
                          (#_UpdateResFile refnum)))

(defun T_LoadResource (theresource)
  (checking-toolbox-error (:void (#_ResError))
                          (#_LoadResource theresource)))

#|
	Change History (most recent last):
	1	3/23/94	dy	
	2	3/23/94	dy	add copyright notice
	3	5/3/94	dy	1157787 fix -- resourceTypeNotFound
	4	5/12/94	dy	New: AddResource, WriteResource, DetachResource, RemoveResource Unique1ID
	5	6/9/94	dy	remove the declare-syms
	6	6/23/94	dy	make get1resource capable of returning false instead of error (for bug Sidney is fixing)
	7	6/25/94	sidney	T_Get1Resource new function that doesn't make a gcable pointer out of the handle
	8	6/25/94	sidney	multiple-value-bind_X didn't do the obvious. Backed out the use of it.
	9  	 9/ 6/94	dy      	Get1Resource checking-toolbox-error :pointer rework
	10 	10/31/94	sidney  	touch to force recompile of T_Get1Resource functions with changed checking-toolbox-error :notFoundOK t macro
	11 	10/31/94	dy      	fix T_Get1ResourceGC multivals return problem
	12 	11/28/94	dy      	gc macro mods
	13 	 1/17/95	till    	T_OpenResFile, T_CreateResFile, T_UseResFile, T_CloseResFile
							                                          T_GetResInfo, T_Get1IndResource, T_ReleaseResource, T_Get1NamedResource
	14 	 1/18/95	till    	add RemoveResFile
	15 	 1/19/95	till    	D'Oh!
	16 	 2/10/95	dy      	tweaks to invocations of macros that reduce to make-terminable-macptr
	17 	 3/16/95	till    	add T_getResource
	3  	 2/27/97	Hernan  	Removing case preserving reader macro.
						2   9/ 4/96Hernan  Putting the traps in sk8dev.
|# ;(do not edit past this line!!)
