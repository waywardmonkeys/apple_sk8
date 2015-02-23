
#include "BlankWDEFDefines.h"	
	// Must be included before Apple interfaces.

#include <Memory.h>
#include <ToolUtils.h>
#include <Types.h>
#include <Windows.h>

// ÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑ
//
//	Blank WDEF window. 													 
//
// ÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑ
//		This is the SK8 window WDEF that allows arbitrarily shaped windows.
//
// 		This is done by copying the actor's boundsRegion into the struct and 
//		content regions of the window. This copying should be done as part of
//		this WDEF but when in the PPC, MCL requires its environment in order to
//		run lisp code. When a lisp-defined WDEF is invoked in the background, the
// 		app that causes the invocation dies into MacsBugs.
//
//		The work of copying the regions is done instead from SK8 at certain 
//		times when we know the window will change regions. The WDEF definition
// 		does nothing.
// ÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑ

pascal long main(short varCode, WindowPeek window, short message, long param) 
{
	long result = 0;

	switch (message) {
			
		Point hitPt;
		hitPt.v = HiWord(param);
		hitPt.h = LoWord(param);
			
		case wHit:	
			result = wInContent;
			break;
	}
	
	return result;
}


