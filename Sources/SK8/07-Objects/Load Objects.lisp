;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories

(in-package :cl-user)

;;; Builds every object in the object files... We build from
;;; simple to complex. (There should be no ordering problem since
;;; every file requires the right thing). Also, require is used since
;;; files might be loaded by other files. 

;;; Processes API. Should be loaded as part of the ObjectSystem.

(require "PROCESSES" "objects;02-Object System Core:Processes")

;;; Import/Export

(require "QDPICTURETOPICTTRANSLATOR" "objects;Import/Export:QDPictureToPICTTranslator")
(require "BWPATTERNTOPATTTRANSLATOR" "objects;Import/Export:BWPatternToPATTranslator")
(require "COLORCURSORRSRCTOCRSRTRANSLATOR" "objects;Import/Export:ColorCursorRSRCTocrsrTrans")
(require "COLORPATTERNTOPPATTRANSLATOR" "objects;Import/Export:ColorPatternToppatTrans")
(require "CURSORRSRCTOCURSTRANSLATOR" "objects;Import/Export:CursorRSRCTocursTranslator")
(require "ICONRSRCTOCICNTRANSLATOR" "objects;Import/Export:IconRSRCTocicnTranslator")
(require "PIXELMAPTOPICTTRANSLATOR" "objects;Import/Export:PixelMapToPICTTranslator")
(require "SOUNDRSRCTOSNDTRANSLATOR" "objects;Import/Export:SoundRSRCTosndTranslator")

(require "INDIRECTTRANSLATOR" "objects;Import/Export:IndirectTranslator")
(require "IMAGERENDERERTOCICNTRANSLATOR" "objects;Import/Export:ImageRendererTocicnTrans")
(require "IMAGERENDERERTOPATTRANSLATOR" "objects;Import/Export:ImageRendererToPATTrans")
(require "IMAGERENDERERTOPICTTRANSLATOR" "objects;Import/Export:ImageRendererToPICTTrans")
(require "IMAGERENDERERTOPPATTRANSLATOR" "objects;Import/Export:ImageRendererToppatTrans")
(require "SOUNDTOSNDTRANSLATOR" "objects;Import/Export:SoundTosndTranslator")

;;; Clocks

(require "DATETIME" "objects;Clocks:DateTime")
(require "DATETIMEINDIRECT" "objects;Clocks:DateTimeIndirect")

(require "ABSTRACTCLOCK" "objects;Clocks:AbstractClock")
(require "CLOCK" "objects;Clocks:Clock")
(require "SLAVECLOCK" "objects;Clocks:SlaveClock")
(require "REALTIMECLOCK" "objects;Clocks:RealTimeClock")
(require "SECONDSCLOCK" "objects;Clocks:SecondsClock")
(require "MINUTESCLOCK" "objects;Clocks:MinutesClock")

;;; Media

(require "COLORCURSOR" "objects;Media:ColorCursor")
(require "ANIMATEDCURSOR" "objects;Media:AnimatedCursor")
(require "SOUND" "objects;Media:Sound")

;;; Apple Events

(require "APPLEEVENTS" "objects;02-Object System Core:Apple Events")
(require "APPLESCRIPTOBJECT" "objects;02-Object System Core:AppleScript Object")

;;; Effects

(require "VISUALEFFECTS" "objects;Effects:VisualEffects")

(require "COMPLEXRGBCOLOR" "objects;Effects:ComplexRGBColor")
(require "GRADIENT" "objects;Effects:Gradient")
(require "COMPLEXGRADIENT" "objects;Effects:ComplexGradient")
(require "BEVELRENDERER" "objects;Effects:BevelRenderer")
(require "HATCH" "objects;Effects:Hatch")
(require "MULTIRENDERER" "objects;Effects:MultiRenderer")
(require "SKETCHRENDERER" "objects;Effects:SketchRenderer")
(require "TBSKETCHRENDERER" "objects;Effects:TBSketchRenderer")
(require "SWATCHRENDERER" "objects;Effects:SwatchRenderer")
(require "DYNAMICRENDERER" "objects;Effects:DynamicRenderer")
(require "LONERANGERRENDERER" "objects;Effects:LoneRangerRenderer")

;;; Movies

(require "QUICKTIME" "objects;Movies:main")

;;; Shapes

(require "OVAL" "objects;Shapes:Oval")
(require "ROUNDRECT" "objects;Shapes:RoundRect")
(require "MASKEDACTOR" "objects;Shapes:MaskedActor")
(require "HALO" "objects;Shapes:Halo")
(require "POLYGON" "objects;Shapes:Polygon")
(require "LINESEGMENT" "objects;Shapes:LineSegment")
(require "ARROW" "objects;Shapes:Arrow")
(require "SELECTIONDOTS" "objects;Shapes:SelectionDots")

;;; Widgets

(require "LABEL" "objects;Widgets:Label")
(require "CHECKBOX" "objects;Widgets:CheckBox")
(require "MULTIOBJECTSTATECHECKBOX" "objects;Widgets:MultiObjectStateCheckBox")
(require "RADIOBUTTON" "objects;Widgets:RadioButton")
(require "SCROLLER" "objects;Widgets:Scroller")
(require "SLIDER" "objects;Widgets:Slider")
(require "CONNECTOR" "objects;Widgets:Connector")
(require "DIRECTIONALCONNECTOR" "objects;Widgets:DirectionalConnector")
(require "SPLITTER" "objects;Widgets:Splitter")
(require "OBJECTDATARECT" "objects;Widgets:ObjectDataRect")
(require "PICKERMENU" "objects;Widgets:PickerMenu")
(require "PAINTFIELD" "objects;Widgets:PaintField")

;;; EditText

(require "EDITTEXT" "objects;EditText:EditText")
(require "TEXTFIELD" "objects;EditText:TextField")
(require "SCRIPTEDITTEXT" "objects;EditText:ScriptEditText")
(require "EDITORSCRIPTEDITTEXT" "objects;EditText:EditorScriptEditText")

;;; Pickers

(require "PICKER" "objects;Pickers:Picker")
(require "TEXTLIST" "objects;Pickers:TextList")
(require "STYLEDPICKER" "objects;Pickers:StyledPicker")
(require "ICONTEXTPICKER" "objects;Pickers:IconTextPicker")
(require "HIERARCHICALPICKER" "objects;Pickers:HierarchicalPicker")
(require "MULTILINEPICKER" "objects;Pickers:MultiLinePicker")

(require "TABLEPICKER" "objects;Pickers:TablePicker")
(require "SELECTBYROWTABLEPICKERADDON" "objects;Pickers:SelectByRowTPAddOn")
(require "TABLEPICKEREDITORADDON" "objects;Pickers:TablePickerEditorAddOn")
(require "COLORPICKER" "objects;Pickers:ColorPicker")

(require "LINEARTEXTPICKER" "objects;Pickers:LinearTextPicker")

;;; Browser Components

(require "PROPERTYVALUEDATA" "objects;Browser Components:PropertyValueData")
(require "UNDOABLESETLOG" "objects;Browser Components:UndoableSetLog")
(require "BROWSERCOMPONENTS" "objects;Browser Components:Browser Components")
(require "SIMPLEPROPERTYEDITOR" "objects;Browser Components:SimplePropertyEditor")

;;; Dialogs

(require "MODALDIALOGMODE" "objects;Dialogs:ModalDialogMode")
(require "DIALOGBOX" "objects;Dialogs:DialogBox")
(require "MESSAGETOUSER" "objects;Dialogs:MessageToUserDialogBox")
(require "YESORNODIALOGBOX" "objects;Dialogs:YesOrNoDialogBox")
(require "GETANSWERFROMUSER" "objects;Dialogs:GetAnswerFromUser")
(require "SELECTFROMCOLLECTIONDIALOG" "objects;Dialogs:SelectFromCollectionDialog")
(require "FILEDIALOGS" "objects;Dialogs:File Dialogs")

(require "GETFROMUSER" "objects;Dialogs:GetFromUser")
(require "GETLISTFROMUSER" "objects;Dialogs:GetListFromUser")
(require "GETRENDERERFROMUSER" "objects;Dialogs:GetRendererFromUser")
(require "GETCONTENTFROMUSER" "objects;Dialogs:GetContentFromUser")

(require "GETNEWNUMBERFROMUSER" "objects;Dialogs:GetNewNumberFromUser")
(require "GETNEWOBJECTFROMUSER" "objects;Dialogs:GetNewObjectFromUser")
(require "GETNEWSTRINGFROMUSER" "objects;Dialogs:GetNewStringFromUser")
(require "GETNEWSYMBOLFROMUSER" "objects;Dialogs:GetNewSymbolFromUser")
(require "GETNEWRGBCOLORFROMUSER" "objects;Dialogs:GetNewRGBColorFromUser")

(require "OBJECTSYSTEMDIALOGS" "objects;Dialogs:ObjectSystemDialogs")

;;; Palettes

(require "DRAWPALETTE" "objects;Palettes:DrawPalette")

;;; Browsers

(require "FILECHOOSER" "objects;Browsers:FileChooser")
(require "RESOURCECHOOSER" "objects;Browsers:ResourceChooser")
(require "SK8ABOUTBOX" "objects;Browsers:SK8AboutBox")
(require "TREEBROWSER" "objects;Browsers:TreeBrowser")

;;; MacWidgets


#|
	Change History (most recent last):
	2  	 4/12/96	Hernan  	Getting rid of non existant files.
	3  	 4/29/96	Hernan  	Loading the processes.
	4  	 5/ 2/96	Hernan  	Adding the textField which for some reason we had forgotten.
	5  	 5/ 7/96	Hernan  	Adding propertyValueData.
	6  	 9/16/96	Brian   	Adding Apple Events.lisp
	7  	 9/26/96	Hernan  	Adding the paintField and LoneRangeRenderer.
	8  	11/14/96	Hernan  	Apple Events have to load after the media types.
	9  	11/25/96	Hernan  	Adding the AppleScript Object.
	10 	 2/27/97	Hernan  	
|# ;(do not edit past this line!!)
