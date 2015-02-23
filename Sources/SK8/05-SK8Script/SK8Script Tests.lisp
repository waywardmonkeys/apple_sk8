;;; SK8 © 1997 by Apple Computer, Inc.
;;; The code in this file is protected under the terms of the current SK8 License.
;;; For information on this license, see http://sk8.research.apple.com
;;; Apple Computer, Inc. -- Apple Research Laboratories


;;;
(in-package :skil)

;;;;---------------------------------------------------------------------------
;;;;SK8Script tests
;;;;---------------------------------------------------------------------------

(time

(progn

(progn

(defun sk8::twoArgFun (x y)
  (mod x y))

  ;;basic expressions
(test-it "Blue" sk8::Blue)
(test-it "1" 1)
(test-it "(((1)))" 1)
(test-it "1 + 1" 2)
(test-it "1-1" 0)
(test-it "1 + 2 * 3" 7)
(test-it "not 1 + 2 * 3 = 9" t)
(test-it "true or false" t)
(test-it "true and not false" t)
(test-it "not true or not false" t)
(test-it "set uiselection to the (((blue)))" sk8::Blue)
(test-it "7 's twoArgFun ( 4 )" 3)
(test-it "(1 + 6) 's twoArgFun ( 4 )" 3)
(test-it "5 + ((((((3 * (2 + 1))))))) + 6" 20)
(test-it "1 is in {1,2,3}" t)
(test-it "4 is not in {1,2,3}" t)
(test-it "1 <> 2" t)
(test-it "{2,3,4} doesn't contain 1" t)


;;function call tests
(test-it "(((fillcolor of stage)))" sk8::desktopcolor)
(test-it "the fillcolor of stage" sk8::desktopcolor)
(test-it "the media of messagebox's container's fillcolor" sk8::desktoppattern)
(test-it "media of (the messagebox's container)'s fillcolor" sk8::desktoppattern)
(test-it "sendtolog of the objectstring of the rectangle's fillcolor's forered's twoArgFun of 256" "255")
(test-it "sendtolog 3 with attention sendtolog 2" 3)
(test-it "sendtolog of 3 with attention sendtolog of 2" 3)
(test-it "objectstring of the string 3 & the string 2" "\"32\"")
(test-it "apply 'twoArgFun' to {7,4}" 3)
(test-it "call 'twoArgFun' of 7, 4" 3)

;;byzantine path expressions
(test-it "last actor whose fillcolor is white in {1,2,rectangle,3,4,oval,5}" sk8::oval)
(test-it "last actor whose fillcolor is vomit in {1,2,rectangle,3,4,oval,5}" nil)
(test-it "item 1 in list 1 thru 2 where number 1 in it = 2 and integer 2 in it = 3 in {{oval, 2,3},{4,5},{2,3,4}}" (list sk8::oval 2 3))
(test-it "first item in first thru second list where first number in it = 2 and second integer in it = 3 in {{oval, 2,3},{4,5},{2,3,4}}" (list sk8::oval 2 3))
(test-it "objectstring of first item in first thru second list where first number in it = 2 and second integer in it = 3 in {{oval, 2,3},{4,5},{2,3,4}}" (sk8::objectstring (list sk8::oval 2 3)))
(test-it "position of item 1 that = 2 in the first item in first thru second list where first number in it = 2 and second integer in it = 3 in {{oval, 2,3},{4,5},{2,3,4}}" 2)
(test-it "integer 2 thru 3 after the first list where item 1 in it = 2 before actor 1 in {1, {2,5}, 2,{2,2} ,3,{1,3}, 4, actor, 5, 6, 7}" (list 4 5))
(test-it "integer 2 thru 3 after the first list where item 1 in it = 2 in everything before actor 1 in ((({1, {2,5}, 2,{2,2} ,3,{1,3}, 4, actor, 5, 6, 7})))" (list 3 4))
(test-it "objectstring of integer 2 thru 3 after the first symbol before actor 1 in {1,2,'symbol',3,'symbol', 4, actor, 5, 6, 7}" (list "5" "6"))
(test-it "objectstring of integer 1 after the first symbol before actor 1 in {1,2,'symbol',3,'symbol', 4, actor, 5, 6, 7}" "4")
(test-it "the item 1 where odd of it in {2,4, 6, 889,4}" 889)
(test-it "item 1 whose odd in {2,4, 6, 889,4}" 889)
(test-it "insert 222 after 3 in {1,rectangle,3,roundrect,oval,5}" (list 1 sk8::Rectangle 3 222 sk8::RoundRect sk8::Oval 5))
(test-it "insert 222 after integer 2 in {1,rectangle,3,roundrect,oval,5}" (list 1 sk8::Rectangle 3 222 sk8::RoundRect sk8::Oval 5))
(test-it "remove item 1 before integer 2 in {1,rectangle,3,roundrect,oval,5}" (list 1 3  sk8::RoundRect sk8::Oval 5))
(test-it "position of oval 1 after 3 in {1,rectangle,3,roundrect,oval,5}" 5)
(test-it "position of actor 3 thru 2 after 3 in {1,rectangle,3,roundrect,4,polygon, 6, oval,5}" (list 8 6))
(test-it "fillcolor of actor 10 thru 2 before 5 in {1,rectangle,3, messagebox,4, oval,5}" (list UI::UIWindowColor sk8::White))
(test-it "fillcolor of actor 1 thru 2 before 5 in {1,rectangle,3, messagebox,4, oval,5}" (list UI::UIWindowColor sk8::White))
(test-it "remove every actor in {1,rectangle,3,roundrect,oval,5}" (list 1 3 5))
(test-it "twoArgFun of 7, every number in {1,rectangle,3,roundrect,oval,5}" (list 0 1 2))
(test-it "first item before item 1 that = oval in {1,oval,2}" 1)
(test-it "first item before oval in {1,oval,2}" 1)
(test-it "insert oval after 2 in {3,2,1}" (list 3 2 sk8::oval 1))
(test-it "middle item in {1,2,3,4,5}" 3)
(test-it "middle item in {1,2,3,4}" 2)
(test-it "middle actor in {1,rectangle,3,oval, polygon,4}" sk8::oval)
(test-it "middle number that's less than 4 in {1,rectangle,3,oval, 2, polygon,4}" 3)
(test-it "any number that's less than 4 in {3,rectangle,3,oval, 3, polygon,4}" 3)

;;collection protocol tests
;;these need to be evaluated one after the other in their groups... 

;; Lists
(test-it "set uiselection to {1,2,3,4}" (list 1 2 3 4))
(test-it "insert actor at the start of uiselection" (list sk8::actor 1 2 3 4))
(test-it "insert 5 at the end of uiselection" (list sk8::actor 1 2 3 4 5))
(test-it "get integer 3 thru 2 in uiselection" (list 3 2))
(test-it "the first integer that is greater than 3 after 2 in uiselection" 4)
(test-it "insert 666 before 3 in uiselection" (list sk8::actor 1 2 666 3 4 5))
(test-it "insert 777 into uiselection" (list sk8::actor 1 2 666 3 4 5 777))
(test-it "insert 888 at the start of uiselection" (list 888 sk8::actor 1 2 666 3 4 5 777))
(test-it "remove every integer that's less than 100 in uiselection" (list 888 sk8::actor 666 777))
(test-it "set the first actor in uiselection to its fillcolor" (list 888 sk8::white 666 777))
(test-it "set every number in uiselection to its objectstring" (list "888" sk8::white "666" "777"))
(test-it "every string where it as a number > 700 in uiselection" (list "888" "777"))
(test-it "second item that starts with 666 in {{1,2},{666,3},{666,4}}" '(666 4))
(test-it "every item that starts with 666 in {{1,2},{666,3},{666,4}}" '((666 3) (666 4)))
(test-it "first item that ends with 3 in {{1,2},{666,3},{666,4}}" '(666 3))
(test-it "every item that ends with 3 in {{1,2},{666,3},{666,4}}" '((666 3)))
(test-it "any item that ends with 3 in {{1,2},{666,3},{666,3}}" '(666 3))

;; test the nasty stuff of using lisp lists for our linked list object.
(test-it "set uiselection to {1}" (list 1))
(test-it "remove item 1 from uiselection" nil)
(test-it "set uiselection to {1}" (list 1))
(test-it "remove 1 from uiselection" nil)
(test-it "set uiselection to {1,2,3,4}" (list 1 2 3 4))
(test-it "remove every item from uiselection" nil)
(test-it "set uiselection to {1,2,3,4}" (list 1 2 3 4))
(test-it "remove item 1 thru 4 from uiselection" nil)


;; STRINGS
(test-it "set uiselection to \"i see helena and kate\"" "i see helena and kate")
(test-it "get uiselection contains \"helena\"" t)
(test-it "get word 2 thru 3 in uiselection" (list "see" "helena"))
(test-it "get result" (list "see" "helena"))
(test-it "set word 3 in uiselection to \"tyson\"" "i see tyson and kate")
(test-it "set second thru third word in uiselection to \"isaac\"" "i isaac isaac and kate")
(test-it "get word 3 that starts with \"i\" in uiselection" "isaac")
(test-it "get every word that ends with \"c\" in uiselection" (list "isaac" "isaac"))
(test-it "get every word that contains \"a\" in uiselection" (list "isaac" "isaac" "and" "kate"))
(test-it "remove word 1 that = \"isaac\" from uiselection" "i  isaac and kate")
(test-it "insert \" shop\" after word 1 in uiselection" "i shop  isaac and kate")
(test-it "remove the first character that = \" \" after \"p\" from uiselection"  "i shop isaac and kate")
(test-it "remove word 2 thru 3 from uiselection" "i   and kate")
(test-it "remove every word from uiselection" "    ")
(test-it "insert \"hi\" at the front in uiselection" "hi    ")
(test-it "insert \"bye\" at the end in uiselection" "hi    bye")

;; TABLES
(run-it "set uiselection to a new table")
(test-it "every item in uiselection" nil)
(test-it "set item red in uiselection to rectangle" sk8::uiselection)
(test-it "item red in uiselection" sk8::rectangle)
(test-it "set item orange in uiselection to oval" sk8::uiselection)
(test-it "set item 1 in uiselection to 666" sk8::uiselection)
(test-it "every item in uiselection" (list 666 sk8::oval sk8::rectangle))
(test-it "set every actor in uiselection to 777" sk8::uiselection)
(test-it "every item in uiselection" (list 666 777 777))
(test-it "set every item that's less than 700 in uiselection to 888" sk8::uiselection)
(test-it "every item in uiselection" (list 888 777 777))
(test-it "item red in uiselection" 777)
(test-it "item 1 in uiselection" 888)
(test-it "position of item red in uiselection" sk8::red)
(test-it "position of every item in uiselection" (list 1 sk8::orange sk8::red))
(test-it "set every item in uiselection to 777" sk8::uiselection)
(test-it "any item in uiselection" 777)
(test-it "any item in uiselection" 777)
(test-it "any item in uiselection" 777)


;; ARRAYS
(run-it "set uiselection to a new array with dimensions {2,2}")
(test-it "every item in uiselection" (list nil nil nil nil))
(test-it "set item {1,1} in uiselection to rectangle" sk8::uiselection)
(test-it "item {1,1} in uiselection" sk8::rectangle)
(test-it "set item {1,1} thru {2,1} in uiselection to oval" sk8::uiselection)
(test-it "every item in uiselection" (list sk8::oval sk8::oval sk8::oval nil))
(test-it "set actor {1,1} thru {2,2} in uiselection to red" sk8::uiselection)
(test-it "every item in uiselection" (list sk8::red sk8::red sk8::red nil))
(test-it "every red in uiselection" (list sk8::red sk8::red sk8::red))
(test-it "position of every item that = false in uiselection" (list (list 2 2)))
(test-it "position of renderer 2 in uiselection" (list 1 2))

;; Vectors
(run-it "set uiselection to a new vector with length 4")
(test-it "every item in uiselection" (list nil nil nil nil))
(test-it "set item 3 in uiselection to rectangle" sk8::uiselection)
(test-it "item 3 in uiselection" sk8::rectangle)
(test-it "set item 2 thru 3 in uiselection to oval" sk8::uiselection)
(test-it "every item in uiselection" (list nil sk8::oval sk8::oval nil))
(test-it "set every actor in uiselection to red" sk8::uiselection)
(test-it "every item in uiselection" (list nil sk8::red sk8::red nil))
(test-it "every red in uiselection" (list sk8::red sk8::red))


t)


;;;_____________________________________________________________________________________________________________
;;;  Command Tests

(progn 

;;Set and Get..
(test-it "get red" sk8::red)
(test-it "get result" sk8::red)
(run-it "set uiselection to a new rectangle")
(test-it "set the fillcolor of uiselection to red" sk8::red)
(test-it "get the fillcolor of uiselection" sk8::red)
(test-it "set the framecolor of uiselection to its fillcolor" sk8::red)
(test-it "get the framecolor of uiselection" sk8::red)
(run-it "set uiselection to {(new rectangle), (new rectangle)}")
(test-it "set the fillcolor of all of uiselection to red" sk8::uiselection)
(test-it "get the fillcolor of all of uiselection" (list sk8::red sk8::red))
(test-it "set the framecolor of all of uiselection to its fillcolor" sk8::uiselection)
(test-it "get the framecolor of all of uiselection" (list sk8::red sk8::red))

;;Unless Do
(test-it "unless the fillcolor of item 1 in uiselection is blue do get 1" 1)
(test-it "unless the fillcolor of item 1 in uiselection is red do get 1" nil)
(test-it "unless the fillcolor of item 1 in uiselection is blue do 
             get 1
          end unless" 1)
(test-it "unless the fillcolor of item 1 in uiselection is red do 
             get 1
          end unless" nil)


;;if 
(test-it "if the fillcolor of item 1 in uiselection is blue then get 1" nil)
(test-it "if the fillcolor of item 1 in uiselection is red then get 1" 1)
(test-it "if the fillcolor of item 1 in uiselection is blue then
              get 1
          end if" nil)
(test-it "if the fillcolor of item 1 in uiselection is red then
              get 1
          end if" 1)
(test-it "if 333<222+1 then
              get 1
          else 
              get 2
          end if" 2)
(test-it "if \"abcd\" contains \"bc\" then
              get 1
          else 
              get 2
          end if" 1)
(test-it "if \"abcd\" starts with \"bc\" then
              get 1
          else get 2" 2)
(test-it "if \"abcd\" starts with \"bc\" then
              get 1
          else if 2 = 1 - -1 then
              get 2
          else get 3" 2)


;; if one of
(test-it "if one of
             fillcolor of rectangle = blue:
              get 1
             fillcolor of actor = white:
              get 2
          end if" 2)
(test-it "if red is one of
             blue:
              get 1
             orange:
              get 2
          end if" nil)
(test-it "if red is one of
             blue:
              get 1
             orange:
              get 2
          else get 3" 3)
(test-it "if the fillcolor of rectangle is one of
             blue:
              get 1
             white:
              get 2
          end if" 2)
(test-it "if {1,2,3} contains one of
             666:
              get 1
             2:
              get 2
          end if" 2)

;;;wait
(test-it "wait 1 tick" nil)
(test-it "wait 0.01 seconds" nil)
(test-it "wait without events 1 tick" nil)
(test-it "wait with events for 0.01 seconds" nil)
(test-it "wait while 1 > 2" nil)
(test-it "wait until 1 < 2" nil)
(test-it "wait with events while 1 > 2" nil)
(test-it "wait without events until 1 < 2" nil)


;;;Repeat
(test-it "set uiselection to false" nil)
(test-it "repeat for 3 times
             insert 666 into uiselection
          end repeat" nil)
(test-it "get uiselection" (list 666 666 666))

(test-it "set uiselection to false" nil)
(test-it "repeat with xxx from 1 to 9 by 2
             insert xxx into uiselection
          end repeat" nil)
(test-it "get uiselection" (list 1 3 5 7 9))

(test-it "set uiselection to false" nil)
(test-it "repeat with xxx from 1 to 9 by 2
             insert xxx into uiselection
             if xxx > 2 then exit repeat
          end repeat" nil)
(test-it "get uiselection" (list 1 3 ))

(test-it "set uiselection to false" nil)
(test-it "repeat with xxx from 1 to 9 by 2
            if xxx = 5 then next repeat
            insert xxx into uiselection
          end repeat" nil)
(test-it "get uiselection" (list 1 3 7 9))

(test-it "set uiselection to false" nil)
(test-it "repeat with xxx in {5,4,3,2,1}
             insert xxx into uiselection
          end repeat" nil)
(test-it "get uiselection" (list 5 4 3 2 1))

(test-it "set uiselection to false" nil)
(test-it "repeat while (the length of uiselection < 2)
             insert 666 into uiselection
          end repeat" nil)
(test-it "get uiselection" (list 666 666))

(test-it "set uiselection to false" nil)
(test-it "repeat until (the length of uiselection >= 2)
             insert 666 into uiselection
          end repeat" nil)
(test-it "get uiselection" (list 666 666))

(test-it "set uiselection to false" nil)
(test-it "repeat
             insert 666 into uiselection
          until uiselection = uiselection" nil)
(test-it "get uiselection" (list 666))


;;;Function, Handler Definition:

(run-it "on testfun of x
            return x + 1
         end testfun")
(test-it "testfun of 1" 2)

(run-it "on testfun of x with increment (a number defaulting to 1) with decrement (a number defaulting to 1)
            return x + increment - decrement
         end testfun")
(test-it "testfun of 1" 1)
(test-it "testfun of 1 with increment 2" 2)
(test-it "testfun of 1 with increment 2 with decrement 8" -5)

(test-it "set uiselection to 2" 2)
(run-it "on testfun of x
            local uiselection (a number defaulting to 1)
            return x + uiselection
         end testfun")
(test-it "testfun of 1" 2)

(test-it "set uiselection to 2" 2)
(run-it "on testfun of x
            local uiselection = 1
            return x + uiselection
         end testfun")
(test-it "testfun of 1" 2)



(run-it "set rect to new rectangle")
(run-it "on simpleHand of me (rect) with inc (defaulting to 1)
            return (length of my contents) + inc
         end simpleHand")
(test-it "simpleHand of rect" 1)

(run-it "set rect2 to new rect")
(run-it "on simpleHand of me (rect2) with inc (defaulting to 1)
            set res to do inherited
            return res + 1
         end simpleHand")
(test-it "simpleHand of rect2" 2)

(run-it "set rect3 to new rect")
(run-it "on simpleHand of me (rect3) with inc (defaulting to 1)
            set res to do inherited of me with inc 2
            return res + 1
         end simpleHand")
(test-it "simpleHand of rect3" 3)



;;;GETTERS AND SETTERS AND DO INHERITED
(test-it "addproperty of rect, 'testProp'" 'sk8::testProp)
(run-it "on testProp of me (rect)
            set val to do inherited
            unless val do set val to 1
            return val + 1
         end testProp")
(test-it "testprop of rect" 2)
(run-it "on testProp of me (rect2)
            set val to do inherited
            if val = 2 then set val to 1
            return val + 1
         end testProp")
(test-it "testprop of rect2" 2)


(test-it "addproperty of rect, 'testProp2'" 'sk8::testProp2)
(run-it "on set testProp2 of me (rect) to newval
            return do inherited
         end set testProp2")
(test-it "set testProp2 of rect to 2" 2)

(run-it "on set testProp2 of me (rect2) with inc to newval
            if inc then set newval to newval + 1
            return do inherited of me to newval
         end set testProp2")
(test-it "set testProp2 of rect2 to 2" 2)
(test-it "testProp2 of rect2" 2)
(test-it "set testProp2 of rect2 with inc to 2" 3)
(test-it "testProp2 of rect2" 3)

(run-it "set rect2Child to new rect2")
(run-it "on set testProp2 of me (rect2Child) with inc to newval
            if inc then set newval to newval + 1
            return do inherited of me with inc inc to newval
         end set testProp2")
(test-it "set testProp2 of rect2Child to 2" 2)
(test-it "testProp2 of rect2Child" 2)
(test-it "set testProp2 of rect2Child with inc to 2" 4)
(test-it "testProp2 of rect2Child" 4)

;;;WITH FUNCTIONS

(test-it "with cursor of watchcursor
             set uiselection to cursor of the stage
          end with" sk8::watchcursor)

(test-it "on with tiny of x
               sendtolog of x
               do body
               cleanup:
                  set uiselection to x - 1
          end with tiny" 'SK8::|WITH TINY|)

(test-it "with tiny of 10
             get 3
             set uiselection to result
          end with" nil)
(test-it "get uiselection" 9)


;;NEED TO IMPLEMENT AND TEST ERROR SIGNALLING!!!!!!!!!!!!!!!!!!!!!*************


t)


t)

)

#|
(pprint 
 (skil-compile-form (second (fifth (sk8::translatescriptcommandorexpr sk8::sk8 
                                                    "with tiny of 10
             set uiselection to 3
             error of \"haha\"
          end with"
                                                     )))))
(pprint (second (fifth (sk8::translatescriptcommandorexpr sk8::sk8 
                                                    "on with tiny of x
               sendtolog of x
               do body
               cleanup:
                  set uiselection to x - 1
          end with tiny"))))
;;(test-it "insert 222 after every actor in {1,rectangle,3,roundrect,oval,5}" (list 1 sk8::Rectangle 222 3  sk8::RoundRect 222 sk8::Oval 222 5))

|#

#|
	Change History (most recent last):
	1  	 4/19/96	Brian   	Moving all tests here.
	2  	 4/22/96	Brian   	Adding command tests.
	4  	 4/30/96	Brian   	mod is now an operator.
	6  	 5/ 3/96	Brian   	more and more tests...
	9  	 7/26/96	Brian   	adding remove a single item from a list with a length of 1.
	10 	 8/ 8/96	Brian   	
	11 	10/14/96	Brian   	do inherited now can be used in setting.
	12 	 2/27/97	Hernan  	
|# ;(do not edit past this line!!)
