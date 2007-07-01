* $Header: /home/bude/bieker/RAID/cvsroot/studium/ALP3/uebung07/assertfun.lhs,v 1.1 2005/11/30 16:06:53 bieker Exp $
*
* AssertFun 
* A Haskell module for generic function testing
*
* Copyright (C) 2005  Fabian Bieker 
*
* AssertFun comes with ABSOLUTELY NO WARRANTY, 
* use at your own risk!
* see GPL.txt or http://www.gnu.org/licenses/gpl.txt
* for details.
* 
*     This program is free software; you can redistribute
*     it and/or modify it under the terms of the 
*     GNU General Public License as published by the Free
*     Software Foundation; either version 2 of the
*     License, or any later version.
* 
*     This program is distributed in the hope that it will
*     be useful, but WITHOUT ANY WARRANTY; without even
*     the implied warranty of MERCHANTABILITY or FITNESS
*     FOR A PARTICULAR PURPOSE.
*     See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General
*     Public License along with this program;
*     if not, write to the
*     Free Software Foundation, Inc., 51 Franklin St,
*       Fifth Floor, Boston, MA  02110-1301  USA
*
* Contact: Fabian Bieker - fabian dot bieker at web dot de
*
* GPL License: 
*     see GPL.txt or http://www.gnu.org/licenses/gpl.txt
*
*********************************************************
*
* You don't like it? -> Send me a patch (s. Contact)
*
*********************************************************
*
* Changelog:
*
* * 05.11.05, v0.1.11:
*	- added GPL header
*
* * 05.11.05, v0.1.10:
*	- added documentation patch from Uwe Kamper 
*	- more tests
*	- changed paramter format of assertfun1
*
*********************************************************

> module Lib.AssertFun (assertfun1, assertfun2, assertfun3,
>                   afun1)
> where
> import Prelude

AssertFun provides asserts for simple generic function
testing. Using these asserts you can evaluate if a 
function you wrote really returns the desired values.
Here is one example: Image that you programmed a function
that takes one parameter and returns a value. To test that
 function you can use "assertfun1".

This is the function we would like to test. This is a very
simple one, that returns the squared input value.

*AsserFun> let foo n = n * n

To test this function we use assertfun1 to test 3 distinct
test cases.

*AssertFun> assertfun1 foo "foo" [(1,1), (2,4), (4,16)]
True

The function name in double quotes is used for the error
message.

Note: The second formal paramter of assertfun* should be
	the functionname of the tested function.
        (Getting it at runtime is still on the TODO-List)

The values in the list are the testcases, conisting of a
n-tupel of the paramter(s) and an expected result for
this testcase.
Each test case is written as follows:

[(paramter, expected), (parameter2, expected2), ... ]

assertfun1 will return "True" if all the 3 testcases
evaluate to the desired values. If any of the tests fails,
assertfun1 will throw an exception cantainig a detailed
error message.

*AssertFun> assertfun1 foo "foo" [(1,1), (2,4), (4,17)]
*** Exception: assertfun: foo testcase(s) failed:
        testcase 3 - foo 4 == 16, expected 17
testcases failed: 1/3

In this example assertfun1 throws an exception because
"foo 4" returns the value 16 (and not 17 as stated in
the example above).

*********************************************************

Reference:

assertfun1 tests functions with one formal parameter
assertfun2 tests functions with 2 formal paramters,
  by uncurrying the tested function 
assertfun3 tests functions with 3 formal paramters
  by uncurrying the tested function

*********************************************************

Examples:

Let's test the not function with 2 testcases:
not True == False
not False == True

*AssertFun> assertfun1 not "not"
                [(True, False), (False, True)]
True

If a tests fails an expection is thrown:

*AssertFun> assertfun1 not "not"
                [(True, False), (False, False)]
*** Exception: assertfun: not testcase(s) failed:
        testcase 2 - not False == True, expected False
testcases failed: 1/2


afun1 just has a diffrent paramter format:

*AssertFun> afun1 not "not" [True,False] [False,True]
True

*AssertFun> afun1 not "not" [True,False] [False,False]
*** Exception: assertfun: not testcase(s) failed:
        testcase 2 - not False == True, expected False
testcases failed: 1/2


testing a function with 2 formal paramters:

*AssertFun> assertfun2 (-) "(-)" [(2,1,1), (1,1,0)]
True

*AssertFun> assertfun2 (-) "(-)"
                [(2,1,1), (1,1,0), (1,2,(-5))]
*** Exception: assertfun: (-) testcase(s) failed:
        testcase 3 - (-) (1,2) == -1, expected -5
testcases failed: 1/3


s. end of file for more examples

*********************************************************

Key Concept of this Module is the function afun1:

afun1 :: (Show a, Show b, Eq b) =>
		(a -> b) -> String -> [a] -> [b] -> Bool

afun1 f name testcases expected
	// ...
	result == expected || error (makeErrorMsg ...)
	// ...
	where
	result = map f testcases

Thats it. The rest of the code handels error massage
formating and paramter exctraction.

*********************************************************

FIXME: howto test function with n formal paramters
       generate a String etc. of Haskell Code and eval s?
       How can i do (apply #'foo '(1 2 3 4)) in Haskell?

FIXME: handle Show problem with functions (s. end of file)

TODO: how can i get the function name of f?

*********************************************************

> assertfun f = assertfun2 f -- FIXME

assertfun for 3 paramter functions
expand paramter list xs, "uncurry" f, call afun1

> assertfun3 :: (Show a, Show b, Show c, Show r, Eq r) =>
>              (a -> b -> c -> r) -> String
>                                 -> [(a, b, c, r)]
>                                 -> Bool

> assertfun3 f name xs
>	= afun1 (\(x,y,z) -> f x y z)
>               name testcases expected
>	where
>	testcases = map (\(x,y,z,_) -> (x,y,z)) xs
>	expected  = map (\(_,_,_,e) -> e)       xs


assertfun for 2 paramter functions
expand paramter list xs, uncurry f, call afun1

> assertfun2 :: (Show a, Show b, Eq b, Show c, Eq c) =>
>		(a -> b -> c) -> String -> [(a,b,c)]
>                                -> Bool
> assertfun2 f name xs
>	= afun1 (uncurry f) name testcases expected
>	where
>	testcases = map (\(x,y,_) -> (x,y)) xs
>	expected  = map (\(_,_,z) -> z)     xs


assertfun for 2 paramter functions
expand paramter list xs, call afun1

> assertfun1 :: (Show a, Show b, Eq b) =>
>		(a -> b) -> String -> [(a,b)] -> Bool
>
> assertfun1 f name xs
>	= afun1 f name testcases expected
>	where
>	testcases = map (\(x,_) -> x) xs
>	expected  = map (\(_,y) -> y) xs

helper function for assertfun* ,
runs the tests and checks the result.
real work done here.

> afun1 :: (Show a, Show b, Eq b) =>
>		(a -> b) -> String -> [a] -> [b] -> Bool
>
> afun1 f name testcases expected
>	| length result == length expected =
>		result == expected ||
>			error (makeErrorMsg name testcases 
>                              result expected)
>	| otherwise =
>           error ("assertfun: length of testcases !="
>                  ++ " length of expceted")
>	where
>	result = map f testcases

generate a nicly formated error msg

> makeErrorMsg :: (Eq a, Show a, Show b) =>
>	String -> [b] -> [a] -> [a] -> String
> makeErrorMsg name testcases result expected =
>	"assertfun: " ++ name ++ " testcase(s) failed:\n"
>		++ (concat . (map showDiff)) ldiff
>		++ "testcases failed: "
>               ++ show (length ldiff) ++ "/"
>		++ show (length testcases)
>	where
>	ldiff = listdiff result expected
>	-- TODO: extraced testcases!!pos to uncurried
>       --       notation if needed
>	showDiff (x,y,pos) = "\ttestcase "
>               ++ show (pos + 1)
>		++  " - " ++ name ++ " "
>               ++ show (testcases!!pos)
>		++ " == " ++ show x
>		++ ", expected " ++ show y ++ "\n" 
>	

where is xs diffrent from ys?
returns diffrent elements and their posiston in xs/ys .
Assumes: length xs == length ys

> listdiff :: (Eq a, Show a) =>
>             [a] -> [a] -> [(a, a, Int)]
> listdiff xs ys = ld 0 xs ys
>	where
>	ld _ [] [] = []
>	ld _ [] lst = error ("listdiff: second list to"
>			 ++" long :" ++ show lst )
>	ld _ lst [] = error ("listdiff: first list to"
>			 ++" long :" ++ show lst )
>	ld pos (x:xs) (y:ys) 
>		| x == y	= ld (pos + 1) xs ys
>		| otherwise	=
>                   (x,y,pos) : ld (pos + 1) xs ys

*********************************************************

Problem: Functions are not instances of Show class
		-> can't show them
		-> can't call show to show testcases etc.

*Main> assertfun42 map "map"
        [ ((+2),[1,2,3]), ((*2),[1,2,3]) ]
	[[3,4,5],[2,4,6]]
True
*Main> assertfun map "map"
        [ ((+2),[1,2,3]), ((*2),[1,2,3]) ]
	[[3,4,5],[2,4,6]]
error: ... No instance for (Show (a -> a)) ...

> assertfun42 f name testcases expected =
>	result == expected || error errormsg
>	where
>	result = map (uncurry f) testcases
>	errormsg =
>           "assertfun42: test " ++ name ++ " failed!"


---------------------------------------------------------
		MODULE TESTING
	run testAll to run all tests
---------------------------------------------------------
TODO: more (less self depend) tests

Note: some of these test have recursive depenencies...

> testListDiff = assertfun2 listdiff "listdiff" [ 
>	([], [],	     []), 
>	([1,2,3], [1,2,3],   []), 
>	([1,2,3], [0,2,3],   [(1,0,0)]),
>	([1,2,3], [1,0,3],   [(2,0,1)]),
>	([1,2,3], [1,2,0],   [(3,0,2)]),
>	([1,2,3], [0,0,3],   [(1,0,0), (2,0,1)]),
>	([1,2,3], [1,0,0],   [(2,0,1), (3,0,2)]),
>	([1,2,3], [0,0,0],   [(1,0,0), (2,0,1), (3,0,2)])
>	]

> testAFun1 = afun1 (+2) "(+2)" [] []
>	&& afun1 (+2) "(+2)" [1] [3]
>	&& afun1 not "not" [True,False] [False,True]
>	&& afun1 (+2) "(+2)" [1,2,3,4,5,6] [3,4,5,6,7,8]

> testAssertFun1 = assertfun1 not "not" []
>	&& assertfun1 not "not"
>              [(True,False), (False, True)]
>	&& assertfun1 (+2) "(+2)"
>              [(1,3), (2,4), (4,6), (6,8)]

> testAssertFun2 = assertfun2 (-) "(-)"
>              [(2,1,1), (1,1,0), (1,2,(-1))]

> testAssertFun3
>	= assertfun3 foo "foo" [(1,2,3,2), (5,6,10,9)]
>	where
>	foo x y z = x - y + z

> testAll = testListDiff && testAFun1 && testAssertFun1 
>		&& testAssertFun2 && testAssertFun3
