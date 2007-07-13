- rel-eval is a leazy relational algebra interpreter written in haskell. -

  Copyright (C) 2007  Daniel Waeber, Fabian Bieker
  
  This program is free software: you can redistribute it and/or modify
  it under the terms of the GNU Lesser General Public License as 
  published by the Free Software Foundation, version 3 of the License.
  
  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.
  
  You should have received a copy of the GNU Lesser General Public License
  along with this program.  If not, see <http://www.gnu.org/licenses/>.

> module PrimeOps (
>   TableOps,
>   testPrimeOps,
> )
> where
>
> import Primes
> import Table
> import Lib.AssertFun
> import qualified Data.Set as Set
> import qualified Data.List as List
> import Data.List ((\\))


-- Class to encapsulte Primitive Relation Algebra Operations --
---------------------------------------------------------------

TODO: simplify class/type constraints

> class (Table tab l t, Type t, Literal l t, Eq tab, Eq l, Show t)
>	=> TableOps tab l t | tab -> l t where

basic set operations

> 	union :: tab -> tab -> tab
>	union = applyOnTableRows List.union
>	difference :: tab -> tab -> tab
>	difference = applyOnTableRows (\\)
>	intersect :: tab -> tab -> tab
>	intersect = applyOnTableRows List.intersect

cross-join
TODO: default impl.

>	cross :: tab -> tab -> tab

basic Relational Algebra Operations 

TODO: default impl.

> 	select :: ((Row l) -> Bool) -> tab -> tab
> 	project :: [ColumnName] -> tab -> tab

TODO: default impl.

> 	renameUnsafe :: [(ColumnName,ColumnName)] -> tab -> tab
> 	rename ::[(ColumnName,ColumnName)] -> tab -> tab

syntatic sugar

>	(&&&) :: tab -> tab -> tab
>	(&&&) = intersect
>	(|||) :: tab -> tab -> tab
> 	(|||) = union
>	(***) :: tab -> tab -> tab
> 	(***) = cross
>	(\\\) :: tab -> tab -> tab
> 	(\\\) = difference

Higher order function to apply f on the rows of two tables.
Checks if the headers of the table match, if not error is raised.
This is usefull to impl. union, intersection etc.

>	applyOnTableRows :: ([Row l] -> [Row l] -> [Row l]) -> tab -> tab -> tab
> 	applyOnTableRows f t1 t2
>		| h1 == h2 = mkTable h1 (f r1 r2)
>		| otherwise = error ("applyOnTableRows: table headers"
>			++ "do not match: " ++
>			show h1 ++ " /= " ++ show h2)
>		where
>		h1 = header t1
>		h2 = header t2
>		r1 = rows t1
>		r2 = rows t2


-- Primitive Relation Algebra Operations --
-------------------------------------------

applyOnTableSets is a abstract higer order function used to
build union, difference etc. It boils down to "Building
Abstraction by Functions" as outlined in SICP.

applyOnTableSets only do some base sanity checks and relies
mostly on mkTable to create only valid tables.
The reason for this is, that I (fb) want it to be leazy
(as in operate on infinite tables).

TODO: more sanity checking

> applyOnTableSets :: (Show l, Show t, Ord l, Literal l t) =>
>       (Set.Set (Row l) -> Set.Set (Row l) -> Set.Set (Row l)) 
>       -> (SetTable l t) -> (SetTable l t) -> (SetTable l t)
> applyOnTableSets _ t1@(SetTab [] rows) t2 = 
>	if Set.null rows then
>		t2
>	else
>		error ("applyOnTableSets: Invalid table: " ++ show t2
>			++ "schema is empty but rows are not!" ) 
> applyOnTableSets f t1 t2@(SetTab [] rows) = applyOnTableSets f t2 t1
> applyOnTableSets f (SetTab head1 rows1) (SetTab head2 rows2) =
>       if head1 == head2 then
>	    mkTableFromSet head1 (f rows1 rows2)
>       else
>           error ("applyOnTableSets: table headers do not match: "
>		++ show head1 ++ " != " ++ show head2)

> instance (Show l, Show t, Ord l, Literal l t) => TableOps (SetTable l t) l t where
>	union = applyOnTableSets Set.union
> 	difference = applyOnTableSets Set.difference
>	intersect = applyOnTableSets Set.intersection

cross-join

cross table1 table2 -- looks good, imho...

TODO: optimize, Set.toList sux!

> 	cross (SetTab h1 r1) (SetTab h2 r2) =
>		mkTable newHeader [ x++y | x <- l1, y <- l2]
>		where
>		newHeader = h1 ++ h2
>		l1 = Set.toList r1
>		l2 = Set.toList r2

Note: A Table containing a Set like {{}} is considured invalid. Use a
Table containg the empty Set {} (aka mkTable [] []) to represent the
empty table. This might not be formaly correct, but having two
representations of the empty table is not programtic and makes code
more complex and less readable.

> 	select p (SetTab head rows) = mkTableFromSet head
>		(Set.filter p rows)

Projection

TODO: projectUnsafe?
TODO: optimize! this is insanley slow :-(

>	project wantedNames tab@(SetTab header rows) = if checkHeader
>		then SetTab newHeader newRows
>		-- FIXME: this is a really bad errormsg - open questions: why? how did i get here?
>           	else error "you cheat: this is not possible!"
>   		where
>       	checkHeader = all (\n -> checkSize n (length [a | (a,_) <- header, n == a])) wantedNames
>       	checkSize :: String -> Int -> Bool
>       	checkSize n s 
>               	| s == 0 = error ("project: could not project to " 
>                   	    ++ show n ++ ": name unknown.")
>               	| s == 1 = True
>               	| otherwise = error ("project: could not project to " 
>                       	++ show n ++ ": name is ambigious.")
>       	posList = [i | s <- wantedNames, (i,(n,_)) <- zip [1..] header, n==s ]
>      		newHeader = [h | p <- posList, (i,h) <- zip [1..] header, p==i ]
>      		newRows = Set.map 
>	                (\r -> [ l | p <- posList, (i,l) <- zip [1..] r, p==i] )
>               	rows

Renaming

Syntax: rename{Unsafe} [(oldName, newName)] table

> 	renameUnsafe names tab@(SetTab sch _) = mkTable (replaceNames sch) (rows tab)
>		where
>		replaceNames = map (getNewName names)
>		getNewName [] header = header
>		getNewName ((old,new):xs) h@(name, ctype) = if name == old
>			then (new, ctype)
>			else getNewName xs h

> 	rename names tab@(SetTab schema _) = if checkParams
>		then renameUnsafe names tab
>		else error ("invalid rename: names = " ++ show names 
>			++ "\n table schema = " ++ show schema)
>		where
>		checkParams = null (oldNames \\ schemaNames)
>		(oldNames,_) = unzip names
>		(schemaNames, _) = unzip schema

-- UnitTesting --
------------------

Format of assertfun2 is:
assertfun f name_of_f [ (param1, param2, expected) ]

empty table with schema of table1,2,3 .


> tableEmpty = mkTable [] [] :: Tab

> table1 = mkTable [("ID",Number), ("Name",String)] [
>       [IntLit 23, StrLit "fb"],
>       [IntLit 42, StrLit "daniel"]  ] :: Tab

Yes, those two are valid!

> table2 = mkTable [("ID",Number), ("Name",String)] [
>       [IntLit 23, StrLit "fb"],
>       [Null, StrLit "daniel"]  ] :: Tab
>
> table3 = mkTable [("ID",Number), ("Name",String)] [
>       [Null, StrLit "fb"],
>       [IntLit 42, StrLit "daniel"]  ] :: Tab

union of table 2 and table 3

> table23 = mkTable [("ID",Number), ("Name",String)] [
>       [IntLit 23, StrLit "fb"],
>       [Null, StrLit "fb"],
>       [Null, StrLit "daniel"], 
>       [IntLit 42, StrLit "daniel"]  ] :: Tab

> tableInvalid = mkTableUnsave [("ID",Number), ("Name",String)] [
>       [IntLit 23, StrLit "fb"],
>       [CharLit 'a', StrLit "daniel"]  ] :: Tab

> table123Empty = mkTable [("ID",Number), ("Name",String)] [] :: Tab

> testUnion = assertfun2 union "union" 
>	[ (tableEmpty, tableEmpty, tableEmpty),
>	  (table123Empty, table1, table1),
>	  (table1, table123Empty, table1),
>	  (table123Empty, table2, table2),
>	  (table2, table123Empty, table2),
>	  (table1, table1, table1),
>	  (table2, table2, table2),
>	  (table3, table3, table3),
>	  (table2, table3, table23),
>	  (table3, table2, table23)
>	]

> testDifference = assertfun2 difference "difference"
>	[ (tableEmpty, tableEmpty, tableEmpty),
>	  (table123Empty, table1, table123Empty),
>	  (table1, table123Empty, table1),
>	  (table123Empty, table2, table123Empty),
>	  (table2, table123Empty, table2),
>	  (table1, table1, table123Empty),
>	  (table2, table2, table123Empty),
>	  (table3, table3, table123Empty),
>	  (table23, table3, table2),
>	  (table23, table2, table3)
>	]

> testIntersect = assertfun2 intersect "intersect"
> 	[ (tableEmpty, tableEmpty, tableEmpty),
> 	  (table123Empty, table1, table123Empty),
> 	  (table1, table123Empty, table123Empty),
> 	  (table123Empty, table2, table123Empty),
> 	  (table2, table123Empty, table123Empty),
> 	  (table1, table1, table1),
> 	  (table2, table2, table2),
> 	  (table3, table3, table3),
> 	  (table23, table3, table3),
> 	  (table23, table2, table2)
> 	]


selections on table2

> table2onlyFB = mkTable [("ID",Number), ("Name",String)] [
>       [IntLit 23, StrLit "fb"] ] :: Tab

> testSelect = assertfun2 select "select"
>	[ ( (\_ -> False), tableEmpty, tableEmpty),
>	  ( (\_ -> False), table1, table123Empty),
>	  ( (\_ -> False), table23, table123Empty),
>	  ( (\_ -> True), tableEmpty, tableEmpty),
>	  ( (\_ -> True), table23, table23),
>	  ( (\x -> x!!0 == IntLit 64), table2, table123Empty),
>	  ( (\x -> x!!0 == IntLit 23), table2, table2onlyFB)
>	]

projections on table23

> table23ID = mkTable [("ID",Number)] [
>       [IntLit 23],
>       [Null],
>       [IntLit 42] ]  :: Tab
>
> table23Name = mkTable [("Name",String)] [
>       [StrLit "fb"],
>       [StrLit "daniel"] ] :: Tab

> testProject = assertfun2 project "project"
>	[ ([], tableEmpty, tableEmpty),
>	  ([], table123Empty, tableEmpty),
>	  (["ID"], table123Empty, mkTable [("ID", Number)] []),
>	  (["Name"], table123Empty, mkTable [("Name", String)] []),
>	  (["ID", "Name"], table123Empty, mkTable
>		[("ID", Number), ("Name", String)] []),
>	  (["Name", "ID"], table123Empty, mkTable
>		[("Name", String), ("ID", Number)] []),
>	  (["ID", "Name"], table1, table1),
>	  (["ID", "Name"], table2, table2),
>	  (["ID", "Name"], table23, table23),
>	  (["ID"], table23, table23ID),	
>	  (["Name"], table23, table23Name)
>	]

> table1Xtable1 = mkTable
>	[("ID",Number),("Name",String),("ID",Number),("Name",String)]
>	[[IntLit 23, StrLit "fb", IntLit 23, StrLit "fb"],
>	 [IntLit 23, StrLit "fb", IntLit 42, StrLit "daniel"],
>	 [IntLit 42, StrLit "daniel", IntLit 23, StrLit "fb"],
>	 [IntLit 42, StrLit "daniel", IntLit 42, StrLit "daniel"]]  :: Tab

> testCross = assertfun2 cross "cross"
>	[ (tableEmpty, tableEmpty, tableEmpty),
>	  (table123Empty, table1, SetTab [("ID",Number), ("Name",String),
>				       ("ID",Number),("Name",String)]
>		                       (Set.fromList [])),
>	  (table1, table123Empty, SetTab [("ID",Number), ("Name",String),
>				       ("ID",Number),("Name",String)]
>		                       (Set.fromList [])),

>	  (table1, table1, table1Xtable1),
>	  (table123Empty, table123Empty, mkTable
>		[("ID",Number), ("Name",String), ("ID",Number),
>		 ("Name",String)]
>		[]),
>	  (table2, table2, read
>		("| ID: Number | Name: String | ID: Number | Name: String |" ++
> 		"| Null       | \"daniel\"     | Null       | \"daniel\"     |" ++
>		"| Null       | \"daniel\"     | 23         | \"fb\"         |" ++
>		"| 23         | \"fb\"         | Null       | \"daniel\"     |" ++
>		"| 23         | \"fb\"         | 23         | \"fb\"         |")) 
>	]

> testRename = assertfun2 rename "rename" [ 
>	([], tableEmpty, tableEmpty),
>	([], table1, table1),
>	([], table123Empty, table123Empty),
>	([("ID", "FOO")], table123Empty,
>		read "| FOO: Number | Name: String |" :: Tab),
>	([("Name", "FOO")], table123Empty,
>		read "| ID: Number | FOO: String |" :: Tab),
>	([("ID", "BAR"), ("Name", "FOO")], table123Empty,
>		read "| BAR: Number | FOO: String |" :: Tab),
>	([("Name", "FOO"), ("ID", "BAR")], table123Empty,
>		read "| BAR: Number | FOO: String |" :: Tab),
>	([("Name", "FOO"), ("ID", "BAR")], table1,
>		read (     "| BAR: Number | FOO: String |"
>			++ "| 23          | \"fb\"      |"
>			++ "| 42          | \"daniel\"  |") :: Tab) 
>	]

> testPrimeOps = testUnion && testDifference && testIntersect
>       && testSelect && testProject
>       && testCross && testRename

