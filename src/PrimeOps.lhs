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
>   union,
>   difference,
>   intersection,
>   cross,
>   project,
>   select,
>   testPrimeOps,
>   (|||),
>   (\\\),
>   (&&&),
>   (***),
> )
> where
>
> import Primes
> import Table
> import Lib.AssertFun
> import qualified Data.Set as Set

-- Primitive Relation Algebra Operations --
-------------------------------------------

applyOnTableSets is a abstract higer order function used to
build union, difference etc. It boils down to "Building
Abstraction by Functions" as outlined in SICP.

applyOnTableSets only do some base sanity checks and relies
mostly on mkTable to create only valid tables.
The reason for this is, that I (fb) want it to be leazy
(as in operate on infinite tables).

TODO: rename

> applyOnTableSets :: (Ord l, Literal l t) =>
>       (Set.Set (Row l) -> Set.Set (Row l) -> Set.Set (Row l)) 
>       -> (SetTable l t) -> (SetTable l t) -> (SetTable l t)
> applyOnTableSets _ t1@(SetTab [] rows) t2 = 
>	if Set.null rows then
>		t2
>	else
>		error ("applyOnTableSets: Invalid table: " -- ++ show t2
>			++ "schema is empty but rows are not!" ) 
> applyOnTableSets f t1 t2@(SetTab [] rows) = applyOnTableSets f t2 t1
> applyOnTableSets f (SetTab head1 rows1) (SetTab _ rows2) =
>	mkTableFromSet head1 (f rows1 rows2)

> union :: (Ord l, Literal l t) => (SetTable l t) -> (SetTable l t) -> (SetTable l t)
> union = applyOnTableSets Set.union

> difference :: (Ord l, Literal l t) => (SetTable l t) -> (SetTable l t) -> (SetTable l t)
> difference = applyOnTableSets Set.difference

> intersection :: (Ord l, Literal l t) => (SetTable l t) -> (SetTable l t) -> (SetTable l t)
> intersection = applyOnTableSets Set.intersection

cross-join

cross table1 table2 -- looks good, imho...

TOOD: optimize, Set.toList sux!

> cross :: (Ord l, Literal l t) => (SetTable l t) -> (SetTable l t) -> (SetTable l t)
> cross (SetTab h1 r1) (SetTab h2 r2) =
>	mkTable newHeader [ x++y | x <- l1, y <- l2]
>	where
>	newHeader = h1 ++ h2
>	l1 = Set.toList r1
>	l2 = Set.toList r2

> select :: (Ord l, Literal l t) => ((Row l) -> Bool) -> (SetTable l t) -> (SetTable l t)
> select p (SetTab head rows) = mkTableFromSet head (Set.filter p rows)

FIXME: is there a diffrence in a Set {{}} and {} in relation algebra?
	'cause if so, the above code, is incorrect!
	s. http://en.wikipedia.org/wiki/Relational_algebra

Projection

TODO: optimize! this is insanley slow :-(
TODO: return Maybe Table to handle cases, when projection is invalid?

> project :: (Ord l, Literal l t) => [ColumnName] -> (SetTable l t) -> (SetTable l t)
> project [] (SetTab _ _) = mkTable [] [] 
> project wantedNames (SetTab header rows) =
> 	SetTab newHeader newRows
>	where
>	names = [n | (n,_) <- header]

posList is a List of postions needed to do the projection

>	posList :: [Int]
>	posList = map (`pos` names) wantedNames
>	newHeader = [header!!i | i <- posList]
>	newRows = Set.map (\row -> [row!!i | i <- posList ]) rows

find postion of an elemt in a List

> pos :: (Eq a, Show a) => a -> [a] -> Int
> pos e xs = pos' xs 0
>	where
>	pos' [] _ = error ("project: pos: not found: " ++ show e)
>	pos' (x:xs) i =
>		if e == x then 
>			i
>		else
>			pos' xs (i + 1)

syntatic sugar

> (&&&) :: (Ord l, Literal l t) => (SetTable l t) -> (SetTable l t) -> (SetTable l t)
> (&&&) = intersection
> (|||) :: (Ord l, Literal l t) => (SetTable l t) -> (SetTable l t) -> (SetTable l t)
> (|||) = union
> (***) :: (Ord l, Literal l t) => (SetTable l t) -> (SetTable l t) -> (SetTable l t)
> (***) = cross
> (\\\) :: (Ord l, Literal l t) => (SetTable l t) -> (SetTable l t) -> (SetTable l t)
> (\\\) = difference

-- UnitTesting --
------------------

TODO: unit tests are way to coupled on test tables!

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

> testIntersection = assertfun2 intersection "intersection"
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
>		[])
>	  -- TODO:
>	  -- (table1, table1, XXX),
>	  -- (table2, table2, XXX),
>	  -- (table3, table3, XXX),
>	  -- (table23, table3, XXX),
>	  -- (table23, table2, XXX)
>	]


> testPrimeOps = testUnion && testDifference && testIntersection
>       && testSelect && testProject
>       && testCross

