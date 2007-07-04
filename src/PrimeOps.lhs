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

> applyOnTableSets :: (Set.Set Row -> Set.Set Row -> Set.Set Row) -> Table -> Table -> Table
> applyOnTableSets _ t1@(Tab [] rows) t2 = 
>	if Set.null rows then
>		t2
>	else
>		error ("applyOnTableSets: Invalid table: " ++ show t2
>			++ "schema is empty but rows are not!" ) 
> applyOnTableSets f t1 t2@(Tab [] rows) = applyOnTableSets f t2 t1
> applyOnTableSets f (Tab head1 rows1) (Tab _ rows2) =
>	mkTableFromSet head1 (f rows1 rows2)

> union :: Table -> Table -> Table
> union = applyOnTableSets Set.union

> difference :: Table -> Table -> Table
> difference = applyOnTableSets Set.difference

> intersection :: Table -> Table -> Table
> intersection = applyOnTableSets Set.intersection

cross-join

cross table1 table2 -- looks good, imho...

TOOD: optimize, Set.toList sux!

> cross :: Table -> Table -> Table
> cross (Tab h1 r1) (Tab h2 r2) =
>	mkTable newHeader [ x++y | x <- l1, y <- l2]
>	where
>	newHeader = h1 ++ h2
>	l1 = Set.toList r1
>	l2 = Set.toList r2

> select :: (Row -> Bool) -> Table -> Table
> select p (Tab head rows) = mkTableFromSet head (Set.filter p rows)

FIXME: is there a diffrence in a Set {{}} and {} in relation algebra?
	'cause if so, the above code, is incorrect!
	s. http://en.wikipedia.org/wiki/Relational_algebra

Projection

TODO: optimize! this is insanley slow :-(
TODO: return Maybe Table to handle cases, when projection is invalid?

> project :: [ColumnName] -> Table -> Table
> project [] (Tab _ _) = mkTable [] [] 
> project wantedNames (Tab header rows) =
> 	Tab newHeader newRows
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

> (&&&) = intersection
> (|||) = union
> (***) = cross
> (\\\) = difference

-- UnitTesting --
------------------

TODO: unit tests are way to coupled on test tables!

Format of assertfun2 is:
assertfun f name_of_f [ (param1, param2, expected) ]

empty table with schema of table1,2,3 .


> tableEmpty = mkTable [] []

> table1 = mkTable [("ID",Number), ("Name",String)] [
>       [IntLit 23, StrLit "fb"],
>       [IntLit 42, StrLit "daniel"]  ]

Yes, those two are valid!

> table2 = mkTable [("ID",Number), ("Name",String)] [
>       [IntLit 23, StrLit "fb"],
>       [Null, StrLit "daniel"]  ]
>
> table3 = mkTable [("ID",Number), ("Name",String)] [
>       [Null, StrLit "fb"],
>       [IntLit 42, StrLit "daniel"]  ]

union of table 2 and table 3

> table23 = mkTable [("ID",Number), ("Name",String)] [
>       [IntLit 23, StrLit "fb"],
>       [Null, StrLit "fb"],
>       [Null, StrLit "daniel"], 
>       [IntLit 42, StrLit "daniel"]  ]

> tableInvalid = mkTableLazy [("ID",Number), ("Name",String)] [
>       [IntLit 23, StrLit "fb"],
>       [CharLit 'a', StrLit "daniel"]  ]

> table123Empty = mkTable [("ID",Number), ("Name",String)] []

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
>	[ (tableEmpty, tableEmpty, tableEmpty),
>	  (table123Empty, table1, table123Empty),
>	  (table1, table123Empty, table123Empty),
>	  (table123Empty, table2, table123Empty),
>	  (table2, table123Empty, table123Empty),
>	  (table1, table1, table1),
>	  (table2, table2, table2),
>	  (table3, table3, table3),
>	  (table23, table3, table3),
>	  (table23, table2, table2)
>	]


just to be able to unit test the predicates in select...

> instance Show (a -> b) where
> 	show _ = "(\\a -> b) :: (a -> b)"

selections on table2

> table2onlyFB = mkTable [("ID",Number), ("Name",String)] [
>       [IntLit 23, StrLit "fb"] ]

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
>       [IntLit 42] ] 
>
> table23Name = mkTable [("Name",String)] [
>       [StrLit "fb"],
>       [StrLit "daniel"] ]

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
>	 [IntLit 42, StrLit "daniel", IntLit 42, StrLit "daniel"]] 

> testCross = assertfun2 cross "cross"
>	[ (tableEmpty, tableEmpty, tableEmpty),
>	  (table123Empty, table1, Tab [("ID",Number), ("Name",String),
>				       ("ID",Number),("Name",String)]
>		                       (Set.fromList [])),
>	  (table1, table123Empty, Tab [("ID",Number), ("Name",String),
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

