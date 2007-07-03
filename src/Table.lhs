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


- The Table Type -
------------------

Or how to trick Haskell to get a Table with different data types inside each
column, but with arbitrary length.

> module Table where
> 
> import Lib.AssertFun
> import qualified Data.Set as Set




-- Our First Try --
-------------------

... in tricking the type system. The type class look good, but still
we got a problem: There is no way to instantiate it.

> {- the class looks good, but can be instanciated
>
> class (Show a) => Row a where
>       name :: a -> String
>       content :: (Show b, Ord b) => a -> [b]
>
> --type Table = (Row a) => [a]
>
> newtype StringRow = SRow (String, [String])
>       deriving (Show, Eq, Read)
>
> instance Row StringRow where
>       name (SRow (n,_))  = n
>       name _ = error "StringRow.name"
>       content (SRow (_,c)) = c
>       content _ = error "StringRow.content"
> -}



-- The Second Try --
--------------------

The idea was to create a TableType as a LISP-like cons-cell to represent Tables.
Trying to ignore the type of the second argument, so we can put a TableType d e
did not work, as the type system cam back as we tried to write a on TableType. The
compile will construct the type for the function, but that's an infinite type, so
BOOOMM!! as soon as we use the type.

> {- cant be used either
>
> data TableType a b = Nil | Tab (Row a) b
>        deriving (Show, Eq, Read)
>
>
> names Nil = [] 
> names (Tab (name, _ ) b) = names b -- INFINITE TYPE! BOOM!!
> names _ = error "StringRow.name"
> -}



-- Better: Use ASTs --
----------------------

Well, screw it, just use ASTs

--- Basic AST Types ---
-----------------------
Lit is a Literal.
TODO: impl. own Show and Read ...

> data Lit = Null | IntLit Int | StrLit String | CharLit Char |
>       BoolLit Bool
>       deriving (Eq)

Type is used to store Type information in the table schema.
Note: Null has Type Any.

> data Type = Any | Number | String | Char | Bool
>       deriving (Show, Eq, Read)

Now we can just use Lists as rows, as all data just has the type Lit.

Ord for Lit needed to stuff Lits in Sets

> instance Ord Lit where
>       compare Null Null = EQ
>       compare (IntLit a) (IntLit b) = compare a b 
>       compare (StrLit a) (StrLit b) = compare a b 
>       compare (CharLit a) (CharLit b) = compare a b 
>       compare (BoolLit a) (BoolLit b) = compare a b 
>       -- just to keep it consitent
>       compare Null _ = LT
>       compare _ Null = GT
>       compare (IntLit _) _ = LT
>       compare _ (IntLit _) = GT
>       compare (StrLit _) _ = LT
>       compare _ (StrLit _) = GT
>       compare (CharLit _) _ = LT
>       compare _ (CharLit _) = GT

Own Show and Read for Lits

> showsLit :: Lit -> ShowS
> showsLit Null = ("Null" ++)
> showsLit (IntLit l) = shows l
> showsLit (StrLit l) = shows l
> showsLit (CharLit l) = shows l
> showsLit (BoolLit l) = shows l
> instance Show Lit where showsPrec _ = showsLit

> readsLit :: ReadS Lit
> readsLit s = [(Null,s) | ("Null", s) <- lex s ] ++
>              [(IntLit l, s) | (l,s) <- reads s ] ++
>              [(StrLit l, s) | (l,s) <- reads s ] ++
>              [(CharLit l, s) | (l,s) <- reads s ] ++
>              [(BoolLit l, s) | (l,s) <- reads s ]
> instance Read Lit where readsPrec _ = readsLit

check it two types are compatilbe

> checkType :: Type -> Type -> Bool
> checkType Any _ = True
> checkType _ Any = True
> checkType Number Number = True
> checkType String String = True
> checkType Char Char = True
> checkType Bool Bool = True
> checkType _ _ = False

get type information

> getLitType :: Lit -> Type
> getLitType Null = Any
> getLitType (IntLit _) = Number
> getLitType (StrLit _) = String
> getLitType (CharLit _) = Char
> getLitType (BoolLit _) = Bool

check for right type

> checkLitType :: Type -> Lit -> Bool
> checkLitType t l = checkType t $ getLitType l

compare the Type of two Lits

> cmpLitType :: Lit -> Lit -> Bool
> cmpLitType Null Null = True
> cmpLitType (IntLit _) (IntLit _) = True
> cmpLitType (StrLit _) (StrLit _) = True
> cmpLitType (CharLit _) (CharLit _) = True
> cmpLitType (BoolLit _) (BoolLit _) = True
> cmpLitType _ _ = False


--- The Table ---
-----------------
Now Rows and Tables are easy, as we just can use a Set of List. We only have to
check the types when createing or changeing a Table at runtime.

> type Row = [Lit]
> type ColumName = String
> type ColumHeader = (ColumName, Type)
> type TableHeader = [ColumHeader]
>
> data Table = Tab TableHeader (Set.Set Row) 
>       deriving (Show, Eq) -- , Read)

FIXME:
 =>  No instance for (Read (Data.Set.Set Row))
     arising from the 'deriving' clause of a data type declaration at Table.lhs:127:6
     Probable fix: add an instance declaration for (Read (Data.Set.Set Row))
     When deriving the `Read' instance for type `Table'

TODO: impl. own Show and Read ...

> readsRow s = 
>       [ (l:r,w) | ("|", u) <- lex s, (l, v) <- readsLit u,(r, w) <- readsRow v] ++
>       [ ([],u) | ("|",u) <- lex s, ("|",v) <- lex u] ++
>       [ ([],u) | ("|",u) <- lex s, ("",v) <- lex u] ++
>       [ ([],'|':u) | ("||",u) <- lex s]
> readsRows s = 
>       [ (r:rs , v ) | (r,u) <- readsRow s, (rs,v) <- readsRows u ] ++
>       [ ([],u) | ("",u) <- lex s]


> mkTable :: TableHeader -> [Row] -> Table
> mkTable header rows = Tab header (Set.fromList rows)

> mkTableFromSet :: TableHeader -> Set.Set Row -> Table
> mkTableFromSet header rows = Tab header rows

TODO: error will fuck up unit test :(
      and infinite Tables won't be possible if we check them first :(

>-- mkTable header rows = if checkTable tab then tab
>--       else error "the table contains invalid valuse"
>--       where tab = 

Note: mkTable [] [[]] is considered invalid

> checkTable :: Table -> Bool
> checkTable (Tab [] rows) = Set.null rows
> checkTable (Tab heads rows) = ctypes && clength
>       where 
>           types = map snd heads
>           size = length heads
>           ctypes = Set.fold ((==) . all (uncurry checkLitType) . zip types) True rows
>           clength = Set.fold ((==) . (size ==) . length) True rows

getters for Table ADT

> schema :: Table -> [Type]
> schema (Tab header _) = map (\(_,t) -> t) header

> columNames :: Table -> [ColumName]
> columNames (Tab header _) = map (\(name,_) -> name) header

-- Primitive Relation Algebra Operations --
-------------------------------------------

applyOnTableSets is a abstract higer order function used to
build union, difference etc. It boils down to "Building
Abstraction by Functions" as outlined in SICP.

applyOnTableSets only do some base sanity checks and relies
mostly on mkTable to create only valid tables.
The reason for this is, that I (fb) want it to be leazy
(as in operate on infinite tables).

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

TODO: intersection untested (fb's fault)

> intersection :: Table -> Table -> Table
> intersection = applyOnTableSets Set.intersection

> select :: (Row -> Bool) -> Table -> Table
> select p (Tab head rows) = mkTableFromSet head (Set.filter p rows)

FIXME: is there a diffrence in a Set {{}} and {} in relation algebra?
	'cause if so, the above code, is incorrect!
	s. http://en.wikipedia.org/wiki/Relational_algebra

Projection

TODO: cleanup this mess, 2 testcases fail ...
TODO: optimize! this is insanley slow :-(
TODO: return Maybe Table to handle cases, when projection is invalid?

> project :: [ColumName] -> Table -> Table
> project [] (Tab schema _) = mkTable schema [] 
> project wantedNames (Tab header rows) =
> 	mkTable newHeader newRows
>	where
>	names = [n | (n,_) <- header]

posList is a List of postions needed to do the projection

>	posList :: [Int]
>	posList = map (`pos` names) wantedNames
>	newHeader = [header!!i | i <- posList]
>	newRows = map (\row -> [row!!i | i <- posList ]) (Set.toList rows)

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

 > project projectedNames (Tab header rows) =
 >	map (filter (\(name,_) -> name `elem` projectedNames) . (\(n,v) -> v)) namedRows
 >	where 
 >	(names,_) = header 
 >	namedRows = map (zip names) rows

TODO: impl. $foo type class to have ops like + and - 
	as table union and diffrence?

-- UnitTesting --
------------------

--- Type System ---
-------------------

> int = IntLit 32
> str = StrLit "wabu"
> chr = CharLit '_'
> bool = BoolLit True
> null = Null
> types = [Number, String, Char, Bool]
> lits  = [int, str, chr, bool]

> testCheckTypeEq = assertfun2 checkType "checkType"
>       ( [(a,a,True) | a <- types] ++
>         [(a,Any,True) | a <- types] ++
>         [(a,Bool,False) | a <- [String,Number,Char]] ++
>         [(a,b,checkType b a) | a <- types, b <- types] ++
>         [(Any,Any,True)] )

> testCheckLitType = assertfun2 checkLitType "checkLitType"
>       ( [(t,l,True) | (t,l) <- zip types lits] ++
>         [(t,l,False) | i <- [1..4], l <- drop i lits, t <- take i types] ++
>         [(t,l,False) | i <- [1..4], l <- take i lits, t <- drop i types] ++
>         [(Any,l,True) | l <- lits] )

> testCheckCmpType = assertfun2 cmpLitType "cmpLitType"
>       ( [(a,b,True) | (a,b) <- zip lits lits] ++
>         [(a,b,False) | i <- [1..4], a <- drop i lits, b <- take i lits] ++
>         [(a,b,False) | i <- [1..4], a <- take i lits, b <- drop i lits] ++
>         [(a,b,cmpLitType b a) | a <- lits, b <- lits] ++
>         [(Null,l,False) | l <- lits] ++ [(Null,Null,True)] )

> testCheckTypes = testCheckTypeEq && testCheckLitType && testCheckCmpType


--- Table ---
-------------

sample tables used for testing:

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

> tableInvalid = mkTable [("ID",Number), ("Name",String)] [
>       [IntLit 23, StrLit "fb"],
>       [CharLit 'a', StrLit "daniel"]  ]

unit test of checkTable

> testCheckTable = afun1 checkTable "checktab"
>       [tableEmpty, table1, tableInvalid, table2, table3]
>       [True, True, False, True, True]

--- Getters for Table ADT ---
-----------------------------

> testSchema = afun1 schema "schema"
>       [tableEmpty, table1, table2, table3, table123Empty]
>       [ [], [Number, String],[Number, String],
>	  [Number, String],[Number, String] ]

> testColumNames = afun1 columNames "columNames"
>       [tableEmpty, table1, table2, table3, table123Empty]
>       [ [], ["ID", "Name"], ["ID", "Name"], ["ID", "Name"],
>	  ["ID", "Name"] ]


--- Primitive Relational Algebra Operatations ---
-------------------------------------------------

TODO: unit tests are way to coupled on test tables!

Format of assertfun2 is:
assertfun f name_of_f [ (param1, param2, expected) ]

empty table with schema of table1,2,3 .

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

just to be able to unit test the predicates in select...

> instance Show (a -> b) where
> 	show _ = "(\a -> b) :: (a -> b)"

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
>       [StrLit "daniel"], 
>	[Null] ]

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
>	  -- TODO: add more tests
>	]

TODO: tests 2 + 11 fail

> testCross = True

--- Putting it all together(tm) ---
-----------------------------------

> testAll = testCheckTypes && testCheckTable
>	&& testSchema && testColumNames
>	&& testUnion && testDifference
>	&& testSelect && testProject
>	&& testCross


--- Legal Foo ---
-----------------

TODO: print Note on startup?

> licenseNote = "rel-eval \tCopyright (C) 2007 \tDaniel Waeber, Fabian Bieker\n" ++
>       "This program comes with ABSOLUTELY NO WARRANTY; for details " ++
>       "read the LICENSE.txt file.\n" ++
>       "This is free software, and you are welcome to " ++
>       "redistribute it under certain conditions; type LGPL.txt for " ++
>       "details.\n"
