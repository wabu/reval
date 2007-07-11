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

> module Table (
>   -- TODO: hide stuff, write getters
>   Row,
>   ColumnName,
>   ColumnHeader,
>   TableHeader,
>   Tab,
>   Table(..),
>   SetTable(..),
>   mkTableFromSet,
>   testTable,
> )
> where
> 
> import Primes
> import Lib.AssertFun
> import qualified Data.Set as Set


--- The Table ---
-----------------
Now Rows and Tables are easy, as we just can use a Set of List. We only have to
check the types when createing or changeing a Table at runtime.

> type Row l = [l]
> type ColumnName = String
> type ColumnHeader t = (ColumnName, t)
> type TableHeader t = [ColumnHeader t]

The class table consists of the table data tab, and the type system l t, which
if functional dependent on tab.

Note: To Implement this class you need to implement either rows or
	foldRows, mkTableUnsafe and the basic getters. Basic getters
	are header, schema and columnNames.
	We recommend implementing more functions if you care about
	performance.

> class (Type t, Literal l t, Eq tab) => Table tab l t | tab -> l t where

Constructors and schema checking:

>       checkTable :: tab -> Bool
>       checkTable t = allRows validRow t && checkSize
>		where
>		sch = schema t
>		validRow :: (Row l) -> Bool
>		validRow r = all (uncurry checkType) (zip sch r)
>		checkSize = allRows (\r -> length sch == length r) t
>
>       mkTableUnsave :: (TableHeader t) -> [(Row l)] -> tab
>
>       mkTable :: (TableHeader t) -> [(Row l)] -> tab
>       mkTable h r = checkedTable (mkTableUnsave h r)
>          where checkedTable t = if checkTable t 
>                                 then t 
>                                 else error "invalid table"

Getters:

>       header :: tab -> TableHeader t
>       schema :: tab -> [t]
>       columnNames :: tab -> [String]
>       rows :: tab -> [Row l]
>	rows = foldRows (:) []

basic functional programming higher order functions:
Note: default impl. of foldRow is really slow, should be overwriten.

>       foldRows :: ((Row l) -> b -> b) -> b -> tab -> b
>	foldRows f init = foldr f init . rows 
>       mapRows :: ((Row l) -> (Row l)) -> tab -> tab
>	mapRows f t = foldRows
>	 	(\row table -> cons (f row) table) 
>		(mkTable (header t) [])
>		t
>       filterRows :: ((Row l) -> Bool) -> tab -> tab
>	filterRows f t = foldRows
>	 	(\row table -> if f row then cons row table else table) 
>		(mkTable (header t) [])
>		t

Basic list-like operations:

>	cons :: (Row l) -> tab -> tab
>	cons r t = mkTable (header t) (r : rows t) 
>	isEmpty :: tab -> Bool
>	isEmpty = null . rows

Size returns (count of columns, count of rows)

>	size :: tab -> (Int,Int)
>	size t = (length (schema t), length (rows t))

Basic logic operations:

>       allRows :: ((Row l) -> Bool) -> tab -> Bool
>       allRows f = foldRows ((&&) . f) True
>       anyRow :: ((Row l) -> Bool) -> tab -> Bool
>       anyRow f = foldRows ((||) . f) False

--- A Table Implemation using Set ---
-------------------------------------

The default implementaion of the table is just a Header an a Set of lits. It
can be used with any type system, but the literals has to be ordered.

> data (Ord l, Literal l t) => SetTable l t = SetTab (TableHeader t) (Set.Set (Row l)) 
>       deriving Eq
> type Tab = (SetTable SimpleLit SimpleType)

> rowSet :: (Ord l, Literal l t) => (SetTable l t) -> (Set.Set (Row l))
> rowSet (SetTab _ rows) = rows

> mkTableFromSet :: (Ord l, Literal l t) => (TableHeader t) -> Set.Set (Row l) -> (SetTable l t)
> mkTableFromSet header rows = SetTab header rows

> instance (Ord l, Literal l t) => Table (SetTable l t) l t where
>       mapRows f (SetTab head rows) = (SetTab head (Set.map f rows))
>       filterRows f (SetTab head rows) = (SetTab head (Set.filter f rows))
>       foldRows f i = Set.fold f i . rowSet

>       header (SetTab head _) = head 
>       schema (SetTab head _) = map snd head
>       columnNames (SetTab head _) = map fst head
>       rows (SetTab _ rows) = Set.toList rows

Note: mkTable [] [[]] is considered invalid

>       mkTableUnsave h r = SetTab h (Set.fromList r) 

show and read instance for the table

> showsTable :: (Ord l, Show l, Show t, Literal l t) => (SetTable l t)-> ShowS
> showsTable (SetTab header rows) = heads . alls (map mapcells (Set.toList rows))
>       where 
>           space = (' ':)            -- space ShowS
>           col = ('|':) . space      -- column sperator
>           line = ('|':) . ('\n':)   -- line
>           strings = (++)            -- string -> ShowS
>
>           folds :: (a -> ShowS) -> [a] -> ShowS
>           folds fs = foldr ((.) . fs) id
>
>           cheads (n, t) = (strings n) . (':':) . space . (shows t)    -- column header -> [ShowS]
>           heads = folds ((col .) . (. space) . cheads) header . line  -- | cheader | cheader ... |\n
>           headlength = (map (\h -> length $ cheads h "") header)
>
>           mapcells :: (Show a) => [a] -> [ShowS]                      -- row -> [ShowS]
>           mapcells = map 
>                   (\(n,s) -> strings (take (1 + max n (length $ show s)) (show s ++ repeat ' ')))
>                   . zip headlength
>           lines :: [ShowS] -> ShowS
>           lines = folds (col .)
>           alls :: [[ShowS]] -> ShowS
>           alls = folds ((. line) . lines)
> instance (Ord l, Show l, Show t, Literal l t) => Show (SetTable l t) where showsPrec _ = showsTable

> readsColumnHeader :: (Read t) => ReadS (ColumnHeader t)
> readsColumnHeader s =
>       [ ((n,t),w) | (n,u) <- lex s, (":",v) <- lex u, (t,w) <- reads v ]
> readsTableHeader :: (Read t) => ReadS (TableHeader t)
> readsTableHeader s =
>       [ (l:r,w) | ("|", u) <- lex s, (l, v) <- readsColumnHeader u,(r, w) <- readsTableHeader v] ++
>       [ ([],u) | ("|",u) <- lex s, ("|",v) <- lex u] ++
>       [ ([],u) | ("|",u) <- lex s, ("",v) <- lex u] ++
>       [ ([],'|':u) | ("||",u) <- lex s]
> readsRow :: (Read l) => ReadS (Row l)
> readsRow s = 
>       [ (l:r,w) | ("|", u) <- lex s, (l, v) <- reads u,(r, w) <- readsRow v] ++
>       [ ([],u) | ("|",u) <- lex s, ("|",v) <- lex u] ++
>       [ ([],u) | ("|",u) <- lex s, ("",v) <- lex u] ++
>       [ ([],'|':u) | ("||",u) <- lex s]
> readsRows :: (Read l) => ReadS [Row l]
> readsRows s = 
>       [ (r:rs , v ) | (r,u) <- readsRow s, (rs,v) <- readsRows u ] ++
>       [ ([],u) | ("",u) <- lex s]
> readsTable :: (Ord l, Read l, Read t, Literal l t) => ReadS (SetTable l t)
> readsTable s =
>       [ (mkTableUnsave h r, w) | (h,u) <- readsTableHeader s, (r,w) <- readsRows u]
> instance (Ord l, Read l, Read t, Literal l t) => Read (SetTable l t) where readsPrec _ = readsTable

-- UnitTesting --
------------------

sample tables used for testing:

> tableEmpty = mkTable [] [] :: Tab
> tableEmptyS = "|\n"

> table1 = mkTable [("ID",Number), ("Name",String)] [
>       [IntLit 23, StrLit "fb"],
>       [IntLit 42, StrLit "daniel"]  ] :: Tab
> table1S = 
>       "| ID: Number | Name: String |\n" ++
>       "| 23         | \"fb\"         |\n" ++
>       "| 42         | \"daniel\"     |\n" ++
>       ""

Yes, those two are valid!

> table2 = mkTable [("ID",Number), ("Name",String)] [
>       [IntLit 23, StrLit "fb"],
>       [Null, StrLit "daniel"]  ] :: Tab
> table2S = 
>       "| ID: Number | Name: String |\n" ++
>       "| Null       | \"daniel\"     |\n" ++
>       "| 23         | \"fb\"         |\n" ++
>       ""

> table3 = mkTable [("ID",Number), ("Name",String)] [
>       [Null, StrLit "fb"],
>       [IntLit 42, StrLit "daniel"]  ] :: Tab
> table3S = 
>       "| ID: Number | Name: String |\n" ++
>       "| Null       | \"fb\"         |\n" ++
>       "| 42         | \"daniel\"     |\n" ++
>       ""

union of table 2 and table 3

> table23 = mkTable [("ID",Number), ("Name",String)] [
>       [IntLit 23, StrLit "fb"],
>       [Null, StrLit "fb"],
>       [Null, StrLit "daniel"], 
>       [IntLit 42, StrLit "daniel"]  ] :: Tab
> table23S =
>       "| ID: Number | Name: String |\n" ++
>       "| 23         | \"fb\"         |\n" ++
>       "| Null       | \"fb\"         |\n" ++
>       "| Null       | \"daniel\"     |\n" ++
>       "| 42         | \"daniel\"     |\n" ++
>       ""

> tableInvalid = mkTableUnsave [("ID",Number), ("Name",String)] [
>       [IntLit 23, StrLit "fb"],
>       [CharLit 'a', StrLit "daniel"]  ] :: Tab
> tableInvalidSize = mkTableUnsave [("ID",Number), ("Name",String)] [
>       [IntLit 23, StrLit "fb"],
>       [CharLit 'a', StrLit "daniel", Null]  ] :: Tab
> tableInvalidS =
>       "| ID: Number | Name: String |\n" ++
>       "| 23         | \"fb\"         |\n" ++
>       "| 'a'        | \"daniel\"     |\n" ++
>       ""

> table123Empty = mkTable [("ID",Number), ("Name",String)] [] :: Tab
> table123EmptyS = 
>       "| ID: Number | Name: String |\n" ++
>       ""

> tableLong = mkTable [("i",Number), ("Name",String)] [
>       [IntLit (-100000000), StrLit "fb"],
>       [IntLit 42, StrLit "Daniel Waeber\nCranachstr. 61\nBerlin"]  ] :: Tab
> tableLongS = 
>       "| i: Number | Name: String |\n" ++
>       "| -100000000 | \"fb\"         |\n" ++
>       "| 42        | \"Daniel Waeber\\nCranachstr. 61\\nBerlin\" |\n" ++
>       ""

--- Read Show Test ---
----------------------

> testShow = afun1 show "show"
>   [tableEmpty, table1, table2, table3, tableInvalid, table123Empty, tableLong]
>   [tableEmptyS,table1S,table2S,table3S,tableInvalidS,table123EmptyS,tableLongS]
> testRead = afun1 read "read"
>   [tableEmptyS,table1S,table2S,table3S,tableInvalidS,table123EmptyS,tableLongS]
>   [tableEmpty, table1, table2, table3, tableInvalid, table123Empty, tableLong]

--- CheckTable Unit Test ---
----------------------------

> testCheckTable = afun1 checkTable "checktab"
>       [tableEmpty, table1, tableInvalid, tableInvalidSize, table2, table3]
>       [True, True, False, False, True, True]

--- Getters for Table ADT ---
-----------------------------

> testSchema = afun1 schema "schema"
>       [tableEmpty, table1, table2, table3, table123Empty]
>       [ [], [Number, String],[Number, String],
>	  [Number, String],[Number, String] ]

> testColumnNames = afun1 columnNames "columNames"
>       [tableEmpty, table1, table2, table3, table123Empty]
>       [ [], ["ID", "Name"], ["ID", "Name"], ["ID", "Name"],
>	  ["ID", "Name"] ]

FIXME: more unit tests! mapRows, filterRows, size, ...

show instance needed for unit testing ...

> instance Show ((Row r) -> Bool)  where
>	show _ = "(\\(Row r) -> Bool)"
> instance Show ((Row r) -> (Row r))  where
>	show _ = "(\\(Row r) -> (Row r))"

> testAnyRow = assertfun2 anyRow "anyRow" [
>	((\_ -> True), tableEmpty, False), -- by def
>	((\_ -> False), tableEmpty, False),
>	((\_ -> False), table123Empty, False),
>	((\_ -> True), table123Empty, False),
>	((\_ -> False), table2, False),
>	((\_ -> True), table2, True),
>	((\(x:_) -> x == Null), table2, True),
>	((\(x:_) -> x /= read "2342"), table2, True),
>	((\(x:_) -> x == read "2342"), table2, False),
>	((\(_:x:_) -> length (show x) > 10), table2, False),
>	((\(_:x:_) -> length (show x) > 1), table2, True)
>	]

> testAllRows = assertfun2 allRows "allRows" [
>	((\_ -> True), tableEmpty, True), -- by def
>	((\_ -> False), tableEmpty, True),
>	((\_ -> False), table123Empty, True),
>	((\_ -> True), table123Empty, True),
>	((\_ -> False), table2, False),
>	((\_ -> True), table2, True),
>	((\(x:_) -> x == Null), table2, False),
>	((\(x:_) -> x /= Null), table2, False),
>	((\(x:_) -> x /= read "2342"), table2, True),
>	((\(x:_) -> x == read "2342"), table2, False),
>	((\(_:x:_) -> length (show x) > 10), table2, False),
>	((\(_:x:_) -> length (show x) > 1), table2, True)
>	]

> table1Xtable1 = mkTable
>	[("ID",Number),("Name",String),("ID",Number),("Name",String)]
>	[[IntLit 23, StrLit "fb", IntLit 23, StrLit "fb"],
>	 [IntLit 23, StrLit "fb", IntLit 42, StrLit "daniel"],
>	 [IntLit 42, StrLit "daniel", IntLit 23, StrLit "fb"],
>	 [IntLit 42, StrLit "daniel", IntLit 42, StrLit "daniel"]]

> testSize = assertfun1 size "size" [
>	(tableEmpty, (0,0)),
>	(table123Empty, (2,0)),
>	(table2, (2,2)),
>	(table3, (2,2)),
>	(table1Xtable1, (4,4))
>	]

> testMapRows = assertfun2 mapRows "mapRows" [
>	(id, tableEmpty, tableEmpty),
>	(id, table2, table2),
>	(id, table1Xtable1, table1Xtable1)
>	-- TODO: more tests
>	]

> testFilterRows = assertfun2 filterRows "filterRows" [
>	((\_ -> True), tableEmpty, tableEmpty)
>	]

> testTable = testRead && testShow && testCheckTable && testSchema && testColumnNames 
>	&& testAnyRow && testAllRows && testSize && testMapRows && testFilterRows
