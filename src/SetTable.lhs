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


- A Table Implemation using Set -
---------------------------------

> module SetTable (
>   Table(..),
>   SetTable(..),
>   mkTableFromSet,
>   Tab, -- FIXME: Tab needs to be visible in SetTableOps, this sux
>   testSetTable,
> ) where	
>
> import Table
> import Primes
> import Lib.AssertFun
> import qualified Data.Set as Set

The default implementaion of the table class is just a Header an a Set of lits.
It can be used with any type system, but the literals has to be ordered.

> data (Ord l, Literal l t) => SetTable l t = SetTab (TableHeader t) (Set.Set (Row l)) 
>       deriving Eq
> type Tab = (SetTable SimpleLit SimpleType)

> rowSet :: (Ord l, Literal l t) => (SetTable l t) -> (Set.Set (Row l))
> rowSet (SetTab _ rows) = rows

> mkTableFromSet :: (Ord l, Literal l t) => (TableHeader t) -> Set.Set (Row l) -> (SetTable l t)
> mkTableFromSet header rows = SetTab header rows

> instance (Show l, Show t, Ord l, Literal l t) => Table (SetTable l t) l t where
>       mapRowsUnsafe f (SetTab head rows) = (SetTab head (Set.map f rows))
>       filterRows f (SetTab head rows) = (SetTab head (Set.filter f rows))
>       foldRows f i = Set.fold f i . rowSet

>       header (SetTab head _) = head 
>       schema (SetTab head _) = map snd head
>       columnNames (SetTab head _) = map fst head
>       rows (SetTab _ rows) = Set.toList rows
>       checkedTable t
>		| checkTable t = t
>               | otherwise = error ("checkedTable: " ++
>				     "Invalid table:\n" ++
>                                    show t)

Note: mkTable [] [[]] is considered invalid

>       mkTableUnsafe h r = SetTab h (Set.fromList r) 

show and read instance for the table

> showsTable :: (Ord l, Show l, Show t, Literal l t) => (SetTable l t)-> ShowS
> showsTable (SetTab header rows) = ('\n':) . heads . alls (map mapcells (Set.toList rows))
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
>           headlength = (map (\h -> length $ cheads h "") header) ++ repeat 0
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
> readsTable :: (Show l, Show t, Ord l, Read l, Read t, Literal l t) => ReadS (SetTable l t)
> readsTable s =
>       [ (mkTableUnsafe h r, w) | (h,u) <- readsTableHeader s, (r,w) <- readsRows u]
> instance (Show l, Show t, Ord l, Read l, Read t, Literal l t) => Read (SetTable l t) where readsPrec _ = readsTable

-- UnitTesting --
-----------------

sample tables used for testing:

> tableEmpty = mkTable [] [] :: Tab
> tableEmptyS = "\n|\n"

> table1 = mkTable [("ID",Integer), ("Name",String)] [
>       [IntLit 23, StrLit "fb"],
>       [IntLit 42, StrLit "daniel"]  ] :: Tab
> table1S = '\n':
>       "| ID: Integer | Name: String |\n" ++
>       "| 23          | \"fb\"         |\n" ++
>       "| 42          | \"daniel\"     |\n" ++
>       ""

Yes, those two are valid!

> table2 = mkTable [("ID",Integer), ("Name",String)] [
>       [IntLit 23, StrLit "fb"],
>       [Null, StrLit "daniel"]  ] :: Tab
> table2S = '\n':
>       "| ID: Integer | Name: String |\n" ++
>       "| Null        | \"daniel\"     |\n" ++
>       "| 23          | \"fb\"         |\n" ++
>       ""

> table3 = mkTable [("ID",Integer), ("Name",String)] [
>       [Null, StrLit "fb"],
>       [IntLit 42, StrLit "daniel"]  ] :: Tab
> table3S = '\n':
>       "| ID: Integer | Name: String |\n" ++
>       "| Null        | \"fb\"         |\n" ++
>       "| 42          | \"daniel\"     |\n" ++
>       ""

union of table 2 and table 3

> table23 = mkTable [("ID",Integer), ("Name",String)] [
>       [IntLit 23, StrLit "fb"],
>       [Null, StrLit "fb"],
>       [Null, StrLit "daniel"], 
>       [IntLit 42, StrLit "daniel"]  ] :: Tab
> table23S ='\n':
>       "| ID: Integer | Name: String |\n" ++
>       "| 23          | \"fb\"         |\n" ++
>       "| Null        | \"fb\"         |\n" ++
>       "| Null        | \"daniel\"     |\n" ++
>       "| 42          | \"daniel\"     |\n" ++
>       ""

> tableInvalid = mkTableUnsafe [("ID",Integer), ("Name",String)] [
>       [IntLit 23, StrLit "fb"],
>       [CharLit 'a', StrLit "daniel"]  ] :: Tab
> tableInvalidSize = mkTableUnsafe [("ID",Integer), ("Name",String)] [
>       [IntLit 23, StrLit "fb"],
>       [CharLit 'a', StrLit "daniel", Null]  ] :: Tab
> tableInvalidS ='\n':
>       "| ID: Integer | Name: String |\n" ++
>       "| 23          | \"fb\"         |\n" ++
>       "| 'a'         | \"daniel\"     |\n" ++
>       ""

> table123Empty = mkTable [("ID",Integer), ("Name",String)] [] :: Tab
> table123EmptyS = '\n':
>       "| ID: Integer | Name: String |\n" ++
>       ""

> tableLong = mkTable [("i",Integer), ("Name",String)] [
>       [IntLit (-100000000), StrLit "fb"],
>       [IntLit 42, StrLit "Daniel Waeber\nCranachstr. 61\nBerlin"]  ] :: Tab
> tableLongS = '\n':
>       "| i: Integer | Name: String |\n" ++
>       "| -100000000 | \"fb\"         |\n" ++
>       "| 42         | \"Daniel Waeber\\nCranachstr. 61\\nBerlin\" |\n" ++
>       ""

> table1Xtable1 = mkTable
>	[("ID",Integer),("Name",String),("ID",Integer),("Name",String)]
>	[[IntLit 23, StrLit "fb", IntLit 23, StrLit "fb"],
>	 [IntLit 23, StrLit "fb", IntLit 42, StrLit "daniel"],
>	 [IntLit 42, StrLit "daniel", IntLit 23, StrLit "fb"],
>	 [IntLit 42, StrLit "daniel", IntLit 42, StrLit "daniel"]] :: Tab

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
>       [ [], [Integer, String],[Integer, String],
>	  [Integer, String],[Integer, String] ]

> testColumnNames = afun1 columnNames "columNames"
>       [tableEmpty, table1, table2, table3, table123Empty]
>       [ [], ["ID", "Name"], ["ID", "Name"], ["ID", "Name"],
>	  ["ID", "Name"] ]

show instance needed for unit testing ...

--- Basic Functions ---
-----------------------

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
>	(id, table1Xtable1, table1Xtable1),
>       ((\((IntLit n):xs) -> (IntLit (n+5)):xs), table1, 
>           read "| ID: Integer | Name: String || 28 | \"fb\" || 47  | \"daniel\" |"),
>       ((\(x:(StrLit s):xs) -> x:(StrLit (s++"-")):xs), table1, 
>           read "| ID: Integer | Name: String || 23 | \"fb-\" || 42  | \"daniel-\" |")
>	]

> testFilterRows = assertfun2 filterRows "filterRows" [
>	((\_ -> True), tableEmpty, tableEmpty),
>	((\_ -> True), table1, table1),
>	((\_ -> False), table1, read "| ID: Integer | Name: String |"),
>       (((IntLit 23 ==) . head), table1, read "| ID: Integer | Name: String || 23 | \"fb\" |"),
>       (((IntLit 42 ==) . head), table1, read "| ID: Integer | Name: String || 42 | \"daniel\" |"),
>       (((Null /=) . head), table2, read "| ID: Integer | Name: String || 23 | \"fb\" |")
>	]

> testGetValue = assertfun3 getValue "getValue" [
>       (table1, (rows table1)!!0, "ID", IntLit 23),
>       (table1, (rows table1)!!1, "ID", IntLit 42),
>       (table1, (rows table1)!!0, "Name", StrLit "fb")
>       ]

> testSetTable = testRead && testShow && testCheckTable && testSchema && testColumnNames 
>	&& testAnyRow && testAllRows && testSize && testMapRows && testFilterRows && testGetValue
