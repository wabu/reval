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

> class (Type t, Ord l, Literal l t, Eq tab) => Table tab l t | tab -> l t where
>       header :: tab -> TableHeader t
>       schema :: tab -> [t]
>       columnNames :: tab -> [String]
>       rows :: tab -> [Row l]
>
>       foldRows :: ((Row l) -> b -> b) -> b -> tab -> b
>       mapRows :: ((Row l) -> (Row l)) -> tab -> tab
>	-- TODO: impl. filterRows? map + filter would be nice to have
>       -- filterRows :: ((Row l) -> (Row l)) -> tab -> tab
>       allRows :: ((Row l) -> Bool) -> tab -> Bool
>       allRows f = foldRows ((==) . f) True
>       anyRow :: ((Row l) -> Bool) -> tab -> Bool
>       anyRow f = foldRows ((||) . f) False
>
>       checkTable :: tab -> Bool
>       mkTableUnsave :: (TableHeader t) -> [(Row l)] -> tab
>       mkTable :: (TableHeader t) -> [(Row l)] -> tab
>       mkTable h r = checkedTable (mkTableUnsave h r)
>          where checkedTable t = if checkTable t 
>                                 then t 
>                                 else error "invalid table"

> data (Ord l, Literal l t) => SetTable l t = SetTab (TableHeader t) (Set.Set (Row l)) 
>       deriving Eq

> type Tab = (SetTable SimpleLit SimpleType)

> rowSet :: (Ord l, Literal l t) => (SetTable l t) -> (Set.Set (Row l))
> rowSet (SetTab _ rows) = rows

> mkTableFromSet :: (Ord l, Literal l t) => (TableHeader t) -> Set.Set (Row l) -> (SetTable l t)
> mkTableFromSet header rows = SetTab header rows

> instance (Ord l, Literal l t) => Table (SetTable l t) l t where
>       mapRows f (SetTab head rows) = (SetTab head (Set.map f rows))
>       foldRows f i = Set.fold f i . rowSet

>       header (SetTab head _) = head 
>       schema (SetTab head _) = map snd head
>       columnNames (SetTab head _) = map fst head
>       rows (SetTab _ rows) = Set.toList rows

Note: mkTable [] [[]] is considered invalid

>       mkTableUnsave h r = SetTab h (Set.fromList r) 

>       checkTable (SetTab [] rows) = Set.null rows
>       checkTable tab = clength && ctypes
>           where 
>               size = length (header tab)
>               clength = allRows ((size ==) . length) tab
>               ctypes = allRows (all (uncurry checkType) . zip (schema tab)) tab


show and read instance for the table

> showsTable :: (Ord l, Show l, Show t, Literal l t) => (SetTable l t)-> ShowS
> showsTable (SetTab header rows) = heads . alls (map mapcells (Set.toList rows))
>       where 
>           sp = (' ':)             -- space ShowS
>           cs = ('|':) . sp        -- column sperator
>           ls = ('|':) . ('\n':)   -- line
>           ss = (++)               -- string -> ShowS
>
>           folds :: (a -> ShowS) -> [a] -> ShowS
>           folds fs = foldr ((.) . fs) id
>
>           cheads (n, t) = (ss n) . (':':) . sp . (shows t)        -- column header -> [ShowS]
>           heads = folds ((cs .) . (. sp) . cheads) header . ls    -- | cheader | cheader ... |\n
>           headlength = (map (\h -> length $ cheads h "") header)
>
>           mapcells :: (Show a) => [a] -> [ShowS]                  -- row -> [ShowS]
>           mapcells = map 
>                   (\(n,s) -> ((take (1 + max n (length $ show s)) (show s ++ repeat ' '))++))
>                   . zip headlength
>           lines :: [ShowS] -> ShowS
>           lines = folds (cs .)
>           alls :: [[ShowS]] -> ShowS
>           alls = folds ((. ls) . lines)
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

>
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
>       [tableEmpty, table1, tableInvalid, table2, table3]
>       [True, True, False, True, True]

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

FIXME: more unit tests! mapRows, allRows ...

show instance needed for unit testing ...

> instance Show ((Row r) -> Bool)  where
>	show _ = "(\\(Row r) -> Bool)"

> testAnyRow = assertfun2 anyRow "anyRow" [
>	((\x -> True), tableEmpty, False), -- by def
>	((\x -> False), tableEmpty, False),
>	((\x -> False), table2, False),
>	((\x -> True), table2, True),
>	((\(x:xs) -> x == Null), table2, True),
>	((\(x:xs) -> x == (IntLit 2342)), table2, False)
>	-- TODO: add more ...
>	]


--- Putting it all together(tm) ---
-----------------------------------

> testTable = testRead && testShow && testCheckTable && testSchema && testColumnNames 
>	&& testAnyRow
