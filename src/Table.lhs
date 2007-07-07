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
>   Table(Tab),
>   Row,
>   ColumnName,
>   TableHeader,
>   mkTable,
>   mkTableFromSet,
>   mkTableLazy,
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

> type Row = [SimpleLit]
> type ColumnName = String
> type ColumnHeader = (ColumnName, SimpleType)
> type TableHeader = [ColumnHeader]
>
> data Table = Tab TableHeader (Set.Set Row) 
>       deriving (Eq)

> mkTable :: TableHeader -> [Row] -> Table
> mkTable header rows = if checkTable tab then tab
>      else error "the table contains invalid values"
>      where tab = mkTableLazy header rows

> mkTableFromSet :: TableHeader -> Set.Set Row -> Table
> mkTableFromSet header rows = Tab header rows

creates a Table without checking the schema. This allows to create
infinite leazy tables ...

> mkTableLazy :: TableHeader -> [Row] -> Table
> mkTableLazy header rows = Tab header (Set.fromList rows)

Note: mkTable [] [[]] is considered invalid

> checkTable :: Table -> Bool
> checkTable (Tab [] rows) = Set.null rows
> checkTable (Tab heads rows) = ctypes && clength
>       where 
>           types = map snd heads
>           size = length heads
>           ctypes = Set.fold ((==) . all (uncurry checkType) . zip types) True rows
>           clength = Set.fold ((==) . (size ==) . length) True rows

getters for Table ADT

> schema :: Table -> [SimpleType]
> schema (Tab header _) = map (\(_,t) -> t) header

> columNames :: Table -> [ColumnName]
> columNames (Tab header _) = map (\(name,_) -> name) header

show and read instance for the table

> showsTable :: Table -> ShowS
> showsTable (Tab header rows) = heads . alls (map mapcells (Set.toList rows))
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
>           headlength = (map (\h -> length $ cheads h " ") header) ++ repeat 13
>
>           mapcells :: (Show a) => [a] -> [ShowS]                  -- row -> [ShowS]
>           mapcells = map (\(n,s) -> ss (take n $ (show s) ++ (repeat ' '))) . zip headlength
>           lines :: [ShowS] -> ShowS
>           lines = folds (cs .)
>           alls :: [[ShowS]] -> ShowS
>           alls = folds ((. ls) . lines)
> instance Show Table where showsPrec _ = showsTable

> readsColumnHeader :: ReadS ColumnHeader
> readsColumnHeader s =
>       [ ((n,t),w) | (n,u) <- lex s, (":",v) <- lex u, (t,w) <- reads v ]
> readsTableHeader :: ReadS TableHeader
> readsTableHeader s =
>       [ (l:r,w) | ("|", u) <- lex s, (l, v) <- readsColumnHeader u,(r, w) <- readsTableHeader v] ++
>       [ ([],u) | ("|",u) <- lex s, ("|",v) <- lex u] ++
>       [ ([],u) | ("|",u) <- lex s, ("",v) <- lex u] ++
>       [ ([],'|':u) | ("||",u) <- lex s]
> readsRow :: ReadS Row
> readsRow s = 
>       [ (l:r,w) | ("|", u) <- lex s, (l, v) <- reads u,(r, w) <- readsRow v] ++
>       [ ([],u) | ("|",u) <- lex s, ("|",v) <- lex u] ++
>       [ ([],u) | ("|",u) <- lex s, ("",v) <- lex u] ++
>       [ ([],'|':u) | ("||",u) <- lex s]
> readsRows :: ReadS [Row]
> readsRows s = 
>       [ (r:rs , v ) | (r,u) <- readsRow s, (rs,v) <- readsRows u ] ++
>       [ ([],u) | ("",u) <- lex s]
> readsTable :: ReadS Table
> readsTable s =
>       [ (mkTable h r, w) | (h,u) <- readsTableHeader s, (r,w) <- readsRows u]
> instance Read Table where readsPrec _ = readsTable

-- UnitTesting --
------------------

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

> tableInvalid = mkTableLazy [("ID",Number), ("Name",String)] [
>       [IntLit 23, StrLit "fb"],
>       [CharLit 'a', StrLit "daniel"]  ]

> table123Empty = mkTable [("ID",Number), ("Name",String)] []


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

> testColumnNames = afun1 columNames "columNames"
>       [tableEmpty, table1, table2, table3, table123Empty]
>       [ [], ["ID", "Name"], ["ID", "Name"], ["ID", "Name"],
>	  ["ID", "Name"] ]


--- Putting it all together(tm) ---
-----------------------------------

> testTable = testCheckTable && testSchema && testColumnNames


--- Legal Foo ---
-----------------

TODO: print Note on startup?

> licenseNote = "rel-eval \tCopyright (C) 2007 \tDaniel Waeber, Fabian Bieker\n" ++
>       "This program comes with ABSOLUTELY NO WARRANTY; for details " ++
>       "read the LICENSE.txt file.\n" ++
>       "This is free software, and you are welcome to " ++
>       "redistribute it under certain conditions; type LGPL.txt for " ++
>       "details.\n"
