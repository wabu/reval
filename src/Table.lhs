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
>   Table(..),
>   SimpleTable,
>   Row,
>   TableHeader,
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

> type Row l t = [l]
> type ColumnName = String
> type ColumnHeader l t = (ColumnName, t)
> type TableHeader l t = [ColumnHeader l t]

> class (Ord l, Literal l t) => Table table l t where
>       header :: table -> TableHeader l t
>       schema :: table -> [t]
>       columnNames :: table -> [String]
>       rows :: table -> [Row l t]
>
>       foldTable :: (l -> b -> b) -> b -> table -> b
>       mapTable :: (l -> l) -> table -> Bool
>       allTable :: (l -> Bool) -> table -> Bool
>
>       checkTable :: table -> Bool
>       mkTableLazy :: (TableHeader l t) -> [(Row l t)] -> table
>       mkTable :: (TableHeader l t) -> [(Row l t)] -> table
>       mkTable = checkTab . mkTableLazy
>           where 
>               checkTab :: table -> Bool
>               checkTab tab = if check tab then tab else error "not a valid table"

> data (Ord l, Literal l t) => SimpleTable l t = Tab (TableHeader l t) (Set.Set (Row l t)) 

> instance (Ord l, Literal l t) => Table (SimpleTable l t) l t where
>       mapTable f (Tab _ rows) = Set.map f rows
>       foldTable f i (Tab _ rows) = Set.fold f i rows
>       allTable f = Set.fold ((==) . f) True

>       schema = map snd . header
>       columnNames (Tab header _) = map (\(name,_) -> name) header


>       mkTableLazy header rows = Tab header (Set.fromList rows)

Note: mkTable [] [[]] is considered invalid

>       checkTable (Tab [] rows) = Set.null rows
>       checkTable tab = ctypes && clength
>           where 
>               size = length $ header tab
>               ctypes = Set.fold ((==) . all (uncurry checkType) . zip header) True tab
>               clength = Set.fold ((==) . (size ==) . length) True tab

> mkTableFromSet :: (Literal l t) => (TableHeader l t) -> Set.Set (Row l t) -> (SimpleTable l t)
> mkTableFromSet header rows = Tab header rows

show and read instance for the table

> showsTable :: (Show l, Show t, Literal l t) => (SimpleTable l t)-> ShowS
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
> instance (Show l, Show t, Literal l t) => Show (SimpleTable l t) where showsPrec _ = showsTable

> readsColumnHeader :: (Read t) => ReadS (ColumnHeader l t)
> readsColumnHeader s =
>       [ ((n,t),w) | (n,u) <- lex s, (":",v) <- lex u, (t,w) <- reads v ]
> readsTableHeader :: (Read t) => ReadS (TableHeader l t)
> readsTableHeader s =
>       [ (l:r,w) | ("|", u) <- lex s, (l, v) <- readsColumnHeader u,(r, w) <- readsTableHeader v] ++
>       [ ([],u) | ("|",u) <- lex s, ("|",v) <- lex u] ++
>       [ ([],u) | ("|",u) <- lex s, ("",v) <- lex u] ++
>       [ ([],'|':u) | ("||",u) <- lex s]
> readsRow :: (Read l) => ReadS (Row l t)
> readsRow s = 
>       [ (l:r,w) | ("|", u) <- lex s, (l, v) <- reads u,(r, w) <- readsRow v] ++
>       [ ([],u) | ("|",u) <- lex s, ("|",v) <- lex u] ++
>       [ ([],u) | ("|",u) <- lex s, ("",v) <- lex u] ++
>       [ ([],'|':u) | ("||",u) <- lex s]
> readsRows :: (Read l) => ReadS [Row l t]
> readsRows s = 
>       [ (r:rs , v ) | (r,u) <- readsRow s, (rs,v) <- readsRows u ] ++
>       [ ([],u) | ("",u) <- lex s]
> readsTable :: (Ord l, Read l, Read t, Literal l t) => ReadS (SimpleTable l t)
> readsTable s =
>       [ (mkTable h r, w) | (h,u) <- readsTableHeader s, (r,w) <- readsRows u]
> instance (Ord l, Read l, Read t, Literal l t) => Read (SimpleTable l t) where readsPrec _ = readsTable

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

> testColumnNames = afun1 columnNames "columNames"
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
