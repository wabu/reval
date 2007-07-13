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


- The Table ADT -
------------------

> module Table (
>   -- TODO: hide stuff
>   -- testTable -- TODO: impl + export
>   Row,
>   ColumnName,
>   ColumnHeader,
>   TableHeader,
>   Table(..),
> )
> where
> 
> import Primes
> import Lib.AssertFun


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
>		validRow r = all (uncurry checkType) (zip sch r)
>		checkSize = allRows (\r -> length sch == length r) t

check the Table and return it, if it is valid, otherwise genrate in error

>       checkedTable :: tab -> tab
>       checkedTable t
>		| checkTable t = t
>               | otherwise = error ("chechedTable: " ++ 
>			"Invalid unshowable table (you loose).")
>
>       mkTableUnsave :: (TableHeader t) -> [(Row l)] -> tab
>
>       mkTable :: (TableHeader t) -> [(Row l)] -> tab
>       mkTable h r = checkedTable (mkTableUnsave h r)

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
>       filterRows :: ((Row l) -> Bool) -> tab -> tab
>	filterRows f t = foldRows
>	 	(\row table -> if f row then cons row table else table) 
>		(mkTable (header t) [])
>		t

mapRowsUnsafe might invalidate the table.
Use mapRows if you want the table to be validated against the schema.
Use mapRows unsafe if you want the table to be unchecked.

>       mapRowsUnsafe :: ((Row l) -> (Row l)) -> tab -> tab
>	mapRowsUnsafe f t = foldRows
>	 	(\row table -> cons (f row) table) 
>		(mkTable (header t) [])
>		t
>	mapRows :: ((Row l) -> (Row l)) -> tab -> tab
>	mapRows f t = checkedTable (mapRowsUnsafe f t)

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

TODO: unit test default impls of class Table
