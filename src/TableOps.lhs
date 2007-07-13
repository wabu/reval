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

> module TableOps (
>   TableOps(..),
>   -- testTableOps, -- TODO: export + write unit tests
> )
> where
>
> import Primes
> import Table
> import Lib.AssertFun
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

>	cross :: tab -> tab -> tab
> 	cross t1 t2 = mkTable newHeader
>		[ x++y | x <- r1, y <- r2]
>		where
>		newHeader = header t1 ++ header t2
>		r1 = rows t1
>		r2 = rows t2

Selection

> 	select :: ((Row l) -> Bool) -> tab -> tab
> 	select p tab = mkTable (header tab) (filter p (rows tab))

Projection
TODO: default impl., but clean instance project first!

> 	project :: [ColumnName] -> tab -> tab

Renaming

Syntax: rename{Unsafe} [(oldName, newName)] table

> 	renameUnsafe :: [(ColumnName,ColumnName)] -> tab -> tab
> 	rename ::[(ColumnName,ColumnName)] -> tab -> tab
> 	renameUnsafe names tab = mkTable (replaceNames h) (rows tab)
>		where
>		h = header tab
>		replaceNames = map (getNewName names)
>		getNewName [] header = header
>		getNewName ((old,new):xs) h@(name, ctype)
>			| name == old = (new, ctype)
>			| otherwise = getNewName xs h

> 	rename names tab
>		| checkParams = renameUnsafe names tab
>		| otherwise = error ("invalid rename: names = " ++ show names 
>			++ "\n table column names = " ++
>			 show columnNames)
>		where
>		h = header tab
>		checkParams = null (oldNames \\ columnNames)
>		(oldNames,_) = unzip names
>		(columnNames,_) = unzip h


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
