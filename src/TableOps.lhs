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

- TableOps -
------------

Class to encapsulte Primitive Relation Algebra Operations on Table
ADT. Default implemantations are provied, too.


> module TableOps (
>   TableOps(..),
>   testTableOps,
> )
> where
>
> import Primes
> import Table
> import Lib.AssertFun
> import qualified Data.List as List
> import Data.List ((\\))

> class (Table tab l t, Type t, Literal l t, Eq tab, Eq l)
>	=> TableOps tab l t | tab -> l t where

---- basic set operations ----

>	union :: tab -> tab -> tab
>	union = applyOnTableRows List.union
>	difference :: tab -> tab -> tab
>	difference = applyOnTableRows (\\)
>	intersect :: tab -> tab -> tab
>	intersect = applyOnTableRows List.intersect

---- cross-join ----

>	cross :: tab -> tab -> tab
>	cross t1 t2 = foldRows (addRowsFrom t2) newEmpty t1

(addRowsFrom t) is a high order function. It creates a function out of a table t'.
The function will take a row r and a table t, and appends each rows form t' to
r and puts them into t, resulting in a table where r is crossed with each row
of t.

>		where
>               addRowsFrom :: tab -> (Row l -> tab -> tab)
>               addRowsFrom t' r t = foldRows (cons . (r++)) t t'
>		newEmpty = mkTable (header t1 ++ header t2) []

---- Selection ----

> 	select :: ((Row l) -> Bool) -> tab -> tab
> 	select p tab = mkTable (header tab) (filter p (rows tab))

---- Projection ----


>	projectUnsafe :: [ColumnName] -> tab -> tab
>	project :: [ColumnName] -> tab -> tab
>	projectUnsafe wanted tab = mkTable newHeader $ rows newTab
>		where
>               filterWanted = map snd . filter fst . zip [elem n wanted | n <- columnNames tab]
>               newHeader = filterWanted (header tab)
>      		newTab = mapRowsUnsafe filterWanted tab
>       project wanted tab
>               | not checkExist = error(
>                   "invalid projection: a name can't be found inside the table in project" ++
>                   show wanted ++ " form " ++ show names)
>               | not checkMulti = error(
>                   "invalid projection: multiple rows with same name in project " ++
>                   show wanted ++ " form " ++ show names)
>               | otherwise = projectUnsafe wanted tab
>               where
>               names = columnNames tab
>               checkExist = null (wanted \\ names)
>               checkMulti = length wanted == 
>                   length [n | w <- wanted, n <- names, w==n]

---- Renaming ----

Syntax: rename{Unsafe} [(oldName, newName)] table

>	renameUnsafe :: [(ColumnName,ColumnName)] -> tab -> tab
>	rename ::[(ColumnName,ColumnName)] -> tab -> tab
>	renameUnsafe names tab = mkTable (replaceNames h) (rows tab)
>		where
>		h = header tab
>		replaceNames = map (getNewName names)
>		getNewName [] header = header
>		getNewName ((old,new):xs) h@(name, ctype)
>			| name == old = (new, ctype)
>			| otherwise = getNewName xs h

>	rename names tab
>		| checkParams = renameUnsafe names tab
>		| otherwise = error ("invalid rename: names = " ++ show names 
>			++ "\n table column names = " ++
>			 show columnNames)
>		where
>		h = header tab
>		checkParams = null (oldNames \\ columnNames)
>		(oldNames,_) = unzip names
>		(columnNames,_) = unzip h

---- Advnced Operations ----

>       theta :: (Row l -> Row l -> Bool) -> tab -> tab -> tab
>       theta f t1 t2 = mkTable newHeader
>		[ x++y | x <- r1, y <- r2, f x y]
>		where
>		newHeader = header t1 ++ header t2
>		r1 = rows t1
>		r2 = rows t2


>	natural :: tab -> tab -> tab

We can cons all valid rows into a new emptyTable.

>	natural tab1 tab2 = foldRows (consRows tab2) (mkTable newHeader []) tab1
>		where
>               h1 = header tab1
>               h2 = header tab2
>               newHeader = List.union h1 h2

To check if a row is valid, we create a new Row with the header of each row
inside, so we can check if thats is a functional relation. That means, there is
only one value for each header, so the value in both rows has to be equal.

>               zipedRow r1 r2 = List.union (zip h1 r1) (zip h2 r2)
>               checkFunctional ziped = all
>                       (\h -> 1 == length [h | (h',_) <- ziped, h==h']) 
>                       newHeader 

So we have to fold over each table, and check all rows inside the inner fold

>               consRows tab r t = foldRows 
>                       (\r' t' -> checkedCons (zipedRow r r') t') 
>                       t tab
>               checkedCons r = if checkFunctional r then cons $ map snd r else id


As our table header is a list, not a set, so we have some problems with the
definition of outer joins: Our implementation expects the join to return a
table with a header ordered in a certain way, but it should work on all
possible combinations of orders for the header.

TODO: Better implementation of left right full outer joins

>       left :: (tab -> tab -> tab) -> tab -> tab -> tab
>       left f tab1 tab2
>               | checkHeader = union original $ cross (notInOriginal tab1) (projectAway tab1 $ (nullTab tab2))
>               | otherwise = error("This function you tried to call as a left join " ++
>                                   "can't be modefied to be become outer!")
>               where
>                       original = f tab1 tab2
>                       checkHeader = all (uncurry (==)) $ zip (header original) (header tab1)
>                       notInOriginal t = t \\\ projectTo t original
>                       projectTo t' t = project (columnNames t') t 
>
>                       projectAway t' t = project (columnNames t \\ columnNames t') t
>                       nullTab t = mkTable (header t) $ [map (\a -> getNull) (header t)]

>       right :: (tab -> tab -> tab) -> tab -> tab -> tab
>       right f tab1 tab2
>               | checkHeader = union original $ cross (notInOriginal tab2) (projectAway tab2 $ (nullTab tab1))
>               | otherwise = error("This function you tried to call as a right join "++
>                                   "can't be modefied to be become outer!")
>               where
>                       original = f tab1 tab2
>                       checkHeader = all (uncurry (==)) $ zip (header original) (header tab2)
>                       notInOriginal t = t \\\ projectTo t original
>                       projectTo t' t = natural (crossN (columnNames t') t) (project (columnNames t') t)
>                       crossN :: [String] -> tab -> tab
>                       crossN [] t = t
>                       crossN (n:ns) t = cross (project [n] t) (crossN ns t)
>
>                       projectAway t' t = project (columnNames t \\ columnNames t') t
>                       nullTab t = mkTable (header t) $ [map (\a -> getNull) (header t)]


---- syntatic sugar ----

>       σ :: ((Row l) -> Bool) -> tab -> tab
>       σ = select
>       π :: [ColumnName] -> tab -> tab
>       π = project
>       ρ :: [(ColumnName,ColumnName)] -> tab -> tab
>       ρ = rename
>       θ :: (Row l -> Row l -> Bool) -> tab -> tab -> tab
>       θ = theta
>	(&&&) :: tab -> tab -> tab
>	(&&&) = intersect
>	(|||) :: tab -> tab -> tab
>	(|||) = union
>	(***) :: tab -> tab -> tab
>	(***) = cross
>	(\\\) :: tab -> tab -> tab
>	(\\\) = difference

Higher order function to apply f on the rows of two tables.
Checks if the headers of the table match, if not error is raised.
This is usefull to impl. union, intersection etc.

>	applyOnTableRows :: ([Row l] -> [Row l] -> [Row l]) -> tab -> tab -> tab
>	applyOnTableRows f t1 t2
>		| h1 == h2 = mkTable h1 (f r1 r2)
>		| otherwise = error ("applyOnTableRows: table headers"
>			++ "do not match: " ++
>			(show $ columnNames t1) ++ " /= " ++ (show $ columnNames t1))
>		where
>		h1 = header t1
>		h2 = header t2
>		r1 = rows t1
>		r2 = rows t2


-- Unit Test --
---------------

TODO: write testTable + unit tests

> testTableOps = True
