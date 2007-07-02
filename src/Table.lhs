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

Well, screw it, just use ASTs:
* just a Test List
* you can say something here
  and spread it of more linse
* cool ha?
* you just
  * can't have
  * a sublist
* but you can continue
Your normal Text


--- Basic AST Types ---
-----------------------
Lit is a Literal.
TODO: impl. own Show and Read ...

> data Lit = Null | IntLit Int | StrLit String | CharLit Char |
>       BoolLit Bool
>       deriving (Show, Eq, Read)

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
> type ColumNames = [ColumName]

> data Table = Tab (ColumNames, Set.Set Row) 
>       deriving (Show, Eq) -- , Read)

FIXME:
 =>  No instance for (Read (Data.Set.Set Row))
     arising from the 'deriving' clause of a data type declaration at Table.lhs:127:6
     Probable fix: add an instance declaration for (Read (Data.Set.Set Row))
     When deriving the `Read' instance for type `Table'

TODO: 
change ADT to: 

ColumHeader = (ColumName, ColumType)

type ColumHeader = (String, Type)
type ColumHeaders = [ColumHeader]

data Table = Tab (ColumHeaders, Set.Set Row)
	deriving (Show, Eq) -- , Read)

Do you like that? (fb)	
       

TODO: impl. own Show and Read ...

> mkTable :: ColumNames -> [Row] -> Table
> mkTable names rows =
>        if all (== (length names)) (map length rows) then
>              Tab (names, Set.fromList rows)
>       else
>              error ("length name (" ++ show (length names)
>                     ++ ") /= length rows")

Note: mkTable [] [[]] is considered invalid

> checkTable :: Table -> Bool
> checkTable (Tab ([], rows)) = Set.null rows
> checkTable (Tab (names, rows)) =
>       Set.null (Set.filter checkLength rows)
>       	&& checkTypes rows	
>       where
>       checkLength row = length row /= length names
>       rowsSize = Set.size rows
>       first = head (Set.toList rows) 

check the type of all rows

>       checkTypes :: Set.Set Row -> Bool
>       checkTypes rows = rowsSize == 0
>       	||  all checkTyp (Set.toList rows)

check the type of one row

>       checkTyp :: Row -> Bool
>       checkTyp row = foldr (==) True (map
>       		(\(f,x) -> f x)
>       		(zip (map cmpLitType first) row))
>       	  	

TODO: perhaps something like this is cooler for typecheking ;)
	yes the above code is insane and performs badly (fb)

> {-
> all (all (== True)) $ 
>     map                                   -- for each row
>         ((map (uncurry (==))) .           -- check the types of the pair list
>               ((zip ['a','b','c']) .      -- pairs are zipped out of the table type
>                     map head))            -- and getType map of the row
>         [["a","b","c"],["a","a","c"]] 
> 
>         -- ['a','b','c']->types, head->getType, [[],[]]->table
> -}



-- UnitTesting: --
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

> tableEmpty = mkTable [] []

> table1 = mkTable ["ID", "Name"] [
>       [IntLit 23, StrLit "fb"],
>       [IntLit 42, StrLit "daniel"]  ]

Yes, those two are valid!

> table2 = mkTable ["ID", "Name"] [
>       [IntLit 23, StrLit "fb"],
>       [Null, StrLit "daniel"]  ]
>
> table3 = mkTable ["ID", "Name"] [
>       [Null, StrLit "fb"],
>       [IntLit 42, StrLit "daniel"]  ]

> tableInvalid = mkTable ["ID", "Name"] [
>       [IntLit 23, StrLit "fb"],
>       [CharLit 'a', StrLit "daniel"]  ]

> testCheckTable = afun1 checkTable "checktab"
>       [tableEmpty, table1, tableInvalid, table2, table3]
>       [True, True, False, True, True]

> testAll = testCheckTypes && testCheckTable


License foo:

> licenseNote = "rel-eval \tCopyright (C) 2007 \tDaniel Waeber, Fabian Bieker\n" ++
>       "This program comes with ABSOLUTELY NO WARRANTY; for details " ++
>       "read the LICENSE.txt file.\n" ++
>       "This is free software, and you are welcome to " ++
>       "redistribute it under certain conditions; type LGPL.txt for " ++
>       "details.\n"

