- The Table Type -
Or how to trick Haskell to get a Table with different data types inside each
column, but with arbitrary length.

> module Table where
> 
> import Lib.AssertFun
> import qualified Data.Set as Set

-- Our First Try --

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

Well, screw it, just use ASTs:

> data Lit = Null | IntLit Int | StrLit String | CharLit Char -- ...
>       deriving (Show, Eq, Read)
>
> data Type = Any | Number | String | Char
>       deriving (Show, Eq, Read)

Now we can just use Lists as rows, as all data just has the type Lit. Creating
our own type System, we also define need type types to check things.


Ord for Lit needed to stuff Lits in Sets

> instance Ord Lit where
>       compare Null Null = EQ
>       compare (IntLit a) (IntLit b) = compare a b 
>       compare (StrLit a) (StrLit b) = compare a b 
>       compare (CharLit a) (CharLit b) = compare a b 

just to keep it consitent

>       compare Null _ = LT
>       compare _ Null = GT
>       compare (IntLit _) _ = LT
>       compare _ (IntLit _) = GT
>       compare (StrLit _) _ = LT
>       compare _ (StrLit _) = GT


Row is a typealias for a tupel.
Format is (name, [values]).
Note: Rows shoudl be showable

 type Row a = (String, [a]) 

Getters for Row

 getRowName :: Row a -> String
 getRowName (name, _) = name

 getRowContent :: Row a -> [a]
 getRowContent (_, content) = content



compare the Type of two Lits

> cmpLitType :: Lit -> Lit -> Bool
> cmpLitType Null Null = True
> cmpLitType (IntLit _) (IntLit _) = True
> cmpLitType (StrLit _) (StrLit _) = True
> cmpLitType (CharLit _) (CharLit _) = True
> cmpLitType _ _ = False

TODO: impl. own Show and Read ...

> type Row = [Lit]

> type ColumName = String
> type ColumNames = [ColumName]

> data Table = Tab (ColumNames, Set.Set Row) 
>       deriving (Show, Eq, Read)

TODO: impl. own Show and Read ...

> mkTable :: ColumNames -> [Row] -> Table
> mkTable names rows =
> 	if all (== (length names)) (map length rows) then
>              Tab (names, Set.fromList rows)
>       else
>              error ("length name (" ++ show (length names)
>                     ++ ") /= length rows")

Note: mkTable [] [[]] is considered invalid

> checkTable :: Table -> Bool
> checkTable (Tab ([], rows)) = Set.null rows
> checkTable (Tab (names, rows)) =
>	Set.null (Set.filter checkLength rows)
>		&& checkTypes rows	
>	where
>	checkLength row = length row /= length names
>	rowsSize = Set.size rows
>	first :: Row
>	first = head (Set.toList rows) 

check the type of all rows

>	checkTypes :: Set.Set Row -> Bool
>	checkTypes rows = rowsSize == 0
>		||  all checkTyp (Set.toList rows)

check the type of one row

>	checkTyp :: Row -> Bool
>	checkTyp row = foldr (==) True (map
>			(\(f,x) -> f x)
>			(zip (map cmpLitType first) row))
>		  	

UnitTesting:

> tableEmpty = mkTable [] []

> table1 = mkTable ["ID", "Name"] [
>       [IntLit 23, StrLit "fb"],
>       [IntLit 42, StrLit "daniel"]  ]

> tableInvalid = mkTable ["ID", "Name"] [
>       [IntLit 23, StrLit "fb"],
>       [CharLit 'a', StrLit "daniel"]  ]

> testCheckTable = afun1 checkTable "checktab"
>       [tableEmpty, table1, tableInvalid]
>       [True, True, False]

> testAll = testCheckTable


