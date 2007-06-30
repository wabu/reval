> module Table where
> 

FIXME: howto import from lib/AssertFun.lhs ?
	import Lib.AssertFun !

> import qualified AssertFun
> import qualified Data.Set as Set

this will not work :-(

 class (Show a) => Row a where
	name :: a -> String
	content :: (Show b, Ord b) => a -> [b]

 --type Table = (Row a) => [a]

 newtype StringRow = SRow (String, [String])
	deriving (Show, Eq, Read)

 instance Row StringRow where
	name (SRow (n,_))  = n
	name _ = error "StringRow.name"
	content (SRow (_,c)) = c
	content _ = error "StringRow.content"


Second Try - will not work, either ...

TabelType is a LISP-like cons-cell to represent Tabels.

 data TabelType a b = Nil | Tab (Row a) (Tab a b)
 	deriving (Show, Eq, Read)


 names Nil = [] 
 names (Tab (name, _ ) b) = names b -- INFINITE TYPE! 
 names _ = error "StringRow.name"


Row is a typealias for a tupel.
Format is (name, [values]).
Note: Rows shoudl be showable

 type Row a = (String, [a]) 

Getters for Row

 getRowName :: Row a -> String
 getRowName (name, _) = name

 getRowContent :: Row a -> [a]
 getRowContent (_, content) = content


Well, screw it, just use ASTs:

> data Lit = Null | IntLit Int | StrLit String | CharLit Char -- ...
>	deriving (Show, Eq, Read)

Ord for Lit needed to stuff Lits in Sets

> instance Ord Lit where
>	compare Null Null = EQ
>	compare (IntLit a) (IntLit b) = compare a b 
>	compare (StrLit a) (StrLit b) = compare a b 
>	compare (CharLit a) (CharLit b) = compare a b 

just to keep it consitent

>	compare Null _ = LT
>	compare _ Null = GT
>	compare (IntLit _) _ = LT
>	compare _ (IntLit _) = GT
>	compare (StrLit _) _ = LT
>	compare _ (StrLit _) = GT

TODO: impl. own Show and Read ...

> type Row = [Lit]

> type ColumName = String
> type ColumNames = [ColumName]

> data Tabel = Tab (ColumNames, Set.Set Row) 
>	deriving (Show, Eq, Read)

TODO: impl. own Show and Read ...

> mkTabel :: ColumNames -> [Row] -> Tabel
> mkTabel names rows = if all (== (length names)) (map length rows) then
>		Tab (names, Set.fromList rows)
>	else
>		error ("length name (" ++ show (length names)
>			++ ") /= length rows")
	

UnitTesting:

> tabel1 = mkTabel ["ID", "Name"] [
>	[IntLit 23, StrLit "fb"],
>	[IntLit 42, StrLit "daniel"]  ]

