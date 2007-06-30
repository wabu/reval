> module Table where
> 

FIXME: howto import from lib/AssertFun.lhs ?
	import Lib.AssertFun !

> import qualified AssertFun

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

UnitTesting:

