> module Table where
> 

FIXME: howto import from lib/AssertFun.lhs ?

> import qualified AssertFun

FIXME: this will not work :-(

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

Just go easy, and everthing is fine...

Tabel is a LISP-like cons-cell to represent Tabels.

> data Tabel a b = Nil |  Tab a b
> 	deriving (Show, Eq, Read)
> 

Row is a typealias for a tupel.
Format is (name, [values]).
Note: Rows shoudl be showable

> type Row a = (String, [a]) 
