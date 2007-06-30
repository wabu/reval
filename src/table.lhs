> module Table where
> 

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

> data Tabel a b = Nil |  Tab a b
> 	deriving (Show, Eq, Read)
> 
> type Row a = [a] 
