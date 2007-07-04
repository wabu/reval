> module Primes (
>   Type(..),
>   readsLit, -- TODO: abstraction!
>   Lit(..),
>   checkType,
>   getLitType,
>   checkLitType,
>   cmpLitType,
>   testPrimes,
> )
> where
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

Well, screw it, just use ASTs

--- Basic AST Types ---
-----------------------
Lit is a Literal.

> data Lit = Null | IntLit Int | StrLit String | CharLit Char |
>       BoolLit Bool
>       deriving (Eq)

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

Own Show and Read for Lits

> showsLit :: Lit -> ShowS
> showsLit Null = ("Null" ++)
> showsLit (IntLit l) = shows l
> showsLit (StrLit l) = shows l
> showsLit (CharLit l) = shows l
> showsLit (BoolLit l) = shows l
> instance Show Lit where showsPrec _ = showsLit

> readsLit :: ReadS Lit
> readsLit s = [(Null,s) | ("Null", s) <- lex s ] ++
>              [(IntLit l, s) | (l,s) <- reads s ] ++
>              [(StrLit l, s) | (l,s) <- reads s ] ++
>              [(CharLit l, s) | (l,s) <- reads s ] ++
>              [(BoolLit l, s) | (l,s) <- reads s ]
> instance Read Lit where readsPrec _ = readsLit

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

-- UnitTesting --
------------------

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

> testPrimes = testCheckTypeEq && testCheckLitType && testCheckCmpType

