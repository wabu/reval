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

> module Primes (
>   Type(..),
>   Literal(..),
>   SimpleType(..),
>   SimpleLit(..),
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

> data SimpleLit = Null | IntLit Int | StrLit String | CharLit Char |
>       BoolLit Bool
>       deriving (Eq)

Type is used to store Type information in the table schema.
Note: Null has Type Any.

> data SimpleType = Any | Number | String | Char | Bool
>       deriving (Show, Eq, Read)

Now we can just use Lists as rows, as all data just has the type Lit.

--- Type System Classes ---
---------------------------

As these Types are just not only a short list, we give the user the abillity to
create his own typesystem

> class (Eq t, Show t, Read t) => Type t where
>       -- check if to types Are compatible
>       check :: t -> t -> Bool

> class (Ord l, Show l, Read l, Type t) => Literal l t where
>       -- get the type of a literal
>       getType :: l -> t
>
>       -- check if the Litral is compatible to type
>       checkType :: t -> l -> Bool
>       checkType typ lit = check typ (getType lit)

--- Implmentation ---
---------------------

Ord for SimpleLit needed to stuff SimpleLits in Sets

> instance Ord SimpleLit where
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

Own Show and Read for SimpleLits

> showsLit :: SimpleLit -> ShowS
> showsLit Null = ("Null" ++)
> showsLit (IntLit l) = shows l
> showsLit (StrLit l) = shows l
> showsLit (CharLit l) = shows l
> showsLit (BoolLit l) = shows l
> instance Show SimpleLit where showsPrec _ = showsLit

> readsLit :: ReadS SimpleLit
> readsLit s = 
>       [(IntLit l, s) | (l,s) <- reads s ] ++
>       [(StrLit l, s) | (l,s) <- reads s ] ++
>       [(CharLit l, s) | (l,s) <- reads s ] ++
>       [(BoolLit l, s) | (l,s) <- reads s ] ++
>       [(Null,s) | ("Null", s) <- lex s ]
> instance Read SimpleLit where readsPrec _ = readsLit

check it two types are compatilbe

> instance Type SimpleType where
>       check Any _ = True
>       check _ Any = True
>       check Number Number = True
>       check String String = True
>       check Char Char = True
>       check Bool Bool = True
>       check _ _ = False

> instance Literal SimpleLit SimpleType where
>       getType Null = Any
>       getType (IntLit _) = Number
>       getType (StrLit _) = String
>       getType (CharLit _) = Char
>       getType (BoolLit _) = Bool

-- UnitTesting --
------------------

> int = IntLit 32
> str = StrLit "wabu"
> chr = CharLit '_'
> bool = BoolLit True
> null = Null
> types = [Number, String, Char, Bool]
> lits  = [int, str, chr, bool]

> testCheckCheck = assertfun2 check "check"
>       ( [(a,a,True) | a <- types] ++
>         [(a,Any,True) | a <- types] ++
>         [(a,Bool,False) | a <- [String,Number,Char]] ++
>         [(a,b,check b a) | a <- types, b <- types] ++
>         [(Any,Any,True)] )

> testCheckCheckType = assertfun2 checkType "checkType"
>       ( [(t,l,True) | (t,l) <- zip types lits] ++
>         [(t,l,False) | i <- [1..4], l <- drop i lits, t <- take i types] ++
>         [(t,l,False) | i <- [1..4], l <- take i lits, t <- drop i types] ++
>         [(Any,l,True) | l <- lits] )

> testPrimes = testCheckCheck && testCheckCheckType

