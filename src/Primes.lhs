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


- What to put into Tables -
---------------------------

Or how to trick Haskell to get a Table with different data types inside each
column, but with arbitrary length.

-- Our First Try --
-------------------

... in tricking the type system. The type class look good, but still
we got a problem: There is no way to instantiate it.

> {- the class looks good, but can be instanciated
>
> class (Show a) => Column a where
>       name :: a -> String
>       content :: (Show b, Ord b) => a -> [b]
>
> newtype StringColumn = SColumn (String, [String])
>       deriving (Show, Eq, Read)
>
> instance Column StringColumn where
>       name (SColumn (n,_))  = n
>       content (SColumn (_,c)) = c
> -}

Any java programmer would say: yes, this looks good: c is a String, so it
should match the generall type b. But as haskells type system is a relly static
and strict, the code will genrate an error:

  Expected type: [b]
  Inferred type: [String]

We have to return a value of the general undefined type b, which is simply not
possible.


-- The Second Try --
--------------------

The idea was to create a TableType as a LISP-like cons-cell to represent
Tables.  Trying to ignore the type of the second argument, so we can put a
TableType d e did not work, as the strict type system cam back as we tried to
write a method on TableType. The compile will construct the type for the
function, but that's an infinite type, so BOOOMM!! as soon as we use the type.

> {- cant be used either
>
> data TableType a b = Nil | Tab (Row a) b
>        deriving (Show, Eq, Read)
>
>
> names Nil = [] 
> names (Tab (name, _ ) b) = names b -- INFINITE TYPE! BOOM!!
> names _ = error "StringRow.name"
>
> -}



-- Better: Use ASTs --
----------------------

Well, screw it, just use ASTs to create an own type system.

--- Basic AST Types ---
-----------------------

SimpleLit is a Literal, that consists of a type Information and its value.

> data SimpleLit = Null | IntLit Int | StrLit String | CharLit Char |
>       BoolLit Bool
>       deriving (Eq)

Type is used to store Type information in the table schema.
Note: Null has Type Any.

> data SimpleType = Any | Integer | String | Char | Bool
>       deriving (Show, Eq, Read)

Now we can just use Lists as rows, as all data inside the row has the same type
Lit.

--- Type System Classes ---
---------------------------

As these Types are just small fraction of possible valuse stored inside tables,
we give the user the abillity to create his own typesystem and let all thing
work on typeclasses.

> class (Eq t) => Type t where
>       -- check if to types Are compatible
>       check :: t -> t -> Bool

We need glasgow/98 extension because Literal is conjunct to its Type. So we
define the Literal class to have a data l, the literal, and a data t, the type,
where the literal l determinds the type it is based on.

> class (Type t) => Literal l t | l -> t where

The Literal calss has functions to get the type and to check if it matches a
given Type.

>       getType :: l -> t
>
>       checkType :: t -> l -> Bool
>       checkType typ lit = check typ (getType lit)

--- Implmentation ---
---------------------

Now we can implement these function for our SimpleLit and use it to create
Tables.

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

> instance Type SimpleType where
>       check Any _ = True
>       check _ Any = True
>       check Integer Integer = True
>       check String String = True
>       check Char Char = True
>       check Bool Bool = True
>       check _ _ = False

> instance Literal SimpleLit SimpleType where
>       getType Null = Any
>       getType (IntLit _) = Integer
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
> types = [Integer, String, Char, Bool]
> lits  = [int, str, chr, bool]

> testCheck = assertfun2 check "check"
>       ( [(a,a,True) | a <- types] ++
>         [(a,Any,True) | a <- types] ++
>         [(a,Bool,False) | a <- [String,Integer,Char]] ++
>         [(a,b,check b a) | a <- types, b <- types] ++
>         [(Any,Any,True)] )

> testCheckType = assertfun2 checkType "checkType"
>       ( [(t,l,True) | (t,l) <- zip types lits] ++
>         [(t,l,False) | i <- [1..4], l <- drop i lits, t <- take i types] ++
>         [(t,l,False) | i <- [1..4], l <- take i lits, t <- drop i types] ++
>         [(Any,l,True) | l <- lits] )

> testPrimes = testCheck && testCheckType

