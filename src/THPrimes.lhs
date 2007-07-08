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

> module THPrimes (
>   THType(..),
>   THLit(..),
>   testTHPrimes,
> )
> where
> 
> import Lib.AssertFun
> import Primes
> import Language.Haskell.TH
> import Language.Haskell.TH.Syntax
> import Language.Haskell.TH.Lib
> import Language.Haskell.TH.Ppr

-- Use Template Haskel to get Type info --
------------------------------------------

> data THType = THType TypeQ
> data THLit  = THLit  ExpQ

> printAST :: ExpQ -> IO ()
> printAST  ast = runQ ast >>= putStrLn . show
>
> printCode :: ExpQ -> IO ()
> printCode ast = (runQ ast) >>= (putStrLn . pprint)

hm, this sucks, all these thing are evil Monard, how shold we implement read/show

> testTHPrimes = True

