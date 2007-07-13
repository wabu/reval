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

> module Reval where
> -- TODO: declare exports + export testAll
> import Primes
> import Table
> import TableOps
> import SetTable
> import SetTableOps

> testAll = testPrimes
>	-- && testTable && testTableOps -- TODO: impl and call
> 	&& testSetTable && testSetTableOps

--- Some Sample Tables ---
--------------------------

Some example databases

> exampleTable1 :: Tab
> exampleTable1 = read (
>	"| ID: Number | Name: String |" ++
>	"| 23 | \"fb\" |" ++
>	"| 42 | \"wabu\" |" ++
>	"| 2 | \"foobar\" |" ++
>	"")
>
> exampleTable2 :: Tab
> exampleTable2 = read (
>	"| ID: Number | Name: String | Student: Bool |" ++
>	"| 23 | \"fb\" | True |" ++
>	"| 42 | \"wabu\" | True |" ++
>	"| 2 | \"foobar\" | False |" ++
>	"")

> exampleTableEmpty :: Tab
> exampleTableEmpty = read "||"

--- Legal Foo ---
-----------------

> licenseNote = "rel-eval \tCopyright (C) 2007 \tDaniel Waeber, Fabian Bieker\n" ++
>       "This program comes with ABSOLUTELY NO WARRANTY; for details " ++
>       "read the LICENSE.txt file.\n" ++
>       "This is free software, and you are welcome to " ++
>       "redistribute it under certain conditions; type LGPL.txt for " ++
>       "details.\n"
> 
