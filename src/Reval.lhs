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

- Reval -
---------

Reval is an acronym for relational algebra evaluation.

--- Recommended reading ---
---------------------------

Frist read Primes, Table and SetTable to get an overview.
Then check out TableOps and SetTableOps where union, cross-joins etc.
are declared and implemented.

--- Version Managment ---
-------------------------

We use hg ( http://en.wikipedia.org/wiki/Mercurial_%28software%29 ) for
version mangement. The hg webinterface can be found at
http://forti.ath.cx:20480/pro/hg/reval/ .
To view the Documentation click on "Manifest" and navigate to "docs" .
The hg changelog is available as an rss-feed:
http://forti.ath.cx:20480/pro/hg/reval/rss-log .
To browse the files check out
http://forti.ath.cx:20480/pro/hg/reval/file/ .

--- Contribute ---
------------------

Feel free to submit patches, they are welcome.
Try to stick to the coding standards. No, currently they are not
documented...

--- Testing ---
---------------

Run testAll to run unitTests. If testAll returns true, everthing is
fine, otherwise an error is raised.

> module Reval where
> -- TODO: declare exports + export testAll
> import Primes
> import Table
> import TableOps
> import SetTable
> import SetTableOps

> testAll = testPrimes
>       && testTable && testTableOps
>       && testSetTable && testSetTableOps

--- Some Sample Tables ---
--------------------------

Some example tables to get you started:

> exampleTable1 :: Tab
> exampleTable1 = read (
>       "| ID: Integer | Name: String |" ++
>       "| 23         | \"fb\"       |" ++
>       "| 42         | \"wabu\"     |" ++
>       "| 2          | \"foobar\"   |" ++
>       "")
>
> exampleTable2 :: Tab
> exampleTable2 = read (
>       "| ID: Integer | Name: String | Student: Bool |" ++
>       "| 23         | \"fb\"       | True          |" ++
>       "| 42         | \"wabu\"     | True          |" ++
>       "| 2          | \"foobar\"   | False         |" ++
>       "")

> exampleTableEmpty :: Tab
> exampleTableEmpty = read "||"

> exampleTableID :: Tab
> exampleTableID = read "| ID: Integer || 23 || 42 || 2 |"

--- Some pointers to get you started ---
----------------------------------------

try something like:

* cross-join (full outer join):

  cross exampleTable1 exampleTable2

* select rows with ID == 2

  select (\(x:_) -> x == IntLit 2) exampleTable1

* combine queries:  

  select (\(x:_) -> x == IntLit 2) exampleTable1
        (cross exampleTable1 exampleTable2)

--- Legal Foo ---
-----------------

> licenseNote = "rel-eval \tCopyright (C) 2007 \tDaniel Waeber, Fabian Bieker\n" ++
>       "This program comes with ABSOLUTELY NO WARRANTY; for details " ++
>       "read the LICENSE.txt file.\n" ++
>       "This is free software, and you are welcome to " ++
>       "redistribute it under certain conditions; type LGPL.txt for " ++
>       "details.\n"
> 
