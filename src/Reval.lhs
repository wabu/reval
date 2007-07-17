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

We use hg ( http://en.wikipedia.org/wiki/Mercurial_(software) ) for
version mangement. The hg webinterface can be found at
http://forti.ath.cx:20480/pro/hg/reval/ .
To view the Documentation click on "Manifest" and navigate to "docs" .
The hg changelog is available as an rss-feed:
http://forti.ath.cx:20480/pro/hg/reval/rss-log .
To browse the files check out
http://forti.ath.cx:20480/pro/hg/reval/file/ .

--- Contribute ---
------------------

Feel free to submit usefull patches and bugreports, they are welcome.
Check out todo.txt and todo.tmp.txt for open tasks.
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

> tablePersons :: Tab
> tablePersons = read (
>       "| ID: Integer | Name: String | Student: Bool | FavLang: String |" ++
>       "| 23          | \"fb\"       | True          | \"lisp\"        |" ++
>       "| 42          | \"wabu\"     | True          | \"ruby\"        |" ++
>       "| 1337        | \"Schweppe\" | False         | \"haskell\"     |" ++
>       "")
>
> tableProject :: Tab
> tableProject = read (
>       "| Project: String | Supervisor: Integer | Language: String | Detail: String |" ++
>       "| \"reval\" | 12 | \"haskell\"  | \"reval is a relational algebra interpretor\" |" ++
>       "| \"x2c\"   | 23 | \"ruby\"     | \"x2c is a command line client for xmms2\" |" ++
>       "")
>
> tableDeveloper :: Tab
> tableDeveloper = read (
>       "| Dev: Integer | Project: String |" ++
>       "| 23           | \"reval\"       |" ++
>       "| 42           | \"reval\"       |" ++
>       "| 42           | \"x2c\"         |" ++
>       "")

> tableEmpty :: Tab
> tableEmpty = read "|"

> tableID :: Tab
> tableID = read "| ID: Integer || 23 || 42 |"

--- Some pointers to get you started ---
----------------------------------------

try something like:

* cross-join (full outer join):

> testCross = cross tablePersons tableProject

* select rows with ID == 1337

> testSelect = select (\(x:_) -> x == IntLit 1337) tablePersons

* advanced joins: theta-join tablePerson with tableDeveloper on ID == Dev

  with the power of haskell we can create a nice theta join with the condtion
  seperated form theta join, so it will be readable even for larger operations

> testTheta = theta sameID tablePersons tableDeveloper
>       where 
>       sameID person developer = (personID person == developerID developer)
>       personID = getValue tablePersons "ID"
>       developerID = getValue tableDeveloper "Dev"

* combine queries:  

TODO: natural join with Project would be really nice

> developing = project ["Name", "Project"] (theta sameID tablePersons tableDeveloper)
>       where 
>       sameID person developer = (personID person == developerID developer)
>       personID = getValue tablePersons "ID"
>       developerID = getValue tableDeveloper "Dev"

TODO: more examles
* supervisors that are not developing ...
* developers with projects in there favourt language

--- Legal Foo ---
-----------------

> licenseNote = "rel-eval \tCopyright (C) 2007 \tDaniel Waeber, Fabian Bieker\n" ++
>       "This program comes with ABSOLUTELY NO WARRANTY; for details " ++
>       "read the LICENSE.txt file.\n" ++
>       "This is free software, and you are welcome to " ++
>       "redistribute it under certain conditions; type LGPL.txt for " ++
>       "details.\n"
> 
