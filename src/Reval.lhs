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

First read Primes, Table and SetTable to get an overview.
Then check out TableOps and SetTableOps where union, cross-joins etc.
are declared and implemented.

--- Version Management ---
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

Feel free to submit useful patches and bug-reports, they are welcome.
Check out todo.txt and todo.tmp.txt for open tasks.
Try to stick to the coding standards. No, currently they are not
documented...

--- Testing ---
---------------

Run testAll to run unitTests. If testAll returns true, everthing is
fine, otherwise an error is raised.

> module Reval (
>	testAll,
>	Table(..),
>	TableOps(..),
>	mkTableFromSet,
>	-- TODO: export something else?
> )
> where
> import Primes
> import Table
> import TableOps
> import SetTable
> import SetTableOps

> testAll = testPrimes
>       && testTable && testTableOps
>       && testSetTable && testSetTableOps

--- Sample Usage ---
--------------------

If you just want to some simple relational algebra expressions, the most
comfortable way is to put the tables and some expression in a file like this:

* import Primes, SetTable and SetTableOps
* write table declaration
* write some queries and call them inside ghci
* or input queries directly to ghci

Below is an example, with some table and some queries. Simply load Reval.lhs
into ghci with the following command:
  ghci -fglasgow-exts -i<path/to/reval/src> <your/file>
Now you can view a table, for example, by typing tablePersons, view the result
of a query by evaluating developing or just input your own query, e.g. cross
developing tableProject.


---- Some Sample Tables ----

Some example tables, you must declare the type type (here it is Tab) and read
in a table, which consists of a header declaration and it rows. Each column is
separated by a |, beginning and ending with a |.  You can use Integer, String,
Bool and Char as types. Literals are formated just like normal Haskell
literals.

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

---- Some pointers to get you started ----

You can find a full list of operations for table in the class declaration
inside TableOps.lhs. But if you go through the following example and play a
little with the oparations in ghci, you will know everything you need to
know in order to use reval.

* cross-join (full outer join):

> testCross = cross tablePersons tableProject

* select rows with ID == 1337

> testSelect = select (\(x:_) -> x == IntLit 1337) tablePersons

* advanced joins: theta-join tablePerson with tableDeveloper on ID == Dev

  With the power of Haskell we can create a nice theta join with the condition
  separated form theta join, so it will be readable even for larger operations

> testTheta = theta sameID tablePersons tableDeveloper
>       where 
>       sameID person developer = (personID person == developerID developer)
>       personID = getValue tablePersons "ID"
>       developerID = getValue tableDeveloper "Dev"

* combine queries:  

> developing = project ["Name", "Project"] (theta sameID tablePersons tableDeveloper)
>       where 
>       sameID person developer = (personID person == developerID developer)
>       personID = getValue tablePersons "ID"
>       developerID = getValue tableDeveloper "Dev"

> developing' = project ["Name", "Project", "Language", "Detail"] $ natural developing tableProject

> developing'' = project ["Name", "Project"] (left (theta sameID) tablePersons tableDeveloper)
>       where 
>       sameID person developer = (personID person == developerID developer)
>       personID = getValue tablePersons "ID"
>       developerID = getValue tableDeveloper "Dev"

TODO: more examples
* supervisors that are not developing ...
* developers with projects in there favourite language

--- Legal Foo ---
-----------------

A default GPL Note. Not printed at startup, 'cause we consider this
annoying.

> licenseNote = "rel-eval \tCopyright (C) 2007 \tDaniel Waeber, Fabian Bieker\n" ++
>       "This program comes with ABSOLUTELY NO WARRANTY; for details " ++
>       "read the LICENSE.txt file.\n" ++
>       "This is free software, and you are welcome to " ++
>       "redistribute it under certain conditions; type LGPL.txt for " ++
>       "details.\n"
> 
