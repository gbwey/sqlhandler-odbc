Here is a sample postgres signature for a query that takes one input parameter of type Int and returns one resultset with four columns Int Double UTCTime and a String
```
test0 :: Sql (DBPG a) '[Int] '[Sel (Int,Double,UTCTime,String)]
test0 = mkSql' "select * from mixed where id > ? order by id"

a <- fd $ runSql pgW (I1 1) test0
```

* fd indicates we want to log this sql query in debug mode
* runSql for typed sql queries that also extract the metadata so we can have nice names for display
* pgW is the connection (in this case uses Postgres database that is writeable)
* (I1 1) is the input parameter for the sql i test0

```
wprint' a -- display the resultset in ascii (wprint for unicode)

resultset 1 Sel
+----+--------+---------------------+-------------+
| id | total  |      eventdate      | description |
+----+--------+---------------------+-------------+
|  2 | 212.04 | 2005-03-04 00:00:00 | second row  |
+----+--------+---------------------+-------------+
|  3 |   0.0  | 1990-10-12 00:00:00 | third row   |
+----+--------+---------------------+-------------+
|  4 |  10.34 | 2007-01-02 00:00:00 | fourth row  |
+----+--------+---------------------+-------------+
```

a sample sql signature for a postgres query that takes 2 input parameters (Int and Text)
and returns two resultsets (one is a single row with a text and bool field and an update that must return 1 (ie number of rows updated))

```
t1 :: Sql (DBPG Writeable) '[Int, Text] '[SelOne (Text,Bool), U1]
t1 = mkSql' [st|
   select count(*) as cnt,true from mixed;
   update mixed set total = total+1 where id = ? and description = ?
   |]

a <- fd $ runSql pgW (I2 2 "second row") t1

wprint' a
```
```
resultset 1 SelOne
+-----+------+
| cnt | bool |
+-----+------+
|   6 | True |
+-----+------+

resultset 2 a == 1

UpdN 1
```

```
mkdir test1
cd test1

git clone https://github.com/gbwey/logging
git clone https://github.com/gbwey/hdbc-odbc2
git clone https://github.com/gbwey/table-layout
git clone https://github.com/gbwey/typelevel
git clone https://github.com/gbwey/predicate-typed
git clone https://github.com/gbwey/sqlhandler
git clone https://github.com/gbwey/sqlhandler-odbc

cd sqlhandler-odbc
stack ghci
```

add the odbc connection information in conn.dhall\
you will also need to specify the correct name for the odbc drivers that you use

```
stack ghci
:l test/integration/TestSqlite_TH.hs test/integration/TestConnections
```


* fd is runs the command in debug mode
* pgW is the postgres db connection that you set up in conn.dhall
* RNil says we dont have any input parameters
* t1 is the sql to run

run the sql
```
a <- fd $ runSql pgW RNil pgsql1

-- pretty print the result in a table
wprint a
```
template haskell see test/TestPG_TH.hs

$(genSql "pg9a" pgW (\f -> [st|select * from orders #{f "limit 4"}; select * from agents #{f "limit 3"}|]))

generates the following type signature
```
>:i pg9a
pg9a ::
  Sql
    (DBPG a)
    '[]
    '[Sel
        (Rec
           ElField
           '['("ord_num", Double), '("ord_amount", Double),
             '("ord_date", UTCTime), '("cust_code", String),
             '("agent_code", String), '("ord_description", String)]),
      Sel
        (Rec
           ElField
           '['("agent_code", String), '("agent_name", Maybe String),
             '("commission", Maybe Double)])]
```

* DBPG is postgres
* '[] means no input is required
* Sel means a normal select
* there are two result sets returned a rows of vinyl named records
* the first result set has six fields: ord_num thru ord_description
* the second result set has three fields agent_code thru commision

sqlite should work as long as you have the odbc driver for sqlite3\
s3.db holds the test database

sqlite
------

run sqlite.sql to create test tables for sqlite
just specify the filename that you used to load the above sql

```
stack ghci
:load test/integration/TestSqlite_TH.hs test/integration/TestConnections.hs
a <- fd $ runSql s3W RNil s3_3
wprint a
```
postgres
--------

run pgsql.sql to create test tables and set the conn.dhall connection information for pgW

```
stack ghci
:load test/integration/TestPG_TH.hs test/integration/TestConnections.hs
a <- fd $ runSql pgW RNil pgsql1
wprint a
```

sqlserver [sql version >= 2012]
---------

run mssql.sql to create test tables for mssql

```
stack ghci
:load test/integration/TestMS_SP_TH.hs test/integration/TestConnections.hs
a <- fd $ runSql msW RNil ms1
wprint a
```

sqlserver [for older versions eg < 2012]
---------

run mssql.sql to create test tables for mssql

```
stack ghci
:load test/integration/TestMS_TH.hs test/integration/TestConnections.hs
a <- fd $ runSql msW RNil msold1
wprint a
```

mysql
-----

run mysql.sql to create test tables

```
stack ghci
:load test/integration/TestMY_TH.hs test/integration/TestConnections.hs
a <- fd $ runSql myW RNil my6
wprint a
```

oracle
------

run oracle.sql to create test tables

```
stack ghci
:load test/integration/TestOracle_TH.hs test/integration/TestConnections.hs
a <- fd $ runSql orW RNil or1
wprint a
```

set flags to activate builds for database tests [sqlite is activated by default]
```
stack ghci --test --flag sqlhandler-odbc:pg
stack ghci --test --flag sqlhandler-odbc:ms
stack ghci --test --flag sqlhandler-odbc:msold
stack ghci --test --flag sqlhandler-odbc:or
stack ghci --test --flag sqlhandler-odbc:my
```
