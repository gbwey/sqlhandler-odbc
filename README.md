# sqlhandler-odbc
## database package using [hdbc-odbc](https://github.com/gbwey/hdbc-odbc)

* write native SQL for Postgres/MSSQL/Oracle/MySql/Sqlite using the ODBC protocol
* fully configurable encoding of SQL input parameters and decoding of SQL output (see [sqlhandler](https://github.com/gbwey/sqlhandler))
* supports typed SQL signatures
* supports multiple resultsets where supported, i.e. MSSQL and Postgres
* optional template haskell methods for generating type safe signatures from SQL metadata
* optional template haskell methods to generate database connections using configuration file conn.dhall
* supports configurable logging
* supports a configurable pretty printer of SQL results in table format leveraging SQL metadata
* can mark your queries or connections as ReadOnly which is enforced at compile time
* supports streaming queries using conduit combinators
* supports [refined types](https://github.com/gbwey/predicate-typed) allowing for refined fields for any sql columns:
    the encoding / decoding and validation is all handled
* supports [predicates](https://github.com/gbwey/predicate) for the SQL output which if fails then will log where the failure occurred and rollback the transaction

* log.dhall has the logging configuration
* conn.dhall has database connections for use in template haskell and testing
  -- by default there is an entry for sqlite3 using a test database s3.db



```text
stack ghci --test
:l test\integration\TestSqlite_TH.hs test\integration\TestConnections.hs

-- run an untyped query
-- s3W is a sqlite3 connection
-- [] means no input parameters

>a <- fd $ runSqlRaw s3W [] "select 10 as age, 'abcd' as name union select 20,'xyz'"

>wprint a

1 of 1 Select 2 rows
+-----+------+
| age | name |
+-----+------+
| 10  | abcd |
+-----+------+
| 20  | xyz  |
+-----+------+

>a <- fd $ runSqlRaw (sqliteX "s3.db") [] "select rowid,* from mixed limit 10"

>wprint a

1 of 1 Select 6 rows
+------------------------------------------------+
¦ rowid ¦ id ¦ total  ¦ eventdate  ¦ description ¦
¦-------+----+--------+------------+-------------¦
¦ 1     ¦ 1  ¦ 123.76 ¦ 2010-01-02 ¦ first row   ¦
+-------+----+--------+------------+-------------¦
¦ 2     ¦ 2  ¦ 212.04 ¦ 2005-03-04 ¦ second row  ¦
+-------+----+--------+------------+-------------¦
¦ 3     ¦ 3  ¦ 0.0    ¦ 1990-10-12 ¦ third row   ¦
+-------+----+--------+------------+-------------¦
¦ 4     ¦ 4  ¦ 10.34  ¦ 2007-01-02 ¦ fourth row  ¦
+-------+----+--------+------------+-------------¦
¦ 5     ¦ 5  ¦ 20.56  ¦ 2012-01-02 ¦ fifth row   ¦
+-------+----+--------+------------+-------------¦
¦ 6     ¦ 6  ¦ 777.0  ¦ 2005-01-02 ¦ sixth row   ¦
+------------------------------------------------+

it :: ()

-- run a typed query using refinement types
>a <- fd $ runSql s3W RNil s3_test1

>wprint a


resultset 1 Sel
+----------------------------------------------------------------------------+
¦                 seconds                 ¦               ssn                ¦
¦-----------------------------------------+----------------------------------¦
¦ R3:(2019-01-01 01:01:01 UTC,1546304461) ¦ R3:([123,34,2224],"123-34-2224") ¦
+-----------------------------------------+----------------------------------¦
¦ R3:(1970-01-01 00:00:00 UTC,0)          ¦ R3:([123,12,2222],"123-12-2222") ¦
+----------------------------------------------------------------------------+
```
