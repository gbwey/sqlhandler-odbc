# sqlhandler-odbc

* write native SQL for Postgres/MSSQL/Oracle/MySql/Sqlite using the ODBC protocol
* fully configurable encoding of SQL input parameters and decoding of SQL output (see [sqlhandler](https://github.com/gbwey/sqlhandler))
* supports fully typed SQL signatures
* supports multiple resultsets where supported, i.e. MSSQL and Postgres
* optional template haskell methods for generating type safe signatures from SQL metadata
* optional template haskell methods to generate database connections using configuration file db.cfg
* supports fully configurable logging using Dhall
* supports [predicates](https://github.com/gbwey/predicate) for the SQL output which if fails then will log where the failure occurred and rollback the transaction
* supports a fully configurable pretty printer of SQL results in table format leveraging SQL metadata
* can mark your queries or connections as ReadOnly which is enforced at compile time
* supports streaming queries using conduit combinators


