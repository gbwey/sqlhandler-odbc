# sqlhandler-odbc

* write native SQL for Postgres/MSSQL/Oracle/MySql/Sqlite 
* supports fully typed SQL signatures 
* uses the ODBC protocol to interface with databases
* optional template haskell methods for generating type safe signatures from SQL metadata
* optional template haskell methods to generate database connections using configuration file db.cfg 
* supports fully configurable logging 
* supports predicates on the SQL output. If the validation fails then will log where the failure occurred and rollback the transaction
* supports a fully configurable pretty printer of SQL results in table format leveraging SQL metadata
* supports multiple resultsets where supported, i.e. MSSQL and Postgres
* can mark your queries or connections as ReadOnly and this will be enforced at compile time
* supports streaming queries using conduit combinators 
* fully configurable encoding of SQL input parameters and decoding of SQL output


