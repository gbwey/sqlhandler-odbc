# sqlhandler-odbc

* write native sql for postgres/mssql/oracle/mysql/sqlite 
* supports fully typed sql signatures 
* uses the ODBC protocol to interface with databases
* optional template haskell methods for generating type safe signatures from sql metadata
* optional template haskell methods to generate database connections using configuration file db.cfg 
* supports fully configurable logging 
* supports predicates on the sql output. If the validation fails then will log where the failure occurred and rollback the transaction
* supports a fully configurable pretty printer of sql results in table format leveraging sql metadata
* supports multiple resultsets where supported ie ms sqlserver and postgres
* can mark your queries or connections as readonly and this will be enforced at compile time
* supports streaming queries using conduit combinators 
* fully configurable encoding of sql input parameters and decoding of sql result sets


