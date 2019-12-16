let x = ./coredb.dhall
let msdriver = "{ODBC Driver 17 for SQL Server}"
let mydriver = "{MySQL ODBC 8.0 ANSI Driver}"
let ordriver = "{Oracle in XE}"
let pgdriver = "{PostgreSQL ODBC Driver(ANSI)}"
let s3driver = "{SQLite3 ODBC Driver}"
in { msW = x.msdef :: {  driver = msdriver, server = "????", db = "????" }
   , msa = x.msdef :: {  driver = msdriver, server = "????", db = "????" }

   , pgW = x.pgdef :: { driver = pgdriver, server = "????", uid = "????", pwd = "????", db = "????" }

   , orW = x.ordef :: { ConnType = x.ortns ordriver "XE", schema = "????", uid = "????", pwd = "????" }
   , orR = x.ordef :: { ConnType = x.ortns ordriver "XE", schema = "????", uid = "????", pwd = "????" }

   , s3W = x.s3def :: { driver = s3driver, fn = "s3.db" }

   , myW = x.mydef :: { driver = mydriver, server = "????", uid = "????", pwd = "????", db = "????" }

   }
