let x = ./coredb.dhall
let orgo = { ConnType = x.ortns "{Oracle in XE}" "XE", dict = x.nodict }
let pgdriver = "{PostgreSQL ODBC Driver(ANSI)}"
let msdriver = "{ODBC Driver 17 for SQL Server}"
let s3driver = "{SQLite3 ODBC Driver}"
let mydriver = "{MySQL ODBC 8.0 ANSI Driver}"
in { msW = x.msdef :: {  driver = msdriver, server = "????", db = "????" }
   , msa = x.msdef :: {  driver = msdriver, server = "????", db = "????" }

   , pgW = x.pgdef :: { driver = pgdriver, server = "????", uid = "????", pwd = "????", db = "????" }

   , orW = { ConnType = x.ortns "{Oracle in XE}" "XE", schema = "????", uid = "????", pwd = "????", dict = x.nodict }
   , orR = orgo /\ { schema = "????", uid = "????", pwd = "????" }

   , s3W = x.s3def :: { driver = s3driver, fn = "s3.db" }

   , myW = x.mydef :: { driver = mydriver, server = "????", uid = "????", pwd = "????", db = "????" }

   }
