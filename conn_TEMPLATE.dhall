let x = ./coredb.dhall
let msgo = { driver = "{ODBC Driver 17 for SQL Server}", authn = x.mstrusted, dict = x.nodict }
let pggo = { driver = "{PostgreSQL ODBC Driver(ANSI)}", schema = None Text, port = None Natural, dict = x.nodict }
let s3go = { driver = "{SQLite3 ODBC Driver}", dict = [ x.kv "Timeout" "10000", x.kv "NoTxn" "1" ] }
let mygo = { driver = "{MySQL ODBC 8.0 ANSI Driver}", port = None Natural, dict = [ x.kv "option" "67108864" ] }
let orgo = { ConnType = x.ortns "{Oracle in XE}" "XE", dict = x.nodict }
in { msW = msgo /\ { server = "????", db = "????" }
   , msa = msgo /\ { server = "????", db = "????" }
--   , msX = msgo // { server = "????", authn = x.msauthn "????" "????", db = "????" }

   , pgW = pggo /\ { server = "????", uid = "????", pwd = "????", db = "????" }
   , orW = { ConnType = x.ortns "{Oracle in XE}" "XE", schema = "????", uid = "????", pwd = "????", dict = x.nodict }
   , orR = orgo /\ { schema = "????", uid = "????", pwd = "????" }
   , s3W = s3go /\ { fn = "s3.db" }
   , myW = mygo /\ { server = "????", uid = "????", pwd = "????", db = "????" }

   }
