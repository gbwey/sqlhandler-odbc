let x = ./coreconn.dhall
let msgo = { driver = "Driver={ODBC Driver 17 for SQL Server}", authn = x.mstrusted }
let pggo = { driverdsn = "DRIVER={PostgreSQL ODBC Driver(ANSI)}", schema = None Text, port = None Natural }
let s3go = { driverdsn = "DRIVER={SQLite3 ODBC Driver};Timeout=10000;NoTxn=1"  }
let mygo = { driverdsn = "DRIVER={MySQL ODBC 8.0 ANSI Driver}", port = None Natural  }
let orgo = { ConnType = x.ortns "driver= {Oracle in XE}" "XE" }
in { msW = msgo /\ { server = "????", db = "????" }
   , msa = msgo /\ { server = "????", db = "????" }
--   , msX = msgo // { server = "????", authn = x.msauthn "????" "????", db = "????" }

   , pgW = pggo /\ { server = "????", uid = "????", pwd = "????", db = "????" }
   , orW = { ConnType = x.ortns "driver= {Oracle in XE}" "XE", schema = "????", uid = "????", pwd = "????" }
   , orR = orgo /\ { schema = "????", uid = "????", pwd = "????" }
   , s3W = s3go /\ { schema = None Text, fn = "s3.db" }
   , myW = mygo /\ { server = "????", uid = "????", pwd = "????", db = "????" }

   }
