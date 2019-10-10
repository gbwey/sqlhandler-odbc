let x = ./coreconn.dhall
let s3go = { driverdsn = "DRIVER={SQLite3 ODBC Driver};Timeout=10000;NoTxn=1"  }
in { s3W = s3go /\ { schema = None Text, fn = "s3.db" }
   }
