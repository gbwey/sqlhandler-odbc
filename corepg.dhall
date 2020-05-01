let x = ./coredb.dhall

let DBPGT = { driver : Text, server : Text, db : Text, schema : Optional Text, port : Optional Natural, uid : Text, pwd : Text, dict : List x.DictT }

in {
   , Type = DBPGT
   , default = { driver = ./pgdriver.dhall, schema = None Text, port = None Natural, dict = x.nodict }

   , nodict = x.nodict
   , kv = x.kv

   , DBPGT = DBPGT
   }
