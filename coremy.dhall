let x = ./coredb.dhall

let DBMYT = { driver : Text, server : Text, db : Text, port : Optional Natural, uid : Text, pwd : Text, dict : List x.DictT }

in {

   , Type = DBMYT
   , default = { driver = ./mydriver.dhall, port = None Natural, dict = [ x.kv "option" "67108864" ] }

   , nodict = x.nodict
   , dictT = x.DictT
   , kv = x.kv

   , DBMYT = DBMYT
   }
