let x = ./coredb.dhall

let DBS3T = { driver : Text, fn : Text, dict : List x.DictT }

in {
   , Type = DBS3T
   , default = { driver = ./s3driver.dhall, dict = [ x.kv "Timeout" "10000", x.kv "NoTxn" "1" ] }

   , nodict = x.nodict
   , kv = x.kv

   , DBS3T = DBS3T
   }
