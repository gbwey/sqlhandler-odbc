let x = ./coredb.dhall

let DBS3T = { driver : Text, fn : Text, dict : List x.DictT }

in x /\ {
   , Type = DBS3T
   , default = { driver = ./s3driver.dhall, dict = [ x.kv "Timeout" "10000", x.kv "NoTxn" "1" ] }
   }
