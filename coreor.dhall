let x = ./coredb.dhall

let ConT = < TnsName : { driver : Text, tns : Text } | DsnOracle : Text >
let tns = \(driver : Text) -> \(tns : Text) -> ConT.TnsName { driver = driver, tns = tns }
let DBORT = { schema : Text, ConnType : ConT, uid : Text, pwd : Text, dict : List x.DictT }

in {
   , conT = ConT
   , tns = tns

   , Type = DBORT
   , default = { dict = x.nodict }
   , tnsDef = tns ./ordriver.dhall
   , nodict = x.nodict
   , kv = x.kv

   , DBORT = DBORT
   }
