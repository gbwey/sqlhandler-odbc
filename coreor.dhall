let x = ./coredb.dhall

let ConT = < TnsName : { driver : Text, tns : Text } | DsnOracle : Text >
let tns = \(driver : Text) -> \(tnsval : Text) -> ConT.TnsName { driver = driver, tns = tnsval }
let DBORT = { schema : Text, ConnType : ConT, uid : Text, pwd : Text, dict : List x.DictT }

in x /\ {
   , Type = DBORT
   , default = { dict = x.nodict }
   , tnsDef = tns ./ordriver.dhall
   , ConT
   , tns
   , tnsDef1 = \(tnsval : Text) -> { Type = DBORT, default = { ConnType = tns ./ordriver.dhall, dict = x.nodict } }
   }
