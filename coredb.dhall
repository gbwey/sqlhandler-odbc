let kv = \(k : Text) -> \(v : Text) -> { _1 = k, _2 = v }
let DictT = List { _1 : Text, _2 : Text }
let nodict = [] : DictT

let MSAuthnT = < Trusted | UserPwd : { User : Text, Password : Text } >
let msauthn = \(user : Text) -> \(pwd : Text) -> MSAuthnT.UserPwd { User = user, Password = pwd }
let OrConT = < TnsName : { driver : Text, tns : Text } | DsnOracle : Text >
let ortns = \(driver : Text) -> \(tns : Text) -> OrConT.TnsName { driver = driver, tns = tns }
let DBMST = { driver : Text, server : Text, db : Text, authn : MSAuthnT, dict : DictT }
let DBPGT = { driver : Text, server : Text, db : Text, schema : Optional Text, port : Optional Natural, uid : Text, pwd : Text, dict : DictT }
let DBMYT = { driver : Text, server : Text, db : Text, port : Optional Natural, uid : Text, pwd : Text, dict : DictT }
let DBS3T = { driver : Text, fn : Text, dict : DictT }
let DBORT = { schema : Text, ConnType : OrConT, uid : Text, pwd : Text, dict : DictT }

let s3def = { Type = DBS3T, default = { dict = [ kv "Timeout" "10000", kv "NoTxn" "1" ] } }
let mydef = { Type = DBMYT, default = { port = None Natural, dict = [ kv "option" "67108864" ] } }
let ordef = { Type = DBORT, default = { dict = nodict } }
let msdef = { Type = DBMST, default = { authn = MSAuthnT.Trusted, dict = nodict } }
let pgdef = { Type = DBPGT, default = { schema = None Text, port = None Natural, dict = nodict } }

in {
   , mstrusted = MSAuthnT.Trusted
   , msauthn = msauthn
   , orconT = OrConT
   , ortns = ortns

   , msdef = msdef
   , pgdef = pgdef
   , mydef = mydef
   , ordef = ordef
   , s3def = s3def

   , nodict = nodict
   , dictT = DictT
   , kv = kv

   , DBMST = DBMST
   , DBPGT = DBPGT
   , DBMYT = DBMYT
   , DBORT = DBORT
   , DBS3T = DBS3T
   , MSAuthnT = MSAuthnT
   }
