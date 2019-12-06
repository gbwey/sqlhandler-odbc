let kv = \(k : Text) -> \(v : Text) -> { _1 = k, _2 = v }
let DictT = List { _1 : Text, _2 : Text }
let MSAuthnT = < Trusted | UserPwd : { User : Text, Password : Text } >
let msauthn = \(user : Text) -> \(pwd : Text) -> MSAuthnT.UserPwd { User = user, Password = pwd }
let OrConT = < TnsName : { driver : Text, tns : Text } | DsnOracle : Text >
let ortns = \(driver : Text) -> \(tns : Text) -> OrConT.TnsName { driver = driver, tns = tns }
let DBMST = { driver : Text, server : Text, db : Text, authn : MSAuthnT, dict : DictT }
let DBPGT = { driver : Text, server : Text, db : Text, schema : Optional Text, port : Optional Natural, uid : Text, pwd : Text, dict : DictT }
-- option=67108864 -- add to mysql as a default!
let DBMYT = { driver : Text, server : Text, db : Text, port : Optional Natural, uid : Text, pwd : Text, dict : DictT }
let DBORT = { schema : Text, ConnType : OrConT, uid : Text, pwd : Text, dict : DictT }
let DBS3T = { driver : Text, fn : Text, dict : DictT }
let DBSumT = < MS : DBMST | PG : DBPGT | MY : DBMYT | OR : DBORT | S3 : DBS3T >
let nodict = [] : DictT
in {
   , mstrusted = MSAuthnT.Trusted
   , msauthn = msauthn
   , orconT = OrConT
   , ortns = ortns
   , dbsum = DBSumT
   , ms = DBSumT.MS
   , pg = DBSumT.PG
   , my = DBSumT.MY
   , or = DBSumT.OR
   , s3 = DBSumT.S3
   , nodict = nodict
   , dictT = DictT
   , kv = kv
   }
