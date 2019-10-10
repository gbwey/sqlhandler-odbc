let MSAuthnT = < Trusted | UserPwd : { User : Text, Password : Text } >
let msauthn = \(user : Text) -> \(pwd : Text) -> MSAuthnT.UserPwd { User = user, Password = pwd }
let OrConT = < TnsName : { driver : Text, tns : Text } | DsnOracle : Text >
let ortns = \(driver : Text) -> \(tns : Text) -> OrConT.TnsName { driver = driver, tns = tns }
in { mstrusted = MSAuthnT.Trusted
   , msauthn = msauthn
   , orconT = OrConT
   , ortns = ortns
   }
