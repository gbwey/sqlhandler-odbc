let x = ./coredb.dhall

let AuthnT = < Trusted | UserPwd : { user : Text, password : Text } >
let authn = \(user : Text) -> \(pwd : Text) -> AuthnT.UserPwd { user = user, password = pwd }

let DBMST = { driver : Text, server : Text, db : Text, authn : AuthnT, dict : List x.DictT }

in x /\ {
   , Type = DBMST
   , default = { driver = ./msdriver.dhall, authn = AuthnT.Trusted, dict = x.nodict }
   , trusted = AuthnT.Trusted
   , authn
   , AuthnT
   }
