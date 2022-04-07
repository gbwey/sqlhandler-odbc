let ms = ./corems.dhall
let pg = ./corepg.dhall
let or = ./coreor.dhall
let s3 = ./cores3.dhall
let my = ./coremy.dhall
in { msW = ms :: { server = "localhost", db = "test" } : ms.Type
   , msa = ms :: { server = "localhost", db = "test" } : ms.Type
   , pgW = pg :: { server = "localhost", uid = "??", pwd = "postgres", db = "??" } : pg.Type
   , orW = or :: { ConnType = or.tnsDef "XE", schema = "??", uid = "??", pwd = "??" } : or.Type
   , orR = or :: { ConnType = or.tnsDef "XE", schema = "??", uid = "??", pwd = "??" } : or.Type
   , s3W = s3 :: { fn = "s3.db" } : s3.Type
   , myW = my :: { server = "localhost", uid = "??", pwd = "??", db = "??" } : my.Type
   }
