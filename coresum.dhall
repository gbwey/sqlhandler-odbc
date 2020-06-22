let ms = ./corems.dhall
let pg = ./corepg.dhall
let my = ./coremy.dhall
let or = ./coreor.dhall
let s3 = ./cores3.dhall

let DBSumT = < MS : ms.Type
             | PG : pg.Type
             | MY : my.Type
             | OR : or.Type
             | S3 : s3.Type >

in {
   , dbsum = DBSumT
   , ms = DBSumT.MS
   , pg = DBSumT.PG
   , my = DBSumT.MY
   , or = DBSumT.OR
   , s3 = DBSumT.S3
   }
