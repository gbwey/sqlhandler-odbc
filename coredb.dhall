let DictT = { _1 : Text, _2 : Text }
let kv =
    \(k : Text)
 -> \(v : Text)
 -> { _1 = k, _2 = v }
 : DictT

let nodict = [] : List DictT

in {
   , nodict = nodict
   , DictT = DictT
   , kv = kv

   }
