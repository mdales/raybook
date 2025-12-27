type t = { id : int; transform : Matrix.t }

let v id = { id; transform = Matrix.identity 4 }
let id t = t.id
let set_transform t m = { t with transform = m }
let transform t = t.transform
