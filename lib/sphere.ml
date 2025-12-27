type t = { id : int; transform : Matrix.t; inverse_transform : Matrix.t }

let v id =
  { id; transform = Matrix.identity 4; inverse_transform = Matrix.identity 4 }

let id t = t.id

let set_transform t m =
  { t with transform = m; inverse_transform = Matrix.inverse m }

let transform t = t.transform
let inverse_transform t = t.inverse_transform
