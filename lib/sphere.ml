type t = {
  id : int;
  material : Material.t;
  transform : Matrix.t;
  inverse_transform : Matrix.t;
  transpose_inverse_transform : Matrix.t;
}

let v ?material id =
  let material =
    match material with
    | Some m -> m
    | None -> Material.v ~colour:(Colour.v 1. 1. 1.) ()
  in
  {
    id;
    material;
    transform = Matrix.identity 4;
    inverse_transform = Matrix.identity 4;
    transpose_inverse_transform = Matrix.transpose (Matrix.identity 4);
  }

let id t = t.id
let material t = t.material

let set_transform t m =
  {
    t with
    transform = m;
    inverse_transform = Matrix.inverse m;
    transpose_inverse_transform = Matrix.transpose (Matrix.inverse m);
  }

let transform t = t.transform
let inverse_transform t = t.inverse_transform
let transpose_inverse_transform t = t.transpose_inverse_transform
