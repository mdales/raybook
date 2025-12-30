type t = { light : Light.t; shapes : Shape.t list }

let v light shapes = { light; shapes }
let light t = t.light
let shapes t = t.shapes
