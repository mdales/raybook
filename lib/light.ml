type t = { position : Tuple.t; intensity : Colour.t }

let v position intensity =
  if not (Tuple.is_point position) then
    raise (Invalid_argument "Position must be a point");
  { position; intensity }

let position t = t.position
let intensity t = t.intensity

let lighting ~light ~eye ~normal ~material ~point ~shadow () =
  let effective_colour =
    Colour.multiply (Material.colour material) (intensity light)
  in
  let lightv = Tuple.normalize (Tuple.subtract (position light) point) in

  let ambient_colour =
    Colour.fmultiply effective_colour (Material.ambient material)
  in

  match shadow with
  | true -> ambient_colour
  | false ->
      let light_dot_normal = Tuple.dot lightv normal in

      let black = Colour.v 0. 0. 0. in

      let diffuse =
        if light_dot_normal < 0. then black
        else
          Colour.fmultiply effective_colour
            (Material.diffuse material *. light_dot_normal)
      in

      let specular =
        if light_dot_normal < 0. then black
        else
          let negative_lightv = Tuple.negate lightv in
          let reflectv = Tuple.reflect negative_lightv normal in
          let reflect_dot_eye = Tuple.dot reflectv eye in

          if reflect_dot_eye < 0. then black
          else
            let factor =
              Float.pow reflect_dot_eye (Material.shininess material)
            in
            Colour.fmultiply (intensity light)
              (factor *. Material.specular material)
      in

      Colour.add (Colour.add ambient_colour diffuse) specular
