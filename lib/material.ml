type t = {
  ambient : float;
  colour : Colour.t;
  diffuse : float;
  shininess : float;
  specular : float;
}

let v ~colour ?(ambient = 0.1) ?(diffuse = 0.9) ?(specular = 0.9)
    ?(shininess = 200.0) () =
  { colour; ambient; diffuse; specular; shininess }

let ambient t = t.ambient
let colour t = t.colour
let diffuse t = t.diffuse
let shininess t = t.shininess
let specular t = t.specular
