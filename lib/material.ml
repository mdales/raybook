type t = {
  ambient : float;
  diffuse : float;
  pattern : Pattern.t;
  reflectivity : float;
  refractive_index : float;
  shininess : float;
  specular : float;
  transparency : float;
}

let v ~pattern ?(ambient = 0.1) ?(diffuse = 0.9) ?(reflectivity = 0.0) ?(refractive_index = 1.0) ?(specular = 0.9)
    ?(shininess = 200.0) ?(transparency = 0.0) () =
  { ambient; diffuse; pattern; reflectivity; refractive_index; specular; shininess; transparency }

let ambient t = t.ambient
let diffuse t = t.diffuse
let pattern t = t.pattern
let reflectivity t = t.reflectivity
let refractive_index t = t.refractive_index
let shininess t = t.shininess
let specular t = t.specular
let transparency t = t.transparency
