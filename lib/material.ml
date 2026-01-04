type t = {
  ambient : float;
  pattern : Pattern.t;
  diffuse : float;
  shininess : float;
  specular : float;
  reflectivity : float;
}

let v ~pattern ?(ambient = 0.1) ?(diffuse = 0.9) ?(specular = 0.9)
    ?(shininess = 200.0) ?(reflectivity = 0.0) () =
  { pattern; ambient; diffuse; specular; shininess; reflectivity }

let ambient t = t.ambient
let pattern t = t.pattern
let diffuse t = t.diffuse
let shininess t = t.shininess
let specular t = t.specular
let reflectivity t = t.reflectivity
