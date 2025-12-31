type t = Sphere of Sphere.t

let material t = match t with Sphere s -> Sphere.material s
