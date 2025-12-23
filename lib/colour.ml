type t = {
    red: float;
    green: float;
    blue: float;
}

let v red green blue = { red ; green; blue}

let red t = t.red
let green t = t.green
let blue t = t.blue

let add t o =
    {
        red = t.red +. o.red;
        green = t.green +. o.green;
        blue = t.blue +. o.blue;
    }

let subtract t o =
    {
        red = t.red -. o.red;
        green = t.green -. o.green;
        blue = t.blue -. o.blue;
    }

let fmultiply t s =
    {
        red = t.red *. s;
        green = t.green *. s;
        blue = t.blue *. s;
    }

let multiply t o =
    {
        red = t.red *. o.red;
        green = t.green *. o.green;
        blue = t.blue *.o.blue;
    }

let fp_equal a b = abs_float (a -. b) < epsilon_float

let is_equal t o =
  fp_equal t.red o.red && fp_equal t.green o.green && fp_equal t.blue o.blue
