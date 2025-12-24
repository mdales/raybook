type t = float array array

let v a =
  match Array.length a with
  | 0 -> raise (Invalid_argument "Array dimensions can not be zero")
  | height -> (
      match Array.length a.(0) with
      | 0 -> raise (Invalid_argument "Array dimensions can not be zero")
      | width ->
          for y = 1 to height - 1 do
            if Array.length a.(y) != width then
              raise (Invalid_argument "Array rows must all have same length")
          done;
          a)

let dimensions t =
  match Array.length t with
  | 0 -> (0, 0)
  | height -> (height, Array.length t.(0))

let cell t (y, x) =
  if x < 0 then raise (Invalid_argument "Invalid x");
  if y < 0 then raise (Invalid_argument "Invalid y");
  let h, w = dimensions t in
  if x >= w then raise (Invalid_argument "Invalid x");
  if y >= h then raise (Invalid_argument "Invalid y");
  t.(y).(x)

let is_equal t o =
  let t_dims = dimensions t and o_dims = dimensions o in
  match t_dims = o_dims with
  | false -> false
  | true ->
      let h, w = t_dims in
      let rec outer j =
        let rec inner i =
          let tv = cell t (j, i) and ov = cell o (j, i) in
          let r = Float.abs (tv -. ov) < Float.epsilon in
          match r with
          | false -> false
          | true -> ( match i with 0 -> true | _ -> inner (i - 1))
        in
        let r = inner (w - 1) in
        match r with
        | false -> false
        | true -> ( match j with 0 -> true | _ -> outer (j - 1))
      in
      outer (h - 1)

let multiply a b =
  let a_height, a_width = dimensions a and b_height, b_width = dimensions b in
  if a_width != b_height then
    raise (Invalid_argument "Matrix sizes not compatible");
  let results =
    Array.init a_height (fun j ->
        let row = a.(j) in
        Array.init b_width (fun i ->
            let rec loop index acc =
              let v1 = row.(index) and v2 = b.(index).(i) in
              let acc = acc +. (v1 *. v2) in
              match index with 0 -> acc | _ -> loop (index - 1) acc
            in
            loop (a_width - 1) 0.))
  in
  v results
