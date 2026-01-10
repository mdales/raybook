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
          (* The x10 here is gross, but epsilon itself is too close
          when we want to do a == (a * b) * inverse(b)
          *)
          let r = Float.abs (tv -. ov) < Float.epsilon *. 10. in
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

let identity s =
  match s with
  | 0 -> raise (Invalid_argument "Matrix size must be greater than 0")
  | _ ->
      Array.init s (fun j -> Array.init s (fun i -> if i = j then 1. else 0.))

let transpose t =
  let h, w = dimensions t in
  Array.init w (fun i -> Array.init h (fun j -> t.(j).(i)))

let submatrix t (r, c) =
  let h, w = dimensions t in
  if h < 2 || w < 2 then raise (Invalid_argument "Cannot make a submatrix");
  if r >= h || c >= w || r < 0 || c < 0 then
    raise (Invalid_argument "row and/or column out of bounds");
  Array.init (h - 1) (fun j ->
      Array.init (w - 1) (fun i ->
          let y = if j < r then j else j + 1
          and x = if i < c then i else i + 1 in
          t.(y).(x)))

let rec determinant t =
  let h, w = dimensions t in
  if h <> w then
    raise (Invalid_argument "Can only calculate determinant on square matrices");
  match h with
  | 0 -> raise (Invalid_argument "Matrix too small")
  | 1 -> t.(0).(0)
  | 2 -> (t.(0).(0) *. t.(1).(1)) -. (t.(1).(0) *. t.(0).(1))
  | _ ->
      let c = cofactor t in
      let top_row = t.(0) in
      let rec loop acc idx =
        let acc = acc +. (top_row.(idx) *. cell c (0, idx)) in
        match idx with 0 -> acc | _ -> loop acc (idx - 1)
      in
      loop 0. (w - 1)

and cofactor t =
  let h, w = dimensions t in
  if h <> w then
    raise (Invalid_argument "Can only calculate minor on square matrices");
  if h < 2 || w < 2 then raise (Invalid_argument "Matrix too small");
  Array.init h (fun j ->
      Array.init w (fun i ->
          let s = submatrix t (j, i) in
          let d = determinant s in
          if (i + j) mod 2 = 0 then d else -1. *. d))

let minor t =
  let h, w = dimensions t in
  if h <> w then
    raise (Invalid_argument "Can only calculate minor on square matrices");
  if h < 2 || w < 2 then raise (Invalid_argument "Matrix too small");
  Array.init h (fun j ->
      Array.init w (fun i ->
          let s = submatrix t (j, i) in
          determinant s))

let invertible t = not (Float.abs (determinant t) < Float.epsilon)

let inverse t =
  if not (invertible t) then raise (Invalid_argument "Matrix not invertible");
  let h, w = dimensions t in
  if h <> w then
    raise (Invalid_argument "Can only calculate inverse on square matrices");
  let c = cofactor t in
  let d = determinant t in
  Array.init h (fun j -> Array.init w (fun i -> cell c (i, j) /. d))
