type t = { width : int; height : int; data : float array }

let of_matrix m =
  let height, width = Matrix.dimensions m in
  let data =
    Array.init (width * height) (fun i ->
        let x = i mod width and y = i / width in
        Matrix.cell m (y, x))
  in
  { width; height; data }

let of_tuple t = of_matrix (Tuple.to_matrix t)
let of_array a = of_matrix (Matrix.v a)
let point x y z = { width = 1; height = 4; data = [| x; y; z; 1. |] }
let vector x y z = { width = 1; height = 4; data = [| x; y; z; 0. |] }
let identity () = of_matrix (Matrix.identity 4)

let cell t (y, x) =
  let index = (y * t.width) + x in
  if index > t.width * t.height || index < 0 then
    raise (Invalid_argument "dimensions invalid");
  t.data.(index)

let is_equal t o =
  let rec equal_loop idx a b =
    let r = Float.abs (a.(idx) -. b.(idx)) < Float.epsilon *. 10. in
    if r then if idx = 0 then true else equal_loop (idx - 1) a b else false
  in
  t.width = o.width && t.height = o.height
  && equal_loop ((t.width * t.height) - 1) t.data o.data

let multiply a b =
  if a.width != b.height then
    raise (Invalid_argument "Matrix sizes not compatible");
  let data =
    Array.init (a.height * b.width) (fun idx ->
        let x = idx mod b.width and y = idx / b.width in
        let rec multiply_loop index acc =
          let v1 = a.data.((y * a.width) + index)
          and v2 = b.data.((index * b.width) + x) in
          let acc = acc +. (v1 *. v2) in
          match index with 0 -> acc | _ -> multiply_loop (index - 1) acc
        in
        multiply_loop (a.width - 1) 0.)
  in
  { width = b.width; height = a.height; data }

let transpose t =
  let data =
    Array.init (t.width * t.height) (fun i ->
        let ox = i / t.height and oy = i mod t.height in
        t.data.(ox + (oy * t.width)))
  in
  { width = t.height; height = t.width; data }

let submatrix t (r, c) =
  if t.height < 2 || t.width < 2 then
    raise (Invalid_argument "Cannot make a submatrix");
  if r >= t.height || c >= t.width || r < 0 || c < 0 then
    raise (Invalid_argument "row and/or column out of bounds");
  let new_width = t.width - 1 in
  let new_height = t.height - 1 in
  let data =
    Array.init (new_width * new_height) (fun idx ->
        let i = idx mod new_width and j = idx / new_width in
        let y = if j < r then j else j + 1 and x = if i < c then i else i + 1 in
        t.data.((y * t.width) + x))
  in
  { width = new_width; height = new_height; data }

let rec determinant t =
  if t.height <> t.width then
    raise (Invalid_argument "Can only calculate determinant on square matrices");
  match t.height with
  | 0 -> raise (Invalid_argument "Matrix too small")
  | 1 -> t.data.(0)
  | 2 -> (t.data.(0) *. t.data.(3)) -. (t.data.(2) *. t.data.(1))
  | _ ->
      let c = cofactor t in
      let rec determinant_loop acc idx =
        let acc = acc +. (t.data.(idx) *. cell c (0, idx)) in
        match idx with 0 -> acc | _ -> determinant_loop acc (idx - 1)
      in
      determinant_loop 0. (t.width - 1)

and cofactor t =
  if t.height <> t.width then
    raise (Invalid_argument "Can only calculate minor on square matrices");
  if t.height < 2 || t.width < 2 then
    raise (Invalid_argument "Matrix too small");

  let data =
    Array.init (t.width * t.height) (fun idx ->
        let i = idx mod t.width and j = idx / t.width in
        let s = submatrix t (j, i) in
        let d = determinant s in
        if (i + j) mod 2 = 0 then d else -1. *. d)
  in
  { t with data }

let minor t =
  if t.height <> t.width then
    raise (Invalid_argument "Can only calculate minor on square matrices");
  if t.height < 2 || t.width < 2 then
    raise (Invalid_argument "Matrix too small");
  let data =
    Array.init (t.width * t.height) (fun idx ->
        let i = idx mod t.width and j = idx / t.width in
        let s = submatrix t (j, i) in
        determinant s)
  in
  { t with data }

let invertible t = not (Float.abs (determinant t) < Float.epsilon)

let inverse t =
  if not (invertible t) then raise (Invalid_argument "Matrix not invertible");
  if t.height <> t.width then
    raise (Invalid_argument "Can only calculate inverse on square matrices");
  let c = cofactor t in
  let d = determinant t in
  let data =
    Array.init (t.width * t.height) (fun idx ->
        let i = idx mod t.width and j = idx / t.width in
        c.data.((i * c.width) + j) /. d)
  in
  { t with data }

let x t =
  if t.width <> 1 && t.height <> 4 then
    raise (Invalid_argument "Matrix not a tuple");
  t.data.(0)

let y t =
  if t.width <> 1 && t.height <> 4 then
    raise (Invalid_argument "Matrix not a tuple");
  t.data.(1)

let z t =
  if t.width <> 1 && t.height <> 4 then
    raise (Invalid_argument "Matrix not a tuple");
  t.data.(2)

let is_point t =
  if t.width <> 1 && t.height <> 4 then
    raise (Invalid_argument "Matrix not a tuple");
  Float.abs (1. -. t.data.(3)) < Float.epsilon

let is_vector t =
  if t.width <> 1 && t.height <> 4 then
    raise (Invalid_argument "Matrix not a tuple");
  Float.abs t.data.(3) < Float.epsilon

let add t o =
  if t.width <> 1 && t.height <> 4 then
    raise (Invalid_argument "Matrix not a tuple");
  if o.width <> 1 && o.height <> 4 then
    raise (Invalid_argument "Matrix not a tuple");
  let data = Array.map2 (fun a b -> a +. b) t.data o.data in
  (* Check W? *)
  { t with data }

let subtract t o =
  if t.width <> 1 && t.height <> 4 then
    raise (Invalid_argument "Matrix not a tuple");
  if o.width <> 1 && o.height <> 4 then
    raise (Invalid_argument "Matrix not a tuple");
  let data = Array.map2 (fun a b -> a -. b) t.data o.data in
  (* Check W? *)
  { t with data }

let negate t =
  (* Check W? *)
  let data = Array.map (fun a -> 0. -. a) t.data in
  { t with data }

let fmultiply t n =
  (* Check W? *)
  let data = Array.map (fun a -> a *. n) t.data in
  { t with data }

let fdivide t n =
  (* Check W? *)
  let data = Array.map (fun a -> a /. n) t.data in
  { t with data }

let magnitude t =
  (* Check W? *)
  Float.sqrt (Array.fold_left (fun acc n -> acc +. (n *. n)) 0. t.data)

let normalize t =
  (* Check W? *)
  let m = magnitude t in
  if m = 0. then t else fdivide t m

let dot t o =
  if t.width <> 1 && t.height <> 4 then
    raise (Invalid_argument "Matrix not a tuple");
  if o.width <> 1 && o.height <> 4 then
    raise (Invalid_argument "Matrix not a tuple");
  (t.data.(0) *. o.data.(0))
  +. (t.data.(1) *. o.data.(1))
  +. (t.data.(2) *. o.data.(2))

let cross t o =
  if t.width <> 1 && t.height <> 4 then
    raise (Invalid_argument "Matrix not a tuple");
  if o.width <> 1 && o.height <> 4 then
    raise (Invalid_argument "Matrix not a tuple");
  let data = Array.init (t.height * t.width) (fun _ -> 0.) in
  data.(0) <- (t.data.(1) *. o.data.(2)) -. (t.data.(2) *. o.data.(1));
  data.(1) <- (t.data.(2) *. o.data.(0)) -. (t.data.(0) *. o.data.(2));
  data.(2) <- (t.data.(0) *. o.data.(1)) -. (t.data.(1) *. o.data.(0));
  { t with data }

let reflect v n = subtract v (fmultiply n (2. *. dot v n))
