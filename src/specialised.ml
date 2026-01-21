type t = {
    width: int;
    height: int;
    data : float array;
}

let of_matrix m =
    let height, width = Matrix.dimensions m in
    let data = Array.init (width * height) (fun i ->
        let x = i mod width
        and y = i / width in
        Matrix.cell m (y, x)
    )
    in
    { width; height; data }

let of_tuple t =
    of_matrix (Tuple.to_matrix t)

let of_array a =
    of_matrix (Matrix.v a)

let of_point x y z =
    {
        width = 1;
        height = 4;
        data = [| x ; y ; z ; 1. |];
    }

let of_vector x y z =
    {
        width = 1;
        height = 4;
        data = [| x ; y ; z ; 0. |];
    }

let identity () =
    of_matrix (Matrix.identity 4)

let cell t (y, x) =
    let index = y * t.width + x in
    if index > (t.width * t.height) || index < 0 then
        raise (Invalid_argument "dimensions invalid");
    t.data.(index)

let is_equal t o =
    let rec loop idx a b =
        let r = Float.abs (a.(idx) -. b.(idx)) < Float.epsilon *. 10. in
        if r then (
            if idx = 0 then true else loop (idx - 1) a b
        ) else false
    in
    (t.width = o.width) &&
    (t.height = o.height) &&
    (loop ((t.width * t.height) - 1) t.data o.data)

let multiply a b =
    if a.width != b.height then
        raise (Invalid_argument "Matrix sizes not compatible");
    let data = Array.init (a.height * b.width) (fun idx ->
        let x = idx mod b.width
        and y = idx / b.width in
        let rec loop index acc =
            let v1 = a.data.((y * a.width) + index)
            and v2 = b.data.(index * b.width + x) in
            let acc = acc +. (v1 *. v2) in
            match index with 0 -> acc | _ -> loop (index - 1) acc
        in
        loop (a.width - 1) 0.
    )
    in
    {
        width = b.width;
        height = a.height;
        data;
    }

let transpose t =
    let data = Array.init (t.width * t.height) (fun i ->
        let ox = i / t.height
        and oy = i mod t.height in
        t.data.(ox + (oy * t.width))
    )
    in
    {
        width=t.height;
        height=t.width;
        data;
    }
