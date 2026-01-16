open Ezxmlm

let chunks n lst =
  if n <= 0 then raise (Invalid_argument "Chunk size must be 1 or more");
  let rec loop acc stash lst =
    match lst with
    | [] when stash = [] -> Result.Ok (List.rev acc)
    | [] -> Result.Error "List not divisible by chunk size"
    | hd :: tl -> (
      let stash = hd :: stash in
      let acc, stash = match (List.length stash) = n with
      | true -> (List.rev stash) :: acc, []
      | false -> acc, stash
      in
      loop acc stash tl
    )
  in loop [] [] lst

let of_file filename =
  let shapes =
    In_channel.with_open_text filename (fun c ->
        let _, xml = from_channel c in
        try
          let scene = member "X3D" xml |> member "Scene" in
          (* todo: things like camera position etc. *)
          let shapes = members "Shape" scene in
          let sl =
            List.filter_map
              (fun n ->
                try
                  let attrs, inode = member_with_attr "IndexedFaceSet" n in
                  let coordIndexStr = List.assoc ("", "coordIndex") attrs in
                  let coordIndex =
                    String.split_on_char ' ' coordIndexStr
                    |> List.map int_of_string
                  in

                  let coord_attrs, _ = member_with_attr "Coordinate" inode in
                  let pointStr = List.assoc ("", "point") coord_attrs in
                  let pointsRaw =
                    String.split_on_char ' ' pointStr
                    |> List.map float_of_string
                  in
                  let rec pointloop acc vals =
                    match vals with
                    | [] -> acc
                    | x :: y :: z :: tl ->
                        pointloop (Tuple.point x y z :: acc) tl
                    | _ -> failwith "unexpected remainder"
                  in
                  let points =
                    pointloop [] pointsRaw |> List.rev |> Array.of_list
                  in

                  let cols =
                    try
                      let col_attrs, _ = member_with_attr "ColorRGBA" inode in
                      let colsStr = List.assoc ("", "color") col_attrs in
                      let channel_vals =
                        String.split_on_char ' ' colsStr
                        |> List.map float_of_string
                      in
                      let rec colloop acc vals =
                        match vals with
                        | [] -> acc
                        | r :: g :: b :: _a :: tl ->
                            colloop (Colour.v r g b :: acc) tl
                        | _ -> failwith "colour count wrong"
                      in
                      colloop [] channel_vals |> List.rev
                    with Tag_not_found _ -> []
                  in

                  let rec polyloop acc stash cols coords =
                    match coords with
                    | [] -> acc
                    | hd :: tl ->
                        let acc, stash, cols =
                          if hd = -1 then
                            match stash with
                            | [ iz; iy; ix ] -> (
                                (* bounds check... *)
                                let x = points.(ix)
                                and y = points.(iy)
                                and z = points.(iz) in

                                match cols with
                                | hd :: tl ->
                                    let m =
                                      Material.v
                                        ~pattern:Pattern.(v (Solid hd))
                                        ()
                                    in
                                    ( Shape.(v ~material:m (Triangle (x, y, z)))
                                      :: acc,
                                      [],
                                      tl )
                                | [] ->
                                    ( Shape.(v (Triangle (x, y, z))) :: acc,
                                      [],
                                      [] ))
                            | _ -> failwith "Polygon wrong shape"
                          else (acc, hd :: stash, cols)
                        in
                        polyloop acc stash cols tl
                  in
                  let sl = polyloop [] [] cols coordIndex in
                  Some Shape.(v (Group sl))
                with Tag_not_found _ -> None)
              shapes
          in
          sl
        with Tag_not_found msg ->
          Printf.printf "no %s\n" msg;
          [])
  in
  shapes
