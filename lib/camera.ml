type t = {
    hsize : int;
    vsize : int;
    field_of_view: float;
}

let v (hsize, vsize) field_of_view =
    if hsize <= 0 then
        raise (Invalid_argument "Horizontal size must be greater than 0");
    if vsize <= 0 then
        raise (Invalid_argument "Vertical size must be greater than 0");
    if field_of_view <= 0. then
        raise (Invalid_argument "Field of view must be greater than 0.");
    {
        hsize ; vsize; field_of_view;
    }

let dimensions t = (t.hsize, t.vsize)
let field_of_view t = t.field_of_view
let transform _t = Matrix.identity 4
