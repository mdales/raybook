type style_t = Stripes

type t = {
    style : style_t;
    colours : (Colour.t * Colour.t);
}

let v style colours = { style ; colours }

let style t = t.style
let colours t = t.colours
