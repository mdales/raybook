module type ship = sig
  val colours : (float * float * float) list
  val points : (float * float * float) array
  val coordinates : (int * int * int) list
end
