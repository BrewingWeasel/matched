type value = 
    | VString of string
    | VNumber of int

let to_string v =
  match v with
  | VString s -> s
  | VNumber n -> string_of_int n
