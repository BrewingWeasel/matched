type pattern =
  | PVar of string
  | PLiteral of string
  | PEither of pattern * pattern
  | POptional of pattern
  | PMultiple of pattern list

let rec pattern_to_string = function
  | PVar v -> "var:" ^ v
  | PLiteral literal -> "literal:\"" ^ literal ^ "\""
  | PEither (first, second) ->
      "either(" ^ pattern_to_string first ^ ", " ^ pattern_to_string second
      ^ ")"
  | POptional pattern -> "optional(" ^ pattern_to_string pattern ^ ")"
  | PMultiple members ->
      "multiple["
      ^ String.concat "; " (List.map pattern_to_string members)
      ^ "]"

type expression =
  | EVar of string
  | EString of string
  | EConcat of expression * expression
  | EFunctionCall of string * expression list

type function_ = { 
    arguments: pattern list;
    expression: expression;
}

type definition =
  | DPattern of string * pattern
  | DFunction of string * function_
