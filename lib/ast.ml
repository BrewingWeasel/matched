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

let rec expression_to_string = function
  | EVar v -> "var:" ^ v
  | EString s -> "string:\"" ^ s ^ "\""
  | EConcat (first, second) ->
      "concat(" ^ expression_to_string first ^ ", "
      ^ expression_to_string second
      ^ ")"
  | EFunctionCall (name, args) ->
      let args_str = String.concat ", " (List.map expression_to_string args) in
      "call " ^ name ^ "(" ^ args_str ^ ")"

type function_ = { arguments : pattern list; expression : expression }

type definition =
  | DPattern of string * pattern
  | DFunction of string * function_

let definition_to_string = function
  | DPattern (name, pattern) ->
      "pattern:[name=" ^ name ^ "] = " ^ pattern_to_string pattern
  | DFunction (name, func) ->
      let args_str =
        String.concat ", " (List.map pattern_to_string func.arguments)
      in
      "function:[name=" ^ name ^ ";args=" ^ args_str ^ "] = "
      ^ expression_to_string func.expression
