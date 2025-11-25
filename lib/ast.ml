open Location

type pattern =
  | PVar of string
  | PReference of string
  | PLiteral of string
  | PEither of pattern located_span * pattern located_span
  | POptional of pattern located_span
  | PAs of string located_span * pattern located_span
  | PMultiple of pattern located_span list
  | PWithAttribute of pattern located_span * string located_span

let rec pattern_to_string = function
  | PVar v -> "var:" ^ v
  | PReference r -> "ref:" ^ r
  | PLiteral literal -> "literal:\"" ^ literal ^ "\""
  | PEither (first, second) ->
      "either("
      ^ pattern_to_string first.value
      ^ ", "
      ^ pattern_to_string second.value
      ^ ")"
  | POptional pattern -> "optional(" ^ pattern_to_string pattern.value ^ ")"
  | PMultiple members ->
      "multiple["
      ^ String.concat "; "
          (List.map (fun pattern -> pattern_to_string pattern.value) members)
      ^ "]"
  | PWithAttribute (pattern, attribute) ->
      "with_attribute("
      ^ pattern_to_string pattern.value
      ^ ", "
      ^ attribute.value
      ^ ")"
  | PAs (name, pattern) ->
      "as(" ^ name.value ^ ", " ^ pattern_to_string pattern.value ^ ")"

type expression =
  | EVar of string
  | EString of string
  | EConcat of expression located_span * expression located_span
  | EFunctionCall of string * expression located_span list

let rec expression_to_string = function
  | EVar v -> "var:" ^ v
  | EString s -> "string:\"" ^ s ^ "\""
  | EConcat (first, second) ->
      "concat("
      ^ expression_to_string first.value
      ^ ", "
      ^ expression_to_string second.value
      ^ ")"
  | EFunctionCall (name, args) ->
      let args_str =
        String.concat ", "
          (List.map (fun expr -> expression_to_string expr.value) args)
      in
      "call " ^ name ^ "(" ^ args_str ^ ")"

type function_ = {
  arguments : pattern located_span list located_span;
  expression : expression located_span;
  assertions : (expression located_span * expression located_span) list;
}

type definition =
  | DPattern of string located_span * pattern located_span
  | DFunction of string located_span * function_

let definition_to_string = function
  | DPattern (name, pattern) ->
      "pattern:[name=" ^ name.value ^ "] = " ^ pattern_to_string pattern.value
  | DFunction (name, func) ->
      let args_str =
        String.concat ", "
          (List.map
             (fun arg -> pattern_to_string arg.value)
             func.arguments.value)
      in
      let assertions_str =
        if func.assertions = [] then ""
        else
          let assertions_list =
            List.map
              (fun (cond, msg) ->
                "assert("
                ^ expression_to_string cond.value
                ^ ", "
                ^ expression_to_string msg.value
                ^ ")")
              func.assertions
          in
          "[assertions:" ^ String.concat ";" assertions_list ^ "]"
      in
      "function:[name=" ^ name.value ^ ";args=" ^ args_str ^ "] = "
      ^ expression_to_string func.expression.value
      ^ assertions_str
