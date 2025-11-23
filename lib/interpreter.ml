open Value
open Scope
open Location
module StringMap = Map.Make (String)

type context = {
  patterns : Ast.pattern located_span StringMap.t;
  functions : Ast.function_ list StringMap.t;
  scopes : scope list;
}

let context_from_definitions definitions =
  let rec rec_get_context definitions acc =
    match definitions with
    | [] -> acc
    | def :: rest -> (
        match def.value with
        | Ast.DPattern (name, pattern) ->
            let new_patterns = StringMap.add name.value pattern acc.patterns in
            rec_get_context rest { acc with patterns = new_patterns }
        | Ast.DFunction (name, function_def) ->
            let existing_functions =
              match StringMap.find_opt name.value acc.functions with
              | Some funcs -> funcs
              | None -> []
            in
            let new_functions =
              StringMap.add name.value
                (function_def :: existing_functions)
                acc.functions
            in
            rec_get_context rest { acc with functions = new_functions })
  in
  rec_get_context definitions
    { patterns = StringMap.empty; functions = StringMap.empty; scopes = [] }

type error =
  | UndefinedFunction of string
  | NoMatchingFunction of string
  | UndefinedVariable of string
  | TypeError of string

let error_to_string = function
  | UndefinedFunction name -> "Undefined function: " ^ name
  | NoMatchingFunction name -> "No matching function found for: " ^ name
  | UndefinedVariable name -> "Undefined variable: " ^ name
  | TypeError msg -> "Type error: " ^ msg

let ( let* ) = Result.bind

let rec get_function_scope values patterns scope =
  match (values, patterns) with
  | [], [] -> Some scope
  | arg_value :: arg_rest, arg_pattern :: pattern_rest -> (
      match arg_value with
      | Value.VString s -> (
          let new_scope =
            Pattern_interpreter.run_match [ arg_pattern.value ] s
          in
          match new_scope with
          | Some new_scope ->
              get_function_scope arg_rest pattern_rest
                (merge_scopes scope new_scope)
          | None -> None)
      | _ -> None)
  | _ -> None

let match_defined_functions (context : context) name arguments =
  match StringMap.find_opt name context.functions with
  | Some function_defs -> (
      let rec find_matching_function defs =
        match defs with
        | [] -> None
        | (func_def : Ast.function_) :: rest -> (
            match
              get_function_scope arguments func_def.arguments.value
                Scope.empty_scope
            with
            | Some func_scope -> Some (func_scope, func_def)
            | None -> find_matching_function rest)
      in
      match find_matching_function function_defs with
      | Some scope -> Ok scope
      | None -> Error (NoMatchingFunction name))
  | None -> Error (UndefinedFunction name)

let rec eval context expr =
  match expr with
  | Ast.EVar name -> (
      match find_var context.scopes name with
      | Some value -> Ok value
      | None -> Error (UndefinedVariable name))
  | Ast.EString literal -> Ok (VString literal)
  | Ast.EConcat (first, second) -> (
      let* first_value = eval context first.value in
      let* second_value = eval context second.value in
      match (first_value, second_value) with
      | VString s1, VString s2 -> Ok (VString (s1 ^ s2))
      | _ -> Error (TypeError "Concatenation requires string values"))
  | Ast.EFunctionCall (name, arguments) ->
      let* args = eval_arguments arguments context in
      eval_function context name args

and eval_arguments arguments context =
  let rec eval_args args acc =
    match args with
    | [] -> Ok acc
    | arg :: rest ->
        let* evaluated_arg = eval context arg.value in
        eval_args rest (evaluated_arg :: acc)
  in
  match eval_args arguments [] with
  | Ok args -> Ok (List.rev args)
  | Error e -> Error e

and eval_function context name argument_values =
  let* scope, func_def = match_defined_functions context name argument_values in
  eval
    { context with scopes = scope :: context.scopes }
    func_def.expression.value

let run_main context function_ line =
  eval_function context function_ [ Value.VString line ]
