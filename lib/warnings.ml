open Location
open Ast
open Interpreter
open Display_error

type warning_type =
  | RedundantPattern of Ast.pattern located_span * Ast.pattern located_span

let display warning =
  let message, highlights =
    match warning.value with
    | RedundantPattern (first, duplicate) ->
        ( "Redundant pattern",
          [
            {
              error_level = Context;
              start_pos = first.start_pos;
              end_pos = first.end_pos;
              message = "This pattern matches the same cases";
            };
            {
              error_level = Warning;
              start_pos = duplicate.start_pos;
              end_pos = duplicate.end_pos;
              message = "So this pattern is redundant";
            };
          ] )
  in
  Display_error.render_error
    { level = Warning; message; start_pos = warning.start_pos; highlights }

let rec lint_pattern acc pattern =
  match pattern.value with
  | PVar _ -> acc
  | PLiteral _ -> acc
  | PEither (first, second) ->
      let acc =
        if first.value = second.value then
          {
            value = RedundantPattern (first, second);
            start_pos = second.start_pos;
            end_pos = second.end_pos;
          }
          :: acc
        else acc
      in
      let acc = lint_pattern acc first in
      lint_pattern acc second
  | POptional inner -> lint_pattern acc inner
  | PMultiple members -> List.fold_left lint_pattern acc members

let lint_function_impl acc func_def =
  List.fold_left lint_pattern acc func_def.arguments.value

let lint_function _func_name function_defs acc =
  List.fold_left lint_function_impl acc function_defs

let lint_file context = StringMap.fold lint_function context.functions []
