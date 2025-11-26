open Location
open Ast
open Interpreter
open Display_error

type warning_type =
  | RedundantPattern of Ast.pattern located_span * Ast.pattern located_span
  | NonexistentPattern of string * string located_span list

let display warning file_map file_name =
  let message, is_error, highlights =
    match warning.value with
    | RedundantPattern (first, duplicate) ->
        ( "Redundant pattern",
          false,
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
    | NonexistentPattern (name, possibly_meant) ->
        let suggestion =
          match possibly_meant with
          | may_have_meant :: _ ->
              [
                {
                  error_level = Context;
                  start_pos = may_have_meant.start_pos;
                  end_pos = may_have_meant.end_pos;
                  message =
                    "Did you mean to reference '" ^ may_have_meant.value
                    ^ "' instead?";
                };
              ]
          | _ -> []
        in
        ( "Nonexistent pattern reference: '" ^ name ^ "'",
          true,
          {
            error_level = Error;
            start_pos = warning.start_pos;
            end_pos = warning.end_pos;
            message =
              Printf.sprintf "Pattern reference '"
              ^ name ^ "' has not been defined";
          }
          :: suggestion )
  in
  let level = if is_error then Error else Warning in
  ( Display_error.render_error
      { level; message; start_pos = warning.start_pos; highlights }
      file_map file_name,
    is_error )

let rec lint_pattern context acc pattern =
  match pattern.value with
  | PVar _ -> acc
  | PReference name -> (
      match StringMap.find_opt name context.patterns with
      | Some _ -> acc
      | None ->
          let patterns =
            String.spellcheck
              (fun yield ->
                StringMap.iter (fun a _ -> yield a) context.patterns)
              name
          in
          let possibly_meant =
            List.map
              (fun pattern_name ->
                let loc, _ = StringMap.find pattern_name context.patterns in
                { loc with value = pattern_name })
              patterns
          in
          { pattern with value = NonexistentPattern (name, possibly_meant) }
          :: acc)
  | PLiteral _ -> acc
  | PEither (first, second) ->
      let is_redundant =
        match (first.value, second.value) with
        | p1, p2 when p1 = p2 -> true
        | PVar _, _ -> true
        | _, _ -> false
      in
      let acc =
        if is_redundant then
          {
            value = RedundantPattern (first, second);
            start_pos = second.start_pos;
            end_pos = second.end_pos;
          }
          :: acc
        else acc
      in
      let acc = lint_pattern context acc first in
      lint_pattern context acc second
  | POptional inner -> lint_pattern context acc inner
  | PAs (_name, inner) -> lint_pattern context acc inner
  | PMultiple members -> List.fold_left (lint_pattern context) acc members
  | PWithAttribute (inner, _attribute) -> (lint_pattern context) acc inner

let lint_function_impl context acc (_, func_def) =
  List.fold_left (lint_pattern context) acc func_def.arguments.value

let lint_function context _func_name function_defs acc =
  List.fold_left (lint_function_impl context) acc function_defs

let lint_file context file_map file_name =
  let rec render_lints is_error = function
    | [] -> is_error
    | lint :: rest ->
        let lint_msg, current_is_error = display lint file_map file_name in
        prerr_endline lint_msg;
        render_lints (is_error || current_is_error) rest
  in
  let lints = StringMap.fold (lint_function context) context.functions [] in
  if render_lints false lints then (
    prerr_endline
      "\x1b[1;31mError:\x1b[0m could not run program due to the errors above";
    exit 1)
