open Interpreter
open Location
open Ast

let colorize passed total =
  let color = if passed = total then "\x1b[1;32m" else "\x1b[1;31m" in
  Printf.sprintf "%s%d/%d\x1b[0m" color passed total

let rec run_assertions context assertions passed ran =
  match assertions with
  | [] -> (passed, ran)
  | (first, second) :: rest -> (
      let ran = ran + 1 in
      match
        ( Interpreter.eval context first.value,
          Interpreter.eval context second.value )
      with
      | Ok v1, Ok v2 when v1 = v2 ->
          run_assertions context rest (passed + 1) ran
      | Ok v1, Ok v2 ->
          Printf.printf "\tTest failed: %s != %s\n" (Value.to_string v1)
            (Value.to_string v2);
          run_assertions context rest passed ran
      | _, _ ->
          (* Evaluation error treated as failure *)
          run_assertions context rest passed ran)

let test_function context _func_name function_defs (passed, num_functions, total)
    =
  Printf.printf "Testing function \x1b[34m'%s'\x1b[0m..." _func_name;
  let newly_passed, newly_ran =
    List.fold_left
      (fun (acc_passed, acc_ran) (_, function_) ->
        run_assertions context function_.assertions acc_passed acc_ran)
      (0, 0) function_defs
  in
  Printf.printf " (%s passed)\n"
    (colorize newly_passed newly_ran);
  (newly_passed + passed, num_functions + 1, newly_ran + total)

let run_file_tests context =
  let passed, functions, total =
    StringMap.fold (test_function context) context.functions (0, 0, 0)
  in
  let function_plural = if functions = 1 then "function" else "functions" in
  Printf.printf "\nPassed %s tests across %d %s.\n" (colorize passed total)
    functions function_plural;
  if passed <> total then exit 1
