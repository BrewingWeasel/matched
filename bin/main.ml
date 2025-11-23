open Lilac

let usage_msg = "lilac <file1> -f <function> -i <input>"
let primary_command = ref ""
let input_file = ref ""
let main_function = ref "main"
let input = ref ""

let anon_fun arg =
  if !primary_command = "" then primary_command := arg else input_file := arg

let speclist =
  [
    ("-f", Arg.Set_string main_function, "Set default function");
    ("-i", Arg.Set_string input, "Use specific input");
  ]

let () = Arg.parse speclist anon_fun usage_msg

let read_file_to_string file =
  In_channel.with_open_text file In_channel.input_all

let run_primary line context continue =
  match Interpreter.run_main context !main_function line with
  | Ok value ->
      print_endline (Value.to_string value);
      continue context
  | Error err ->
      print_endline ("Error: " ^ Interpreter.error_to_string err);
      continue context

let rec main_loop context =
  print_string "> ";
  let line = read_line () in
  if line = ":quit" || line = ":q" then ()
  else run_primary line context main_loop

let run_file file_name run_with =
  let contents = read_file_to_string file_name in
  let file_map = Display_error.to_file_map contents in
  match Lexer.lex contents with
  | Ok tokens -> (
      match Parser.parse_file tokens with
      | Ok definitions ->
          let context = Interpreter.context_from_definitions definitions in
          Analyzer.lint_file context file_map file_name;
          run_with context
      | Error err -> prerr_endline (Parser_error.display err file_map file_name)
      )
  | Error err -> prerr_endline (Lexer.display_error err file_map file_name)

let () =
  match !primary_command with
  | "run" when !input == "" -> run_file !input_file main_loop
  | "run" ->
      run_file !input_file (fun context ->
          run_primary !input context (fun _ -> ()))
  | "test"-> run_file !input_file Test.run_file_tests
  | cmd -> prerr_endline ("Unknown command: " ^ cmd)
