open Matched

let read_file_to_string file =
  In_channel.with_open_text file In_channel.input_all

let rec main_loop context =
  print_string "> ";
  let line = read_line () in
  if line = ":quit" || line = ":q" then ()
  else
    match Interpreter.run_main context line with
    | Ok value ->
        print_endline (Value.to_string value);
        main_loop context
    | Error err ->
        print_endline ("Error: " ^ Interpreter.error_to_string err);
        main_loop context

let run_file file_name =
  let contents = read_file_to_string file_name in
  let file_map = Display_error.to_file_map contents in
  match Lexer.lex contents with
  | Ok tokens -> (
      match Parser.parse_file tokens with
      | Ok definitions ->
          let context = Interpreter.context_from_definitions definitions in
          let warnings = Warnings.lint_file context in
          List.iter
            (fun warning ->
              prerr_endline (Warnings.display warning file_map file_name))
            warnings;
          main_loop context
      | Error err -> prerr_endline (Parser_error.display err file_map file_name)
      )
  | Error err -> prerr_endline (Lexer.display_error err file_map file_name)

let file_name = Sys.argv.(1)
let () = run_file file_name
