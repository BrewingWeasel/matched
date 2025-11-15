open Matched

let run_generic_parsing parse to_string input =
  match Lexer.lex input with
  | Ok tokens -> (
      match parse tokens with
      | Ok parsed -> print_endline @@ to_string parsed
      | Error e -> print_endline @@ Parser_error.error_to_string e)
  | Error _ -> print_endline "Lexing error"

let run_pattern_parsing input =
  run_generic_parsing Pattern_parser.parse_pattern
    (fun (pattern, _) -> Ast.pattern_to_string pattern)
    input

let display_defs defs =
  String.concat "\n" @@ List.map (fun def -> Ast.definition_to_string def) defs

let run_definition_parsing input =
  run_generic_parsing Parser.parse_file display_defs input

let%expect_test "simple parsing literal pattern" =
  run_pattern_parsing {| "asdf" |};
  [%expect {| literal:"asdf" |}]

let%expect_test "simple parsing either pattern" =
  run_pattern_parsing {| "asdf" & "other" | "should prioritize either" |};
  [%expect
    {| multiple[literal:"asdf"; either(literal:"other", literal:"should prioritize either")] |}]

let%expect_test "simple parsing parens" =
  run_pattern_parsing {| ("asdf" & "other") | "should prioritize either" |};
  [%expect
    {| either(multiple[literal:"asdf"; literal:"other"], literal:"should prioritize either") |}]

let%expect_test "simple parsing parens 2" =
  run_pattern_parsing {| ("asdf" & "other") | ("first: " & variable & "!") |};
  [%expect
    {| either(multiple[literal:"asdf"; literal:"other"], multiple[literal:"first: "; multiple[var:variable; literal:"!"]]) |}]

let%expect_test "simple parsing postfix" =
  run_pattern_parsing {| "asdf"? |};
  [%expect {| optional(literal:"asdf") |}]

let%expect_test "simple parsing parens then optional" =
  run_pattern_parsing {| ("asdf" & "other")? |};
  [%expect {| optional(multiple[literal:"asdf"; literal:"other"]) |}]

let%expect_test "parsing basic pattern" =
  run_definition_parsing
    {| 
pattern awesome_pattern = ("asdf" & "other")? & "next"
  |};
  [%expect {| pattern:[name=awesome_pattern] = multiple[optional(multiple[literal:"asdf"; literal:"other"]); literal:"next"] |}]

let%expect_test "parsing basic function definition" =
  run_definition_parsing
    {| 
def main(
	(("thing" & variable)? & "other") | "single"
) = "hello " & name
  |};
  [%expect {| function:[name=main;args=either(multiple[optional(multiple[literal:"thing"; var:variable]); literal:"other"], literal:"single")] = concat(string:"hello ", var:name) |}]
