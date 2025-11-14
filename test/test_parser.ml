open Matched
open Matched.Ast

let run_parsing input =
  match Lexer.lex input with
  | Ok tokens -> (
      match Pattern_parser.parse_pattern tokens with
      | Ok (pattern, _) -> print_endline @@ pattern_to_string pattern
      | Error e -> print_endline @@ Parser_error .error_to_string e)
  | Error _ -> print_endline "Lexing error"

let%expect_test "simple parsing literal pattern" =
  run_parsing {| "asdf" |};
  [%expect {| literal:"asdf" |}]

let%expect_test "simple parsing either pattern" =
  run_parsing {| "asdf" & "other" | "should prioritize either" |};
  [%expect
    {| multiple[literal:"asdf"; either(literal:"other", literal:"should prioritize either")] |}]

let%expect_test "simple parsing parens" =
  run_parsing {| ("asdf" & "other") | "should prioritize either" |};
  [%expect
    {| either(multiple[literal:"asdf"; literal:"other"], literal:"should prioritize either") |}]

let%expect_test "simple parsing parens 2" =
  run_parsing {| ("asdf" & "other") | ("first: " & variable & "!") |};
  [%expect
    {| either(multiple[literal:"asdf"; literal:"other"], multiple[literal:"first: "; multiple[var:variable; literal:"!"]]) |}]

let%expect_test "simple parsing postfix" =
  run_parsing {| "asdf"? |};
  [%expect {| optional(literal:"asdf") |}]

let%expect_test "simple parsing parens then optional" =
  run_parsing {| ("asdf" & "other")? |};
  [%expect {| optional(multiple[literal:"asdf"; literal:"other"]) |}]
