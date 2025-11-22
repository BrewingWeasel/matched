open Matched.Pattern_interpreter
open Matched.Ast
open Matched.Scope
open Matched.Location

let display_vars = function
  | Some { variables } ->
      VariableMap.iter
        (fun name value ->
          Printf.printf "%s: %s\n" name (Matched.Value.to_string value))
        variables
  | None -> print_endline "no match"

let with_span pattern = { value = pattern; start_pos = 0; end_pos = 0 }

let%expect_test "simple literal (match)" =
  display_vars (run_match [ PLiteral "literally!" ] "literally!");
  [%expect {| |}]

let%expect_test "simple literal (no match)" =
  display_vars (run_match [ PLiteral "literally!" ] "not literally!");
  [%expect {| no match |}]

let%expect_test "variable then literal (match)" =
  display_vars
    (run_match [ PVar "var"; PLiteral " is a variable" ] "x is a variable");
  [%expect {| var: x |}]

let%expect_test "literal variable then literal (match)" =
  display_vars
    (run_match
       [ PLiteral "It seems that "; PVar "var"; PLiteral " is a variable" ]
       "It seems that x is a variable");
  [%expect {| var: x |}]

let%expect_test "literal variable then literal (no match)" =
  display_vars
    (run_match
       [ PLiteral "It seems that "; PVar "var"; PLiteral " is a variable" ]
       "It seems that x is not a variable");
  [%expect {| no match |}]

let%expect_test "literal variable then literal (no match 2)" =
  display_vars
    (run_match
       [ PLiteral "It seems that "; PVar "var"; PLiteral " is a variable" ]
       "x is a variable");
  [%expect {| no match |}]

let%expect_test "simple either first (match)" =
  display_vars
    (run_match
       [
         PEither
           ( with_span @@ PLiteral "literally!",
             with_span @@ PLiteral "literally!" );
       ]
       "literally!");
  [%expect {| |}]

let%expect_test "simple either second (match)" =
  display_vars
    (run_match
       [
         PEither
           ( with_span @@ PLiteral "literally!",
             with_span @@ PLiteral "literally 2!" );
       ]
       "literally 2!");
  [%expect {| |}]

let%expect_test "either ends with variable" =
  display_vars
    (run_match
       [
         PLiteral "Value is: ";
         PEither (with_span @@ PLiteral "none", with_span @@ PVar "value");
       ]
       "Value is: 46");
  [%expect {| value: 46 |}]

let%expect_test "either ends with variable but is literal" =
  display_vars
    (run_match
       [
         PLiteral "Value is: ";
         PEither (with_span @@ PLiteral "none", with_span @@ PVar "value");
       ]
       "Value is: none");
  [%expect {| |}]

let%expect_test "either with variable then literal (as variable)" =
  display_vars
    (run_match
       [
         PEither (with_span @@ PLiteral "_", with_span @@ PVar "value");
         PLiteral " then literal";
       ]
       "46 then literal");
  [%expect {| value: 46 |}]

let%expect_test "either with variable then literal (as literal)" =
  display_vars
    (run_match
       [
         PEither (with_span @@ PLiteral "_", with_span @@ PVar "value");
         PLiteral " then literal";
       ]
       "_ then literal");
  [%expect {| |}]

let%expect_test "simple optional (optional missing)" =
  display_vars
    (run_match
       [
         PLiteral "hi";
         POptional (with_span @@ PLiteral ", there");
         PLiteral "!";
       ]
       "hi!");
  [%expect {| |}]

let%expect_test "simple optional (optional present)" =
  display_vars
    (run_match
       [
         PLiteral "hi"; POptional (with_span @@ PLiteral " there"); PLiteral "!";
       ]
       "hi there!");
  [%expect {| |}]

let%expect_test "optional variable at start (variable present)" =
  display_vars
    (run_match
       [
         POptional (with_span @@ PVar "introduction");
         PLiteral "Nice to meet you!";
       ]
       "hi! Nice to meet you!");
  [%expect {| introduction: hi!  |}]

let%expect_test "optional variable at start (variable missing)" =
  display_vars
    (run_match
       [
         POptional (with_span @@ PVar "introduction");
         PLiteral "Nice to meet you!";
       ]
       "Nice to meet you!");
  [%expect {| |}]

let%expect_test "simple multiple" =
  display_vars
    (run_match
       [
         PMultiple
           [
             with_span @@ PLiteral "hi";
             with_span @@ POptional (with_span @@ PLiteral ", there");
             with_span @@ PLiteral "!";
           ];
       ]
       "hi!");
  [%expect {| |}]

let%expect_test "multiple either" =
  display_vars
    (run_match
       [
         PEither
           ( with_span
             @@ PMultiple
                  [
                    with_span @@ PLiteral "hi";
                    with_span @@ POptional (with_span @@ PLiteral ", there");
                    with_span @@ PLiteral "!";
                  ],
             with_span
             @@ PMultiple
                  [
                    with_span @@ PLiteral "Yo ";
                    with_span @@ PVar "name";
                    with_span @@ PLiteral "!";
                  ] );
       ]
       "hi!");
  [%expect {| |}]
