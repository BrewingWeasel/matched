open Matched.Lexer
open Matched.Location

let display_lexed_tokens tokens =
  match tokens with
  | Ok tokens ->
      List.iter
        (fun v -> print_endline @@ to_string_located_span token_to_string v)
        tokens
  | Error _ -> Printf.printf "Lexing error\n"

let%expect_test "simple lexing" =
  display_lexed_tokens @@ lex "hi there & other | 23 thing \n\n\n test32 _yes_";
  [%expect
    {|
    Ident(hi){0:1}
    Ident(there){3:7}
    &{9:9}
    Ident(other){11:15}
    |{17:17}
    Number(23){19:20}
    Ident(thing){22:26}
    Ident(test32){32:37}
    Ident(_yes_){39:43}
    |}]

let%expect_test "lexing string" =
  display_lexed_tokens
  @@ lex {| "thing with a random \\ in the middle and then some \"quotes\"" |};
  [%expect
    {| String(thing with a random \ in the middle and then some "quotes"){1:63} |}]

let%expect_test "lexing empty string" =
  display_lexed_tokens @@ lex {| "" |};
  [%expect {| String(){1:2} |}]
