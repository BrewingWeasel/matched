open Ast
open Lexer
open Parser_error

let ( let* ) = Result.bind

let get_binding_power token =
  match token with
  | TAmpersand -> Some (1, fun first second -> PMultiple [ first; second ])
  | TPipe -> Some (4, fun first second -> PEither (first, second))
  | _ -> None

let rec parse_binding_power tokens min_bp =
  match tokens with
  | [] -> Error UnexpectedEndOfInput
  | token :: rest ->
      let* left_expr, left_rest = parse_left token rest in
      parse_loop min_bp left_expr left_rest

and parse_left token rest =
  match token with
  | TString s -> Ok (Ast.PLiteral s, rest)
  | TIdent name -> Ok (Ast.PVar name, rest)
  | TLParen -> (
      match parse_binding_power rest 0 with
      | Ok (pattern, next_tokens) -> (
          match next_tokens with
          | TRParen :: after_paren -> Ok (pattern, after_paren)
          | _ -> Error (ExpectedToken [ TRParen ]))
      | Error e -> Error e)
  | _ -> Error (UnexpectedToken token)

and parse_loop min_bp left remaining_tokens =
  match remaining_tokens with
  | [] -> Ok (left, [])
  | TQuestionMark :: rest -> Ok (Ast.POptional left, rest)
  | token :: rest -> (
      match get_binding_power token with
      | Some (bp, make_expr) when bp >= min_bp ->
          let* right_expr, right_rest = parse_binding_power rest bp in
          Ok (make_expr left right_expr, right_rest)
      | _ -> Ok (left, remaining_tokens))

let parse_pattern tokens = parse_binding_power tokens 0
