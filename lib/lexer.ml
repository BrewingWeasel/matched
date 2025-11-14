type token =
  | TAmpersand
  | TPipe
  | TLParen
  | TRParen
  | TQuestionMark
  | TEquals
  | TComma
  | TDef
  | TPattern
  | TNumber of int
  | TIdent of string
  | TString of string

let token_to_string = function
  | TAmpersand -> "&"
  | TPipe -> "|"
  | TLParen -> "("
  | TRParen -> ")"
  | TQuestionMark -> "?"
  | TEquals -> "="
  | TComma -> ","
  | TDef -> "def"
  | TPattern -> "pattern"
  | TNumber n -> Printf.sprintf "Number(%d)" n
  | TIdent id -> Printf.sprintf "Ident(%s)" id
  | TString s -> Printf.sprintf "String(%s)" s

type error = UnexpectedCharacter of char | ExpectedCharacter of char

let error_to_string = function
  | UnexpectedCharacter c -> Printf.sprintf "Unexpected character: '%c'" c
  | ExpectedCharacter c -> Printf.sprintf "Expected character: '%c'" c

let is_digit c = Char.code c >= Char.code '0' && Char.code c <= Char.code '9'

let is_alphanumeric c =
  (Char.code c >= Char.code 'a' && Char.code c <= Char.code 'z')
  || (Char.code c >= Char.code 'A' && Char.code c <= Char.code 'Z')
  || is_digit c || c == '_'

let rec lex_number acc chars =
  match Seq.uncons chars with
  | Some (' ', _) | None -> Ok (acc, chars)
  | Some (c, rest) when is_digit c ->
      lex_number ((acc * 10) + (Char.code c - Char.code '0')) rest
  | Some (c, _) -> Error (UnexpectedCharacter c)

let rec lex_whitespace chars =
  match Seq.uncons chars with
  | Some (c, rest) when c = ' ' || c = '\t' || c = '\n' -> lex_whitespace rest
  | _ -> chars

let rec lex_ident acc chars =
  match Seq.uncons chars with
  | Some (c, _rest) when c = ' ' || c = '\t' || c = '\n' -> Ok (acc, chars)
  | Some (c, rest) when is_alphanumeric c ->
      lex_ident (acc ^ String.make 1 c) rest
  | Some (_, _) | None -> Ok (acc, chars)

let rec lex_string acc chars =
  match Seq.uncons chars with
  | None -> Error (ExpectedCharacter '"')
  | Some ('"', rest) -> Ok (acc, rest)
  | Some ('\\', rest) -> (
      match Seq.uncons rest with
      | None -> Error (ExpectedCharacter '"')
      | Some (escaped_char, rest_after_escape) ->
          let escaped =
            match escaped_char with
            | 'n' -> "\n"
            | 't' -> "\t"
            | '"' -> "\""
            | '\\' -> "\\"
            | other -> "\\" ^ String.make 1 other
          in
          lex_string (acc ^ escaped) rest_after_escape)
  | Some (c, rest) -> lex_string (acc ^ String.make 1 c) rest

let ( let* ) = Result.bind

let rec do_lex chars acc =
  match Seq.uncons chars with
  | None -> Ok acc
  | Some (c, rest) when is_digit c ->
      let* num, rest_chars = lex_number (Char.code c - Char.code '0') rest in
      do_lex rest_chars (TNumber num :: acc)
  | Some (c, rest) when c = ' ' || c = '\t' || c = '\n' ->
      let rest_chars = lex_whitespace rest in
      do_lex rest_chars acc
  | Some ('&', rest) -> do_lex rest (TAmpersand :: acc)
  | Some ('|', rest) -> do_lex rest (TPipe :: acc)
  | Some ('(', rest) -> do_lex rest (TLParen :: acc)
  | Some (')', rest) -> do_lex rest (TRParen :: acc)
  | Some ('?', rest) -> do_lex rest (TQuestionMark :: acc)
  | Some ('=', rest) -> do_lex rest (TEquals :: acc)
  | Some (',', rest) -> do_lex rest (TComma :: acc)
  | Some (c, rest) when is_alphanumeric c ->
      let* ident, rest_chars = lex_ident (String.make 1 c) rest in
      let token =
        match ident with
        | "def" -> TDef
        | "pattern" -> TPattern
        | _ -> TIdent ident
      in
      do_lex rest_chars (token :: acc)
  | Some ('"', rest) ->
      let* str_content, rest_chars = lex_string "" rest in
      do_lex rest_chars (TString str_content :: acc)
  | Some (c, _) -> Error (UnexpectedCharacter c)

let lex str = do_lex (String.to_seq str) [] |> Result.map List.rev
