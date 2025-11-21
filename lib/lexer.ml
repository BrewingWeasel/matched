open Location

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

let display_error (error, pos) =
  Display_error.render_error
    {
      level = Error;
      message = error_to_string error;
      start_pos = pos;
      highlights = [];
    }

let is_digit c = Char.code c >= Char.code '0' && Char.code c <= Char.code '9'

let is_alphanumeric c =
  (Char.code c >= Char.code 'a' && Char.code c <= Char.code 'z')
  || (Char.code c >= Char.code 'A' && Char.code c <= Char.code 'Z')
  || is_digit c || c == '_'

let rec lex_number acc chars last_pos =
  match Seq.uncons chars with
  | Some ((_, ' '), _) | None -> Ok (acc, last_pos, chars)
  | Some ((i, c), rest) when is_digit c ->
      lex_number ((acc * 10) + (Char.code c - Char.code '0')) rest i
  | Some ((i, c), _) -> Error (UnexpectedCharacter c, i)

let rec lex_whitespace chars =
  match Seq.uncons chars with
  | Some ((_, c), rest) when c = ' ' || c = '\t' || c = '\n' ->
      lex_whitespace rest
  | _ -> chars

let rec lex_ident acc chars last_pos =
  match Seq.uncons chars with
  | Some ((_, c), _rest) when c = ' ' || c = '\t' || c = '\n' ->
      Ok (acc, last_pos, chars)
  | Some ((i, c), rest) when is_alphanumeric c ->
      lex_ident (acc ^ String.make 1 c) rest i
  | Some (_, _) | None -> Ok (acc, last_pos, chars)

let rec lex_string acc chars last_i =
  match Seq.uncons chars with
  | None -> Error (ExpectedCharacter '"', last_i)
  | Some ((end_pos, '"'), rest) -> Ok (acc, end_pos, rest)
  | Some ((last_i, '\\'), rest) -> (
      match Seq.uncons rest with
      | None -> Error (ExpectedCharacter '"', last_i)
      | Some ((i, escaped_char), rest_after_escape) ->
          let escaped =
            match escaped_char with
            | 'n' -> "\n"
            | 't' -> "\t"
            | '"' -> "\""
            | '\\' -> "\\"
            | other -> "\\" ^ String.make 1 other
          in
          lex_string (acc ^ escaped) rest_after_escape i)
  | Some ((i, c), rest) -> lex_string (acc ^ String.make 1 c) rest i

let ( let* ) = Result.bind

let rec do_lex chars acc =
  let lex_symbol rest symbol i =
    do_lex rest ({ value = symbol; start_pos = i; end_pos = i } :: acc)
  in
  match Seq.uncons chars with
  | None -> Ok acc
  | Some ((start_pos, c), rest) when is_digit c ->
      let* num, end_pos, rest_chars =
        lex_number (Char.code c - Char.code '0') rest start_pos
      in
      do_lex rest_chars ({ value = TNumber num; start_pos; end_pos } :: acc)
  | Some ((_, c), rest) when c = ' ' || c = '\t' || c = '\n' ->
      let rest_chars = lex_whitespace rest in
      do_lex rest_chars acc
  | Some ((i, '&'), rest) -> lex_symbol rest TAmpersand i
  | Some ((i, '|'), rest) -> lex_symbol rest TPipe i
  | Some ((i, '('), rest) -> lex_symbol rest TLParen i
  | Some ((i, ')'), rest) -> lex_symbol rest TRParen i
  | Some ((i, '?'), rest) -> lex_symbol rest TQuestionMark i
  | Some ((i, '='), rest) -> lex_symbol rest TEquals i
  | Some ((i, ','), rest) -> lex_symbol rest TComma i
  | Some ((start_pos, c), rest) when is_alphanumeric c ->
      let* ident, end_pos, rest_chars =
        lex_ident (String.make 1 c) rest start_pos
      in
      let token =
        match ident with
        | "def" -> TDef
        | "pattern" -> TPattern
        | _ -> TIdent ident
      in
      do_lex rest_chars ({ value = token; start_pos; end_pos } :: acc)
  | Some ((start_pos, '"'), rest) ->
      let* str_content, end_pos, rest_chars = lex_string "" rest start_pos in
      do_lex rest_chars
        ({ value = TString str_content; start_pos; end_pos } :: acc)
  | Some ((i, c), _) -> Error (UnexpectedCharacter c, i)

let lex str = do_lex (String.to_seqi str) [] |> Result.map List.rev
