open Ast
open Location
open Scope

type context = {
  char_updater : (char -> char) Option.t;
  trying_to_match : string;
}

let default_context = { char_updater = None; trying_to_match = "" }

let rec could_be_start_of_next scope = function
  | PVar _ -> fun _ -> true
  | PAs (_name, pattern) -> could_be_start_of_next scope pattern.value
  | PReference name -> (
      match VariableMap.find_opt name scope.global_patterns with
      | Some pattern -> could_be_start_of_next scope pattern
      | None -> fun _ -> false)
  | PLiteral lit -> (
      match lit |> String.to_seq |> Seq.uncons with
      | Some (c, _) -> fun x -> x = c
      | None -> fun _ -> false)
  | POptional p -> could_be_start_of_next scope p.value
  | PEither (p1, p2) ->
      let f1 = could_be_start_of_next scope p1.value in
      let f2 = could_be_start_of_next scope p2.value in
      fun x -> f1 x || f2 x
  | PMultiple (first :: _) -> could_be_start_of_next scope first.value
  | PMultiple [] -> fun _ -> false
  | PWithAttribute (p, _attribute) -> could_be_start_of_next scope p.value

let next_char seq ctx =
  match Seq.uncons seq with
  | None -> None
  | Some ((_, c), rest) ->
      let c = match ctx.char_updater with Some f -> f c | None -> c in
      Some (c, rest)

let rec match_literal actual pattern ctx scope =
  match (next_char actual ctx, Seq.uncons pattern) with
  | Some (a, rest_a), Some (p, rest_p) when a = p ->
      match_literal rest_a rest_p ctx scope
  | None, None -> Some (Seq.empty, scope)
  | Some _, None -> Some (actual, scope)
  | _, _ -> None

let rec match_variable inp ctx scope name acc could_be_end run_end =
  match next_char inp ctx with
  | None ->
      run_end inp
        {
          scope with
          variables = VariableMap.add name (Value.VString acc) scope.variables;
        }
  | Some (c, rest) ->
      if could_be_end c then
        let new_context =
          if acc = "" then scope
          else
            {
              scope with
              variables =
                VariableMap.add name (Value.VString acc) scope.variables;
            }
        in
        match run_end inp new_context with
        | None ->
            match_variable rest ctx scope name
              (acc ^ String.make 1 c)
              could_be_end run_end
        | v -> v
      else
        match_variable rest ctx scope name
          (acc ^ String.make 1 c)
          could_be_end run_end

let context_from_attribute ctx =
  let add_char_updater new_updater =
    match ctx.char_updater with
    | Some f -> Fun.compose new_updater f
    | None -> new_updater
  in
  function
  | "upper" ->
      { ctx with char_updater = Some (add_char_updater Char.uppercase_ascii) }
  | "lower" ->
      { ctx with char_updater = Some (add_char_updater Char.lowercase_ascii) }
  | _ -> default_context

let rec match_singular chars (ctx : context) scope pattern =
  match pattern with
  | PVar name ->
      match_variable chars ctx scope name ""
        (fun _ -> false)
        (fun _ scope -> Some (Seq.empty, scope))
  | PAs (name, p) -> (
      let start_at : int =
        match Seq.uncons chars with
        | Some ((i, _), _) -> i
        | None -> String.length ctx.trying_to_match
      in
      match match_singular chars ctx scope p.value with
      | Some (rest, new_scope) ->
          let end_at : int =
            match Seq.uncons rest with
            | Some ((i, _), _) -> i
            | None -> String.length ctx.trying_to_match
          in
          let matched_string =
            String.sub ctx.trying_to_match start_at (end_at - start_at)
          in
          let updated_scope =
            {
              new_scope with
              variables =
                VariableMap.add name.value (Value.VString matched_string)
                  scope.variables;
            }
          in
          Some (rest, updated_scope)
      | None -> None)
  | PReference name -> (
      match VariableMap.find_opt name scope.global_patterns with
      | Some pattern -> match_singular chars ctx scope pattern
      | None -> None)
  | PLiteral lit -> match_literal chars (String.to_seq lit) ctx scope
  | PEither (p1, p2) -> (
      match match_singular chars ctx scope p1.value with
      | Some v -> Some v
      | None -> match_singular chars ctx scope p2.value)
  | POptional p -> (
      match match_singular chars ctx scope p.value with
      | Some v -> Some v
      | None -> Some (chars, scope))
  | PMultiple patterns ->
      try_match chars ctx scope (List.map (fun p -> p.value) patterns)
  | PWithAttribute (pattern, attribute) ->
      match_singular chars
        (context_from_attribute ctx attribute.value)
        scope pattern.value

and try_match chars (ctx : context) scope = function
  | PVar name :: next :: rest ->
      match_variable chars ctx scope name "" (could_be_start_of_next scope next)
        (fun inp new_scope -> try_match inp ctx new_scope @@ (next :: rest))
  | PReference name :: rest -> (
      match VariableMap.find_opt name scope.global_patterns with
      | Some pattern -> try_match chars ctx scope (pattern :: rest)
      | None -> None)
  | PEither (first, second) :: rest -> (
      let result = try_match chars ctx scope (first.value :: rest) in
      match result with
      | Some v -> Some v
      | None -> try_match chars ctx scope (second.value :: rest))
  | POptional pattern :: rest -> (
      let result = try_match chars ctx scope (pattern.value :: rest) in
      match result with
      | Some v -> Some v
      | None -> try_match chars ctx scope rest)
  | PMultiple patterns :: rest -> (
      let result =
        try_match chars ctx scope (List.map (fun p -> p.value) patterns @ rest)
      in
      match result with Some v -> Some v | None -> None)
  | pattern :: rest ->
      continue rest ctx @@ match_singular chars ctx scope pattern
  | [] -> Some (Seq.empty, scope)

and continue rest ctx result =
  Option.bind result (fun (inp, scope) -> try_match inp ctx scope rest)

let run_match patterns text global_patterns =
  let result =
    try_match (String.to_seqi text)
      { default_context with trying_to_match = text }
      (new_scope global_patterns)
      patterns
  in
  Option.bind result (fun (inp, scope) ->
      if Seq.is_empty inp then Some scope else None)
