open Ast
open Location
open Scope

let rec could_be_start_of_next = function
  | PVar _ -> fun _ -> true
  | PLiteral lit -> (
      match lit |> String.to_seq |> Seq.uncons with
      | Some (c, _) -> fun x -> x = c
      | None -> fun _ -> false)
  | POptional p -> could_be_start_of_next p.value
  | PEither (p1, p2) ->
      let f1 = could_be_start_of_next p1.value in
      let f2 = could_be_start_of_next p2.value in
      fun x -> f1 x || f2 x
  | PMultiple (first :: _) -> could_be_start_of_next first.value
  | PMultiple [] -> fun _ -> false

let rec match_literal actual pattern ctx =
  match (Seq.uncons actual, Seq.uncons pattern) with
  | Some (a, rest_a), Some (p, rest_p) when a = p ->
      match_literal rest_a rest_p ctx
  | None, None -> Some (Seq.empty, ctx)
  | Some (a, rest_a), None -> Some (Seq.append (Seq.singleton a) rest_a, ctx)
  | _, _ -> None

let rec match_variable inp ctx name acc could_be_end run_end =
  match Seq.uncons inp with
  | None ->
      run_end inp
        { variables = VariableMap.add name (Value.VString acc) ctx.variables }
  | Some (c, rest) ->
      if could_be_end c then
        let new_context =
          if acc = "" then ctx
          else
            {
              variables = VariableMap.add name (Value.VString acc) ctx.variables;
            }
        in
        match run_end inp new_context with
        | None ->
            match_variable rest ctx name
              (acc ^ String.make 1 c)
              could_be_end run_end
        | v -> v
      else
        match_variable rest ctx name
          (acc ^ String.make 1 c)
          could_be_end run_end

let rec match_singular chars ctx pattern =
  match pattern with
  | PVar name ->
      match_variable chars ctx name ""
        (fun _ -> false)
        (fun _ ctx -> Some (Seq.empty, ctx))
  | PLiteral lit -> match_literal chars (String.to_seq lit) ctx
  | PEither (p1, p2) -> (
      match match_singular chars ctx p1.value with
      | Some v -> Some v
      | None -> match_singular chars ctx p2.value)
  | POptional p -> (
      match match_singular chars ctx p.value with
      | Some v -> Some v
      | None -> Some (chars, ctx))
  | PMultiple patterns ->
      try_match chars ctx (List.map (fun p -> p.value) patterns)

and try_match chars ctx = function
  | PVar name :: next :: rest ->
      match_variable chars ctx name "" (could_be_start_of_next next)
        (fun inp new_ctx -> try_match inp new_ctx @@ (next :: rest))
  | PEither (first, second) :: rest -> (
      let result = try_match chars ctx (first.value :: rest) in
      match result with
      | Some v -> Some v
      | None -> try_match chars ctx (second.value :: rest))
  | POptional pattern :: rest -> (
      let result = try_match chars ctx (pattern.value :: rest) in
      match result with Some v -> Some v | None -> try_match chars ctx rest)
  | pattern :: rest -> continue rest @@ match_singular chars ctx pattern
  | [] -> Some (Seq.empty, ctx)

and continue rest result =
  Option.bind result (fun (inp, ctx) -> try_match inp ctx rest)

let run_match patterns text =
  let result = try_match (String.to_seq text) empty_scope patterns in
  Option.bind result (fun (inp, ctx) ->
      if Seq.is_empty inp then Some ctx else None)
