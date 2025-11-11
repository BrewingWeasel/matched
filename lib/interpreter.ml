open Ast
module VariableMap = Map.Make (String)

type context = { matched_variables : string VariableMap.t }

let new_context = { matched_variables = VariableMap.empty }

let rec could_be_start_of_next = function
  | PVar _ -> fun _ -> true
  | PLiteral lit -> (
      match lit |> String.to_seq |> Seq.uncons with
      | Some (c, _) -> fun x -> x = c
      | None -> fun _ -> false)
  | POptional p -> could_be_start_of_next p
  | PEither (p1, p2) ->
      let f1 = could_be_start_of_next p1 in
      let f2 = could_be_start_of_next p2 in
      fun x -> f1 x || f2 x

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
        { matched_variables = VariableMap.add name acc ctx.matched_variables }
  | Some (c, rest) ->
      if could_be_end c then
        let new_context =
          if acc = "" then ctx
          else
            {
              matched_variables = VariableMap.add name acc ctx.matched_variables;
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
      match match_singular chars ctx p1 with
      | Some v -> Some v
      | None -> match_singular chars ctx p2)
  | POptional p -> (
      match match_singular chars ctx p with
      | Some v -> Some v
      | None -> Some (chars, ctx))

let rec try_match chars ctx = function
  | PVar name :: next :: rest ->
      match_variable chars ctx name "" (could_be_start_of_next next)
        (fun inp new_ctx -> try_match inp new_ctx @@ (next :: rest))
  | PEither (first, second) :: rest -> (
      let result = try_match chars ctx (first :: rest) in
      match result with
      | Some v -> Some v
      | None -> try_match chars ctx (second :: rest))
  | POptional pattern :: rest -> (
      let result = try_match chars ctx (pattern :: rest) in
      match result with Some v -> Some v | None -> try_match chars ctx rest)
  | pattern :: rest -> continue rest @@ match_singular chars ctx pattern
  | [] -> Some (Seq.empty, ctx)

and continue rest result =
  Option.bind result (fun (inp, ctx) -> try_match inp ctx rest)

let run_match patterns text =
  let result = try_match (String.to_seq text) new_context patterns in
  Option.bind result (fun (inp, ctx) ->
      if Seq.is_empty inp then Some ctx else None)
