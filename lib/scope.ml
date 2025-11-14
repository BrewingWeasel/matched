module VariableMap = Map.Make (String)

type scope = { variables : Value.value VariableMap.t }

let empty_scope = { variables = VariableMap.empty }

let merge_scopes scope1 scope2 =
  let merged_variables =
    VariableMap.union (fun _ v1 _ -> Some v1) scope1.variables scope2.variables
  in
  { variables = merged_variables }

let rec find_var scopes name =
  match scopes with
  | [] -> None
  | scope :: rest -> (
      match VariableMap.find_opt name scope.variables with
      | Some value -> Some value
      | None -> find_var rest name)
