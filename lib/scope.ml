open Location

module VariableMap = Map.Make (String)

type scope = {
  variables : Value.value VariableMap.t;
  global_patterns : Ast.pattern VariableMap.t;
}

let new_scope global_patterns =
  {
    variables = VariableMap.empty;
    global_patterns = VariableMap.map (fun (_name, pattern) -> pattern.value) global_patterns;
  }

let merge_scopes scope1 scope2 =
  let merged_variables =
    VariableMap.union (fun _ v1 _ -> Some v1) scope1.variables scope2.variables
  in
  { variables = merged_variables; global_patterns = scope1.global_patterns }

let rec find_var scopes name =
  match scopes with
  | [] -> None
  | scope :: rest -> (
      match VariableMap.find_opt name scope.variables with
      | Some value -> Some value
      | None -> find_var rest name)
