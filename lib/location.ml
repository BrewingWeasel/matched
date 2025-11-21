type 'a located_span = {
    value: 'a;
    start_pos: int;
    end_pos: int;
}

let to_string_located_span to_string loc_span =
    Printf.sprintf "%s{%d:%d}" (to_string loc_span.value) loc_span.start_pos loc_span.end_pos
