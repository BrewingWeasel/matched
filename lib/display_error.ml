type error_level = Warning | Error | Context

type highlight = {
  error_level : error_level;
  start_pos : int;
  end_pos : int;
  message : string;
}

type error = {
  level : error_level;
  start_pos : int;
  message : string;
  highlights : highlight list;
}

type file_line = { start_pos : int; end_pos : int; content : string }

let to_file_map text =
  let rec rec_to_file_map chars start_pos acc current_acc =
    match Seq.uncons chars with
    | Some ((i, '\n'), rest) ->
        rec_to_file_map rest (i + 1)
          ({ start_pos; end_pos = i; content = current_acc } :: acc)
          ""
    | None ->
        Array.of_list
        @@ List.rev
             ({
                start_pos;
                end_pos = start_pos + String.length current_acc;
                content = current_acc;
              }
             :: acc)
    | Some ((_, char), rest) ->
        rec_to_file_map rest start_pos acc (current_acc ^ String.make 1 char)
  in
  rec_to_file_map (String.to_seqi text) 0 [] ""

let find_line file_map pos =
  let rec rec_find_line low high =
    if low > high then None
    else
      let mid = (low + high) / 2 in
      let line = file_map.(mid) in
      if pos < line.start_pos then rec_find_line low (mid - 1)
      else if pos >= line.end_pos then rec_find_line (mid + 1) high
      else Some (mid + 1, line)
  in
  rec_find_line 0 (Array.length file_map - 1)

let error_level_to_info = function
  | Warning -> ("Warning", "\x1b[1;33m")
  | Error -> ("Error", "\x1b[1;31m")
  | Context -> ("Context", "\x1b[1;35m")

let container_style = "\x1b[0;34m"

let digits = function
  | 0 -> 1
  | x -> x |> float_of_int |> log10 |> floor |> ( +. ) 1. |> int_of_float

let rec render_highlight_lines n line current_pos output = function
  | (highlight : highlight) :: rest ->
      let on_line_start_pos =
        highlight.start_pos - line.start_pos - current_pos
      in

      let length = max 1 highlight.end_pos - highlight.start_pos in
      let gap_until_underline =
        match on_line_start_pos with
        | 0 -> ""
        | _ -> String.make on_line_start_pos ' '
      in
      let _, highlight_style = error_level_to_info highlight.error_level in
      output.(0) <-
        Printf.sprintf "%s%s%s%s\x1b[0m" (Array.get output 0)
          gap_until_underline highlight_style (String.make length '^');

      for i = 0 to n do
        let content =
          if i == n then
            Printf.sprintf "%s%s\x1b[0m" highlight_style highlight.message
          else
            let gap_until_next =
              match length - 1 with 0 -> "" | l -> String.make l ' '
            in
            Printf.sprintf "%s|%s" highlight_style gap_until_next
        in
        output.(i + 1) <-
          Array.get output (i + 1) ^ gap_until_underline ^ content
      done;
      render_highlight_lines (n - 1) line
        (on_line_start_pos + length)
        output rest
  | [] -> ()

let render_error_highlight (highlights : highlight list) line_num line buffer
    indent =
  let indent_text = String.make indent ' ' in
  let post_line_number_indent = String.make (indent - digits line_num) ' ' in
  Printf.bprintf buffer "%s%s%s│\x1b[0m %s\n" container_style
    (string_of_int line_num) post_line_number_indent line.content;

  let output =
    Array.init
      (List.length highlights + 1)
      (fun _ -> Printf.sprintf "%s%s│\x1b[0m " container_style indent_text)
  in
  render_highlight_lines
    (List.length highlights - 1)
    line 0 output (List.rev highlights);
  Array.iter (fun s -> Printf.bprintf buffer "%s\n" s) output

let render_highlights highlights buffer indent file_map =
  let indent_text = String.make indent ' ' in
  let rec rec_render_highlights highlights buffer indent file_map last_line_num
      =
    match highlights with
    | [] -> ()
    | (highlight, line_num, line) :: rest ->
        let () =
          match last_line_num with
          | Some last when last + 1 < line_num ->
              Printf.bprintf buffer "%s%s·\n" indent_text container_style
          | _ -> ()
        in
        render_error_highlight highlight line_num line buffer indent;
        rec_render_highlights rest buffer indent file_map (Some line_num)
  in
  (* let highlights = *)
  (*   List.sort *)
  (*     (fun ((a, _, _) : highlight list * int * file_line) (b, _, _) -> *)
  (*       Int.compare a.start_pos b.start_pos) *)
  (*     highlights *)
  (* in *)
  rec_render_highlights highlights buffer indent file_map None

let rec preprocess_highlights highlights file_map current_max_indent acc =
  match highlights with
  | (highlight : highlight) :: rest -> (
      let start_line_opt = find_line file_map highlight.start_pos in
      match start_line_opt with
      | Some (line_num, line) ->
          let new_max_indent = max current_max_indent (digits line_num + 1) in

          let line_highlights =
            match acc with
            | (existing_highlights, existing_line_num, existing_line) :: rest
              when existing_line_num == line_num ->
                ( highlight :: existing_highlights,
                  existing_line_num,
                  existing_line )
                :: rest
            | acc -> ([ highlight ], line_num, line) :: acc
          in

          preprocess_highlights rest file_map new_max_indent line_highlights
      | None -> failwith "Highlight start position out of bounds")
  | [] -> (current_max_indent, acc)

let render_error error file_map file_name =
  let text, style = error_level_to_info error.level in
  let indent, highlights =
    preprocess_highlights error.highlights file_map 2 []
  in
  let indent_text = String.make indent ' ' in
  let buffer = Buffer.create 128 in
  Printf.bprintf buffer "%s%s:\x1b[0m \x1b[1m%s\x1b[0m\n" style text
    error.message;

  let line_num, line =
    match find_line file_map error.start_pos with
    | None -> failwith "Line number out of bounds"
    | Some res -> res
  in
  let col = error.start_pos - line.start_pos in
  Printf.bprintf buffer "%s%s╭─\x1b[0m %s:%s:%s\n%s%s│\n" indent_text
    container_style file_name (string_of_int line_num) (string_of_int col)
    container_style indent_text;

  render_highlights highlights buffer indent file_map;
  Buffer.contents buffer
