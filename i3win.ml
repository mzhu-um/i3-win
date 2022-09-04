open Yojson.Safe
open Yojson.Safe.Util

let (>>=) a b = Option.bind a b

let get_tree () = Unix.open_process_in "i3-msg -t get_tree"

let dump () =
  get_tree () |> In_channel.input_line |> Option.get |> print_endline

let get_tree_as_json () =
  get_tree () |> from_channel

let print_json t = 
  pretty_to_string t|> print_endline


let escape s =
  let open Str in
  global_replace (regexp {|&|}) "amp" s

let name_type t =
  let nm = t |> member "name" in
  let ty = t |> member "type" |> to_string in
  if nm = `Null
  then None
  else let nm = to_string nm in
       if nm = "content" || nm = "root" then None
       else (ty, (escape nm)) |> Option.some

type window =
  { name : string
  ; id : int
  ; idx : int
  }

type workspace =
  { name : string
  ; windows : (string * window) list
  }


let rec prune_tree spc t : (string * (workspace option)) list =
  let ty = t |> member "type" |> to_string in
  let next spc =
    t |> member "nodes" |> to_list |> List.map (prune_tree spc) |> List.flatten
  in
  match t |> member "name" with
  | `Null -> next spc
  | nm ->
     let nm = to_string nm in
     begin match ty with
     | "workspace" ->
        let wins =
          t
          |> member "nodes" |> to_list
          |> List.map (prune_workspace ("  " ^ spc) (ref 0)) |> List.flatten in
        [Printf.sprintf "%s<b>%s</b>" spc nm, Some {name = nm; windows = wins}]
     | "output"  ->
        if nm = "__i3" then []
        else (Printf.sprintf "%s<span foreground=\"purple\">%s</span>" spc nm, None) :: next ("  " ^ spc)
     | _ -> next spc            (* Dig deeper! *)
     end
and prune_workspace spc cntr t : (string * window) list =
  let next spc =
    t
    |> member "nodes" |> to_list |> List.map (prune_workspace spc cntr)
    |> List.flatten
  in
  match t |> member "name" with
  | `Null -> next spc
  | nm ->
     let name = to_string nm in
     let id = t |> member "id" |> to_int in
     let idx = ! cntr in
     cntr := idx + 1; 
     [Printf.sprintf "%s<tt>|%d|</tt> %s" spc idx name, {name; id; idx}]

let get_prompt_opt (wo : workspace option) =
  let open List in
  match wo with
  | None -> []
  | Some { name=_; windows } ->
     map fst windows

let get_prompt (l : (string * (workspace option)) list) =
  let open List in
  map (fun (s, wo) -> s :: get_prompt_opt wo) l |> flatten

let fuse_prompt sws : string =
  get_prompt sws |> String.concat "\n" |> escape

let focus_by_id id =
  let cmd = Printf.sprintf "i3-msg '[con_id=%d]' focus" id in
  Sys.command cmd

let distill_workspace (sws : (string * (workspace option)) list) : workspace list =
  List.(filter_map snd sws |> sort (fun w1 w2 -> String.compare w1.name w2.name))

let focus_by_idx (wksps : workspace list) wkidx winidx =
  let wksp =
    List.find (fun {name; _} -> int_of_string (String.sub name 0 1) = wkidx)
      wksps in
  let _, window = List.nth wksp.windows winidx in
  assert (winidx = window.idx) ;
  focus_by_id window.id

let read_dialog info =
  let cmd =
    Printf.sprintf "yad --text-width=80 --geometry=600x600+0+0 --title='the-i3win' --entry --text='<big>%s</big>'" info in
  let ch = Unix.open_process_in cmd in
  let str = In_channel.input_all ch in
  In_channel.close ch;
  Scanf.sscanf str "%d %d" (fun i j -> i, j)
  

let _ =
  let sws = get_tree_as_json () |> prune_tree "" in
  let wksps = distill_workspace sws in
  let info = fuse_prompt sws in
  let i, j = read_dialog info in
  focus_by_idx wksps i j
