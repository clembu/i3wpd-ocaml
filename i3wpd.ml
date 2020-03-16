open I3ipc
open Lwt.Infix

module RectMap = Map.Make (struct
  type t = Reply.rect

  let compare { Reply.x = x1; y = y1; _ } { Reply.x = x2; y = y2; _ } =
    compare (x1, y1) (x2, y2)
end)

let wp_path_of_wsnum wp_dir i =
  let p = Format.sprintf "%s/%d.jpg" wp_dir i in
  if Sys.file_exists p then Some p else None

let wp_path_of_output wp_dir (o : Reply.output) =
  ( o.Reply.rect,
    match o.Reply.current_workspace with
    | None -> None
    | Some ws -> (
        let n = String.split_on_char ':' ws in
        match n with
        | [] -> None
        | [ s ] -> (
            match int_of_string_opt s with
            | None -> None
            | Some i -> wp_path_of_wsnum wp_dir i )
        | s :: _ -> (
            match int_of_string_opt s with
            | None -> None
            | Some i -> wp_path_of_wsnum wp_dir i ) ) )

let wp_path_of_ws wp_dir (ws : Reply.workspace) =
  wp_path_of_wsnum wp_dir ws.Reply.num

let wp_path_of_node wp_dir (n : Reply.node) =
  Option.map (wp_path_of_wsnum wp_dir) n.Reply.num

let set_wallpapers wps =
  let cmd_args =
    "feh" :: "--no-fehbg"
    :: List.concat
         (List.of_seq
            (Seq.filter_map
               (fun (rect, path) ->
                 Format.printf "%a: %s@." Reply.pp_rect rect
                   (match path with None -> "" | Some p -> p);
                 match path with
                 | Some path -> Some [ "--bg-fill"; path ]
                 | _ -> None)
               (* (function
                *   | _, Some path -> Some [ "--bg-fill"; path ] | _ -> None) *)
               (RectMap.to_seq wps)))
  in
  print_endline (String.concat " " cmd_args);
  let%lwt _ = Lwt_process.exec ("/usr/bin/feh", Array.of_list cmd_args) in
  Lwt.return wps

let handle_ws_event wp_dir (wps_s, set_wps) (e : Event.workspace_event_info) =
  match (e.Event.change, e.Event.current) with
  | Event.Focus, Some cur_node -> (
      let ws_num = cur_node.Reply.num in
      match ws_num with
      | None -> Lwt.return_unit
      | Some num -> (
          let path = wp_path_of_wsnum wp_dir num in
          match path with
          | None -> Lwt.return_unit
          | Some path ->
              let rect = cur_node.Reply.rect in
              set_wps ?step:None
                (RectMap.update rect
                   (fun _ -> Some (Some path))
                   (React.S.value wps_s));
              Lwt.return_unit ) )
  | _ -> Lwt.return_unit

let handle_event wp_dir wps_r = function
  | Event.Shutdown _ -> Lwt.return_true
  | Event.Workspace e -> handle_ws_event wp_dir wps_r e >|= fun () -> false
  | _ -> Lwt.return_false

let init_wps wp_dir outputs =
  let paths = List.map (wp_path_of_output wp_dir) outputs in
  let wps, set_wps = React.S.create (RectMap.of_seq (List.to_seq paths)) in
  let%lwt wps = Lwt_react.S.map_s set_wallpapers wps in
  Lwt.return (wps, set_wps)

let program () =
  if Array.length Sys.argv < 2 then (
    prerr_endline "Missing directory argument";
    Lwt.return_unit )
  else
    let wp_dir = Sys.argv.(1) in
    let%lwt i3 = connect () in
    let%lwt outputs =
      get_outputs i3 >|= List.filter (fun o -> o.Reply.active)
    in
    let%lwt wps_r = init_wps wp_dir outputs in
    (* List.iter (Format.printf "%a@." Reply.pp_output) outputs; *)
    (* let outputs_s, set_outputs = React.S.create outputs in *)
    let%lwt i3evts = connect () in
    let disconnect () =
      let%lwt () = disconnect i3 in
      disconnect i3evts
    in
    let%lwt { Reply.success; _ } = subscribe i3evts [ Workspace; Shutdown ] in
    if not success then disconnect ()
    else
      let rec loop () =
        let%lwt e = next_event i3evts in
        let%lwt shutdown = handle_event wp_dir wps_r e in
        if shutdown then disconnect () else loop ()
      in
      loop ()

let () = Lwt_main.run (program ())
