open I3ipc
open Lwt.Infix

module Assoc = struct
  let rec update key f = function
    | [] -> []
    | (k, v) :: l when key = k -> (k, f v) :: l
    | x :: l -> x :: update key f l
end

type orientation = H | V

let orientation_of_rect Reply.{ width; height; _ } =
  if width >= height then H else V

let string_of_orientation = function H -> "h" | V -> "v"

let wp_path_of_wsnum ?(orientation = H) wp_dir i =
  let p =
    Format.sprintf "%s/%d%s.jpg" wp_dir i
      (match orientation with H -> "h" | V -> "v")
  in
  if Sys.file_exists p then p else ""

let wp_path_of_output ~workspaces wp_dir (o : Reply.output) =
  match o.Reply.current_workspace with
  | None -> ""
  | Some ws ->
      let orientation = orientation_of_rect o.Reply.rect in
      let n =
        List.find_map
          (fun ({ Reply.name; num; _ } : Reply.workspace) ->
            if name = ws then Some num else None)
          workspaces
      in
      Option.map (wp_path_of_wsnum ~orientation wp_dir) n
      |> Option.value ~default:""

let wp_path_of_ws wp_dir (ws : Reply.workspace) =
  let orientation = orientation_of_rect ws.Reply.rect in
  wp_path_of_wsnum ~orientation wp_dir ws.Reply.num

let set_wallpapers wps =
  let cmd_args =
    "feh" :: "--no-fehbg"
    :: List.concat (List.map (fun (_rect, path) -> [ "--bg-fill"; path ]) wps)
  in
  let%lwt _ = Lwt_process.exec ("/usr/bin/feh", Array.of_list cmd_args) in
  Lwt.return wps

let set_wp_for_output (wps_s, set_wps) output wp =
  set_wps ?step:None (Assoc.update output (fun _ -> wp) (React.S.value wps_s))

let handle_event ~outputs handler wps_r = function
  | Event.Shutdown _ -> true
  | Event.Workspace { change = Focus; current = Some current; _ } ->
      handler ~outputs wps_r current;
      false
  | _ -> false

let init_wps ~init_wp ~workspaces outputs =
  let paths =
    List.map
      (fun (o : Reply.output) -> (o.Reply.name, init_wp ~workspaces o))
      outputs
  in
  let wps, set_wps = React.S.create paths in
  let%lwt wps = Lwt_react.S.map_s set_wallpapers wps in
  Lwt.return (wps, set_wps)

type copts = { wp_dir : string }

let main_loop ~init_wp handler =
  Lwt_main.run
  @@ let%lwt i3 = connect () in
     let%lwt outputs =
       let%lwt outputs =
         get_outputs i3 >|= List.filter (fun o -> o.Reply.active)
       in
       let%lwt monitors =
         let%lwt xrandr_output =
           Lwt_process.pread_lines (Lwt_process.shell "xrandr --listmonitors")
           |> Lwt_stream.to_list
         in
         match xrandr_output with
         | [] -> failwith "no monitors found"
         | _ :: l ->
             Lwt.return
             @@ List.map
                  (fun s ->
                    let fields = String.split_on_char ' ' s in
                    match fields with
                    | [ _empty; _num; _id; _spec; _empty2; name ] -> name
                    | _ -> failwith "monitor description format error")
                  l
       in
       Lwt.return
       @@ List.map
            (fun name ->
              List.find (fun (o : Reply.output) -> o.Reply.name = name) outputs)
            monitors
     in
     let%lwt workspaces = get_workspaces i3 in
     let%lwt wps_r = init_wps ~init_wp ~workspaces outputs in
     let%lwt () = disconnect i3 in
     let%lwt i3evts = connect () in
     let disconnect () = disconnect i3evts in
     let%lwt { Reply.success; _ } = subscribe i3evts [ Workspace; Shutdown ] in
     if not success then disconnect ()
     else
       let rec loop () =
         let%lwt e = next_event i3evts in
         let shutdown = handle_event ~outputs handler wps_r e in
         if shutdown then disconnect () else loop ()
       in
       loop ()

let get_output_from_rect ~r outputs =
  List.find
    (fun ({ rect; _ } : Reply.output) ->
      r.Reply.x = rect.Reply.x && r.Reply.y = rect.Reply.y)
    outputs

let random_wallpapers { wp_dir } =
  Random.self_init ();
  let get_random_wp rect =
    let orientation = orientation_of_rect rect |> string_of_orientation in
    let all_files = Sys.readdir (Printf.sprintf "%s/%s" wp_dir orientation) in
    let random_index = Random.int (Array.length all_files) in
    let path =
      Printf.sprintf "%s/%s/%s" wp_dir orientation all_files.(random_index)
    in
    path
  in
  let init_wp ~workspaces:_ (o : Reply.output) = get_random_wp o.Reply.rect in
  main_loop ~init_wp @@ fun ~outputs wps_r (ws : Reply.node) ->
  let random_wp = get_random_wp ws.Reply.rect in
  let output = get_output_from_rect ~r:ws.Reply.rect outputs in
  set_wp_for_output wps_r output.Reply.name random_wp

let assign_wallpapers { wp_dir } =
  let init_wp ~workspaces (o : Reply.output) =
    wp_path_of_output ~workspaces wp_dir o
  in
  main_loop ~init_wp @@ fun ~outputs wps_r (ws : Reply.node) ->
  let ws_num = ws.Reply.num in
  match ws_num with
  | None -> ()
  | Some num ->
      let rect = ws.Reply.rect in
      let orientation = orientation_of_rect rect in
      let path = wp_path_of_wsnum ~orientation wp_dir num in
      let output = get_output_from_rect ~r:ws.Reply.rect outputs in
      set_wp_for_output wps_r output.Reply.name path

open Cmdliner

let copts wp_dir = { wp_dir }

let copts_t =
  let docs = Manpage.s_common_options in
  let wp_dir =
    let doc = "The directory to pick wallpapers from" in
    Arg.(value & opt dir "~/wp" & info [ "dir"; "d" ] ~docs ~doc)
  in
  Term.(const copts $ wp_dir)

let rand_t = Term.(const random_wallpapers $ copts_t)

let rand_i =
  let doc = "Change the wallpaper every time you focus an i3 workspace" in
  Term.info ~doc "random"

let assign_t = Term.(const assign_wallpapers $ copts_t)

let assign_i =
  let doc = "Use specific wallpapers for each workspace" in
  Term.info ~doc "assign"

let prog_i =
  let doc = "A tool to change wallpapers for each workspace in i3" in
  Term.info ~version:"1.0.0" ~doc "i3wpd"

let () =
  Term.exit
  @@ Term.eval_choice (rand_t, prog_i)
       [ (rand_t, rand_i); (assign_t, assign_i) ]
