open Storage
open Lwt

open Dream
module JSON = Yojson.Safe

let ( let* ) = Lwt.bind
let make_log_jsons = [%to_yojson: log_stored list]
let make_log_jsons = make_log_jsons >> JSON.to_string

let get_logs _ =
  let* logs = DB.read_all_logs () in
  let logs = logs |> make_log_jsons in
  print_endline "Client logs read";

  json logs

let get_root _ = html "Up and running..."

let handle_invalid_input e =
  Dream.respond ~code:403 @@ "Invalid json input: " ^ e

let post_log req =
  let* parsed_log = Dream.body req >|= JSON.from_string >|= log_of_yojson in

  match parsed_log with
  | Ok log ->
      let* _ = DB.insert_log_db log in
      Dream.respond ~code:203 "ok"
  | Error e -> handle_invalid_input e

let post_form req =
  let* parsed_form = Dream.body req in
  
  let* resp_code =  KV.save_form [parsed_form] in
  Dream.html (resp_code |> string_of_int)

let all_forms _ =
  let* resp =  KV.get_forms () >|= String.concat "," in
  Dream.json (Printf.sprintf "[%s]" resp)

let all_riders _ =
  let* riders = KV.get_g() >|= Yojson.Safe.to_string in
  Dream.json riders

let start_server port =
  run ~interface:"0.0.0.0" ~port
  @@ router [post "/form" post_form; get "forms" all_forms;get "rides" all_riders;  get "/" get_root; get "/logs" get_logs; post "/logs" post_log ]
