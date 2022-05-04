open Storage
open! Lwt
open Utils
open Dream
module JSON = Yojson.Safe

let ( let* ) = Lwt.bind
let make_log_jsons = [%to_yojson: log_stored list]
let make_log_jsons = make_log_jsons >> JSON.to_string
let port = env "PORT" "8080" |> int_of_string

let get_logs _ =
  let* logs = DB.read_all_logs () in
  let logs = logs |> make_log_jsons in
  print_endline "Client logs read";

  json logs

let get_root _ = html "Up and running..."

let start_server () =
  run ~interface:"0.0.0.0" ~port
  @@ logger
  @@ router
       [
         get "/" get_root;
         get "/logs" get_logs;
         post "/logs" (fun req ->
             let%lwt body = Dream.body req in
             let input_log_option = log_of_yojson (JSON.from_string body) in
             match input_log_option with
             | Ok log ->
                 let* _ = DB.insert_log_db log in
                 Dream.respond ~code:203 "ok"
             | Error e -> Dream.respond ~code:403 @@ "Invalid json input: " ^ e);
       ]
