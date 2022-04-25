open Opium

let ( let* ) = Lwt.bind

open Storage

let status =
  App.get "/" (fun _req ->
      Lwt.return (Response.of_plain_text "up and running..."))

let read_client_logs =
  App.get "/logs" (fun _request ->
      let* logs = Storage.read_all_logs () in
      let json = [%to_yojson: log_stored list] logs in
      Lwt.return (Response.of_json json))

let post_client_log =
  App.post "/logs" (fun req ->
      let* input_log = Request.to_json_exn req in
      let log =
        match log_stored_of_yojson input_log with
        | Ok log -> log
        | Error e -> raise (Invalid_argument e)
      in
      print_string log.message;
      Lwt.return (Response.of_plain_text "ok"))

let start_server () =
  App.empty |> read_client_logs |> post_client_log |> App.run_multicore
