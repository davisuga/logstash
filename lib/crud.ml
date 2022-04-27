open Opium

let ( let* ) = Lwt.bind

open Storage

let status =
  App.get "/" (fun _req ->
      Lwt.return (Response.of_plain_text "Up and running..."))

let make_log_jsons = [%to_yojson: log_stored list]

let read_client_logs =
  App.get "/logs" (fun _ ->
      print_endline "Reading client logs";
      let logs = DB.read_all_logs () in
      List.map print_log_stored logs;
      let json = make_log_jsons [] in
      Lwt.return (Response.of_json json))

let post_client_log =
  App.post "/logs" (fun req ->
      let* input_log = Request.to_json_exn req in
      let log =
        match log_of_yojson input_log with
        | Ok log -> log
        | Error e -> raise (Invalid_argument e)
      in
      print_string log.message;
      Lwt.return (Response.of_plain_text "ok"))

let start_server () =
  App.empty
  |> Utils.with_msg "Starting server at http://localhost:3000"
  |> status |> post_client_log |> read_client_logs |> App.run_multicore
