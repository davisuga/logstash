open Opium
open Lwt

let ( let* ) = Lwt.bind

open Storage

let status =
  App.get "/" (fun _req ->
      Lwt.return (Response.of_plain_text "Up and running..."))

let make_log_jsons = [%to_yojson: log_stored list]

let read_client_logs =
  App.get "/logs" (fun _ ->
      print_endline "Reading client logs";
      let* logs = DB.read_all_logs () in
      let logs = logs |> make_log_jsons in

      print_endline "Client logs read";
      Lwt.return @@ Response.of_json logs)

let post_client_log =
  App.post "/logs" (fun req ->
      let* input_log_option = Request.to_json req in

      match input_log_option |> Option.map log_of_yojson with
      | Some (Ok log) ->
          let* r = DB.insert_log_db log in
          Lwt.return (Response.of_plain_text "ok")
      | None -> Lwt.return (Response.of_plain_text "No json here bro")
      | Some (Error e) ->
          Lwt.return (Response.of_plain_text @@ "Invalid json input: " ^ e))

let start_server () =
  App.empty |> App.port 80
  |> Utils.with_msg "Starting server at http://localhost:80"
  |> status |> post_client_log |> read_client_logs |> App.run_multicore
