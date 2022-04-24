open Opium

type log = {
  stack : string list;
  time : float;
  message : string;
  level : string;
  origin : string;
}
[@@deriving yojson]

let read_client_logs =
  App.get "/" (fun _req ->
      Lwt.return (Response.of_plain_text "up and running..."))

let start_server () = App.empty |> read_client_logs |> App.run_multicore
