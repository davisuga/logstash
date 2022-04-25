let ( $ ) f x = f x

exception Query_failed of string

type log = { stack : string; message : string; level : string; origin : string }
[@@deriving yojson]

type log_stored = {
  stack : string;
  created_at : float;
  message : string;
  level : string;
  origin : string;
  id : string;
}
[@@deriving yojson]

let create_query_failed s = Query_failed s

let ensure_table_exists =
  [%rapper
    execute
      {sql|CREATE TABLE IF NOT EXISTS logs (
            id uuid PRIMARY KEY NOT NULL,
            created_at timestamp NOT NULL,
            message varchar NOT NULL,
            level varchar NOT NULL,
            origin varchar NOT NULL,
            stack varchar NOT NULL
        )|sql}]
    ()

open Caqti_lwt
open Lwt

let ( let* ) = bind

(* open Crud *)
let db_url = Uri.of_string (Unix.getenv "DATABASE_URL")

let pool =
  match Caqti_lwt.connect_pool ~max_size:10 db_url with
  | Ok pool -> pool
  | Error error -> failwith $ Caqti_error.show error

let dispatch f =
  let* result = Caqti_lwt.Pool.use f pool in
  match result with
  | Ok data -> Lwt.return data
  | Error error -> error |> Caqti_error.show |> create_query_failed |> Lwt.fail

let () = dispatch ensure_table_exists |> Lwt_main.run

let insert_log ({ stack; message; level; origin; id; created_at } : log_stored)
    =
  let insert =
    [%rapper
      execute
        {sql|
              INSERT INTO logs
              VALUES(%string{id}, %string{stack}, %string{message}, %string{level}, %string{origin}, %float{created_at})
            |sql}
        record_in]
  in
  dispatch (insert { stack; message; level; origin; created_at; id })

let add_db_details ({ stack; message; level; origin } : log) =
  let id = Uuidm.v `V4 |> Uuidm.to_string in
  let created_at = Unix.time () in

  { stack; message; level; origin; id; created_at }

let read_all_logs () =
  let read_all =
    [%rapper
      get_many
        {sql|
            SELECT @string{id}, @string{stack}, @string{message}, @string{level}, @string{origin}, @float{created_at}
            FROM logs
          |sql}
        record_out]
      ()
  in
  let* messages = dispatch read_all in
  messages
  (* |> List.map (fun { user_name; body; _ } -> { user_name; body }) *)
  |> Lwt.return
