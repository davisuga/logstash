module type DB = Rapper_helper.CONNECTION

exception Query_failed of string
let (>>) f g x = g(f(x));;


let ( $ ) f x = f x

exception Connection_failed

type log = { stack : string; message : string; level : string; origin : string }
[@@deriving show, yojson]

type log_stored = {
  stack : string;
  created_at : float;
  message : string;
  level : string;
  origin : string;
  id : string;
}
[@@deriving show, yojson]

let print_log_stored = Format.printf "%a@." pp_log_stored
let create_query_failed s = Query_failed s

open Lwt.Infix

let flip fn a b = fn b a
let ( let* ) = Lwt.bind

(* open Crud *)
let db_url =
  match Utils.Env.getVar "DATABASE_URL" with
  | Ok url -> Uri.of_string url
  | Error e -> failwith e

let add_db_details ({ stack; message; level; origin } : log) =
  let id = Uuidm.v `V4 |> Uuidm.to_string in
  let created_at = Unix.time () in

  { stack; message; level; origin; id; created_at }

module MariaDB = struct
  open Printf
  module S = Mariadb.Nonblocking.Status

  module M = Mariadb.Nonblocking.Make (struct
    module IO = struct
      type 'a future = 'a Lwt.t

      let ( >>= ) = ( >>= )
      let return = Lwt.return
    end

    let wait mariadb status =
      let fd = Lwt_unix.of_unix_file_descr @@ Mariadb.Nonblocking.fd mariadb in
      assert (S.read status || S.write status || S.timeout status);
      let idle, _ = Lwt.task () in
      let rt = if S.read status then Lwt_unix.wait_read fd else idle in
      let wt = if S.write status then Lwt_unix.wait_write fd else idle in
      let tt =
        match (S.timeout status, Mariadb.Nonblocking.timeout mariadb) with
        | true, 0 -> Lwt.return ()
        | true, tmout -> Lwt_unix.timeout (float tmout)
        | false, _ -> idle
      in
      Lwt.catch
        (fun () ->
          Lwt.nchoose [ rt; wt; tt ] >>= fun _ ->
          Lwt.return
          @@ S.create ~read:(Lwt_unix.readable fd) ~write:(Lwt_unix.writable fd)
               ())
        (function
          | Lwt_unix.Timeout -> Lwt.return @@ S.create ~timeout:true ()
          | e -> Lwt.fail e)
  end)

  let env var def = try Sys.getenv var with Not_found -> def

  let or_die where = function
    | Ok r -> Lwt.return r
    | Error (i, e) -> Lwt.fail_with @@ sprintf "%s: (%d) %s" where i e

  let print_row row =
    Lwt_io.printf "---\n%!" >>= fun () ->
    M.Row.StringMap.fold
      (fun name field _ ->
        Lwt_io.printf "%20s " name >>= fun () ->
        match M.Field.value field with
        | `Int i -> Lwt_io.printf "%d\n%!" i
        | `Float x -> Lwt_io.printf "%f\n%!" x
        | `String s -> Lwt_io.printf "%s\n%!" s
        | `Bytes b -> Lwt_io.printf "%s\n%!" (Bytes.to_string b)
        | `Time t ->
            Lwt_io.printf "%04d-%02d-%02d %02d:%02d:%02d\n%!" (M.Time.year t)
              (M.Time.month t) (M.Time.day t) (M.Time.hour t) (M.Time.minute t)
              (M.Time.second t)
        | `Null -> Lwt_io.printf "NULL\n%!")
      row Lwt.return_unit

  let get_field ~as_type row fieldname =
    let field = M.Row.StringMap.find "message" row in
    match M.Field.value field with
    | `Int i -> Lwt_io.printf "%d\n%!" i
    | `Float x -> Lwt_io.printf "%f\n%!" x
    | `String s -> Lwt_io.printf "%s\n%!" s
    | `Bytes b -> Lwt_io.printf "%s\n%!" (Bytes.to_string b)
    | `Time t ->
        Lwt_io.printf "%04d-%02d-%02d %02d:%02d:%02d\n%!" (M.Time.year t)
          (M.Time.month t) (M.Time.day t) (M.Time.hour t) (M.Time.minute t)
          (M.Time.second t)
    | `Null -> Lwt_io.printf "NULL\n%!"

  let log_of_row (row : M.Field.t M.Row.StringMap.t) =
    let message = M.Row.StringMap.find "message" row |> M.Field.string in
    let level = M.Row.StringMap.find "level" row |> M.Field.string in
    let origin = M.Row.StringMap.find "origin" row |> M.Field.string in
    let stack = M.Row.StringMap.find "stack" row |> M.Field.string in
    let created_at = M.Row.StringMap.find "created_at" row |> M.Field.float in
    let id = M.Row.StringMap.find "id" row |> M.Field.string in
    let log = { stack; message; level; origin; id; created_at } in
    log

  let connect () =
    M.connect
      ~host:(env "OCAML_MARIADB_HOST" "localhost")
      ~user:(env "OCAML_MARIADB_USER" "root")
      ~pass:(env "OCAML_MARIADB_PASS" "")
      ~db:(env "OCAML_MARIADB_DB" "mysql")
      ~options:[ Ssl_ca "/etc/ssl/certs/ca-certificates.crt" ]
      ()

  let stream res =
    let next _ =
      M.Res.fetch (module M.Row.Map) res >>= function
      | Ok (Some _ as row) -> Lwt.return row
      | Ok None -> Lwt.return_none
      | Error _ -> Lwt.return_none
    in
    Lwt.return (Lwt_stream.from next)

  let dispatch query variables =
    connect () >>= or_die "Failed to connect" >>= fun mariadb ->
    M.prepare mariadb query >>= or_die "Failed to prepare" >>= fun stmt ->
    M.Stmt.execute stmt variables
    >>= or_die "Failed to execute with"
    >>= stream >>= Lwt_stream.to_list

  let dispatch q v = try
     dispatch q v
    with Failure f -> Lwt.fail_with ""

  let insert_log_
      ({ stack; message; level; origin; id; created_at } : log_stored) =
    dispatch
      "INSERT INTO logs (id, stack, message, level, origin, created_at) \
       VALUES(?, ?, ?, ?, ?, ?)"
      [|
        `String stack;
        `String message;
        `String level;
        `String origin;
        `Float created_at;
        `String id;
      |]
  let insert_log_db = add_db_details >> insert_log_
  let read_all_logs () =
    dispatch "SELECT * FROM logs" [||] >|= List.map log_of_row 
end

module DB = MariaDB


