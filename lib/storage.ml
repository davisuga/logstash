module type DB = Rapper_helper.CONNECTION

open Utils

exception Query_failed of string

let ( >> ) f g x = g (f x)
let ( $ ) f x = f x

let tap fn stuff =
  fn stuff;
  stuff

exception Connection_failed

type log = { stack : string; message : string; level : string; origin : string }
[@@deriving show, yojson]

type log_stored = {
  stack : string;
  message : string;
  level : string;
  origin : string;
  created_at : int;
  id : string;
}
[@@deriving show, yojson]

let print_log_stored = Format.printf "%a@." pp_log_stored
let create_query_failed s = Query_failed s

open Lwt.Infix

let flip fn a b = fn b a
let ( let* ) = Lwt.bind

(* open Crud *)

let add_db_details ({ stack; message; level; origin } : log) =
  let id = Uuidm.v `V4 |> Uuidm.to_string in
  let created_at = Unix.time () |> int_of_float in

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

  let or_die where = function
    | Ok r -> Lwt.return r
    | Error (i, e) -> Lwt.fail_with @@ sprintf "%s: (%d) %s" where i e

  let log_of_row (row : M.Field.t M.Row.StringMap.t) =
    let message = M.Row.StringMap.find "message" row |> M.Field.string in
    let level = M.Row.StringMap.find "level" row |> M.Field.string in
    let origin = M.Row.StringMap.find "origin" row |> M.Field.string in
    let stack = M.Row.StringMap.find "stack" row |> M.Field.string in
    let created_at = M.Row.StringMap.find "created_at" row |> M.Field.int in
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
    let* mariadb = connect () >>= or_die "Failed to connect" in
    let* stmt = M.prepare mariadb query >>= or_die "Failed to prepare" in
    M.Stmt.execute stmt variables >>= or_die "Failed to execute with" >>= stream

  let dispatch q v = try dispatch q v with Failure _ -> Lwt.fail_with ""

  let insert_log_
      ({ stack; message; level; origin; id; created_at } : log_stored) =
    print_log_stored { stack; message; level; origin; id; created_at };
    dispatch "INSERT INTO logs VALUES(?, ?, ?, ?, ?, ?)"
      [|
        `String id;
        `Int created_at;
        `String message;
        `String level;
        `String origin;
        `String stack;
      |]

  let insert_log_db = add_db_details >> insert_log_

  let read_all_logs () =
    let rows = ref [] in
    let* _ =
      dispatch "SELECT * FROM logs" [||]
      >|= Lwt_stream.iter_p (fun row ->
              rows := log_of_row row :: !rows;
              Lwt.return_unit)
    in

    Lwt.return !rows
end

module DB = MariaDB
type form = {
  name : string;
  email : string;
  ra : string;
}[@@deriving show, yojson]
module KV = struct
  open! Lwt.Syntax

  let conn = Redis_lwt.Client.connect {host ="localhost"; port=6379} |> Lwt_main.run
  let comm = ["GRAPH.QUERY"; "MotoGP" ;"match (n:Rider) return n"]

  let get_forms () = Redis_lwt.Client.lrange conn "forms" 0 (-1)

  let save_form = Redis_lwt.Client.lpush conn "forms"
  
  let run = Redis_lwt.Client.send_request conn
  let rec string_of_reply: Redis_lwt.Client.reply -> string = function
  | `Status s -> Printf.sprintf "(Status %s)" s
  | `Moved {slot; host; port} -> Printf.sprintf "MOVED %d %s:%i" slot host port
  | `Ask {slot; host; port} -> Printf.sprintf "ASK %d %s:%i" slot host port
  | `Error  s -> Printf.sprintf "(Error %s)" s
  | `Int i -> Printf.sprintf "(Int %i)" i
  | `Int64 i -> Printf.sprintf "(Int64 %Li)" i
  | `Bulk None -> "(Bulk None)"
  | `Bulk (Some s) -> Printf.sprintf "(Bulk (Some %s))" s
  | `Multibulk replies ->
    let x = List.map string_of_reply replies |> String.concat "; " in
    Printf.sprintf "Multibulk [ %s; ]" x
 
  let rec json_of_reply :Redis_lwt.Client.reply -> Yojson.Safe.t = function
  | `Status s -> `String s
  | `Moved {slot; host; port} -> `Assoc ["moved",`Assoc [ "slot" , `Int slot; "host" , `String host; "port" , `Int port ]]
  | `Ask {slot; host; port} -> `Assoc ["ask", `Assoc [ "slot" , `Int slot; "host" , `String host; "port" , `Int port ]]
  | `Error  s -> `Assoc ["error", `String s]
  | `Int i -> `Int i
  | `Int64 i -> `Int (i |> Int64.to_int)
  | `Bulk None -> `Null
  | `Bulk (Some s) -> `String s 
  
  (* | `Multibulk [`Multibulk [`Bulk (Some key) ; replyv]] -> `Assoc [key, json_of_reply replyv] *)
  | `Multibulk  [ `Multibulk properties] -> `List (properties |> List.map json_of_reply) 
  | `Multibulk replies -> `List (replies |> List.map json_of_reply)
  let get_g () = run comm >|= json_of_reply
    
end