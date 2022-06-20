let with_msg ?(arg = "") msg any =
  print_endline (msg ^ arg);
  any

(* module String = struct
     let catStringWith separator strA strB = strA ^ separator ^ strB
     let joinArray separator arr = Array.fold_left (catStringWith separator) "" arr
     let joinList separator arr = List.fold_left (catStringWith separator) "" arr
     let replace input output = Str.global_replace (Str.regexp_string input) output
   end

   module File = struct
     let read name =
       (* Adapted from https://stackoverflow.com/a/23456034 *)
       let channel = open_in name in
       let try_read () =
         try Some (input_line channel) with End_of_file -> None
       in
       let rec loop acc =
         match try_read () with
         | Some s -> loop (s :: acc)
         | None ->
             close_in channel;
             List.rev acc
       in
       loop []
   end

   module StringMap = Map.Make (Stdlib.String) *)

(* module Env = struct
     let getUnixVar name =
       try Ok (Sys.getenv name)
       with Not_found -> Error ("Variable " ^ name ^ " not found")

     let readDotEnv () =
       File.read "./.env" |> List.to_seq
       |> Seq.map (fun line ->
              match Stdlib.String.split_on_char '=' line with
              | [ name; value ] -> (name, value)
              | _ -> ("", ""))
       |> StringMap.of_seq

     let getVar name =
       match getUnixVar name with
       | Ok s -> Ok s
       | Error e -> (
           try Ok (StringMap.find name (readDotEnv ())) with Not_found -> Error e)
   end *)
let env var def =
  try Sys.getenv var with
  | Not_found -> def

let env_exn var =
  try Sys.getenv var with
  | Not_found -> failwith (__LOC__ ^ ": Environment variable " ^ var ^ " not found")
