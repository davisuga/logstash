let port = 
  try Sys.argv.(1) |> int_of_string
with _ -> 9000

let () = Logstash.Crud.start_server port
