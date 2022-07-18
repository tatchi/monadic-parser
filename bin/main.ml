module P = Parcoom

let () =
  let open P.O in
  let p =
    let+ p1 = P.int 5 in
    Printf.sprintf "Got result: %i" p1
  in
  let res = P.parse "5" p in
  match res with
  | Ok s -> print_endline s
  | Error e -> failwith (P.Error.desc e)
