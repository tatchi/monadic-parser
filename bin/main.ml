module P = Parcoom

let () =
  let p = P.string "hello" in
  let res = P.parse "helloworld" p in
  match res with
  | Ok s -> Printf.printf "result: s = %s\n" s
  | Error e ->
    failwith
      (Printf.sprintf "Error: %s. Position: %d" (P.Error.desc e) (P.Error.pos e))

let () =
  let open P.O in
  let p1 = P.string "hello" in
  let p2 = P.string " world" in
  let res = P.parse "hello world" (p1 *> p2) in
  match res with
  | Ok s -> Printf.printf "right: s = %s\n" s
  | Error e ->
    failwith
      (Printf.sprintf "Error: %s. Position: %d" (P.Error.desc e) (P.Error.pos e))

let () =
  let open P.O in
  let p1 = P.string "hello" in
  let p2 = P.string " world" in
  let res = P.parse "hello world" (p1 <* p2) in
  match res with
  | Ok s -> Printf.printf "left: s = %s\n" s
  | Error e ->
    failwith
      (Printf.sprintf "Error: %s. Position: %d" (P.Error.desc e) (P.Error.pos e))

let () =
  let open P.O in
  let p1 = P.string "hello" in
  let p2 = P.int 12 in
  let res = P.parse "hello123" (p1 *> p2) in
  match res with
  | Ok s -> Printf.printf "int: s = %i\n" s
  | Error e ->
    failwith
      (Printf.sprintf "Error: %s. Position: %d" (P.Error.desc e) (P.Error.pos e))
