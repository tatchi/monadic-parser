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
  let res = P.parse "hello12" (P.string "hello" *> P.int 12 <* P.end_) in
  match res with
  | Ok s -> Printf.printf "int: s = %i\n" s
  | Error e ->
    failwith
      (Printf.sprintf "Error: %s. Position: %d" (P.Error.desc e) (P.Error.pos e))

let () =
  let open P.O in
  (* let p = P.string "hello" <*> P.int 12 <*> P.end_ in *)
  let p =
    let+ p1 = P.string "hello"
    and+ p2 = P.int 12
    and+ p3 = P.string "world" <* P.end_ in
    p1 ^ " " ^ string_of_int p2 ^ " " ^ p3
  in
  let res = P.parse "hello12world" p in
  match res with
  | Ok s -> Printf.printf "both: s = %s\n" s
  | Error e ->
    failwith
      (Printf.sprintf "Error: %s. Position: %d" (P.Error.desc e) (P.Error.pos e))

let () =
  let open P.O in
  let res =
    P.parse "coucou world"
      (P.string "coucou " *> P.either (P.string "hello") (P.string "world")
      <* P.end_)
  in
  match res with
  | Ok s -> Printf.printf "either: s = %s\n" s
  | Error e ->
    failwith
      (Printf.sprintf "Error: %s. Position: %d" (P.Error.desc e) (P.Error.pos e))

let () =
  let open P.O in
  let p =
    let+ s =
      P.parse_while (function
        | 'a' .. 'z' | ' ' -> true
        | _ -> false)
    and+ n = P.int 12 in
    s ^ (string_of_int n)
  in
  let res = P.parse "hello 12world" p in
  match res with
  | Ok s -> Printf.printf "parse_while: s = %s\n" s
  | Error e ->
    failwith
      (Printf.sprintf "Error: %s. Position: %d" (P.Error.desc e) (P.Error.pos e))
