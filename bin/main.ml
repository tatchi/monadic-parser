module P = Parser

let () = Printexc.record_backtrace true

let () =
  let p = P.string "hello" in
  let res = P.parse "helloworld" p in
  assert (res = Ok "hello")

let () =
  let open P.O in
  let p1 = P.string "hello" in
  let p2 = P.string " world" in
  let res = P.parse "hello world" (p1 *> p2) in
  assert (res = Ok " world")

let () =
  let open P.O in
  let p1 = P.string "hell" in
  let p2 = P.string " world" in
  let res = P.parse "hello world" (p1 *> p2) in
  assert (res = Error { desc = "Expected string ` world`"; pos = 4 })

let () =
  let open P.O in
  let p1 = P.string "hello" in
  let p2 = P.string " world" in
  let res = P.parse "hello world" (p1 <* p2) in
  assert (res = Ok "hello")

let () =
  let open P.O in
  let res = P.parse "hello12" (P.string "hello" *> P.int 12 <* P.end_) in
  assert (res = Ok 12)

let () =
  let open P.O in
  let res = P.parse "hello12rest" (P.string "hello" *> P.int 12 <* P.end_) in
  assert (
    res = Error { desc = "Expected end of input, instead got: `rest`"; pos = 7 })

let () =
  let open P.O in
  let p =
    let+ p1 = P.string "hello"
    and+ p2 = P.int 12
    and+ p3 = P.string "world" <* P.end_ in
    p1 ^ " " ^ string_of_int p2 ^ " " ^ p3
  in
  let res = P.parse "hello12world" p in
  assert (res = Ok "hello 12 world")

let () =
  let open P.O in
  let p =
    P.string "coucou " *> (P.string "hello" <|> P.string "world") <* P.end_
  in
  let res = P.parse "coucou hello" p in
  assert (res = Ok "hello");
  let res = P.parse "coucou world" p in
  assert (res = Ok "world");
  let res = P.parse "coucou orld" p in
  assert (
    res
    = Error
        { desc = "Expected string `hello` or Expected string `world`"; pos = 7 })

let () =
  let open P.O in
  let is_alpha_or_space = function
    | 'a' .. 'z' | 'A' .. 'Z' | ' ' -> true
    | _ -> false
  in
  let p =
    let+ s = P.parse_while is_alpha_or_space
    and+ n = P.int 12 in
    s ^ string_of_int n
  in
  let res = P.parse "hello 12world" p in
  assert (res = Ok "hello 12")

let () =
  let open P.O in
  let p = P.string "hello" *> P.optional (P.int 12) <* P.end_ in
  let res = P.parse "hello12" p in
  assert (res = Ok (Some 12));
  let res = P.parse "hello" p in
  assert (res = Ok None)

let () =
  let open P.O in
  let p = P.string "hello" <* P.not (P.string "world") in
  let res = P.parse "helloworld" p in
  assert (res = Error { desc = "Expected not to parse `world`"; pos = 5 });
  let res = P.parse "helloworl" p in
  assert (res = Ok "hello")
