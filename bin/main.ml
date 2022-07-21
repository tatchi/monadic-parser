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

let () =
  let p = P.bool true in
  let res = P.parse "true" p in
  assert (res = Ok true);
  let p = P.bool false in
  let res = P.parse "false" p in
  assert (res = Ok false);
  let res = P.parse "falsy" p in
  assert (res = Error { desc = "Expected bool `false`"; pos = 0 });
  let res = P.parse "true" p in
  assert (res = Error { desc = "Expected bool `false`"; pos = 0 })

let () =
  let open P.O in
  let p = P.many @@ P.string "hello" in
  let res = P.parse "hellohellohelloworld" p in
  assert (res = Ok [ "hello"; "hello"; "hello" ]);
  let res = P.parse "world" p in
  assert (res = Ok []);
  let p = P.many (P.string "hello") <*> P.string " world" in
  let res = P.parse "hellohellohello world" p in
  assert (res = Ok ([ "hello"; "hello"; "hello" ], " world"));
  let is_digit = function
    | '0' .. '9' -> true
    | _ -> false
  in
  let digits = P.parse_while is_digit in
  let p =
    digits
    <*> P.many (P.space *> digits)
    |> P.map ~f:(fun (x, xs) -> x :: xs)
    |> P.map ~f:(List.map int_of_string)
  in
  let res = P.parse "54 123 45 99" p in
  assert (res = Ok [ 54; 123; 45; 99 ]);
  let p = P.many @@ P.string "hello" <* P.end_ in
  let res = P.parse "hellohellohelloworld" p in
  assert (
    res
    = Error { desc = "Expected end of input, instead got: `world`"; pos = 15 })
