module Error = struct
  type t =
    { desc : string
    ; pos : int
    }
  [@@deriving show]

  let desc t = t.desc

  let pos t = t.pos

  let create desc pos = { desc; pos }
end

module Input = struct
  type t =
    { text : string
    ; pos : int
    }

  let text t = t.text

  let pos t = t.pos

  let sub ~start ~len { text; pos } =
    { text = String.sub text start len; pos = start + pos }

  let of_string text = { text; pos = 0 }

  let lsplit ~prefix t =
    try
      let length = String.length t.text in
      let prefix_len = String.length prefix in
      let input = sub ~start:0 ~len:prefix_len t in
      let rest = sub ~start:prefix_len ~len:(length - prefix_len) t in
      Some (rest, input)
    with Invalid_argument _ -> None

  let take_while p t =
    let length = String.length t.text in
    let i = ref 0 in
    let buff = Buffer.create length in
    while !i < length && p t.text.[!i] do
      Buffer.add_char buff t.text.[!i];
      incr i
    done;
    let prefix = Buffer.contents buff in
    let input = { text = prefix; pos = t.pos + !i } in
    let rest = sub ~start:!i ~len:(length - !i) t in
    (rest, input)
end

type 'a t = Input.t -> Input.t * ('a, Error.t) result

let return t input = (input, Ok t)

let is_number = function
  | '0' .. '9' -> true
  | _ -> false

(* let split_number n =
   let rec loop acc n =
     if n < 10 then n :: acc
     else
       let r = n mod 10 in
       let d = n / 10 in
       loop (r :: acc) d
   in
   loop [] n *)

let int n (input : Input.t) =
  let s = string_of_int n in
  let unexpected_int_error =
    Error.create (Printf.sprintf "Expected number %i" n) input.pos
  in
  match Input.lsplit ~prefix:s input with
  | None -> (input, Error unexpected_int_error)
  | Some (input', prefix_input) ->
    if prefix_input.text = s then (input', Ok n)
    else (input, Error unexpected_int_error)

let end_ (input : Input.t) =
  if String.length input.text = 0 then (input, Ok ())
  else
    ( input
    , Error
        (Error.create
           (Printf.sprintf "Expected end of input, instead got: `%s`" input.text)
           input.pos) )

let string s (input : Input.t) =
  let unexpected_prefix_error =
    Error.create (Printf.sprintf "Expected string `%s`" s) input.pos
  in
  match Input.lsplit ~prefix:s input with
  | None -> (input, Error unexpected_prefix_error)
  | Some (input', prefix_input) ->
    if prefix_input.text = s then (input', Ok s)
    else (input, Error unexpected_prefix_error)

let map p ~f input =
  match p input with
  | (_, Error _) as e -> e
  | input, Ok r -> (input, Ok (f r))

let bind p ~f input =
  match p input with
  | (_, Error _) as e -> e
  | input, Ok r -> (f r) input

let both p1 p2 input =
  match p1 input with
  | (_, Error _) as e -> e
  | input, Ok r1 -> (
    match p2 input with
    | (_, Error _) as e -> e
    | input, Ok r2 -> (input, Ok (r1, r2)))

let right p1 p2 input =
  match p1 input with
  | (_, Error _) as e -> e
  | input', Ok _ -> p2 input'

let left p1 p2 input =
  match p1 input with
  | (_, Error _) as e -> e
  | input', Ok r1 -> (
    match p2 input' with
    | (_, Error _) as e -> e
    | input'', Ok _ -> (input'', Ok r1))

let either p1 p2 (input : Input.t) =
  match p1 input with
  | (_, Ok _) as r -> r
  | _, Error (left_error : Error.t) -> (
    match p2 input with
    | (_, Ok _) as r -> r
    | _, Error (right_error : Error.t) ->
      ( input
      , Error
          (Error.create
             (Printf.sprintf "%s or %s" left_error.desc right_error.desc)
             input.pos) ))

let bool b input =
  let p = either (string "true") (string "false") in
  match p input with
  | input', Ok r when bool_of_string r = b -> (input', Ok b)
  | input', Error _ | input', Ok _ ->
    ( input'
    , Error (Error.create (Printf.sprintf "Expected bool `%b`" b) input.pos) )

let parse_while p input =
  let input', parsed = Input.take_while p input in
  (input', Ok parsed.text)

let optional p input =
  match p input with
  | _, Error _ -> (input, Ok None)
  | input', Ok r -> (input', Ok (Some r))

let not p (input : Input.t) =
  match p input with
  | _, Error _ -> (input, Ok ())
  | _, Ok _ ->
    ( input
    , Error
        (Error.create
           (* TODO: add a [to_string] fn in the type so that we can print what we parsed ? *)
           (Printf.sprintf "Expected not to parse `%s`" input.text)
           input.pos) )

let space = string " "

let many p input =
  let rec loop acc (input : Input.t) =
    match p input with
    | _, Error _ -> (input, Ok (acc |> List.rev))
    | input', Ok r -> loop (r :: acc) input'
  in
  loop [] input

module O = struct
  let ( let+ ) t f = map t ~f

  let ( let* ) t f = bind t ~f

  let ( and+ ) = both

  let ( <* ) = left

  let ( *> ) = right

  let ( <*> ) = both

  let ( <|> ) = either
end

let parse (s : string) (p : 'a t) =
  let _, res = p (Input.of_string s) in
  res
