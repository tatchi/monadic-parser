module Error = struct
  type t =
    { desc : string
    ; pos : int
    }

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

  let lsplit ~prefix t =
    try
      let length = String.length t.text in
      let prefix_len = String.length prefix in
      let input = sub ~start:0 ~len:prefix_len t in
      let rest = sub ~start:prefix_len ~len:(length - prefix_len) t in
      Some (rest, input)
    with Invalid_argument _ -> None

  let of_string text = { text; pos = 0 }
end

type 'a t = { run : Input.t -> Input.t * ('a, Error.t) result }

let return t = { run = (fun input -> (input, Ok t)) }

let error ~got ~expected pos =
  Error.create
    (Printf.sprintf "Expected \"%s\" but got \"%s\"" expected got)
    pos

let string s =
  { run =
      (fun input ->
        let unexpected_prefix_error =
          Error.create (Printf.sprintf "Expected prefix: `%s`" s) input.pos
        in
        match Input.lsplit ~prefix:s input with
        | None -> (input, Error unexpected_prefix_error)
        | Some (input', prefix_input) ->
          if prefix_input.text = s then (input', Ok s)
          else (input, Error unexpected_prefix_error))
  }

let map p ~f =
  { run =
      (fun input ->
        match p.run input with
        | (_, Error _) as e -> e
        | input, Ok r -> (input, Ok (f r)))
  }

let bind p ~f =
  { run =
      (fun input ->
        match p.run input with
        | (_, Error _) as e -> e
        | input, Ok r -> (f r).run input)
  }

let both p1 p2 =
  { run =
      (fun input ->
        match p1.run input with
        | (_, Error _) as e -> e
        | input, Ok r1 -> (
          match p2.run input with
          | (_, Error _) as e -> e
          | input, Ok r2 -> (input, Ok (r1, r2))))
  }

let right p1 p2 =
  { run =
      (fun input ->
        match p1.run input with
        | (_, Error _) as e -> e
        | input', Ok _ -> p2.run input')
  }

let left p1 p2 =
  { run =
      (fun input ->
        match p1.run input with
        | (_, Error _) as e -> e
        | input', Ok r1 -> (
          match p2.run input' with
          | (_, Error _) as e -> e
          | input'', Ok _ -> (input'', Ok r1)))
  }

module O = struct
  let ( let+ ) t f = map t ~f

  let ( let* ) t f = bind t ~f

  let ( and+ ) = both

  let ( <* ) = left

  let ( *> ) = right
end

let parse_full (s : string) (p : 'a t) = p.run (Input.of_string s)

let parse (s : string) (p : 'a t) =
  let _, res = p.run (Input.of_string s) in
  res
