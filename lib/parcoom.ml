type input = string

module Error = struct
  type t =
    { desc : string
    ; pos : int
    }

  let desc t = t.desc

  let pos t = t.pos

  let create desc pos = { desc; pos }
end

type 'a t = { run : input -> input * ('a, Error.t) result }

(* let input_of_string s = { text = s; pos = 0 } *)

let error ~got ~expected pos =
  Error
    (Error.create
       (Printf.sprintf "Expected \"%s\" but got \"%s\"" expected got)
       pos)

let string s =
  { run =
      (fun input ->
        if s = input then ("", Ok s)
        else
          ( s
          , Error
              { desc = Printf.sprintf "Expected \"%s\" but got \"%s\"" s input
              ; pos = 0
              } ))
  }

let int s =
  let error got pos = error ~expected:(string_of_int s) ~got pos in
  { run =
      (fun input ->
        match int_of_string_opt input with
        | None -> (input, error input 0)
        | Some n ->
          if s = n then ("", Ok s) else (input, error (string_of_int n) 0))
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
        | _, Ok r ->
          let { run } = f r in
          run input)
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

module O = struct
  let ( let+ ) t f = map t ~f

  let ( let* ) t f = bind t ~f

  let ( and+ ) = both
end

let parse (s : string) (p : 'a t) =
  let _, res = p.run s in
  res
