(* https://dev.to/matechs/building-custom-dsls-in-typescript-29el *)

type 'a t =
  | String : string -> string t
  | Int : int -> int t
  | Add : (int t * int t) -> int t
  | Concat : (string t * string t) -> string t
  | Stringify : int t -> string t

let string s = String s

let int i = Int i

let add n1 n2 = Add (n1, n2)

let concat s1 s2 = Concat (s1, s2)

let stringify i = Stringify i

let rec compute : type a. a t -> a = function
  | String s -> s
  | Int i -> i
  | Add (i1, i2) -> compute i1 + compute i2
  | Concat (s1, s2) -> compute s1 ^ compute s2
  | Stringify n -> string_of_int (compute n)

let operation = int 0 |> add (int 1) |> add (int 2)

let operationStr = concat (string "operation: ") (stringify operation);;

print_endline (compute operationStr);

print_endline (string_of_int @@ compute operation)
