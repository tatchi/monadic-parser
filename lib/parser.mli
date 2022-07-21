module Error : sig
  type t =
    { desc : string
    ; pos : int
    }

  val desc : t -> string

  val pos : t -> int
end

module Input : sig
  type t
end

type 'a t

val int : int -> int t

val string : string -> string t

val bool : bool -> bool t

val end_ : unit t

val return : 'a -> 'a t

val map : 'a t -> f:('a -> 'b) -> 'b t

val bind : 'a t -> f:('a -> 'b t) -> 'b t

val both : 'a t -> 'b t -> ('a * 'b) t

val left : 'a t -> 'b t -> 'a t

val right : 'a t -> 'b t -> 'b t

val either : 'a t -> 'a t -> 'a t

val parse_while : (char -> bool) -> string t

val optional : 'a t -> 'a option t

val not : 'a t -> unit t

val many : 'a t -> 'a list t

(* val many : 'a t -> 'a list t *)

module O : sig
  val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t

  val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t

  val ( and+ ) : 'a t -> 'b t -> ('a * 'b) t

  val ( <* ) : 'a t -> 'b t -> 'a t

  val ( *> ) : 'a t -> 'b t -> 'b t

  val ( <*> ) : 'a t -> 'b t -> ('a * 'b) t

  val ( <*> ) : 'a t -> 'b t -> ('a * 'b) t

  val ( <|> ) : 'a t -> 'a t -> 'a t
end

val parse : string -> 'a t -> ('a, Error.t) result
