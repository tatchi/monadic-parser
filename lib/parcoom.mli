type input = string

module Error : sig
  type t

  val desc : t -> string

  val pos : t -> int
end

type 'a t

val string : string -> string t

val int : int -> int t

val map : 'a t -> f:('a -> 'b) -> 'b t

val bind : 'a t -> f:('a -> 'b t) -> 'b t

val both : 'a t -> 'b t -> ('a * 'b) t

module O : sig
  val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t

  val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t

  val ( and+ ) : 'a t -> 'b t -> ('a * 'b) t
end

val parse : string -> 'a t -> ('a, Error.t) result
