type 'a t

val empty : 'a t
val add : Sym.t -> 'a -> 'a t -> 'a t
val find : Sym.t -> 'a t -> 'a option
val print : ('a -> string) -> 'a t -> unit
