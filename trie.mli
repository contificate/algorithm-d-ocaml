(** Trie data structure

    This module implements a trie data structure which supports
    computing suffix and output links used in construction of
    Aho-Corasick automatons.
 *)

module Node : sig
  type 'a t
  val follow : 'a t -> 'a -> 'a t
  val outputs : 'a t -> 'a list list
end

type 'a t = {
    root: 'a Node.t
  }
(** The type of tries. *)

val create : unit -> 'a t
(** Create an empty trie (empty root node). *)

val add : 'a t -> 'a list -> unit
(** [add t xs] adds [xs] to the trie [t]. *)

val compute : 'a t -> unit
(** [compute t] computes and modifies suffix and output links. *)

