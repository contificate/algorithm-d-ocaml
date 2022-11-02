
module type P = sig
  type 'a t
end

module Make(P:P) = struct
  type t = t' P.t
  and t' =
    | Node of string * t list
    | Wildcard
end
