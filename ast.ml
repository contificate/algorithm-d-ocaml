
module Identity = struct
  type 'a t = 'a
end

module Tree =
  Tree.Make(Identity)
