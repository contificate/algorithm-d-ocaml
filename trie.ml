
module H = Hashtbl
module Q = Queue

module Node = struct
  type 'a t = {
      parent: 'a t option;
      goto: ('a, 'a t) H.t;
      mutable suffix: 'a t option;
      mutable output: 'a t option;
      mutable pattern: 'a list;
    }

  let create p = {
      parent = p;
      goto = H.create 10;
      suffix = None;
      output = None;
      pattern = [];
    }

  let outputs (t : 'a t) =
    let out : 'a list list ref = ref [] in
    let rec chase node =
      if node.pattern != [] then
        out := node.pattern :: !out;
      (match node.output with
       | Some link -> chase link
       | _ -> ())
    in
    chase t;
    !out

  let follow (t : 'a t) (x : 'a) = match H.find_opt t.goto x with
    | Some target -> target
    | _ ->
       (* follow suffix links *)
       let rec fail link = match link with
         | suffix when suffix.parent = None -> (* root case *)
            (match H.find_opt suffix.goto x with
             | Some actual -> actual
             | _ -> suffix)
         | suffix ->  
            (match H.find_opt suffix.goto x with
             | Some actual -> actual
             | _ -> fail (Option.get suffix.suffix))
       in
       fail (Option.get t.suffix)
end

type 'a t = {
    root: 'a Node.t
  }

let create () = {
    root = Node.create None
  }

let add t xs =
  let node = ref t.root in
  let go x =
    let node' =
      (match H.find_opt !node.goto x with
       | Some node' -> node'
       | _ ->
          let node' = Node.create (Some !node) in
          H.add !node.goto x node';
          node')
    in
    node := node'
  in
  List.iter go xs;
  !node.pattern <- xs

let compute (t : 'a t) =
  let root = t.root in
  root.suffix <- Some root;
  let q : ('a * 'a Node.t) Q.t = Q.create () in
  (* root and its children have root as their suffix,
     queue root's grandchildren for BFS suffix computation. *)
  H.iter (fun _ (child : 'a Node.t) ->
      child.suffix <- Some root;
      H.iter (fun x grandchild ->
          Q.add (x, grandchild) q) child.goto) root.goto;
  while not (Q.is_empty q) do
    let (x, node) = Q.pop q in
    let p = Option.get node.parent in
    let p's_suffix = Option.get p.suffix in
    (* start at a node's parent's suffix, if no edge labelled x
       exists then continually chase up suffix links. If you
       reach the root, break to avoid looping indefinitely. *)
    let rec follow s =
      match s with
      | s when s == root ->
         (match H.find_opt s.goto x with
          | Some actual -> actual
          | _ -> root)
      | s when not (H.mem s.goto x) ->
         follow (Option.get s.suffix)
      | s -> H.find s.goto x
    in
    let suffix = follow p's_suffix in
    node.suffix <- Some suffix;
    (* a node's output link is its suffix link if its suffix link
       has a pattern (is an output node), otherwise it's its suffix's
       output link. *)
    let output =
      (match suffix.pattern with
       | [] -> suffix.suffix
       | _ -> Some suffix)
    in
    node.output <- output;
    (* queue the children of processed node *)
    H.iter (fun x child -> Q.add (x, child) q) node.goto
  done

