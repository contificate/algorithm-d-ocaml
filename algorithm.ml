
type symbol =
  | Label of string
  | Child of int

let show_symbol = function
  | Label f -> f
  | Child i -> string_of_int i

module Labelled = struct
  type 'a t = {
      value: 'a;
      mutable hits: int; 
      mutable matches: bool;
    }
end

module LabelledTree =
  Tree.Make(Labelled)

exception TreeError of string

let instantiate =
  let rec go : Ast.Tree.t -> LabelledTree.t = function
    | Node (f, xs) ->
       let xs = List.map go xs in
       { value = Node (f, xs); hits = 0; matches = false }
    | Wildcard ->
       raise (TreeError "Can't instantiate pattern!")
  in go

let rtl tree =
  let paths : symbol list list ref = ref [] in
  let rec go (acc : symbol list) : Ast.Tree.t -> unit = function
    | Node (f, []) ->
       paths := List.rev (Label (f^"0") :: acc) :: !paths
    | Node (f, xs) ->
       List.iteri (fun i -> go (Child i :: Label (f^string_of_int (List.length xs)) :: acc)) xs
    | Wildcard ->
       paths := List.rev acc :: !paths
  in
  go [] tree;
  !paths

let parse t =
  Parser.start Lexer.tokenise (Lexing.from_string t)

let label : LabelledTree.t -> symbol = function
  | { value = Node (f, xs); _ } -> Label (f ^ string_of_int List.(length xs))
  | _ -> raise (TreeError "Tree must be root labelled!")

type 'a entry = {
    node: LabelledTree.t;
    state: 'a Trie.Node.t;
    mutable visited: int;
  }

module S = Stack

let arity : LabelledTree.t -> int = function
  | { value = Node (_, xs); _ } -> List.length xs
  | _ -> 0

let child i : LabelledTree.t -> LabelledTree.t = function
  | { value = Node (_, xs); _ } -> List.nth xs i
  | _ -> failwith "Must have an ith child!"

let go pattern subject =
  let pattern = parse pattern in
  let subject = instantiate (parse subject) in
  let paths = rtl pattern in
  let trie = Trie.create () in
  List.iter (Trie.add trie) paths; 
  Trie.compute trie;
  (* move on root label of subject tree *)
  let first =
    Trie.Node.follow trie.root (label subject) 
  in
  let stack = Vector.create ~dummy:(Obj.magic 0) in
  Vector.push stack { node = subject; state = first; visited = (-1) };
  let tabulate state =
    let outs = Trie.Node.outputs state in
    let register out =
      let out' = List.filter (function Label _ -> true | _ -> false) out in
      let len = List.length out' in
      let entry = Vector.get stack (Vector.length stack - len) in
      entry.node.hits <- entry.node.hits + 1;
      if (entry.node.hits = List.length paths) then
        entry.node.matches <- true;
    in
    List.iter register outs
  in
  tabulate first;
  while not (Vector.is_empty stack) do
    let { node; state; visited } as top = Vector.top stack in
    if visited = (arity node - 1) then
      ignore (Vector.pop stack)
    else begin
        top.visited <- visited + 1;
        let int_state = Trie.Node.follow state (Child top.visited) in
        tabulate int_state;
        let node' = child top.visited node in
        let state' = Trie.Node.follow int_state (label node') in
        Vector.push stack { node = node'; state = state'; visited = (-1) };
        tabulate state'
      end
  done;
  let rec show : LabelledTree.t -> string = function
    | { value = Node (f, []); matches; _ } ->
       Printf.sprintf (if matches then "[%s]" else "%s") f
    | { value = Node (f, xs); matches; _ } ->
       let xs = String.concat "," List.(map show xs) in
       Printf.sprintf (if matches then "[%s(%s)]" else "%s(%s)") f xs
    | _ -> ""
  in
  print_endline (show subject)
