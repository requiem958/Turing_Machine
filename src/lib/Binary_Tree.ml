    
type binaryTree =
  | NilTree
  | BinTree of binaryTree * binaryTree
             

module Made_Of =
  functor (Bit : Bit_Vector.Two_Values_Sig) ->
  struct
    
    type t = binaryTree
           
    type bit = Bit.t
    type word = bit list
              
              
    let rec merge: t -> t -> t = fun bt1 bt2 ->
      match bt1,bt2 with
      | NilTree, _ -> bt2
      | _ , NilTree -> bt1
      | BinTree(l1,r1), BinTree(l2,r2) -> BinTree(merge l1 l2, merge r1 r2)
                                        
                                        
    let rec make_from: bit list -> t = fun bits ->
      match bits with
      | [] -> NilTree
      | b::bs ->
         if b=Bit.zero then BinTree(make_from bs, NilTree) else BinTree(NilTree, make_from bs)
        
        
    let add: t -> bit list -> t = fun btree bits ->
      merge btree (build_from bits)
      
      
    let build_from: word list -> binaryTree = fun words ->
      List.fold_left add NilTree words
      
      
    let distrib: 'a -> ('a list) list -> ('a list) list = fun bit words ->
      List.map (fun word -> bit::word) words
      
    let rec to_words: t -> word list = fun btree ->
      let epsilon = []
      in match btree with
         | NilTree -> []
         | BinTree(NilTree,NilTree) -> [ epsilon ]
         | BinTree(l_tree,r_tree)
           -> (distrib Bit.zero (to_words l_tree)) @ (distrib Bit.unit (to_words r_tree))
            
    type 'node edge = 'node * bit * 'node
    type 'node edges = ('ndoe edge) list
    type 'node graph = 'node * 'node edges (* graph = (entry_node, edges) *)
                     
    let make_edge: 'node -> bit -> 'node graph option  -> 'node graph option = fun new_node bit graph ->
      match graph with
      | None -> None
      | Some(entry_node,edges) -> Some (new_node,  (new_node,bit,entry_node) :: edges)
                                
    let merge_optional_graph: 'node graph option -> 'node graph option -> 'node graph option = fun og1 og2 ->
      match og1, og2 with
      | None, og2 -> og2
      | og1, None -> og1
      | Some(g1_entry_node, g1_edges), Some(g2_entry_node, g2_edges) when g1_entry_node = g2_entry_node
        -> Some(g1_entry_node, g1_edges @ g2_edges)
         
         
    let to_graph_with
          (mk_node_from: 'node -> bit -> 'node)
          (initial_node: 'node)
          (final_node  : 'node)
          (btree       : t)
        : 'node graph option
      = let rec to_graph_rec: 'node -> t -> 'node graph option = fun current_node btree ->
          match btree with
          | NilTree -> None
          | BinTree(NilTree,NilTree) -> Some(final_node, []) 
          | BinTree(l_tree,r_tree)
            ->
             let l_root_node = mk_node_from current_node Bit.zero
             and r_root_node = mk_node_from current_node Bit.unit
             in merge_optional_graph
                  (make_edge current_node Bit.zero (to_graph_rec l_root_node l_tree))
                  (make_edge current_node Bit.unit (to_graph_rec r_root_node r_tree))
              
        in to_graph_rec initial_node btree 

  end
  
let make_node_from: State.t -> Bit.t -> State.t = fun state bit ->
  match state with
  | Q(i) -> Qs(i,[bit])
  | Qs(i,bits) -> Qs(i,bits@[bit])
  | Qc(s,ints) -> Qc(s,ints@[if bit=Bti.zero then 0 else 1])
                


module Demo =
  struct

    module Bit = Bit_Vector.Bit_as_Int
    module BTree = Binary_Tree.Made_Of(Bit)

    let make_node_from: int -> Bit.t -> int = fun int bit ->
      if bit = Bit.zero then int+2 else int+1
                        
    let demo: unit -> 'a = fun () ->
      let btree = build_from [[0;1];[0;0];[0;1];[1;0]]
      and initial_node = 0
      and final_node = -1
      in 
      (btree, to_graph_with make_node_from
                initial_node
                final_node
                btree)
    end
    
