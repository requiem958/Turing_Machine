    
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
      | [] -> BinTree(NilTree,NilTree)
      | b::bs ->
         if b=Bit.zero
         then BinTree(make_from bs, NilTree)
         else BinTree(NilTree, make_from bs)
        
        
    let add_to: t -> bit list -> t = fun btree bits ->
      merge btree (make_from bits)
      
      
    let build_from: word list -> binaryTree = fun words ->
      List.fold_left add_to NilTree words
      
      
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
    type 'node edges = ('node edge) list
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
          (mk_node_from  : 'node -> bit -> 'node)
          (initial_node  : 'node)
          (opt_final_node: 'node option)
          (btree         : t)
        : 'node edges 
      = let rec to_graph_rec: 'node -> t -> 'node graph option = fun current_node btree ->
          match btree with
          | NilTree -> None
          | BinTree(NilTree,NilTree)
            -> (match opt_final_node with
                | None            -> Some(current_node,[])
                | Some final_node -> Some(final_node, [])
               )
          | BinTree(l_tree,r_tree)
            ->
             let l_root_node = mk_node_from current_node Bit.zero
             and r_root_node = mk_node_from current_node Bit.unit
             in merge_optional_graph
                  (make_edge current_node Bit.zero (to_graph_rec l_root_node l_tree))
                  (make_edge current_node Bit.unit (to_graph_rec r_root_node r_tree))
              
        in match to_graph_rec initial_node btree with
           | None -> []
           | Some(_,edges) -> edges
                            
  end


(* DEMO

open Binary_Tree
open Binary_Tree.Demo
let _ = Binary_Tree.Demo.demo()
m 
*)
  
module Demo =
  struct

    module Bit =
      struct
        type t = int
        let (zero:t) = 0
        let (unit:t) = 1
        let (pretty: t -> string) = string_of_int
      end

    module BinaryTree = Made_Of(Bit)

    type node =
      | W of Bit.t list
      
    let make_node_from: node -> Bit.t -> node = fun node bit ->
      match node with
      | W bits -> W (bits @ [bit])

      
    let demo: unit -> 'a = fun () ->
      let z = Bit.zero
      and u = Bit.unit
      in let words = [[z;u;z];[z;u;z];[z;z];[u;u;z];[u;z]]
         in
         let btree = BinaryTree.build_from words
         and initial_node = W []
         and opt_final_node = None
         in
         (List.map BinaryTree.make_from words,
          btree,
          BinaryTree.to_words btree,
          BinaryTree.to_graph_with make_node_from
            initial_node
            opt_final_node
            btree)
    end
    
