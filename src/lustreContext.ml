(* This file is part of the Kind 2 model checker.

   Copyright (c) 2014 by the Board of Trustees of the University of Iowa

   Licensed under the Apache License, Version 2.0 (the "License"); you
   may not use this file except in compliance with the License.  You
   may obtain a copy of the License at

   http://www.apache.org/licenses/LICENSE-2.0 

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
   implied. See the License for the specific language governing
   permissions and limitations under the License. 

*)

open Lib

(* FIXME: Remove unless debugging *)
module Event = struct let log _ fmt = Format.printf (fmt ^^ "@.") end

module A = LustreAst

module I = LustreIdent
module IT = LustreIdent.LustreIdentHashtbl

module D = LustreIndex

module E = LustreExpr
module ET = E.LustreExprHashtbl

module N = LustreNode

module SVT = StateVar.StateVarHashtbl

module VS = Var.VarSet


(* Node not found, possible forward reference 

   This is just failing at the moment, we'd need some dependency
   analysis to recognize cycles to fully support forward
   referencing. *)
exception Node_not_found of I.t * position


(* Context for typing, flattening of indexed types and constant
   propagation *)
type t = 

  { 

    (* Scope of context *)
    scope : string list;

    (* Definitions for node so far *)
    node : N.t option;

    (* Visible nodes *)
    nodes : N.t list;

    (* Visible contract nodes *)
    contract_nodes : (position * A.contract_node_decl) list;

    (* Type identifiers and their types and bounds of their indexes *)
    ident_type_map : (Type.t D.t) IT.t;

    (* Identifiers and the expresssions they are bound to

       Contains a term of a variable if the identifier denotes a
       stream, and a term of an expression if the identifier denotes a
       constant. Equational definitions are not stored here, this is
       for constant propagation only. *)
    ident_expr_map : (E.t D.t) IT.t list;

    (* Map from expressions to state variables abstracting them

       This map may contain for the same expression several state
       variables with different properties (is_const, is_input,
       for_inv_gen). 

       The most recent binding shadows earlier ones, use ET.find_all
       instead of ET.find. A ET.fold will return all bindings to an
       expression. (Thus turning an annoyance of OCaml into our
       advantage.) *)
    expr_state_var_map : StateVar.t ET.t;

    (* Map from state variables to state variables providing a
       non-deterministic pre-initial value *)
    state_var_oracle_map : StateVar.t SVT.t;

    (* Index to use for next fresh state variable *)
    fresh_local_index : int;

    (* Index to use for next fresh oracle state variable *)
    fresh_oracle_index : int;

    (* Index to use for next fresh observer state variable *)
    fresh_observer_index : int;

    (* [None] if definitions are allowed, otherwise a pair of a
       message and a position to raise an error with. *)
    definitions_allowed : (Lib.position * string) option;
  }


(* Create an initial empty context 

   This must be a function, because it contains a hash table, which is
   modified in place. *)
let mk_empty_context () = 

  { scope = [];
    node = None;
    nodes = [];
    contract_nodes = [];
    ident_type_map = IT.create 7;
    ident_expr_map = [IT.create 7];
    expr_state_var_map = ET.create 7;
    state_var_oracle_map = SVT.create 7;
    fresh_local_index = 0;
    fresh_oracle_index = 0;
    fresh_observer_index = 0;
    definitions_allowed = None }


(* Raise parsing exception *)
let fail_at_position pos msg = 

  Event.log
    L_warn
    "Parser error at %a: %s"
    Lib.pp_print_position pos
    msg;

  raise A.Parser_error
  

(* Raise parsing exception *)
let warn_at_position pos msg = 

  Event.log
    L_warn
    "Parser warning at %a: %s"
    Lib.pp_print_position pos
    msg


(* ********************************************************************** *)
(*                                                                        *)
(* ********************************************************************** *)

(* Set flag context to raise an error when defining an expression. *)
let fail_on_new_definition ctx pos msg = 
  { ctx with definitions_allowed = Some (pos, msg) }


(* Return the scope of the current context *)
let scope_of_node = function 

  (* Within a node: add node name to scope *)
  | { node = Some { N.name } } -> [I.string_of_ident false name]

  (* Outside a node: return empty scope *)
  | { node = None } -> []


(* Return the scope of the current context *)
let scope_of_context ({ scope } as ctx) = 

  (* Start with scope of node *)
  scope_of_node ctx @ List.rev scope


(* Add scope to context *)
let push_scope 
    ({ scope; 
       ident_expr_map;
       fresh_local_index;
       fresh_oracle_index;
       fresh_observer_index } as ctx) 
    ident = 

  { ctx with 
      scope = ident :: scope;
      ident_expr_map = IT.create 7 :: ident_expr_map; }


(* Remove scope from context *)
let pop_scope = function

  (* fail if at top scope *)
  | { scope = [] } 
  | { ident_expr_map = [] } -> raise (Invalid_argument "pop_scope")

  (* Return context with first scope removed *)
  | { scope = _ :: scope; ident_expr_map = _ :: ident_expr_map } as ctx -> 

    { ctx with scope; ident_expr_map }


(* Create an empty node in the context *)
let create_node = function 

  (* Already in a node *)
  | { node = Some _ } -> 

    (* Fail *)
    (function _ -> raise (Invalid_argument "create_node"))

  (* Not in a node *)
  | { node = None; ident_type_map; ident_expr_map; expr_state_var_map } as ctx -> 

    (function ident -> 

      (* Add empty node to context *)
      { ctx with 

          (* Make deep copies of hash tables *)
          ident_type_map = IT.copy ident_type_map;
          ident_expr_map = List.map IT.copy ident_expr_map;
          expr_state_var_map = ET.copy expr_state_var_map;
          node = Some (N.empty_node ident) } )


(* Add a binding of an identifier to an expression to context *)
let add_expr_for_ident ?(shadow = false) ({ ident_expr_map } as ctx) ident expr = 

  (* Must have at least a map for the top level *)
  assert (ident_expr_map <> []);

  (* Fail if hash table for the current scope already contains a
     binding to the identifier, and if the binding should not shadow,
     then also if some of the lower scopes contains a binding to the
     identifier. *)
  if 
    
    (IT.mem (List.hd ident_expr_map) ident)
    || 
    (not shadow 
     && 
     (List.exists (fun m -> IT.mem m ident) (List.tl ident_expr_map)))

  then

    raise (Invalid_argument "add_expr_for_ident")    

  else
    
    (* Modify hash table in place *)
    IT.add (List.hd ident_expr_map) ident expr; 
  
  (* Return context *)
  ctx


(* Add a binding of an identifier to an expression to context *)
let add_expr_for_indexed_ident
    ?(shadow = false)
    ({ ident_expr_map } as ctx) 
    ident 
    index 
    expr = 

  try 

    (* Must have at least a map for the top level *)
    assert (ident_expr_map <> []);

    (* Get trie for expression *)
    let t = IT.find (List.hd ident_expr_map) ident in

    if 

      (* Fail if trie for the idnetifier in the current scope already
         contains a binding to the index, and if the binding should
         not shadow, then also if some of the lower scopes contains a
         binding to the index in the trie for the identifier. *)
      (D.mem index t)
      || 
      (not shadow
       &&
       (List.exists 
          (fun m -> 
             try IT.find m ident |> D.mem index with Not_found -> false)
          (List.tl ident_expr_map)))

    then

      raise (Invalid_argument "add_expr_for_ident")    

    else



      (* Add expression for index to trie *)
      let t' = D.add index expr t in

      (* Add modified trie in-place to the hash table *)
      IT.add (List.hd ident_expr_map) ident t';

      (* Return context *)
      ctx

  with Not_found -> 

    (* Add expression with index to empty trie and add to hash table
       in-place *)
    IT.add (List.hd ident_expr_map) ident (D.singleton index expr);

    (* Return context *)
    ctx


(* Add a binding of an identifier to an expression to context *)
let remove_expr_for_ident ({ ident_expr_map } as ctx) ident = 

  (* Ensure the hash table does not already contain the expression *)
  if not (List.exists (fun m -> IT.mem m ident) ident_expr_map) then 

    raise (Invalid_argument "remove_expr_for_ident")

  else
  
    (

      (* Must have at least a map for the top level *)
      assert (ident_expr_map <> []);

      (* Modify hash table in place *)
      IT.remove (List.hd ident_expr_map) ident; 
      
      (* Return context *)
      ctx

    )


(* Add a binding of an identifier to a type to context *)
let add_type_for_ident ({ ident_type_map } as ctx) ident l_type = 

  (* Modify hash table in place *)
  IT.add ident_type_map ident l_type; 

  (* Return context *)
  ctx


(* Return nodes defined in context *)
let get_nodes { nodes } = nodes 


(* Return a contract node by its identifier *)
let contract_node_decl_of_ident { contract_nodes } ident = 

  try 
    
    (* Return contract node by name *)
    List.find
      (function (_, (i, _, _, _, _, _)) -> i = ident)
      contract_nodes

  (* Raise error again for more precise backtrace *)
  with Not_found -> raise Not_found


(* Add a contract node to the context for inlining later *)
let add_contract_node_decl_to_context
    ({ contract_nodes } as ctx)
    (pos, ((ident, _, _, _, _, _) as contract_node_decl)) =

  if 

    (* Check if contract of with the same identifier exists *)
    List.exists
      (function (_, (i, _, _, _, _, _)) -> i = ident)
      contract_nodes

  then

    fail_at_position
      pos
      "Contract node already defined"
      
  else

    (* Add contract node to context *)
    { ctx with contract_nodes = (pos, contract_node_decl) :: contract_nodes }


(* Create a state variable for from an indexed state variable in a
   scope *)
let mk_state_var
    ?is_input
    ?is_const
    ?for_inv_gen 
    ?(shadow = false)
    ctx
    scope
    ident 
    index 
    state_var_type
    state_var_source = 

  (* Concatenate identifier and indexes *)
  let state_var_name = 
    Format.asprintf "%a%a"
      (I.pp_print_ident false) ident
      (D.pp_print_index false) index
  in

  (* Create or retrieve state variable *)
  let state_var =
    StateVar.mk_state_var
      ?is_input
      ?is_const
      ?for_inv_gen 
      state_var_name
      scope
      state_var_type 
  in

  (* Create expression from state variable *)
  let expr = E.mk_var state_var E.base_clock in

  (* Bind state variable to identifier *)
  let ctx = 

    (* State variable without index? *)
    (if index = D.empty_index then 

       (* Create singleton trie for identifier *)
       D.singleton D.empty_index expr
       |> add_expr_for_ident ~shadow ctx ident 

     else

       (* Add to exisiting trie or create new trie for identifier*)
       add_expr_for_indexed_ident ~shadow ctx ident index expr)
    
  in

  (* Return state variable and changed context *)
  state_var, ctx



(* Resolve an indentifier to an expression in all scopes *)
let rec expr_of_ident' ident = function 

  | [] -> raise Not_found

  | m :: tl ->

    try IT.find m ident with Not_found -> expr_of_ident' ident tl


(* Resolve an indentifier to an expression *)
let expr_of_ident { ident_expr_map } ident = 
  expr_of_ident' ident ident_expr_map


(* Return true if the identifier denotes an output or local variable *)
let assignable_state_var_of_ident = function 

  (* No node in context *)
  | { node = None } -> 

    (function _ -> 
      raise (Invalid_argument "assignable_state_var_of_ident"))

  (* Get locals and outputs from node in context *)
  | { node = Some { N.locals; N.outputs } } as ctx ->

    (function ident -> 

      try 

        (* Get expression from identifier *)
        let expr = expr_of_ident ctx ident in 

        D.map 
          (fun e -> 
        
             (* Expression is a variable *)
             if E.is_var e then 
               
               (* Get state variable of expression *)
               let state_var = E.state_var_of_expr e in 

               if 

                 (* Find state variable in outputs *)
                 D.exists 
                   (fun _ sv -> StateVar.equal_state_vars state_var sv)
                   outputs
                   
                 || 
                 
                 (* Find state variable in locals *)
                 List.exists 
                   (D.exists 
                      (fun _ sv -> StateVar.equal_state_vars state_var sv))
                   locals
                   
               then 
                 
                 (* Return state variable *)
                 state_var
                   
               (* State variable is not a local variable or output *)
               else
                 
                 raise (Invalid_argument "assignable_state_var_of_ident")

            (* Expression is not a variable *)
            else 

              raise (Invalid_argument "assignable_state_var_of_ident"))

          expr
     
              
      (* Identifier does not denote an expression *)
      with Not_found -> raise Not_found)


(* Resolve an indentifier to an expression *)
let type_of_ident { ident_type_map } ident = IT.find ident_type_map ident 


(* Return true if identifier has been declared, raise an exception if
   the identifier is reserved. *)
let expr_in_context { ident_expr_map } ident = 

  if 

    (* Identifier must not be reserved *)
    I.ident_is_reserved ident

  then
    
    raise 
      (Invalid_argument 
         (Format.asprintf 
            "Identifier %a is reserved internal use" 
            (I.pp_print_ident false) ident))

  else

    (* Return if identifier is in context *)
    (List.exists (fun m -> IT.mem m ident) ident_expr_map) 


(* Return true if identifier has been declared, raise an exception if
   the identifier is reserved. *)
let type_in_context { ident_type_map } ident = 

  (* Return if identifier is in context *)
  (IT.mem ident_type_map ident) 


(* Return true if node has been declared in the context *)
let node_in_context { nodes } ident = 
  
  (* Return if identifier is in context *)
  N.exists_node_of_name ident nodes 
  

(* Add newly created variable to locals *)
let add_state_var_to_locals = function 
  | { node = None } -> (function _ -> assert false)
  | { node = Some ({ N.locals } as node) } as ctx ->

    (function state_var -> 

      { ctx with 
          node = Some { node with 
                          N.locals = 
                            D.singleton D.empty_index state_var :: locals} })


(* Add declaration of state variable to observers *)
let add_state_var_to_observers = function 
  | { node = None } -> (function _ -> assert false)
  | { node = Some ({ N.observers } as node) } as ctx ->
    
    (function state_var -> 
      
      { ctx with 
          node = Some { node with N.observers = state_var :: observers } })


(* Create a fresh state variable as an oracle input *)
let mk_fresh_oracle 
    ?is_input
    ?is_const
    ?for_inv_gen
    ({ node; fresh_oracle_index } as ctx) 
    state_var_type =

  (* Are we in a node? *)
  match node with 

    (* Fail if not inside a node *)
    | None -> raise (Invalid_argument "mk_fresh_oracle")

    (* Add to oracles *)
    | Some ({ N.oracles } as node) ->

      (* Create state variable for abstraction *)
      let state_var, ctx = 
        mk_state_var 
          ?is_input:is_input
          ?is_const:is_const
          ?for_inv_gen:for_inv_gen
          ctx
          (scope_of_node ctx)
          (I.push_index I.oracle_ident fresh_oracle_index)
          D.empty_index
          state_var_type
          N.Oracle
      in

      (* Increment index of fresh oracle *)
      let ctx = 
        { ctx with 
            node = Some { node with 
                            N.oracles = state_var :: oracles}; 
            fresh_oracle_index = succ fresh_oracle_index }
      in

      (* Return variable and changed context *)
      (state_var, ctx)


(* Create a fresh state variable as an oracle input for the state variable *)
let mk_fresh_oracle_for_state_var 
    ({ state_var_oracle_map; fresh_oracle_index } as ctx) 
    state_var =

  try 

    (* Return previously created oracle *)
    (SVT.find state_var_oracle_map state_var, ctx)

  with Not_found -> 

    (* Create fresh oracle *)
    let state_var', ctx = 
      mk_fresh_oracle
        ~is_const:true 
        ctx
        (StateVar.type_of_state_var state_var)
    in

    (* Associate oracle with state variable *)
    SVT.add state_var_oracle_map state_var state_var';

    (* Return changed context

       The hash table of state variables to their oracles has been
       modfied in place. *)
    (state_var', ctx)
      

(* Guard unguarded pre expression with a fresh oracle constant

   An unguarded pre is a previous state variable occuring in the
   initial state expression, since the arrow operator has been lifted
   to the top of the expression. *)
let close_expr
    pos
    ({ E.expr_init } as expr, ctx) = 

  (* Get variables in initial state term *)
  let init_vars = Term.vars_of_term (expr_init :> Term.t) in

  (* Filter for variables before the base instant *)
  let init_pre_vars = 
    VS.filter 
      (fun var -> 
         Var.is_state_var_instance var &&
         Numeral.(Var.offset_of_state_var_instance var < E.base_offset))
      init_vars
  in
  
  (* No unguarded pres in initial state term? *)
  if VS.is_empty init_pre_vars then (expr, ctx) else
    
    (warn_at_position
       pos
       "Unguarded pre in expression, adding new oracle input";
     
     (* New oracle for each state variable *)
     let oracle_substs, ctx =
       VS.fold
         (fun var (accum, ctx) -> 
            
            (* Identifier for a fresh variable *)
            let state_var, ctx = 
              
              (* We only expect state variable instances *)
              assert (Var.is_state_var_instance var);

              (* Create a new oracle variable or re-use previously
                 created oracle *)
              mk_fresh_oracle_for_state_var
                ctx
                (Var.state_var_of_state_var_instance var) 

            in
            
            (* Substitute oracle variable for variable *)
            ((var, E.mk_var state_var E.base_clock) :: accum, ctx))
         
         init_pre_vars
         ([], ctx)
     in
     
     (* Return expression with all previous state variables in the init
        expression substituted by fresh constants *)
     ((E.mk_arrow (E.mk_let oracle_substs expr) expr),
      ctx))


(* Define the expression with a state variable *)
let mk_state_var_for_expr
    ?(is_input = false)
    ?(is_const = false)
    ?(for_inv_gen = true)
    ({ expr_state_var_map; 
       fresh_local_index;
       definitions_allowed } as ctx)
    after_mk
    ({ E.expr_type } as expr) = 

  match definitions_allowed with 

    (* Fail with error if no new definitions allowed *)
    | Some (pos, msg) -> fail_at_position pos msg

    (* Continue if definitions allowed *)
    | _ -> 

      try 

        (* Find previous definition of expression

           Use [find_all] to get all state variables that define the
           expression. *)
        let state_var_list =
          ET.find_all
            expr_state_var_map
            expr
        in

        (* Find state variable with same properties *)
        let state_var = 
          List.find
            (fun sv -> 
               StateVar.is_input sv = is_input && 
               StateVar.is_const sv = is_const && 
               StateVar.for_inv_gen sv = for_inv_gen)
            state_var_list
        in

        (* Return state variable used before *)
        (state_var, ctx)

      (* Expresssion has not been abstracted before *)
      with Not_found ->

        (* Create state variable for abstraction *)
        let state_var, ctx = 
          mk_state_var 
            ~is_input:is_input
            ~is_const:is_const
            ~for_inv_gen:for_inv_gen
            ctx
            (scope_of_node ctx)
            (I.push_index I.abs_ident fresh_local_index)
            D.empty_index
            expr_type
            N.Abstract
        in

        (* Record mapping of expression to state variable

           This will shadow but not replace a previous definition. Use
           [find_all] to retrieve the definitions, and the usual
           [fold] to iterate over all definitions. *)
        ET.add
          expr_state_var_map
          expr
          state_var;

        (* Evaluate continuation after creating new variable *)
        let ctx = after_mk ctx state_var in

        (* Hash table is modified in place, increment index of fresh state
           variable *)
        let ctx = 
          { ctx with 
              fresh_local_index = succ fresh_local_index }
        in

        (* Return variable and changed context *)
        (state_var, ctx)


(* Define the expression with a state variable *)
let mk_local_for_expr
    ?is_input
    ?is_const
    ?for_inv_gen
    pos
    ({ node;
       fresh_local_index;
       definitions_allowed } as ctx)
    ({ E.expr_type } as expr) = 

  (* Are we in a node? *)
  match node with 

    (* Fail if not inside a node *)
    | None -> raise (Invalid_argument "mk_local_for_expr")

    (* Add to locals *)
    | Some _ ->

      (* Guard unguarded pres before adding definition *)
      let expr', ctx = close_expr pos (expr, ctx) in

      (* Define the expresssion with a fresh state variable *)
      let state_var, ctx = 
        mk_state_var_for_expr
          ?is_input
          ?is_const
          ?for_inv_gen
          ctx
          add_state_var_to_locals
          expr'
      in

      (* Return variable and changed context *)
      (state_var, ctx)


(* Create a fresh state variable as an oracle input *)
let mk_fresh_local
    ?is_input
    ?is_const
    ?for_inv_gen
    ({ node; fresh_local_index } as ctx) 
    state_var_type =

  (* Are we in a node? *)
  match node with 

    (* Fail if not inside a node *)
    | None -> raise (Invalid_argument "mk_fresh_local")

    (* Add to locals *)
    | Some ({ N.locals } as node) ->

      (* Create state variable for abstraction *)
      let state_var, ctx = 
        mk_state_var 
          ?is_input:is_input
          ?is_const:is_const
          ?for_inv_gen:for_inv_gen
          ctx
          (scope_of_node ctx)
          (I.push_index I.abs_ident fresh_local_index)
          D.empty_index
          state_var_type
          N.Abstract
      in

      (* Increment index of fresh oracle *)
      let ctx = 
        { ctx with 
            node = Some { node with 
                            N.locals = 
                              D.singleton D.empty_index state_var :: locals };
            fresh_local_index = succ fresh_local_index }
      in

      (* Return variable and changed context *)
      (state_var, ctx)


(* Create a fresh state variable as an oracle input *)
let mk_fresh_observer
    ?is_input
    ?is_const
    ?for_inv_gen
    ({ node; fresh_observer_index } as ctx) 
    state_var_type =

  (* Are we in a node? *)
  match node with 

    (* Fail if not inside a node *)
    | None -> raise (Invalid_argument "mk_fresh_observer")

    (* Add to observers *)
    | Some ({ N.observers } as node) ->

      (* Create state variable for abstraction *)
      let state_var, ctx = 
        mk_state_var 
          ?is_input:is_input
          ?is_const:is_const
          ?for_inv_gen:for_inv_gen
          ctx
          (scope_of_node ctx)
          (I.push_index I.observer_ident fresh_observer_index)
          D.empty_index
          state_var_type
          N.Observer
      in

      (* Increment index of fresh observer *)
      let ctx' = 
        { ctx with 
            node = Some { node with 
                            N.observers = state_var :: observers}; 
            fresh_observer_index = succ fresh_observer_index }
      in

      (* Return variable and changed context *)
      (state_var, ctx')


let observer_of_state_var ({ node } as ctx) state_var = 

  (* Are we in a node? *)
  match node with 

    (* Fail if not inside a node *)
    | None -> raise (Invalid_argument "observer_of_state_var")

    (* Add to observers *)
    | Some ({ N.inputs; N.oracles; N.outputs; N.observers; N.locals } as node) ->

      if 

        (* Is state variable an input or an oracle? *)
        D.exists
          (fun _ sv -> StateVar.equal_state_vars state_var sv) 
          inputs 
        || 
        List.exists
          (StateVar.equal_state_vars state_var)
          oracles

      then 

        (* Don't make inputs observers *)
        raise (Invalid_argument "observer_of_state_var")

      else if 

        (* Is state variable an output or an observer? *)
        D.exists
          (fun _ sv -> StateVar.equal_state_vars state_var sv) 
          outputs 
        || 
        List.exists
          (StateVar.equal_state_vars state_var)
          observers


      then

        (* Keep state variable as output or observer *)
        ctx

      else

        (* Filter out state variable from definitions of local variables *)
        let locals' = 
          List.filter
            (fun l -> 
               let i1, sv1 = D.min_binding l in
               let i2, sv2 = D.max_binding l in 

               not 

                 (* Smallest key is empty index *)
                 (i1 = D.empty_index && 

                  (* Smallest key is equal to greatest key *)
                  i1 = i2  && 

                  (* Smallest state variable is our property 

                     sv1 sv2 must be equal, because there is only
                     one entry in the trie *)
                  StateVar.equal_state_vars state_var sv1))
            locals
        in

        (* Move declaration of state variable from locals to observers *)
        let ctx = 
          { ctx with 
              node = Some { node with 
                              N.locals = locals';
                              N.observers = state_var :: observers } }
        in

        (* Return changed context *)
        ctx


(* Return the node of the given name from the context*)
let node_of_name { nodes } ident = N.node_of_name ident nodes


(* Return the output variables of a node call in the context with the
   same parameters *)
let call_outputs_of_node_call 
    { node } 
    ident 
    act_var 
    input_state_vars 
    defaults = 

  (* Are we in a node? *)
  match node with 

    (* Fail if not inside a node *)
    | None -> raise (Invalid_argument "call_outputs_of_node_call")

    (* Add to node calls *)
    | Some { N.calls } ->

      try 

        (* Find a call to the same node on the same clock with the same
           inputs in this node *)
        let { N.call_outputs } = 

          List.find
            (fun { N.call_clock = call_cond;
                   N.call_defaults = call_defaults;
                   N.call_node_name = call_ident;
                   N.call_inputs = call_inputs } -> 

              (* Call must be to the same node, and ... *)
              (I.equal ident call_ident) &&

              (* ... activation conditions must be equal, and ... *)
              (match act_var, call_cond with 

                (* Both calls are with an activation condition *)
                | Some v, Some v' -> 

                  (* Same activation condition *)
                  StateVar.equal_state_vars v v' &&

                  (* Same defaults *)
                  (D.for_all2
                     (fun _ sv1 sv2 -> E.equal sv1 sv2)
                     defaults 
                     call_defaults)

                (* Both calls without activation condtion *)
                | None, None -> true

                (* One call with and the other without activation
                   condition *)
                | _ -> false) &&

              (* ... inputs must be the same up to oracles *)
              D.for_all2 
                (fun _ sv1 sv2 -> StateVar.equal_state_vars sv1 sv2)
                input_state_vars
                call_inputs)

            calls

        in

        (* Return output variables from node call to re-use *)
        Some call_outputs

      (* No node call found *)
      with Not_found -> None 


(* Add node input to context *)
let add_node_input ?is_const ctx ident index_types = 

  match ctx with 

    | { node = None } -> raise (Invalid_argument "add_node_input")

    | { node = Some ({ N.inputs } as node) } -> 

      (* Get next index at root of trie *)
      let next_top_idx = D.top_max_index inputs |> succ in

      (* Add state variables for all indexes of input *)
      let inputs', ctx = 
        D.fold
          (fun index index_type (accum, ctx) ->
         
             (* Create state variable as input and contant *)
             let state_var, ctx = 
               mk_state_var
                 ~is_input:true
                 ?is_const
                 ctx
                 (scope_of_node ctx)
                 ident
                 index
                 index_type
                 N.Input
             in
             
             (* Add expression to trie of identifier *)
             (D.add (D.ListIndex next_top_idx :: index) state_var accum, ctx))
             
          index_types
          (inputs, ctx)
      in

      (* Return node with input added *)
      { ctx with node = Some { node with N.inputs = inputs' } }


(* Add node output to context *)
let add_node_output ?(is_single = false) ctx ident index_types = 

  match ctx with 

    | { node = None } -> raise (Invalid_argument "add_node_output")

    | { node = Some ({ N.outputs } as node) } -> 

      (* Get next index at root of trie *)
      let next_top_idx = D.top_max_index outputs |> succ in

      let outputs', ctx = 
        D.fold
          (fun index index_type (accum, ctx) ->
         
             (* Create state variable as input and contant *)
             let state_var, ctx = 
               mk_state_var
                 ~is_input:false
                 ctx
                 (scope_of_node ctx)
                 ident
                 index
                 index_type
                 N.Output
             in
             
             let index' = 
               if is_single then index else 
                 D.ListIndex next_top_idx :: index
             in
             (* Add expression to trie of identifier *)
             (D.add index' state_var accum, ctx))
             
          index_types
          (outputs, ctx)
      in

      (* Return node with outputs added *)
      { ctx with node = Some { node with N.outputs = outputs' } }


(* Add node local to context *)
let add_node_local ?(ghost = false) ctx ident index_types = 

  match ctx with 

    | { node = None } -> raise (Invalid_argument "add_node_local")

    | { node = Some ({ N.locals } as node) } -> 

      (* Create state variable for each stream *)
      let local, ctx = 
        D.fold
          (fun index index_type (accum, ctx) ->
             
             (* Create state variable as input and contant *)
             let state_var, ctx = 
               mk_state_var
                 ~is_input:false
                 ~shadow:ghost
                 ctx
                 (scope_of_context ctx)
                 ident
                 index
                 index_type
                 (if ghost then N.Ghost else N.Local)
             in
             
             (* Add expression to trie of identifier *)
             (D.add index state_var accum, ctx))
             
          index_types
          (D.empty, ctx)
      in

      (* Return node with local variable added *)
      { ctx with node = Some { node with N.locals = local :: locals } }


(* Add node contract to context *)
let add_node_global_contract ctx pos contract = 

  match ctx with 

    | { node = None } -> raise (Invalid_argument "add_node_contract")

    | { node = Some ({ N.contracts = (Some _, _) } ) } -> 

      fail_at_position
        pos
        "Global contract for node already defined"
      
    | { node = Some ({ N.contracts = (None, mode_contracts) } as node) } -> 

      (* Return node with contract added *)
      { ctx with 
          node = 
            Some
              { node with 
                  N.contracts = 
                    (Some contract, mode_contracts) } }


(* Add node contract to context *)
let add_node_mode_contract ctx pos contract_name contract = 

  match ctx with 

    | { node = None } -> raise (Invalid_argument "add_node_contract")

    | { node = Some ({ N.contracts = (global_contract, mode_contracts) } as node) } -> 

      (* Return node with contract added *)
      { ctx with 
          node = 
            Some
              { node with 
                  N.contracts = 
                    (global_contract, (contract_name, contract) :: mode_contracts) } }


(* Add node assert to context *)
let add_node_assert ctx expr = 

  match ctx with 

    | { node = None } -> raise (Invalid_argument "add_node_assert")

    | { node = Some ({ N.asserts } as node) } -> 

      (* Return node with assertion added *)
      { ctx with node = Some { node with N.asserts = expr :: asserts } }


(* Add node assert to context *)
let add_node_property ctx source expr = 

  match ctx with 

    | { node = None } -> raise (Invalid_argument "add_node_property")

    | { node = Some { N.props; N.outputs; N.observers; N.locals } } -> 

      (* State variable for property and changed environment *)
      let state_var, ctx =

        if 

          (* Property is a state variable at current offset? *)
          E.is_var expr

        then 

          (* State variable of expression *)
          let state_var = E.state_var_of_expr expr in

          (* Make state variable an observer *)
          let ctx = observer_of_state_var ctx state_var in

          (* Return changed context and state variable of
               expression *)
            (state_var, ctx)

        else

          (* State variable of abstraction variable *)
          let state_var, ctx = 
            mk_state_var_for_expr
              ctx
              add_state_var_to_observers
              expr 
          in

          (* State variable is a locally abstracted variable *)
          N.set_state_var_source state_var N.Abstract;

          (* Return changed context and new state variable *)
          (state_var, ctx)

      in

      match ctx with 
        | { node = None } -> assert false
        | { node = Some n } -> 

          (* Return node with property added *)
          { ctx with 
              node = Some { n with
                              N.props = (state_var, source) :: props } }


(* Add state var as observer if it has to be lifted as a property,
   otherwise as a local variable *)
let lift_if_property pos ctx state_var = 

  match ctx with 

    | { node = None } -> raise (Invalid_argument "lift_if_property")

    | { node = Some ({ N.name; N.props } as node) } -> 


      try 

        (* Find source of property *)
        (match List.assq state_var props with

          (* Properties to lift to the calling node *)
          | TermLib.Generated _
          | TermLib.Instantiated _
          | TermLib.PropAnnot _ -> 

            (* Fresh observer for the property *)
            let sv', ctx = 
              mk_fresh_observer
                ctx
                (StateVar.type_of_state_var state_var) 
            in

            (* Mark relatation between state variable and instance *)
            N.set_state_var_instance sv' pos name state_var;

            (* Source for instantiated property *)
            let prop_source = 
              TermLib.Instantiated
                ([(I.string_of_ident false) name], 
                 StateVar.name_of_state_var state_var,
                 pos)
            in

            (* Add as property to node *)
            let ctx = 
              { ctx with 
                  node = Some { node 
                                with N.props = 
                                       (state_var, prop_source) :: props } }
            in

            (* Return modified context and lifted state variable *)
            (ctx, sv')

          (* Don't lift other properties *)
          | _ -> raise Not_found)

      (* Not a property *)
      with Not_found ->

        Format.printf 
          "No source for %a@."
          StateVar.pp_print_state_var state_var;

        (* Fresh local variable, not lifted as observer *)
        let sv', ctx = 
          mk_fresh_local
            ctx
            (StateVar.type_of_state_var state_var) 
        in

        (* Mark relatation between state variable and instance *)
        N.set_state_var_instance sv' pos name state_var;

        (* Return modified context and lifted state variable *)
        (ctx, sv')


(* Add node assert to context *)
let add_node_equation ctx pos state_var bounds expr = 

  match ctx with 

    | { node = None } -> raise (Invalid_argument "add_node_equation")

    | { node = Some { N.equations } } -> 

      (* Type of state variable *)
      let state_var_type = StateVar.type_of_state_var state_var in

      (* Wrap type of expression in arrays for the number of not fixed
         bounds *)
      let expr_type =
        List.fold_left
          (fun t -> function
             | N.Bound _ -> Type.mk_array t Type.t_int
             | N.Fixed _ -> t )
          (E.type_of_lustre_expr expr)
          bounds
      in

      let ctx = 

        if 

          (* Type must be a subtype of declared type *)
          Type.check_type 
            expr_type
            state_var_type

        then

          (* Nothing to add *)
          ctx

        else

          (* Type of expression may not be subtype of declared type *)
          match state_var_type, expr_type with 

            (* Declared type is integer range, expression is of type
               integer *)
            | t, s 
              when Type.is_int_range t && (Type.is_int s || Type.is_int_range s) -> 

              let (lbound, ubound) = Type.bounds_of_int_range t in

              (* Value of expression is in range of declared type: 
                 lbound <= expr and expr <= ubound *)
              let range_expr = 
                (E.mk_and 
                   (E.mk_lte (E.mk_int lbound) expr) 
                   (E.mk_lte expr (E.mk_int ubound)))
              in

              let msg =
                Format.sprintf
                  "Cannot determine correctness of subrange type, \
                   adding constraint as property and reverting to type int for \
                   variable %s." 
                  (StateVar.string_of_state_var state_var) in

              warn_at_position pos msg;

              (* Expanding type of state variable to int *)
              StateVar.change_type_of_state_var state_var (Type.mk_int ());

              (* Add property to node *)
              add_node_property ctx (TermLib.Generated [state_var]) range_expr

            | t, s -> 

              fail_at_position
                pos
                (Format.asprintf 
                   "Type mismatch in equation: %a and %a"
                   Type.pp_print_type t
                   Type.pp_print_type s)

      in

      (* Return node with equation added *)
      match ctx with 
        | { node = None } -> assert false 
        | { node = Some node } -> 

          { ctx with 
              node = Some { node with
                              N.equations = 
                                (state_var, bounds, expr) :: equations } }


(* Add node call to context *)
let add_node_call ctx node_call =

  match ctx with 

    | { node = None } -> raise (Invalid_argument "add_node_call")

    | { node = Some ({ N.calls } as node) } -> 

      (* Add node call to context *)
      { ctx with 
          node = Some { node with 
                          N.calls = node_call :: calls } }



(* Create a node from the context *)
let node_of_context = function

  (* Fail if not in a node *)
  | { node = None } -> 
    raise (Invalid_argument "node_of_context")

  (* Add abstractions to node and return *)
  | { expr_state_var_map; node = Some { N.equations } } as ctx -> 

    (* Add equations from definitions to equations *)
    let node =

      match 
        ET.fold
          (fun e sv ctx -> 
             add_node_equation ctx dummy_pos sv [] e)
          expr_state_var_map
          ctx
      with
        | { node = Some n } -> n
        | _ -> assert false
    in

    (* Return node with equations from definitions added *)
    node


(* Add node from second context to nodes of first *)
let add_node_to_context ctx node_ctx = 

  { ctx with nodes = (node_of_context node_ctx) :: ctx.nodes }


(* Mark node as main node *)
let set_node_main ctx = 

  match ctx with 

    | { node = None } -> raise (Invalid_argument "set_node_main")

    | { node = Some node } -> 

      { ctx with node = Some { node with N.is_main = true } }





(*

(* Add declaration of an identifier to trie and to context 

   Helper function for add_node_input_decl, add_node_output_decl *)
let add_to_trie
    ?is_const
    ?is_input
    ctx
    state_var_trie
    ident
    ident_types 
  state_var_source =

  (* Get next index at root of trie *)
  let next_top_idx = D.top_max_index state_var_trie |> succ in

  (* Create state variable, add as expression with index to map of
     identifiers and as state variable to node inputs *)
  let expr_of_ident', state_var_trie' = 

    D.fold

      (fun index t (expr_of_ident, state_var_trie) ->


         (* Create state variable as input and contant *)
         let state_var = 
           mk_state_var
             ?is_input
             ?is_const
             ctx
             ident
             index
             t
         in

         (* State variable is an input *)
         E.set_state_var_source state_var state_var_source;

         (* Add expression to trie of identifier *)
         (ITrie.add ident' (E.mk_var state_var E.base_clock) expr_of_ident,

          (* Add state variable to trie of inputs *)
          IdxTrie.add index' state_var state_var_trie))

      ident_types
      (expr_of_ident, state_var_trie)

  in

  ({ context with expr_of_ident = expr_of_ident' }, 
   state_var_trie')


(* Add declaration of an identifier to list and to context

   Helper function for add_node_var_decl, add_node_oracle_decl *)
let add_node_decl_to_list
    ({ expr_of_ident } as context)
    state_var_list
    state_var_source
    scope
    ident
    ?is_const
    ?is_input
    ident_types =

  (* Create state variable, add as expression with index to map of
     identifiers and as state variable to node inputs *)
  let expr_of_ident', state_var_list' = 

    IdxTrie.fold

      (fun index t (expr_of_ident, state_var_list) ->

         (* Add index to identifier *)
         let ident' = I.push_back_index index ident in

         (* Create state variable as input and contant *)
         let state_var = 
           E.mk_state_var_of_ident
             ?is_input
             ?is_const
             scope
             ident' 
             t
         in

         (* State variable is an input *)
         E.set_state_var_source state_var state_var_source;

         (* Add expression to trie of identifier *)
         (ITrie.add ident' (E.mk_var state_var E.base_clock) expr_of_ident,

          (* Add state variable to list *)
          state_var :: state_var_list))

      ident_types
      (expr_of_ident, state_var_list)

  in

  ({ context with expr_of_ident = expr_of_ident' }, 
   state_var_list')



*)




(* 
   Local Variables:
   compile-command: "make -k -C .."
   indent-tabs-mode: nil
   End: 
*)
  