(* This file is part of the Kind 2 model checker.

   Copyright (c) 2015 by the Board of Trustees of the University of Iowa

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

(* We have three hashconsed types: uninterpreted function symbols,
   symbols and terms. Hashconsing has been extended to store a record
   of properties with each value, here we store mainly type
   information. 

   Uninterpreted function symbols are hashconsed separately, since
   they need to be declared in each solver instance. A hashconsed
   uninterpreted function symbol stores its type and by iterating over
   or folding the hashcons table we can obtain the necessary
   declarations.

   Symbols are hashconsed so that we can rely on physical equality for
   comparison, as of now there are no useful properties to be stored
   alongside symbols. In particular the `NUMERAL i, `DECIMAL f and
   `SYM (s, t) symbols need to be hashconsed for physical equality. 

   Terms are hashconsed for maximal sharing, comparison with physical
   equality and to store type information.

   For all three types hashtables, maps and set are provided. *)


(* ********************************************************************* *)
(* Types and hash-consing                                                *)
(* ********************************************************************* *)


(* Basic types for terms, input module for {!Ltree.Make} functor *)
module BaseTypes =
struct 

  (* Hashconsed symbols *)
  type symbol = Symbol.t

  (* Hashconsed variables *)
  type var = Var.t

  (* Hashconsed types *)
  type sort = Type.t

  (* Hashconsed attributes *)
  type attr = TermAttr.t

  (* Hash value of a symbol *)
  let hash_of_symbol = Symbol.hash_symbol 

  (* Hash value of a variable *)
  let hash_of_var = Var.hash_var 

  (* Hash value of a sort *)
  let hash_of_sort = Type.hash_type 

  (* Hash value of an attribute *)
  let hash_of_attr = TermAttr.hash_attr 

  (* Get sort of a variable *)
  let sort_of_var = Var.type_of_var

  let import_symbol = Symbol.import 

  let import_var = Var.import 

  let import_sort = Type.import 

  (* Pretty-print a symbol *)
  let pp_print_symbol = Symbol.pp_print_symbol

  (* Pretty-print a variable *)
  let pp_print_var = Var.pp_print_var

  (* Pretty-print a type *)
  let pp_print_sort = Type.pp_print_type

  (* Pretty-print an attribute *)
  let pp_print_attr = TermAttr.pp_print_attr

end


(* AST with base types *)
module T = Ltree.Make (BaseTypes)

(* Hashconsed term over symbols, variables and sorts *)
type t = T.safe T.t

(* Hashconsed lambda expression *)
type lambda = T.safe T.lambda

let stats = T.stats

(* Return the node of the hashconsed term *)
let node_of_term = T.node_of_t


(* Flatten top node of term *)
let destruct = T.destruct


(* Return true if the term is a named term *)
let is_named t =  

  (* Term is annotated? *)
  if T.is_annot t then

    (* Return true if annotation is a name *)
    T.annot_of_t t |> TermAttr.is_named 

  else

    (* Term is not annotated, thus not named *)
    false


(* Return the term of a named term *)
let term_of_named t =

  (* Term is annotated? *)
  if T.is_annot t then

    (* Get annotation of term *)
    let a = T.annot_of_t t in
    
    (* Annotation is a name? *)
    if TermAttr.is_named a then
      
      (* Return term in annotation *)
      T.annot_t_of_t t

    else
      
      (* Annotation is not a name *)
      invalid_arg "term_of_named"

  else

    (* Term is not annotated *)
    invalid_arg "term_of_named"


(* Return the name of a named term *)
let name_of_named t =

  (* Term is annotated? *)
  if T.is_annot t then

    (* Get annotation of term *)
    let a = T.annot_of_t t in
    
    (* Annotation is a name? *)
    if TermAttr.is_named a then
      
      (* Get name from annotation *)
      let (s, n) = TermAttr.named_of_attr a in

      (* Fail if not in term namespace, otherwise return integer *)
      if s <> "t" then invalid_arg "term_of_named" else n

    else
      
      (* Annotation is not a name *)
      invalid_arg "term_of_named"

  else

    (* Term is not annotated *)
    invalid_arg "term_of_named"



(* Return true if the term is an integer constant *)
let rec is_numeral t = match destruct t with 

  (* Term is a numeral constant *)
  | T.Const s when Symbol.is_numeral s -> true

  (* Term is a decimal constant coinciding with an integer *)
  | T.Const s when 
      Symbol.is_decimal s && Decimal.is_int (Symbol.decimal_of_symbol s) -> 

    true

  (* Term is a negated numeral constant *)
  | T.App (s, [a]) when s == Symbol.s_minus && is_numeral a -> true

  | _ -> false


(* Return integer constant of a term *)
let rec numeral_of_term t = match destruct t with 

  (* Term is a numeral constant *)
  | T.Const s when Symbol.is_numeral s -> Symbol.numeral_of_symbol s

  (* Term is a decimal constant coinciding with an integer *)
  | T.Const s when 
      Symbol.is_decimal s && Decimal.is_int (Symbol.decimal_of_symbol s) -> 

    Numeral.of_big_int (Decimal.to_big_int (Symbol.decimal_of_symbol s))

  (* Term is a negated numeral constant *)
  | T.App (s, [a]) when s == Symbol.s_minus && is_numeral a -> 

    Numeral.(~- (numeral_of_term a))

  | _ -> invalid_arg "numeral_of_term"


(* Return decimal constant of a term *)
let rec decimal_of_term t = match destruct t with 

  (* Term is a decimal constant *)
  | T.Const s when Symbol.is_decimal s -> Symbol.decimal_of_symbol s

  (* Term is a negated decimal constant *)
  | T.App (s, [a]) when s == Symbol.s_minus && is_decimal a -> 
    Decimal.(~- (decimal_of_term a))

  (* Term is an integer division *)
  | T.App (s, [n; d]) when 
      s == Symbol.s_div && is_numeral n && is_numeral d ->

    let n' = 
      Decimal.of_big_int 
        (Numeral.to_big_int 
           (numeral_of_term n))
    in

    let d' = 
      Decimal.of_big_int 
        (Numeral.to_big_int 
           (numeral_of_term d))
    in

    Decimal.(n' / d')

  | _ -> invalid_arg "decimal_of_term"


(* Return true if the term is a decimal constant *)
and is_decimal t = match destruct t with 

  (* Term is a decimal constant *)
  | T.Const s when Symbol.is_decimal s -> true

  (* Term is a negated decimal constant *)
  | T.App (s, [a]) when s == Symbol.s_minus && is_decimal a -> true

  (* Term is an integer division *)
  | T.App (s, [n; d]) when 
      s == Symbol.s_div && 
      (is_numeral n || is_decimal n && Decimal.is_int (decimal_of_term n)) && 
      (is_numeral d || is_decimal d && Decimal.is_int (decimal_of_term d)) ->
    
    true

  | _ -> false


(* Return true if the term is a Boolean constant *)
let rec is_bool t = match destruct t with 

  (* Term is a Boolean constant *)
  | T.Const s when Symbol.is_bool s -> true

  (* Term is a negated Boolean constant *)
  | T.App (s, [a]) when s == Symbol.s_not && is_bool a -> true

  | _ -> false


(* Return Boolean constant of a term *)
let rec bool_of_term t = match destruct t with 

  (* Term is a Boolean constant *)
  | T.Const s when Symbol.is_bool s -> Symbol.bool_of_symbol s

  (* Term is a negated numeral constant *)
  | T.App (s, [a]) when s == Symbol.s_not && is_bool a -> 
    not (bool_of_term a)

  | _ -> invalid_arg "bool_of_term"


(* Return true if the term is an application of the select operator *)
let is_select t = match node_of_term t with

  (* Top symbol is a select operator *)
  | T.Node (s, [a; i]) -> s == Symbol.s_select
                                 
  | _ -> false


(* Return the indexes of the select operator 

   The array argument of a select is either another select operation
   or a variable. For the expression [(select (select A j) k)] return
   the pair [A] and [[j; k]]. *)
let rec indexes_and_var_of_select' accum t = match destruct t with 

  | T.Var v -> (v, List.rev accum)

  | T.App (s, [a; i]) when s == Symbol.s_select -> 

    indexes_and_var_of_select' (i :: accum) a

  | T.Attr (t, _) ->  indexes_and_var_of_select' accum t

  |  _ -> invalid_arg "indexes_of_select"



(* Return the indexes of the select operator *)
let indexes_and_var_of_select t = indexes_and_var_of_select' [] t

 


(* ********************************************************************* *)
(* Hashtables, maps and sets                                             *)
(* ********************************************************************* *)


(* Comparison function on terms *)
let compare = T.compare


(* Equality function on terms *)
let equal = T.equal


(* Hashing function on terms *)
let hash = T.hash 


(* Unique identifier for term *)
let tag = T.tag

module T_safe = 
struct 
  type t = T.safe T.t
  let equal = T.equal
  let compare = T.compare
  let hash = T.hash
end

(* Hashtable *)
module TermHashtbl = Hashtbl.Make (T_safe)


(* Set 

   TODO: Try patricia trees over hashcons tags for sets *)
module TermSet = Set.Make (T_safe)


(* Map *)
module TermMap = Map.Make (T_safe)


(* ********************************************************************* *)
(* Pretty-printing                                                       *)
(* ********************************************************************* *)


(* Pretty-print a term *)
let pp_print_term ppf t = T.pp_print_term ppf t

(* Pretty-print a hashconsed term to the standard formatter *)
let print_term t = pp_print_term Format.std_formatter t

(* Return a string representation of a term *)
let string_of_term t = string_of_t pp_print_term t

(* Pretty-print a term *)
let pp_print_lambda ppf t = T.pp_print_lambda ppf t

(* Pretty-print a hashconsed term to the standard formatter *)
let print_lambda t = pp_print_lambda Format.std_formatter t

(* Return a string representation of a term *)
let string_of_lambda t = string_of_t pp_print_lambda t

(* ********************************************************************* *)
(* Folding and utility functions on terms                                *)
(* ********************************************************************* *)

(*
(* Evaluate a term bottom-up right-to-left *)
let eval = T.eval 
*)

(* Evaluate a term bottom-up right-to-left, given the flattened term
   as argument *)
let eval_t = T.eval_t 

(* Evaluate a term bottom-up right-to-left, given the flattened term
   as argument *)
let eval_lambda = T.eval_lambda

(* Bottom-up right-to-left map of the term 

   Must hashcons bottom-up since term was destructed and not all terms
   are necessarily in the hashcons table. *)
let map = T.map


(* ********************************************************************* *)
(* Type checking for terms                                               *)
(* ********************************************************************* *)


(* Return the type of a term node 

   TODO: handle IntRange type correctly 
*)
let rec type_of_term t = match T.destruct t with

  (* Return declared type of variable *)
  | T.Var v -> Var.type_of_var v

  (* Return type of a constant *)
  | T.Const s -> 

    (

      (* Get symbol *)
      match Symbol.node_of_symbol s with 

        (* Boolean constants *)
        | `TRUE 
        | `FALSE -> Type.mk_bool ()
            
        (* Integer constant *)
        | `NUMERAL _ -> Type.mk_int ()

        (* Real constant *)
        | `DECIMAL _ -> Type.mk_real ()
(*
        (* Bitvector constant *)
        | `BV b -> Type.mk_bv (length_of_bitvector b)
*)        
        (* Uninterpreted constant *)
        | `UF s -> UfSymbol.res_type_of_uf_symbol s

        (* No other symbols are nullary *)
        | _ -> assert false 

    )

  (* Return type of a function application *)
  | T.App (s, l) -> 
    
    (

      (* Get symbol *)
      match Symbol.node_of_symbol s with 

        (* Boolean-valued functions *)
        | `NOT 
        | `IMPLIES
        | `AND
        | `OR
        | `XOR
        | `IS_INT
        | `EQ
        | `DISTINCT
        | `LEQ
        | `LT
        | `GEQ
        | `GT
        | `DIVISIBLE _ -> Type.mk_bool ()
(*
        | `BVULT -> Type.mk_bool ()
*)
        (* Integer-valued functions *)
        | `TO_INT
        | `MOD
        | `ABS
        | `INTDIV -> Type.mk_int ()
          
        (* Real-valued functions *)
        | `TO_REAL
        | `DIV -> Type.mk_real ()
(*          
        (* Bitvector-valued function *)
        | `CONCAT -> 

          (match l with 

            (* Concat is binary *)
            | [a; b] -> 
              
              (* Compute width of resulting bitvector *)
              (match 
                  (Type.node_of_type (type_of_term a), 
                   Type.node_of_type (type_of_term b))
               with
                 | Type.BV i, Type.BV j -> 

                   Type.mk_bv (i + j)

                 | _ -> assert false)
                
            | _ -> assert false)
     
            
        (* Bitvector-valued function *)
        | `EXTRACT (i, j) -> 
          
          (* Compute width of resulting bitvector *)
          Type.mk_bv
            ((Numeral.to_int j) - (Numeral.to_int i) + 1)
*)
            
        (* Array-valued function *)
        | `SELECT -> 

          (match l with 

            (* Select is binary *)
            | [a; _] -> 

              (match Type.node_of_type (type_of_term a) with
                | Type.Array (t, _) -> t
                | _ -> assert false)

            | _ -> assert false)

        (* Return type of first argument *)
        | `MINUS
        | `PLUS
        | `TIMES -> 

          (match l with 
              
            (* Function must be at least binary *)
            | a :: _ -> type_of_term a
            | _ -> assert false)

(*
        | `BVNOT
        | `BVNEG
        | `BVAND
        | `BVOR
        | `BVADD
        | `BVMUL
        | `BVDIV
        | `BVUREM
        | `BVSHL
        | `BVLSHR
*)
(*
        | `STORE -> 

          (match l with 
              
            (* Function must be at least binary *)
            | a :: _ -> type_of_term a
            | _ -> assert false)
*)


        (* Return type of second argument *)
        | `ITE -> 

          (match l with 

            (* ite must be ternary *)
            | [_; a; _] -> type_of_term a
            | _ -> assert false)
            
        (* Uninterpreted constant *)
        | `UF s -> UfSymbol.res_type_of_uf_symbol s
  
        (* Ill-formed terms *)
        | `TRUE
        | `FALSE
        | `NUMERAL _
        | `DECIMAL _ -> assert false
(*
        | `BV _ -> assert false
*)
    )

  (* Return type of term *)
  | T.Attr (t, _) -> type_of_term t


(* Type checking disabled

   TODO: re-implement this with a function Types.subtype and allow
   IntRange as subtype of Int etc.

(* Return true of the list of types is valid for the symbol *)
let type_check_app s a = 

  match Symbol.node_of_symbol with

    (* Nullary function symbols *)
    | `TRUE
    | `FALSE
    | `NUMERAL _
    | `DECIMAL _
    | `BV _
        when List.length a = 0 -> true

    (* Unary polymorphic function symbols *)
    | `ATTR _
        when List.length a = 1 -> true

    (* Unary function of Boolean arguments *)
    | `NOT
        when a = [Type.Bool] -> true

    (* Unary function symbols of integer arguments *)
    | `ABS 
    | `TO_REAL 
    | `DIVISIBLE _ 
        when a = [Type.Int] -> true

    (* Unary function symbols of real arguments *)
    | `IS_INT 
    | `TO_INT 
        when a = [Type.Real] -> true

    (* Variadic, but at least binary function symbols of Boolean arguments *)
    | `IMPLIES 
    | `AND 
    | `OR 
    | `XOR 
        when 
          (List.for_all (Type.compatible Type.Bool) a) && 
            List.length a >= 2 -> true

    (* Ternary function symbol with first argument boolean and second
     and third arguments of identical type *)
    | `ITE -> 
      (match a with 
        | [p; t; f] 
            when 
              p = Type.Bool && 
              (Type.compatible t f) && 
              (Type.compatible f t) -> true
        | _ -> false)
        
  (* Polymorphic, variadic but at least binary function symbols with
     arguments of equal types *)
  | { Hashcons.node = `EQ }
  | { Hashcons.node = `DISTINCT }
      when List.length a >= 2 
        && (List.for_all (Type.compatible (List.hd a)) a) -> true

  (* Variadic but at least unary function symbols of all real or all
     integer arguments *)
  | { Hashcons.node = `MINUS }
      when List.length a >= 1 
        && 
          (match List.hd a with 
            | Type.Real 
            | Type.Int -> true 
            | _ -> false)
        && (List.for_all (Type.compatible (List.hd a)) a) -> true
    
  (* Variadic, but at least binary function symbols of all real or all
     integer arguments *)
  | { Hashcons.node = `PLUS }
  | { Hashcons.node = `TIMES }
  | { Hashcons.node = `LEQ }
  | { Hashcons.node = `LT }
  | { Hashcons.node = `GEQ }
  | { Hashcons.node = `GT }
      when List.length a >= 2
        && 
          (match List.hd a with 
            | Type.Real 
            | Type.Int -> true 
            | _ -> false)
        && (List.for_all (Type.compatible (List.hd a)) a) -> true

  (* Variadic, but at least binary function symbols of real arguments *)
  | { Hashcons.node = `DIV }
      when (List.for_all (Type.compatible Type.Real) a) && List.length a >= 2 -> true
    
  (* Variadic, but at least binary function symbols of integer arguments *)
  | { Hashcons.node = `INTDIV }
  | { Hashcons.node = `MOD }
      when (List.for_all (Type.compatible Type.Int) a) && List.length a >= 2 -> true


  (* Function symbol with a defined signature of fixed arity *)
  | { Hashcons.node = `UF s }
      when (UfSymbol.arg_type_of_uf_symbol s) = a -> true

  | _ -> false

*)

(* ********************************************************************* *)
(* Constructors                                                          *)
(* ********************************************************************* *)


(* Return a hashconsed constant *)
let mk_const = T.mk_const


(* Return a hashconsed variable with an empty index *)
let mk_var = T.mk_var


(* Return a hashconsed function application

   TODO: type check arguments *)
let mk_app = T.mk_app
      
(*
(* Return a hashconsed tree *)
let mk_term = T.mk_term
*)

(* Return a hashconsed tree *)
let mk_lambda = T.mk_lambda


(* Return a hashconsed let binding *)
let mk_let  = T.mk_let


(* Return a hashconsed existentially quantified term *)
let mk_exists = T.mk_exists 


(* Return a hashconsed universally quantified term *)
let mk_forall = T.mk_forall


(* Import a term from a different instance into this hashcons table *)
let import = T.import 

(* Import a term from a different instance into this hashcons table *)
let import_lambda = T.import_lambda 

(* Flatten top node of term *)
let construct = T.construct


(* Is the term a Boolean atom? *)
let rec is_atom t = match T.destruct t with 

  (* Function application *)
  | T.App (s, l) -> 

    (* Must be of Boolean type *)
    (type_of_term t == Type.mk_bool ()) &&

    (* All subterms must be not Boolean *)
    (List.for_all
       (function e -> 
         T.eval_t
           (function 

             (* Function application *)
             | T.App (s, l) as f -> 

               (function r -> 

                 (* Must not be of Boolean type *)
                 (not 
                    (type_of_term (T.construct f) == 
                       Type.mk_bool ())) &&

                 (* All subterms must not be of Boolean type *)
                 (List.for_all
                    (function t -> 
                      not (type_of_term t == Type.mk_bool ()))
                    l) &&

                 (* All subterms must be atoms *)
                 (List.fold_left (fun a e -> a && e) true r))

             (* Constant must not be of Boolean type *)
             | T.Const _ as f -> 

               (function 
                 | [] -> 
                   (not 
                      (type_of_term (T.construct f) == 
                         Type.mk_bool ()))
                 | _ -> assert false)

             (* Variable must not be of Boolean type *)
             | T.Var v -> 

               (function 
                 | [] -> 
                   (not (Var.type_of_var v == Type.mk_bool ()))
                 | _ -> assert false)

             (* Annotated term *)
             | T.Attr (t, _) -> (function _ -> is_atom t))

           e)
       l)

  (* A constant is a Boolean atom if it is of Boolean type *)
  | T.Const _ -> type_of_term t == Type.mk_bool ()

  (* A variable is a Boolean atom if it is of Boolean type *)
  | T.Var v -> Var.type_of_var v == Type.mk_bool ()

  (* Annotated term *)
  | T.Attr (t, _) -> is_atom t



(* Return true if the top symbol of the term is a negation *)
let is_negated term = match T.destruct term with
  | T.App (s, _) when s == Symbol.s_not -> true
  | _ -> false


(* Return a hashconsed constant *)
let mk_const_of_symbol_node s = 

  (* Hashcons the symbol and construct a constant term *)
  let s' = Symbol.mk_symbol s in mk_const s'


(* Return a hashconsed function application *)
let mk_app_of_symbol_node s a = 

  (* Hashcons the symbol and construct an application term *)
  let s' = Symbol.mk_symbol s in mk_app s' a


(* Return the hashconsed propositional constant true *)
let mk_true () = mk_const_of_symbol_node `TRUE


(* Keep a the hashconsed true as a value *)
let t_true = mk_true ()


(* Return the hashconsed propositional constant false *)
let mk_false () = mk_const_of_symbol_node `FALSE


(* Keep a the hashconsed false as a value *)
let t_false = mk_false ()


(* Hashcons a unary negation *)
let mk_not t = mk_app_of_symbol_node `NOT [t]


(* Hashcons an implication *)
let mk_implies = function
  | [] -> mk_false ()
  | [a] -> a
  | a -> mk_app_of_symbol_node `IMPLIES a


(* Hashcons an conjunction, accept nullary and unary conjunctions and
   convert to a propositional constant and return the single argument,
   respectively. *)
let mk_and = function
  | [] -> mk_true ()
  | [a] -> a
  | a -> mk_app_of_symbol_node `AND a 


(* Hashcons a disjunction, accept nullary and unary disjunctions and
   convert to a propositional constant and return the single argument,
   respectively. *)
let mk_or = function 
  | [] -> mk_false ()
  | [a] -> a 
  | a -> mk_app_of_symbol_node `OR a 


(* Hashcons an exclusive disjunction, fail if list of arguments is
   empty and if only one argument given return it. *)
let mk_xor = function
  | [] -> invalid_arg "Term.mk_xor"
  | [a] -> a
  | a -> mk_app_of_symbol_node `XOR a 


(* Hashcons an equation, a chain of equations for arity greater than
   binary *)
let mk_eq a = mk_app_of_symbol_node `EQ a


(* Hashcons an pairwise disjointness predicate *)
let mk_distinct a = mk_app_of_symbol_node `DISTINCT a


(* Hashcons a ternary if-then-else expression *)
let mk_ite p l r = mk_app_of_symbol_node `ITE [p; l; r]


(* Hashcons a unary minus or higher arity minus *)
let mk_minus a = mk_app_of_symbol_node `MINUS a


(* Hashcons an integer numeral *)
let mk_num n = (* mk_const_of_symbol_node (`NUMERAL n) *)
                
  (* Positive numeral or zero *)                
  if Numeral.(n >= zero) then           
                
    mk_const_of_symbol_node (`NUMERAL n)                
                
  else          
                
    (* Wrap a negative numeral in a unary minus *)              
    mk_minus [(mk_const_of_symbol_node (`NUMERAL (Numeral.(~- n))))]


(* Hashcons an integer numeral given an integer *)
let mk_num_of_int i = mk_num (Numeral.of_int i)


(* Hashcons a real decimal *)
(* let mk_dec d = mk_const_of_symbol_node (`DECIMAL d) *)
let mk_dec d =

  (* Positive rational or zero *)               
  if Decimal.(d >= zero) then           
                
    mk_const_of_symbol_node (`DECIMAL d)                
                
  else          
                
    (* Wrap a negative rational in a unary minus *)             
    mk_minus [(mk_const_of_symbol_node (`DECIMAL (Decimal.(~- d))))]

(*

(* Hashcons a floating-point decimal given a float *)
let mk_dec_of_float = function

  (* Positive decimal *)
  | f when f >= 0. -> 
    mk_const_of_symbol_node (`DECIMAL (decimal_of_float f))

  (* Negative decimal *)
  | f -> 
    mk_minus [mk_const_of_symbol_node (`DECIMAL (decimal_of_float (-. f)))]
*)

(*
(* Hashcons a bitvector *)
let mk_bv b = mk_const_of_symbol_node (`BV b)
*)

(* Hashcons an addition *)
let mk_plus = function
  | [] -> invalid_arg "Term.mk_plus"
  | [a] -> a
  | a -> mk_app_of_symbol_node `PLUS a


(* Hashcons a multiplication *)
let mk_times = function
  | [] -> invalid_arg "Term.mk_times"
  | [a] -> a
  | a -> mk_app_of_symbol_node `TIMES a


(* Hashcons a real division *)
let mk_div = function
  | [] -> invalid_arg "Term.mk_div"
  | [a] -> a
  | a -> mk_app_of_symbol_node `DIV a


(* Hashcons an integer division *)
let mk_intdiv = function
  | [] -> invalid_arg "Term.mk_intdiv"
  | [a] -> a
  | a -> mk_app_of_symbol_node `INTDIV a


(* Hashcons a binary modulus operator *)
let mk_mod a b = mk_app_of_symbol_node `MOD [a; b]


(* Hashcons a unary absolute value function *)
let mk_abs t = mk_app_of_symbol_node `ABS [t]


(* Hashcons a binary less than or equal relation, a chain of relation
   for higher arities *)
let mk_leq = function
  | [] | [_] -> invalid_arg "Term.mk_leq"
  | a -> mk_app_of_symbol_node `LEQ a


(* Hashcons a binary less than relation, a chain of relation for higher
   arities *)
let mk_lt  = function
  | [] | [_] -> invalid_arg "Term.mk_lt"
  | a -> mk_app_of_symbol_node `LT a


(* Hashcons a binary greater than or equal relation, a chain of relations
   for higher arities *)
let mk_geq  = function
  | [] | [_] -> invalid_arg "Term.mk_geq"
  | a -> mk_app_of_symbol_node `GEQ a


(* Hashcons a binary greater than relation, a chain of relations for
   higher arities *)
let mk_gt  = function
  | [] | [_] -> invalid_arg "Term.mk_gt"
  | a -> mk_app_of_symbol_node `GT a


(* Hashcons a unary conversion to a real decimal *)
let mk_to_real t = mk_app_of_symbol_node `TO_REAL [t]


(* Hashcons a unary conversion to an integer numeral *)
let mk_to_int t = mk_app_of_symbol_node `TO_INT [t]


(* Hashcons a predicate for coincidence of a real with an integer *)
let mk_is_int t = mk_app_of_symbol_node `IS_INT [t]


(* Hashcons a divisibility predicate for the given divisor *)
let mk_divisible n t = mk_app_of_symbol_node (`DIVISIBLE n) [t]

(* Hashcons an array read *)
let mk_select a i = mk_app_of_symbol_node `SELECT [a; i]

(* Generate a new tag *)
let newid =
  let r = ref 0 in
  fun () -> incr r; !r


(* Hashcons a named term *)
let mk_named t = 

  (* Name term with its unique tag *)
  let n = newid () in

  (* Return name and named term

     Order pair in this way to put it an association list *)
  (n, T.mk_annot t (TermAttr.mk_named "t" n))


(* Hashcons a named term *)
let mk_named_unsafe t s n = 

  (* Reject namespace used by mk_named to avoid clashes *)
  if s = "t" then raise (Invalid_argument "mk_named_unsafe") else
    
    (* Return named term *)
    T.mk_annot t (TermAttr.mk_named s n)


(* Hashcons an uninterpreted function or constant *)
let mk_uf s = function 

  (* Create a constant for an empty list of arguments *)
  | [] -> mk_const_of_symbol_node (`UF s)

  (* Create a function application for non-empty list of arguments *)
  | a -> mk_app_of_symbol_node (`UF s) a

   
(* Hashcons a propositional constant *)
let mk_bool = function 
  | true -> mk_const_of_symbol_node `TRUE
  | false -> mk_const_of_symbol_node `FALSE


(* Hashcons an increment of the term one *)
let mk_succ t = mk_app_of_symbol_node `PLUS [t; (mk_num_of_int 1)]


(* Hashcons a decrement of the term by one *)
let mk_pred t = mk_app_of_symbol_node `MINUS [t; (mk_num_of_int 1)]


(* Hashcons a negation of the term, avoiding double negation *)
let negate t = match T.destruct t with 

  (* Top symbol is a negation, then remove negation 

     Must hashcons bottom-up since term was destructed and not all
     terms are necessarily in the hashcons table. *)
  | T.App (s, [t]) when s == Symbol.s_not -> t

  (* Top symbol is not a negation, then negate given term *)
  | _ -> mk_not t



(* Negates a term by modifying the top node if it is a not, true,
   false, or an arithmetic inequality. *)
let negate_simplify t = match T.destruct t with

  | T.Const symb ->
     ( match Symbol.node_of_symbol symb with

       (* Bool constants. *)
       | `TRUE -> t_false
       | `FALSE -> t_true
                     
       | _ -> mk_not t )

  | T.App (symb, kids) ->
     ( match Symbol.node_of_symbol symb, kids with

       (* Top symbol is a negation, removing it. *)
       | `NOT, [term] -> term

       (* Aritmetic inequalities. *)
       | `LEQ, kids -> mk_gt kids
       | `LT, kids -> mk_geq kids
       | `GT, kids -> mk_leq kids
       | `GEQ, kids -> mk_lt kids

       | _ -> mk_not t )

  (* Top symbol is not a negation, then negate given term *)
  | _ -> mk_not t


(* Remove negation if it is the topmost symbol *)
let unnegate t = match T.destruct t with

  (* Top symbol is a negation, then remove negation 

     Must hashcons bottom-up since term was destructed and not all
     terms are necessarily in the hashcons table. *)
  | T.App (s, [t]) when s == Symbol.s_not -> t

  (* Top symbol is not a negation, then return unchanged *)
  | _ -> t 

(* Convert (= 0 (mod t n)) to (divisble n t) 

   Use this function in a Term.map, therefore it considers only the
   top symbol.
   
   TODO: Also accept negative constants as n. *)
let mod_to_divisible env term = 

  try 

  let mod_to_divisible' l r = 
    match T.destruct_unsafe env l, T.destruct_unsafe env r with 
      
      | T.Const c, T.App (s, [t; n]) 
        when 
          Symbol.is_numeral c &&  
          Symbol.numeral_of_symbol c |> Numeral.(equal zero) &&
          Symbol.equal_symbols s Symbol.s_mod ->
        
        (match T.destruct_unsafe env n with 
          
          | T.Const d when Symbol.is_numeral d ->
            
            (* Return (divisible n t) *)
            Some (mk_divisible (Symbol.numeral_of_symbol d) t)
              
          | _ -> None)
        
      | _ -> None

  in

  (* Check top symbol in term *)
  (match T.destruct_unsafe env term with 

    (* Equality between two term? *)
    | T.App (s, [l; r]) when Symbol.equal_symbols s Symbol.s_eq -> 

      (* Try to convert (= 0 (mod t n)) to (divisible n t) *)
      mod_to_divisible' l r 
        
      |> 

      (function 

        (* Return converted term *)
        | Some term' -> Some term' 
          
        (* Try to convert (= (mod t n) 0) to (divisible n t) *)
        | None -> mod_to_divisible' r l)

      |> 

      (function 
        
        (* Return converted term *)
        | Some term' -> term' 
        
        (* Keep original term *)
        | None -> term)

    | _ -> term)

  (* Keep original term if quantifed *)
  with Invalid_argument _ -> term

(*





  let rewrite_eq (s, a) = 

    if Symbol.equal_symbols s_eq s then 

      match a with 

        | [l; r] -> 

          if Termis_node 

        | _ -> term

    else

      term

  in


  if T.is_node term then 
    
    (T.node_symbol_of_t term, T.node_args_of_t term)
    |> rewrite_eq
    
  else
    
    term
  




  let T.node_sym











  let mod_to_divisible' t_mod = 

    if 
      
      (* Top symbol is mod? *)
      T.node_symbol_of_t t_mod
      |> Symbol.equal_symbols Symbol.s_mod 

    then
      
      (* Get arguments of mod *)
      match T.node_args_of_t t_mod with 

        (* Get second argument of mod *)
        | [_; t_const] ->
          
          (* Second argument is constant? *)
          if T.is_leaf t_const then 

            (* Get symbol of constant *)
            let s_const = T.leaf_of_t t_const in

            (* Constant is a numeral? *)
            if Symbol.is_numeral s_const then 

              (* Return (divisible n t) *)
              mk_divisible (Symbol.numeral_of_symbol n) t

            else

              (* Constant is not a numeral  *)
              term

          else
            
            (* Second argument is not a constant *)
            term
              
        (* mod is a binary operator *)
        | _ -> assert false


    else

      (* Top symbol is not mod *)
      term

  in

  if 

    (* Top symbol is equality? *)
    T.node_symbol_of_t term
    |> Symbol.equal_symbols Symbol.s_eq

  then

    match T.node_args_of_t term with 

      | [l; r] -> 

        (* First argument is constant? *)
        if T.is_leaf l then 
          
          (* Get symbol of constant *)
          let s_const = T.leaf_of_t t_const in
          
          (* Constant is a numeral? *)
          if Symbol.is_numeral s_const then 
            
            

            (* Return (divisible n t) *)
            mk_divisible (Symbol.numeral_of_symbol n) t
              
            else
        

      (* Equation is not binary *)
      | _ -> term

  else

    (* Top symbol is not equality *)
    term


  match T.node_of_t term with

    (* Term is (= 0 t) or (= t 0) *)
    | T.Node (s_eq, [l; r])
      when 
        s_eq == Symbol.s_eq && 
        is_numeral l &&
        Numeral.(equal (numeral_of_term l) zero) ->

      mod_to_divisible' r

    | T.Node (s_eq, [l; r])
      when s_eq == Symbol.s_eq && r == (mk_num_of_int 0) ->

      mod_to_divisible' l

    (* Keep other terms unchanged *)
    | _ -> term
*)

(*
(* Convert (divisble n t) to (= 0 (mod t n)) *)
let divisible_to_mod term = 

  match T.node_of_t term with
    
    (* Term is a unary function application *)
    | T.Node (s_divisble, [t]) -> 

      (* Symbol is a divisibility symbol?  *)
      (match Symbol.node_of_symbol s_divisble with

        (* Convert to (= (mod t n) 0) *)
        | `DIVISIBLE n -> mk_eq [mk_mod t (mk_num n); mk_num_of_int 0]

        (* Keep other terms unchanged *)
        | _ -> term)

    (* Keep other terms unchanged *)
    | _ -> term 
*)

let mod_to_divisible _ = assert false 

let divisible_to_mod _ = assert false 

(* Convert negative numerals and decimals to negative terms *)
let nums_to_pos_nums term = match T.node_of_t term with 

  | T.Leaf s -> 

    (match Symbol.node_of_symbol s with 

      (* Negative numeral *)
      | `NUMERAL n when Numeral.(n < zero) ->
        mk_minus [mk_num Numeral.(abs n)]
        
      (* Negative decimal *)
      | `DECIMAL n when Decimal.(n < zero) -> 
        mk_minus [mk_dec Decimal.(abs n)]

     (* Return other terms unchanged *)
      | _ -> term)

  (* Return other terms unchanged *)
  | _ -> term 


(* Add to offset of state variable instances *)
let bump_state i term = 

  (* Bump offset of state variables *)
  T.map
    (function _ -> function 
       | t when T.is_free_var t -> 
         mk_var 
           (let v = T.free_var_of_t t in
            Var.bump_offset_of_state_var_instance i v)
       | _ as t -> t)
    term


(* Apply function to term for instants 0..k *)
let rec bump_and_apply_k f k term =

  let rec loop lbound ubound =
    if Numeral.(lbound > ubound) then ()
    else (
      bump_state lbound term |> f ;
      loop Numeral.(succ lbound) ubound
    )
  in

  if Numeral.(k >= zero)
  then loop Numeral.zero k
  else loop k Numeral.zero


(* Return all state variables in term *)
let state_vars_of_term term  = 

  eval_t
    (function 
      | T.Var v -> 
        (function 
          | [] -> 
            StateVar.StateVarSet.singleton 
              (Var.state_var_of_state_var_instance v)
          | _ -> assert false)
      | T.Const _ -> 
        (function [] -> StateVar.StateVarSet.empty | _ -> assert false)
      | T.App _ -> 
        List.fold_left 
          StateVar.StateVarSet.union 
          StateVar.StateVarSet.empty
      | T.Attr (t, _) -> 
        (function [s] -> s | _ -> assert false))
    term


(* Return all variables in term *)
let vars_of_term term = 

  (* Collect all variables in a set *)
  let var_set = 
    eval_t
      (function 
        | T.Var v -> 
          (function [] -> Var.VarSet.singleton v | _ -> assert false)
        | T.Const _ -> 
          (function [] -> Var.VarSet.empty | _ -> assert false)
        | T.App _ -> List.fold_left Var.VarSet.union Var.VarSet.empty
        | T.Attr (t, _) -> 
          (function [s] -> s | _ -> assert false))
      term
  in

  (* Return elements of a set as list *)
  var_set
 

(* Return set of state variables at given offsets in term *)
let state_vars_at_offset_of_term i term = 

  (* Collect all variables in a set *)
  eval_t
    (function 
      | T.Var v 
        when 
          Var.is_state_var_instance v &&
          Numeral.(Var.offset_of_state_var_instance v = i) -> 
        (function 
          | [] -> 
            StateVar.StateVarSet.singleton
              (Var.state_var_of_state_var_instance v)
          | _ -> assert false)
      | T.Var _ 
      | T.Const _ -> 
        (function [] -> StateVar.StateVarSet.empty | _ -> assert false)
      | T.App _ -> 
        List.fold_left StateVar.StateVarSet.union StateVar.StateVarSet.empty
      | T.Attr (t, _) -> 
        (function [s] -> s | _ -> assert false))
    term


(* Return set of state variables at given offsets in term *)
let vars_at_offset_of_term i term = 

  (* Collect all variables in a set *)
  eval_t
    (function 
      | T.Var v 
        when 
          Var.is_state_var_instance v &&
          Numeral.(Var.offset_of_state_var_instance v = i) -> 
        (function 
          | [] -> Var.VarSet.singleton v
          | _ -> assert false)
      | T.Var _ 
      | T.Const _ -> 
        (function [] -> Var.VarSet.empty | _ -> assert false)
      | T.App _ -> 
        List.fold_left Var.VarSet.union Var.VarSet.empty
      | T.Attr (t, _) -> 
        (function [s] -> s | _ -> assert false))
    term


(* Return minimal and maximal offsets of state variable instances in term *)
let rec var_offsets_of_term expr = 
  
  let max_none e1 e2 = match e1, e2 with 
    | None, None -> None 
    | None, Some e 
    | Some e, None -> Some e
    | Some e1, Some e2 -> Some Numeral.(max e1 e2)
  in
      
  let min_none e1 e2 = match e1, e2 with 
    | None, None -> None 
    | None, Some e 
    | Some e, None -> Some e
    | Some e1, Some e2 -> Some Numeral.(min e1 e2)
  in
      
  let min_max_none (l1, u1) (l2, u2) = 
    Numeral.(min_none l1 l2, max_none u1 u2) 
  in

  eval_t 
    (function 
      | T.Var v when Var.is_state_var_instance v -> 
        (function 
          | [] -> 
            let o = Var.offset_of_state_var_instance v in
            (Some o, Some o)
          | _ -> assert false)

      | T.Const _
      | T.Var _ -> 
        (function [] -> (None, None) | _ -> assert false)

      | T.App _ -> 
        (function l -> List.fold_left min_max_none (None, None) l)

      | T.Attr _ -> (function [v] -> v | _ -> assert false))
    expr


(* Infix notation for constructors *)
module Abbrev = 
struct

  let ( ?%@ ) i = mk_num_of_int i

  let ( !@ ) t = mk_not t

  let ( =>@ ) a b = mk_implies [a; b]

  let ( &@ ) a b = mk_and [a; b]

  let ( |@ ) a b = mk_or [a; b]

  let ( =@ ) a b = mk_eq [a; b]

  let ( ~@ ) a = mk_minus [a]

  let ( -@ ) a b = mk_minus [a; b]

  let ( +@ ) a b = mk_plus [a; b]

  let ( *@ ) a b = mk_times [a; b]

  let ( //@ ) a b = mk_div [a; b]

  let ( /%@ ) a b = mk_div [a; b]

  let ( <=@ ) a b = mk_leq [a; b]

  let ( <@ ) a b = mk_lt [a; b]

  let ( >=@ ) a b = mk_geq [a; b]

  let ( >@ ) a b = mk_gt [a; b]

end





(* 
   Local Variables:
   compile-command: "make -C .. -k"
   tuareg-interactive-program: "./kind2.top -I ./_build -I ./_build/SExpr"
   indent-tabs-mode: nil
   End: 
*)


      
