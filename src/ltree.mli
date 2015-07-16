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

(** Abstract terms with let bindings and quantifiers

    An abstract term extends a basic first-order term structure with
    let bindings and existential and universal quantifiers. 

    You should generally use the functions provided in the {!Term}
    module. The functions provided by this module are more elementary
    and less safe, and in particular allow constructing ill-typed
    terms. See {!Ltree.Make} for the functions that can be accessed as
    {!Term.T}.

    {1 Implementation Notes} 

    {2 Functorial Interface} 

    The abstract term is parametrized by the four types of symbols,
    free variables, sorts and attributes, which are paramters to the
    {!Make} functor. Physical equality [(==)] on the types
    {!BaseTypes.symbol} {!BaseTypes.var}, {!BaseTypes.sort}, and also
    {!BaseTypes.attr} must imply structural equality [(=)]. This may
    be achieved by using the {!Hashcons} module. Note that it is also
    required to be able to compare attributes with physical equality.

    Values of types {!S.t} and {!S.lambda} are private hash-consed
    phantom types. That means they can only be constructed with their
    respective constructors and not outside the module to preserve
    invariants. Structural equality and physical equality are
    equivalent, thus physical equality [(==)] can always be used in
    place of structural equality [(=)]. The types have a parameter
    ['a] that is instantiated either to the type {!S.safe} or
    {!S.unsafe}, indicating if the term is a proper term.

    {2 deBruijn Indexes} 
    
    We use deBruijn indexes in reverse to do nameless abstraction for
    bound variables. The variable with index [i] is bound to the
    [i]-th lambda abstraction counted upwards the parse tree from the
    occurrence of the variable:

    {[ \x. \y. (f x y) ]}

    [x]'s index is 2, [y]'s index is 1.

    This means that although two bound variables may have the
    different indexes, they can be bound to the same lambda
    abstraction. The advantage of this representation is better
    sharing, because labmda abstracting a term does not change the
    indexes of the bound variables in the term.

    To give examples, let us omit the names of the bound variables and
    write a bound variable as [xn], where [n] is its deBruijn
    index. Also note that these examplese are to illustrate the
    indexes only, and lambda abstractions do not occur in this pure
    form in the terms of this module.

    In

    {[ \. (f x1 \. (g x2 a)) ]}

    both [x1] and [x2] are bound to the outermost lambda. Abstracting
    the constant [a] to a fresh bound variable results in

    {[ \. (\. (f x1 \. (g x2 x3))) ]}

    where the indexes of [x1] and [x2] are unchanged and the two terms
    share as many subterms as possible.

    {1 Old Notes}

    A basic term is 

    - a constant {!Leaf}
    - a 



    A basic term is a tree structure, where a term is either a leaf
    containing a symbol or a variable. A term can also be a node
    containing a symbol with one or more subterms. All symbols are
    variadic and arity constraints are not checked or enforced in this
    module.

    For let bindings and quantifiers we add typed lambda abstractions
    and distinguish between free and bound variables. A typed lambda
    abstraction is a term where one or more variables are bound. We do
    nameless abstraction with de Bruijn indexes, hence a bound
    variables is just its index, whereas free variables are values of
    the type of free variables.

    A let binding is a lambda abstraction of [n] bound variables
    together with [n] terms that are to be substituted for the bound
    variables. Quantifiers are just lambda abstractions. 

    In order to maintain invariants about de Bruijn indexes, the type
    of an abstract syntax term is private to this module and terms
    must be created with the appropriate constructors.

    In addition, there is a type of flat terms, where the topmost let
    binding has been evaluated. An abstract syntax term can be
    converted to a flat term with the {!S.destruct} function, which
    distributes let bindings over nodes and ensures that the top
    symbols of the term is a node, a leaf or a variable. Subterms of a
    nodes are abstract syntax terms with binders and {!S.destruct} can
    be repeatedly applied to these subterms.

    Tail-recursive fold and map functions are provided. The
    {!S.eval_t} function presents the subterms bottom-up and
    right-to-left to the folding function and lazily evaluates all let
    bindings. It fails when the term contains quantifiers. The
    {!S.map} function presents all subterms to the function, again
    bottom-up and right-to-left, let bindings are not unfolded. Hence,
    not every subterm is a proper abstract syntax term and the mapping
    function is given the number of let binding the subterm is under
    as an argument.

    @author Christoph Sticksel *)


(** Input signature for functor *)
module type BaseTypes =
sig

  (** Symbol *)
  type symbol
  
  (** Variable *)
  type var
    
  (** Sort *)
  type sort

  (** Attribute *)
  type attr

  (** Hash value of a symbol *)
  val hash_of_symbol : symbol -> int

  (** Hash value of a variable *)
  val hash_of_var : var -> int

  (** Hash value of a sort *)
  val hash_of_sort : sort -> int
    
  (** Hash value of an attribute *)
  val hash_of_attr : attr -> int
    
  (** Return the sort of a variable *)
  val sort_of_var : var -> sort

  (** Import a symbol created in another process and restore physical
      equality *)
  val import_symbol : symbol -> symbol

  (** Import a variable created in another process and restore
      physical equality *)
  val import_var : var -> var

  (** Import a sort created in another process and restore physical
      equality *)
  val import_sort : sort -> sort

  (** Import an attribute created in another process and restore
      physical equality *)
  val import_attr : attr -> attr

  (** Pretty-print a nameless bound variable *)
  val pp_print_bound_var : Format.formatter -> int -> unit
    
  (** Pretty-print a symbol *)
  val pp_print_symbol : Format.formatter -> symbol -> unit

  (** Pretty-print a variable *)
  val pp_print_var : Format.formatter -> var -> unit

  (** Pretty-print a sort *)
  val pp_print_sort : Format.formatter -> sort -> unit

  (** Pretty-print an attribute *)
  val pp_print_attr : Format.formatter -> attr -> unit

end

(** Output signature of functor *)
module type S =
sig

  (** {1 Type Definitions} *)

  (** Symbol *)
  type symbol

  (** Variable *)
  type var

  (** Sort *)
  type sort

  (** Attribute *)
  type attr

  (** Types to instantiate the phantom type paramter

      A term is safe if no bound variable has an index greater than
      the number of lambdas it is under. This may or may not hold for
      an unsafe term.*)
  type safe
  type unsafe

  (** Lambda abstraction over symbols, variables and sort of the types
      given. Values of the type cannot be constructed outside this
      module in order to maintain invariants about the data type. *)
  type lambda_node = private L of sort list * t_t

  (** Hashconsed lambda abstraction *)
  and lambda_t = private (lambda_node, unit) Hashcons.hash_consed

  (** Lambda abstraction with a phantom type *)
  and 'a lambda = private lambda_t

  (** Term over symbols, variables and sort of the types given. Values
      of the type cannot be constructed outside this module in order
      to maintain invariants about the data type. Use the respective
      constructors {!mk_var}, {!mk_const}, {!mk_app}, {!mk_let},
      {!mk_exists} and {!mk_forall}. *)
  and t_node = private
    | FreeVar of var
    | BoundVar of int
    | App of symbol * t_t list
    | Let of lambda_t * t_t list
    | Exists of lambda_t
    | Forall of lambda_t
    | Attr of t_t * attr

  (** Properties of a term *)
  and t_prop = private { bound_vars : int list } 

  (** Hashconsed term *)
  and t_t = private (t_node, t_prop) Hashcons.hash_consed

  (** Term with a phantom type *)
  and 'a t = private t_t 

  (** Enviroment containing bindings for variables to destruct an
      unsafe term *)
  type 'a env

  (** Term over symbols, variables and sort of the types given where
      the topmost symbol is not a binder

  *)
  type 'a flat = private 
    | Var of var
    | App of symbol * 'a t list
    | Attr of 'a t * attr
    | Exists of 'a lambda
    | Forall of 'a lambda
        
  (** {1 Predicates} *)

  (** Total order on terms *)
  val compare : 'a t -> 'a t -> int

  (** Equality on terms *)
  val equal : 'a t -> 'a t -> bool

  (** Hash function on terms *)
  val hash : 'a t -> int

  (** Unique identifier for term *)
  val tag : 'a t -> int

  (** {1 Constructors} *)

  (** Construct a lambda abstraction binding the given free variables

      Generates a safe lambda if and only if the term is safe. *)
  val mk_lambda : var list -> 'a t -> 'a lambda

  (** Beta-evaluate a lambda abstraction by adding a let binding
      around the lambda, instantiating each bound variables to a term

      The list of terms must be of the same length as the list of
      variables in the lambda, otherwise the exception
      [Invalid_argument] is raised.

      Generates a safe term only if the lambda is safe, and all values
      are safe. Does not allow mixing safe and unsafe terms as
      arguments. Does not allow evaluating a safe term with unsafe
      values, or an unsafe term with safe values. *)
  val eval_lambda : 'a lambda -> 'a t list -> 'a t

  (** Construct a term of a free variable

      Always generates a safe term, because there are no bound
      variables in a leaf. *)
  val mk_var : var -> 'a t

  (** Construct a term of a constant symbol

      Always generates a safe term, because there are no bound
      variables in a leaf. The arity of the symbol is not checked. *)
  val mk_const : symbol -> 'a t

  (** Apply the function symbol to the arguments 

      Generates a safe term if and only if the arguments are safe
      terms. Does not allow mixing safe and unsafe terms as
      arguments. Must convert all safe terms to unsafe ones before in
      those cases. *)
  val mk_app : symbol -> 'a t list -> 'a t

  (** Bind free variables to terms 

      Generates a safe term if and only if all assignments are safe
      terms. 

      Does not allow mixing safe and unsafe terms as arguments. Does
      not allow binding variables in a safe term to unsafe values, or
      variables in an unsafe term to safe values. Must convert all
      safe terms to unsafe ones before in those cases. *)
  val mk_let : (var * 'a t) list -> 'a t -> 'a t

  (** Bind free variables to terms and reduce the binding to contain
      only variables that occur in the term

      Does not allow mixing safe and unsafe terms as arguments. Does not
      allow binding variables in a safe term to unsafe values, or
      variables in an unsafe term to safe values. Must convert all
      safe terms to unsafe ones before in those cases. *)
  val mk_let_elim : (var * 'a t) list -> 'a t -> 'a t

  (** Existentially quantificaty free variables 

      Generates a safe term if and only if the term quantified over is
      safe. *)
  val mk_exists : var list -> 'a t -> 'a t

  (** Existentially quantificaty free variables 

      Generates a safe term if and only if the term quantified over is
      safe. *)
  val mk_forall : var list -> 'a t -> 'a t

  (** Annotate a term

      Generates a safe term if and only if the term annotated is
      safe. *)
  val mk_attr : 'a t -> attr -> 'a t

  (** {1 Predicates and Accessors} *)

  (** Return [true] if the term is a free variable *)
  val is_free_var : 'a t -> bool

  (** Return the variable of a free variable term *)
  val free_var_of_t : 'a t -> var

  (** Return [true] if the term is a bound variable *)
  val is_bound_var : 'a t -> bool

  (** Return [true] if the term is a leaf symbol *)
  val is_leaf : 'a t -> bool

  (** Return the symbol of a leaf term *)
  val leaf_of_t : 'a t -> symbol

  (** Return [true] if the term is a function application *)
  val is_app : 'a t -> bool

  (** Return the symbol of a function application *)
  val app_symbol_of_t : 'a t -> symbol

  (** Return the arguments of a function application *)
  val app_args_of_t : 'a t -> 'a t list

  (** Return [true] if the term is a let binding *)
  val is_let : 'a t -> bool

  (** Return [true] if the term is an existential quantifier *)
  val is_exists : 'a t -> bool

  (** Return the lambda abstraction of an existential quantifier *)
  val lambda_of_exists : 'a t -> 'a lambda

  (** Return true if the term is a universal quantifier *)
  val is_forall : 'a t -> bool 

  (** Return the lambda abstraction of a universal quantifier *)
  val lambda_of_forall : 'a t -> 'a lambda

  (** Return true if the term is a named term *)
  val is_attr : 'a t -> bool

  (** Return the term of a named term *)
  val attr_t_of_t : 'a t -> 'a t

  (** Return the name of a named term *)
  val attr_of_t : 'a t -> attr

  (** Return the node of a hashconsed term *)
  val node_of_t : 'a t -> t_node

  (** Return the node of a hashconsed lamda abstraction *)
  val node_of_lambda : 'a lambda -> lambda_node

  (** Return the sorts of a hashconsed lambda abstraction *)
  val sorts_of_lambda : 'a lambda -> sort list

  (** Return the unique tag of a hashconsed term *)
  val tag_of_t : 'a t -> int

  (** {2 Iterators} *)

  (** Evaluate the term bottom-up and right-to-left. The function is
      evaluated at each subterm that is a function application and at
      each constant of the term.

      If there are two identical subterms, they will be evaluated
      twice. No caching is performed. An exception are let bindings:
      the function is evaluated once for each term assigned to a bound
      variable at the first occurrence of the bound variable
      only. That means, each at most once for each term assigned to a
      bound variable.
      
      If the term to be evaluated contains quantifiers the exception
      [Invalid_argument] is raised.

      The first argument on each evaluation is the flat representation
      of the term being evaluated, the second argument is the result
      computed for the subterms of the term. *)
  val eval : (safe flat -> 'a list -> 'a) -> safe t -> 'a

  (** Tail-recursive bottom-up right-to-left map on the term. The
      function is evaluated at each subterm, including let bindings
      and quantifiers. No eta-conversion is performed, that is,
      assignments to bound variables are evaluated regardless whether
      the bound variable occurs.

      Not every subterm is a proper term, since an occurrence of a
      bound variable can be refer to a binder outside the subterm,
      hence the term is unsafe. However, we can give an environment to
      evaluate the term in. The first two arguments can be passed to
      {!destruct_unsafe} to obtain a safe term for use in pattern
      matching.

      If the function returns [None], the subterm is unchanged,
      otherwise the subterm is replaced by the given term. If the term
      to substitute contains a bound variable that would make the
      resulting term non-proper, the exception [Invalid_argument] is
      raised. *)
  val map : (safe env -> unsafe t -> 'a t option) -> 'b t -> 'b t

(*
  val map_top : (unsafe t -> 'a t option) -> 'b t -> 'b t
*)

  (** Return the top symbol of a term along with its subterms

      If the top symbol of a term is a let binding, the binding is
      distributed over the subterms. *)
  val destruct : safe t -> safe flat

  (** Return the top symbol of a term along with its subterms

      If the top symbol of a term is a let binding, the binding is
      distributed over the subterms. *)
  val destruct_unsafe : 'a env -> 'b t -> 'a flat

  (** Convert the flattened representation back into a term *)
  val construct : 'a flat -> 'a t


  (** Return a safe term if the unsafe term if all bound variables
      are bound within the term
      
      Fail with [Invalid_argument] otherwise *)
  val safe_of_unsafe : 'a t -> safe t

  (** Return an unsafe term of a safe term

      The term is not changed, only its type, and it can be converted
      back immediately with {!safe_of_unsafe}. *)
  val unsafe_of_safe : 'a t -> unsafe t

  (** Import a term into the hashcons table by rebuilding it bottom
      up *)
  val import : 'a t -> 'a t

  (** Import a lambda abstraction into the hashcons table by
      rebuilding it bottom up *)
  val import_lambda : 'a lambda -> 'a lambda

  (** Pretty-print a term *)
  val pp_print_term : ?db:int -> Format.formatter -> 'a t -> unit

  (** Pretty-print a term *)
  val pp_print_term : ?db:int -> Format.formatter -> 'a t -> unit

  val pp_print_lambda_w : (?arity:int -> Format.formatter -> symbol -> unit) ->
    ?db:int -> Format.formatter -> 'a lambda -> unit

  val pp_print_term_w : (?arity:int -> Format.formatter -> symbol -> unit) ->
    ?db:int -> Format.formatter -> 'a t -> unit

  (** Pretty-print a term *)
  val print_term : ?db:int -> 'a t -> unit

  (** Pretty-print a lambda abstraction *)
  val pp_print_lambda : ?db:int -> Format.formatter -> 'a lambda -> unit

  (** Pretty-print a lambda abstraction *)
  val print_lambda : ?db:int -> 'a lambda -> unit

  val stats : unit -> int * int * int * int * int * int
  
end

(** Functor to create a higher-order abstract syntax tree module *)
module Make (T : BaseTypes) :
  (S with type symbol = T.symbol 
      and type var = T.var 
      and type sort = T.sort 
      and type attr = T.attr) 


(* 
   Local Variables:
   compile-command: "make -C .. -k"
   tuareg-interactive-program: "./kind2.top -I ./_build -I ./_build/SExpr"
   indent-tabs-mode: nil
   End: 
*)
