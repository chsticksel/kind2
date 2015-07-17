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

    You should use the functions provided in the {!Term} module. The
    functions provided by this module are more elementary and not
    necessarily safe. In particular one may construct ill-typed terms,
    or even terms that are ill-formed regarding bound variables. See
    {!Ltree.Make} for the functions that can be accessed as {!Term.T}.

    {1 Usage}

    Values of types {!S.t} and {!S.lambda} are private hash-consed
    phantom types. That means they can only be constructed with their
    respective constructors in order to preserve invariants about
    their structure. Structural equality and physical equality are
    equivalent, thus physical equality [(==)] on {!S.t} and
    {!S.lambda} can always be used in place of structural equality
    [(=)].

    A term {!S.t} is a first-order structure over a signature of
    symbols {!S.symbol} and free variables {!S.var}. A constant is a
    function application with an empty list of arguments. A bound
    variable is bound to exactly one lambda abstraction {!S.lambda}. A
    lambda abstraction may bind several variables
    simultaneously. Terms contain lambda abstractions either as let
    bindings, or as universal or existential quantifiers. One can
    attach an attribute to a term, this constructs a new term that is
    different from the under the attribute.

    {2 Constructors}

    Construct a term with the functions {!S.mk_var}, {!S.mk_const},
    {!S.mk_app}, {!S.mk_let}, {!S.mk_exists}, {!S.mk_forall}, and
    {!S.mk_attr}. The constructor {!S.mk_const} is an alias for
    {!S.mk_app} with an empty list of arguments.

    There is neither type checking nor checking of arity constraints
    of symbols in this module. To enable typing in higher modules,
    every bound variable is annotated with a type {!S.sort}. Typing
    rules can then be implemented on higer levels, making use of
    typing rules for free variables {!S.var} and typing rules based on
    the signature {!S.symbol}.

    {2 Accessing Terms} 

    To distinguish the type of a term, use the predicates
    {!S.is_free_var}, {!S.is_bound_var}, {!S.is_const}, {!S.is_app},
    {!S.is_let}, {!S.is_exists}, {!S.is_forall} and {!S.is_attr}, or
    the function {!S.node_of_t} followed by a pattern matching on the
    type {!S.t_node}. There are also accessors {!S.free_var_of_t},
    {!S.const_of_t}, {!S.app_symbol_of_t}, {!S.app_args_of_t},
    {!S.lambda_of_exists}, {!S.lambda_of_forall}, {!S.attr_of_t}, and
    {!S.attr_t_of_t}. The accessors will fail with [Invalid_argument]
    if applied to the wrong kind of term.

    Alternatively, use the function {!S.destruct}, which evaluates a
    term to a flat term of type {!S.flat}, where no let bindings
    exists at the top of the term. Let bindings are distributed over
    function applications, no bound variables will ever be visible at
    the top level of a flat term. Quantifiers cannot be eliminated,
    hence a flat term may contain a quantifier at the top. Values of
    type {!t.flat} are not perfectly shared, hence two values may be
    structurally equalt but not physically.

    {2 Quantifiers} 

    To instantiate a quantifier, obtain the lambda abstraction of the
    quantifier with {!S.lambda_of_exists} or {!S.lambda_of_forall},and
    use the function {!S.eval_lambda} to instantiate the bound
    variable to a particular term.

    {2 Iterators}

    Finally, there are tail-recursive iterators and evaluators for
    terms {!S.map}, {!S.map_top}, {!S.fold}, and {!S.eval}.

    The function {!S.map} constructs a new term by applying a given
    function [f] to each subterm bottom-up and right to left. Not
    every subterm is a proper term, because it may contain a bound
    variable but not its binder. Therefore, the function [f] is given
    an environment in its first argument that can be passed to
    {!S.destruct_unsafe} to obtain a flat term as with
    {!S.destruct}. The function [f] may return [None] to keep the
    subterm unchanged, or [Some t] to replace the subterm with
    [t]. The term [t] to replace the subterm must not contain bound
    variables that are not bound inside the term. If it does, the
    exception [Invalid_argument] is raised.

    The function {!S.map_top} constructs a new term by applying the
    function [f] to each subterm top-down and right to left. As with
    {!S.map} the function [f] is evaluated with an environment as
    first argument to enable {!S.destruct_unsafe}. If the function [f]
    returns [None], the subterm is unchanged and its subterms are
    considered. If the function returns [Some t], the subterm is
    replaced by [t], without further considering its subterms.

    The function {!S.eval} reduces the term to a value by
    instantiating each let binding and applying the function [f] to
    each subterm. 

    The function {!S.fold} reduces the term to a value in a similar
    way as {!S.eval} except that let bindings are not evaluated, and
    quantified terms are allowed. The function [f] is presented with
    each subterm bottom-up and right to left in the same way as
    {!S.map}, where [f] is evaluated with the list of evaluating the
    subterms in the third argument similar to {!S.eval}.

    {1 Implementation Notes} 

    The types have a parameter ['a] that is instantiated either to the
    type {!S.safe} or {!S.unsafe}, indicating if the term is a proper
    term. A term is proper if it contains the binder for every bound
    variable.

    We use de Bruijn indexes in reverse to do nameless abstraction for
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

    {[ \_. (f x1 \_. (g x2 a)) ]}

    both [x1] and [x2] are bound to the outermost lambda. Abstracting
    the constant [a] to a fresh bound variable results in

    {[ \_. (\_. (f x1 \_. (g x2 x3))) ]}

    where the indexes of [x1] and [x2] are unchanged and the two terms
    share as many subterms as possible.

    If a lambda abstraction binds several variables simultaneously,
    the first element in the list binds the first bound variable. 

    {[ \_ _. (f x1 x2) ]}

    The bound variable [x1] is bound by the leftmost variable in the
    lambda abstraction, [x2] by the rightmost.

    {2 Functorial Interface} 

    The abstract term is parametrized by the four types of symbols,
    free variables, sorts and attributes, which are paramters to the
    {!Make} functor. The four types {!BaseTypes.symbol}
    {!BaseTypes.var}, {!BaseTypes.sort}, and also {!BaseTypes.attr}
    must be perfectly shared, that is structural equality [(=)] must
    imply physical equality [(==)]. This may be achieved by using the
    {!Hashcons} module. 

    @author Christoph Sticksel *)


(** Input signature for functor {!S.Make} *)
module type BaseTypes =
sig

  (** Symbol in the signature *)
  type symbol
  
  (** Variable in the signature *)
  type var
    
  (** Type of a term *)
  type sort

  (** Attribute of a term *)
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

(** Output signature of functor {!S.Make} *)
module type S =
sig

  (** {1 Type Definitions} *)

  (** Symbol in the signature *)
  type symbol

  (** Variable in the signature*)
  type var

  (** Type of a term *)
  type sort

  (** Attribute of a term *)
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

  (** Lambda abstraction with a phantom type signaling its safeness *)
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

  (** Properties of a term

      [bounds_vars] is the sorted list of the indexes of bound
      variables in the term. This value is computed when constructing
      the term and used instead of recomputing it every time. *)
  and t_prop = private { bound_vars : int list } 

  (** Hashconsed term *)
  and t_t = private (t_node, t_prop) Hashcons.hash_consed

  (** Term with a phantom type signaling its safeness *)
  and 'a t = private t_t 

  (** Enviroment containing bindings for variables to destruct an
      unsafe term *)
  type 'a env

  (** Term over symbols, variables and sort of the types given where
      the topmost symbol is not a binder *)
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
      variables in a variable. *)
  val mk_var : var -> 'a t

  (** Construct a term of a constant symbol

      Always generates a safe term, because there are no bound
      variables in a constant. The arity of the symbol is not checked. *)
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

  (** Return [true] if the term is a constant *)
  val is_const : 'a t -> bool

  (** Return the symbol of a constant term *)
  val const_of_t : 'a t -> symbol

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

  (** {2 Iterators and Evaluators} *)

  (** Evaluate the term bottom-up and right-to-left. The function is
      evaluated at each subterm that is a function application and at
      each constant of the term. If the term to be evaluated contains
      quantifiers the exception [Invalid_argument] is raised.

      If there are two identical subterms, they will be evaluated
      twice. No caching is performed. An exception are let bindings:
      the function is evaluated once for each term assigned to a bound
      variable at the first occurrence of the bound variable
      only. That means, each at most once for each term assigned to a
      bound variable.

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
