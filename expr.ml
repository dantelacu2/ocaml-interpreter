(* 
                         CS 51 Final Project
                        MiniML -- Expressions
*)

(*......................................................................
  Abstract syntax of MiniML expressions 
 *)

type unop =
  | Negate
;;
    
type binop =
  | Plus
  | Minus
  | Times
  | Equals
  | LessThan
;;

type varid = string ;;
  
type expr =
  | Var of varid                         (* variables *)
  | Num of int                           (* integers *)
  | Bool of bool                         (* booleans *)
  | Unop of unop * expr                  (* unary operators *)
  | Binop of binop * expr * expr         (* binary operators *)
  | Conditional of expr * expr * expr    (* if then else *)
  | Fun of varid * expr                  (* function definitions *)
  | Let of varid * expr * expr           (* local naming *)
  | Letrec of varid * expr * expr        (* recursive local naming *)
  | Raise                                (* exceptions *)
  | Unassigned                           (* (temporarily) unassigned *)
  | App of expr * expr                   (* function applications *)
;;
  
(*......................................................................
  Manipulation of variable names (varids)
 *)

(* varidset -- Sets of varids *)
module SS = Set.Make (struct
                       type t = varid
                       let compare = String.compare
                     end ) ;;

type varidset = SS.t ;;

(* same_vars :  varidset -> varidset -> bool
   Test to see if two sets of variables have the same elements (for
   testing purposes) *)
let same_vars : varidset -> varidset -> bool =
  SS.equal;;

(* vars_of_list : string list -> varidset
   Generate a set of variable names from a list of strings (for
   testing purposes) *)
let vars_of_list : string list -> varidset =
  SS.of_list ;; 
  
(* free_vars : expr -> varidset
   Return a set of the variable names that are free in expression
   exp *)
let rec exp_to_varid_strings (exp: expr) : string list =
  match exp with 
  | Var x -> [x]
  | Unop (_, y) -> exp_to_varid_strings y
  | Binop (_, y, z) -> List.append (exp_to_varid_strings y) (exp_to_varid_strings z)
  | Conditional (x, y, z) -> List.append (List.append (exp_to_varid_strings x) (exp_to_varid_strings y)) (exp_to_varid_strings z)
  | Fun (_, y) -> (exp_to_varid_strings y)
  | Let (_, y, z) -> List.append (exp_to_varid_strings y) (exp_to_varid_strings z)
  | Letrec (_, y, z) -> List.append (exp_to_varid_strings y) (exp_to_varid_strings z)
  | App (x, y) -> List.append (exp_to_varid_strings x) (exp_to_varid_strings y)
  | _ -> [] ;;

let rec free_vars (exp : expr) : varidset =
  vars_of_list (exp_to_varid_strings exp) ;;

(* new_varname : unit -> varid
   Return a fresh variable, constructed with a running counter a la
   gensym. Assumes no variable names use the prefix "var". (Otherwise,
   they might accidentally be the same as a generated variable name.) *)
let counter = ref 0 ;;
let new_varname () : varid =
  counter := !counter + 1;
  "var" ^ (string_of_int !counter) ;;

(*......................................................................
  Substitution 

  Substitution of expressions for free occurrences of variables is the
  cornerstone of the substitution model for functional programming
  semantics.
 *)

(* subst : varid -> expr -> expr -> expr
   Substitute repl for free occurrences of var_name in exp 
   check for fun, let, letrec if the bound variable is already 
   a freevariable change the name of the bound variable
   *)
let rec subst (var_name : varid) (repl : expr) (exp : expr) : expr =
  match exp with
  | Var x -> if (x = var_name) then repl else Var x
  | Unop (x, y) -> Unop (x, (subst var_name repl y))
  | Binop (x, y, z) -> Binop (x, (subst var_name repl y), (subst var_name repl z))
  | Conditional (x, y, z) -> Conditional ((subst var_name repl x), (subst var_name repl y), (subst var_name repl z))

  | Fun (x, y) -> 
    if (var_name = x) 
    then exp
    else if not (SS.mem x (free_vars repl))
    then Fun (x, (subst var_name repl y))
    else let new_name = new_varname() in
    Fun (new_name, subst var_name repl (subst x (Var(new_name)) y))

                  (*Do I check if the bound variable isn't in exp1 and exp2?*)
  | Let (x, y, z) ->
    if (var_name = x)
    then Let (x, (subst var_name repl y), z)
    else if not (SS.mem x (free_vars repl))
    then Let (x, (subst var_name repl y), (subst var_name repl z))
    else 
    let new_name = new_varname() in
    Let (new_name, subst var_name repl y, subst var_name repl (subst x (Var(new_name)) z))

  | Letrec (x, y, z) -> 
    if (var_name = x)
    then Letrec (x, (subst var_name repl y), z)
    else if not (SS.mem x (free_vars repl))
    then Letrec (x, (subst var_name repl y), (subst var_name repl z))
    else 
    let new_name = new_varname() in
    Letrec (new_name, subst var_name repl y, subst var_name repl (subst x (Var(new_name)) z))

  | App (x, y) -> App ((subst var_name repl x), (subst var_name repl y))

    (*Can i do this?*)
  | x -> x

  ;;

(*......................................................................
  String representations of expressions
 *)
   
let binop_to_concrete_string (bin: binop) : string =
  match bin with
  | Plus -> "+"
  | Minus -> "-"
  | Times -> "*"
  | Equals -> "="
  | LessThan -> "<"
  ;;
(* exp_to_concrete_string : expr -> string
   Returns a concrete syntax string representation of the expr *)
let rec exp_to_concrete_string (exp : expr) : string =
  match exp with
  | Var x -> x
  | Num x -> string_of_int x
  | Bool x -> string_of_bool x
  | Unop (_, y) -> "-" ^ (exp_to_concrete_string y)
  | Binop (x, y, z) -> (exp_to_concrete_string y) ^ 
                       (binop_to_concrete_string x) ^ 
                       (exp_to_concrete_string z)
  | Conditional (x, y, z) -> "if (" ^ (exp_to_concrete_string x) ^ ") then " 
                                    ^ (exp_to_concrete_string y) ^ "else " 
                                    ^ (exp_to_concrete_string z)
  | Fun (x, y) -> "function (" ^ x ^ ") -> " 
                               ^ (exp_to_concrete_string y) 
  | Let (x, y, z) -> "let " ^ x ^ " = " ^ (exp_to_concrete_string y) ^ " in " ^ (exp_to_concrete_string z) 
  | Letrec (x, y, z) -> "let rec " ^ x ^ " = " ^ (exp_to_concrete_string y) ^ " in " ^ (exp_to_concrete_string z) 
  | Raise -> raise Exit
  | Unassigned -> "Unassigned"
  | App (x, y) -> (exp_to_concrete_string x) ^ " applied to " ^ (exp_to_concrete_string y) ;;

(* exp_to_abstract_string : expr -> string
   Returns a string representation of the abstract syntax of the expr *)

let binop_to_abstract_string (bin: binop) : string =
  match bin with
  | Plus -> "Plus"
  | Minus -> "Minus"
  | Times -> "Times"
  | Equals -> "Equls"
  | LessThan -> "LessThan"
  ;;

let rec exp_to_abstract_string (exp : expr) : string =
  match exp with
  | Var x -> "Var(" ^ x ^ ")"
  | Num x -> "Num(" ^ (string_of_int x) ^ ")"
  | Bool x -> "Bool(" ^ (string_of_bool x) ^ ")"
  | Unop (_, y) -> "Unop(" ^ "Negate, " ^ (exp_to_abstract_string y) ^ ")"
  | Binop (x, y, z) -> "Binop(" ^ (binop_to_abstract_string x) ^ ", "
                                ^ (exp_to_abstract_string y) ^ ", "
                                ^ (exp_to_abstract_string z) ^ ")"
  | Conditional (x, y, z) -> "Conditional(" ^ (exp_to_abstract_string x)  ^ ", " 
                                            ^ (exp_to_abstract_string y) ^ ", " 
                                            ^ (exp_to_abstract_string z) ^ ")"
  | Fun (x, y) -> "Fun(" ^ x ^ ", " ^ (exp_to_abstract_string y) ^ ")"
  | Let (x, y, z) -> "Let(" ^ x ^ ", " ^ (exp_to_abstract_string y) ^ ", "
                                       ^ (exp_to_abstract_string z) ^ ")"
  | Letrec (x, y, z) -> "Letrec(" ^ x ^ ", " ^ (exp_to_abstract_string y) ^ ", "
                                       ^ (exp_to_abstract_string z) ^ ")"
  | Raise -> raise Exit
  | Unassigned -> "Unassigned"
  | App (x, y) -> "App(" ^ (exp_to_abstract_string x) ^ ", " ^ (exp_to_abstract_string y) ^ ")" ;;
