(* Opening a library for generic programming (https://github.com/dboulytchev/GT).
   The library provides "@type ..." syntax extension and plugins like show, etc.
*)
open GT
open Ostap

(* Opening a library for combinator-based syntax analysis *)
open Ostap.Combinators
       
(* Simple expressions: syntax and semantics *)
module Expr =
  struct
    
    (* The type for expressions. Note, in regular OCaml there is no "@type..." 
       notation, it came from GT. 
    *)
    @type t =
    (* integer constant *) | Const of int
    (* variable         *) | Var   of string
    (* binary operator  *) | Binop of string * t * t with show

    (* Available binary operators:
        !!                   --- disjunction
        &&                   --- conjunction
        ==, !=, <=, <, >=, > --- comparisons
        +, -                 --- addition, subtraction
        *, /, %              --- multiplication, division, reminder
    *)
                                                            
    (* State: a partial map from variables to integer values. *)
    type state = string -> int 

    (* Empty state: maps every variable into nothing. *)
    let empty = fun x -> failwith (Printf.sprintf "Undefined variable %s" x)

    (* Update: non-destructively "modifies" the state s by binding the variable x 
      to value v and returns the new state.
    *)
    let update x v s = fun y -> if x = y then v else s y

    (* Bin operation evaluator

          val evalOp : op -> (int -> int -> int)

    *)
    let evalOp op =
      let evalBool b = if b then 1 else 0 in
      let evalInt i = if i = 0 then false else true in
      let comp f g x y = f (g x) (g y) in
      let evalBoolOp op =
        match op with
          | "<"  -> ( < )
          | "<=" -> ( <= )
          | ">"  -> ( > )
          | ">=" -> ( >= )
          | "==" -> ( == )
          | "!=" -> ( <> )
          | "&&" -> comp ( && ) evalInt
          | "!!" -> comp ( || ) evalInt
          | _    -> failwith "Unsupported operator" in
      match op with
        | "+" -> ( + )
        | "-" -> ( - )
        | "*" -> ( * )
        | "/" -> ( / )
        | "%" -> ( mod )
        | _   -> fun x y -> evalBool @@ evalBoolOp op x y

    (* Expression evaluator

          val eval : state -> t -> int

       Takes a state and an expression, and returns the value of the expression in
       the given state.
    *)
    let rec eval s e =
      match e with
        | Var x             -> s x
        | Const c           -> c
        | Binop (op, x, y)  ->
          let ex = eval s x in
          let ey = eval s y in
          evalOp op ex ey


    let ostap_binop_map opps = List.map (fun op -> ostap ($(op)), fun x y -> Binop(op, x, y)) opps

    (* Expression parser. You can use the following terminals:

         IDENT   --- a non-empty identifier a-zA-Z[a-zA-Z0-9_]* as a string
         DECIMAL --- a decimal constant [0-9]+ as a string
   
    *)
    ostap (
      parse: 
        !(Util.expr
          (fun x -> x)
          [|
            `Lefta , ostap_binop_map ["!!"];
            `Lefta , ostap_binop_map ["&&"];
            `Nona  , ostap_binop_map ["<"; "<="; "=="; "!="; ">"; ">="];
            `Lefta , ostap_binop_map ["+"; "-"];
            `Lefta , ostap_binop_map ["*"; "/"; "%"]
          |]
          primary
        );
      primary: x:IDENT {Var x} | c:DECIMAL {Const c} | -"(" parse -")"
    )

  end
                    
(* Simple statements: syntax and sematics *)
module Stmt =
  struct

    (* The type for statements *)
    @type t =
    (* read into the variable           *) | Read   of string
    (* write the value of an expression *) | Write  of Expr.t
    (* assignment                       *) | Assign of string * Expr.t
    (* composition                      *) | Seq    of t * t with show

    (* The type of configuration: a state, an input stream, an output stream *)
    type config = Expr.state * int list * int list 

    (* Statement evaluator

          val eval : config -> t -> config

       Takes a configuration and a statement, and returns another configuration
    *)
    let rec eval c s =
      match (c, s) with
        | ((st, i, o), Assign (x, e)) ->
            let ev = Expr.eval st e in
            (Expr.update x ev st, i, o)
        | ((st, z::i, o), Read x)     -> (Expr.update x z st, i, o)
        | ((st, i, o), Write e)       ->
            let ev = Expr.eval st e in
            (st, i, o @ [ev])
        | (co, Seq (s1, s2))          ->
            let sc1 = eval co s1 in
            eval sc1 s2
        | _                           -> failwith "Unsupported configuration and statement"

    (* Statement parser *)
    ostap (
      stmt:
          x:IDENT -":=" e:!(Expr.parse)       {Assign (x,e)}
        | -"read" -"(" x:IDENT -")"           {Read x}
        | -"write" -"(" e:!(Expr.parse) -")"  {Write e};
      parse: <s::ss> : !(Util.listBy)[ostap (";")][stmt] {List.fold_left (fun x y -> Seq (x, y)) s ss}
    )
      
  end

(* The top-level definitions *)

(* The top-level syntax category is statement *)
type t = Stmt.t    

(* Top-level evaluator

     eval : t -> int list -> int list

   Takes a program and its input stream, and returns the output stream
*)
let eval p i =
  let _, _, o = Stmt.eval (Expr.empty, i, []) p in o

(* Top-level parser *)
let parse = Stmt.parse                                                     
