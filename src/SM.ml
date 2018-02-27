open GT       
       
(* The type for the stack machine instructions *)
@type insn =
(* binary operator                 *) | BINOP of string
(* put a constant on the stack     *) | CONST of int                 
(* read to stack                   *) | READ
(* write from stack                *) | WRITE
(* load a variable to the stack    *) | LD    of string
(* store a variable from the stack *) | ST    of string with show

(* The type for the stack machine program *)                                                               
type prg = insn list

(* The type for the stack machine configuration: a stack and a configuration from statement
   interpreter
 *)
type config = int list * Syntax.Stmt.config

(* Stack machine interpreter

     val eval : config -> prg -> config

   Takes a configuration and a program, and returns a configuration as a result
 *)                         
let rec eval c p =
  match (c, p) with
    | ((st, co), [])                   -> (st, co)
    | ((y::x::st, co), (BINOP op)::ps) -> eval ((Syntax.Expr.evalOp op x y)::st, co) ps
    | ((st, co), (CONST x)::ps)        -> eval (x::st, co) ps
    | ((st, (s, z::i, o)), READ::ps)   -> eval (z::st, (s, i, o)) ps
    | ((z::st, (s, i, o)), WRITE::ps)  -> eval (st, (s, i, o @ [z])) ps
    | ((st, (s, i, o)), (LD x)::ps)    -> eval ((s x)::st, (s, i, o)) ps 
    | ((z::st, (s, i, o)), (ST x)::ps) -> eval (st, (Syntax.Expr.update x z s, i, o)) ps
    | _                                -> failwith "Unsupported configuration and program"

(* Top-level evaluation

     val run : int list -> prg -> int list

   Takes an input stream, a program, and returns an output stream this program calculates
*)
let run i p = let (_, (_, _, o)) = eval ([], (Syntax.Expr.empty, i, [])) p in o

(* Stack machine compiler

     val compile : Syntax.Stmt.t -> prg

   Takes a program in the source language and returns an equivalent program for the
   stack machine
 *)

let rec compile s =
  let rec compile_e e =
    match e with
      | Syntax.Expr.Const n            -> [CONST n]
      | Syntax.Expr.Var x              -> [LD x]
      | Syntax.Expr.Binop (op, e1, e2) -> compile_e e1 @ compile_e e2 @ [BINOP op]
      | _                              -> failwith "Unsupported expression" in
  match s with
    | Syntax.Stmt.Assign (x, e) -> compile_e e @ [ST x]
    | Syntax.Stmt.Read x        -> [READ; ST x]
    | Syntax.Stmt.Write e       -> compile_e e @ [WRITE]      
    | Syntax.Stmt.Seq (s1, s2)  -> compile s1 @ compile s2
    | _                         -> failwith "Unsupported statement"
