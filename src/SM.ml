open GT       
open Language
       
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
type config = int list * Stmt.config

(* Stack machine interpreter

     val eval : config -> prg -> config

   Takes a configuration and a program, and returns a configuration as a result
 *)                         
let rec eval c p =
  match (c, p) with
    | ((st, co), [])                   -> (st, co)
    | ((y::x::st, co), (BINOP op)::ps) -> eval ((Language.Expr.evalOp op x y)::st, co) ps
    | ((st, co), (CONST x)::ps)        -> eval (x::st, co) ps
    | ((st, (s, z::i, o)), READ::ps)   -> eval (z::st, (s, i, o)) ps
    | ((z::st, (s, i, o)), WRITE::ps)  -> eval (st, (s, i, o @ [z])) ps
    | ((st, (s, i, o)), (LD x)::ps)    -> eval ((s x)::st, (s, i, o)) ps
    | ((z::st, (s, i, o)), (ST x)::ps) -> eval (st, (Language.Expr.update x z s, i, o)) ps
    | _                                -> failwith "Unsupported configuration and program"

(* Top-level evaluation

     val run : prg -> int list -> int list

   Takes a program, an input stream, and returns an output stream this program calculates
*)
let run p i = let (_, (_, _, o)) = eval ([], (Language.Expr.empty, i, [])) p in o

(* Stack machine compiler

     val compile : Language.Stmt.t -> prg

   Takes a program in the source language and returns an equivalent program for the
   stack machine
 *)
let rec compile s =
  let rec compile_e e =
    match e with
      | Language.Expr.Const n            -> [CONST n]
      | Language.Expr.Var x              -> [LD x]
      | Language.Expr.Binop (op, e1, e2) -> compile_e e1 @ compile_e e2 @ [BINOP op]
      | _                              -> failwith "Unsupported expression"
  in
  match s with
    | Language.Stmt.Assign (x, e) -> compile_e e @ [ST x]
    | Language.Stmt.Read x        -> [READ; ST x]
    | Language.Stmt.Write e       -> compile_e e @ [WRITE]
    | Language.Stmt.Seq (s1, s2)  -> compile s1 @ compile s2
    | _                         -> failwith "Unsupported statement"
