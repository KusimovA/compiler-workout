open GT
open Syntax      
 
       
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
let rec evalInsn (stack, (state, input, output)) prg = match prg with
	| BINOP op -> (match stack with 
					| y::x::tail -> ((Expr.signToOp op x y)::tail, (state, input, output))
					| _ -> failwith "BINOP. Not enough values on the stack")
	| CONST n -> (n::stack, (state, input, output))
	| READ -> (match input with
				| z::tail -> (z::stack, (state, tail, output))
				| _ -> failwith "READ. Input is empty")
	| WRITE -> (match stack with
				| z::tail -> (tail, (state, input, output @ [z]))
				| _ -> failwith "WRITE. Stack is empty")
	| LD x -> ((state x)::stack, (state, input, output))
	| ST x -> (match stack with
				| z::tail -> (tail, (Expr.update x z state, input, output))
				| _ -> failwith "ST. Stack is empty")
	
let eval config prg = List.fold_left evalInsn config prg

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

let rec compileExpr e = match e with
	| Expr.Const n -> [CONST n]
	| Expr.Var x -> [LD x]
	| Expr.Binop (op, a, b) -> compileExpr a @ compileExpr b @ [BINOP op]
	
let rec compile stmt = match stmt with
	| Stmt.Read x -> [READ; ST x]
	| Stmt.Write e -> compileExpr e @ [WRITE]
	| Stmt.Assign (x, e) -> compileExpr e @ [ST x]
	| Stmt.Seq (op1, op2) -> compile op1 @ compile op2
