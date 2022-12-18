open Front
open Front.Ast_print

let dpos = Lexing.dummy_pos

let ast1 = Ast.BinOp (dpos, Ast.Int (dpos, 1), Ast.Op_Plus, Ast.Int (dpos, 2))
let ast2 = Ast.BinOp (dpos, Ast.Int (dpos, 3), Ast.Op_Plus, ast1)
let () = print_endline (print_expr create_printer ast2)