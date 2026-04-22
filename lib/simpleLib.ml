module AST = SimpleAST
module Lexer = SimpleLexer
module Parser = SimpleParser
module SymbInterpreter = SimpleSymbInterpreter

let parse src =
  let lexbuf = Lexing.from_channel (open_in src) in
  Parser.prog Lexer.read lexbuf

let symb_exec stmt ~mode =
  SymbInterpreter.Symex.run ~mode
    (SymbInterpreter.symb_eval_stmt SymbInterpreter.Map.empty stmt)
