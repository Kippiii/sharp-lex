/// ===========================================
/// Main module for processing lexical specifications and generating token streams
/// ===========================================
module Lexer

// Create DFA from specification
let makeLexer (spec: Specification.Specification) : DFA.DFA = todo

// Create lexer from file
let fMakeLexer (spec_file: string) (out_file: string) : unit = todo

// Run the lexer on an input
let runLexer (dfa: Automata.DFA) (input: string) : Token.TokenStream = todo

// Run the lexer from a file
let fRunLexer (dfa_file: string) (input: string) : unit = todo