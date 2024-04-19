/// ===========================================
/// Main module for processing lexical specifications and generating token streams
/// ===========================================
module Lexer

open System.IO

// Create DFA from specification
let makeLexer (spec: Specification.Specification) : Automata.DFA =
    let nfa = Specification.toNFA spec
    Automata.toDFA nfa

// Create lexer from file
let fMakeLexer (spec_file: string) (out_file: string) : unit =
    let spec_str =
        try
            let inStream = File.OpenText spec_file
            let content = inStream.ReadToEnd ()
            inStream.Close()
            content
        with
            | ex -> "" // TODO
    let spec = Specification.makeSpec spec_str
    let dfa = makeLexer spec
    try
        let outStream = File.CreateText out_file
        outStream.Write (Automata.toFile dfa)
        outStream.Close ()
    with
        | ex -> "" // TODO

// Run the lexer on an input
let runLexer (dfa: Automata.DFA) (input: string) : Token.TokenStream = Automata.run

// Run the lexer from a file
let fRunLexer (dfa_file: string) (output_file : string) (input: string) : unit =
    let dfa =
        try
            let inStream = File.OpenText dfa_file
            let content = inStream.ReadToEnd ()
            inStream.Close()
            Automata.fromFile content
        with
            | ex -> "" // TODO
    let stream = runLexer dfa input
    try
        let outStream = File.CreateText output_file
        outStream.Write (Token.toFile stream)
        outStream.Close ()
    with
        | ex -> "" // TODO