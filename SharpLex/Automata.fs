/// ===========================================
/// Module for NFAs and DFAs
/// ===========================================
module Automata

// Type for regular expressions
type Regex = todo

// Converts a string to a regular expression
let makeRegex (str : string) : Regex = todo

// Type for a modified NFA
type ModNFA = todo

// Generate a modified NFA from regex
let fromRegex (re: Regex) : ModNFA = todo

// Merge two modified NFAs
let merge (nfa1: ModNFA) (nfa2: ModNFA) : ModNFA = todo

// Create a DFA from a modified NFA
let toDFA (nfa: ModNFA) : DFA = todo

// The type for a DFA
type DFA = todo

// Create a DFA
let makeDFA (states: int) (start: int) (final: (int * string) list) (error: int) (transition: int[][]) : DFA = todo

// Run a DFA
let run (dfa: DFA) (input: string) : Token.TokenStream = todo

// Convert a DFA to a file
let toFile (dfa: DFA) : string = todo

// Generate a DFA from a file
let fromFile (input: string) : DFA = todo