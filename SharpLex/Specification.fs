/// ===========================================
/// Module for processing specification files
/// ===========================================
module Specification

// Specification file type
type Specification = todo

// Create a specification file
let makeSpec (text: string) : Specification = todo

// Get the tokens from a specification file
let getTokens (spec: Specification) : (string * Automata.Regex) list = todo

// Get ignored tokens
let getIgnored (spec: Specification) : Automata.Regex list = todo