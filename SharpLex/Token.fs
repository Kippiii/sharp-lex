/// ===========================================
/// Module for tokens
/// ===========================================
module Token

// Type for tokens
type Token = todo

// Create a token
let makeToken (id: string) (content: string) : Token = todo

// Get the id of a token
let getId (t: Token) : string = todo

// Get content of a token
let getContent (t: Token) : string = todo

// Type for a token stream
type TokenStream = todo

// Make a token stream from a list
let makeStream (ts: Token list) : TokenStream = todo

// Add to a token stream
let add (ts: TokenStream) (t: Token) : TokenStream = todo

// Convert a stream to a file
let toFile (ts: TokenStream) : string = todo