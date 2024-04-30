/// ===========================================
/// Module for General Utility Functions
/// ===========================================

module Utilities

exception HexParseError of string

let parse_hex (s: string) : int =
    let rec hex_rec (l: char list) : int =
        match l with
        | c :: rest ->
            let cur = 
                if int c >= int '0' && int c <= int '9' then
                    int c - int '0'
                elif int c >= int 'A' && int c <= int 'F' then
                    int c - int 'A' + 10
                elif int c >= int 'a' && int c <= int 'f' then
                    int c - int 'a' + 10
                else
                    raise <| HexParseError $"{c}"
            cur + 16 *  hex_rec rest
        | [] -> 0
    hex_rec <| (List.rev <| Seq.toList s)