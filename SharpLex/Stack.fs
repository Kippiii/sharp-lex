/// ===========================================
/// Module for mutable stacks
/// ===========================================

module MutableStack

open System

type 'a Stack = Stuff of 'a * 'a Stack | Empty

let empty () : 'a Stack = Empty

let rec from_list (l : 'a list) : 'a Stack =
    match l with
    | (x :: rest) -> Stuff (x, from_list rest)
    | [] -> Empty

let pop (s: byref<'a Stack>) : 'a =
    match s with
        | Stuff (x, rest) ->
            s <- rest
            x
        | Empty ->
            raise "ERROR!"

let peek (s: byref<'a Stack>) : 'a =
    match s with
    | Stuff (x, _) -> x
    | Empty -> raise "ERROR!"

let push (s: byref<'a Stack>) (x: 'a) : unit =
    s <- Stuff (x, s)