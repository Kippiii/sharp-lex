/// ===========================================
/// Module for mutable stacks
/// ===========================================

module Stack

type 'a Stack = Stuff of 'a * int * 'a Stack | Empty

exception EmptyStackError of unit

let empty () : 'a Stack = Empty

let rec from_list (l: 'a list) : 'a Stack =
    match l with
    | (x :: rest) -> Stuff (x, l.Length, from_list rest)
    | [] -> Empty

let length (s: 'a Stack) : int =
    match s with
    | Stuff (_, i, _) -> i
    | Empty -> 0

let pop (s: byref<'a Stack>) : 'a =
    match s with
        | Stuff (x, _, rest) ->
            s <- rest
            x
        | Empty ->
            raise <| EmptyStackError ()

let peek (s: byref<'a Stack>) : 'a =
    match s with
    | Stuff (x, _, _) -> x
    | Empty -> raise <| EmptyStackError ()

let push (s: byref<'a Stack>) (x: 'a) : unit =
    let size =
        match s with
        | Stuff (_, i, _) -> i
        | Empty -> 0
    s <- Stuff (x, size+1, s)