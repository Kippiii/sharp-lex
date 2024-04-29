/// ===========================================
/// Module for dealing with Regular Expressions
/// ===========================================
module RegularExpressions

open System

type RegexOp =
    | SStar
    | SPlus
    | SOption
    | SMatch of int * int
    | SAlternate
    | SConcat
    | Paren

type Regex =
    | Alter of Regex * Regex
    | Concat of Regex * Regex
    | Kleine of Regex
    | Char of char
    | Empty

let all_chars: char list = List.map char [0..255]
let not_chars (cs: char list) : char list =
    let arr = [|for i in 0 .. 255 -> false|]
    for c in cs do
        arr[int c] <- true
    let mutable result = []
    for i in 0 .. 255 do
        if not arr[i] then
            result <- char i :: result
    result
let rec make_or (cs: char list) : Regex =
    match cs with
    | [c] -> Char c
    | c :: rest -> Alter (Char c, make_or rest)

let rec concat_n_times (re: Regex) (n: int) : Regex =
    if n > 1 then
        Concat (re, concat_n_times re (n - 1))
    else
        re
let rec concat_up_to_n (re: Regex) (n: int) : Regex =
    if n > 0 then
        Alter (Empty, Concat (re, concat_up_to_n re (n-1)))
    else
        Empty

let process (str: string) : Regex =
    let mutable cs = Stack.from_list <| Seq.toList str
    let mutable ops = Stack.empty ()
    let mutable results = Stack.empty ()
    let pop_stack () : unit =
        let cur = Stack.pop &ops
        match cur with
        | SStar ->
            let a = Stack.pop &results
            Stack.push &results (Kleine a)
        | SPlus ->
            let a = Stack.pop &results
            Stack.push &results (Concat (a, Kleine a))
        | SOption ->
            let a = Stack.pop &results
            Stack.push &results (Alter (Empty, a))
        | SMatch (i, j) ->
            let a = Stack.pop &results
            Stack.push &results (Concat (concat_n_times a i, concat_up_to_n a (j - i)))
        | SAlternate ->
            let a = Stack.pop &results
            let b = Stack.pop &results
            Stack.push &results (Alter (a, b))
        | SConcat ->
            let a = Stack.pop &results
            let b = Stack.pop &results
            Stack.push &results (Concat (a, b))
        | Paren ->
            raise "ERROR! Unclosed parentheses"
    let add_to_stack (op : RegexOp) : unit =
        match op with
        | SStar | SPlus | SOption | SMatch (_, _) | Paren | SConcat ->
            Stack.push &ops op
        | SAlternate ->
            while Stack.peek &ops = SConcat do
                pop_stack ()
            Stack.push &ops op
    let read_int () : int option =
        if not <| Char.IsDigit (Stack.peek &cs) then
            None
        else
            let mutable i = 0
            while cs.Length > 0 && Char.IsDigit (Stack.peek &cs) do
                let j = int (Stack.pop &cs) - int '0'
                i <- i*10 + j
            Some i
    while cs.Length > 0 do
        let cur = Stack.pop &cs
        match cur with
        | '(' ->
            add_to_stack Paren
        | ')' ->
            while (ops.Length > 0 && Stack.peek &ops <> Paren) do
                pop_stack ()
            if ops.Length = 0 then
                raise "Too many close parens"
            Stack.pop &ops
            ()
        | '.' ->
            Stack.push &results (make_or all_chars)
        | '*' ->
            add_to_stack SStar
            pop_stack ()
        | '+' ->
            add_to_stack SPlus
            pop_stack ()
        | '?' ->
            add_to_stack SOption
            pop_stack ()
        | '{' ->
            let lower = read_int ()
            let upper =
                match Stack.peek &cs with
                | ',' ->
                    Stack.pop &cs
                    let i = read_int ()
                    if Stack.pop &cs <> '}' then
                        raise "ERROR!"
                    i
                | '}' ->
                    Stack.pop &cs
                    None
                | _ ->
                    raise "ERROR!"
            let to_add =
                match (lower, upper) with
                | (Some i, Some j) -> SMatch (i, j)
                | (None, Some j) -> SMatch (0, j)
                | (Some i, None) -> SMatch (i, Int32.MaxValue)
                | _ -> raise "ERROR!"
            add_to_stack to_add
            pop_stack ()
        | '|' ->
            add_to_stack SAlternate
        | '[' ->
            let is_not =
                if Stack.peek &cs = '^' then
                    Stack.pop &cs
                    true
                else
                    false
            let mutable chars = []
            if Stack.peek &cs = ']' then
                chars <- ']' :: chars
                Stack.pop &cs
                ()
            while cs.Length > 0 && Stack.peek &cs <> ']' do
                chars <- pop &cs :: chars
            if cs.Length = 0 then
                raise "ERROR! Did not close ']'"
            let to_add =
                if is_not then
                    make_or (not_chars chars)
                else
                    make_or chars
            Stack.push &results to_add
        | '\\' ->
            let ch = Stack.pop &cs
            let to_add =
                match ch with
                | '.' | '*' | '+' | '?' | '(' | ')' | '{' | '}' | '[' | ']' | '|' -> ch
                | 'n' -> '\n'
                | 't' -> '\t'
                | 'r' -> '\r'
                | 'b' -> '\b'
                | 'f' -> '\f'
                | 'v' -> '\v'
                | '0' -> char 0
                | 'x' -> 
                    let h1 = Stack.pop &cs
                    let h2 = Stack.pop &cs
                    let get_hex (h: char) : int =
                        if int h >= int '0' && int h <= int '9' then
                            int h - int '0'
                        elif int h >= int 'A' && int h <= int 'F' then
                            int h - int 'A' + 10
                        elif int h >= int 'a' && int h <= int 'f' then
                            int h - int 'a' + 10
                        else
                            raise "Invalid hex!"
                    char (get_hex h1 * 16 + get_hex h2)
            add_to_stack SConcat
            Stack.push &results (Char to_add)
        | c ->
            add_to_stack SConcat
            Stack.push &results (Char c)
    while ops.Length > 0 do
        pop_stack ()
    if results.Length <> 1 then
        raise "ERROR!"
    Stack.pop &results