open Printf

module Mem = Map.Make(struct type t = int let compare = compare end)

type state = State of int * int * int Mem.t * int list * int list

let read_memory filename =
    let chan = open_in filename in
    let content = input_line chan in
    let list = List.map int_of_string (String.split_on_char ',' content) in
    let ilist = List.mapi (fun i a -> (i, a)) list in
    List.fold_left (fun acc (i, m) -> Mem.add i m acc) Mem.empty ilist

let state0 memory input = State (0, 0, memory, input, [])

let step (State (ip, rb, mem, input, output)) =
    let default opt =
        match opt with
        | Some v -> v
        | None -> 0
    in
    let mode_div i =
        match i with
        | 1 -> 100
        | 2 -> 1000
        | 3 -> 10000
        | _ -> raise (Failure "Invalid position")
    in
    let arg (ip, rb, mem) i =
        let op = Mem.find ip mem in
        let mode = (op / (mode_div i) mod 10) in
        let a = Mem.find (ip + i) mem in
        match mode with
        | 0 -> default (Mem.find_opt a mem)
        | 1 -> a
        | 2 -> default (Mem.find_opt (a + rb) mem)
        | _ -> raise (Failure "Invalid arg mode")
    in
    let sto (ip, rb, mem) i value =
        let op = Mem.find ip mem in
        let mode = (op / (mode_div i) mod 10) in
        let a = Mem.find (ip + i) mem in
        match mode with
        | 0 -> Mem.add a value mem
        | 2 -> Mem.add (a + rb) value mem
        | _ -> raise (Failure "Invalid sto mode")
    in
    let add (State (ip, rb, mem, input, output)) =
        let a1 = arg (ip, rb, mem) 1 in
        let a2 = arg (ip, rb, mem) 2 in
        State (ip + 4, rb, (sto (ip, rb, mem) 3 (a1 + a2)), input, output)
    in
    let mul (State (ip, rb, mem, input, output)) =
        let a1 = arg (ip, rb, mem) 1 in
        let a2 = arg (ip, rb, mem) 2 in
        State (ip + 4, rb, (sto (ip, rb, mem) 3 (a1 * a2)), input, output)
    in
    let inp (State (ip, rb, mem, input, output)) =
        match input with
        | [] -> raise (Failure "No input")
        | (i :: rest) -> State (ip + 2, rb, (sto (ip, rb, mem) 1 (i)), rest, output)
    in
    let outp (State (ip, rb, mem, input, output)) =
        let a1 = arg (ip, rb, mem) 1 in
        printf "%c" (if a1 < 255 then char_of_int a1 else '%');
        State (ip + 2, rb, mem, input, a1 :: output)
    in
    let jmpt (State (ip, rb, mem, input, output)) =
        let a1 = arg (ip, rb, mem) 1 in
        let a2 = arg (ip, rb, mem) 2 in
        State ((if a1 <> 0 then a2 else ip + 3), rb, mem, input, output)
    in
    let jmpf (State (ip, rb, mem, input, output)) =
        let a1 = arg (ip, rb, mem) 1 in
        let a2 = arg (ip, rb, mem) 2 in
        State ((if a1 = 0 then a2 else ip + 3), rb, mem, input, output)
    in
    let lt (State (ip, rb, mem, input, output)) =
        let a1 = arg (ip, rb, mem) 1 in
        let a2 = arg (ip, rb, mem) 2 in
        State (ip + 4, rb, (sto (ip, rb, mem) 3 (if a1 < a2 then 1 else 0)), input, output)
    in
    let eq (State (ip, rb, mem, input, output)) =
        let a1 = arg (ip, rb, mem) 1 in
        let a2 = arg (ip, rb, mem) 2 in
        State (ip + 4, rb, (sto (ip, rb, mem) 3 (if a1 = a2 then 1 else 0)), input, output)
    in
    let arb (State (ip, rb, mem, input, output)) =
        let a1 = arg (ip, rb, mem) 1 in
        State (ip + 2, rb + a1, mem, input, output)
    in
    let halt (State (_, rb, mem, input, output)) =
        State (-1, rb, mem, input, output)
    in

    let op = (Mem.find ip mem) mod 100 in
    match op with
    | 1 -> add (State (ip, rb, mem, input, output))
    | 2 -> mul (State (ip, rb, mem, input, output))
    | 3 -> inp (State (ip, rb, mem, input, output))
    | 4 -> outp (State (ip, rb, mem, input, output))
    | 5 -> jmpt (State (ip, rb, mem, input, output))
    | 6 -> jmpf (State (ip, rb, mem, input, output))
    | 7 -> lt (State (ip, rb, mem, input, output))
    | 8 -> eq (State (ip, rb, mem, input, output))
    | 9 -> arb (State (ip, rb, mem, input, output))
    | 99 -> halt (State (ip, rb, mem, input, output))
    | _ -> raise (Failure "Invalid instruction")

let halted (State (ip, _, _, _, _)) = ip < 0

let rec loop_while cond state =
    if not (halted state) && cond state then
        loop_while cond (step state)
    else
        state

let droid memory program =
    let chars = List.init (String.length program) (fun i -> int_of_char (String.get program i)) in
    let (State (ip, _, _, _, output)) = loop_while (fun st -> true) (state0 memory chars) in
    let (o :: _) = output in
    printf "%d\n" o

let () =
    let filename = Sys.argv.(1) in
    let memory = read_memory filename in
    let t1 = "NOT A T\nOR T J\nNOT B T\nOR T J\nNOT C T\nOR T J\nAND D J\nWALK\n" in
    droid memory t1;
    let t2 = "OR E J\nAND F J\nAND G J\nNOT H T\nOR T J\nOR C J\nOR E T\nAND I T\nOR B T\nAND T J\nAND A J\nNOT J J\nAND D J\nRUN\n" in
    droid memory t2
    (* Mem.iter (fun a b -> printf "%d %d\n" a b) memory; *)

