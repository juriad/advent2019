% UTILS

replace0(0, [_|T], X, [X|T]).
replace0(I, [H|T], X, [H|R]) :-
	I > -1,
	NI is I-1,
	replace0(NI, T, X, R),
	!.
replace0(_, L, _, L).

% INSTRUCTION UTILS

inst_mode(Instruction, Arg, Mode) :-
	Shift is 10 * 10 ^ Arg,
	Mode is (Instruction div Shift) mod 10.

inst_decode(Instruction, OpCode-Modes) :-
	OpCode is Instruction mod 100,
	inst_mode(Instruction, 1, M1),
	inst_mode(Instruction, 2, M2),
	inst_mode(Instruction, 3, M3),
	inst_mode(Instruction, 4, M4),
	Modes = [ M1, M2, M3, M4 ].

inst_load(IP-Memory, Inst) :-
	nth0(IP, Memory, Instruction),
	inst_decode(Instruction, Inst).

arg_load_mode(_, ArgValue, 1, ArgValue).
arg_load_mode(Memory, ArgValue, 0, Value) :-
	nth0(ArgValue, Memory, Value).

arg_load(IP-Memory, Modes, Arg, Value) :-
	ArgP is IP + Arg,
	nth0(ArgP, Memory, ArgValue),
	nth1(Arg, Modes, Mode),
	arg_load_mode(Memory, ArgValue, Mode, Value).

res_store(IP-Memory, Arg, Value, Memory2) :-
	ArgP is IP + Arg,
	nth0(ArgP, Memory, StoP),
	replace0(StoP, Memory, Value, Memory2).

% INSTRUCTIONS

% add
inst(IP-Memory-In-Out, 1-Modes, IP2-Memory2-In-Out) :-
	arg_load(IP-Memory, Modes, 1, A1),
	arg_load(IP-Memory, Modes, 2, A2),
	R is A1 + A2,
	res_store(IP-Memory, 3, R, Memory2),
	IP2 is IP + 4,
	!.

% mul
inst(IP-Memory-In-Out, 2-Modes, IP2-Memory2-In-Out) :-
	arg_load(IP-Memory, Modes, 1, A1),
	arg_load(IP-Memory, Modes, 2, A2),
	R is A1 * A2,
	res_store(IP-Memory, 3, R, Memory2),
	IP2 is IP + 4,
	!.

% inp
inst(IP-Memory-[I|In]-Out, 3-_, IP2-Memory2-In-Out) :-
	res_store(IP-Memory, 1, I, Memory2),
	IP2 is IP + 2,
	!.

% outp
inst(IP-Memory-In-Out, 4-Modes, IP2-Memory-In-[O|Out]) :-
	arg_load(IP-Memory, Modes, 1, O),
	IP2 is IP + 2,
	!.

% jmpt
inst(IP-Memory-In-Out, 5-Modes, IP2-Memory-In-Out) :-
	arg_load(IP-Memory, Modes, 1, A1),
	arg_load(IP-Memory, Modes, 2, A2),
	( A1 \= 0 -> IP2 = A2; IP2 is IP + 3 ),
	!.

% jmpf
inst(IP-Memory-In-Out, 6-Modes, IP2-Memory-In-Out) :-
	arg_load(IP-Memory, Modes, 1, A1),
	arg_load(IP-Memory, Modes, 2, A2),
	( A1 = 0 -> IP2 = A2; IP2 is IP + 3 ),
	!.

% lt
inst(IP-Memory-In-Out, 7-Modes, IP2-Memory2-In-Out) :-
	arg_load(IP-Memory, Modes, 1, A1),
	arg_load(IP-Memory, Modes, 2, A2),
	( A1 < A2 -> R = 1; R = 0 ),
	res_store(IP-Memory, 3, R, Memory2),
	IP2 is IP + 4,
	!.

% eq
inst(IP-Memory-In-Out, 8-Modes, IP2-Memory2-In-Out) :-
	arg_load(IP-Memory, Modes, 1, A1),
	arg_load(IP-Memory, Modes, 2, A2),
	( A1 = A2 -> R = 1; R = 0 ),
	res_store(IP-Memory, 3, R, Memory2),
	IP2 is IP + 4,
	!.

% halt
inst(_-Memory-In-Out, 99-_, -1-Memory-In-Out) :-
	!.

% PROCESSING

step(IP-Memory-In-Out, IP2-Memory2-In2-Out2) :-
	inst_load(IP-Memory, Inst),
	inst(IP-Memory-In-Out, Inst, IP2-Memory2-In2-Out2).

steps1(IP-Memory-In-Out, Predicate, IP-Memory-In-Out) :-
	call(Predicate, IP, Memory, Out),
	!.
steps1(IP-Memory-In-Out, Predicate, IP3-Memory3-In3-Out3) :-
	step(IP-Memory-In-Out, IP2-Memory2-In2-Out2),
	steps1(IP2-Memory2-In2-Out2, Predicate, IP3-Memory3-In3-Out3).

steps(IP-Memory-In, Predicate, IP2-Memory2-Out) :-
	steps1(IP-Memory-In-[], Predicate, IP2-Memory2-_-Out).

until_halt(-1, _, _).

until_output(-1, _, _) :-
	!.
until_output(_, _, [X | _]) :-
	nonvar(X).

% MEMORY UTILS

read_memory_string(String, Memory) :-
	split_string(String, ",", "\n ", Tokens),
	maplist([S, N] >> number_string(N, S), Tokens, Memory).

read_memory_file(FileName, Memory) :-
	read_file_to_string(FileName, String, []),
	read_memory_string(String, Memory).

% PHASES

phases1(Min, Min, [Min]) :-
	!.
phases1(N, Min, [N | Ns]) :-
	N2 is N - 1,
	phases1(N2, Min, Ns).

phases(N, Min, Ns) :-
	N2 is Min + N - 1,
	phases1(N2, Min, Ns).

all_phases(Phases, AllPhases) :-
	findall(P, permutation(P, Phases), AllPhases).

% AMPLIFIER

amplifiers(_, [], []) :-
	!.
amplifiers(Memory, [P | Phases], [0-Memory-[P]-[] | Amplifiers]) :-
	amplifiers(Memory, Phases, Amplifiers).

amplify1([], _, []).
amplify1([IP-Memory-In-Out | Amplifiers], 
	Value, 
	[IP3-Memory3-[]-Out3 | Rest]) :-
	append(In, [Value], In2),
	steps(IP-Memory-In2, until_output, IP3-Memory3-Out2),
	( [Value2] = Out2
		-> amplify1(Amplifiers, Value2, Rest), Out3 = Out2
		; Out3 = Out, Rest = Amplifiers).

amplify(Amplifiers, Value, Output) :-
	amplify1(Amplifiers, Value, Processed),
	last(Processed, _-_-_-[NextValue]),
	( Processed = [-1-_-_-_ | _]
		-> Output = NextValue
		; amplify(Processed, NextValue, Output)).

amplify(Memory, Phases, Value, Output) :-
	amplifiers(Memory, Phases, Amplifiers),
	amplify(Amplifiers, Value, Output).

% MAXIMUM

max_value(Memory, Phases, Value, Output) :-
	all_phases(Phases, AllPhases),
	maplist({Memory, Value}/[P, O] >> amplify(Memory, P, Value, O),
		AllPhases, AllValues),
	max_list(AllValues, Output).

task(FileName, Offset) :-
	read_memory_file(FileName, Memory),
	phases(5, Offset, Phases),
	max_value(Memory, Phases, 0, Output),
	writeln(Output).

