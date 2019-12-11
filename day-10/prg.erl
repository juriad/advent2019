-module(prg).
-export([readMap/1, findAsteroids/1, angle/2, mapReduce/6, explore/4, reduce/4, task1/2, task2/3, tasks/1]).

readMap(FileName) ->
	{_, Input} = file:read_file(FileName),
	Trimmed = string:trim(binary_to_list(Input)),
	Lines = string:split(Trimmed, "\n", all),
	lists:map(
		fun(Line) -> 
			lists:map(
				fun(Char) -> 
					case Char of
						$# -> true;
						$. -> false
					end
				end,
				Line
			)
		end,
		Lines
	).

findAsteroids(Map) ->
	lists:filtermap(
		fun({X, Y, Cell}) ->
			case Cell of
				false -> false;
				true -> {true, {X, Y}}
			end
		end,
		lists:flatmap(
			fun({Row, Y}) ->
				lists:map(
					fun({Cell, X}) ->
						{X, Y, Cell}
					end,
					lists:zip(Row, lists:seq(0, length(Row) - 1))
				)
			end,
			lists:zip(Map, lists:seq(0, length(Map) - 1))
		)
	).

gcd(A, B) ->
	if
		B == 0 -> A;
		true -> gcd(B, A rem B)
	end.

sgn(A) ->
	if
		A < 0 -> -1;
		A == 0 -> 0;
		A > 0 -> 1
	end.

normalize1(DX, DY) ->
	G = gcd(DX, DY),
	{DX div G, DY div G}.

normalize(DX, DY) ->
	if
		{DX, DY} == {0, 0} -> {0, 0};
		DX == 0 -> {0, sgn(DY)};
		DY == 0 -> {sgn(DX), 0};
		true -> 
			{A, B} = normalize1(abs(DX), abs(DY)),
			{A * sgn(DX), B * sgn(DY)}
	end.

angle({X1, Y1}, {X2, Y2}) ->
	normalize(X2 - X1, Y2 - Y1).

pol({A1, A2}) ->
	A = math:atan2(A1, -A2),
	if
		A < 0 -> A + 2 * math:pi();
		true -> A
	end.

distance({X1, Y1}, {X2, Y2}) ->
	abs(X2 - X1) + abs(Y2 - Y1).

explore(As, Map, PostMap, ReducerPID) ->
	receive
		kill ->
			ok;
		A ->
			Result = PostMap(
				A,
				lists:map(
					fun(B) ->
						Map(A, B)
					end,
					As
				)
			),
			ReducerPID ! Result,
			explore(As, Map, PostMap, ReducerPID)
	end.

reduce(Acc, Reduce, N, ReportPID) ->
	receive
		Res ->
%			io:fwrite("Partial: "), io:write(Res), io:nl(), 
			Acc2 = Reduce(Acc, Res),
			if
				N == 1 -> 
%					io:fwrite("Result: "), io:write(Acc2), io:nl(),
					ReportPID ! Acc2;
				true -> reduce(Acc2, Reduce, N - 1, ReportPID)
			end
	end.

mapReduce(As, Par, Map, PostMap, Initial, Reduce) ->
	ReducerPID = spawn(?MODULE, reduce, [Initial, Reduce, length(As), self()]),
	Explorers = lists:map(fun(_) -> spawn(?MODULE, explore, [As, Map, PostMap, ReducerPID]) end, lists:seq(1, Par)),
	lists:map(
		fun(A) ->
			I = rand:uniform(Par),
			E = lists:nth(I, Explorers),
			E ! A
		end,
		As
	),
	lists:map(
		fun(E) -> E ! kill end,
		Explorers
	),
	receive
		Res ->
			Res
	end.

task1(As, Par) ->
	{{X, Y}, Cnt} = mapReduce(As, Par,
		fun(A, B) -> angle(A, B) end,
		fun(A, Bs) -> {A, sets:size(sets:from_list(Bs)) - 1} end,
		{{-1, -1}, -1},
		fun({A1, Cnt1}, {A2, Cnt2}) -> 
			if
				Cnt1 > Cnt2 -> {A1, Cnt1};
				true -> {A2, Cnt2}
			end
		end
	),
	io:fwrite("~b,~b: ~b~n", [X, Y, Cnt]),
	{X, Y}.

scan(As, {X, Y}) ->
	Scan = lists:filtermap(
		fun(B) ->
			if
				B == {X, Y} -> false;
				true -> {true, {B, pol(angle({X, Y}, B)), distance({X, Y}, B)}}
			end
		end,
		As
	),
	Sorted = lists:sort(
		fun({_, A1, D1}, {_, A2, D2}) ->
			if
				A1 < A2 -> true;
				(A1 == A2) and (D1 =< D2) -> true;
				true -> false
			end
		end,
		Scan
	),
	{_, L} = lists:foldl(
		fun({B, A, _}, {S, L}) ->
			E = sets:is_element(A, S),
			if
				E -> {S, L};
				true -> {sets:add_element(A, S), lists:append(L, [B])}
			end
		end,
		{sets:new(), []},
		Sorted
	),
	Rest = lists:subtract(As, L),
	{Rest, L}.

task2loop(As, A, Prefix) ->
	{Rest, List} = scan(As, A),
	Ordered = lists:append(Prefix, List),
	if
		Rest == [] -> Ordered;
		true -> task2loop(Rest, A, Ordered)
	end.

task2(As, A, Nth) ->
	Rest = lists:delete(A, As),
	Ordered = task2loop(Rest, A, []),
	Indexed = lists:zip(lists:seq(1, length(As) - 1), Ordered),
	{I, {X, Y}} = lists:nth(Nth, Indexed),
	io:fwrite("~b: ~b~n", [I, X * 100 + Y]).

tasks(FileName) ->
	M = prg:readMap(FileName),
	As = prg:findAsteroids(M),
	A = prg:task1(As, 3),
	prg:task2(As, A, 200),
	ok.

