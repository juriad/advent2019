#!/usr/bin/env lua

bn = require("bn")

function table.clone(org)
	return org
--	local copy = {}
--	for k,v in pairs(org) do
--		copy[k] = v
--	end
--	return copy
end

function newState(ip, rb, mem)
	return {
		['ip'] = ip,
		['rb'] = rb,
		['mem'] = mem
	}
end

function readInput(fileName)
	local file = io.open(fileName, "r")
	line = file:read("*l")
	file:close()

	local memory = {}
	local i = 1
	for t in string.gmatch(line, "[+-]?[0-9]+") do
		memory[i] = bn:new(t)
		i = i + 1
	end
	return newState(0, 0, memory)
end

function dump(state)
	print(state.ip, state.rb, state.mem)
	for k, v in pairs(state.mem) do
		print(k, v)
	end
end

function argument(state, i)
	local modes = state.mem[state.ip + 1]:asnumber() // 100
	local shift = math.floor(10 ^ (i - 1))
	local mode = (modes // shift) % 10
	local val = state.mem[state.ip + i + 1] or 0

	if mode == 0 then
		return state.mem[val:asnumber() + 1] or 0
	elseif mode == 1 then
		return val
	elseif mode == 2 then
--		print("Rel Arg", state.rb)
		return state.mem[val:asnumber() + state.rb + 1] or 0
	else
		print("Unknown mode", mode)
	end
end

function store(state, i, value)
	local modes = state.mem[state.ip + 1]:asnumber() // 100
	local shift = math.floor(10 ^ (i - 1))
	local mode = (modes // shift) % 10
	local val = state.mem[state.ip + i + 1] or 0
	
	local mem = table.clone(state.mem)
	if mode == 0 then
		mem[val:asnumber() + 1] = value
	elseif mode == 2 then
--		print("Rel Sto", state.rb)
		mem[val:asnumber() + state.rb + 1] = value
	else
		print("Unknown mode", mode)
	end
	return mem
end

function add(state)
	local a1 = argument(state, 1)
	local a2 = argument(state, 2)
--	print("add", a1, a2)
	return newState(state.ip + 4, state.rb, store(state, 3, a1 + a2))
end

function mul(state)
	local a1 = argument(state, 1)
	local a2 = argument(state, 2)
--	print("mul", a1, a2)
	return newState(state.ip + 4, state.rb, store(state, 3, a1 * a2))
end

function inp(state)
	local n = bn:new(io.read("*n"))
	print("inp", n)
	return newState(state.ip + 2, state.rb, store(state, 1, n))
end

function outp(state)
	local a1 = argument(state, 1)
	print("outp", a1)
	return newState(state.ip + 2, state.rb, state.mem)
end

function halt(state)
--	print("halt")
	return newState(-1, state.rb, state.mem)
end

function jmpt(state)
	local a1 = argument(state, 1)
	local a2 = argument(state, 2)
--	print("jmpt", a1, a2)
	if a1 ~= bn:new(0) then
		return newState(a2:asnumber(), state.rb, state.mem)
	else
		return newState(state.ip + 3, state.rb, state.mem)
	end
end

function jmpf(state)
	local a1 = argument(state, 1)
	local a2 = argument(state, 2)
--	print("jmpf", a1, a2)
	if a1 == bn:new(0) then
		return newState(a2:asnumber(), state.rb, state.mem)
	else
		return newState(state.ip + 3, state.rb, state.mem)
	end
end

function lt(state)
	local a1 = argument(state, 1)
	local a2 = argument(state, 2)
--	print("lt", a1, a2)
	if a1 < a2 then
		return newState(state.ip + 4, state.rb, store(state, 3, bn:new(1)))
	else
		return newState(state.ip + 4, state.rb, store(state, 3, bn:new(0)))
	end
end

function eq(state)
	local a1 = argument(state, 1)
	local a2 = argument(state, 2)
--	print("eq", a1, a2)
	if a1 == a2 then
		return newState(state.ip + 4, state.rb, store(state, 3, bn:new(1)))
	else
		return newState(state.ip + 4, state.rb, store(state, 3, bn:new(0)))
	end
end

function arb(state)
	local a1 = argument(state, 1)
--	print("arb", a1)
	return newState(state.ip + 2, state.rb + a1:asnumber(), state.mem)
end

function step(state)
	local inst = state.mem[state.ip + 1]:asnumber() % 100

	if inst == 1 then
		return add(state)
	elseif inst == 2 then
		return mul(state)
	elseif inst == 3 then
		return inp(state)
	elseif inst == 4 then
		return outp(state)
	elseif inst == 5 then
		return jmpt(state)
	elseif inst == 6 then
		return jmpf(state)
	elseif inst == 7 then
		return lt(state)
	elseif inst == 8 then
		return eq(state)
	elseif inst == 9 then
		return arb(state)
	elseif inst == 99 then
		return halt(state)
	else
		print("Unknown instruction", inst)
	end
end

function loop(state)
	while state.ip ~= -1 do
		state = step(state)
--		dump(state)
	end
end

function main()
	local state = readInput(arg[1])
	loop(state)
end

main()
