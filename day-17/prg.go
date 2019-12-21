package main

import (
	"fmt"
	"io/ioutil"
	"os"
	"strconv"
	"strings"
)

type Mem int64

func readMemory(fileName string) map[Mem]Mem {
	dat, _ := ioutil.ReadFile(fileName)
	m := make(map[Mem]Mem)
	for i, v := range strings.Split(string(dat), ",") {
		val, _ := strconv.ParseInt(strings.TrimSpace(v), 10, 64)
		m[Mem(i)] = Mem(val)
	}
	return m
}

func newState(mem map[Mem]Mem, in chan Mem, out chan Mem) State {
	return State{
		ip:  0,
		rb:  0,
		mem: mem,
		in:  in,
		out: out,
	}
}

// STATE

type State struct {
	ip  Mem
	rb  Mem
	mem map[Mem]Mem
	in  chan Mem
	out chan Mem
}

func (state *State) argument(i int, modes []int) Mem {
	a := state.mem[state.ip+Mem(i)+1]
	switch modes[i] {
	case 0:
		return state.mem[a]
	case 1:
		return a
	case 2:
		return state.mem[a+state.rb]
	default:
		panic(fmt.Errorf("illegal mode %d", modes[i]))
	}
	return a
}

func (state *State) store(i int, modes []int, value Mem) {
	a := state.mem[state.ip+Mem(i)+1]
	switch modes[i] {
	case 0:
		state.mem[a] = value
	case 2:
		state.mem[a+state.rb] = value
	default:
		panic(fmt.Errorf("illegal mode %d", modes[i]))
	}
}

func (state *State) input() Mem {
	in, ok := <-state.in
	if ok {
		return in
	} else {
		panic("No input.")
	}
}

func (state *State) output(value Mem) {
	state.out <- value
}

func (state *State) halted() bool {
	return state.ip < 0
}

func (state *State) mode(i int) int {
	m := state.mem[state.ip] / 100
	for ; i > 0; i-- {
		m /= 10
	}
	return int(m % 10)
}

func (state *State) decode() Instruction {
	op := state.mem[state.ip]
	switch op % 100 {
	case 1:
		return &Add{
			modes: [3]int{state.mode(0), state.mode(1), state.mode(2)},
		}
	case 2:
		return &Mul{
			modes: [3]int{state.mode(0), state.mode(1), state.mode(2)},
		}
	case 3:
		return &Inp{
			modes: [1]int{state.mode(0)},
		}
	case 4:
		return &Outp{
			modes: [1]int{state.mode(0)},
		}
	case 5:
		return &JmpT{
			modes: [2]int{state.mode(0), state.mode(1)},
		}
	case 6:
		return &JmpF{
			modes: [2]int{state.mode(0), state.mode(1)},
		}
	case 7:
		return &Lt{
			modes: [3]int{state.mode(0), state.mode(1), state.mode(2)},
		}
	case 8:
		return &Eq{
			modes: [3]int{state.mode(0), state.mode(1), state.mode(2)},
		}
	case 9:
		return &ARB{
			modes: [1]int{state.mode(0)},
		}
	case 99:
		return &Halt{
			modes: [0]int{},
		}
	default:
		panic(fmt.Errorf("no such instruction %d (at IP %d)", op%100, state.ip))
	}
}

// INSTRUCTIONS

type Instruction interface {
	perform(state *State) *State
}

type Add struct {
	modes [3]int
}

func (instr *Add) perform(state *State) *State {
	a1 := state.argument(0, instr.modes[:])
	a2 := state.argument(1, instr.modes[:])
	// fmt.Printf("Add %d %d\n", a1, a2)
	state.store(2, instr.modes[:], a1+a2)
	state.ip += 4
	return state
}

type Mul struct {
	modes [3]int
}

func (instr *Mul) perform(state *State) *State {
	a1 := state.argument(0, instr.modes[:])
	a2 := state.argument(1, instr.modes[:])
	// fmt.Printf("Mul %d %d\n", a1, a2)
	state.store(2, instr.modes[:], a1*a2)
	state.ip += 4
	return state
}

type Inp struct {
	modes [1]int
}

func (instr *Inp) perform(state *State) *State {
	a1 := state.input()
	// fmt.Printf("Inp %d\n", a1)
	state.store(0, instr.modes[:], a1)
	state.ip += 2
	return state
}

type Outp struct {
	modes [1]int
}

func (instr *Outp) perform(state *State) *State {
	a1 := state.argument(0, instr.modes[:])
	// fmt.Printf("Outp %d\n", a1)
	state.output(a1)
	state.ip += 2
	return state
}

type JmpT struct {
	modes [2]int
}

func (instr *JmpT) perform(state *State) *State {
	a1 := state.argument(0, instr.modes[:])
	a2 := state.argument(1, instr.modes[:])
	// fmt.Printf("JmpT %d\n", a1)
	if a1 != 0 {
		state.ip = a2
	} else {
		state.ip += 3
	}
	return state
}

type JmpF struct {
	modes [2]int
}

func (instr *JmpF) perform(state *State) *State {
	a1 := state.argument(0, instr.modes[:])
	a2 := state.argument(1, instr.modes[:])
	// fmt.Printf("JmpF %d\n", a1)
	if a1 == 0 {
		state.ip = a2
	} else {
		state.ip += 3
	}
	return state
}

type Lt struct {
	modes [3]int
}

func (instr *Lt) perform(state *State) *State {
	a1 := state.argument(0, instr.modes[:])
	a2 := state.argument(1, instr.modes[:])
	// fmt.Printf("Lt %d %d\n", a1, a2)
	if a1 < a2 {
		state.store(2, instr.modes[:], 1)
	} else {
		state.store(2, instr.modes[:], 0)
	}
	state.ip += 4
	return state
}

type Eq struct {
	modes [3]int
}

func (instr *Eq) perform(state *State) *State {
	a1 := state.argument(0, instr.modes[:])
	a2 := state.argument(1, instr.modes[:])
	// fmt.Printf("Eq %d %d\n", a1, a2)
	if a1 == a2 {
		state.store(2, instr.modes[:], 1)
	} else {
		state.store(2, instr.modes[:], 0)
	}
	state.ip += 4
	return state
}

type ARB struct {
	modes [1]int
}

func (instr *ARB) perform(state *State) *State {
	a1 := state.argument(0, instr.modes[:])
	// fmt.Printf("ARB %d\n", a1)
	state.ip += 2
	state.rb += a1
	return state
}

type Halt struct {
	modes [0]int
}

func (instr *Halt) perform(state *State) *State {
	// fmt.Printf("Halt\n")
	state.ip = -1
	close(state.out)
	return state
}

// PROCESSING

func (state *State) step() *State {
	instr := state.decode()
	// fmt.Printf("Instr is %+v\n", instr)
	return instr.perform(state)
}

func (state *State) loopWhile(cond func(*State) bool) *State {
	st := state
	for ! st.halted() && cond(st) {
		// fmt.Printf("State is %+v\n", st)
		st = st.step()
	}
	return st
}

// MAP

type Point struct {
	x, y int
}

func (p Point) left() Point {
	return Point{
		x: p.x - 1,
		y: p.y,
	}
}

func (p Point) right() Point {
	return Point{
		x: p.x + 1,
		y: p.y,
	}
}

func (p Point) up() Point {
	return Point{
		x: p.x,
		y: p.y - 1,
	}
}

func (p Point) down() Point {
	return Point{
		x: p.x,
		y: p.y + 1,
	}
}

func readMap(mem map[Mem]Mem) map[Point]rune {
	in := make(chan Mem)
	out := make(chan Mem)
	state := newState(mem, in, out)

	go state.loopWhile(func(st *State) bool { return true })

	m := make(map[Point]rune)

	x, y := 0, 0
	for i := range out {
		// fmt.Printf("%c", rune(i))

		if i == 10 {
			y++
			x = 0
		} else {
			m[Point{x, y}] = rune(i)
			x++
		}
	}
	close(in)
	return m
}

func task1(m map[Point]rune) {
	cnt := 0
	dists := 0
	for p, r := range m {
		if r == '#' {
			// fmt.Printf("%c %c %c %c\n", m[p.left()], m[p.right()], m[p.up()], m[p.down()])
			if m[p.left()] == '#' && m[p.right()] == '#' && m[p.up()] == '#' && m[p.down()] == '#' {
				cnt++
				dists += p.x * p.y
			}
		}
	}
	fmt.Printf("Intersections: %d; distances: %d\n", cnt, dists)
}

// ROBOT

type Direction int

type Robot struct {
	p   Point
	dir Direction
}

func findRobot(m map[Point]rune) Robot {
	for p, r := range m {
		switch r {
		case '^':
			return Robot{p: p, dir: 0}
		case '<':
			return Robot{p: p, dir: 3}
		case '>':
			return Robot{p: p, dir: 1}
		case 'v':
			return Robot{p: p, dir: 2}
		}
	}
	panic("Could not found the robot on the map.")
}

func (p Point) move(direction Direction, dist int) Point {
	switch direction {
	case 0:
		return Point{p.x, p.y - dist}
	case 1:
		return Point{p.x + dist, p.y}
	case 2:
		return Point{p.x, p.y + dist}
	case 3:
		return Point{p.x - dist, p.y}
	default:
		panic("Incorrect direction")
	}
}

func (r Robot) tryWalk(m map[Point]rune) int {
	walk := 0
	for m[r.p.move(r.dir, walk+1)] == '#' {
		walk++
	}
	return walk
}

func (r Robot) walk(dist int) Robot {
	return Robot{
		p:   r.p.move(r.dir, dist),
		dir: r.dir,
	}
}

func (r Robot) left() Robot {
	return Robot{
		p:   r.p,
		dir: (r.dir + 3) % 4,
	}
}

func (r Robot) right() Robot {
	return Robot{
		p:   r.p,
		dir: (r.dir + 1) % 4,
	}
}

func findPath(m map[Point]rune, r Robot) string {
	rl := r.left()
	wl := rl.tryWalk(m)
	if wl > 0 {
		// fmt.Printf("L%d\n", wl)
		return fmt.Sprintf("L,%d,%s", wl, findPath(m, rl.walk(wl)))
	}

	rr := r.right()
	wr := rr.tryWalk(m)
	if wr > 0 {
		// fmt.Printf("R%d\n", wr)
		return fmt.Sprintf("R,%d,%s", wr, findPath(m, rr.walk(wr)))
	}

	// nowhere to walk
	return ""
}

func inputProgram(in chan Mem, prg string) {
	for _, char := range prg {
		in <- Mem(char)
	}
	in <- 10
}

func task2(mem map[Mem]Mem, m map[Point]rune) {
	mem[0] = 2

	in := make(chan Mem, 1000)
	out := make(chan Mem)
	state := newState(mem, in, out)

	go state.loopWhile(func(st *State) bool { return true })

	println(findPath(m, findRobot(m)))

	// manual optimization
	inputProgram(in, "A,A,B,C,B,C,B,C,B,A")
	inputProgram(in, "L,10,L,8,R,8,L,8,R,6")
	inputProgram(in, "R,6,R,8,R,8")
	inputProgram(in, "R,6,R,6,L,8,L,10")
	inputProgram(in, "n")

	var last Mem
	for i := range out {
		last = i
		// fmt.Printf("%c", rune(i))
	}

	fmt.Printf("Dust particles: %d\n", last)
	close(in)
}

func main() {
	mem := readMemory(os.Args[1])
	m := readMap(mem)
	task1(m)

	mem = readMemory(os.Args[1])
	task2(mem, m)
}
