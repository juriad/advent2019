import sys
from queue import Queue


def read_memory(file_name):
    with open(file_name, 'r') as file:
        content = file.read()
        mem_list = list(map(lambda t: int(t), content.split(",")))
        return {i: mem_list[i] for i in range(0, len(mem_list))}


class State:
    def __init__(self, mem, input, output, ip=0, rb=0):
        self.ip = ip
        self.rb = rb
        self.mem = mem
        self.input = input
        self.output = output

    def _mode(self, i):
        return self.mem[self.ip] // int(pow(10, i + 1)) % 10

    def _arg(self, i):
        a = self.mem[self.ip + i]
        m = self._mode(i)
        if m == 0:
            return self.mem.get(a, 0)
        elif m == 1:
            return a
        elif m == 2:
            return self.mem.get(a + self.rb, 0)
        else:
            raise Exception(f"Invalid arg mode {m}")

    def _sto(self, i, val):
        a = self.mem[self.ip + i]
        m = self._mode(i)
        mem = self.mem #.copy()
        if m == 0:
            mem[a] = val
        elif m == 2:
            mem[a + self.rb] = val
        else:
            raise Exception(f"Invalid sto mode {m}")
        return mem

    def _dup(self, ip=None, rb=None, mem=None):
        return State(self.mem if mem is None else mem,
                     self.input,
                     self.output,
                     self.ip if ip is None else ip,
                     self.rb if rb is None else rb)

    def _add(self):
        return self._dup(ip=self.ip + 4, mem=self._sto(3, self._arg(1) + self._arg(2)))

    def _mul(self):
        return self._dup(ip=self.ip + 4, mem=self._sto(3, self._arg(1) * self._arg(2)))

    def _inp(self):
        return self._dup(ip=self.ip + 2, mem=self._sto(1, self.input()))

    def _outp(self):
        self.output(self._arg(1))
        return self._dup(ip=self.ip + 2)

    def _jmpt(self):
        return self._dup(ip=self._arg(2) if self._arg(1) != 0 else self.ip + 3)

    def _jmpf(self):
        return self._dup(ip=self._arg(2) if self._arg(1) == 0 else self.ip + 3)

    def _lt(self):
        return self._dup(ip=self.ip + 4, mem=self._sto(3, 1 if self._arg(1) < self._arg(2) else 0))

    def _eq(self):
        return self._dup(ip=self.ip + 4, mem=self._sto(3, 1 if self._arg(1) == self._arg(2) else 0))

    def _arb(self):
        return self._dup(ip=self.ip + 2, rb=self.rb + self._arg(1))

    def _halt(self):
        return self._dup(ip=-1)

    INSTRUCTIONS = {
        1: _add,
        2: _mul,
        3: _inp,
        4: _outp,
        5: _jmpt,
        6: _jmpf,
        7: _lt,
        8: _eq,
        9: _arb,
        99: _halt,
    }

    def step(self):
        op = self.mem[self.ip] % 100
        if op not in self.INSTRUCTIONS:
            raise Exception(f"Invalid instruction {op}")
        return self.INSTRUCTIONS[op](self)

    def halted(self):
        return self.ip < 0

    def loop_while(self, cond):
        st = self
        while not st.halted() and cond(st):
            st = st.step()
        return st

    def __str__(self):
        return "State[ip={0}, rb={1}, mem=...]".format(self.ip, self.rb)


START = """south
take fuel cell
north
west
take mouse
west
south
take dark matter
north
east
east
north
west
south
take planetoid
west
take antenna
east
east
take mutex
south
take whirled peas
south
east
"""


def qq(q, s):
    for c in s:
        q.put(ord(c))
    if not s.endswith("\n"):
        q.put(10)


def weight(q):
    things = ["fuel cell", "mouse", "dark matter", "planetoid", "antenna", "mutex", "whirled peas"]
    for s in range(0, 2 ** len(things)):
        for t in things:
            qq(q, "drop " + t)
        for i, t in enumerate(things):
            if s & (1 << i):
                qq(q, "take " + t)
        qq(q, "north")


def main():
    q = Queue()

    def inp():
        if q.empty():
            cmd = input("Empty q:")
            if cmd == "start":
                qq(q, START)
            elif cmd == "weight":
                weight(q)
            else:
                qq(q, cmd)
        return q.get()

    mem = read_memory(sys.argv[1])
    st0 = State(mem, inp, lambda o: print(chr(o), end=""))
    st1 = st0.loop_while(lambda s: True)
    print(st1)


if __name__ == '__main__':
    main()
