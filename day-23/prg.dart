import 'dart:collection';
import 'dart:io';
import 'dart:math';

typedef Input = int Function();
typedef Output = void Function(int);
typedef Predicate = bool Function(State);

class State {
  final int ip;
  final int rb;
  final Map<int, int> mem;
  final Input input;
  final Output output;

  const State._initial(Map<int, int> mem, Input input, Output output)
      : this._next(0, 0, mem, input, output);

  const State._next(this.ip, this.rb, this.mem, this.input, this.output);

  State step() {
    var op = mem[ip] % 100;
    switch (op) {
      case 1:
        return _add();
      case 2:
        return _mul();
      case 3:
        return _inp();
      case 4:
        return _outp();
      case 5:
        return _jmpt();
      case 6:
        return _jmpf();
      case 7:
        return _lt();
      case 8:
        return _eq();
      case 9:
        return _arb();
      case 99:
        return _halt();
      default:
        throw Exception("Invalid instruction $op.");
    }
  }

  int _arg(int i) {
    int mode = mem[ip] ~/ pow(10, i + 1) % 10;
    int addr = mem[ip + i];
    switch (mode) {
      case 0:
        return mem[addr] ?? 0;
      case 1:
        return addr;
      case 2:
        return mem[addr + rb] ?? 0;
      default:
        throw Exception("Invalid arg mode $mode.");
    }
  }

  Map<int, int> _sto(int i, int val) {
    int mode = mem[ip] ~/ pow(10, i + 1) % 10;
    int addr = mem[ip + i];
    var nextMem = mem; //Map<int, int>.from(mem);
    switch (mode) {
      case 0:
        nextMem[addr] = val;
        break;
      case 2:
        nextMem[addr + rb] = val;
        break;
      default:
        throw Exception("Invalid arg mode $mode.");
    }
    return nextMem;
  }

  State _add() =>
      State._next(ip + 4, rb, _sto(3, _arg(1) + _arg(2)), input, output);

  State _mul() =>
      State._next(ip + 4, rb, _sto(3, _arg(1) * _arg(2)), input, output);

  State _inp() => State._next(ip + 2, rb, _sto(1, input()), input, output);

  State _outp() {
    output(_arg(1));
    return State._next(ip + 2, rb, mem, input, output);
  }

  State _jmpt() =>
      State._next(_arg(1) != 0 ? _arg(2) : ip + 3, rb, mem, input, output);

  State _jmpf() =>
      State._next(_arg(1) == 0 ? _arg(2) : ip + 3, rb, mem, input, output);

  State _lt() => State._next(
      ip + 4, rb, _sto(3, _arg(1) < _arg(2) ? 1 : 0), input, output);

  State _eq() => State._next(
      ip + 4, rb, _sto(3, _arg(1) == _arg(2) ? 1 : 0), input, output);

  State _arb() => State._next(ip + 2, rb + _arg(1), mem, input, output);

  State _halt() => State._next(-1, rb, mem, input, output);

  get halted => ip < 0;

  State loopWhile(Predicate cond) {
    var st = this;
    while (!st.halted && cond(st)) {
      st = st.step();
    }
    return st;
  }

  @override
  String toString() => "State{ ip: $ip, rb: $rb, mem: $mem}";
}

Map<int, int> readMemory(String fileName) {
  return File(fileName)
      .readAsStringSync()
      .split(",")
      .map((l) => int.parse(l.trim()))
      .toList()
      .asMap();
}

class Computer {
  State state;
  Queue<int> inputs = Queue();
  Queue<int> outputs = Queue();
  bool idle = false;

  Computer(Map<int, int> memory, int id) {
    state = State._initial(Map.from(memory), () {
      if (inputs.isNotEmpty) {
        idle = false;
        return inputs.removeFirst();
      } else {
        idle = true;
        return -1;
      }
    }, (o) {
      idle = false;
      outputs.addLast(o);
    });
    inputs.addLast(id);
  }

  void feed(int x, int y) {
    idle = false;
    inputs.addLast(x);
    inputs.addLast(y);
  }

  bool step() {
    if (state.halted) {
      return false;
    }

    state = state.step();
    return outputs.length >= 3;
  }
}

class NAT {
  int prevy;
  int x;
  int y;

  void feed(int x, int y) {
    if (this.x == null) {
      print("NAT: Fed $y");
    }
    this.x = x;
    this.y = y;
  }

  bool send(Computer c) {
    if (x == null) {
      return false;
    }
//    print("NAT -> 0: X=$x Y=$y");
    c.feed(x, y);
    var repeating = prevy == y;
    prevy = y;
    if (repeating) {
      print("NAT: Repeating $y");
    }
    return repeating;
  }
}

main(List<String> args) {
  var memory = readMemory(args[0]);

  NAT nat = NAT();

  List<Computer> comps = [];
  for (int i = 0; i < 50; i++) {
    comps.add(Computer(memory, i));
  }

  while (true) {
    int idles = 0;
    for (int i = 0; i < comps.length; i++) {
      var c = comps[i];
      if (c.step()) {
        var t = c.outputs.removeFirst();
        var x = c.outputs.removeFirst();
        var y = c.outputs.removeFirst();
//        print("$i -> $t: X=$x Y=$y");
        if (t == 255) {
          nat.feed(x, y);
        } else {
          comps[t].feed(x, y);
        }
      }
      if (c.idle) {
        idles++;
      }
    }
    if (idles == comps.length) {
      if (nat.send(comps[0])) {
        break;
      }
    }
  }
}
