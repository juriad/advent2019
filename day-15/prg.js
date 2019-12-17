function readMemory(fileName) {
    const fs = require('fs');
    return {...fs.readFileSync(fileName, 'utf8').trim().split(',').map(v => +v)}
}

function state0(mem, input, output) {
    return {
        ip: 0,
        rb: 0,
        mem: mem,
        input: input,
        output: output,
    };
}

// CALL ON STATE

function decode() {
    const instr = this.mem[this.ip];
    return {
        op: Math.floor(instr % 100),
        mode: i => Math.floor(instr / Math.pow(10, i + 1)) % 10,
    }
}

function argument(instr, i) {
    const a = this.mem[this.ip + i] || 0;
    const mode = instr.mode(i);

    switch (mode) {
        case 0:
            return this.mem[a] || 0;
        case 1:
            return a;
        case 2:
            return this.mem[a + this.rb] || 0;
        default:
            throw new Error(`Invalid mode ${mode}.`);
    }
}

function store(instr, i, val) {
    const a = this.mem[this.ip + i] || 0;
    const mode = instr.mode(i);
    let key;

    switch (mode) {
        case 0:
            key = a;
            break;
        case 2:
            key = a + this.rb;
            break;
        default:
            throw new Error(`Invalid mode ${mode}.`);
    }
    return {
        ...this.mem,
        [key]: val,
    }
}

const INSTRUCTIONS = {
    1: function add(instr) {
        const a1 = argument.call(this, instr, 1);
        const a2 = argument.call(this, instr, 2);
        return {
            ...this,
            mem: store.call(this, instr, 3, a1 + a2),
            ip: this.ip + 4,
        }
    },
    2: function multiply(instr) {
        const a1 = argument.call(this, instr, 1);
        const a2 = argument.call(this, instr, 2);
        return {
            ...this,
            mem: store.call(this, instr, 3, a1 * a2),
            ip: this.ip + 4,
        }
    },
    3: function input(instr) {
        const a1 = this.input(this);
        if (a1 === undefined) {
            return {
                ...this,
                ip: -1,
            }
        } else {
            return {
                ...this,
                mem: store.call(this, instr, 1, a1),
                ip: this.ip + 2,
            }
        }
    },
    4: function output(instr) {
        const a1 = argument.call(this, instr, 1);
        this.output(a1);
        return {
            ...this,
            ip: this.ip + 2,
        }
    },
    5: function jumpIfTrue(instr) {
        const a1 = argument.call(this, instr, 1);
        const a2 = argument.call(this, instr, 2);
        return {
            ...this,
            ip: a1 !== 0 ? a2 : this.ip + 3,
        }
    },
    6: function jumpIfFalse(instr) {
        const a1 = argument.call(this, instr, 1);
        const a2 = argument.call(this, instr, 2);
        return {
            ...this,
            ip: a1 === 0 ? a2 : this.ip + 3,
        }
    },
    7: function lessThan(instr) {
        const a1 = argument.call(this, instr, 1);
        const a2 = argument.call(this, instr, 2);
        return {
            ...this,
            mem: store.call(this, instr, 3, a1 < a2 ? 1 : 0),
            ip: this.ip + 4,
        }
    },
    8: function equals(instr) {
        const a1 = argument.call(this, instr, 1);
        const a2 = argument.call(this, instr, 2);
        return {
            ...this,
            mem: store.call(this, instr, 3, a1 === a2 ? 1 : 0),
            ip: this.ip + 4,
        }
    },
    9: function adjustRelativeBase(instr) {
        const a1 = argument.call(this, instr, 1);
        return {
            ...this,
            rb: this.rb + a1,
            ip: this.ip + 2,
        }
    },
    99: function halt(instr) { // halt
        return {
            ...this,
            ip: -1,
        }
    }
};

function step() {
    const instr = decode.call(this);
    const op = INSTRUCTIONS[instr.op];
    if (op === undefined) {
        throw new Error(`Invalid operation ${instr.op}.`);
    }
//    console.log(this, op);
    return op.call(this, instr);
}

function halted() {
    return this.ip < 0;
}

function loop_while(cond) {
    let state = this;
    while (!halted.call(state) && cond(state)) {
        state = step.call(state);
    }
    return state;
}

// TASKS

function neighbors(point) {
    let [x, y] = point.split(",");
    return [`${x},${+y + 1}`, `${+x + 1},${y}`, `${x},${+y - 1}`, `${+x - 1},${y}`];
}

function queue() {
    let ptr = 0;
    let arr = [];
    return {
        has: function () {
            return ptr < arr.length;
        },
        add: function (e) {
            arr.push(e);
        },
        addAll: function (es) {
            for (const e of es) {
                arr.push(e);
            }
        },
        get: function () {
            return arr[ptr++];
        }
    }
}

function stack() {
    let arr = [];
    return {
        has: function () {
            return arr.length > 0;
        },
        add: function (e) {
            arr.push(e);
        },
        addAll: function (es) {
            for (const e of es) {
                arr.push(e);
            }
        },
        get: function () {
            return arr.pop();
        }
    }
}

function bfs(from, to, map) {
    const visit = queue();
    visit.add({
        point: from,
        distance: 0
    });
    const visited = {};

    while (visit.has()) {
        const {point, distance} = visit.get();
        if (visited[point] !== undefined) {
            continue;
        }

        visited[point] = distance;
        for (const n of neighbors(point)) {
            if (map[n] === 1 || map[n] === 2 || n === to) {
                visit.add({point: n, distance: distance + 1});
            }
        }

        if (point === to) {
            break;
        }
    }

    return visited;
}

function findPath(from, to, visited) {
    const path = [to];
    while (path[path.length - 1] !== from) {
        const last = path[path.length - 1];
        for (const n of neighbors(last)) {
            if (visited[n] === undefined || visited[n] !== visited[last] - 1) {
                continue
            }
            path.push(n);
            break;
        }
    }
    path.pop();
    return path.reverse();
}

function direction(from, to) {
    const [xx1, yy1] = from.split(",");
    const [xx2, yy2] = to.split(",");
    const x1 = parseInt(xx1);
    const y1 = parseInt(yy1);
    const x2 = parseInt(xx2);
    const y2 = parseInt(yy2);
    return x1 === x2 ? (y1 < y2 ? 1 : 2) : (x1 < x2 ? 4 : 3);
}

function drawMap(map) {
    let minx = 1000;
    let maxx = -1000;
    let miny = 1000;
    let maxy = -1000;
    for (const [p] of Object.entries(map)) {
        const [xx, yy] = p.split(",");
        const x = parseInt(xx);
        const y = parseInt(yy);
        if (x < minx) {
            minx = x;
        }
        if (x > maxx) {
            maxx = x;
        }
        if (y < miny) {
            miny = y;
        }
        if (y > maxy) {
            maxy = y;
        }
    }

    const a = [];
    for (let y = miny; y <= maxy; y++) {
        const aa = [];
        for (let x = minx; x <= maxx; x++) {
            aa.push(" ");
        }
        a.push(aa)
    }

    for (const [p, c] of Object.entries(map)) {
        const [xx, yy] = p.split(",");
        const x = parseInt(xx);
        const y = parseInt(yy);

        a[y - miny][x - minx] = (c === 0 ? "#" : (c === 1 ? "." : "O"));
    }
    const m = a.map(aa => aa.join("")).join("\n");
    console.log(m);
}

function maze(fileName, start) {
    const map = {[start]: 1};

    const investigate = stack();
    investigate.add(start);

    const plan = queue();

    let position = start;
    let nextPosition;

    const state = state0(readMemory("input"), st => {
        if (!plan.has()) {
//            console.log("Plan is empty");
            investigate.addAll(neighbors(position));
            while (investigate.has()) {
                const target = investigate.get();
                //console.log("Investigate?", target);
                if (map[target] !== undefined) {
                    continue
                }
//                console.log("Will investigate", target, "from", position);
                const visited = bfs(position, target, map);
                //console.log("Visited", visited);
                const path = findPath(position, target, visited);
//                console.log("Path", path);
                plan.addAll(path);
                break;
            }
        }
        if (!plan.has()) {
            return undefined;
        }
        nextPosition = plan.get();
        let dir = direction(position, nextPosition);
//        console.log("At", position, "moving to", nextPosition, "by", dir);
        return dir;
    }, val => {
//        console.log("Output", val, "at", nextPosition);
        map[nextPosition] = val;
        if (val !== 0) {
            position = nextPosition;
        }
    });
    loop_while.call(state, st => true);
    return map;
}

function findOxygen(map) {
    for (const [p, c] of Object.entries(map)) {
        if (c === 2) {
            return p;
        }
    }
}

function tasks() {
    const start = "0,0";
    const map = maze("input", start);
    const ox = findOxygen(map);
    const visited = bfs(start, ox, map);
    const path = findPath(start, ox, visited);
    console.log("Distance to oxygen", path.length);

    const spread = bfs(ox, undefined, map);
    const furthest = Math.max(...Object.entries(spread).map(kv => kv[1]));
    console.log("Distance to furthest from oxygen", furthest);
}

tasks();
