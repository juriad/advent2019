<?php /** @noinspection PhpUnhandledExceptionInspection */

class Instruction
{
    private $code;

    public function __construct(int $code)
    {
        $this->code = $code;
    }

    public function opCode(): int
    {
        return $this->code % 100;
    }

    public function mode($i): int
    {
        $modes = intdiv($this->code, 100);
        return intdiv($modes, 10 ** ($i - 1)) % 10;
    }
}

class State
{
    private $ip;
    private $rb;
    private $mem;
    private $input;
    private $output;

    public function __construct(int $ip, int $rb, array $mem, callable $input, array $output)
    {
        $this->ip = $ip;
        $this->rb = $rb;
        $this->mem = $mem;
        $this->input = $input;
        $this->output = $output;
    }

    public function decode(): Instruction
    {
        return new Instruction($this->mem[$this->ip]);
    }

    private function access(int $i): int
    {
        return array_key_exists($i, $this->mem) ? $this->mem[$i] : 0;
    }

    public function argument(int $i, int $mode): int
    {
        $val = $this->access($this->ip + $i);
        switch ($mode) {
            case 0:
                return $this->access($val);
            case 1:
                return $val;
            case 2:
                return $this->access($val + $this->rb);
            default:
                throw new Exception('Unexpected value ' . $mode);
        }
    }

    public function read(): int
    {
        return ($this->input)();
    }

    public function copy(): State
    {
        return new State($this->ip, $this->rb, $this->mem, $this->input, $this->output);
    }

    public function store(int $i, int $mode, int $value): State
    {
        $val = $this->access($this->ip + $i);
        switch ($mode) {
            case 0:
                $this->mem[$val] = $value;
                break;
            case 2:
                $this->mem[$val + $this->rb] = $value;
                break;
            default:
                throw new Exception('Unexpected value ' . $mode);
        }
        return $this;
    }

    public function adjustRelBase(int $adj): State
    {
        $this->rb += $adj;
        return $this;
    }

    public function setIp(int $ip): State
    {
        $this->ip = $ip;
        return $this;
    }

    public function moveIp(int $move): State
    {
        $this->ip += $move;
        return $this;
    }

    public function write($value): State
    {
        $this->output[] = $value;
        return $this;
    }

    public function halted(): bool
    {
        return $this->ip < 0;
    }

    public function peekOutputs(): array
    {
        return $this->output;
    }

    public function consumeOutputs(): array
    {
        $out = $this->output;
        $this->output = [];
        return $out;
    }
}

interface Instr
{
    public function perform(Instruction $instruction, State $state): State;
}

class Add implements Instr
{
    public function perform(Instruction $instruction, State $state): State
    {
        $a1 = $state->argument(1, $instruction->mode(1));
        $a2 = $state->argument(2, $instruction->mode(2));
        return $state->copy()->store(3, $instruction->mode(3), $a1 + $a2)->moveIp(4);
    }
}

class Mul implements Instr
{
    public function perform(Instruction $instruction, State $state): State
    {
        $a1 = $state->argument(1, $instruction->mode(1));
        $a2 = $state->argument(2, $instruction->mode(2));
        return $state->copy()->store(3, $instruction->mode(3), $a1 * $a2)->moveIp(4);
    }
}

class Inp implements Instr
{
    public function perform(Instruction $instruction, State $state): State
    {
        $a1 = $state->read();
        return $state->copy()->store(1, $instruction->mode(1), $a1)->moveIp(2);
    }
}

class Outp implements Instr
{
    public function perform(Instruction $instruction, State $state): State
    {
        $a1 = $state->argument(1, $instruction->mode(1));
        return $state->copy()->write($a1)->moveIp(2);
    }
}

class JmpT implements Instr
{
    public function perform(Instruction $instruction, State $state): State
    {
        $a1 = $state->argument(1, $instruction->mode(1));
        $a2 = $state->argument(2, $instruction->mode(2));
        if ($a1 !== 0) {
            return $state->copy()->setIp($a2);
        } else {
            return $state->copy()->moveIp(3);
        }
    }
}

class JmpF implements Instr
{
    public function perform(Instruction $instruction, State $state): State
    {
        $a1 = $state->argument(1, $instruction->mode(1));
        $a2 = $state->argument(2, $instruction->mode(2));
        if ($a1 === 0) {
            return $state->copy()->setIp($a2);
        } else {
            return $state->copy()->moveIp(3);
        }
    }
}

class Lt implements Instr
{
    public function perform(Instruction $instruction, State $state): State
    {
        $a1 = $state->argument(1, $instruction->mode(1));
        $a2 = $state->argument(2, $instruction->mode(2));
        return $state->copy()->store(3, $instruction->mode(3), $a1 < $a2 ? 1 : 0)->moveIp(4);
    }
}

class Eq implements Instr
{
    public function perform(Instruction $instruction, State $state): State
    {
        $a1 = $state->argument(1, $instruction->mode(1));
        $a2 = $state->argument(2, $instruction->mode(2));
        return $state->copy()->store(3, $instruction->mode(3), $a1 === $a2 ? 1 : 0)->moveIp(4);
    }
}

class ARb implements Instr
{
    public function perform(Instruction $instruction, State $state): State
    {
        $a1 = $state->argument(1, $instruction->mode(1));
        return $state->copy()->adjustRelBase($a1)->moveIp(2);
    }
}

class Halt implements Instr
{
    public function perform(Instruction $instruction, State $state): State
    {
        return $state->copy()->setIp(-1);
    }
}

class Computer
{
    private $state;
    private $instrs;

    public function __construct(array $mem, callable $input)
    {
        $this->state = new State(0, 0, $mem, $input, []);
        $this->instrs = [
            1 => new Add(),
            2 => new Mul(),
            3 => new Inp(),
            4 => new Outp(),
            5 => new JmpT(),
            6 => new JmpF(),
            7 => new Lt(),
            8 => new Eq(),
            9 => new ARb(),
            99 => new Halt()
        ];
    }

    function step(): State
    {
        $instruction = $this->state->decode();
        $opCode = $instruction->opCode();

        if (!array_key_exists($opCode, $this->instrs)) {
            throw new Exception("Invalid op code: " . $opCode);
        }

        $instr = $this->instrs[$opCode];
        $newState = $instr->perform($instruction, $this->state);

        $this->state = $newState;
        return $newState;
    }

    function state(): State
    {
        return $this->state;
    }

    function loopWhile(callable $condition): State
    {
        while (!$this->state->halted() && $condition($this->state)) {
            $this->step();
        }
        return $this->state;
    }

    function halted(): bool
    {
        return $this->state->halted();
    }
}

function readMemory(string $file)
{
    return array_map(
        function ($token) {
            return intval($token);
        },
        explode(",", file_get_contents($file))
    );
}

class Position
{
    private $x;
    private $y;
    private $dx;
    private $dy;

    public function __construct($x, $y, $dx = 0, $dy = -1)
    {
        $this->x = $x;
        $this->y = $y;
        $this->dx = $dx;
        $this->dy = $dy;
    }

    function leftAndGo(): Position
    {
        return new Position($this->x + $this->dy, $this->y - $this->dx, $this->dy, -$this->dx);
    }

    function rightAndGo(): Position
    {
        return new Position($this->x - $this->dy, $this->y + $this->dx, -$this->dy, $this->dx);
    }

    public function __toString()
    {
        return "$this->x,$this->y";
    }
}

class Painter
{
    private $comp;
    private $hull;
    private $position;

    public function __construct(array $mem, array $hull = [])
    {
        $this->position = new Position(0, 0);
        $this->comp = new Computer($mem, $this);
        $this->hull = $hull;
    }

    public function __invoke() // computer reads
    {
        $key = strval($this->position);
        $color = array_key_exists($key, $this->hull) ? $this->hull[$key] : 0;
#        echo "Read $color at $this->position\n";
        return $color;
    }

    public function step()
    {
        $state = $this->comp->loopWhile(function (State $st) {
            return count($st->peekOutputs()) < 2;
        });
        $out = $state->consumeOutputs();

        if (array_key_exists(0, $out)) {
            $key = strval($this->position);
            $color = $out[0];
            $this->hull[$key] = $color;
#            echo "Painted hull $color at $this->position\n";
        }

        if (array_key_exists(1, $out)) {
            $dir = $out[1];
#            echo "Moving $dir at $this->position\n";
            switch ($dir) {
                case 0:
                    $this->position = $this->position->leftAndGo();
                    break;
                case 1:
                    $this->position = $this->position->rightAndGo();
                    break;
                default:
                    throw new Exception('Invalid direction ' . $dir);
            }
        }
    }

    public function loop()
    {
        while (!$this->comp->halted()) {
            $this->step();
        }
    }

    public function hull(): array
    {
        return $this->hull;
    }
}

function printHull(array $hull)
{
    $minx = PHP_INT_MAX;
    $miny = PHP_INT_MAX;
    $maxx = PHP_INT_MIN;
    $maxy = PHP_INT_MIN;
    foreach ($hull as $k => $v) {
        list($x, $y) = explode(",", $k);
        $x = intval($x);
        $y = intval($y);
        $minx = min($minx, $x);
        $miny = min($miny, $y);
        $maxx = max($maxx, $x);
        $maxy = max($maxy, $y);
    }
    $paint = [];
    for ($y = $miny; $y <= $maxy; $y++) {
        $paint[$y - $miny] = [];
        for ($x = $minx; $x <= $maxx; $x++) {
            $paint[$y - $miny][$x - $minx] = '.';
        }
    }
    foreach ($hull as $k => $v) {
        list($x, $y) = explode(",", $k);
        $x = intval($x);
        $y = intval($y);
        $paint[$y - $miny][$x - $minx] = $v != 0 ? '#' : '.';
    }

    echo implode("\n", array_map(function ($row) {
        return implode("", $row);
    }, $paint));
}

function paint($hull)
{
    $painter = new Painter(readMemory("input"), $hull);
    $painter->loop();
    return $painter->hull();
}

function tasks() {
    $hull1 = paint([]);
    echo "Estimate: " . count($hull1) . "\n";

    $hull2 = paint(["0,0"=>1]);
    printHull($hull2);
}

tasks();
