def read_memory(file_name)
  str = IO.read(file_name).strip
  str.split(",").map { |t| t.to_i }
end

class Operation
  attr_accessor :op

  def initialize(op)
    @op = op % 100
    @modes = op / 100
  end

  def mode(i)
    @modes / (10 ** (i - 1)) % 10
  end
end

class State
  def initialize(ip, rb, mem, input, output)
    @ip = ip
    @rb = rb
    @mem = mem
    @input = input
    @output = output
  end

  def decode
    Operation.new(@mem[@ip])
  end

  def argument(op, i)
    a = @mem[@ip + i]
    mode = op.mode(i)
    case mode
    when 0
      @mem[a]
    when 1
      a
    when 2
      @mem[a + @rb]
    else
      raise "Unknown argument mode #{mode}"
    end
  end

  def input
    @input.call(self)
  end

  def dup
    #State.new(@ip, @rb, @mem.dup, @input, @output)
    self
  end

  def store(op, i, val)
    a = @mem[@ip + i]
    mode = op.mode(i)
    case mode
    when 0
      @mem[a] = val
    when 2
      @mem[a + @rb] = val
    else
      raise "Unknown store mode #{mode}"
    end
    self
  end

  def move_by(dip)
    @ip += dip
    self
  end

  def move_to(ip)
    @ip = ip
    self
  end

  def adjust_rb(drb)
    @rb += drb
    self
  end

  def output(val)
    @output.call(val)
    self
  end

  def halted?
    @ip < 0
  end

  def to_s
    "ip=#{@ip}, rb=#{@rb}, output=#{@output}, mem=#{@mem}"
  end

  def hack(i)
    @mem[i]
  end
end

class Processor
  def add(state, op)
    a1 = state.argument(op, 1)
    a2 = state.argument(op, 2)
    state.dup.store(op, 3, a1 + a2).move_by(4)
  end

  def mul(state, op)
    a1 = state.argument(op, 1)
    a2 = state.argument(op, 2)
    state.dup.store(op, 3, a1 * a2).move_by(4)
  end

  def inp(state, op)
    a1 = state.input
    if a1 == nil
      raise "No input"
    end
    state.dup.store(op, 1, a1).move_by(2)
  end

  def outp(state, op)
    a1 = state.argument(op, 1)
    state.dup.output(a1).move_by(2)
  end

  def jmpt(state, op)
    a1 = state.argument(op, 1)
    a2 = state.argument(op, 2)
    if a1 != 0
      state.dup.move_to(a2)
    else
      state.dup.move_by(3)
    end
  end

  def jmpf(state, op)
    a1 = state.argument(op, 1)
    a2 = state.argument(op, 2)
    if a1 == 0
      state.dup.move_to(a2)
    else
      state.dup.move_by(3)
    end
  end

  def lt(state, op)
    a1 = state.argument(op, 1)
    a2 = state.argument(op, 2)
    state.dup.store(op, 3, a1 < a2 ? 1 : 0).move_by(4)
  end

  def eq(state, op)
    a1 = state.argument(op, 1)
    a2 = state.argument(op, 2)
    state.dup.store(op, 3, a1 == a2 ? 1 : 0).move_by(4)
  end

  def arb(state, op)
    a1 = state.argument(op, 1)
    state.dup.adjust_rb(a1).move_by(2)
  end

  def halt(state, op)
    state.dup.move_to(-1)
  end

  def step(state, op)
    case op.op
    when 1
      add(state, op)
    when 2
      mul(state, op)
    when 3
      inp(state, op)
    when 4
      outp(state, op)
    when 5
      jmpt(state, op)
    when 6
      jmpf(state, op)
    when 7
      lt(state, op)
    when 8
      eq(state, op)
    when 9
      arb(state, op)
    when 99
      halt(state, op)
    else
      raise "Unknown operation #{op.op}"
    end
  end
end

class Computer
  attr_accessor :state, :output

  def initialize(mem, input)
    m = Hash.new(0)
    mem.each_with_index do |val, index|
      m[index] = val
    end

    @output = []

    @state = State.new(0, 0, m, input, lambda { |val| @output.push(val) })
    @processor = Processor.new
  end

  def step
    op = @state.decode
    @state = @processor.step(@state, op)
    self
  end

  def steps
    while !halted? && (!block_given? || (yield @state))
      step
    end
    self
  end

  def halted?
    @state.halted?
  end

  def to_s
    "halted=#{halted?}, state=#{@state}"
  end
end

TILES = [' ', '#', '@', '_', 'o']

def tile(output)
  minx = 1000
  maxx = -1000
  miny = 1000
  maxy = -1000
  output.each_slice(3) do |x, y|
    if x != -1
      minx = minx < x ? minx : x
      maxx = maxx > x ? maxx : x
      miny = miny < y ? miny : y
      maxy = maxy > y ? maxy : y
    end
  end

  a = Array.new(maxy - miny + 1) { Array.new(maxx - minx + 1) { ' ' } }

  output.each_slice(3) do |x, y, t|
    a[y - miny][x - minx] = TILES[t]
  end
  a.map { |aa| aa.join('') }.join("\n")
end

def score(output)
  s = -1
  output.each_slice(3) do |x, y, t|
    if x == -1 && t > s
      s = t
    end
  end
  s
end

def task1(mem)
  comp = Computer.new(mem, [])
  comp.steps { |x| true }

  t = tile comp.output
  puts t
  puts t.count("@")
end

def task2(mem)
  mem2 = mem.dup
  mem2[0] = 2

  pad = 20
  comp = Computer.new(mem2, lambda { |st|
    bx = st.hack(388)

    dx = 0
    if bx < pad
      pad -= 1
      dx = -1
    elsif bx == pad
      0
    else
      pad += 1
      dx = 1
    end
    dx
  })
  comp.steps { |x| true }

  puts tile(comp.output)
  puts score(comp.output)
end

mem = read_memory("input")

task1(mem)
task2(mem)
