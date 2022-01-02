module BQN
using TimerOutputs

const to = TimerOutput()

""" BQN error."""
struct BQNError <: Exception
  msg::String
end

""" A special value designating an absence of an argument."""
struct None end
const none = None()

module Refs

abstract type BaseRef end

""" Reference which cannot hold any value."""
struct RefNot <: BaseRef end

Base.show(io::IO, r::RefNot) = print(io, "BQN.RefNot")

""" Reference which can hold a single value."""
mutable struct Ref <: BaseRef
  value::Union{Any,Nothing}
end

Base.show(io::IO, r::Ref) = print(io, "BQN.Ref")

""" A list of references."""
struct RefList <: BaseRef
  refs::Vector{BaseRef}
  function RefList(n::Int64)
    v = new(Vector{BaseRef}())
    sizehint!(v.refs, n)
    v
  end
end

Base.show(io::IO, rs::RefList) = show(io, rs.refs)

""" Get a value out of a reference."""
getv(ref::BaseRef) = @assert false

function getv(ref::Ref)
  @assert ref.value !== nothing
  ref.value
end

function getv(ref::RefList)
  map(getv, ref.refs)
end

""" Set an initial value to a reference."""
setn!(ref::BaseRef) = @assert false

function setn!(ref::Ref, value::Any)
  @assert ref.value == nothing
  ref.value = value
end

function setn!(ref::RefList, value::AbstractArray)
  @assert length(ref.refs) == length(value)
  for (refitem, valueitem) in zip(ref.refs, value)
    setn!(refitem, valueitem)
  end
end

function setn!(ref::RefNot, value::Any) end

""" Update a reference value."""
setu!(ref::BaseRef) = @assert false

function setu!(ref::Ref, value::Any)
  @assert ref.value != nothing
  ref.value = value
end

function setu!(ref::RefList, value::AbstractArray)
  @assert length(ref.refs) == length(value)
  for (refitem, valueitem) in zip(ref.refs, value)
    setu!(refitem, valueitem)
  end
end

function setu!(ref::RefNot, value::Any) end

end

struct Frame
  parent::Union{Frame,Nothing}
  vars::Vector{Refs.Ref}
end

struct VM
  src::String
  code::Array{Int64}
  consts::Array{Any}
  blocks::Array{Any}
  bodies::Array{Any}
end

struct F
  vm::VM
  frame::Frame
  block::Any
  𝕘::Union{Any,Nothing}
  𝕣::Union{Any,Nothing}
  𝕗::Union{Any,Nothing}
end

Base.show(io::IO, f::F) = show(io, "<BQN function>")

struct FN
  run::Function
  𝕘::Union{Any,Nothing}
  𝕣::Union{Any,Nothing}
  𝕗::Union{Any,Nothing}
end

Base.show(io::IO, f::FN) = show(io, "<BQN native function>")

struct TR2D
  h::Any
  𝕘::Any
end

struct TR3D
  h::Any
  𝕘::Any
  𝕗::Any
end

struct TR3O
  h::Any
  𝕘::Any
  𝕗::Any
end

struct M1I
  vm::VM
  frame::Frame
  block::Any
end

Base.show(io::IO, f::M1I) = show(io, "<BQN immediate 1-modifier>")

struct M1D
  vm::VM
  frame::Frame
  block::Any
end

Base.show(io::IO, f::M1D) = show(io, "<BQN deferred 1-modifier>")

struct M2I
  vm::VM
  frame::Frame
  block::Any
end

Base.show(io::IO, f::M2I) = show(io, "<BQN immediate 2-modifier>")

struct M2D
  vm::VM
  frame::Frame
  block::Any
end

Base.show(io::IO, f::M2D) = show(io, "<BQN deferred 2-modifier>")


(𝕤::AbstractArray)(𝕨, 𝕩) = 𝕤
(𝕤::Float64)(𝕨, 𝕩) = 𝕤
(𝕤::Int)(𝕨, 𝕩) = 𝕤
(𝕤::Char)(𝕨, 𝕩) = 𝕤
(𝕤::Bool)(𝕨, 𝕩) = 𝕤
(𝕤::String)(𝕨, 𝕩) = 𝕤
(𝕤::F)(𝕨, 𝕩) = run_block_body(𝕤.vm, 𝕤.frame, 𝕤.block, 𝕤, 𝕨, 𝕩, 𝕤.𝕘, 𝕤.𝕗)
(𝕤::FN)(𝕨, 𝕩) = 𝕤.run(𝕨, 𝕩)
(𝕤::M1I)(𝕨, 𝕩) = run_block_body(𝕤.vm, 𝕤.frame, 𝕤.block, 𝕤, 𝕨, 𝕩, nothing, nothing)
(𝕣::M1D)(𝕘, 𝕗) = F(𝕣.vm, 𝕣.frame, 𝕣.block, 𝕘, 𝕣, 𝕗)
(𝕤::M2I)(𝕨, 𝕩) = run_block_body(𝕤.vm, 𝕤.frame, 𝕤.block, 𝕤, 𝕨, 𝕩, nothing, nothing)
(𝕣::M2D)(𝕘, 𝕗) = F(𝕣.vm, 𝕣.frame, 𝕣.block, 𝕘, 𝕣, 𝕗)
(𝕤::TR2D)(𝕨, 𝕩) = 𝕤.h(none, 𝕤.𝕘(𝕨, 𝕩))
function (𝕤::TR3D)(𝕨, 𝕩)
  𝕩´ = 𝕤.𝕗(𝕨, 𝕩)
  𝕨´ = 𝕤.𝕘(𝕨, 𝕩)
  𝕤.h(𝕨´, 𝕩´)
end
function (𝕤::TR3O)(𝕨, 𝕩)
  𝕩´ = 𝕤.𝕗(𝕨, 𝕩)
  𝕨´ = 𝕤.𝕘 != none ? 𝕤.𝕘(𝕨, 𝕩) : none
  𝕤.h(𝕨´, 𝕩´)
end

function run_code(vm::VM, frame::Frame, pc::Int64)
  stack = []
  while true
    instr = vm.code[pc + 1]
    if instr == 0x00 # PUSH
      @timeit_debug to "PUSH" begin
      pc += 1
      v = vm.consts[vm.code[pc + 1] + 1]
      push!(stack, v)
      end
    elseif instr == 0x01 # DFND
      @timeit_debug to "DFND" begin
      pc += 1
      block = vm.blocks[vm.code[pc + 1] + 1]
      push!(stack, run_block(vm, frame, block))
      end
    elseif instr == 0x06 # POPS
      @timeit_debug to "POPS" begin
      pop!(stack)
      end
    elseif instr == 0x07 # RETN
      @timeit_debug to "RETN" begin
      return pop!(stack)
      end
    elseif instr == 0x0B # ARRO
      @timeit_debug to "ARRO" begin
      pc += 1
      n = vm.code[pc + 1]
      # try to "infer" the type
      # TODO: benchmark if it helps...
      T = if n > 0
        len = length(stack)
        T = typeof(stack[len])
        for i in 1:(n-1)
          T′ = typeof(stack[Int(len - i)])
          if T != T′; T = Any; break end
        end
        T
      else
        Any
      end
      # alloc storage
      v = T[]
      sizehint!(v, Int(n))
      for i in 1:n
        push!(v, popat!(stack, Int(length(stack) - n + i)))
      end
      push!(stack, v)
      end
    elseif instr == 0x0C # ARRM
      @timeit_debug to "ARRM" begin
      pc += 1
      n = vm.code[pc + 1]
      v = Refs.RefList(Int(n))
      for i in 1:n
        push!(v.refs, popat!(stack, Int(length(stack) - n + i)))
      end
      push!(stack, v)
      end
    elseif instr == 0x10 # FN1C
      @timeit_debug to "FN1C" begin
      s, x = pop!(stack), pop!(stack)
      v = s(none, x)
      push!(stack, v)
      end
    elseif instr == 0x11 # FN2C
      @timeit_debug to "FN2C" begin
      w, s, x = pop!(stack), pop!(stack), pop!(stack)
      v = s(w, x)
      push!(stack, v)
      end
    elseif instr == 0x12 # FN1O
      @timeit_debug to "FN10" begin
      s, x = pop!(stack), pop!(stack)
      if x !== none
        v = s(none, x)
        push!(stack, v)
      else
        push!(stack, none)
      end
      end
    elseif instr == 0x13 # FN2O
      @timeit_debug to "FN20" begin
      w, s, x = pop!(stack), pop!(stack), pop!(stack)
      if x !== none
        v = s(w, x)
        push!(stack, v)
      else
        push!(stack, none)
      end
      end
    elseif instr == 0x14 # TR2D
      @timeit_debug to "TR2D" begin
      h, 𝕘 = pop!(stack), pop!(stack)
      push!(stack, TR2D(h, 𝕘))
      end
    elseif instr == 0x15 # TR3D
      @timeit_debug to "TR3D" begin
      𝕘, h, 𝕗 = pop!(stack), pop!(stack), pop!(stack)
      push!(stack, TR3D(h, 𝕘, 𝕗))
      end
    elseif instr == 0x17 # TR3O
      @timeit_debug to "TR3O" begin
      𝕘, h, 𝕗 = pop!(stack), pop!(stack), pop!(stack)
      push!(stack, TR3O(h, 𝕘, 𝕗))
      end
    elseif instr == 0x1A # MD1C
      @timeit_debug to "MD1C" begin
      f, r = pop!(stack), pop!(stack)
      push!(stack, r(nothing, f))
      end
    elseif instr == 0x1B # MD2C
      @timeit_debug to "MD2C" begin
      f, r, g = pop!(stack), pop!(stack), pop!(stack)
      push!(stack, r(g, f))
      end
    elseif instr == 0x20 # VARO
      @timeit_debug to "VARO" begin
      pc += 1
      d = vm.code[pc + 1]
      pc += 1
      i = vm.code[pc + 1]
      cenv = frame
      while d > 0; cenv = cenv.parent; d -= 1 end
      ref = cenv.vars[i + 1]
      push!(stack, Refs.getv(ref))
      end
    elseif instr == 0x21 # VARM
      @timeit_debug to "VARM" begin
      pc += 1
      d = vm.code[pc + 1]
      pc += 1
      i = vm.code[pc + 1]
      cenv = frame
      while d > 0; cenv = cenv.parent; d -= 1 end
      ref = cenv.vars[i + 1]
      push!(stack, ref)
      end
    elseif instr == 0x22 # VARU
      @timeit_debug to "VARU" begin
      pc += 1
      d = vm.code[pc + 1]
      pc += 1
      i = vm.code[pc + 1]
      cenv = frame
      while d > 0; cenv = cenv.parent; d -= 1 end
      ref = cenv.vars[i + 1]
      # TODO: need to clear the ref
      push!(stack, Refs.getv(ref))
      end
    elseif instr == 0x2C # NOTM
      @timeit_debug to "NOTM" begin
      push!(stack, Refs.RefNot())
      end
    elseif instr == 0x30 # SETN
      @timeit_debug to "SETN" begin
      ref, value = pop!(stack), pop!(stack)
      Refs.setn!(ref, value)
      push!(stack, value)
      end
    elseif instr == 0x31 # SETU
      @timeit_debug to "SETU" begin
      ref, value = pop!(stack), pop!(stack)
      Refs.setu!(ref, value)
      push!(stack, value)
      end
    elseif instr == 0x32 # SETM
      @timeit_debug to "SETM" begin
      ref, 𝕗, 𝕩 = pop!(stack), pop!(stack), pop!(stack)
      value = 𝕗(Refs.getv(ref), 𝕩)
      Refs.setu!(ref, value)
      push!(stack, value)
      end
    elseif instr == 0x33 # SETC
      @timeit_debug to "SETC" begin
      ref, 𝕗 = pop!(stack), pop!(stack)
      value = 𝕗(none, Refs.getv(ref))
      Refs.setu!(ref, value)
      push!(stack, value)
      end
    else
      @error "UNKNOWN BYTECODE 0x$(string(instr, base=16))"
      @assert false
    end
    pc += 1
  end
end

function run_body(vm::VM, parent::Frame, body_idx::Int64, 𝕤, 𝕨, 𝕩, 𝕘, 𝕗)
  pc, num_vars = vm.bodies[body_idx + 1]
  vars = Refs.Ref[]
  sizehint!(vars, num_vars)
  for _ in 1:num_vars; push!(vars, Refs.Ref(nothing)) end
  if num_vars >= 1 vars[1].value = 𝕤 end
  if num_vars >= 2 vars[2].value = 𝕩 end
  if num_vars >= 3 vars[3].value = 𝕨 end
  # TODO: handle 𝕣
  # if num_vars >= 4 vars[4].value = 𝕣 end
  if num_vars >= 5 vars[5].value = 𝕗 end
  if num_vars >= 6 vars[6].value = 𝕘 end
  frame = Frame(parent, vars)
  @timeit_debug to string("run_code", body_idx) run_code(vm, frame, pc)
end

function run_block_body(vm::VM, frame::Frame, block, 𝕤, 𝕨, 𝕩, 𝕘, 𝕗)
  body_idx = block[3]
  if isa(body_idx, Int)
    run_body(vm, frame, body_idx, 𝕤, 𝕨, 𝕩, 𝕘, 𝕗)
  elseif isa(body_idx, AbstractArray)
    ret = nothing
    for body in body_idx
      for idx in body
        # TODO: need to check for PRED/SETH failures here
        ret = run_body(vm, frame, idx, 𝕤, 𝕨, 𝕩, 𝕘, 𝕗)
      end
    end
    @assert ret !== nothing
    ret
  end
end

function run_block(vm::VM, frame::Frame, block)
  typ, imm = block
  if typ == 0 && imm == 1 # immediate
    run_block_body(vm, frame, block, nothing, nothing, nothing, nothing, nothing)
  elseif typ == 0 && imm == 0 # function
    F(vm, frame, block, nothing, nothing, nothing)
  elseif typ == 1 && imm == 1 # mod1 immediate
    M1I(vm, frame, block)
  elseif typ == 2 && imm == 1 # mod2 immediate
    M2I(vm, frame, block)
  elseif typ == 1 && imm == 0 # mod1 deferred
    M1D(vm, frame, block)
  elseif typ == 2 && imm == 0 # mod2 deferred
    M2D(vm, frame, block)
  end
end

""" Run compiler bytecode, this is the entry point to VM."""
function run(src, code, consts, blocks, bodies)
  vm = VM(src, code, consts, blocks, bodies)
  frame = Frame(nothing, [])
  run_block(vm, frame, blocks[1])
end

""" Compile the BQN expression using bootstrap compiler."""
function compile0(code)
  jlsrc = read(`./cjl.bqn $(code)`, String)
  jlcode = eval(Meta.parse(jlsrc))
  return jlcode
end

""" Compile and run the BQN expression (using bootstrap compiler)."""
function bqn0(code)
  jlcode = compile0(code)
  boot = eval(jlcode)
  # @time run(code, boot...)
  run(code, boot...)
end

str(s::String) = s

include("./provide.jl")
using .Provide

include("./runtime0.jl")
using .Runtime0

include("./runtime.jl")
using .Runtime

module C
import ..runtime, ..str
include("./c.jl")
end

c = run("<none>", C.value...)

""" Compile BQN expression using self-hosted compiler."""
function compile(src)
  c(_runtime, src)
end

""" Compile and eval BQN expression (using self-hosted compiler)."""
function bqn(src)
  code, consts, blocks, bodies, toks, names = bqncompile(src)
  run(src, code, consts, blocks, bodies)
end

""" Test suite using the bootstrap compiler."""
module Tests0
import ..BQNError, ..bqn0 as bqn
include("./test/test.jl")
end

""" Test suite using the self hosted compiler."""
module Tests
import ..BQNError, ..bqn
include("./test/test.jl")
end

""" REPL mode."""
module Repl
using ReplMaker

# TODO: now using the bootstrap compiler, switch to bqn once self-hosted
# compiler is fast enough.
import ..bqn0 as bqn

function init()
  initrepl(bqn,
           prompt_text="BQN) ",
           prompt_color=:blue, 
           startup_text=true,
           start_key=')', 
           mode_name="BQN")
  nothing
end
end

export bqn, bqn0

end
