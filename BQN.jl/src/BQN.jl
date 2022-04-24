module BQN
import TimerOutputs

const runto = TimerOutputs.TimerOutput()
const to = TimerOutputs.TimerOutput()
const xto = TimerOutputs.TimerOutput()

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
    resize!(v.refs, n)
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

abstract type BQNF end

struct F <: BQNF
  vm::VM
  frame::Frame
  block::Any
  𝕘::Union{Any,Nothing}
  𝕣::Union{Any,Nothing}
  𝕗::Union{Any,Nothing}
end

Base.show(io::IO, f::F) = show(io, "<BQN function>")

struct FN <: BQNF
  run::Function
  𝕘::Union{Any,Nothing}
  𝕣::Union{Any,Nothing}
  𝕗::Union{Any,Nothing}
end

Base.show(io::IO, f::FN) = show(io, "<BQN native function>")

struct TR2D <: BQNF
  h::Any
  𝕘::Any
end

struct TR3D <: BQNF
  h::Any
  𝕘::Any
  𝕗::Any
end

struct TR3O <: BQNF
  h::Any
  𝕘::Any
  𝕗::Any
end

struct M1N <: BQNF
  run::Function
end

struct M1I <: BQNF
  vm::VM
  frame::Frame
  block::Any
end

Base.show(io::IO, f::M1I) = show(io, "<BQN immediate 1-modifier>")

struct M1D <: BQNF
  vm::VM
  frame::Frame
  block::Any
end

Base.show(io::IO, f::M1D) = show(io, "<BQN deferred 1-modifier>")

struct M2N <: BQNF
  run::Function
end

struct M2I <: BQNF
  vm::VM
  frame::Frame
  block::Any
end

Base.show(io::IO, f::M2I) = show(io, "<BQN immediate 2-modifier>")

struct M2D <: BQNF
  vm::VM
  frame::Frame
  block::Any
end

Base.show(io::IO, f::M2D) = show(io, "<BQN deferred 2-modifier>")

type(𝕩::AbstractArray) = 0.0
type(𝕩::Number) = 1.0
type(𝕩::Char) = 2.0
type(𝕩::Function) = 3.0
type(𝕩::TR2D) = 3.0
type(𝕩::TR3D) = 3.0
type(𝕩::TR3O) = 3.0
type(𝕩::F) = 3.0
type(𝕩::FN) = 3.0
type(𝕩::M1N) = 4.0
type(𝕩::M1D) = 4.0
type(𝕩::M1I) = 4.0
type(𝕩::M2N) = 5.0
type(𝕩::M2D) = 5.0
type(𝕩::M2I) = 5.0

@nospecialize
(𝕤::AbstractArray)(𝕨, 𝕩) = 𝕤
(𝕤::Float64)(𝕨, 𝕩) = 𝕤
(𝕤::Int)(𝕨, 𝕩) = 𝕤
(𝕤::Char)(𝕨, 𝕩) = 𝕤
(𝕤::Bool)(𝕨, 𝕩) = 𝕤
(𝕤::String)(𝕨, 𝕩) = 𝕤
(𝕤::F)(𝕨, 𝕩) = run_block_body(𝕤.vm, 𝕤.frame, 𝕤.block,
                              Args(𝕤, 𝕨, 𝕩, 𝕤.𝕘, 𝕤.𝕗))
(𝕤::FN)(𝕨, 𝕩) = 𝕤.run(𝕨, 𝕩)
(𝕤::M1N)(𝕘::Nothing, 𝕗) = 𝕤.run(𝕘, 𝕗)
(𝕤::M1I)(𝕨, 𝕩) = run_block_body(𝕤.vm, 𝕤.frame, 𝕤.block,
                                Args(𝕤, 𝕨, 𝕩, nothing, nothing))
(𝕣::M1D)(𝕘, 𝕗) = F(𝕣.vm, 𝕣.frame, 𝕣.block, 𝕘, 𝕣, 𝕗)
(𝕤::M2N)(𝕘, 𝕗) = 𝕤.run(𝕘, 𝕗)
(𝕤::M2I)(𝕨, 𝕩) = run_block_body(𝕤.vm, 𝕤.frame, 𝕤.block,
                                Args(𝕤, 𝕨, 𝕩, nothing, nothing))
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
@specialize

function ok(@nospecialize(v))
  @assert (
           v isa Array && all(ok(x) for x in v) ||
           v isa Char         ||
           v isa Float64      ||
           v isa Refs.BaseRef ||
           v isa BQNF         ||
           v isa None         ||
           v isa Function
          ) typeof(v)
  true
end

function pushstack!(stack::Vector{Any}, @nospecialize(v))
  # ok(v)
  push!(stack, v)
end

function run_code(vm::VM, frame::Frame, pc::Int64)
  stack = []
  while true
    instr = vm.code[pc + 1]
    if instr == 0x00 # PUSH
      pc += 1
      @inbounds v = vm.consts[vm.code[pc + 1] + 1]
      pushstack!(stack, v)
    elseif instr == 0x01 # DFND
      pc += 1
      @inbounds block = vm.blocks[vm.code[pc + 1] + 1]
      pushstack!(stack, run_block(vm, frame, block))
    elseif instr == 0x06 # POPS
      pop!(stack)
    elseif instr == 0x07 # RETN
      return pop!(stack)
    elseif instr == 0x0B # ARRO
      pc += 1
      @inbounds n = Int(vm.code[pc + 1])
      v = []
      resize!(v, n)
      for i in 1:n
        @inbounds v[i] = popat!(stack, length(stack) - n + i)
      end
      pushstack!(stack, v)
    elseif instr == 0x0C # ARRM
      pc += 1
      @inbounds n = Int(vm.code[pc + 1])
      v = Refs.RefList(n)
      for i in 1:n
        @inbounds v.refs[i] = popat!(stack, length(stack) - n + i)
      end
      pushstack!(stack, v)
    elseif instr == 0x10 # FN1C
      len = length(stack)
      @inbounds s = stack[len]
      @inbounds x = stack[len - 1]
      # @info s
      resize!(stack, len - 2)
      v = s(none, x)
      pushstack!(stack, v)
    elseif instr == 0x11 # FN2C
      len = length(stack)
      @inbounds w = stack[len]
      @inbounds s = stack[len - 1]
      @inbounds x = stack[len - 2]
      # @info s
      resize!(stack, len - 3)
      v = s(w, x)
      pushstack!(stack, v)
    elseif instr == 0x12 # FN1O
      len = length(stack)
      @inbounds s = stack[len]
      @inbounds x = stack[len - 1]
      # @info s
      resize!(stack, len - 2)
      if x !== none
        v = s(none, x)
        pushstack!(stack, v)
      else
        pushstack!(stack, none)
      end
    elseif instr == 0x13 # FN2O
      len = length(stack)
      @inbounds w = stack[len]
      @inbounds s = stack[len - 1]
      @inbounds x = stack[len - 2]
      # @info "FN20" s w x
      resize!(stack, len - 3)
      if x !== none
        v = s(w, x)
        pushstack!(stack, v)
      else
        pushstack!(stack, none)
      end
    elseif instr == 0x14 # TR2D
      len = length(stack)
      @inbounds h = stack[len]
      @inbounds 𝕘 = stack[len - 1]
      resize!(stack, len - 2)
      pushstack!(stack, TR2D(h, 𝕘))
    elseif instr == 0x15 # TR3D
      len = length(stack)
      @inbounds 𝕘 = stack[len]
      @inbounds h = stack[len - 1]
      @inbounds 𝕗 = stack[len - 2]
      resize!(stack, len - 3)
      pushstack!(stack, TR3D(h, 𝕘, 𝕗))
    elseif instr == 0x17 # TR3O
      len = length(stack)
      @inbounds 𝕘 = stack[len]
      @inbounds h = stack[len - 1]
      @inbounds 𝕗 = stack[len - 2]
      resize!(stack, len - 3)
      pushstack!(stack, TR3O(h, 𝕘, 𝕗))
    elseif instr == 0x1A # MD1C
      len = length(stack)
      @inbounds f = stack[len]
      @inbounds r = stack[len - 1]
      resize!(stack, len - 2)
      pushstack!(stack, r(nothing, f))
    elseif instr == 0x1B # MD2C
      len = length(stack)
      @inbounds f = stack[len]
      @inbounds r = stack[len - 1]
      @inbounds g = stack[len - 2]
      resize!(stack, len - 3)
      pushstack!(stack, r(g, f))
    elseif instr == 0x20 # VARO
      pc += 1
      @inbounds d = vm.code[pc + 1]
      pc += 1
      @inbounds i = vm.code[pc + 1]
      cenv = frame
      while d > 0; cenv = cenv.parent; d -= 1 end
      @inbounds ref = cenv.vars[i + 1]
      pushstack!(stack, Refs.getv(ref))
    elseif instr == 0x21 # VARM
      pc += 1
      @inbounds d = vm.code[pc + 1]
      pc += 1
      @inbounds i = vm.code[pc + 1]
      cenv = frame
      while d > 0; cenv = cenv.parent; d -= 1 end
      @inbounds ref = cenv.vars[i + 1]
      pushstack!(stack, ref)
    elseif instr == 0x22 # VARU
      pc += 1
      @inbounds d = vm.code[pc + 1]
      pc += 1
      @inbounds i = vm.code[pc + 1]
      cenv = frame
      while d > 0; cenv = cenv.parent; d -= 1 end
      @inbounds ref = cenv.vars[i + 1]
      # TODO: need to clear the ref
      pushstack!(stack, Refs.getv(ref))
    elseif instr == 0x2C # NOTM
      pushstack!(stack, Refs.RefNot())
    elseif instr == 0x30 # SETN
      len = length(stack)
      @inbounds ref = stack[len]
      @inbounds value = stack[len - 1]
      resize!(stack, len - 2)
      Refs.setn!(ref, value)
      pushstack!(stack, value)
    elseif instr == 0x31 # SETU
      len = length(stack)
      @inbounds ref = stack[len]
      @inbounds value = stack[len - 1]
      resize!(stack, len - 2)
      Refs.setu!(ref, value)
      pushstack!(stack, value)
    elseif instr == 0x32 # SETM
      len = length(stack)
      @inbounds ref = stack[len]
      @inbounds 𝕗 = stack[len - 1]
      @inbounds 𝕩 = stack[len - 2]
      resize!(stack, len - 3)
      value = 𝕗(Refs.getv(ref), 𝕩)
      Refs.setu!(ref, value)
      pushstack!(stack, value)
    elseif instr == 0x33 # SETC
      len = length(stack)
      @inbounds ref = stack[len]
      @inbounds 𝕗 = stack[len - 1]
      resize!(stack, len - 2)
      value = 𝕗(none, Refs.getv(ref))
      Refs.setu!(ref, value)
      pushstack!(stack, value)
    else
      @error "UNKNOWN BYTECODE 0x$(string(instr, base=16))"
      @assert false
    end
    pc += 1
  end
end

struct Args
  𝕤::Any
  𝕨::Any
  𝕩::Any
  𝕘::Any
  𝕗::Any
end

function run_body(vm::VM, parent::Frame, body_idx::Int64, args::Args)
  @inbounds pc, num_vars = vm.bodies[body_idx + 1]
  inum_vars = Int(num_vars)
  vars = Refs.Ref[]
  resize!(vars, inum_vars)
  for i in 1:inum_vars; vars[i] = Refs.Ref(nothing) end
  if num_vars >= 1 vars[1].value = args.𝕤 end
  if num_vars >= 2 vars[2].value = args.𝕩 end
  if num_vars >= 3 vars[3].value = args.𝕨 end
  # TODO: handle 𝕣
  # if num_vars >= 4 vars[4].value = 𝕣 end
  if num_vars >= 5 vars[5].value = args.𝕗 end
  if num_vars >= 6 vars[6].value = args.𝕘 end
  frame = Frame(parent, vars)
  run_code(vm, frame, Int(pc))
end

function run_block_body(vm::VM, frame::Frame, @nospecialize(block), args::Args)
  body_idx = block[3]
  if isa(body_idx, Number)
    run_body(vm, frame, Int(body_idx), args)
  elseif isa(body_idx, AbstractArray)
    ret = nothing
    for body in body_idx
      for idx in body
        # TODO: need to check for PRED/SETH failures here
        ret = run_body(vm, frame, Int(idx), args)
      end
    end
    @assert ret !== nothing
    ret
  end
end

function run_block(vm::VM, frame::Frame, @nospecialize(block))
  typ, imm = block
  if typ == 0 && imm == 1 # immediate
    run_block_body(vm, frame, block,
                   Args(nothing, nothing, nothing, nothing, nothing))
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
  return eval(jlcode)
end

""" Compile and run the BQN expression (using bootstrap compiler)."""
function bqn0(src)
  code, consts, blocks, bodies = compile0(src)
  # @time run(code, boot...)
  run(src, code, consts, blocks, bodies)
end

str(s::String) = collect(s)

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

module Fmt
import ..runtime, ..str, ..run
include("./f.jl")

makefmt = run("<none>", value...)

import ..none, ..None, ..Provide, ..Runtime

function glyph(𝕨::None, @nospecialize(𝕩))
  idx = get(Runtime._runtime_indices, 𝕩, nothing)
  idx !== nothing ? Runtime.names[idx+1].first[1] : 'f'
end

fmtnum(𝕨, 𝕩) =
  collect(string(𝕩))

fmt0, repr =
  makefmt(none, [Provide.bqntype, Runtime.decompose, glyph, fmtnum])
fmt(@nospecialize(v)) =
  join(x == 0.0 ? ' ' : x for x in fmt0(none, v))

end

fmt = Fmt.fmt


""" Compile BQN expression using self-hosted compiler."""
function compile(src)
  c(Runtime.value, str(src))
end

""" Compile and eval BQN expression (using self-hosted compiler)."""
function bqn(src)
  code, consts, blocks, bodies, toks, names = compile(src)
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

import ..fmt
import ..bqn
# import ..bqn0 as bqn

function init()
  show_function(io::IO, mime::MIME"text/plain", x) = print(io, fmt(x))
  initrepl(bqn,
           prompt_text="BQN) ",
           prompt_color=:blue, 
           show_function=show_function,
           startup_text=true,
           start_key=')', 
           mode_name="BQN")
  nothing
end
end

""" Reset all performance timers."""
function reset_timers!()
  TimerOutputs.reset_timer!(to)
  TimerOutputs.reset_timer!(xto)
  TimerOutputs.reset_timer!(runto)
  nothing
end

""" Enable performance timers."""
function enable_timers!()
  TimerOutputs.enable_debug_timings(BQN)
  TimerOutputs.enable_debug_timings(BQN.Provide)
  TimerOutputs.enable_debug_timings(BQN.Runtime0)
  TimerOutputs.enable_debug_timings(BQN.Runtime)
  nothing
end

macro bqn_str(src); bqn(src) end
macro bqn0_str(src); bqn0(src) end

export bqn, bqn0, fmt
export @bqn_str, @bqn0_str

end
