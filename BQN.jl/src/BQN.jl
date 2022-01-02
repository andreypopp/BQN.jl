module BQN
using Logging
using Debugger

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

struct M1
  run::Function
end

Base.show(io::IO, f::M1) = show(io, "<BQN 1-modifier>")

struct M2
  run::Function
end

Base.show(io::IO, f::M2) = show(io, "<BQN 2-modifier>")

(𝕤::AbstractArray)(𝕨, 𝕩) = 𝕤
(𝕤::Float64)(𝕨, 𝕩) = 𝕤
(𝕤::Int)(𝕨, 𝕩) = 𝕤
(𝕤::Char)(𝕨, 𝕩) = 𝕤
(𝕤::Bool)(𝕨, 𝕩) = 𝕤
(𝕤::String)(𝕨, 𝕩) = 𝕤
(𝕤::F)(𝕨, 𝕩) = run_block_body(𝕤.vm, 𝕤.frame, 𝕤.block, 𝕤, 𝕨, 𝕩, 𝕤.𝕘, 𝕤.𝕗)
(𝕤::M1)(𝕨, 𝕩) = 𝕤.run(𝕨, 𝕩)
(𝕤::M2)(𝕨, 𝕩) = 𝕤.run(𝕨, 𝕩)
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

module Runtime
  using Debugger
  import ..None, ..none, ..F, ..TR2D, ..TR3D, ..TR3O, ..M1, ..M2, ..BQNError

  bqnadd(𝕨::None, 𝕩) = 𝕩
  bqnadd(𝕨, 𝕩) = 𝕨 + 𝕩

  bqnsub(𝕨::None, 𝕩::Number) = -𝕩
  bqnsub(𝕨, 𝕩) = 𝕨 - 𝕩

  bqnmul(𝕨::None, 𝕩::Number) = sign(𝕩)
  bqnmul(𝕨::Number, 𝕩::Number) = 𝕨 * 𝕩

  bqndiv(𝕨::None, 𝕩::Number) = 1/𝕩
  bqndiv(𝕨::Number, 𝕩::Number) = 𝕨/𝕩

  bqnpow(𝕨::None, 𝕩::Number) = ℯ^𝕩
  bqnpow(𝕨::Number, 𝕩::Number) = 𝕨^𝕩

  bqnroot(root::None, v) = sqrt(v)
  bqnroot(root, v) = v^(1/root)

  bqnabs(𝕨::None, v) = abs(v)

  bqnmin(𝕨::Int64, 𝕩::Number) = min(𝕨, 𝕩)
  bqnmin(𝕨::None, 𝕩::Number) = floor(𝕩)

  bqnnot(𝕨::None, 𝕩::Number) = +(1 - 𝕩)
  bqnnot(𝕨::Number, 𝕩::Number) = 1 + (𝕨 - 𝕩)

  bqnand(𝕨::Number, 𝕩::Number) = 𝕨*𝕩

  bqnor(𝕨::Number, 𝕩::Number) = (𝕨+𝕩)-(𝕨*𝕩)

  bqnidleft(𝕨, 𝕩) = 𝕨

  bqnidright(𝕨, 𝕩) = 𝕩

  function bqnvalences(𝕘, 𝕗)
    function (𝕨, 𝕩)
      if 𝕨 === none
        𝕗(𝕨, 𝕩)
      else
        𝕘(𝕨, 𝕩)
      end
    end
  end

  function bqncatch(𝕘, 𝕗)
    function (𝕨, 𝕩)
      try
        𝕗(𝕨, 𝕩)
      catch e
        𝕘(𝕨, 𝕩)
      end
    end
  end

  bqneq(𝕨::None, 𝕩::AbstractArray) = ndims(𝕩)
  bqneq(𝕨::None, 𝕩::String) = 1
  bqneq(𝕨::None, 𝕩) = 0
  bqneq(𝕨, 𝕩) = Int(𝕨 == 𝕩)

  bqnlte(𝕨, 𝕩) = Int(𝕨 <= 𝕩)
  bqnlte(𝕨::Number, 𝕩::Char) = 1
  bqnlte(𝕨::Char, 𝕩::Number) = 0

  bqnshape(𝕨, 𝕩::AbstractArray) = reverse([x for x in size(𝕩)])
  bqnshape(𝕨, 𝕩::String) = Int[length(𝕩)]
  bqnshape(𝕨, 𝕩) = []

  bqndeshape(𝕨::None, 𝕩::AbstractArray) = vec(𝕩)
  bqndeshape(𝕨::None, 𝕩::String) = 𝕩
  bqndeshape(𝕨::None, 𝕩) = [𝕩]

  function bqndeshape(𝕨::AbstractArray, 𝕩::AbstractArray)
    size = reverse(Tuple(Int(x) for x in 𝕨))
    if size == Base.size(𝕩); return 𝕩 end
    reshape(𝕩, size)
  end

  function bqndeshape(𝕨::AbstractArray, 𝕩::String)
    𝕩 = collect(𝕩)
    bqndeshape(𝕨, 𝕩)
  end
        
  function bqndeshape(𝕨::AbstractArray, 𝕩::Any)
    @assert length(𝕨) == 0
    collect(𝕩)
  end

  bqnpick(𝕨::Number, 𝕩::Number) = 𝕩
  bqnpick(𝕨::Float64, 𝕩::AbstractArray) = bqnpick(Int(𝕨), 𝕩)
  function bqnpick(𝕨::Int64, 𝕩::AbstractArray)
    if 𝕨 >= 0; 𝕩[𝕨 + 1] else 𝕩[end + (𝕨 + 1)] end
  end
  bqnpick(𝕨::None, 𝕩::AbstractArray) = bqnpick(0, 𝕩)
  # TODO: get rid of collect, this is slow!
  bqnpick(𝕨::Number, 𝕩::String) = bqnpick(𝕨, collect(𝕩))
  bqnpick(𝕨::None, 𝕩::String) = bqnpick(0, collect(𝕩))
  bqnpick(𝕨::None, 𝕩) = 𝕩

  bqnwindow(𝕨, 𝕩) = [x for x in 0:(𝕩-1)]

  function bqntable(𝕘, 𝕗)
    # TODO: need to get rid of calls to collect() here, instead need to iterate
    # over graphemes for Strings
    function(𝕨, 𝕩)
      res =
        if 𝕨 === none
          𝕩 = if !isa(𝕩, AbstractArray); collect(𝕩) else 𝕩 end
          [𝕗(none, x) for x in 𝕩]
        else
          𝕨 = if !isa(𝕨, AbstractArray); collect(𝕨) else 𝕨 end
          𝕩 = if !isa(𝕩, AbstractArray); collect(𝕩) else 𝕩 end
          rsize = (size(𝕩)..., size(𝕨)...)
          r = [𝕗(w, x) for w in 𝕨 for x in 𝕩]
          reshape(r, rsize)
        end
      res
    end
  end

  function bqnscan(𝕘, 𝕗)
    function(𝕨, 𝕩::AbstractArray)
      bqnassert(
                "`: Argument cannot have rank 0",
                Int(ndims(𝕩) != 0))
      bqnassert(
                "`: Shape of 𝕨 must match the cell of 𝕩",
                Int(𝕨 == none ||
                    size(𝕨) == () && ndims(𝕩) == 1 ||
                    size(𝕨)[1:1] == size(𝕩)[1:1]))
      # @debug "PRIMITIVE bqnscan"
      storage = if 𝕨 == none
        accumulate(𝕗, 𝕩, dims=ndims(𝕩))
      elseif size(𝕨) == ()
        accumulate(𝕗, 𝕩, dims=ndims(𝕩), init=𝕨)
      else
        # Because accumulate() doesn't support init being an array we provide
        # init value by concatenating it over the major dimension with hvncat():
        storage = hvncat(ndims(𝕩), 𝕨, 𝕩)
        storage = accumulate(𝕗, storage, dims=ndims(𝕩))
        # ... but this will produce an extra "row" in this dimension so we
        # produce a view which "cuts" that out with a view over this array:
        # TODO: Revisit that for performance!
        indices = [(:) for _ in size(storage)[1:end - 1]]
        storage = @view storage[indices..., 2:end]
        storage
      end
      storage
    end
  end

  function bqntype(𝕨::None, 𝕩)
    type = bqntype′(𝕨, 𝕩)
    # @info "bqntype" 𝕩 type
    type
  end
  bqntype′(𝕨::None, 𝕩::AbstractArray) = 0
  bqntype′(𝕨::None, 𝕩::String) = 0
  bqntype′(𝕨::None, 𝕩::Number) = 1
  bqntype′(𝕨::None, 𝕩::Char) = 2
  bqntype′(𝕨::None, 𝕩::Function) = 3
  bqntype′(𝕨::None, 𝕩::TR2D) = 3
  bqntype′(𝕨::None, 𝕩::TR3D) = 3
  bqntype′(𝕨::None, 𝕩::TR3O) = 3
  bqntype′(𝕨::None, 𝕩::F) = 3
  bqntype′(𝕨::None, 𝕩::M1) = 4
  bqntype′(𝕨::None, 𝕩::M2) = 5

  bqnfill(𝕨::None, 𝕩::String) = ' '
  bqnfill(𝕨::None, 𝕩::AbstractArray) = 0
  bqnfill(𝕨, 𝕩) = 𝕩

  bqnlog(𝕨::None, 𝕩::Number) = log(ℯ, 𝕩)
  bqnlog(𝕨::Number, 𝕩::Number) = log(𝕨, 𝕩)

  function bqngrouplen(𝕨, 𝕩::AbstractArray)
    order = []
    lengths = Dict{Int,Int}()
    max𝕩 = -1
    for x in 𝕩
      max𝕩 = max(max𝕩, x)
      if haskey(lengths, x)
        lengths[Int(x)] += 1
      else
        lengths[Int(x)] = 1
        push!(order, x)
      end
    end
    minl = max(max𝕩, 𝕨 !== none ? (𝕨 - 1) : -1)
    [get(lengths, x, 0) for x in 0:minl]
  end

  function bqngroupord(𝕨, 𝕩::AbstractArray)
    indices = [[] for _ in 1:length(𝕨)]
    for (idx, x) in enumerate(𝕩)
      if x < 0; continue end
      push!(indices[Int(x) + 1], idx - 1)
    end
    vcat(indices...)
  end

  function bqnassert(𝕨, 𝕩)
    if 𝕩 == 1
      1
    else
      # TODO: should we use 𝕩 as error message in case it's a string? r1.bqn
      # seems to be relying on that behaviour... see !∘"msg" pattern.
      msg = 𝕨 === none ? (isa(𝕩, String) ? 𝕩 : "ERROR") : 𝕨
      if isa(msg, AbstractArray); msg = join(msg) end
      throw(BQNError(msg))
    end
  end

  function bqnfillby(𝕘, 𝕗)
    function(𝕨, 𝕩)
      𝕗(𝕨, 𝕩)
    end
  end
end

function run_code(vm::VM, frame::Frame, pc::Int64)
  stack = []
  while true
    instr = vm.code[pc + 1]
    if instr == 0x00 # PUSH
      pc += 1
      v = vm.consts[vm.code[pc + 1] + 1]
      # @info "BYTECODE 00 PUSH" v
      push!(stack, v)
    elseif instr == 0x01 # DFND
      # @debug "BYTECODE 01 DFND"
      pc += 1
      block = vm.blocks[vm.code[pc + 1] + 1]
      push!(stack, run_block(vm, frame, block))
    elseif instr == 0x06 # POPS
      # @debug "BYTECODE 06 POPS"
      pop!(stack)
    elseif instr == 0x07 # RETN
      # @info "BYTECODE 07 RETN" stack
      return pop!(stack)
    elseif instr == 0x0B # ARRO
      pc += 1
      n = vm.code[pc + 1]
      # @info "BYTECODE 0B ARRO N=$(n)"
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
    elseif instr == 0x0C # ARRM
      # @info "BYTECODE 1C ARRM"
      pc += 1
      n = vm.code[pc + 1]
      v = Refs.RefList(Int(n))
      for i in 1:n
        push!(v.refs, popat!(stack, Int(length(stack) - n + i)))
      end
      push!(stack, v)
    elseif instr == 0x10 # FN1C
      # @info "BYTECODE 10 FN1C"
      s, x = pop!(stack), pop!(stack)
      v = s(none, x)
      push!(stack, v)
    elseif instr == 0x11 # FN2C
      w, s, x = pop!(stack), pop!(stack), pop!(stack)
      v = s(w, x)
      # @info "BYTECODE 11 FN2C" w s x v s.𝕤
      push!(stack, v)
    elseif instr == 0x12 # FN1O
      # @debug "BYTECODE 12 FN1O"
      s, x = pop!(stack), pop!(stack)
      if x !== none
        v = s(none, x)
        push!(stack, v)
      else
        push!(stack, none)
      end
    elseif instr == 0x13 # FN2O
      w, s, x = pop!(stack), pop!(stack), pop!(stack)
      # @debug "BYTECODE 13 FN20"
      if x !== none
        v = s(w, x)
        push!(stack, v)
      else
        push!(stack, none)
      end
    elseif instr == 0x14 # TR2D
      # @debug "BYTECODE 14 TR2D"
      h, 𝕘 = pop!(stack), pop!(stack)
      push!(stack, TR2D(h, 𝕘))
    elseif instr == 0x15 # TR3D
      # @debug "BYTECODE 15 TR3D"
      𝕘, h, 𝕗 = pop!(stack), pop!(stack), pop!(stack)
      push!(stack, TR3D(h, 𝕘, 𝕗))
    elseif instr == 0x17 # TR3O
      # @debug "BYTECODE 17 TR3O"
      𝕘, h, 𝕗 = pop!(stack), pop!(stack), pop!(stack)
      push!(stack, TR3O(h, 𝕘, 𝕗))
    elseif instr == 0x1A # MD1C
      # @debug "BYTECODE 1A MD1C"
      f, r = pop!(stack), pop!(stack)
      push!(stack, r(nothing, f))
    elseif instr == 0x1B # MD2C
      # @debug "BYTECODE 1B MD2C"
      f, r, g = pop!(stack), pop!(stack), pop!(stack)
      push!(stack, r(g, f))
    elseif instr == 0x20 # VARO
      pc += 1
      d = vm.code[pc + 1]
      pc += 1
      i = vm.code[pc + 1]
      cenv = frame
      while d > 0; cenv = cenv.parent; d -= 1 end
      ref = cenv.vars[i + 1]
      # @info "BYTECODE 20 VARO D=$(d) I=$(i)" ref
      push!(stack, Refs.getv(ref))
    elseif instr == 0x21 # VARM
      pc += 1
      d = vm.code[pc + 1]
      pc += 1
      i = vm.code[pc + 1]
      # @info "BYTECODE 21 VARM D=$(d) I=$(i)"
      cenv = frame
      while d > 0; cenv = cenv.parent; d -= 1 end
      ref = cenv.vars[i + 1]
      push!(stack, ref)
    elseif instr == 0x22 # VARU
      pc += 1
      d = vm.code[pc + 1]
      pc += 1
      i = vm.code[pc + 1]
      cenv = frame
      while d > 0; cenv = cenv.parent; d -= 1 end
      ref = cenv.vars[i + 1]
      # @info "BYTECODE 22 VARU D=$(d) I=$(i)"
      # TODO: need to clear the ref
      # @info "BYTECODE 20 VARO D=$(d) I=$(i)" ref
      push!(stack, Refs.getv(ref))
    elseif instr == 0x2C # NOTM
      push!(stack, Refs.RefNot())
    elseif instr == 0x30 # SETN
      ref, value = pop!(stack), pop!(stack)
      # @debug "BYTECODE 30 SETN"
      Refs.setn!(ref, value)
      push!(stack, value)
    elseif instr == 0x31 # SETU
      ref, value = pop!(stack), pop!(stack)
      # @debug "BYTECODE 31 SETU"
      Refs.setu!(ref, value)
      push!(stack, value)
    elseif instr == 0x32 # SETM
      ref, 𝕗, 𝕩 = pop!(stack), pop!(stack), pop!(stack)
      # @debug "BYTECODE 32 SETM"
      value = 𝕗(Refs.getv(ref), 𝕩)
      Refs.setu!(ref, value)
      push!(stack, value)
    elseif instr == 0x33 # SETC
      ref, 𝕗 = pop!(stack), pop!(stack)
      # @debug "BYTECODE 33 SETC"
      value = 𝕗(none, Refs.getv(ref))
      Refs.setu!(ref, value)
      push!(stack, value)
    else
      @error "UNKNOWN BYTECODE 0x$(string(instr, base=16))"
      @assert false
    end
    pc += 1
  end
end

function run_body(vm::VM, parent::Frame, body_idx::Int64, 𝕤, 𝕨, 𝕩, 𝕘, 𝕗)
  pc, num_vars = vm.bodies[body_idx + 1]
  # @debug "BODY@$(idx-1) $(num_vars)"
  vars = Refs.Ref[]
  for _ in 1:num_vars; push!(vars, Refs.Ref(nothing)) end
  if num_vars >= 1 vars[1].value = 𝕤 end
  if num_vars >= 2 vars[2].value = 𝕩 end
  if num_vars >= 3 vars[3].value = 𝕨 end
  # TODO: handle 𝕣
  # if num_vars >= 4 vars[4].value = 𝕣 end
  if num_vars >= 5 vars[5].value = 𝕗 end
  if num_vars >= 6 vars[6].value = 𝕘 end
  frame = Frame(parent, vars)
  # @info "run_body"
  run_code(vm, frame, pc)
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
    # @info "mod1 immediate"
    𝕣 = M1(function(𝕨, 𝕩)
           run_block_body(vm, frame, block, 𝕣, 𝕨, 𝕩, nothing, nothing)
         end)
    𝕣
  elseif typ == 2 && imm == 1 # mod2 immediate
    𝕣 = M2(function(𝕨, 𝕩) run_block_body(vm, frame, block, 𝕣, 𝕨, 𝕩, nothing, nothing) end)
    𝕣
  elseif typ == 1 && imm == 0 # mod1 deferred
    # @info "mod1 deferred"
    𝕣 = M1(function(𝕘, 𝕗)
      F(vm, frame, block, nothing, 𝕣, 𝕗)
    end)
    𝕣
  elseif typ == 2 && imm == 0 # mod2 deferred
    𝕣 = M2(function(𝕘, 𝕗)
      F(vm, frame, block, 𝕘, 𝕣, 𝕗)
    end)
    𝕣
  end
end

""" Run compiler bytecode, this is the entry point to VM."""
function run(src, code, consts, blocks, bodies)
  vm = VM(src, code, consts, blocks, bodies)
  frame = Frame(nothing, [])
  run_block(vm, frame, blocks[1])
end

""" Compile the BQN expression using bootstrap compiler."""
function bqncompile0(code)
    jlsrc = read(`./cjl.bqn $(code)`, String)
    jlcode = eval(Meta.parse(jlsrc))
    return jlcode
end

""" Compile and run the BQN expression (using bootstrap compiler)."""
function bqneval0(code)
    jlcode = bqncompile0(code)
    boot = eval(jlcode)
    run(code, boot...)
end

_provide = [
  Runtime.bqntype,
  Runtime.bqnfill,
  Runtime.bqnlog,
  Runtime.bqngrouplen,
  Runtime.bqngroupord,
  Runtime.bqnassert,
  Runtime.bqnadd,
  Runtime.bqnsub,
  Runtime.bqnmul,
  Runtime.bqndiv,
  Runtime.bqnpow,
  Runtime.bqnmin,
  Runtime.bqneq,
  Runtime.bqnlte,
  Runtime.bqnshape,
  Runtime.bqndeshape,
  Runtime.bqnpick,
  Runtime.bqnwindow,
  Runtime.bqntable,
  Runtime.bqnscan,
  Runtime.bqnfillby,
  Runtime.bqnvalences,
  Runtime.bqncatch,
]
provide(n::Int64) = _provide[n + 1]

str(s::String) = s

module R
import ..provide, ..str
include("./r.jl")
end

_runtime, set_prims, set_inv = run("<none>", R.value...)

runtime(n::Int64) = _runtime[n + 1]

function decompose(𝕨, 𝕩)
  kind =
    if     𝕩 in _runtime;                [0, 𝕩]
    elseif isa(𝕩, F) && 𝕩.𝕘 !== nothing; [5, 𝕩.𝕗, 𝕩.𝕣, 𝕩.𝕘]
    elseif isa(𝕩, F) && 𝕩.𝕗 !== nothing; [4, 𝕩.𝕗, 𝕩.𝕣]
    elseif isa(𝕩, F);                    [1, 𝕩]
    elseif isa(𝕩, TR2D);                 [2, 𝕩.h, 𝕩.𝕘]
    elseif isa(𝕩, TR3D);                 [3, 𝕩.𝕘, 𝕩.h, 𝕩.𝕗]
    elseif isa(𝕩, TR3O);                 [3, 𝕩.𝕘, 𝕩.h, 𝕩.𝕗]
    elseif isa(𝕩, M1);                   [4, 𝕩.𝕗, 𝕩]
    elseif isa(𝕩, M2);                   [5, 𝕩.𝕗, 𝕩, 𝕩.𝕘]
    else                                 [-1, 𝕩]
    end
  # @info "decompose" 𝕩 kind
  kind
end

function prim_ind(𝕨, 𝕩)
  # @info "prim_ind" 𝕨 𝕩
  for (idx, 𝕗) in enumerate(_runtime);
    if 𝕗 === 𝕩; return (idx - 1); end
  end
  return length(_runtime)
end

set_prims(none, [decompose, prim_ind])

module C
import ..runtime, ..str
include("./c.jl")
end

c = run("<none>", C.value...)

""" Compile BQN expression using self-hosted compiler."""
function bqncompile(src)
  c(_runtime, src)
end

""" Compile and eval BQN expression (using self-hosted compiler)."""
function bqneval(src)
  code, consts, blocks, bodies, toks, names = bqncompile(src)
  run(src, code, consts, blocks, bodies)
end

export bqneval

""" Test suite using the bootstrap compiler."""
module Tests0
import ..BQNError, ..bqneval0 as bqneval
include("./test/test.jl")
end

""" Test suite using the self hosted compiler."""
module Tests
import ..BQNError, ..bqneval
include("./test/test.jl")
end

""" REPL mode."""
module Repl
using ReplMaker
import ..bqneval, ..bqneval0

function init()
  # TODO: now using the bootstrap compiler, switch to bqneval once self-hosted
  # compiler is fast enough.
  initrepl(bqneval0,
           prompt_text="BQN) ",
           prompt_color=:blue, 
           startup_text=true,
           start_key=')', 
           mode_name="BQN")
  nothing
end
end

end
