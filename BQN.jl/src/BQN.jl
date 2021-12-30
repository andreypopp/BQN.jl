module BQN
using Logging
using Debugger

struct BQNError <: Exception msg::String end

abstract type Var end

struct None end
const none = None()

mutable struct Ref <: Var
  value::Union{Any,Nothing}
end

Base.show(io::IO, r::Ref) = print(io, "BQN.Ref")

struct RefList <: Var
  vec::Vector{Var}
  function RefList(n::Int64)
    v = new(Vector{Var}())
    sizehint!(v.vec, Int(n))
    v
  end
end

function Base.show(io::IO, rs::RefList)
  for x in rs.vec; show(io, x) end
end

struct RefNot <: Var end

struct Env
  parent::Union{Env,Nothing}
  vars::Vector{Var}
end

function getv(ref::Ref)
  @assert ref.value !== nothing
  ref.value
end

function getv(ref::RefList)
  map(getv, ref.vec)
end

function setn!(ref::Ref, value::Any)
  @assert ref.value == nothing
  ref.value = value
end

function setn!(ref::RefList, value::AbstractArray)
  @assert length(ref.vec) == length(value)
  for (refitem, valueitem) in zip(ref.vec, value)
    setn!(refitem, valueitem)
  end
end

function setn!(ref::RefNot, value::Any)
end

function setu!(ref::Ref, value::Any)
  @assert ref.value != nothing
  ref.value = value
end

function setu!(ref::RefList, value::AbstractArray)
  @assert length(ref.vec) == length(value)
  for (refitem, valueitem) in zip(ref.vec, value)
    setu!(refitem, valueitem)
  end
end

function setu!(ref::RefNot, value::Any)
end

struct F
  𝕤::Function
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
(𝕤::F)(𝕨, 𝕩) = 𝕤.𝕤(𝕨, 𝕩)
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
(𝕤::M1)(𝕨, 𝕩) = 𝕤.run(𝕨, 𝕩)
(𝕤::M2)(𝕨, 𝕩) = 𝕤.run(𝕨, 𝕩)

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
      # @debug "PRIMITIVE bqnvalences"
      if 𝕨 === none
        𝕗(𝕨, 𝕩)
      else
        𝕘(𝕨, 𝕩)
      end
    end
  end

  function bqncatch(𝕘, 𝕗)
    function (𝕨, 𝕩)
      # @debug "PRIMITIVE bqncatch"
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

  # function row_major_reshape(𝕩::AbstractArray, size...)
  #   𝕩 = reshape(𝕩, reverse([size...])...)
  #   if size != ()
  #     size_perm = length(size):-1:1
  #     𝕩 = permutedims(𝕩, size_perm)
  #   end
  #   𝕩
  # end

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
  bqnpick(𝕨::Number, 𝕩::AbstractArray) = 𝕩[Int(𝕨) + 1]
  bqnpick(𝕨::None, 𝕩::AbstractArray) = bqnpick(0, 𝕩)
  # TODO: get rid of collect, this is slow!
  bqnpick(𝕨::Number, 𝕩::String) = collect(𝕩)[Int(𝕨) + 1]
  bqnpick(𝕨::None, 𝕩::String) = bqnpick(0, 𝕩)
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
    # @info "bqngrouplen" 𝕨 𝕩
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
    storage = [get(lengths, x, 0) for x in 0:minl]
    storage
  end

  function bqngroupord(𝕨, 𝕩::AbstractArray)
    # @info "bqngroupord" 𝕨 𝕩
    # TODO: Use info in 𝕨 (which is `grouplen𝕩`)?
    indices = [[] for _ in 1:length(𝕨)]
    for (idx, x) in enumerate(𝕩)
      if x < 0; continue end
      push!(indices[Int(x) + 1], idx - 1)
    end
    storage = vcat(indices...)
    # @info "bqngroupord" 𝕩 storage
    storage
  end

  function bqnassert(𝕨, 𝕩)
    if 𝕩 == 1
      1
    else
      # TODO: should we use 𝕩 as error message in case it's a string? r1.bqn
      # seems to be relying on that behaviour... see !∘"msg" pattern.
      msg = 𝕨 === none ? (isa(𝕩, String) ? 𝕩 : "ERROR") : 𝕨
      if isa(msg, AbstractArray)
        msg = join(msg)
      end
      throw(BQNError(msg))
    end
  end

  function bqnfillby(𝕘, 𝕗)
    function(𝕨, 𝕩)
      # @debug "PRIMITIVE bqnfillby"
      𝕗(𝕨, 𝕩)
    end
  end

  function runtime_not_implemented(idx)
    return function(w, x)
      @error "$(idx) runtime function is not implemented"
      @assert false
    end
  end

  function provide_not_implemented(idx)
    return function(w, x)
      @error "$(idx) provide function is not implemented"
      @assert false
    end
  end
end

str(s::String) = s

struct VM
  src::String
  code::Array{Int64}
  consts::Array{Any}
  blocks::Array{Any}
  bodies::Array{Any}
end

function run_code(vm::VM, env::Env, pc::Int64)
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
      push!(stack, run_block(vm, env, block))
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
      # @debug "BYTECODE 1C ARRM"
      pc += 1
      n = vm.code[pc + 1]
      v = RefList(Int(n))
      for i in 1:n
        push!(v.vec, popat!(stack, Int(length(stack) - n + i)))
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
      cenv = env
      while d > 0; cenv = cenv.parent; d -= 1 end
      ref = cenv.vars[i + 1]
      # @info "BYTECODE 20 VARO D=$(d) I=$(i)" ref
      push!(stack, getv(ref))
    elseif instr == 0x21 # VARM
      pc += 1
      d = vm.code[pc + 1]
      pc += 1
      i = vm.code[pc + 1]
      # @debug "BYTECODE 21 VARM D=$(d) I=$(i)"
      cenv = env
      while d > 0; cenv = cenv.parent; d -= 1 end
      ref = cenv.vars[i + 1]
      push!(stack, ref)
    elseif instr == 0x22 # VARU
      pc += 1
      d = vm.code[pc + 1]
      pc += 1
      i = vm.code[pc + 1]
      cenv = env
      while d > 0; cenv = cenv.parent; d -= 1 end
      ref = cenv.vars[i + 1]
      # @debug "BYTECODE 22 VARU D=$(d) I=$(i)"
      # TODO: need to clear the ref
      # @info "BYTECODE 20 VARO D=$(d) I=$(i)" ref
      push!(stack, getv(ref))
    elseif instr == 0x2C # NOTM
      push!(stack, RefNot())
    elseif instr == 0x30 # SETN
      ref, value = pop!(stack), pop!(stack)
      # @debug "BYTECODE 30 SETN"
      setn!(ref, value)
      push!(stack, value)
    elseif instr == 0x31 # SETU
      ref, value = pop!(stack), pop!(stack)
      # @debug "BYTECODE 31 SETU"
      setu!(ref, value)
      push!(stack, value)
    elseif instr == 0x32 # SETM
      ref, 𝕗, 𝕩 = pop!(stack), pop!(stack), pop!(stack)
      # @debug "BYTECODE 32 SETM"
      value = 𝕗(getv(ref), 𝕩)
      setu!(ref, value)
      push!(stack, value)
    elseif instr == 0x33 # SETC
      ref, 𝕗 = pop!(stack), pop!(stack)
      # @debug "BYTECODE 33 SETC"
      value = 𝕗(none, getv(ref))
      setu!(ref, value)
      push!(stack, value)
    else
      @error "UNKNOWN BYTECODE 0x$(string(instr, base=16))"
      @assert false
    end
    pc += 1
  end
end

function run_body(vm::VM, parent::Env, body_idx::Int64, 𝕤, 𝕨, 𝕩, 𝕘, 𝕗)
  pc, num_vars = vm.bodies[body_idx + 1]
  # @debug "BODY@$(idx-1) $(num_vars)"
  vars = Ref[]
  for _ in 1:num_vars; push!(vars, Ref(nothing)) end
  if num_vars >= 1 vars[1].value = 𝕤 end
  if num_vars >= 2 vars[2].value = 𝕩 end
  if num_vars >= 3 vars[3].value = 𝕨 end
  # TODO: handle 𝕣
  # if num_vars >= 4 vars[4].value = 𝕣 end
  if num_vars >= 5 vars[5].value = 𝕗 end
  if num_vars >= 6 vars[6].value = 𝕘 end
  env = Env(parent, vars)
  run_code(vm, env, pc)
end

function run_block(vm::VM, env::Env, block)
  typ, imm, body_idx = block
  # @debug "BLOCK type=$(typ) immediate=$(imm) body=$(body_idx)"
  function run(𝕤, 𝕨, 𝕩, 𝕘, 𝕗)
    if isa(body_idx, Int)
      run_body(vm, env, body_idx, 𝕤, 𝕨, 𝕩, 𝕘, 𝕗)
    elseif isa(body_idx, Array) || isa(body_idx, AbstractArray)
      ret = nothing
      for body in body_idx
        for idx in body
          # TODO: need to check for PRED/SETH failures here
          ret = run_body(vm, env, idx, 𝕤, 𝕨, 𝕩, 𝕘, 𝕗)
        end
      end
      @assert ret !== nothing
      ret
    end
  end
  if typ == 0 && imm == 1 # immediate
    run(nothing, nothing, nothing, nothing, nothing)
  elseif typ == 0 && imm == 0 # function
    𝕤 = F(
          function(𝕨, 𝕩) run(𝕤, 𝕨, 𝕩, nothing, nothing) end,
          nothing,
          nothing,
          nothing)
    𝕤
  elseif typ == 1 && imm == 1 # mod1 immediate
    # @info "mod1 immediate"
    𝕣 = M1(function(𝕨, 𝕩) run(𝕣, 𝕨, 𝕩, nothing, nothing) end)
    𝕣
  elseif typ == 2 && imm == 1 # mod2 immediate
    𝕣 = M2(function(𝕨, 𝕩) run(𝕣, 𝕨, 𝕩, nothing, nothing) end)
    𝕣
  elseif typ == 1 && imm == 0 # mod1 deferred
    # @info "mod1 deferred"
    𝕣 = M1(function(𝕘, 𝕗)
      𝕤 = F(
            function(𝕨, 𝕩) run(𝕤, 𝕨, 𝕩, nothing, 𝕗) end,
            nothing,
            𝕣,
            𝕗)
      𝕤
    end)
    𝕣
  elseif typ == 2 && imm == 0 # mod2 deferred
    𝕣 = M2(function(𝕘, 𝕗)
      𝕤 = F(
            function(𝕨, 𝕩) run(𝕤, 𝕨, 𝕩, 𝕘, 𝕗) end,
            𝕘,
            𝕣,
            𝕗)
      𝕤
    end)
    𝕣
  end
end

function run(src, code, consts, blocks, bodies)
  vm = VM(src, code, consts, blocks, bodies)
  env = Env(nothing, [])
  run_block(vm, env, blocks[1])
end

function bqncompile0(code)
    jlsrc = read(`./cjl.bqn $(code)`, String)
    jlcode = eval(Meta.parse(jlsrc))
    return jlcode
end

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

module R
import ..provide, ..str
include("./r.jl")
end

_runtime, set_prims, set_inv = run("<none>", R.value...)

runtime(n::Int64) = _runtime[n + 1]

function decompose(𝕨, 𝕩)
  kind =
    if     𝕩 in _runtime;     [0, 𝕩]
    elseif isa(𝕩, F) && 𝕩.𝕘 !== nothing; [5, 𝕩.𝕗, 𝕩.𝕣, 𝕩.𝕘]
    elseif isa(𝕩, F) && 𝕩.𝕗 !== nothing; [4, 𝕩.𝕗, 𝕩.𝕣]
    elseif isa(𝕩, F);                    [1, 𝕩]
    elseif isa(𝕩, TR2D);      [2, 𝕩.𝕘, 𝕩.h]
    elseif isa(𝕩, TR3D);      [2, 𝕩.𝕗, 𝕩.𝕘, 𝕩.h]
    elseif isa(𝕩, TR3O);      [2, 𝕩.𝕗, 𝕩.𝕘, 𝕩.h]
    elseif isa(𝕩, M1);        [4, 𝕩.𝕗, 𝕩]
    elseif isa(𝕩, M2);        [5, 𝕩.𝕗, 𝕩, 𝕩.𝕘]
    else                      [-1, 𝕩]
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

function bqncompile(src)
  c(_runtime, src)
end

function bqneval(src)
  code, consts, blocks, bodies, toks, names = bqncompile(src)
  run(src, code, consts, blocks, bodies)
end

export bqneval

module Tests0
import ..BQNError, ..bqneval0 as bqneval
include("./test/test.jl")
end

module Tests
import ..BQNError, ..bqneval
include("./test/test.jl")
end

module Repl
using ReplMaker
import ..bqneval

function init()
  initrepl(bqneval,
           prompt_text="BQN) ",
           prompt_color=:blue, 
           startup_text=true,
           start_key=')', 
           mode_name="BQN")
  nothing
end
end

end
