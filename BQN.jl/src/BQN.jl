module BQN
using Logging
using Debugger

struct BQNError <: Exception msg::String end

abstract type Var end

struct None end
none = None()

mutable struct Ref <: Var
  value::Union{Any,Nothing}
end

Base.show(io::IO, r::Ref) = print(io, "BQN.Ref")

struct RefList <: Var
  vec::Vector{Var}
  function RefList(n::Int64)
    v = new(Vector{Var}())
    sizehint!(v.vec, n)
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

struct Arr
  storage::Array{Any}
  function Arr(n::Int64)
    𝕩 = new(Vector{Any}())
    sizehint!(𝕩.storage, n)
    𝕩
  end
  function Arr(storage::Any)
    new(storage)
  end
end

function Base.display(𝕩::Arr)
  size = Base.size(𝕩)
  if size == ()
    display(𝕩.storage)
  else
    display(permutedims(𝕩.storage, length(size):-1:1))
  end
end

Base.size(𝕩::Arr) = size(𝕩.storage)
Base.iterate(𝕩::Arr) = iterate(𝕩.storage)
Base.iterate(𝕩::Arr, n::Int64) = iterate(𝕩.storage, n)
Base.getindex(𝕩::Arr, idx::Int64) = getindex(𝕩.storage, idx)
Base.length(𝕩::Arr) = length(𝕩.storage)

function Base.map(f, coll::Arr)
  res = Arr(length(coll))
  for v in coll.storage; push!(res.storage, f(nothing, v)) end
  res
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

function setn!(ref::RefList, value::Arr)
  @assert length(ref.vec) == length(value.storage)
  for (refitem, valueitem) in zip(ref.vec, value.storage)
    setn!(refitem, valueitem)
  end
end

function setn!(ref::RefNot, value::Any)
end

function setu!(ref::Ref, value::Any)
  @assert ref.value != nothing
  ref.value = value
end

function setu!(ref::RefList, value::Arr)
  @assert length(ref.vec) == length(value.storage)
  for (refitem, valueitem) in zip(ref.vec, value.storage)
    setu!(refitem, valueitem)
  end
end

function setu!(ref::RefNot, value::Any)
end

struct F
  𝕗::Function
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
  𝕗::Any
end

Base.show(io::IO, f::M1) = show(io, "<BQN 1-modifier>")

struct M2
  run::Function
  𝕘::Any
  𝕗::Any
end

Base.show(io::IO, f::M2) = show(io, "<BQN 2-modifier>")

(𝕤::Arr)(𝕨, 𝕩) = 𝕤
(𝕤::Float64)(𝕨, 𝕩) = 𝕤
(𝕤::Int)(𝕨, 𝕩) = 𝕤
(𝕤::Char)(𝕨, 𝕩) = 𝕤
(𝕤::Bool)(𝕨, 𝕩) = 𝕤
(𝕤::String)(𝕨, 𝕩) = 𝕤
(𝕤::F)(𝕨, 𝕩) = 𝕤.𝕗(𝕨, 𝕩)
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
(𝕤::M1)(𝕨, 𝕩) = 𝕤.run(𝕤, 𝕨, 𝕩, nothing, 𝕤.𝕗)
(𝕤::M2)(𝕨, 𝕩) = 𝕤.run(𝕤, 𝕨, 𝕩, 𝕤.𝕘, 𝕤.𝕗)

module Runtime
  using Debugger
  import ..Arr, ..None, ..none, ..F, ..TR2D, ..TR3D, ..TR3O, ..M1, ..M2, ..BQNError

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
      @debug "PRIMITIVE bqnvalences"
      if 𝕨 === none
        𝕗(𝕨, 𝕩)
      else
        𝕘(𝕨, 𝕩)
      end
    end
  end

  function bqncatch(𝕘, 𝕗)
    function (𝕨, 𝕩)
      @debug "PRIMITIVE bqncatch"
      try
        𝕗(𝕨, 𝕩)
      catch e
        𝕘(𝕨, 𝕩)
      end
    end
  end

  bqneq(𝕨::None, 𝕩::Arr) = ndims(𝕩.storage)
  bqneq(𝕨::None, 𝕩::String) = 1
  bqneq(𝕨::None, 𝕩) = 0
  bqneq(𝕨, 𝕩) = Int(𝕨 == 𝕩)

  bqnlte(𝕨, 𝕩) = 𝕨 <= 𝕩
  bqnlte(𝕨::Number, 𝕩::Char) = 1
  bqnlte(𝕨::Char, 𝕩::Number) = 0

  bqnshape(𝕨, 𝕩::Arr) = Arr(reverse([x for x in size(𝕩)]))
  bqnshape(𝕨, 𝕩::String) = Arr([length(𝕩)])
  bqnshape(𝕨, 𝕩) = Arr([])

  bqndeshape(𝕨::None, 𝕩::Arr) = Arr(vec(𝕩.storage))
  bqndeshape(𝕨::None, 𝕩::String) = 𝕩
  bqndeshape(𝕨::None, 𝕩) = Arr([𝕩])

  # function row_major_reshape(𝕩::AbstractArray, size...)
  #   𝕩 = reshape(𝕩, reverse([size...])...)
  #   if size != ()
  #     size_perm = length(size):-1:1
  #     𝕩 = permutedims(𝕩, size_perm)
  #   end
  #   𝕩
  # end

  function bqndeshape(𝕨::Arr, 𝕩::Arr)
    size = reverse(Tuple(Int(x) for x in 𝕨))
    if size == Base.size(𝕩.storage); return 𝕩 end
    Arr(reshape(𝕩.storage, size))
  end

  function bqndeshape(𝕨::Arr, 𝕩::String)
    𝕩 = Arr(collect(𝕩))
    bqndeshape(𝕨, 𝕩)
  end
        
  function bqndeshape(𝕨::Arr, 𝕩::Any)
    @assert length(𝕨) == 0
    Arr(collect(𝕩))
  end

  bqnpick(𝕨::Number, 𝕩::Number) = 𝕩
  bqnpick(𝕨::Number, 𝕩::Arr) = 𝕩.storage[Int(𝕨) + 1]
  bqnpick(𝕨::None, 𝕩::Arr) = bqnpick(0, 𝕩)
  # TODO: get rid of collect, this is slow!
  bqnpick(𝕨::Number, 𝕩::String) = collect(𝕩)[Int(𝕨) + 1]
  bqnpick(𝕨::None, 𝕩::String) = bqnpick(0, 𝕩)
  bqnpick(𝕨::None, 𝕩) = 𝕩

  bqnwindow(𝕨, 𝕩) = Arr([x for x in 0:(𝕩-1)])

  function bqntable(𝕘, 𝕗)
    # TODO: need to get rid of calls to collect() here, instead need to iterate
    # over graphemes for Strings
    function(𝕨, 𝕩)
      @debug "PRIMITIVE bqntable"
      if 𝕨 === none
        if !isa(𝕩, Arr); 𝕩 = collect(𝕩) end
        len𝕩, size𝕩 = length(𝕩), size(𝕩)
        storage = []
        sizehint!(storage, len𝕩)
        for i in 1:len𝕩
          push!(storage, 𝕗(none, 𝕩[i]))
        end
        Arr(reshape(storage, size𝕩))
      else
        if !isa(𝕨, Arr); 𝕨 = collect(𝕨) end
        if !isa(𝕩, Arr); 𝕩 = collect(𝕩) end
        sizeres = (size(𝕩)..., size(𝕨)...)
        storage = []
        sizehint!(storage, sizeres != () ? *(sizeres...) : 1)
        for i in 1:length(𝕨)
          for j in 1:length(𝕩)
            v = 𝕗(𝕨[i], 𝕩[j])
            push!(storage, v)
          end
        end
        storage = reshape(storage, sizeres)
        Arr(storage)
      end
    end
  end

  function bqnscan(𝕘, 𝕗)
    function(𝕨, 𝕩::Arr)
      bqnassert(
                "`: Argument cannot have rank 0",
                Int(ndims(𝕩.storage) != 0))
      bqnassert(
                "`: Shape of 𝕨 must match the cell of 𝕩",
                Int(𝕨 == none ||
                    size(𝕨) == () && ndims(𝕩.storage) == 1 ||
                    size(𝕨)[1:1] == size(𝕩)[1:1]))
      @debug "PRIMITIVE bqnscan"
      storage = if 𝕨 == none
        accumulate(𝕗, 𝕩.storage, dims=ndims(𝕩.storage))
      elseif size(𝕨) == ()
        accumulate(𝕗, 𝕩.storage, dims=ndims(𝕩.storage), init=𝕨)
      else
        # Because accumulate() doesn't support init being an array we provide
        # init value by concatenating it over the major dimension with hvncat():
        storage = hvncat(ndims(𝕩.storage), 𝕨.storage, 𝕩.storage)
        storage = accumulate(𝕗, storage, dims=ndims(𝕩.storage))
        # ... but this will produce an extra "row" in this dimension so we
        # produce a view which "cuts" that out with a view over this array:
        # TODO: Revisit that for performance!
        indices = [(:) for _ in size(storage)[1:end - 1]]
        storage = @view storage[indices..., 2:end]
        storage
      end
      Arr(storage)
    end
  end

  bqntype(𝕨::None, 𝕩::Arr) = 0
  bqntype(𝕨::None, 𝕩::String) = 0
  bqntype(𝕨::None, 𝕩::Number) = 1
  bqntype(𝕨::None, 𝕩::Char) = 2
  bqntype(𝕨::None, 𝕩::Function) = 3
  bqntype(𝕨::None, 𝕩::TR2D) = 3
  bqntype(𝕨::None, 𝕩::TR3D) = 3
  bqntype(𝕨::None, 𝕩::TR3O) = 3
  bqntype(𝕨::None, 𝕩::F) = 3
  bqntype(𝕨::None, 𝕩::M1) = 4
  bqntype(𝕨::None, 𝕩::M2) = 5

  bqnfill(𝕨::None, 𝕩::String) = ' '
  bqnfill(𝕨::None, 𝕩::Arr) = 0
  bqnfill(𝕨, 𝕩) = 𝕩

  bqnlog(𝕨::None, 𝕩::Number) = log(ℯ, 𝕩)
  bqnlog(𝕨::Number, 𝕩::Number) = log(𝕨, 𝕩)

  function bqngrouplen(𝕨, 𝕩::Arr)
    # @info "bqngrouplen" 𝕨 𝕩
    order = []
    lengths = Dict{Int,Int}()
    max𝕩 = -1
    for x in 𝕩.storage
      max𝕩 = max(max𝕩, x)
      if haskey(lengths, x)
        lengths[x] += 1
      else
        lengths[x] = 1
        push!(order, x)
      end
    end
    minl = max(max𝕩, 𝕨 !== none ? (𝕨 - 1) : -1)
    storage = [get(lengths, x, 0) for x in 0:minl]
    # @info "bqngrouplen" minl storage
    Arr(storage)
  end

  function bqngroupord(𝕨, 𝕩::Arr)
    # @info "bqngroupord" 𝕨 𝕩
    # TODO: Use info in 𝕨 (which is `grouplen𝕩`)?
    indices = [[] for _ in 1:length(𝕨)]
    for (idx, x) in enumerate(𝕩)
      if x < 0; continue end
      push!(indices[Int(x) + 1], idx - 1)
    end
    storage = vcat(indices...)
    Arr(storage)
  end

  function bqnassert(𝕨, 𝕩)
    if 𝕩 == 1
      1
    else
      # TODO: should we use 𝕩 as error message in case it's a string? r1.bqn
      # seems to be relying on that behaviour... see !∘"msg" pattern.
      msg = 𝕨 === none ? (isa(𝕩, String) ? 𝕩 : "ERROR") : 𝕨
      if isa(msg, Arr)
        msg = join(msg.storage)
      end
      throw(BQNError(msg))
    end
  end

  function bqnfillby(𝕘, 𝕗)
    function(𝕨, 𝕩)
      @debug "PRIMITIVE bqnfillby"
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

module Bytecode
  known = Dict(
               0x00 => ("PUSH", ["I"]),
               0x01 => ("DFND", ["I"]),
               0x06 => ("POPS", []),
               0x07 => ("RETN", []),
               0x0B => ("ARRO", ["N"]),
               0x0C => ("ARRM", ["N"]),
               0x10 => ("FN1C", []),
               0x11 => ("FN2C", []),
               0x12 => ("FN1O", []),
               0x13 => ("FN2O", []),
               0x14 => ("TR2D", []),
               0x15 => ("TR3D", []),
               0x17 => ("TR3O", []),
               0x1A => ("MD1C", []),
               0x1B => ("MD2C", []),
               0x20 => ("VARO", ["D", "I"]),
               0x21 => ("VARM", ["D", "I"]),
               0x22 => ("VARU", ["D", "I"]),
               0x2C => ("NOTM", []),
               0x30 => ("SETN", []),
               0x31 => ("SETU", []),
               0x32 => ("SETM", []),
               0x33 => ("SETC", []),
  )

  function decode(bytecode; pos=0, len=10)
    result = []
    while length(result) < len
      name, argnames = get(known, bytecode[pos + 1], (nothing, nothing))
      if name === nothing; break end
      args = []
      for arg in argnames
        pos += 1
        push!(args, (arg, bytecode[pos + 1]))
      end
      push!(result, (name, args))
      pos += 1
    end
    return result
  end
end

function vm(src, code, consts, blocks, bodies, toks)
  cbodies = []
  for (idx, body) in enumerate(bodies)
    code_idx, num_vars = body
    push!(cbodies, function(parent, 𝕤, 𝕨, 𝕩, 𝕘, 𝕗)
            @debug "BODY@$(idx-1) $(num_vars)"
            vars = []
            for _ in 1:num_vars; push!(vars, Ref(nothing)) end
            if num_vars >= 1 vars[1].value = 𝕤 end
            if num_vars >= 2 vars[2].value = 𝕩 end
            if num_vars >= 3 vars[3].value = 𝕨 end
            # TODO: handle 𝕣
            # if num_vars >= 4 vars[4].value = 𝕣 end
            if num_vars >= 5 vars[5].value = 𝕗 end
            if num_vars >= 6 vars[6].value = 𝕘 end
            env = Env(parent, vars)
            run_body(code_idx, env)
          end)
  end

  function run_block(block, env)
    typ, imm, body_idx = block
    @debug "BLOCK type=$(typ) immediate=$(imm) body=$(body_idx)"
    function run(𝕤, 𝕨, 𝕩, 𝕘, 𝕗)
      if isa(body_idx, Int)
        cbodies[body_idx + 1](env, 𝕤, 𝕨, 𝕩, 𝕘, 𝕗)
      elseif isa(body_idx, Vector)
        ret = nothing
        for body in body_idx
          for idx in body
            # TODO: need to check for PRED/SETH failures here
            ret = cbodies[idx + 1](env, 𝕤, 𝕨, 𝕩, 𝕘, 𝕗)
          end
        end
        @assert ret !== nothing
        ret
      end
    end
    if typ == 0 && imm == 1 # immediate
      run(nothing, nothing, nothing, nothing, nothing)
    elseif typ == 0 && imm == 0 # function
      𝕤 = F(function(𝕨, 𝕩) run(𝕤, 𝕨, 𝕩, nothing, nothing) end)
      𝕤
    elseif typ == 1 && imm == 1 # mod1 immediate
      M1(run, nothing)
    elseif typ == 2 && imm == 1 # mod2 immediate
      M2(run, nothing, nothing)
    elseif typ == 1 && imm == 0 # mod1 deferred
      function(𝕘, 𝕗)
        𝕤 = F(function(𝕨, 𝕩) run(𝕤, 𝕨, 𝕩, nothing, 𝕗) end)
        𝕤
      end
    elseif typ == 2 && imm == 0 # mod2 deferred
      function(𝕘, 𝕗)
        𝕤 = F(function(𝕨, 𝕩) run(𝕤, 𝕨, 𝕩, 𝕘, 𝕗) end)
        𝕤
      end
    end
  end

  function run_body(code_idx, env)
    stack = []
    s = src
    x = toks
    while true
      instr = code[code_idx + 1]
      if instr == 0x00 # PUSH
        code_idx += 1
        v = consts[code[code_idx + 1] + 1]
        @debug "BYTECODE 00 PUSH"
        push!(stack, v)
      elseif instr == 0x01 # DFND
        @debug "BYTECODE 01 DFND"
        code_idx += 1
        block = blocks[code[code_idx + 1] + 1]
        push!(stack, run_block(block, env))
      elseif instr == 0x06 # POPS
        @debug "BYTECODE 06 POPS"
        pop!(stack)
      elseif instr == 0x07 # RETN
        @debug "BYTECODE 07 RETN"
        return pop!(stack)
      elseif instr == 0x0B # ARRO
        code_idx += 1
        n = code[code_idx + 1]
        @debug "BYTECODE 0B ARRO N=$(n)"
        v = Arr(n)
        for i in 1:n
          push!(v.storage, popat!(stack, length(stack) - n + i))
        end
        push!(stack, v)
      elseif instr == 0x0C # ARRM
        @debug "BYTECODE 1C ARRM"
        code_idx += 1
        n = code[code_idx + 1]
        v = RefList(n)
        for i in 1:n
          push!(v.vec, popat!(stack, length(stack) - n + i))
        end
        push!(stack, v)
      elseif instr == 0x10 # FN1C
        @debug "BYTECODE 10 FN1C"
        s, x = pop!(stack), pop!(stack)
        push!(stack, s(none, x))
      elseif instr == 0x11 # FN2C
        @debug "BYTECODE 11 FN2C"
        w, s, x = pop!(stack), pop!(stack), pop!(stack)
        push!(stack, s(w, x))
      elseif instr == 0x12 # FN1O
        @debug "BYTECODE 12 FN1O"
        s, x = pop!(stack), pop!(stack)
        if x !== none
          push!(stack, s(none, x))
        else
          push!(stack, none)
        end
      elseif instr == 0x13 # FN2O
        w, s, x = pop!(stack), pop!(stack), pop!(stack)
        @debug "BYTECODE 13 FN20"
        if x !== none
          push!(stack, s(w, x))
        else
          push!(stack, none)
        end
      elseif instr == 0x14 # TR2D
        @debug "BYTECODE 14 TR2D"
        h, 𝕘 = pop!(stack), pop!(stack)
        push!(stack, TR2D(h, 𝕘))
      elseif instr == 0x15 # TR3D
        @debug "BYTECODE 15 TR3D"
        𝕘, h, 𝕗 = pop!(stack), pop!(stack), pop!(stack)
        push!(stack, TR3D(h, 𝕘, 𝕗))
      elseif instr == 0x17 # TR3O
        @debug "BYTECODE 17 TR3O"
        𝕘, h, 𝕗 = pop!(stack), pop!(stack), pop!(stack)
        push!(stack, TR3O(h, 𝕘, 𝕗))
      elseif instr == 0x1A # MD1C
        @debug "BYTECODE 1A MD1C"
        f, r = pop!(stack), pop!(stack)
        push!(stack, r(nothing, f))
      elseif instr == 0x1B # MD2C
        @debug "BYTECODE 1B MD2C"
        f, r, g = pop!(stack), pop!(stack), pop!(stack)
        push!(stack, r(g, f))
      elseif instr == 0x20 # VARO
        code_idx += 1
        d = code[code_idx + 1]
        code_idx += 1
        i = code[code_idx + 1]
        cenv = env
        while d > 0; cenv = cenv.parent; d -= 1 end
        ref = cenv.vars[i + 1]
        @debug "BYTECODE 20 VARO D=$(d) I=$(i)"
        push!(stack, getv(ref))
      elseif instr == 0x21 # VARM
        code_idx += 1
        d = code[code_idx + 1]
        code_idx += 1
        i = code[code_idx + 1]
        @debug "BYTECODE 21 VARM D=$(d) I=$(i)"
        cenv = env
        while d > 0; cenv = cenv.parent; d -= 1 end
        ref = cenv.vars[i + 1]
        push!(stack, ref)
      elseif instr == 0x22 # VARU
        code_idx += 1
        d = code[code_idx + 1]
        code_idx += 1
        i = code[code_idx + 1]
        cenv = env
        while d > 0; cenv = cenv.parent; d -= 1 end
        ref = cenv.vars[i + 1]
        @debug "BYTECODE 22 VARU D=$(d) I=$(i)"
        # TODO: need to clear the ref
        push!(stack, getv(ref))
      elseif instr == 0x2C # NOTM
        push!(stack, RefNot())
      elseif instr == 0x30 # SETN
        ref, value = pop!(stack), pop!(stack)
        @debug "BYTECODE 30 SETN"
        setn!(ref, value)
        push!(stack, value)
      elseif instr == 0x31 # SETU
        ref, value = pop!(stack), pop!(stack)
        @debug "BYTECODE 31 SETU"
        setu!(ref, value)
        push!(stack, value)
      elseif instr == 0x32 # SETM
        ref, 𝕗, 𝕩 = pop!(stack), pop!(stack), pop!(stack)
        @debug "BYTECODE 32 SETM"
        value = 𝕗(getv(ref), 𝕩)
        setu!(ref, value)
        push!(stack, value)
      elseif instr == 0x33 # SETC
        ref, 𝕗 = pop!(stack), pop!(stack)
        @debug "BYTECODE 33 SETC"
        value = 𝕗(none, getv(ref))
        setu!(ref, value)
        push!(stack, value)
      else
        @error "UNKNOWN BYTECODE 0x$(string(instr, base=16))"
        @assert false
      end
      code_idx += 1
    end
  end

  run_block(blocks[1], Env(nothing, []))
end

function bqncompile(code)
    jlsrc = read(`./BQN/src/cjs.bqn -i $(code)`, String)
    jlcode = eval(Meta.parse(jlsrc))
    return jlcode
end

function bqneval(code)
    jlcode = bqncompile(code)
    boot = eval(jlcode)
    vm(code, boot...)
end

using Test

function run_testsuite(cases; only=nothing, title=nothing)
  if title !== nothing; @info "=== TEST SUITE $(title)" end
  for (idx, (expected, code)) in enumerate(cases)
    if only !== nothing && !(idx in only); continue end
    @info "=== TEST@$(idx) $(code)"
    if isa(expected, DataType) && expected <: Exception
      Test.@test_throws expected bqneval(code)
    else
      Test.@test expected == bqneval(code)
    end
  end
end

function test_bytecode(only=nothing)
  cases = [
    (5, "5                       "), #  0 PUSH,  7 RETN
    (3, "4⋄3                     "), #  6 POPS
    (5, "a←5                     "), # 33 VARM, 48 SETN
    (4, "a←5⋄a↩4                 "), # 49 SETU
    (2, "a←2⋄b←3⋄a               "), # 32 VARO
    (1, "a←1⋄A 4                 "), # 16 FN1C
    (2, "a←2⋄3 A 4               "), # 17 FN2C
    (6, "{𝕩}6                    "), #  1 DFND
    (3, "A←{𝕨}⋄3 A 4             "), #    dyadic block function
    (2, "a‿b←7‿2⋄b               "), # 11 ARRO, 12 ARRM
    (7, "a‿b←7‿2⋄a               "), # 11 ARRO, 12 ARRM
    (2, "·‿b←7‿2⋄b               "), # 36 NOTM
    (2, "0{𝕨𝕏1}2                 "), # 19 FN2O (doesn't test call type)
    (5, "{({𝕨}𝕨)𝕏𝕩}5             "), # 18 FN1O not called
    (8, "{𝕩{a‿b←𝕨}𝕨,𝕩}8          "), # 19 FN2O not called
    (4, "4{𝔽}                    "), # 26 MD1C
    (6, "4{𝔽⋄𝕩}6                 "), #    deferred modifier
    (1, "3{𝔾}{𝕩} 1               "), # 27 MD2C
    (2, "(2{𝔽}{𝕩})3              "), # 20 TR2D
    (3, "3({a‿b←𝕩⋄a}{𝕨‿𝕩})4      "), #    dyadic 2-train
    (4, "4({𝕨‿𝕩}{𝕩}{𝕨})5         "), # 21 TR3D
    (2, "a‿b←(2{𝕨‿𝕩}{𝕩})5⋄a      "), #    monadic, data in left branch
    (2, "({a↩2⋄𝕩}{𝕩⋄a}{a↩3⋄𝕩})a←4"), # ordering
    (8, "a←3⋄a{𝕩}↩8⋄a            "), # 50 SETM
    (5, "a←4⋄a{𝕨⋄5}↩6            "), #    returns new value
    (1, "a←3⋄a{𝕩⋄1}↩⋄a           "), # 51 SETC
    (4, "a‿b←2‿1⋄a‿b{𝕩‿𝕨}↩4⋄a    "), #    lists

    (7, "     {𝕨{a←𝕩⋄{a↩𝕩}𝕨⋄a}𝕩}7"),  # 19 FN2O call type
    (3, "    3{𝕨{a←𝕩⋄{a↩𝕩}𝕨⋄a}𝕩}7"),  # 19 FN2O call type
    (7, "{𝕏0} {𝕨{a←𝕩⋄{a↩𝕩}𝕨⋄a}𝕏}7"),  # 23 TR3O call type
    (3, "{𝕏0}3{𝕨{a←𝕩⋄{a↩𝕩}𝕨⋄a}𝕏}7"),  # 23 TR3O call type

    (1, "a←1⋄{a←2}⋄a"),
    (2, "a←1⋄{a↩2}⋄a"),
    (6, "f‿g←{a←2⋄{a↩𝕩}‿{𝕩⋄a}}⋄F 6⋄G 0"),
    (5, "L←{𝕩{𝕏𝕗}}⋄{𝕏𝕤}L L L 5"),
    (3, "_l←{𝕩{𝕏𝕗} 𝔽}⋄{𝕏𝕤} {𝕩}_l 3 _l 5"),
    (1, "1{𝕨}{𝔽{𝕩𝔽𝕨}𝔾𝔽}{𝕩}0"),    # 0≠1 via Church booleans
    (2, "0‿(0‿{𝕩}){{a‿b←𝕩⋄t←𝕤⋄{𝕤⋄T↩{𝕤⋄{a‿b←𝕩⋄a}}}{B𝕗}0⋄(T b){a‿b←𝕩⋄𝔽b}}𝕗} 0‿(1‿(2‿(3‿(4‿{𝕩}))))"),
  ]
  run_testsuite(cases, only=only, title="Bytecode")
end

function test_simple(only=nothing)
  cases = [
          (2    , "1+1"),
          (-2   , "1⌊-2"),
          (-1   , "-2⌊1"),
          (1    , "(÷2)+(÷3)+(÷6)"),
          (4    , "⊢4⊣5"),
          (0    , "sq←√5,⌊9×|5-sq×sq"),
          (-0.5 , "((-3)+√(3×3)-4×2×1)÷2×2"),
          (-0.5 , "a←2,b←3,c←1⋄((-b)+√(b×b)-4×a×c)÷2×a"),
          (-0.5 , "b←1+a←1+c←1⋄((-b)+√(b×b)-4×a×c)÷2×a"),
          (-0.5 , "b←3⋄⊢d←(b×b)-4×2×1⋄((-b)+√d)÷2×2"),
          (6    , "a←3,b←4,c←5⋄⊣s←(÷2)×a+b+c⋄√s×(s-a)×(s-b)×(s-c)"),
          (3.1415, "t←2×5⋄3+(1+(4+(1+5÷t)÷t)÷t)÷t"),
          (3.1415, "3+(1+(4+(1+5÷10)÷10)÷10)÷10"),
          (3    , "√25-16"),
          (0.25 , "¬15÷20"),
          (0    , "(3∧4)-¬(¬3)∨(¬4)"),
          (1    , "p←¬q←÷4⋄(q∧q)+(p∨p)"),
          (109  , "105¬-3"),
          (-0.5 , "{{-3}+√{3×3}-4×2×1}÷2×2"),
          (1    , "{a←1⋄{a←2}⋄a}"),
    ]
  run_testsuite(cases, only=only, title="Simple")
end

function test_prim_0(only=nothing)
  cases = [
           (1, """0≡¯2+2"""),
           (1, """1e4≡5e3+5e3"""),
           (1, """'c'≡'a'+2"""),
           (1, """'a'≡¯2+'c'"""),
           (MethodError, """'a'+'c'"""),
           (MethodError, """F←-⋄f+2"""),
           (1, """¯∞≡1e6-∞"""),
           (1, """4≡-¯4"""),
           (1, """¯∞≡-∞"""),
           (1, """∞≡-¯∞"""),
           (1, """4≡9-5"""),
           (1, """@≡'a'-97"""),
           (1, """3≡'d'-'a'"""),
           (1, """'Q'≡'q'+'A'-'a'"""),
           (MethodError, """97-'a'"""),
           (InexactError, """@-1"""),
           (MethodError, """-'a'"""),
           (MethodError, """F←÷⋄-f"""),
           (1, """1.5≡3×0.5"""),
           (MethodError, """2×'a'"""),
           (1, """4≡÷0.25"""),
           (1, """∞≡÷0"""),
           (1, """0≡÷∞"""),
           (MethodError, """÷'b'"""),
           (MethodError, """F←√-⋄÷f"""),
           (1, """1≡⋆0"""),
           (1, """¯1≡¯1⋆5"""),
           (1, """1≡¯1⋆¯6"""),
           (MethodError, """⋆'π'"""),
           (MethodError, """'e'⋆'π'"""),
           (1, """3≡⌊3.9"""),
           (1, """¯4≡⌊¯3.9"""),
           (1, """∞≡⌊∞"""),
           (1, """¯∞≡⌊¯∞"""),
           (1, """¯1e30≡⌊¯1e30"""),
           (MethodError, """F←⌈⋄⌊f"""),
           (1, """1≡1=1"""),
           (1, """0≡¯1=∞"""),
           (1, """1≡'a'='a'"""),
           (1, """0≡'a'='A'"""),
           (1, """1≡{F←+⋄f=f}"""),
           (1, """1≡{a‿b←⟨+´,+´⟩⋄a=b}"""),
           (1, """0≡{_op←{𝕗}⋄op='o'}"""),
           (1, """0≡{F←{𝕩}⋄G←{𝕩}⋄f=g}"""),
           (1, """1≡{F←{𝕩}⋄f=f}"""),
           (1, """1≡1≤1"""),
           (1, """1≡¯∞≤¯1e3"""),
           (1, """0≡∞≤¯∞"""),
           (1, """1≡∞≤@"""),
           (1, """0≡'z'≤¯0.5"""),
           (1, """1≡'a'≤'a'"""),
           (1, """0≡'c'≤'a'"""),
           (MethodError, """F←+⋄G←-⋄f≤g"""),
           (1, """⟨⟩≡≢<2"""),
           (1, """⟨3⟩≡≢"abc" """),
           (1, """⟨2,3⟩≡≢>"abc"‿"fed" """),
           (1, """⟨2,3,4,5⟩≡≢2‿3‿4‿5⥊↕120"""),
           (1, """⟨6⟩≡≢⥊>"abc"‿"fed" """),
           (1, """"abc"≡0⊑"abc"‿"de" """),
           (1, """"de"≡1⊑"abc"‿"de" """),
           (1, """⟨⟩≡↕0"""),
           (1, """⟨0⟩≡↕1"""),
           (1, """⟨0,1,2,3,4,5,6⟩≡↕7"""),
           (1, """1≡!1"""),
           (1, """1≡'e'!1"""),
           (BQNError, """!0"""),
           (BQNError, """"error"!"abc" """),
  ]
  run_testsuite(cases, only=only, title="Prim, Layer 0")
end

function test_prim_1(only=nothing)
  cases = [
           (1, "3≡4>◶+‿-1"),
           (1, "3≡4⊢◶+‿-1"),
           (1, "3≡4 1◶+‿-1"),
           (1, "5≡4<◶+‿-1"),
           (1, "5≡4 0◶+‿-1"),
           (1, "1≡-⊘0 ¯1"),
           (1, "1≡¯1-⊘+2"),
           (1, """ "abc"≡⊢"abc" """),
           (1, """ ""≡3⊢"" """),
           (1, "⟨⟩≡⊣⟨⟩"),
           (1, """ "ab"≡"ab"⊣⟨⟩ """),
           (1, "4≡+˜2"),
           (1, "3≡1-˜4"),
           (1, "1≡-∘×¯6"),
           (1, "¯6≡2-∘×3"),
           (1, "1≡-○×¯7"),
           (1, "2≡5-○×¯7"),
           (1, "¯20≡1⊸-⊸×5"),
           (1, """ (0‿2+⌜0‿1)≡(>⟨"ab","cd"⟩)≢⊸⥊↕4 """),
           (1, "20≡×⟜(-⟜1)5"),
           (1, "4≡5+⟜×¯3"),
           (1, "7≡5+⟜2 ¯3"),
           (1, "2≡√4"),
           (1, "3≡3√27"),
           (MethodError, "√'x'"),
           (1, "6≡2∧3"),
           (1, "0≡¯2∧0"),
           (MethodError, "'a'∧¯1"),
           (1, "0.75≡∨˜0.5"),
           (1, "1.75≡2∨0.25"),
           (MethodError, "F←-⋄2∨f"),
           (1, "0≡¬1"),
           (1, "1≡¬0"),
           (1, "2≡¬¯1"),
           (MethodError, "¬'a'"),
           (1, "0≡3¬4"),
           (1, "2≡4¬3"),
           (1, "4≡5¬2"),
           (1, "5≡'g'¬'c'"),
           (1, "'b'≡'c'¬2"),
           (MethodError, "2¬'c'"),
           (MethodError, "F←{𝕩}⋄0¬f"),
           (1, "0≡|0"),
           (1, "5≡|¯5"),
           (1, "6≡|6"),
           (1, "∞≡|¯∞"),
           (MethodError, "F←+-⋄|f"),
           (1, "2≡3|8"),
           (1, "2≡3|¯7"),
           (1, "¯1≡¯3|8"),
           (MethodError, "26|'A'"),
           (1, """ "a"≡⥊<'a' """),
           (1, """ "abcd"≡⊑<"abcd" """),
           (1, "⟨⟩≡≢<⟨2,⟨3,4⟩⟩"),
           (1, "0≡4<2"),
           (1, "0≡5>5"),
           (1, "0≡3≥4"),
           (1, """ 0≡≠"" """),
           (1, """ 1≡≠"a" """),
           (1, "1≡≠'a'"),
           (1, """ 2≡≠"ab" """),
           (1, "25≡≠↕25"),
           (1, "1≡×5"),
           (1, "¯1≡×¯2.5"),
           (1, "3≡3⌊4"),
           (1, "¯3≡¯3⌊∞"),
           (1, "4≡3⌈4"),
           (1, "1≡1⌈¯1"),
           (1, "5≡⌈4.01"),
           (1, "⟨⟩≡≢'a'"),
           (1, "⟨⟩≡≢0"),
           (1, "⟨0⟩‿⟨1⟩‿⟨2⟩≡⥊¨↕3"),
           (1, """(↕6)≡⟜(≠¨)○(2‿3⊸⥊)⟨⟩‿"a"‿"ab"‿"abc"‿"abcd"‿"abcde"‿"abcdef" """),
           (1, "≡⟜(≠¨)4‿0‿2⥊↕0"),
           (1, "6≡+´↕4"),
           (1, """ (⊑≡⊣´)"a"‿2‿(3‿"d") """),
           (1, """ 0(⊑≡⊣´)"a"‿2‿(3‿"d") """),
           (1, """ (2⊸⊑≡⊢´)"a"‿2‿(3‿"d") """),
           (BQNError, """ ⊑"" """),
           (BQNError, "⊑2‿0⥊⟨⟩"),
           (1, """ 2(⊣≡⊢´)"a"‿2‿(3‿"d") """),
           (1, "7‿10≡+¨´⟨⟨2,3⟩,⟨5,7⟩⟩"),
           (BQNError, "+´11"),
           (BQNError, "-´<'a'"),
           (BQNError, """ ×´3‿1⥊"abc" """),
  ]
  run_testsuite(cases, only=only, title="Prim, Layer 1")
end

function test_prim_2(only=nothing)
  cases = [
           (1, """ ⟨⟩≡⟨⟩∾"" """),
           (1, """ "a"≡⟨⟩∾"a" """),
           (1, """ "a"≡"a"∾⟨⟩ """),
           (1, """ "aBCD"≡"a"∾"BCD" """),
           (1, """ ((+⌜˜≠¨)≡(≠¨∾⌜˜))""‿⟨2,3⟩‿"abcde" """),
           (1, """ (⥊⟜(↕×´)≡(×⟜4)⊸(+⌜)○↕´)3‿4 """),
           (1, """ (⥊⟜(↕×´)≡(×⟜4)⊸(+⌜)○↕´)0‿4 """),
           (1, """ (3‿2‿0⥊"")≡(3‿2⥊↕6)+⌜"" """),
           (1, """ (<-2)≡-¨2 """),
           (1, """ (<<2)≡<¨2 """),
           (1, """ ⟨1,⟨3,2,2‿2⥊⟨1,0,2,0⟩⟩,⟨5,4⟩⟩≡-⟨-1,⟨-3,-2,-¨2‿2⥊⟨1,0,2,0⟩⟩,⟨-5,-4⟩⟩ """),
           (1, """ 3(+¨≡+⌜)↕6 """),
           (BQNError, """ 2‿3⊢¨4‿5‿6 """),
           (BQNError, """ "abcd"-"a" """),
           (1, """ 3‿4‿5‿6‿6≡{𝕊⍟(×≡)⊸∾⟜⥊´𝕩}⟨2,1⟩+⟨⟨⟨⟨1,2⟩,3⟩,4⟩,5⟩ """),
           (1, """ 3‿2≡≢(↕3)(⊣×⊢⌜)↕2 """),
           (1, """ (<-4)≡-<4 """),
           (1, """ (<2)≡1+<1 """),
           (BQNError, """ (↕4)×(↕3)⊢⌜↕2 """),
           (1, """ (=¨⟜(⥊⟜(↕×´)3‿4)≡(↕4)=⌜˜4|⊢)1‿6‿8 """),
           (1, """ 0‿1≡+‿-=⊑⟨-⟩ """),
          ]
  run_testsuite(cases, only=only, title="Prim, Layer 2")
end

function test_prim_3(only=nothing)
  cases = [
           (1, """ 2≡⊑2 """),
           (1, """ 2≡⊑⟨2⟩ """),
           (1, """ "ab"≡⊑⟨"ab"⟩ """),
           (1, """ 0≡⊑↕20 """),
           (1, """ 4≡⊑3‿2‿1⥊4⥊⊸∾5⥊0 """),
           (1, """ 'c'≡2⊑"abcd" """),
           (1, """ 'c'≡¯2⊑"abcd" """),
           (1, """ 7≡7⊑↕10 """),
           (1, """ 7≡⟨7⟩⊑↕10 """),
           (1, """ 0≡¯10⊑↕10 """),
           (BQNError, """ 10⊑↕10 """),
           (BQNError, """ ¯11⊑↕10 """),
           (BQNError, """ 0.5⊑↕10 """),
           (BQNError, """ 'x'⊑↕10 """),
           (BQNError, """ ⟨⟩⊑↕10 """),
           (1, """ 21≡2‿¯3⊑(10×↕3)+⌜↕4 """),
           (BQNError, """ 2⊑3+⌜○↕4 """),
           (1, """ 21‿12‿03≡⟨2‿¯3,1‿2,0‿¯1⟩⊑(10×↕3)+⌜↕4 """),
           (BQNError, """ 21‿12‿03≡⟨2‿¯3‿0,1‿2,0‿¯1⟩⊑(10×↕3)+⌜↕4 """),
           (BQNError, """ ⟨2,⟨3⟩⟩⊑↕4 """),
           (BQNError, """ (<2)⊑↕4 """),
           (BQNError, """ (≍≍2)⊑↕4 """),
           (BQNError, """ ⟨≍1‿2⟩⊑↕5‿5 """),
           (1, """ "dfeb"≡(⥊¨-⟨3,1,2,5⟩)⊑"abcdef" """),
           (1, """ "abc"≡⟨⟩⊑<"abc" """),
           (1, """ 'a'≡⟨⟩⊑'a' """),
           (1, """ ⟨7,7‿7,7⟩≡⟨⟨⟩,⟨⟨⟩,⟨⟩⟩,⟨⟩⟩⊑<7 """),
           (1, """ ⟨7,⟨7,<7⟩⟩≡⟨⟨⟩,⟨⟨⟩,<⟨⟩⟩⟩⊑7 """),
           (1, """ "abcfab"≡⥊(↕2‿3)⊑5‿5⥊"abcdef" """),
           (1, """ "aedcaf"≡⥊(-↕2‿3)⊑5‿5⥊"abcdef" """),
           (BQNError, """ ↕@ """),
           (BQNError, """ ↕2.4 """),
           (BQNError, """ ↕<6 """),
           (BQNError, """ ↕≍2‿3 """),
           (BQNError, """ ↕¯1‿2 """),
           (1, """ (<6⥊0)(⊑≡<∘⊑∘⊢)(6⥊1)⥊5 """),
           (1, """ ¯6≡1‿0◶(2‿2⥊0‿0‿-‿0)6 """),
           (BQNError, """ -˙◶÷‿× 4 """),
           (1, """ ⟨3⟩≡⥊3 """),
           (1, """ (⟨⟩⊸⥊≡<)3 """),
           (1, """ ⟨3,3,3⟩≡3⥊3 """),
           (1, """ ⟨3,3,3⟩≡3<⊸⥊3 """),
           (BQNError, """ ¯3⥊3 """),
           (BQNError, """ 1.6‿2.5⥊↕4 """),
           (BQNError, """ (≍2‿3)⥊↕3 """),
           (BQNError, """ "     "≡5⥊"" """),
           (1, """ 6(⊢⌜≡∾○≢⥊⊢)○↕3 """),
           (1, """ (<≡↕)⟨⟩ """),
           (1, """ (↕∘⥊≡⥊¨∘↕)9 """),
           (1, """ ∧´(⟨∘⟩⊸⥊≡⥊)¨ ⟨4,↕4,↕2‿4⟩ """),
           (BQNError, """ 4‿∘⥊↕15 """),
           (1, """ 1‿2‿3‿0‿1≡⥊5‿⌽⥊↑‿4⥊3‿⌊⥊1+↕4 """),
           (1, """ ≡´⟨2‿⌽‿4,2‿3‿4⟩⥊¨<↕19 """),
           (1, """ ¬'a'≡<'a' """),
           (1, """ ¬"a"≡≍"a" """),
           (1, """ ¬⟨1,2,⟨4,4⟩,5⟩≡○(2‿2⊸⥊)⟨1,2,⟨3,4⟩,5⟩ """),
           (1, """ ¬2‿3‿4≡2‿3 """),
           (1, """ ¬1.001≡1.002 """),
           (1, """ 'a'≢2 """),
           (1, """ 2≢<2 """),
           (1, """ 2‿3≢2‿4 """),
           (1, """ 2‿3≢≍2‿3 """),
           (1, """ 0≡≡'a' """),
           (1, """ 1≡≡↕6 """),
           (1, """ 2≡≡↕2‿4 """),
           (1, """ 3≡≡<<<4 """),
           (1, """ (1¨≡-○≡˜⟜↕¨)⟨0,⟨⟩,⟨1⟩,2,⟨3,4⟩⟩ """),
           (1, """ 2≡≡⟨5,⟨'c',+,2⟩⟩ """),
           (1, """ 0≡≡⊑⟨-⟩ """),
  ]
  run_testsuite(cases, only=only, title="Prim, Layer 3")
end

function test_prim_4(only=nothing)
  cases = [
           (1, """ "a"≡⋈'a' """),
           (1, """ ({⟨𝕩⟩}≡⋈)'a'‿2 """),
           (1, """ "abc"‿1≡"abc"⋈1 """),
           (1, """ ⋈´⊸≡"ab" """),
           (1, """ ∧´≡⟜>¨⟨1,<'a',<∞,↕5,5‿3⥊2⟩ """),
           (1, """ 2‿3‿2≡≢>↕2‿3 """),
           (1, """ 2‿3≡>⟨<2,3⟩ """),
           (BQNError, """ >↕¨2‿3 """),
           (BQNError, """ >⟨⥊2,3⟩ """),
           (BQNError, """ >(≍⋈⊢)↕4 """),
           (1, """ ((4⥊2)⊸⥊≡(>2‿2⥊·<2‿2⥊⊢))"abcd" """),
           (1, """ (⊢≡>∘<)5‿3⥊↕15 """),
           (1, """ (⊢≡(><¨))5‿3⥊↕15 """),
           (1, """ (⥊≡≍)'a' """),
           (1, """ (⥊≡≍)<'a' """),
           (1, """ (1‿2⊸⥊≡≍)"ab" """),
           (1, """ 1‿2≡1≍2 """),
           (1, """ 2‿1(≍≡2‿2⥊∾)4‿3 """),
           (1, """ (≍⟜<≡≍˜)'a' """),
           (BQNError, """ 1‿0≍1‿2‿3 """),
           (BQNError, """ ≍⟜≍↕3 """),
           (BQNError, """ ⌽⎉1.1 ↕4 """),
           (BQNError, """ ⌽⎉'x' ↕4 """),
           (BQNError, """ ⌽⎉(<<0) ↕4 """),
           (BQNError, """ ⌽⎉≍ ↕4 """),
           (1, """ (≍˘˜⥊˘1‿5‿9)≡⌽⎉2⊸+⥊⟜(↕×´)3‿2‿1 """),
           (1, """ (<0)≡≡˘0 """),
           (1, """ (<1)≡≡˘<0 """),
           (1, """ (2⥊<<"ab") ≡ ⋈˜˘<"ab" """),
           (1, """ (3⥊0) ≡ {-}=˘↕3 """),
           (1, """ (↕4)(×⌜≡×⎉0‿2)↕5 """),
           (1, """ (↕4)(⋆˜⌜˜≡⋆⎉∞‿¯4)↕5 """),
           (1, """ (⟨2⟩⊸∾⍟(2‿2⥊0‿1‿1‿1)2‿3)≡≢¨≍⎉(⌊○=)⌜˜⟨↕3,2‿3⥊↕6⟩ """),
           (1, """ (2=⌜○↕3)≡(2‿4⥊"abc")≡⎉1(2‿3‿4⥊"abc") """),
           (1, """ ⟨0,0⟩≡(2‿4⥊"abc")≡⎉¯1(2‿3‿4⥊"abc") """),
           (BQNError, """ ⌽⚇2‿2.5 ↕3 """),
           (1, """ (-≡-⚇¯1)5 """),
           (1, """ ⟨5,⟨15,1⟩⟩≡+´⚇1⟨⟨3,2⟩,⟨⟨4,5,6⟩,⟨1⟩⟩⟩ """),
           (1, """ 5‿6‿15≡∾´+´⚇1⟨⟨0,1⟩,⟨⟨⟩⟩⟩⥊⊸∾⚇¯2‿1⟨⟨2,3⟩,⟨4,5,6⟩⟩ """),
           (1, """ (5⥊1)≡(↕5)=○=⚇0{≍} """),
           (BQNError, """ 2+⍟1‿'c'4 """),
           (BQNError, """ ⋆⍟1.5 2 """),
           (1, """ 4≡2+⍟¯1 6 """),
           (1, """ (2×↕7)≡2+⍟(¯3+↕7)6 """),
           (1, """ (3⌊↕5)≡{i←0⋄r←{i+↩1⋄1+𝕩}⍟(↕4)𝕩⋄r∾i}0 """),
           (1, """ (+⌜˜≡·>1+⍟⊢⊢)↕5 """),
           (1, """ 0‿1‿3‿6‿10≡+`↕5 """),
           (1, """ (-0‿1‿3‿6‿10)≡-`↕5 """),
           (1, """ ((0∾¨↕3)≍3⥊0)≡≡`↕2‿3 """),
           (1, """ ⟨⟩≡×`⟨⟩ """),
           (1, """ ≡⟜(!∘0`)3‿0‿2⥊"" """),
           (MethodError, """ +`4 """),
           (BQNError, """ +`<'c' """),
           (1, """ 2‿3‿5‿8‿12≡2+`↕5 """),
           (BQNError, """ 3‿4+`4+⌜○↕3 """),
           (1, """ (2⋆1‿2‿6×⌜0‿2)≡3‿4⋆`3+⌜○↕2 """),
  ]
  run_testsuite(cases, only=only, title="Prim, Layer 4")
end

function test_prim_5(only=nothing)
  cases = [
           (1, """ (<'a')≡⊏"abc" """),
           (BQNError, """ ⊏"" """),
           (1, """ "a"≡⊏⥊˘"abc" """),
           (BQNError, """ ⊏0‿3⥊"" """),
           (1, """ (<'c')≡2⊏"abc" """),
           (BQNError, """ 3⊏"abc" """),
           (BQNError, """ 1.5⊏"abc" """),
           (BQNError, """ 'x'⊏"abc" """),
           (1, """ (<'c')≡¯1⊏"abc" """),
           (1, """ "ccc"≡2‿¯1‿2⊏"abc" """),
           (BQNError, """ ⟨⥊0,1⟩⊏≍"abc" """),
           (1, """ ((3-˜↕5)⊸⊏≡2⊸⌽)↕5‿2 """),
           (1, """ (0‿3⥊0)≡⟨⟩⊏2‿3⥊↕6 """),
           (1, """ ⟨3‿0,2‿1‿2⟩(×⟜5⊸+⌜´∘⊣≡⊏)⥊⟜(↕×´)6‿5 """),
           (BQNError, """ 0‿0<¨⊸⊏"abc" """),
           (1, """ (2‿0⥊0)≡⟨3‿¯1,⟨⟩⟩⊏4‿3⥊0 """),
           (BQNError, """ ⟨3‿¯∞,⟨⟩⟩⊏4‿3⥊0 """),
           (1, """ 5‿1(<⊸⊏≡⊏)↕6‿2 """),
           (BQNError, """ (≍≍<5‿1)⊏↕6‿2 """),
           (1, """ ⟨4‿0,1‿2‿3‿2‿1‿0⟩(+⌜´⊸(×⌜)≡⊏⟜(×⌜˜))+⌜˜↕5 """),
           (1, """ ∧´1=≡¨(<⟨⟩)(↑¨∾↓¨)⟨@,+,<@,↕3⟩ """),
           (1, """ "abc"≡3↑"abce" """),
           (1, """ "e"≡¯1↑"abce" """),
           (1, """ ""≡0↑"ab" """),
           (BQNError, """ 2.5↑"abce" """),
           (1, """ (<⟜3⊸×↕5)≡5↑↕3 """),
           (1, """ (6⥊0)≡¯6↑↕0 """),
           (1, """ (≍↕3)≡1↑2‿3⥊↕6 """),
           (1, """ (↑⟜4≡⥊⟜0)↕3 """),
           (1, """ (≍"abc")≡(<1)↑2‿3↑"abcd" """),
           (BQNError, """ 2‿'c'↑"abcd" """),
           (BQNError, """ (≍2‿3)↑"abcd" """),
           (1, """ (6⥊1)(↑≡⥊⟜⊑)2‿3⥊↕6 """),
           (1, """ (↕¨∘↕∘(1⊸+)≡↑∘↕)5 """),
           (1, """ (↑≡((↕4)≍¨2)⥊¨<)3‿2⥊"abcdef" """),
           (1, """ "d"≡3↓"abcd" """),
           (BQNError, """ 0.1↓"abcd" """),
           (BQNError, """ ⟨∘⟩↓"abcd" """),
           (1, """ 1‿2≡⟜(¯3⊸↓)○↕4‿2 """),
           (1, """ 1‿1‿3‿2‿1≡≢(5⥊0)↓↕3‿2‿1 """),
           (1, """ (↓∘↕≡↕∘(1⊸+)+⟜⌽↑∘↕)5 """),
           (1, """ (↕3‿4)≡1↓¨⊏↕2‿3‿4 """),
           (1, """ (4+⌜○↕2)≡2↕↕5 """),
           (BQNError, """ @↕↕5 """),
           (BQNError, """ 2‿1↕↕5 """),
           (BQNError, """ ¯1↕↕5 """),
           (BQNError, """ 7↕↕5 """),
           (1, """ ⟨⟩(↕≡⊢)4‿3⥊"abcd" """),
           (1, """ (0⊸↕≡(0≍˜1+≠)⊸⥊)↕6 """),
           (1, """ (7↕6‿0⥊"")≡0‿7‿0⥊"" """),
           (BQNError, """ 'a'«'b' """),
           (BQNError, """ "a"»'b' """),
           (BQNError, """ ≍⊸»"abc" """),
           (1, """ (»˜⊸≡∧«˜⊸≡)"" """),
           (1, """ "a"≡⟨⟩»"a" """),
           (1, """ ⟨⟩≡"a"»⟨⟩ """),
           (1, """ "aBC"≡"a"»"BCD" """),
           (1, """ "CDa"≡"a"«"BCD" """),
           (1, """ "d"≡"abcd"«⟨4⟩ """),
           (1, """ ((⊢⌜˜≠¨)≡(≠¨«⌜˜))""‿⟨2,3⟩‿"abcde" """),
           (1, """ "Zcab"≡"WXYZ"«´"ab"‿"c"‿"" """),
           (1, """ "dab"≡'d'»"abc" """),
           (1, """ "dab"≡'d'<⊸»"abc" """),
           (1, """ (1⊸⌽≡⊏⊸«)'a'+⥊⟜(↕×´)4‿2 """),
           (1, """ ¯2(⌽≡↑»⊢)'a'+⥊⟜(↕×´)4‿2 """),
           (1, """ 6(↑≡»⟜(⥊⟜0)˜)↕4 """),
           (1, """ «˜⊸≡2‿3⥊"abcdef" """),
           (1, """ (»≡0⌈-⟜1)↕6 """),
           (1, """ («≡1⊸⌽)↕6 """),
           (1, """ (»≡0⌈-⟜2)⥊⟜(↕×´)5‿2 """),
           (1, """ («≡1⌽1⊸<⊸×)⥊⟜(↕×´)5‿2 """),
           (BQNError, """ ⌽'a' """),
           (BQNError, """ ⌽<∞ """),
           (1, """ ≡⟜⌽⟨⟩ """),
           (1, """ ≡⟜⌽"a" """),
           (1, """ "ba"≡⟜⌽"ab" """),
           (1, """ (⌽≡(1-˜≠)(-○⊑∾1↓⊢)⚇1⊢)↕3‿2‿4 """),
           (1, """ ≡⟜⌽↕↕3 """),
           (BQNError, """ 2⌽'a' """),
           (BQNError, """ 1‿2⌽↕4 """),
           (BQNError, """ ⌽‿2⌽3+⌜○↕4 """),
           (BQNError, """ (<<3)⌽↕4 """),
           (1, """ ∧´5(⌽≡⊢)¨⟨"",⥊∞,↕5,↕0‿4,2‿0‿3⥊""⟩ """),
           (1, """ ∧´("bcdea"≡⌽⟜"abcde")¨1+5×¯10‿¯2‿¯1‿0‿1‿6‿61 """),
           (1, """ ∧´⟨1,0‿2,¯1‿1‿3⟩(⊑∘⌽≡(3⊸↑)⊸⊑)⚇¯1‿∞ 2‿3‿5⥊"abcdef" """),
           (1, """ (⟨⟩⊸⌽≡<)'a' """),
           (BQNError, """ /2 """),
           (BQNError, """ /1‿¯1‿0 """),
           (BQNError, """ /=⌜˜↕2 """),
           (1, """ 0‿4≡/1‿0‿0‿0‿1‿0 """),
           (1, """ 1‿1‿2≡/0‿2‿1 """),
           (1, """ ≡⟜/⟨⟩ """),
           (BQNError, """ 2/<2 """),
           (BQNError, """ 0‿1/"abc" """),
           (BQNError, """ ⟨↕3,↕3⟩/"abc" """),
           (BQNError, """ 1‿2/○≍"ab" """),
           (BQNError, """ ¯1‿2/"ab" """),
           (1, """ "aabbcc"≡2/"abc" """),
           (1, """ ""≡4/"" """),
           (1, """ (6‿0⥊"")≡⟨5,1⟩‿⟨⟩/2‿0⥊"" """),
           (1, """ 3‿3‿3‿2‿2‿1≡/˜3‿2‿1 """),
           (1, """ 3‿3‿3‿2‿2‿1≡<⊸/3‿2‿1 """),
           (1, """ (≍1∾¨1‿2‿2)≡(↕¨/↕)2‿3 """),
           (1, """ (⟨⟩⊸/≡<)'a' """),
           (1, """ ⟨⟩(/≡⊢)↕10 """),
           (1, """ ⟨⟩(/≡⊢)≍"ab" """),
           (1, """ ⟨2,<3⟩(/≡⥊˜¨⟜≢/⊢)'a'+4‿2⥊↕8 """),
  ]
  run_testsuite(cases, only=only, title="Prim, Layer 5")
end

function test_prim_6(only=nothing)
  cases = [
           (BQNError, """ ∾'c' """),
           (1, """ ≡⟜(∾⥊¨)"abc" """),
           (1, """ (∾´≡∾)"ab"‿"cde"‿"" """),
           (BQNError, """ ∾"abc" """),
           (BQNError, """ ∾≍"ab"‿"cde"‿"" """),
           (1, """ "abc"≡∾"ab"‿'c'‿"" """),
           (1, """ 1‿2‿3‿4‿6‿9≡∾(⊢×≠↑↓)1+↕3 """),
           (1, """ (≡⟜∾∧≡⟜(∾<))<4 """),
           (1, """ ⟨1‿4,⥊2⟩((∾⋆⌜⌜)≡⋆⌜○∾)⟨2‿3‿4,⟨⟩,⥊5⟩ """),
           (1, """ (6‿3⥊0)≡∾⟨2‿3,3,3‿3⟩⥊¨0 """),
           (BQNError, """ ∾⟨2‿3,1‿3,2‿2⟩⥊¨0 """),
           (1, """ "abcd"≡"abc"∾'d' """),
           (1, """ "abcd"≡"abc"∾<'d' """),
           (1, """ (↕4‿3)≡(↕3‿3)∾3∾¨↕3 """),
           (1, """ (∾˜≡·¯1⊸(×´∘↓∾↑)∘≢⊸⥊≍˜)2‿3⥊"abcdef" """),
           (1, """ (∾´≡∾)⟨3‿2‿1,0‿2‿1⟩⥊¨<↕6 """),
           (BQNError, """ 'a'∾≍"abc" """),
           (BQNError, """ "ab"∾○≍"cde" """),
           (BQNError, """ (2‿3⥊↕6)∾↕2 """),
           (1, """ ⟨1‿2,⥊0,⥊3⟩≡⊔1‿0‿0‿2 """),
           (1, """ ⟨⟩≡⊔5⥊¯1 """),
           (1, """ ≡⟜⊔⟨⟩ """),
           (BQNError, """ ⊔3 """),
           (BQNError, """ ⊔<3 """),
           (BQNError, """ ⊔≍↕3 """),
           (BQNError, """ ⊔1.5‿0‿2 """),
           (BQNError, """ ⊔1‿¯2 """),
           (1, """ (⊔≡⥊¨¨∘⊔∘⊑)⟨1‿0‿0‿2⟩ """),
           (1, """ (≍⍟2∘<¨⌽↕3‿2)≡⊔⟨2‿1‿0,0‿1⟩ """),
           (1, """ (↕0‿0)≡⊔⟨⟩‿⟨⟩ """),
           (1, """ (⊔≡·≍⍟2∘<·∾⌜´/∘(0⊸=)¨)⟨0‿¯1‿0‿0,¯1‿0‿0⟩ """),
           (1, """ (0‿0‿1↑⌜≍⍟2∘<∘⥊¨1‿0)≡⊔⟨2,1‿0⟩ """),
           (1, """ (0‿0‿1↑⌜≍⍟2∘(<0‿0‿0⊸∾)¨1‿0)≡⊔0‿0⊸↓¨⟨2,1‿0⟩ """),
           (1, """ 4‿3‿2(⋈≡·(≠¨⋈∾)/⊸⊔)"abcdefghi" """),
           (1, """ ⟨⟩≡(3⥊¯1)⊔"abc" """),
           (1, """ ⟨⟩≡(2⥊¯1)⊔"a" """),
           (BQNError, """ ⊔˜'a'‿1‿0 """),
           (BQNError, """ 4⊔○↕2 """),
           (1, """ (≍˘1‿1‿4<∘⥊⎉1 16‿4+⌜↕4)≡2↓⟨3‿2,¯1‿0‿¯1⟩⊔2‿3‿4⥊↕24 """),
           (1, """ ⥊⚇0⊸≡○⊔⟜(⥊<)1‿2‿2‿¯1‿0 """),
           (1, """ (∾↕¨∘≢⊸⊔)⊸≡ 3‿2‿4⥊↕24 """),
           (1, """ -⟜'a'⊸(⊔≡⊔○⥊)"acc"≍"bac" """),
           (1, """ (2‿1/⟨↕0‿1,1‿1⥊3⟩)≡2⊔⥊3 """),
           (1, """ ((<=·↕1⊸+)≡·≢¨<¨⊸⊔⟜(<@))2‿1‿3 """),
           (BQNError, """ ⟨1‿2,3‿1⟩⊔2‿3⥊0 """),
           (BQNError, """ ⟨1‿2,3‿4‿5,6‿7⟩⊔2‿3⥊0 """),
           (BQNError, """ ≍⊸⊔≍˘↕3 """),
           (BQNError, """ ⟨⟨<3,2⟩,¯1‿0‿¯1⟩⊔2‿3‿4⥊↕24 """),
           (1, """ (1‿3/⟨"a",""⟩)≡0‿¯1‿4⊔"ab" """),
           (1, """ ¯1⊸↓⊸(≡○(⊔⟜"ab"))2‿3‿1 """),
           (1, """ (≍1‿1‿0≍∘/⟜≍¨"bac")≡⟨0,1‿0‿3⟩⊔"ab" """),
           (1, """ (⌽˘≡·∾⟨2‿2,1‿0‿1⟩⊸⊔)"ab"≍"cd" """),
           (BQNError, """ (2‿3⥊↕4)⊔↕2‿2 """),
           (BQNError, """ (3‿3⥊↕4)⊔↕2‿2 """),
           (BQNError, """ ⊐˜'a' """),
           (BQNError, """ ⊏⊸⊐"abc" """),
           (BQNError, """ (3‿2‿4⥊0)⊐4⥊1 """),
           (1, """ 2‿0‿4≡"abcd"⊐"cae" """),
           (1, """ ⟨1⟩≡"abcd"⊐"b" """),
           (1, """ (<2)≡"cdef"⊐'e' """),
           (1, """ (<3)≡⊐⟜(3⊸⊏)"abcd" """),
           (1, """ (5⌊3+↕5)≡⊐⟜(3‿0‿0+⚇1⊢)↕5‿2‿1 """),
           (BQNError, """ ⊐+˙@ """),
           (1, """ 0‿0‿1‿0‿2≡⊐"ccacb" """),
           (1, """ 0‿0‿1‿0‿2≡⊐≍˜˘"ccacb" """),
           (1, """ ≡⟜⊐⟨⟩ """),
           (BQNError, """ (↕5)∊1 """),
           (BQNError, """ 2∊≍˘↕4 """),
           (1, """ 1‿0‿0‿1≡"acef"∊"adf" """),
           (1, """ (∊⟜(↕2)≡<⟜2)3⋆⌜○↕5 """),
           (1, """ (<1)≡3‿4‿5∊4+⌜○↕3 """),
           (BQNError, """ ∊<4 """),
           (1, """ ('0'≠"11010001")≡∊"abacbacd" """),
           (1, """ (↑⟜1≡⟜∊⥊⟜∞)9 """),
           (1, """ (⥊⟜1≡∊∘↕)6 """),
           (1, """ ≡⟜∊⟨⟩ """),
           (1, """ ≡○∊⟜(≍˜˘)"abcadbba" """),
           (BQNError, """ ⍷'a' """),
           (1, """ ≡⟜⍷⟨⟩ """),
           (1, """ "ba"≡⍷"baa" """),
           (BQNError, """ ≍⊸⍷"abc" """),
           (1, """ 0‿1‿0‿0≡"abc"⍷"aabcba" """),
           (1, """ (0‿1≍0‿0)≡(1‿2≍4‿5)⍷3‿3⥊↕9 """),
           (1, """ (↕3‿0)≡⍷⟜(≍˘)"abc" """),
           (1, """ 'a'(=≡⍷)"abc" """),
           (1, """ (⌽¨≡⍉)↕2⥊3 """),
           (1, """ (⍉≡<)'a' """),
           (1, """ ∧´⍉⊸≡¨⟨<'a',"a","abc",""⟩ """),
           (1, """ (↕4)(-˜⌜˜≡·⍉-⌜)↕3‿2 """),
           (BQNError, """ 0‿¯1‿1⍉(3⥊1)⥊1 """),
           (BQNError, """ 1‿0≍˘⊸⍉"ab"≍"cd" """),
           (BQNError, """ 0‿2⍉+⌜˜↕3 """),
           (BQNError, """ 2‿0‿0⍉↕↕3 """),
           (BQNError, """ 3⍉↕↕3 """),
           (1, """ (2×↕3)≡0‿0⍉6+⌜○↕3 """),
           (1, """ (⟨⟩⊸⍉≡<)4 """),
           (1, """ ⟨⟩(⍉≡⊢)<4 """),
           (1, """ (2‿0‿1⥊⟨⟩)≡1‿2‿0‿1⍉↕↕4 """),
           (1, """ (↕1‿2‿0‿3)≡2<⊸⍉↕↕4 """),
           (1, """ 0⊸⍉⊸≡2‿3⥊↕6 """),
           (BQNError, """ ⍋'a' """),
           (MethodError, """ ⍋'a'‿∘ """),
           (BQNError, """ ⍒2 """),
           (1, """ 2‿0‿3‿1‿4≡⍋"bdace" """),
           (1, """ 5‿2‿4‿3‿0‿1≡⍋↓"deabb" """),
           (1, """ (⍋≡⍒)⟨"",↕0,0↑<"abc"⟩ """),
           (1, """ (⍋≡↕∘≠)4‿0⥊@ """),
           (1, """ (⍒≡⌽∘↕∘≠)⟨¯∞,¯1.5,π,∞,'A','a','b'⟩ """),
           # TODO: issue with number formatting...
           # (1, """ (⍒≡⌽∘↕∘≠)⟨↕0,¯1.1,¯1,¯1‿¯∞,¯1‿0,¯1‿0‿0,¯1‿∞,0,6⥊0,1e¯20,1,1+1e¯15⟩ """),
           (1, """ (⍒≡⌽∘↕∘≠)(<∾⟨↕0,1,1‿1,2‿1‿1,2‿1,2,1‿2,2‿2,3⟩⥊¨<)'a' """),
           (1, """ (⍋≡↕∘≠)⥊⍉(↕5)⥊⟜1⊸⥊⌜1‿'b' """),
           (1, """ (⊢≡○⍋(0‿1+≠)⥊⊢)⟨¯2,'a',1,'f'⟩ """),
           (1, """ ⟨1,2,3,1‿2,2‿1,1‿3,2‿2,3‿1⟩(⥊⊸(≠∘⊣∾˜¯1⊸⊑⊸(⌊∾⊣)∾×´⊸⌊)⌜≡○(⍋⥊)⥊⌜⟜(+`∘≠⟜(↕6)¨))↕4 """),
           (1, """ ((⥊˜-⥊⟜2‿0)∘≠≡⍋+⍒)2/↕5 """),
           (BQNError, """ ∧⊏⟨+⟩ """),
           (MethodError, """ ∧+‿- """),
           (BQNError, """ ∨'c' """),
           (1, """ "edcba"≡∨"bdace" """),
           (1, """ (↕7)≡∧⍋|⟜⌽1+↕7 """),
           (BQNError, """ ⍋˜6 """),
           (BQNError, """ ⍒⟜↕4 """),
           (BQNError, """ (3‿2‿4⥊0)⍋4⥊1 """),
           (BQNError, """ (3‿2‿4⥊0)⍒1 """),
           (MethodError, """ ⟨+⟩⍋↕6 """),
           (BQNError, """ ⟨1‿3‿1,1‿3‿2⟩⍒⟨1‿3‿{𝕩}⟩ """),
           (1, """ ⟨1,3,∞,'e','i'⟩ (⍋≡≠∘⊣(⊣↓⊢⍋⊸⊏+`∘>)⍋∘∾) (2÷˜↕8)∾"aegz" """),
           (1, """ ⟨'z','d',1‿0,0⟩ (⍒≡≠∘⊣(⊣↓⊢⍋⊸⊏+`∘>)⍒∘∾) (2÷˜↕8)∾"aegz" """),
           (1, """ (<∘⌈≡(↕6)⊸⍋)2.5 """),
           (1, """ (<1)≡(↕2‿3)⍋1+↕3 """),
           (1, """ (<0)≡"abc"⥊⊸⍒○<≍"acc" """),
  ]
  run_testsuite(cases, only=only, title="Prim, Layer 6")
end

function test_all()
  # pointless after we've tried to load the runtime but let's do it anyway
  test_bytecode()
  test_simple()
  test_prim_0()
  test_prim_1()
  test_prim_2()
  test_prim_3()
  test_prim_4()
  test_prim_5()
  test_prim_6()
end

function provide_decompose(𝕨, 𝕩)
  if     isa(𝕩, F);         Arr([1, 𝕩])
  elseif isa(𝕩, TR2D);      Arr([2, 𝕩.𝕘, 𝕩.h])
  elseif isa(𝕩, TR3D);      Arr([2, 𝕩.𝕗, 𝕩.𝕘, 𝕩.h])
  elseif isa(𝕩, TR3O);      Arr([2, 𝕩.𝕗, 𝕩.𝕘, 𝕩.h])
  elseif isa(𝕩, M1);        Arr([4, 𝕩.𝕗, 𝕩])
  elseif isa(𝕩, M2);        Arr([5, 𝕩.𝕗, 𝕩, 𝕩.𝕘])
  elseif 𝕩 in _provide_set; Arr([0, 𝕩])
  else                      Arr([-1, 𝕩])
  end
end

function provide_prim_ind(𝕨, 𝕩)
  for (idx, 𝕗) in enumerate(_provide);
    if 𝕗 === 𝕩; return idx; end
  end
  return length(_provide)
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
  provide_decompose,
  provide_prim_ind,
]
_provide_set = Set(𝕗 for 𝕗 in _provide)
provide(n::Int64) = _provide[n + 1]

# _runtime_0 = bqneval("r0")
# runtime_0(n::Int64) = _runtime_0[n + 1]

_runtime, set_prims, set_inv = bqneval("r")
runtime(n::Int64) = _runtime[n + 1]

end
