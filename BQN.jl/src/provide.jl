""" Core primitives required for r0."""
module Provide

using TimerOutputs

import ..none, ..None, ..BQNError, ..type, ..to
import ..F, ..FN, ..TR2D, ..TR3D, ..TR3O
import ..M1D, ..M1I, ..M1N, ..M2D, ..M2I, ..M2N, ..BQNF

@nospecialize

bqnadd(𝕨::None, 𝕩) = 𝕩
bqnadd(𝕨::Char, 𝕩::Float64) = 𝕨 + Int(𝕩)
bqnadd(𝕨::Float64, 𝕩::Char) = Int(𝕨) + 𝕩
bqnadd(𝕨, 𝕩) = float(𝕨 + 𝕩)

bqnsub(𝕨::None, 𝕩::Float64) = -𝕩
bqnsub(𝕨::Char, 𝕩::Float64) = 𝕨 - Int(𝕩)
bqnsub(𝕨, 𝕩) = float(𝕨 - 𝕩)

bqnmul(𝕨::None, 𝕩::Float64) = float(sign(𝕩))
bqnmul(𝕨::Float64, 𝕩::Float64) = float(𝕨 * 𝕩)

bqndiv(𝕨::None, 𝕩::Float64) = 1/𝕩
bqndiv(𝕨::Float64, 𝕩::Float64) = 𝕨/𝕩

bqnpow(𝕨::None, 𝕩::Float64) = ℯ^𝕩
bqnpow(𝕨::Float64, 𝕩::Float64) = if 𝕩>=0; float(𝕨^𝕩) else 1/(𝕨^(-𝕩)) end

bqnroot(root::None, v) = sqrt(v)
bqnroot(root, v) = v^(1/root)

bqnabs(𝕨::None, v) = float(abs(v))

bqnmin(𝕨::Float64, 𝕩::Float64) = float(min(𝕨, 𝕩))
bqnmin(𝕨::None, 𝕩::Float64) = float(floor(𝕩))

bqnnot(𝕨::None, 𝕩::Float64) = float(+(1 - 𝕩))
bqnnot(𝕨::Float64, 𝕩::Float64) = float(1 + (𝕨 - 𝕩))

bqnand(𝕨::Float64, 𝕩::Float64) = float(𝕨*𝕩)

bqnor(𝕨::Float64, 𝕩::Float64) = float((𝕨+𝕩)-(𝕨*𝕩))

bqnidleft(𝕨, 𝕩) = 𝕨

bqnidright(𝕨, 𝕩) = 𝕩

function bqnvalences(𝕘, 𝕗)
  @nospecialize
  𝕣 = bqnvalences
  run = function(𝕨, 𝕩)
    if 𝕨 === none
      𝕗(𝕨, 𝕩)
    else
      𝕘(𝕨, 𝕩)
    end
  end
  FN(run, 𝕘, 𝕣, 𝕗)
end

function bqncatch(𝕘, 𝕗)
  @nospecialize
  𝕣 = bqncatch
  run = function(𝕨, 𝕩)
    try
      𝕗(𝕨, 𝕩)
    catch e
      𝕘(𝕨, 𝕩)
    end
  end
  FN(run, 𝕘, 𝕣, 𝕗)
end

bqneq(𝕨::None, @nospecialize(𝕩::Array)) =
  @timeit_debug to "Provide.bqneqM" float(ndims(𝕩))
bqneq(𝕨::None, @nospecialize(𝕩)) = 0.0
bqneq(@nospecialize(𝕨), @nospecialize(𝕩)) =
  @timeit_debug to "Provide.bqneq" float(𝕨 == 𝕩)

bqnlte(𝕨, 𝕩) = float(𝕨 <= 𝕩)
bqnlte(𝕨::Float64, 𝕩::Char) = 1.0
bqnlte(𝕨::Char, 𝕩::Float64) = 0.0

bqnshape(𝕨, @nospecialize(𝕩::Array)) =
  reverse(Float64[x for x in size(𝕩)])
bqnshape(𝕨, @nospecialize(𝕩)) =
  Float64[]

bqndeshape(𝕨::None, @nospecialize(𝕩::Array)) =
  vec(𝕩)
bqndeshape(𝕨::None, @nospecialize(𝕩)) =
  [𝕩]

function bqndeshape(𝕨::Array, 𝕩::Array)
  @nospecialize
  @timeit_debug to "Provide.bqndeshape" begin
  size = reverse(Tuple(Int(x) for x in 𝕨))
  if size == Base.size(𝕩); return 𝕩 end
  reshape(𝕩, size)
  end
end

function bqndeshape(𝕨::Array, 𝕩::Any)
  @nospecialize
  @timeit_debug to "Provide.bqndeshape" begin
  @assert length(𝕨) == 0
  collect(𝕩)
  end
end

bqnpick(𝕨::Float64, 𝕩::Float64) = 𝕩
bqnpick(𝕨::Float64, @nospecialize(𝕩::Array)) =
  @timeit_debug to "Provide.bqnpick" begin
    𝕨 = Int(𝕨)
    if 𝕨 >= 0; 𝕩[𝕨 + 1] else 𝕩[end + (𝕨 + 1)] end
  end
bqnpick(𝕨::Int64, @nospecialize(𝕩::Array)) =
  @timeit_debug to "Provide.bqnpick" begin
    if 𝕨 >= 0; 𝕩[𝕨 + 1] else 𝕩[end + (𝕨 + 1)] end
  end
bqnpick(𝕨::None, @nospecialize(𝕩::Array)) = 𝕩[1]
bqnpick(𝕨::None, 𝕩) = 𝕩

bqnwindow(𝕨, 𝕩) =
  @timeit_debug to "Provide.bqnwindow" [x for x in 0:(𝕩-1)]

bqntable(𝕘::Nothing, @nospecialize(𝕗)) = FNTable(bqntable´, 𝕗)
bqntable´ = M1N(bqntable)

struct FNTable <: BQNF
  𝕣::M1N
  𝕗::Any
end

(𝕣::FNTable)(@nospecialize(𝕨), @nospecialize(𝕩)) =
  @timeit_debug to "Provide.bqntable" begin
    if 𝕨 === none
      𝕩 = if !isa(𝕩, Array); collect(𝕩) else 𝕩 end
      [𝕣.𝕗(none, x) for x in 𝕩]
    else
      𝕨 = if !isa(𝕨, Array); collect(𝕨) else 𝕨 end
      𝕩 = if !isa(𝕩, Array); collect(𝕩) else 𝕩 end
      rsize = (size(𝕩)..., size(𝕨)...)
      r = [𝕣.𝕗(w, x) for w in 𝕨 for x in 𝕩]
      reshape(r, rsize)
    end
  end

type(𝕩::FNTable) = 3.0

bqnscan(𝕘::Nothing, @nospecialize(𝕗)) = FNScan(bqnscan´, 𝕗)
bqnscan´ = M1N(bqnscan)

struct FNScan <: BQNF
  𝕣::M1N
  𝕗::Any
end

(𝕣::FNScan)(@nospecialize(𝕨), @nospecialize(𝕩)) = begin
  @timeit_debug to "Provide.bqnscan" begin
    bqnassert(
              "`: Argument cannot have rank 0",
              Int(ndims(𝕩) != 0))
    bqnassert(
              "`: Shape of 𝕨 must match the cell of 𝕩",
              Int(𝕨 == none ||
                  size(𝕨) == () && ndims(𝕩) == 1 ||
                  size(𝕨)[1:1] == size(𝕩)[1:1]))
    if 𝕨 == none
      # Any here is to allow heterogenous scans... try this: ≡`↕2‿2
      res = Array{Any}(undef, size(𝕩))
      accumulate!(𝕣.𝕗, res, 𝕩, dims=ndims(𝕩))
      res
    elseif size(𝕨) == ()
      accumulate(𝕣.𝕗, 𝕩, dims=ndims(𝕩), init=𝕨)
    else
      # Because accumulate() doesn't support init being an array we provide
      # init value by concatenating it over the major dimension with hvncat():
      ndims𝕩 = ndims(𝕩)
      𝕩 = hvncat(ndims𝕩, 𝕨, 𝕩)
      𝕩 = accumulate(𝕣.𝕗, 𝕩, dims=ndims𝕩)
      # ... but this will produce an extra "row" in this dimension so we
      # produce a view which "cuts" that out with a view over this array:
      # TODO: Revisit that for performance!
      indices = [(:) for _ in size(𝕩)[1:end - 1]]
      collect(@view 𝕩[indices..., 2:end])
    end
  end
end

type(𝕩::FNScan) = 3.0

bqnfill(𝕨::None, @nospecialize(𝕩::Array)) = 0.0
bqnfill(@nospecialize(𝕨), @nospecialize(𝕩)) = 𝕩

bqnlog(𝕨::None, 𝕩::Float64) = log(ℯ, 𝕩)
bqnlog(𝕨::Float64, 𝕩::Float64) = log(𝕨, 𝕩)

function bqngrouplen(𝕨, 𝕩::Array)
  @timeit_debug to "Provide.bqngrouplen" begin
  order = []
  lengths = Dict{Int,Float64}()
  max𝕩 = -1.0
  for x in 𝕩
    max𝕩 = max(max𝕩, x)
    if haskey(lengths, x)
      lengths[Int(x)] += 1.0
    else
      lengths[Int(x)] = 1.0
      push!(order, x)
    end
  end
  minl = max(max𝕩, 𝕨 !== none ? (𝕨 - 1) : -1)
  [get(lengths, x, 0.0) for x in 0:minl]
  end
end

function bqngroupord(𝕨, 𝕩::Array)
  @timeit_debug to "Provide.bqngroupord" begin
  indices = [[] for _ in 1:length(𝕨)]
  for (idx, x) in enumerate(𝕩)
    if x < 0; continue end
    push!(indices[Int(x) + 1], float(idx) - 1)
  end
  collect(vcat(indices...))
  end
end

function bqnassert(𝕨, 𝕩)
  if 𝕩 == 1
    1.0
  else
    # TODO: should we use 𝕩 as error message in case it's a string? r1.bqn
    # seems to be relying on that behaviour... see !∘"msg" pattern.
    msg =
      if 𝕨 === none
        if isa(𝕩, AbstractString) || isa(𝕩, Vector); 𝕩
        else "ERROR" end
      else 𝕨 end
    if isa(msg, Array); msg = join(msg) end
    throw(BQNError(msg))
  end
end

function bqnfillby(𝕘, 𝕗)
  @nospecialize
  𝕣 = bqnfillby
  run = function(𝕨, 𝕩)
    @nospecialize
    𝕗(𝕨, 𝕩)
  end
  FN(run, 𝕘, 𝕣, 𝕗)
end

bqntype(𝕨::None, @nospecialize(𝕩)) = type(𝕩)

@specialize

const value = [
  bqntype,
  bqnfill,
  bqnlog,
  bqngrouplen,
  bqngroupord,
  bqnassert,
  bqnadd,
  bqnsub,
  bqnmul,
  bqndiv,
  bqnpow,
  bqnmin,
  bqneq,
  bqnlte,
  bqnshape,
  bqndeshape,
  bqnpick,
  bqnwindow,
  bqntable´,
  bqnscan´,
  bqnfillby,
  bqnvalences,
  bqncatch,
]

provide(n::Int64) = value[n + 1]

export provide

# for 𝕗 in value
#   types = [None, Any,
#            Float64, Char,
#            Array, Vector{Float64}, Vector{Char}]
#   for 𝕨 in types
#     for 𝕩 in types
#       precompile(𝕗, (𝕨, 𝕩))
#     end
#   end
# end

end
