""" Core primitives required for r0."""
module Provide

using TimerOutputs

import ..none, ..None, ..BQNError
import ..F, ..FN, ..TR2D, ..TR3D, ..TR3O, ..M1D, ..M1I, ..M2D, ..M2I

const to = TimerOutput()

bqnadd(𝕨::None, 𝕩) = 𝕩
bqnadd(𝕨, 𝕩) = @timeit_debug to "bqnadd" 𝕨 + 𝕩

bqnsub(𝕨::None, 𝕩::Number) = -𝕩
bqnsub(𝕨, 𝕩) = @timeit_debug to "bqnsub" 𝕨 - 𝕩

bqnmul(𝕨::None, 𝕩::Number) = sign(𝕩)
bqnmul(𝕨::Number, 𝕩::Number) = @timeit_debug to "bqnmul" 𝕨 * 𝕩

bqndiv(𝕨::None, 𝕩::Number) = 1/𝕩
bqndiv(𝕨::Number, 𝕩::Number) = @timeit_debug to "bqndiv" 𝕨/𝕩

bqnpow(𝕨::None, 𝕩::Number) = ℯ^𝕩
bqnpow(𝕨::Number, 𝕩::Number) = @timeit_debug to "bqnpow" if 𝕩>=0; 𝕨^𝕩 else 1/(𝕨^(-𝕩)) end

bqnroot(root::None, v) = @timeit_debug to "bqnrootM" sqrt(v)
bqnroot(root, v) = @timeit_debug to "bqnroot" v^(1/root)

bqnabs(𝕨::None, v) = @timeit_debug to "bqnabsM" abs(v)

bqnmin(𝕨::Int64, 𝕩::Number) = @timeit_debug to "bqnminM" min(𝕨, 𝕩)
bqnmin(𝕨::None, 𝕩::Number) = @timeit_debug to "bqnmin" floor(𝕩)

bqnnot(𝕨::None, 𝕩::Number) = @timeit_debug to "bqnnotM" +(1 - 𝕩)
bqnnot(𝕨::Number, 𝕩::Number) = @timeit_debug to "bqnnot" 1 + (𝕨 - 𝕩)

bqnand(𝕨::Number, 𝕩::Number) = @timeit_debug to "bqnand" 𝕨*𝕩

bqnor(𝕨::Number, 𝕩::Number) = @timeit_debug to "bqnor" (𝕨+𝕩)-(𝕨*𝕩)

bqnidleft(𝕨, 𝕩) = 𝕨

bqnidright(𝕨, 𝕩) = 𝕩

function bqnvalences(𝕘, 𝕗)
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

bqneq(𝕨::None, 𝕩::AbstractArray) = @timeit_debug to "bqneqM" ndims(𝕩)
bqneq(𝕨::None, 𝕩::String) = 1
bqneq(𝕨::None, 𝕩) = 0
bqneq(𝕨, 𝕩) = @timeit_debug to "bqneq" Int(𝕨 == 𝕩)

bqnlte(𝕨, 𝕩) = @timeit_debug to "bqnlte" Int(𝕨 <= 𝕩)
bqnlte(𝕨::Number, 𝕩::Char) = 1
bqnlte(𝕨::Char, 𝕩::Number) = 0

bqnshape(𝕨, 𝕩::AbstractArray) = @timeit_debug to "bqnshape" reverse([x for x in size(𝕩)])
bqnshape(𝕨, 𝕩::String) = @timeit_debug to "bqnshape" Int[length(𝕩)]
bqnshape(𝕨, 𝕩) = @timeit_debug to "bqnshape" []

bqndeshape(𝕨::None, 𝕩::AbstractArray) = @timeit_debug to "bqndeshapeM" vec(𝕩)
bqndeshape(𝕨::None, 𝕩::String) = 𝕩
bqndeshape(𝕨::None, 𝕩) = @timeit_debug to "bqndeshapeM" [𝕩]

function bqndeshape(𝕨::AbstractArray, 𝕩::AbstractArray)
  @timeit_debug to "bqndeshape" begin
  size = reverse(Tuple(Int(x) for x in 𝕨))
  if size == Base.size(𝕩); return 𝕩 end
  reshape(𝕩, size)
  end
end

function bqndeshape(𝕨::AbstractArray, 𝕩::String)
  @timeit_debug to "bqndeshape" begin
  𝕩 = collect(𝕩)
  bqndeshape(𝕨, 𝕩)
  end
end
      
function bqndeshape(𝕨::AbstractArray, 𝕩::Any)
  @timeit_debug to "bqndeshape" begin
  @assert length(𝕨) == 0
  collect(𝕩)
  end
end

bqnpick(𝕨::Number, 𝕩::Number) = 𝕩
bqnpick(𝕨::Float64, 𝕩::AbstractArray) = bqnpick(Int(𝕨), 𝕩)
function bqnpick(𝕨::Int64, 𝕩::AbstractArray)
  @timeit_debug to "bqnpick" begin
  if 𝕨 >= 0; 𝕩[𝕨 + 1] else 𝕩[end + (𝕨 + 1)] end
  end
end
bqnpick(𝕨::None, 𝕩::AbstractArray) = bqnpick(0, 𝕩)
# TODO: get rid of collect, this is slow!
bqnpick(𝕨::Number, 𝕩::String) = bqnpick(𝕨, collect(𝕩))
bqnpick(𝕨::None, 𝕩::String) = bqnpick(0, collect(𝕩))
bqnpick(𝕨::None, 𝕩) = 𝕩

bqnwindow(𝕨, 𝕩) = @timeit_debug to "bqnwindow" [x for x in 0:(𝕩-1)]

function bqntable(𝕘, 𝕗)
  𝕣 = bqntable
  # TODO: need to get rid of calls to collect() here, instead need to iterate
  # over graphemes for Strings
  run = function(𝕨, 𝕩)
    @timeit_debug to "bqntable" begin
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
  FN(run, 𝕘, 𝕣, 𝕗)
end

function bqnscan(𝕘, 𝕗)
  @assert 𝕘 === nothing
  𝕣 = bqnscan
  run = function(𝕨, 𝕩::AbstractArray)
    @timeit_debug to "bqnscan" begin
    bqnassert(
              "`: Argument cannot have rank 0",
              Int(ndims(𝕩) != 0))
    bqnassert(
              "`: Shape of 𝕨 must match the cell of 𝕩",
              Int(𝕨 == none ||
                  size(𝕨) == () && ndims(𝕩) == 1 ||
                  size(𝕨)[1:1] == size(𝕩)[1:1]))
    if 𝕨 == none
      accumulate(𝕗, 𝕩, dims=ndims(𝕩))
    elseif size(𝕨) == ()
      accumulate(𝕗, 𝕩, dims=ndims(𝕩), init=𝕨)
    else
      # Because accumulate() doesn't support init being an array we provide
      # init value by concatenating it over the major dimension with hvncat():
      ndims𝕩 = ndims(𝕩)
      𝕩 = hvncat(ndims𝕩, 𝕨, 𝕩)
      𝕩 = accumulate(𝕗, 𝕩, dims=ndims𝕩)
      # ... but this will produce an extra "row" in this dimension so we
      # produce a view which "cuts" that out with a view over this array:
      # TODO: Revisit that for performance!
      indices = [(:) for _ in size(𝕩)[1:end - 1]]
      @view 𝕩[indices..., 2:end]
    end
    end
  end
  FN(run, 𝕘, 𝕣, 𝕗)
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
bqntype′(𝕨::None, 𝕩::FN) = 3
bqntype′(𝕨::None, 𝕩::M1D) = 4
bqntype′(𝕨::None, 𝕩::M1I) = 4
bqntype′(𝕨::None, 𝕩::M2D) = 5
bqntype′(𝕨::None, 𝕩::M2I) = 5

bqnfill(𝕨::None, 𝕩::String) = ' '
bqnfill(𝕨::None, 𝕩::AbstractArray) = 0
bqnfill(𝕨, 𝕩) = 𝕩

bqnlog(𝕨::None, 𝕩::Number) = log(ℯ, 𝕩)
bqnlog(𝕨::Number, 𝕩::Number) = log(𝕨, 𝕩)

function bqngrouplen(𝕨, 𝕩::AbstractArray)
  @timeit_debug to "bqngrouplen" begin
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
end

function bqngroupord(𝕨, 𝕩::AbstractArray)
  @timeit_debug to "bqngroupord" begin
  indices = [[] for _ in 1:length(𝕨)]
  for (idx, x) in enumerate(𝕩)
    if x < 0; continue end
    push!(indices[Int(x) + 1], idx - 1)
  end
  vcat(indices...)
  end
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
  𝕣 = bqnfillby
  run = function(𝕨, 𝕩)
    𝕗(𝕨, 𝕩)
  end
  FN(run, 𝕘, 𝕣, 𝕗)
end

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
  bqntable,
  bqnscan,
  bqnfillby,
  bqnvalences,
  bqncatch,
]

provide(n::Int64) = value[n + 1]

export provide

end
