module Runtime0

import ..run, ..none, ..None, ..FN, ..M1N, ..M2N, ..Provide

names = ['⌊' => "bqnmin",
         '⌈' => "bqnmax",
         '|' => "bqnabs",
         '<' => "bqnlt",
         '>' => "bqngt",
         '≠' => "bqnneq",
         '≥' => "bqngte",
         '⊢' => "bqnright",
         '⊣' => "bqnleft",
         '∾' => "bqnjoin",
         '⋈' => "bqnpair",
         '↑' => "bqntake",
         '↓' => "bqndrop",
         '⊏' => "bqnselect",
         '˙' => "bqnconst",
         '˜' => "bqnswap",
         '¨' => "bqneach",
         '´' => "bqnfold",
         '∘' => "bqnatop",
         '○' => "bqnover",
         '⊸' => "bqnbefore",
         '⟜' => "bqnafter",
         '◶' => "bqnchoose",
         '⍟' => "bqnrepeat"]

indices = Dict{String, Int}()

# If we want to use r0.bqn as the 0-runtime
use_r0 = false

module R0
import ....provide, ....str, ..use_r0
if use_r0
  include("./r0.jl")
end
end

value = if use_r0
  value = run("<none>", R0.value...)
  # define r0 versions as with 0 suffix
  for (idx, name) in enumerate(names)
    name = Symbol("$(name.second)0")
    eval(quote $(name) = $(value[idx]) end)
  end
  value
else
  repeat(Any[function(𝕨, 𝕩) @assert false end], length(names))
end

runtime_0(n::Int64) = value[n + 1]

function set_override(func::Any; name=nothing)
  if name === nothing; name = string(Symbol(func)) end
  idx = indices[name]
  if !use_r0; value[idx] = func end
end
set_override(func::M1N) = set_override(func, name=string(Symbol(func.run)))
set_override(func::M2N) = set_override(func, name=string(Symbol(func.run)))

# define r0 names with prime, so we can refer to them in overrides
for (idx, name) in enumerate(names)
  indices[name.second] = idx
  if use_r0
  name = Symbol("$(name.second)0")
  eval(quote       $(name) = $(value[idx]) end)
  end
end

# ⌊ bqnmin floor
bqnmin(𝕨::None, 𝕩::Number) = floor(𝕩)
bqnmin(𝕨::None, 𝕩::AbstractArray) = floor.(𝕩)
# ⌊ bqnmin minimum
bqnmin(𝕨::Number, 𝕩::Number) = min(𝕨, 𝕩)
bqnmin(𝕨::Number, 𝕩::AbstractArray) = min.(𝕨, 𝕩)
bqnmin(𝕨::AbstractArray, 𝕩::Number) = min.(𝕨, 𝕩)
bqnmin(𝕨::AbstractArray, 𝕩::AbstractArray) = min.(𝕨, 𝕩)

set_override(bqnmin)

# ⌈ bqnmax ceil
bqnmax(𝕨::None, 𝕩::Number) = ceil(𝕩)
bqnmax(𝕨::None, 𝕩::AbstractArray) = ceil.(𝕩)
# ⌈ bqnmax maximum
bqnmax(𝕨::Number, 𝕩::Number) = max(𝕨, 𝕩)
bqnmax(𝕨::Number, 𝕩::AbstractArray) = max.(𝕨, 𝕩)
bqnmax(𝕨::AbstractArray, 𝕩::Number) = max.(𝕨, 𝕩)
bqnmax(𝕨::AbstractArray, 𝕩::AbstractArray) = max.(𝕨, 𝕩)

set_override(bqnmax)

# | bqnabs absolute value
bqnabs(𝕨::None, 𝕩::Number) = abs(𝕩)
bqnabs(𝕨::None, 𝕩::AbstractArray) = abs.(𝕩)
# | bqnabs modulus
bqnabs(𝕨::Number, 𝕩::Number) = mod(𝕩, 𝕨)
bqnabs(𝕨::AbstractArray, 𝕩::Number) = mod.(𝕩, 𝕨)
bqnabs(𝕨::Number, 𝕩::AbstractArray) = mod.(𝕩, 𝕨)
bqnabs(𝕨::AbstractArray, 𝕩::AbstractArray) = mod.(𝕩, 𝕨)

set_override(bqnabs)

# < bqnlt box
bqnlt(𝕨::None, 𝕩) = fill(𝕩)
# < bqnlt less than
bqnlt(𝕨::Number, 𝕩::Number) = Int(𝕨 < 𝕩) # TODO: allow Bool?
bqnlt(𝕨::AbstractArray, 𝕩::Number) = bqnlt.(𝕨, 𝕩)
bqnlt(𝕨::Number, 𝕩::AbstractArray) = bqnlt.(𝕨, 𝕩)
bqnlt(𝕨::AbstractArray, 𝕩::AbstractArray) = bqnlt.(𝕨, 𝕩)
bqnlt(𝕨::Char, 𝕩::Number) = 0
bqnlt(𝕨::Number, 𝕩::Char) = 1

set_override(bqnlt)

# > bqngt greater than
bqngt(𝕨::Number, 𝕩::Number) = Int(𝕨 > 𝕩) # TODO: allow Bool?
bqngt(𝕨::AbstractArray, 𝕩::Number) = bqngt.(𝕨, 𝕩)
bqngt(𝕨::Number, 𝕩::AbstractArray) = bqngt.(𝕨, 𝕩)
bqngt(𝕨::AbstractArray, 𝕩::AbstractArray) = bqngt.(𝕨, 𝕩)
bqngt(𝕨::Char, 𝕩::Char) = bqngt(Int(𝕨), Int(𝕩))
bqngt(𝕨::Char, 𝕩::Number) = 1
bqngt(𝕨::Number, 𝕩::Char) = 0

set_override(bqngt)

# ≠ bqnneq length
bqnneq(𝕨::None, 𝕩::Vector) = length(𝕩)
bqnneq(𝕨::None, 𝕩::AbstractArray) = begin
  size𝕩 = size(𝕩)
  size𝕩 != () ? size𝕩[end] : 1
end
bqnneq(𝕨::None, 𝕩) = length(𝕩)
# ≠ bqnneq not equals
bqnneq(𝕨::Number, 𝕩::Number) = Int(𝕨 != 𝕩)
bqnneq(𝕨::AbstractArray, 𝕩::Number) = 𝕨 .!= 𝕩
bqnneq(𝕨::Number, 𝕩::AbstractArray) = 𝕨 .!= 𝕩
bqnneq(𝕨::AbstractArray, 𝕩::AbstractArray) = 𝕨 .!= 𝕩

set_override(bqnneq)

# ≥ bqngte greater or equal
bqngte(𝕨::Number, 𝕩::Number) = Int(𝕨 >= 𝕩)
bqngte(𝕨::AbstractArray, 𝕩::Number) = 𝕨 .>= 𝕩
bqngte(𝕨::Number, 𝕩::AbstractArray) = 𝕨 .>= 𝕩
bqngte(𝕨::AbstractArray, 𝕩::AbstractArray) = 𝕨 .>= 𝕩

set_override(bqngte)

# ⊢ bqnright identity
bqnright(𝕨::None, 𝕩) = 𝕩
# ⊢ bqnright right
bqnright(𝕨, 𝕩) = 𝕩

set_override(bqnright)

# ⊣ bqnleft identity
bqnleft(𝕨::None, 𝕩) = 𝕩
# ⊣ bqnleft left
bqnleft(𝕨, 𝕩) = 𝕨

set_override(bqnleft)

# ∾ bqnjoin
bqnjoin(𝕨::AbstractArray, 𝕩::AbstractArray) = vcat(𝕨, 𝕩)
bqnjoin(𝕨::String, 𝕩::String) = string(𝕨, 𝕩)
bqnjoin(𝕨::String, 𝕩::AbstractArray) = vcat(collect(𝕨), 𝕩)
bqnjoin(𝕨::AbstractArray, 𝕩::String) = vcat(𝕨, collect(𝕩))

set_override(bqnjoin)

# ⋈ bqnpair
bqnpair(𝕨::None, 𝕩::T) where T = T[𝕩]
bqnpair(𝕨::T, 𝕩::T) where T = T[𝕨, 𝕩]
bqnpair(𝕨, 𝕩) = [𝕨, 𝕩] 

set_override(bqnpair)

# ↑ bqntake
bqntake(𝕨::Number, 𝕩::AbstractArray) = 𝕩[1:Int(𝕨)]
bqntake(𝕨::Number, 𝕩::String) = 𝕩[1:Int(𝕨)]

set_override(bqntake)

# ↓ bqndrop
bqndrop(𝕨::Number, 𝕩::AbstractArray) = 𝕩[Int(𝕨)+1:end]
bqndrop(𝕨::Number, 𝕩::String) = 𝕩[Int(𝕨)+1:end]

set_override(bqndrop)

# ⊏ bqnselect
bqnselect(𝕨::AbstractArray{Int}, 𝕩::AbstractArray) =
  selectdim(𝕩, ndims(𝕩), 𝕨 .+ 1)
bqnselect(𝕨::AbstractArray, 𝕩::AbstractArray) =
  bqnselect(map(Int, 𝕨), 𝕩)
bqnselect(𝕨::AbstractArray, 𝕩::String) =
  bqnselect(𝕨, collect(𝕩))

set_override(bqnselect)

# ˙ bqnconst
bqnconst(𝕘::Nothing, 𝕗) = begin
  𝕗′ = (_, _) -> 𝕗
  # TODO: M1N(...) should be pre-allocated
  FN(𝕗′,𝕘, M1N(bqnconst), 𝕗)
end

set_override(M1N(bqnconst))

# ˜ bqnswap
bqnswap(𝕘::Nothing, 𝕗) = begin
  𝕗′ = (𝕨, 𝕩) -> 𝕨 == none ? 𝕗(𝕩, 𝕩) : 𝕗(𝕩, 𝕨)
  # TODO: M1N(...) should be pre-allocated
  FN(𝕗′, 𝕘, M1N(bqnswap), 𝕗)
end

set_override(M1N(bqnswap))

# ¨ bqneach
bqneach(𝕘::Nothing, 𝕗) = begin
  𝕗′ = (𝕨, 𝕩) -> bqneach′′(𝕗, 𝕨, 𝕩)
  # TODO: M1N(...) should be pre-allocated
  FN(𝕗′, 𝕘, M1N(bqneach), 𝕗)
end

bqneach′′(𝕗, 𝕨::AbstractArray, 𝕩::AbstractArray) = 𝕗.(𝕨, 𝕩)
bqneach′′(𝕗, 𝕨::String, 𝕩::String) = bqneach′′(𝕗, collect(𝕨), collect(𝕩))
bqneach′′(𝕗, 𝕨::String, 𝕩::AbstractArray) = bqneach′′(𝕗, collect(𝕨), 𝕩)
bqneach′′(𝕗, 𝕨::AbstractArray, 𝕩::String) = bqneach′′(𝕗, 𝕨, collect(𝕩))

set_override(M1N(bqneach))

# ´ bqnfold
bqnfold(𝕘::Nothing, 𝕗) = begin
  𝕗′ = (𝕨, 𝕩) -> 𝕨 == none ? foldr(𝕗, 𝕩) : foldr(𝕗, 𝕩, init=𝕨)
  # TODO: M1N(...) should be pre-allocated
  FN(𝕗′, 𝕘, M1N(bqnfold), 𝕗)
end

set_override(M1N(bqnfold))

# ∘ bqnatop
bqnatop(𝕘, 𝕗) = begin
  𝕗′ = (𝕨, 𝕩) -> 𝕗(none, 𝕘(𝕨, 𝕩))
  # TODO: M2N(...) should be pre-allocated
  FN(𝕗′, 𝕘, M2N(bqnatop), 𝕗)
end

set_override(M2N(bqnatop))

# ○ bqnover
bqnover(𝕘, 𝕗) = begin
  𝕗′ = (𝕨, 𝕩) -> 𝕨===none ? 𝕗(none, 𝕘(none, 𝕩)) : 𝕗(𝕘(none, 𝕨), 𝕘(none, 𝕩))
  # TODO: M2N(...) should be pre-allocated
  FN(𝕗′, 𝕘, M2N(bqnover), 𝕗)
end

set_override(M2N(bqnover))

# ⊸ bqnbefore
bqnbefore(𝕘, 𝕗) = begin
  𝕗′ = (𝕨, 𝕩) -> 𝕨===none ? 𝕘(𝕗(none, 𝕩), 𝕩) : 𝕘(𝕗(none, 𝕨), 𝕩)
  # TODO: M2N(...) should be pre-allocated
  FN(𝕗′, 𝕘, M2N(bqnbefore), 𝕗)
end

set_override(M2N(bqnbefore))

# ⟜ bqnafter
bqnafter(𝕘, 𝕗) = begin
  𝕗′ = (𝕨, 𝕩) -> 𝕨===none ? 𝕗(𝕩, 𝕘(none, 𝕩)) : 𝕗(𝕨, 𝕘(none, 𝕩))
  # TODO: M2N(...) should be pre-allocated
  FN(𝕗′, 𝕘, M2N(bqnafter), 𝕗)
end

set_override(M2N(bqnafter))

# ◶ bqnchoose
bqnchoose(𝕘, 𝕗) = begin
  𝕗′ = (𝕨, 𝕩) -> begin
    𝕗′′ = Provide.bqnpick(𝕗(𝕨, 𝕩), 𝕘)
    𝕗′′(𝕨, 𝕩)
  end
  FN(𝕗′, 𝕘, M2N(bqnchoose), 𝕗)
end

set_override(M2N(bqnchoose))

# ⍟ bqnrepeat
bqnrepeat(𝕘, 𝕗) = begin
  𝕗′ = (𝕨, 𝕩) -> convert(Bool, 𝕘(𝕨, 𝕩)) ? 𝕗(𝕨, 𝕩) : 𝕩
  FN(𝕗′, 𝕘, M2N(bqnrepeat), 𝕗)
end

set_override(M2N(bqnrepeat))

export runtime_0

end
