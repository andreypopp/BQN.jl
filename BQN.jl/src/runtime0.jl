module Runtime0

import TimerOutputs
import TimerOutputs: @timeit_debug

to = TimerOutputs.TimerOutput()

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

const indices = Dict{String, Int}(name.second => idx
                                  for (idx, name) in enumerate(names))

# If we want to use r0.bqn as the 0-runtime
const use_r0 = false

module R0
import ....provide, ....str, ..use_r0
if use_r0
  include("./r0.jl")
end
end

const value = if use_r0
  value = run("<none>", R0.value...)
  # define r0 versions as with 0 suffix
  for (idx, name) in enumerate(names)
    name = Symbol("$(name.second)0")
    eval(quote const $(name) = $(value[idx]) end)
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

# ⌊ bqnmin floor
bqnmin(𝕨::None, 𝕩::Number) = @timeit_debug to "bqnminM" floor(𝕩)
bqnmin(𝕨::None, 𝕩::AbstractArray) = @timeit_debug to "bqnminM" floor.(𝕩)
# ⌊ bqnmin minimum
bqnmin(𝕨::Number, 𝕩::Number) = @timeit_debug to "bqnmin" min(𝕨, 𝕩)
bqnmin(𝕨::Number, 𝕩::AbstractArray) = @timeit_debug to "bqnmin" min.(𝕨, 𝕩)
bqnmin(𝕨::AbstractArray, 𝕩::Number) = @timeit_debug to "bqnmin" min.(𝕨, 𝕩)
bqnmin(𝕨::AbstractArray, 𝕩::AbstractArray) = @timeit_debug to "bqnmin" min.(𝕨, 𝕩)

set_override(bqnmin)

# ⌈ bqnmax ceil
bqnmax(𝕨::None, 𝕩::Number) = @timeit_debug to "bqnmaxM" ceil(𝕩)
bqnmax(𝕨::None, 𝕩::AbstractArray) = @timeit_debug to "bqnmaxM" ceil.(𝕩)
# ⌈ bqnmax maximum
bqnmax(𝕨::Number, 𝕩::Number) = @timeit_debug to "bqnmax" max(𝕨, 𝕩)
bqnmax(𝕨::Number, 𝕩::AbstractArray) = @timeit_debug to "bqnmax" max.(𝕨, 𝕩)
bqnmax(𝕨::AbstractArray, 𝕩::Number) = @timeit_debug to "bqnmax" max.(𝕨, 𝕩)
bqnmax(𝕨::AbstractArray, 𝕩::AbstractArray) = @timeit_debug to "bqnmax" max.(𝕨, 𝕩)

set_override(bqnmax)

# | bqnabs absolute value
bqnabs(𝕨::None, 𝕩::Number) = @timeit_debug to "bqnabsM" abs(𝕩)
bqnabs(𝕨::None, 𝕩::AbstractArray) = @timeit_debug to "bqnabsM" abs.(𝕩)
# | bqnabs modulus
bqnabs(𝕨::Number, 𝕩::Number) = @timeit_debug to "bqnabs" mod(𝕩, 𝕨)
bqnabs(𝕨::AbstractArray, 𝕩::Number) = @timeit_debug to "bqnabs" mod.(𝕩, 𝕨)
bqnabs(𝕨::Number, 𝕩::AbstractArray) = @timeit_debug to "bqnabs" mod.(𝕩, 𝕨)
bqnabs(𝕨::AbstractArray, 𝕩::AbstractArray) = @timeit_debug to "bqnabsx" mod.(𝕩, 𝕨)

set_override(bqnabs)

# < bqnlt box
bqnlt(𝕨::None, 𝕩) = @timeit_debug to "bqnltM" fill(𝕩)
# < bqnlt less than
bqnlt(𝕨::Number, 𝕩::Number) = @timeit_debug to "bqnlt" Int(𝕨 < 𝕩) # TODO: allow Bool?
bqnlt(𝕨::AbstractArray, 𝕩::Number) = bqnlt.(𝕨, 𝕩)
bqnlt(𝕨::Number, 𝕩::AbstractArray) = bqnlt.(𝕨, 𝕩)
bqnlt(𝕨::AbstractArray, 𝕩::AbstractArray) = bqnlt.(𝕨, 𝕩)
bqnlt(𝕨::Char, 𝕩::Number) = 0
bqnlt(𝕨::Number, 𝕩::Char) = 1

set_override(bqnlt)

# > bqngt greater than
bqngt(𝕨::Number, 𝕩::Number) = @timeit_debug to "bqngt" Int(𝕨 > 𝕩) # TODO: allow Bool?
bqngt(𝕨::AbstractArray, 𝕩::Number) = bqngt.(𝕨, 𝕩)
bqngt(𝕨::Number, 𝕩::AbstractArray) = bqngt.(𝕨, 𝕩)
bqngt(𝕨::AbstractArray, 𝕩::AbstractArray) = bqngt.(𝕨, 𝕩)
bqngt(𝕨::Char, 𝕩::Char) = bqngt(Int(𝕨), Int(𝕩))
bqngt(𝕨::Char, 𝕩::Number) = 1
bqngt(𝕨::Number, 𝕩::Char) = 0

set_override(bqngt)

# ≠ bqnneq length
bqnneq(𝕨::None, 𝕩::Vector) = @timeit_debug to "bqnneqM" length(𝕩)
bqnneq(𝕨::None, 𝕩::AbstractArray) = begin
  @timeit_debug to "bqnneqM" begin
  size𝕩 = size(𝕩)
  size𝕩 != () ? size𝕩[end] : 1
  end
end
bqnneq(𝕨::None, 𝕩) = @timeit_debug "bqnneqM" length(𝕩)
# ≠ bqnneq not equals
bqnneq(𝕨::Number, 𝕩::Number) = @timeit_debug "bqnneq" Int(𝕨 != 𝕩)
bqnneq(𝕨::AbstractArray, 𝕩::Number) = @timeit_debug "bqnneq" 𝕨 .!= 𝕩
bqnneq(𝕨::Number, 𝕩::AbstractArray) = @timeit_debug "bqnneq" 𝕨 .!= 𝕩
bqnneq(𝕨::AbstractArray, 𝕩::AbstractArray) = @timeit_debug "bqnneq" 𝕨 .!= 𝕩

set_override(bqnneq)

# ≥ bqngte greater or equal
bqngte(𝕨::Number, 𝕩::Number) = @timeit_debug "bqngte" Int(𝕨 >= 𝕩)
bqngte(𝕨::AbstractArray, 𝕩::Number) = @timeit_debug "bqngte" 𝕨 .>= 𝕩
bqngte(𝕨::Number, 𝕩::AbstractArray) = @timeit_debug "bqngte" 𝕨 .>= 𝕩
bqngte(𝕨::AbstractArray, 𝕩::AbstractArray) = @timeit_debug "bqngte" 𝕨 .>= 𝕩

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
bqnjoin(𝕨::AbstractArray, 𝕩::AbstractArray) = @timeit_debug "bqnjoin" vcat(𝕨, 𝕩)
bqnjoin(𝕨::AbstractString, 𝕩::AbstractString) = @timeit_debug "bqnjoin" string(𝕨, 𝕩)
bqnjoin(𝕨::AbstractString, 𝕩::AbstractArray) = @timeit_debug "bqnjoin" vcat(collect(𝕨), 𝕩)
bqnjoin(𝕨::AbstractArray, 𝕩::AbstractString) = @timeit_debug "bqnjoin" vcat(𝕨, collect(𝕩))

set_override(bqnjoin)

# ⋈ bqnpair
bqnpair(𝕨::None, 𝕩::T) where T = T[𝕩]
bqnpair(𝕨::T, 𝕩::T) where T = T[𝕨, 𝕩]
bqnpair(𝕨, 𝕩) = [𝕨, 𝕩] 

set_override(bqnpair)

# ↑ bqntake
bqntake(𝕨::Number, 𝕩::AbstractArray) =
  @timeit_debug "bqntake" 𝕩[1:Int(𝕨)]
bqntake(𝕨::Number, 𝕩::AbstractString) =
  @timeit_debug "bqntake" 𝕩[1:Int(𝕨)]

set_override(bqntake)

# ↓ bqndrop
bqndrop(𝕨::Number, 𝕩::AbstractArray) =
  @timeit_debug "bqndrop" 𝕩[Int(𝕨)+1:end]
bqndrop(𝕨::Number, 𝕩::AbstractString) =
  @timeit_debug "bqndrop" 𝕩[Int(𝕨)+1:end]

set_override(bqndrop)

# ⊏ bqnselect
bqnselect(𝕨::AbstractArray{Int}, 𝕩::AbstractArray) =
  @timeit_debug "bqnselect" selectdim(𝕩, ndims(𝕩), 𝕨 .+ 1)
bqnselect(𝕨::AbstractArray, 𝕩::AbstractArray) =
  bqnselect(map(Int, 𝕨), 𝕩)
bqnselect(𝕨::AbstractArray, 𝕩::AbstractString) =
  bqnselect(𝕨, collect(𝕩))

set_override(bqnselect)

# ˙ bqnconst
bqnconst(𝕘::Nothing, 𝕗) = FNConst(bqnconst′, 𝕗)
bqnconst′ = M1N(bqnconst)

struct FNConst
  𝕣::M1N
  𝕗::Any
end

(𝕣::FNConst)(𝕨, 𝕩) = 𝕣.𝕗

Provide.bqntype′(𝕨::None, 𝕩::FNConst) = 3

set_override(bqnconst′)

# ˜ bqnswap
bqnswap(𝕘::Nothing, 𝕗) = FNSwap(bqnswap′, 𝕗)
bqnswap′ = M1N(bqnswap)

struct FNSwap
  𝕣::M1N
  𝕗::Any
end

(𝕣::FNSwap)(𝕨::None, 𝕩) = 𝕣.𝕗(𝕩, 𝕩)
(𝕣::FNSwap)(𝕨, 𝕩) = 𝕣.𝕗(𝕩, 𝕨)

Provide.bqntype′(𝕨::None, 𝕩::FNSwap) = 3

set_override(bqnswap′)

# ¨ bqneach
bqneach(𝕘::Nothing, 𝕗) = FNEach(bqneach′, 𝕗)
bqneach′ = M1N(bqneach)

struct FNEach
  𝕣::M1N
  𝕗::Any
end

(𝕣::FNEach)(𝕨::AbstractArray, 𝕩::AbstractArray) = 𝕣.𝕗.(𝕨, 𝕩)
(𝕣::FNEach)(𝕨::AbstractString, 𝕩::AbstractString) = 𝕣.𝕗.(collect(𝕨), collect(𝕩))
(𝕣::FNEach)(𝕨::AbstractArray, 𝕩::AbstractString) = 𝕣.𝕗.(𝕨, collect(𝕩))
(𝕣::FNEach)(𝕨::AbstractString, 𝕩::AbstractArray) = 𝕣.𝕗.(collect(𝕨), 𝕩)

Provide.bqntype′(𝕨::None, 𝕩::FNEach) = 3

set_override(bqneach′)

# ´ bqnfold
bqnfold(𝕘::Nothing, 𝕗) = FNFold(bqnfold′, 𝕗)
bqnfold′ = M1N(bqnfold)

struct FNFold
  𝕣::M1N
  𝕗::Any
end

(𝕣::FNFold)(𝕨::None, 𝕩) = foldr(𝕣.𝕗, 𝕩)
(𝕣::FNFold)(𝕨, 𝕩) = foldr(𝕣.𝕗, 𝕩, init=𝕨)

Provide.bqntype′(𝕨::None, 𝕩::FNFold) = 3

set_override(bqnfold′)

# ∘ bqnatop
bqnatop(𝕘, 𝕗) = @timeit_debug to "bqnatop" FNAtop(𝕘, bqnatop′, 𝕗)

struct FNAtop
  𝕘::Union{Any,Nothing}
  𝕣::Union{Any,Nothing}
  𝕗::Union{Any,Nothing}
end

(𝕣::FNAtop)(𝕨, 𝕩) = 𝕣.𝕗(none, 𝕣.𝕘(𝕨, 𝕩))

Provide.bqntype′(𝕨::None, 𝕩::FNAtop) = 3

bqnatop′ = M2N(bqnatop)
set_override(bqnatop′)

# ○ bqnover
bqnover(𝕘, 𝕗) = @timeit_debug to "bqnover" FNOver(𝕘, bqnover′, 𝕗)

struct FNOver
  𝕘::Union{Any,Nothing}
  𝕣::Union{Any,Nothing}
  𝕗::Union{Any,Nothing}
end

(𝕣::FNOver)(𝕨, 𝕩) = 𝕨===none ? 𝕣.𝕗(none, 𝕣.𝕘(none, 𝕩)) : 𝕣.𝕗(𝕣.𝕘(none, 𝕨), 𝕣.𝕘(none, 𝕩))

Provide.bqntype′(𝕨::None, 𝕩::FNOver) = 3

bqnover′ = M2N(bqnover)
set_override(bqnover′)

# ⊸ bqnbefore
bqnbefore(𝕘, 𝕗) = @timeit_debug to "bqnbefore" FNBefore(𝕘, bqnbefore′, 𝕗)

struct FNBefore
  𝕘::Union{Any,Nothing}
  𝕣::Union{Any,Nothing}
  𝕗::Union{Any,Nothing}
end

(𝕣::FNBefore)(𝕨, 𝕩) = 𝕨===none ? 𝕣.𝕘(𝕣.𝕗(none, 𝕩), 𝕩) : 𝕣.𝕘(𝕣.𝕗(none, 𝕨), 𝕩)

Provide.bqntype′(𝕨::None, 𝕩::FNBefore) = 3

bqnbefore′ = M2N(bqnbefore)
set_override(bqnbefore′)

# ⟜ bqnafter
bqnafter(𝕘, 𝕗) = @timeit_debug to "bqnafter" FNAfter(𝕘, bqnafter′, 𝕗)

struct FNAfter
  𝕘::Union{Any,Nothing}
  𝕣::Union{Any,Nothing}
  𝕗::Union{Any,Nothing}
end

(𝕣::FNAfter)(𝕨, 𝕩) = 𝕨===none ? 𝕣.𝕗(𝕩, 𝕣.𝕘(none, 𝕩)) : 𝕣.𝕗(𝕨, 𝕣.𝕘(none, 𝕩))

Provide.bqntype′(𝕨::None, 𝕩::FNAfter) = 3

bqnafter′ = M2N(bqnafter)
set_override(bqnafter′)

# ◶ bqnchoose
bqnchoose(𝕘, 𝕗) = @timeit_debug to "bqnchoose" FNChoose(𝕘, bqnchoose′, 𝕗)

struct FNChoose
  𝕘::Union{Any,Nothing}
  𝕣::Union{Any,Nothing}
  𝕗::Union{Any,Nothing}
end

(𝕣::FNChoose)(𝕨, 𝕩) = begin
  𝕗 = Provide.bqnpick(𝕣.𝕗(𝕨, 𝕩), 𝕣.𝕘)
  𝕗(𝕨, 𝕩)
end

Provide.bqntype′(𝕨::None, 𝕩::FNChoose) = 3

const bqnchoose′ = M2N(bqnchoose)
set_override(bqnchoose′)

# ⍟ bqnrepeat
bqnrepeat(𝕘, 𝕗) = @timeit_debug to "bqnrepeat" FNRepeat(𝕘, bqnrepeat′, 𝕗)

struct FNRepeat
  𝕘::Union{Any,Nothing}
  𝕣::Union{Any,Nothing}
  𝕗::Union{Any,Nothing}
end

(𝕣::FNRepeat)(𝕨, 𝕩) = convert(Bool, 𝕣.𝕘(𝕨, 𝕩)) ? 𝕣.𝕗(𝕨, 𝕩) : 𝕩

Provide.bqntype′(𝕨::None, 𝕩::FNRepeat) = 3

const bqnrepeat′ = M2N(bqnrepeat)
set_override(bqnrepeat′)

export runtime_0

end
