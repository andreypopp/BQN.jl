module Runtime0

import TimerOutputs
import TimerOutputs: @timeit_debug

import ..run, ..none, ..type, ..None, ..FN, ..M1N, ..M2N, ..Provide, ..to

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

funname(𝕗::Function) = string(Symbol(𝕗))
funname(𝕗::Union{M1N,M2N}) = funname(𝕗.run)

macro override(𝕗)
  if !use_r0
    eval(quote value[indices[funname($𝕗)]] = $𝕗 end)
  end
end

@nospecialize

# ⌊ bqnmin floor
bqnmin(𝕨::None, 𝕩::Number) = float(floor(𝕩))
bqnmin(𝕨::None, 𝕩::AbstractArray) = bqnmin.(Ref(𝕨), 𝕩)
# ⌊ bqnmin minimum
bqnmin(𝕨::Number, 𝕩::Number) = float(min(𝕨, 𝕩))
bqnmin(𝕨::Number, 𝕩::AbstractArray) = bqnmin.(𝕨, 𝕩)
bqnmin(𝕨::AbstractArray, 𝕩::Number) = bqnmin.(𝕨, 𝕩)
bqnmin(𝕨::AbstractArray, 𝕩::AbstractArray) = bqnmin.(𝕨, 𝕩)

@override(bqnmin)

# ⌈ bqnmax ceil
bqnmax(𝕨::None, 𝕩::Number) =  float(ceil(𝕩))
bqnmax(𝕨::None, 𝕩::AbstractArray) = bqnmax.(Ref(none), 𝕩)
# ⌈ bqnmax maximum
bqnmax(𝕨::Number, 𝕩::Number) = float(max(𝕨, 𝕩))
bqnmax(𝕨::Number, 𝕩::AbstractArray) = bqnmax.(𝕨, 𝕩)
bqnmax(𝕨::AbstractArray, 𝕩::Number) = bqnmax.(𝕨, 𝕩)
bqnmax(𝕨::AbstractArray, 𝕩::AbstractArray) = bqnmax.(𝕨, 𝕩)

@override(bqnmax)

# | bqnabs absolute value
bqnabs(𝕨::None, 𝕩::Number) = float(abs(𝕩))
bqnabs(𝕨::None, 𝕩::AbstractArray) = bqnabs.(Ref(none), 𝕩)
# | bqnabs modulus
bqnabs(𝕨::Number, 𝕩::Number) = float(mod(𝕩, 𝕨))
bqnabs(𝕨::AbstractArray, 𝕩::Number) = bqnabs.(𝕩, 𝕨)
bqnabs(𝕨::Number, 𝕩::AbstractArray) = bqnabs.(𝕩, 𝕨)
bqnabs(𝕨::AbstractArray, 𝕩::AbstractArray) = bqnabs.(𝕩, 𝕨)

@override(bqnabs)

# < bqnlt box
bqnlt(𝕨::None, 𝕩) = fill(𝕩)
# < bqnlt less than
bqnlt(𝕨::Number, 𝕩::Number) = float(𝕨 < 𝕩)
bqnlt(𝕨::AbstractArray, 𝕩::Number) = bqnlt.(𝕨, 𝕩)
bqnlt(𝕨::Number, 𝕩::AbstractArray) = bqnlt.(𝕨, 𝕩)
bqnlt(𝕨::AbstractArray, 𝕩::AbstractArray) = bqnlt.(𝕨, 𝕩)
bqnlt(𝕨::Char, 𝕩::Number) = 0.0
bqnlt(𝕨::Number, 𝕩::Char) = 1.0

@override(bqnlt)

# > bqngt greater than
bqngt(𝕨::Number, 𝕩::Number) = float(𝕨 > 𝕩)
bqngt(𝕨::AbstractArray, 𝕩::Number) = bqngt.(𝕨, 𝕩)
bqngt(𝕨::Number, 𝕩::AbstractArray) = bqngt.(𝕨, 𝕩)
bqngt(𝕨::AbstractArray, 𝕩::AbstractArray) = bqngt.(𝕨, 𝕩)
bqngt(𝕨::Char, 𝕩::Char) = bqngt(Int(𝕨), Int(𝕩))
bqngt(𝕨::Char, 𝕩::Number) = 1.0
bqngt(𝕨::Number, 𝕩::Char) = 0.0

@override(bqngt)

# ≠ bqnneq length
bqnneq(𝕨::None, 𝕩::Vector) = float(length(𝕩))
bqnneq(𝕨::None, 𝕩::AbstractArray) = begin
  @timeit_debug to "Runtime0.bqnneqM" begin
  size𝕩 = size(𝕩)
  float(size𝕩 != () ? size𝕩[end] : 1)
  end
end
bqnneq(𝕨::None, 𝕩) = float(length(𝕩))
# ≠ bqnneq not equals
bqnneq(𝕨::Number, 𝕩::Number) = float(𝕨 != 𝕩)
bqnneq(𝕨::AbstractArray, 𝕩::Number) = bqnneq.(𝕨, 𝕩)
bqnneq(𝕨::Number, 𝕩::AbstractArray) = bqnneq.(𝕨, 𝕩)
bqnneq(𝕨::AbstractArray, 𝕩::AbstractArray) = bqnneq.(𝕨, 𝕩)

@override(bqnneq)

# ≥ bqngte greater or equal
bqngte(𝕨::Number, 𝕩::Number) = float(𝕨 >= 𝕩)
bqngte(𝕨::AbstractArray, 𝕩::Number) = bqngte.(𝕨, 𝕩)
bqngte(𝕨::Number, 𝕩::AbstractArray) = bqngte.(𝕨, 𝕩)
bqngte(𝕨::AbstractArray, 𝕩::AbstractArray) = bqngte.(𝕨, 𝕩)

@override(bqngte)

# ⊢ bqnright identity
bqnright(𝕨::None, @nospecialize(𝕩)) = 𝕩
# ⊢ bqnright right
bqnright(@nospecialize(𝕨), @nospecialize(𝕩)) = 𝕩

@override(bqnright)

# ⊣ bqnleft identity
bqnleft(𝕨::None, @nospecialize(𝕩)) = 𝕩
# ⊣ bqnleft left
bqnleft(@nospecialize(𝕨), @nospecialize(𝕩)) = 𝕨

@override(bqnleft)

# ∾ bqnjoin
bqnjoin(𝕨::AbstractArray, 𝕩::AbstractArray) = collect(vcat(𝕨, 𝕩))

@override(bqnjoin)

# ⋈ bqnpair
bqnpair(𝕨::None, 𝕩::T) where T = T[𝕩]
bqnpair(𝕨::T, 𝕩::T) where T = T[𝕨, 𝕩]
bqnpair(𝕨, 𝕩) = [𝕨, 𝕩] 

@override(bqnpair)

# ↑ bqntake
bqntake(𝕨::Number, 𝕩::AbstractArray) = 𝕩[1:Int(𝕨)]

@override(bqntake)

# ↓ bqndrop
bqndrop(𝕨::Number, 𝕩::AbstractArray) = 𝕩[Int(𝕨)+1:end]

@override(bqndrop)

# ⊏ bqnselect
bqnselect(𝕨::AbstractArray{Int}, 𝕩::AbstractArray) =
  collect(selectdim(𝕩, ndims(𝕩), 𝕨 .+ 1))
bqnselect(𝕨::AbstractArray, 𝕩::AbstractArray) =
  bqnselect(map(Int, 𝕨), 𝕩)

@override(bqnselect)

# ˙ bqnconst
bqnconst(𝕘::Nothing, @nospecialize(𝕗)) =
  FNConst(bqnconst′, 𝕗)
bqnconst′ = M1N(bqnconst)

struct FNConst
  𝕣::M1N
  𝕗::Any
end

(𝕣::FNConst)(@nospecialize(𝕨), @nospecialize(𝕩)) = 𝕣.𝕗

type(𝕩::FNConst) = 3.0

@override(bqnconst′)

# ˜ bqnswap
bqnswap(𝕘::Nothing, @nospecialize(𝕗)) = FNSwap(bqnswap′, 𝕗)
bqnswap′ = M1N(bqnswap)

struct FNSwap
  𝕣::M1N
  𝕗::Any
end

(𝕣::FNSwap)(𝕨::None, @nospecialize(𝕩)) = 𝕣.𝕗(𝕩, 𝕩)
(𝕣::FNSwap)(@nospecialize(𝕨), @nospecialize(𝕩)) = 𝕣.𝕗(𝕩, 𝕨)

type(𝕩::FNSwap) = 3.0

@override(bqnswap′)

# ¨ bqneach
bqneach(𝕘::Nothing, @nospecialize(𝕗)) = FNEach(bqneach′, 𝕗)
bqneach′ = M1N(bqneach)

struct FNEach
  𝕣::M1N
  𝕗::Any
end

(𝕣::FNEach)(𝕨::AbstractArray, 𝕩::AbstractArray) = 𝕣.𝕗.(𝕨, 𝕩)

type(𝕩::FNEach) = 3.0

@override(bqneach′)

# ´ bqnfold
bqnfold(𝕘::Nothing, @nospecialize(𝕗)) = FNFold(bqnfold′, 𝕗)
bqnfold′ = M1N(bqnfold)

struct FNFold
  𝕣::M1N
  𝕗::Any
end

(𝕣::FNFold)(𝕨::None, 𝕩) = foldr(𝕣.𝕗, 𝕩)
(𝕣::FNFold)(𝕨, 𝕩) = foldr(𝕣.𝕗, 𝕩, init=𝕨)

type(𝕩::FNFold) = 3.0

@override(bqnfold′)

# ∘ bqnatop
bqnatop(@nospecialize(𝕘), @nospecialize(𝕗)) =
  @timeit_debug to "Runtime0.bqnatop" FNAtop(𝕘, bqnatop′, 𝕗)

struct FNAtop
  𝕘::Union{Any,Nothing}
  𝕣::Union{Any,Nothing}
  𝕗::Union{Any,Nothing}
end

(𝕣::FNAtop)(𝕨, 𝕩) = 𝕣.𝕗(none, 𝕣.𝕘(𝕨, 𝕩))

type(𝕩::FNAtop) = 3.0

bqnatop′ = M2N(bqnatop)
@override(bqnatop′)

# ○ bqnover
bqnover(@nospecialize(𝕘), @nospecialize(𝕗)) =
  @timeit_debug to "Runtime0.bqnover" FNOver(𝕘, bqnover′, 𝕗)

struct FNOver
  𝕘::Union{Any,Nothing}
  𝕣::Union{Any,Nothing}
  𝕗::Union{Any,Nothing}
end

(𝕣::FNOver)(𝕨, 𝕩) =
  𝕨===none ? 𝕣.𝕗(none, 𝕣.𝕘(none, 𝕩)) : 𝕣.𝕗(𝕣.𝕘(none, 𝕨), 𝕣.𝕘(none, 𝕩))

type(𝕩::FNOver) = 3.0

bqnover′ = M2N(bqnover)
@override(bqnover′)

# ⊸ bqnbefore
bqnbefore(@nospecialize(𝕘), @nospecialize(𝕗)) =
  @timeit_debug to "Runtime0.bqnbefore" FNBefore(𝕘, bqnbefore′, 𝕗)

struct FNBefore
  𝕘::Union{Any,Nothing}
  𝕣::Union{Any,Nothing}
  𝕗::Union{Any,Nothing}
end

(𝕣::FNBefore)(𝕨, 𝕩) =
  𝕨===none ? 𝕣.𝕘(𝕣.𝕗(none, 𝕩), 𝕩) : 𝕣.𝕘(𝕣.𝕗(none, 𝕨), 𝕩)

type(𝕩::FNBefore) = 3.0

bqnbefore′ = M2N(bqnbefore)
@override(bqnbefore′)

# ⟜ bqnafter
bqnafter(@nospecialize(𝕘), @nospecialize(𝕗)) =
  @timeit_debug to "Runtime0.bqnafter" FNAfter(𝕘, bqnafter′, 𝕗)

struct FNAfter
  𝕘::Union{Any,Nothing}
  𝕣::Union{Any,Nothing}
  𝕗::Union{Any,Nothing}
end

(𝕣::FNAfter)(𝕨, 𝕩) =
  𝕨===none ? 𝕣.𝕗(𝕩, 𝕣.𝕘(none, 𝕩)) : 𝕣.𝕗(𝕨, 𝕣.𝕘(none, 𝕩))

type(𝕩::FNAfter) = 3.0

bqnafter′ = M2N(bqnafter)
@override(bqnafter′)

# ◶ bqnchoose
bqnchoose(@nospecialize(𝕘), @nospecialize(𝕗)) =
  @timeit_debug to "Runtime0.bqnchoose" FNChoose(𝕘, bqnchoose′, 𝕗)

struct FNChoose
  𝕘::Union{Any,Nothing}
  𝕣::Union{Any,Nothing}
  𝕗::Union{Any,Nothing}
end

(𝕣::FNChoose)(𝕨, 𝕩) = begin
  𝕗 = Provide.bqnpick(𝕣.𝕗(𝕨, 𝕩), 𝕣.𝕘)
  𝕗(𝕨, 𝕩)
end

type(𝕩::FNChoose) = 3.0

bqnchoose′ = M2N(bqnchoose)
@override(bqnchoose′)

# ⍟ bqnrepeat
bqnrepeat(@nospecialize(𝕘), @nospecialize(𝕗)) =
  @timeit_debug to "Runtime0.bqnrepeat" FNRepeat(𝕘, bqnrepeat′, 𝕗)

struct FNRepeat
  𝕘::Union{Any,Nothing}
  𝕣::Union{Any,Nothing}
  𝕗::Union{Any,Nothing}
end

(𝕣::FNRepeat)(@nospecialize(𝕨), @nospecialize(𝕩)) =
  convert(Bool, 𝕣.𝕘(𝕨, 𝕩)) ? 𝕣.𝕗(𝕨, 𝕩) : 𝕩

type(𝕩::FNRepeat) = 3.0

bqnrepeat′ = M2N(bqnrepeat)
@override(bqnrepeat′)

@specialize

export runtime_0

# for 𝕗 in value
#   types = [None, Any,
#            Float64, Int64, Number, Char,
#            AbstractArray, Vector{Float64}, Vector{Char}]
#   for 𝕨 in types
#     for 𝕩 in types
#       precompile(𝕗, (𝕨, 𝕩))
#     end
#   end
# end

end
