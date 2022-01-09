module Runtime

module R1
import ....provide, ....runtime_0, ....str
include("./r1.jl")
end

import TimerOutputs: @timeit_debug
import TimerOutputs
import ..run, ..BQNError
import ..none, ..None, ..F, ..FN, ..TR2D, ..TR3D, ..TR3O, ..M1N, ..M2N, ..Runtime0

const to = TimerOutputs.TimerOutput()

const names = ["+" => "bqnadd",
               "-" => "bqnsub",
               "×" => "bqnmul",
               "÷" => "bqndiv",
               "⋆" => "bqnpow",
               "√" => "bqnroot",
               "⌊" => "bqnmin",
               "⌈" => "bqnmax",
               "|" => "bqnabs",
               "¬" => "bqnnot",
               "∧" => "bqnand",
               "∨" => "bqnor",
               "<" => "bqnlt",
               ">" => "bqngt",
               "≠" => "bqnneq",
               "=" => "bqneq",
               "≤" => "bqnlte",
               "≥" => "bqngte",
               "≡" => "bqndepth",
               "≢" => "bqnshape",
               "⊣" => "bqnleft",
               "⊢" => "bqnright",
               "⥊" => "bqndeshape",
               "∾" => "bqnjoin",
               "≍" => "bqncouple",
               "⋈" => "bqnpair",
               "↑" => "bqntake",
               "↓" => "bqndrop",
               "↕" => "bqnwindow",
               "«" => "bqnlshift",
               "»" => "bqnrshift",
               "⌽" => "bqnrev",
               "⍉" => "bqntranspose",
               "/" => "bqnreplicate",
               "⍋" => "bqngradeup",
               "⍒" => "bqngradedown",
               "⊏" => "bqnselect",
               "⊑" => "bqnpick",
               "⊐" => "bqnrevselect",
               "⊒" => "bqnrevpick",
               "∊" => "bqnmember",
               "⍷" => "bqnfind",
               "⊔" => "bqngroup",
               "!" => "bqnassert",
               "˙" => "bqnconst",
               "˜" => "bqnswap",
               "˘" => "bqncell",
               "¨" => "bqneach",
               "⌜" => "bqntable",
               "⁼" => "bqnundo",
               "´" => "bqnfold",
               "˝" => "bqninsert",
               "`" => "bqnscan",
               "∘" => "bqnatop",
               "○" => "bqnover",
               "⊸" => "bqnbefore",
               "⟜" => "bqnafter",
               "⌾" => "bqnunder",
               "⊘" => "bqnvalences",
               "◶" => "bqnchoose",
               "⎉" => "bqnrank",
               "⚇" => "bqndepthm2",
               "⍟" => "bqnrepeat",
               "⎊" => "bqncatch",
              ]

const indices = Dict{String, Int}(name.second => idx
                                  for (idx, name) in enumerate(names))

const value, set_prims, set_inv = run("<none>", R1.value...)

for (idx, name) in enumerate(names)
  name0 = Symbol("$(name.second)0")
  eval(quote $name0 = $(value[idx]) end)
end

prim_ind(𝕨, 𝕩) = get(_runtime_indices, 𝕩, _runtime_length)

function decompose(𝕨, 𝕩)
  @nospecialize
  @timeit_debug to "decompose" begin
    if haskey(_runtime_indices, 𝕩);       [ 0.0, 𝕩]
    elseif isa(𝕩, F) && 𝕩.𝕘 !== nothing;  [ 5.0, 𝕩.𝕗, 𝕩.𝕣, 𝕩.𝕘]
    elseif isa(𝕩, FN) && 𝕩.𝕘 !== nothing; [ 5.0, 𝕩.𝕗, 𝕩.𝕣, 𝕩.𝕘]
    elseif isa(𝕩, Runtime0.FNChoose);     [ 5.0, 𝕩.𝕗, 𝕩.𝕣, 𝕩.𝕘]
    elseif isa(𝕩, Runtime0.FNAfter);      [ 5.0, 𝕩.𝕗, 𝕩.𝕣, 𝕩.𝕘]
    elseif isa(𝕩, Runtime0.FNBefore);     [ 5.0, 𝕩.𝕗, 𝕩.𝕣, 𝕩.𝕘]
    elseif isa(𝕩, Runtime0.FNRepeat);     [ 5.0, 𝕩.𝕗, 𝕩.𝕣, 𝕩.𝕘]
    elseif isa(𝕩, Runtime0.FNAtop);       [ 5.0, 𝕩.𝕗, 𝕩.𝕣, 𝕩.𝕘]
    elseif isa(𝕩, Runtime0.FNOver);       [ 5.0, 𝕩.𝕗, 𝕩.𝕣, 𝕩.𝕘]
    elseif isa(𝕩, Runtime0.FNFold);       [ 4.0, 𝕩.𝕗, 𝕩.𝕣]
    elseif isa(𝕩, Runtime0.FNConst);      [ 4.0, 𝕩.𝕗, 𝕩.𝕣]
    elseif isa(𝕩, Runtime0.FNSwap);       [ 4.0, 𝕩.𝕗, 𝕩.𝕣]
    elseif isa(𝕩, Runtime0.FNEach);       [ 4.0, 𝕩.𝕗, 𝕩.𝕣]
    elseif isa(𝕩, F) && 𝕩.𝕗 !== nothing;  [ 4.0, 𝕩.𝕗, 𝕩.𝕣]
    elseif isa(𝕩, FN) && 𝕩.𝕗 !== nothing; [ 4.0, 𝕩.𝕗, 𝕩.𝕣]
    elseif isa(𝕩, F);                     [ 1.0, 𝕩]
    elseif isa(𝕩, FN);                    [ 1.0, 𝕩]
    elseif isa(𝕩, TR2D);                  [ 2.0, 𝕩.h, 𝕩.𝕘]
    elseif isa(𝕩, TR3D);                  [ 3.0, 𝕩.𝕘, 𝕩.h, 𝕩.𝕗]
    elseif isa(𝕩, TR3O);                  [ 3.0, 𝕩.𝕘, 𝕩.h, 𝕩.𝕗]
    else                                  [-1.0, 𝕩]
    end
  end
end

set_prims(none, [decompose, prim_ind])

runtime(n::Int64) = value[n + 1]

funname(𝕗::Function) = string(Symbol(𝕗))
funname(𝕗::Union{M1N,M2N}) = funname(𝕗.run)

macro override(𝕗)
  eval(quote value[indices[funname($𝕗)]] = $𝕗 end)
end

""" Generate a function body which broadcasts 𝕗 along the leading axis."""
macro along𝕨𝕩(𝕗, 𝕨, 𝕩)
  quote
    𝕗, 𝕨, 𝕩 = $(esc(𝕗)), $(esc(𝕨)), $(esc(𝕩))
    size𝕨, size𝕩 = size(𝕨), size(𝕩)
    ndims𝕨, ndims𝕩 = ndims(𝕨), ndims(𝕩)
    if ndims𝕨 == 0 && ndims𝕩 == 0
      fill(𝕗(𝕨[1], 𝕩[1]))
    elseif ndims𝕨 == 0
      𝕗.(𝕨, 𝕩)
    elseif ndims𝕩 == 0
      𝕗.(𝕨, 𝕩)
    elseif ndims𝕨 == ndims𝕩
      if size𝕨 != size𝕩; throw(BQNError("Expected equal shape prefix")) end
      𝕗.(𝕨, 𝕩)
    elseif ndims𝕨 < ndims𝕩
      for n in 0:(ndims𝕨 - 1)
        if size𝕨[end-n] != size𝕩[end-n]
          throw(BQNError("Expected equal shape prefix"))
        end
      end
      i = 0
      mapslices(function(𝕩i)
                  i += 1
                  𝕨i = ndims𝕨 < 2 ? 𝕨[i] : selectdim(𝕨, length(size𝕨), i)
                  𝕗(𝕨i, 𝕩i)
                end, 𝕩, dims=1:(ndims𝕩-1))
    else # ndims𝕨 > ndims𝕩
      for n in 0:(ndims𝕩 - 1)
        if size𝕨[end-n] != size𝕩[end-n]
          throw(BQNError("Expected equal shape prefix"))
        end
      end
      i = 0
      mapslices(function(𝕨i)
                  i += 1
                  𝕩i = ndims𝕩 < 2 ? 𝕩[i] : selectdim(𝕩, length(size𝕩), i)
                  𝕗(𝕨i, 𝕩i)
                end, 𝕨, dims=1:(ndims𝕨-1))
    end
  end
end

macro along𝕨(𝕗, 𝕨, 𝕩)
  quote
    𝕗, 𝕨, 𝕩 = $(esc(𝕗)), $(esc(𝕨)), $(esc(𝕩))
    size(𝕨) == () ? collect(𝕗(𝕨[1], 𝕩)) : 𝕗.(𝕨, 𝕩)
  end
end

macro along𝕩(𝕗, 𝕨, 𝕩)
  quote
    𝕗, 𝕨, 𝕩 = $(esc(𝕗)), $(esc(𝕨)), $(esc(𝕩))
    size(𝕩) == () ? collect(𝕗(𝕨, 𝕩[1])) : 𝕗.(𝕨, 𝕩)
  end
end

@nospecialize
# + bqnadd plus
bqnadd(𝕨::None, 𝕩) = 𝕩
# + bqnadd addition
bqnadd(𝕨::Char, 𝕩::Number) = 𝕨 + Int(𝕩)
bqnadd(𝕨::Number, 𝕩::Char) = Int(𝕨) + 𝕩
bqnadd(𝕨::Number, 𝕩::Number) = float(𝕨 + 𝕩)
bqnadd(𝕨::Union{Number,Char}, 𝕩::AbstractArray) = @along𝕩(bqnadd, 𝕨, 𝕩)
bqnadd(𝕨::AbstractArray, 𝕩::Union{Number,Char}) = @along𝕨(bqnadd, 𝕨, 𝕩)
bqnadd(𝕨::AbstractArray, 𝕩::AbstractArray) = @along𝕨𝕩(bqnadd, 𝕨, 𝕩)

@override(bqnadd)

# - bqnsub minus
bqnsub(𝕨::None, 𝕩) = float(-𝕩)
bqnsub(𝕨::None, 𝕩::AbstractArray) =
  size(𝕩) == () ? collect(float(-𝕩[1])) : bqnsub.(Ref(none), 𝕩)
# + bqnsub substract
bqnsub(𝕨::Char, 𝕩::Number) = 𝕨 - Int(𝕩)
bqnsub(𝕨::Char, 𝕩::Char) = float(𝕨 - 𝕩)
bqnsub(𝕨::Number, 𝕩::Number) = float(𝕨 - 𝕩)
bqnsub(𝕨::Union{Number,Char}, 𝕩::AbstractArray) = @along𝕩(bqnsub, 𝕨, 𝕩)
bqnsub(𝕨::AbstractArray, 𝕩::Union{Number,Char}) = @along𝕨(bqnsub, 𝕨, 𝕩)
bqnsub(𝕨::AbstractArray, 𝕩::AbstractArray) = @along𝕨𝕩(bqnsub, 𝕨, 𝕩)

@override(bqnsub)

# × bqnmul sign
bqnmul(𝕨::None, 𝕩::Number) = float(sign(𝕩))
bqnmul(𝕨::None, 𝕩::AbstractArray) = bqnmul.(Ref(none), 𝕩)
# × bqnmul multiplication
bqnmul(𝕨::Number, 𝕩::Number) = float(𝕨 * 𝕩)
bqnmul(𝕨::Number, 𝕩::AbstractArray) = @along𝕩(bqnmul, 𝕨, 𝕩)
bqnmul(𝕨::AbstractArray, 𝕩::Number) = @along𝕨(bqnmul, 𝕨, 𝕩)
bqnmul(𝕨::AbstractArray, 𝕩::AbstractArray) = @along𝕨𝕩(bqnmul, 𝕨, 𝕩)

@override(bqnmul)

# ÷ bqndiv
bqndiv(𝕨::None, 𝕩::Number) = float(1/𝕩)
bqndiv(𝕨::None, 𝕩::AbstractArray) = bqndiv.(Ref(none), 𝕩)
# ÷ bqndiv division
bqndiv(𝕨::Number, 𝕩::Number) = float(𝕨 / 𝕩)
bqndiv(𝕨::Number, 𝕩::AbstractArray) = @along𝕩(bqndiv, 𝕨, 𝕩)
bqndiv(𝕨::AbstractArray, 𝕩::Number) = @along𝕨(bqndiv, 𝕨, 𝕩)
bqndiv(𝕨::AbstractArray, 𝕩::AbstractArray) = @along𝕨𝕩(bqndiv, 𝕨, 𝕩)

@override(bqndiv)

# ⋆ bqnpow
bqnpow(𝕨::None, 𝕩::Number) = float(ℯ^𝕩)
bqnpow(𝕨::None, 𝕩::AbstractArray) = bqnpow.(Ref(none), 𝕩)
# ⋆ bqnpow division
bqnpow(𝕨::Number, 𝕩::Number) = if 𝕩>=0; float(𝕨^𝕩) else 1/(𝕨^(-𝕩)) end
bqnpow(𝕨::Number, 𝕩::AbstractArray) = @along𝕩(bqnpow, 𝕨, 𝕩)
bqnpow(𝕨::AbstractArray, 𝕩::Number) = @along𝕨(bqnpow, 𝕨, 𝕩)
bqnpow(𝕨::AbstractArray, 𝕩::AbstractArray) = @along𝕨𝕩(bqnpow, 𝕨, 𝕩)

@override(bqnpow)

# √ bqnroot square root
bqnroot(root::None, 𝕩::Number) = sqrt(𝕩)
bqnroot(root::None, 𝕩::AbstractArray) = sqrt.(𝕩)
# √ bqnroot root
bqnroot(𝕨::Number, 𝕩::Number) = 𝕩^(1/𝕨)
bqnroot(𝕨::Number, 𝕩::AbstractArray) = @along𝕩(bqnroot, 𝕨, 𝕩)
bqnroot(𝕨::AbstractArray, 𝕩::Number) = @along𝕨(bqnroot, 𝕨, 𝕩)
bqnroot(𝕨::AbstractArray, 𝕩::AbstractArray) = @along𝕨𝕩(bqnroot, 𝕨, 𝕩)

@override(bqnroot)

# ⌊ bqnmin floor
bqnmin(𝕨::None, 𝕩::Number) = float(floor(𝕩))
bqnmin(𝕨::None, 𝕩::AbstractArray) = bqnmin.(Ref(𝕨), 𝕩)
# ⌊ bqnmin minimum
bqnmin(𝕨::Number, 𝕩::Number) = float(min(𝕨, 𝕩))
bqnmin(𝕨::Number, 𝕩::AbstractArray) = @along𝕩(bqnmin, 𝕨, 𝕩)
bqnmin(𝕨::AbstractArray, 𝕩::Number) = @along𝕨(bqnmin, 𝕨, 𝕩)
bqnmin(𝕨::AbstractArray, 𝕩::AbstractArray) = @along𝕨𝕩(bqnmin, 𝕨, 𝕩)

@override(bqnmin)

# ⌈ bqnmax ceil
bqnmax(𝕨::None, 𝕩::Number) =  float(ceil(𝕩))
bqnmax(𝕨::None, 𝕩::AbstractArray) = bqnmax.(Ref(none), 𝕩)
# ⌈ bqnmax maximum
bqnmax(𝕨::Number, 𝕩::Number) = float(max(𝕨, 𝕩))
bqnmax(𝕨::Number, 𝕩::AbstractArray) = @along𝕩(bqnmax, 𝕨, 𝕩)
bqnmax(𝕨::AbstractArray, 𝕩::Number) = @along𝕨(bqnmax, 𝕨, 𝕩)
bqnmax(𝕨::AbstractArray, 𝕩::AbstractArray) = @along𝕨𝕩(bqnmax, 𝕨, 𝕩)

@override(bqnmax)

# | bqnabs absolute value
bqnabs(𝕨::None, 𝕩::Number) = float(abs(𝕩))
bqnabs(𝕨::None, 𝕩::AbstractArray) = bqnabs.(Ref(none), 𝕩)
# | bqnabs modulus
bqnabs(𝕨::Number, 𝕩::Number) = float(mod(𝕩, 𝕨))
bqnabs(𝕨::Number, 𝕩::AbstractArray) = @along𝕩(bqnabs, 𝕨, 𝕩)
bqnabs(𝕨::AbstractArray, 𝕩::Number) = @along𝕨(bqnabs, 𝕨, 𝕩)
bqnabs(𝕨::AbstractArray, 𝕩::AbstractArray) = @along𝕨𝕩(bqnabs, 𝕨, 𝕩)

@override(bqnabs)

# ¬ bqnnot not
bqnnot(𝕨::None, 𝕩::Number) = float(+(1 - 𝕩))
bqnnot(𝕨::None, 𝕩::AbstractArray) = bqnnot.(Ref(none), 𝕩)
bqnnot(𝕨, 𝕩) = bqnnot0(𝕨, 𝕩)

@override(bqnnot)

# ≠ bqnneq length
bqnneq(𝕨::None, 𝕩::Vector) = float(length(𝕩))
bqnneq(𝕨::None, 𝕩::AbstractArray) = begin
  size𝕩 = size(𝕩)
  float(size𝕩 != () ? size𝕩[end] : 1)
end
bqnneq(𝕨::None, 𝕩::Union{Number,Char}) = 1.0
# ≠ bqnneq not equals
bqnneq(𝕨, 𝕩) = float(𝕨 != 𝕩)
bqnneq(𝕨, 𝕩::AbstractArray) = @along𝕩(bqnneq, 𝕨, 𝕩)
bqnneq(𝕨::AbstractArray, 𝕩) = @along𝕨(bqnneq, 𝕨, 𝕩)
bqnneq(𝕨::AbstractArray, 𝕩::AbstractArray) = @along𝕨𝕩(bqnneq, 𝕨, 𝕩)

@override(bqnneq)

# < bqnlt box
bqnlt(𝕨::None, 𝕩) = fill(𝕩)
# < bqnlt less than
bqnlt(𝕨::Number, 𝕩::Number) = float(𝕨 < 𝕩)
bqnlt(𝕨::Char, 𝕩::Char) = float(𝕨 < 𝕩)
bqnlt(𝕨::Char, 𝕩::Number) = 0.0
bqnlt(𝕨::Number, 𝕩::Char) = 1.0
bqnlt(𝕨::Union{Number,Char}, 𝕩::AbstractArray) = @along𝕩(bqnlt, 𝕨, 𝕩)
bqnlt(𝕨::AbstractArray, 𝕩::Union{Number,Char}) = @along𝕨(bqnlt, 𝕨, 𝕩)
bqnlt(𝕨::AbstractArray, 𝕩::AbstractArray) = @along𝕨𝕩(bqnlt, 𝕨, 𝕩)

@override(bqnlt)

# ≤ bqnlte
bqnlte(𝕨::Number, 𝕩::Number) = float(𝕨 ≤ 𝕩)
bqnlte(𝕨::Char, 𝕩::Char) = float(𝕨 ≤ 𝕩)
bqnlte(𝕨::Char, 𝕩::Number) = 0.0
bqnlte(𝕨::Number, 𝕩::Char) = 1.0
bqnlte(𝕨::Union{Number,Char}, 𝕩::AbstractArray) = @along𝕩(bqnlte, 𝕨, 𝕩)
bqnlte(𝕨::AbstractArray, 𝕩::Union{Number,Char}) = @along𝕨(bqnlte, 𝕨, 𝕩)
bqnlte(𝕨::AbstractArray, 𝕩::AbstractArray) = @along𝕨𝕩(bqnlte, 𝕨, 𝕩)

@override(bqnlte)

# ≥ bqngte
bqngte(𝕨::Number, 𝕩::Number) = float(𝕨 ≥ 𝕩)
bqngte(𝕨::Char, 𝕩::Char) = float(𝕨 ≥ 𝕩)
bqngte(𝕨::Char, 𝕩::Number) = 1.0
bqngte(𝕨::Number, 𝕩::Char) = 0.0
bqngte(𝕨::AbstractArray, 𝕩) = float(𝕨 ≥ 𝕩)
bqngte(𝕨::Union{Number,Char}, 𝕩::AbstractArray) = @along𝕩(bqngte, 𝕨, 𝕩)
bqngte(𝕨::AbstractArray, 𝕩::Union{Number,Char}) = @along𝕨(bqngte, 𝕨, 𝕩)
bqngte(𝕨::AbstractArray, 𝕩::AbstractArray) = @along𝕨𝕩(bqngte, 𝕨, 𝕩)

@override(bqngte)

# > bqngt
bqngt(𝕨::None, 𝕩) = bqngt0(𝕨, 𝕩)
# > bqngt greater than
bqngt(𝕨::Number, 𝕩::Number) = float(𝕨 > 𝕩)
bqngt(𝕨::Char, 𝕩::Char) = float(𝕨 > 𝕩)
bqngt(𝕨::Char, 𝕩::Number) = 1.0
bqngt(𝕨::Number, 𝕩::Char) = 0.0
bqngt(𝕨::Union{Number,Char}, 𝕩::AbstractArray) = @along𝕩(bqngt, 𝕨, 𝕩)
bqngt(𝕨::AbstractArray, 𝕩::Union{Number,Char}) = @along𝕨(bqngt, 𝕨, 𝕩)
bqngt(𝕨::AbstractArray, 𝕩::AbstractArray) = @along𝕨𝕩(bqngt, 𝕨, 𝕩)

@override(bqngt)

# ≢ bqndepth depth
bqndepth(𝕨::None, 𝕩::AbstractArray) =
  isempty(𝕩) ? 1.0 : 1.0 + maximum(bqndepth.(Ref(none), 𝕩))
bqndepth(𝕨::None, 𝕩) = 0.0
# ≢ bqndepth match
bqndepth(𝕨, 𝕩) = float(𝕨 == 𝕩)

@override(bqndepth)

# ≢ bqnshape shape
bqnshape(𝕨::None, 𝕩::AbstractArray) = begin
  shape = Float64[x for x in size(𝕩)]
  reverse!(shape)
  shape
end
bqnshape(𝕨::None, 𝕩) = Float64[]
# ≢ bqnshape not match
bqnshape(𝕨, 𝕩) = float(𝕨 != 𝕩)

@override(bqnshape)

# ↕ bqnwindow
bqnwindow(𝕨::None, 𝕩::Number) = begin
  if !isinteger(𝕩); throw(BQNError("Expected non-negative integer")); end
  Float64[0.0:(𝕩-1.0)...]
end
bqnwindow(𝕨, 𝕩) = bqnwindow0(𝕨, 𝕩) # TODO: ...

@override(bqnwindow)

# ⊏ bqnselect
bqnselect(𝕨::Vector{Float64}, 𝕩::AbstractArray) = begin
  size𝕩 = size(𝕩)
  selectdim(𝕩, ndims(𝕩), makeidx.(𝕨, length(size𝕩), Ref(size𝕩)))
end
bqnselect(𝕨::SubArray{Float64}, 𝕩::AbstractArray) = begin
  size𝕩 = size(𝕩)
  selectdim(𝕩, ndims(𝕩), makeidx.(𝕨, length(size𝕩), Ref(size𝕩)))
end
bqnselect(𝕨::Vector{Int}, 𝕩::AbstractArray) = begin
  size𝕩 = size(𝕩)
  selectdim(𝕩, ndims(𝕩), makeidx.(𝕨, length(size𝕩), Ref(size𝕩)))
end
bqnselect(𝕨::SubArray{Int}, 𝕩::AbstractArray) = begin
  size𝕩 = size(𝕩)
  selectdim(𝕩, ndims(𝕩), makeidx.(𝕨, length(size𝕩), Ref(size𝕩)))
end
bqnselect(𝕨::AbstractArray, 𝕩::AbstractArray) = begin
  length𝕨, size𝕩, ndims𝕩 = length(𝕨), size(𝕩), ndims(𝕩)
  if !isempty(𝕨) && isa(𝕨[1], AbstractArray)
    if ndims(𝕨) > 1
      throw(BQNError("𝕨⊏𝕩: Compound 𝕨 must have rank at most 1"))
    end
    if length𝕨 > ndims𝕩
      throw(BQNError("𝕨⊏𝕩: Length of compound 𝕨 must be at most rank of 𝕩"))
    end
    inds = Array{Any}(undef, ndims𝕩)
    for dim𝕩 in ndims𝕩:-1:1
      i𝕨 = ndims𝕩 - dim𝕩 + 1
      if i𝕨 ≤ length𝕨
        @inbounds inds𝕨 = 𝕨[i𝕨]
        if !isa(inds𝕨, AbstractArray)
          throw(BQNError("𝕨⊏𝕩: 𝕨 must be an array of numbers or list of such arrays"))
        end
        @inbounds inds[dim𝕩] = makeidx.(𝕨[i𝕨], dim𝕩, Ref(size𝕩))
      else
        @inbounds inds[dim𝕩] = (:)
      end
    end
    getindex(𝕩, inds...)
  else
    selectdim(𝕩, ndims𝕩, makeidx.(𝕨, length(size𝕩), Ref(size𝕩)))
  end
end
bqnselect(𝕨::Number, 𝕩::AbstractArray) = begin
  size𝕩 = size(𝕩)
  selectdim(𝕩, ndims(𝕩), makeidx(𝕨, length(size𝕩), size𝕩))
end
bqnselect(𝕨, 𝕩) = bqnselect0(𝕨, 𝕩)

makeidx(idx::Number, d::Int, size::Tuple) = begin
  idx′ = Int(idx)
  idx′ >= 0 ? idx′ + 1 : size[d] + idx′ + 1
end

@override(bqnselect)

# ∨ bqnor Sort Descending
bqnor(𝕨::None, 𝕩::Vector) = sort(𝕩, rev=true)
bqnor(𝕨::None, 𝕩) = bqnor0(𝕨, 𝕩)
# ∨ bqnor Or
bqnor(𝕨::Number, 𝕩::Number) = float((𝕨+𝕩)-(𝕨*𝕩))
bqnor(𝕨::Number, 𝕩::AbstractArray) = @along𝕩(bqnor, 𝕨, 𝕩)
bqnor(𝕨::AbstractArray, 𝕩::Number) = @along𝕨(bqnor, 𝕨, 𝕩)
bqnor(𝕨::AbstractArray, 𝕩::AbstractArray) = @along𝕨𝕩(bqnor, 𝕨, 𝕩)

@override(bqnor)

# ∧ bqnand Sort Ascending
bqnand(𝕨::None, 𝕩::Vector) = sort(𝕩)
bqnand(𝕨::None, 𝕩) = bqnand0(𝕨, 𝕩)
# ∧ bqnand And
bqnand(𝕨::Number, 𝕩::Number) = float(𝕨*𝕩)
bqnand(𝕨::Number, 𝕩::AbstractArray) = @along𝕩(bqnand, 𝕨, 𝕩)
bqnand(𝕨::AbstractArray, 𝕩::Number) = @along𝕨(bqnand, 𝕨, 𝕩)
bqnand(𝕨::AbstractArray, 𝕩::AbstractArray) = @along𝕨𝕩(bqnand, 𝕨, 𝕩)

@override(bqnand)

# ⊑ bqnpick
bqnpick(𝕨::None, 𝕩::Number) = 𝕩
bqnpick(𝕨::None, 𝕩) = bqnpick0(𝕨, 𝕩)
bqnpick(𝕨::Number, 𝕩::Vector) = 
  if 𝕨 >= 0; 𝕩[Int(𝕨) + 1] else 𝕩[end + (Int(𝕨) + 1)] end
bqnpick(𝕨, 𝕩) = bqnpick0(𝕨, 𝕩)

@override(bqnpick)

# = bqneq Rank
bqneq(𝕨::None, 𝕩) = if isa(𝕩, AbstractArray); float(ndims(𝕩)) else 0.0 end
# = bqneq Equality
bqneq(𝕨, 𝕩) = begin
  𝕨isarr = isa(𝕨, AbstractArray)
  𝕩isarr = isa(𝕩, AbstractArray)
  if 𝕨isarr && 𝕩isarr
    @along𝕨𝕩(bqneq, 𝕨, 𝕩)
  elseif 𝕩isarr
    @along𝕩(bqneq, 𝕨, 𝕩)
  elseif 𝕨isarr
    @along𝕨(bqneq, 𝕨, 𝕩)
  else
    float(𝕨 == 𝕩)
  end
end

@override(bqneq)

# ∾ bqnjoin
bqnjoin(𝕨::None, 𝕩::Vector) = bqnjoin0(𝕨, 𝕩)
bqnjoin(𝕨::Union{Number,Char}, 𝕩::Union{Number,Char}) =
  [𝕨, 𝕩]
bqnjoin(𝕨::Union{Number,Char}, 𝕩::AbstractArray) =
  if ndims(𝕩) < 2; vcat(𝕨, 𝕩)
  else bqnjoin0(𝕨, 𝕩) end
bqnjoin(𝕨::AbstractArray, 𝕩::Union{Number,Char}) =
  if ndims(𝕨) < 2; vcat(𝕨, 𝕩)
  else bqnjoin0(𝕨, 𝕩) end
bqnjoin(𝕨::AbstractArray, 𝕩::AbstractArray) = begin
  if ndims(𝕨) < 2 && ndims(𝕩) < 2; vcat(𝕨, 𝕩)
  elseif length(𝕨) == 0; 𝕩
  elseif length(𝕩) == 0; 𝕨
  else hcat(𝕨, 𝕩) end
end
bqnjoin(𝕨, 𝕩) = begin
  bqnjoin0(𝕨, 𝕩)
end

@override(bqnjoin)

# / bqnreplicate
bqnreplicate(𝕨::AbstractArray, 𝕩::AbstractArray) = begin
  ndims𝕨, ndims𝕩 = ndims(𝕨), ndims(𝕩)
  if !(ndims𝕨 == 1 && ndims𝕩 == 1); return bqnreplicate0(𝕨, 𝕩) end
  if length(𝕨) == 0; return 𝕩 end
  if length(𝕨) != length(𝕩); throw(BQNError("/: length mismatch")) end
  s = 0
  for n in 𝕨
    if n < 0; throw(BQNError("/: negative number")) end
    s = s + n
  end
  z = similar(𝕩, Int(s))
  i = 0
  for (x, n) in zip(𝕩, 𝕨), k = 1:n
    @inbounds z[i += 1] = x
  end
  z
end
bqnreplicate(𝕨::None, 𝕩::AbstractArray) = begin
  if ndims(𝕩) != 1; return bqnreplicate0(𝕨, 𝕩) end
  bqnreplicate(𝕩, 0.0:(length(𝕩) - 1))
end
bqnreplicate(𝕨, 𝕩) = bqnreplicate0(𝕨, 𝕩)

@override(bqnreplicate)

# » bqnrshift
bqnrshift(𝕨::Union{Char,Number}, 𝕩::Vector) = begin
  len𝕩 = length(𝕩)
  if len𝕩 == 0; 𝕩
  elseif len𝕩 == 1; [𝕨]
  else vcat(𝕨, 𝕩[1:end-1])
  end
end
bqnrshift(𝕨::None, 𝕩::Vector) =
  # TODO: here we must use fill value
  bqnrshift(0.0, 𝕩)
bqnrshift(𝕨, 𝕩) = bqnrshift0(𝕨, 𝕩)

@override(bqnrshift)

# « bqnlshift
bqnlshift(𝕨::Union{Char,Number}, 𝕩::Vector) = begin
  len𝕩 = length(𝕩)
  if len𝕩 == 0; 𝕩
  elseif len𝕩 == 1; [𝕨]
  else vcat(𝕩[2:end], 𝕨)
  end
end
bqnlshift(𝕨::None, 𝕩::Vector) =
  # TODO: here we must use fill value
  bqnlshift(0.0, 𝕩)
bqnlshift(𝕨, 𝕩) = bqnlshift0(𝕨, 𝕩)

@override(bqnlshift)

# ↓ bqndrop
bqndrop(𝕨::Number, 𝕩::AbstractArray) = begin
  if ndims(𝕩) == 1; bqndropone(Int(𝕨), 𝕩)
  else bqndrop0(𝕨, 𝕩) end
end
bqndrop(𝕨, 𝕩) = bqndrop0(𝕨, 𝕩)

bqndropone(𝕨::Int, 𝕩::AbstractArray) =
  if 𝕨 == 0; 𝕩
  elseif 𝕨 > 0; 𝕩[𝕨+1:end]
  else 𝕩[1:end+𝕨] end

@override(bqndrop)
@specialize

const _runtime_length = length(value)
const _runtime_indices = IdDict(𝕗 => idx - 1
                                for (idx, 𝕗) in enumerate(value))

export runtime

end
