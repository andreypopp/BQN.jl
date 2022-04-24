module Runtime

module R1
import ....provide, ....runtime_0, ....str
include("./r1.jl")
end

import TimerOutputs: @timeit_debug
import TimerOutputs
import ..run, ..BQNError, ..type, ..to
import ..none, ..None, ..F, ..FN, ..TR2D, ..TR3D, ..TR3O, ..M1N, ..M2N, ..Runtime0, ..Provide, ..BQNF

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
  name00 = Symbol("$(name.second)00")
  name0 = Symbol("$(name.second)0")
  if false
    eval(
      quote
        $name00 = $(value[idx])
        if $name00 isa Function || $name00 isa M1N || $name00 isa M2N
          $name0 = $name00
        else
          function $name0(𝕨, 𝕩)
            label = $(name.second)
            @timeit_debug to label $(name00)(𝕨, 𝕩)
          end
        end
      end)
    value[idx] = eval(quote $name0 end)
  else
    eval(quote $name0 = $(value[idx]) end)
  end
end

prim_ind(𝕨, 𝕩) = get(_runtime_indices, 𝕩, float(_runtime_length))

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
    elseif isa(𝕩, Provide.FNScan);        [ 4.0, 𝕩.𝕗, 𝕩.𝕣]
    elseif isa(𝕩, Provide.FNTable);       [ 4.0, 𝕩.𝕗, 𝕩.𝕣]
    elseif isa(𝕩, FNEach);                [ 4.0, 𝕩.𝕗, 𝕩.𝕣]
    elseif isa(𝕩, FNFold);                [ 4.0, 𝕩.𝕗, 𝕩.𝕣]
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
bqnadd(𝕨::Char, 𝕩::Float64) = 𝕨 + Int(𝕩)
bqnadd(𝕨::Float64, 𝕩::Char) = Int(𝕨) + 𝕩
bqnadd(𝕨::Float64, 𝕩::Float64) = float(𝕨 + 𝕩)
bqnadd(𝕨::Union{Float64,Char}, 𝕩::Array) = @along𝕩(bqnadd, 𝕨, 𝕩)
bqnadd(𝕨::Array, 𝕩::Union{Float64,Char}) = @along𝕨(bqnadd, 𝕨, 𝕩)
bqnadd(𝕨::Array, 𝕩::Array) = @along𝕨𝕩(bqnadd, 𝕨, 𝕩)

@override(bqnadd)

# - bqnsub minus
bqnsub(𝕨::None, 𝕩) = float(-𝕩)
bqnsub(𝕨::None, 𝕩::Array) =
  size(𝕩) == () ? collect(float(-𝕩[1])) : bqnsub.(Ref(none), 𝕩)
# + bqnsub substract
bqnsub(𝕨::Char, 𝕩::Float64) = 𝕨 - Int(𝕩)
bqnsub(𝕨::Char, 𝕩::Char) = float(𝕨 - 𝕩)
bqnsub(𝕨::Float64, 𝕩::Float64) = float(𝕨 - 𝕩)
bqnsub(𝕨::Union{Float64,Char}, 𝕩::Array) = @along𝕩(bqnsub, 𝕨, 𝕩)
bqnsub(𝕨::Array, 𝕩::Union{Float64,Char}) = @along𝕨(bqnsub, 𝕨, 𝕩)
bqnsub(𝕨::Array, 𝕩::Array) = @along𝕨𝕩(bqnsub, 𝕨, 𝕩)

@override(bqnsub)

# × bqnmul sign
bqnmul(𝕨::None, 𝕩::Float64) = float(sign(𝕩))
bqnmul(𝕨::None, 𝕩::Array) = bqnmul.(Ref(none), 𝕩)
# × bqnmul multiplication
bqnmul(𝕨::Float64, 𝕩::Float64) = float(𝕨 * 𝕩)
bqnmul(𝕨::Float64, 𝕩::Array) = @along𝕩(bqnmul, 𝕨, 𝕩)
bqnmul(𝕨::Array, 𝕩::Float64) = @along𝕨(bqnmul, 𝕨, 𝕩)
bqnmul(𝕨::Array, 𝕩::Array) = @along𝕨𝕩(bqnmul, 𝕨, 𝕩)

@override(bqnmul)

# ÷ bqndiv
bqndiv(𝕨::None, 𝕩::Float64) = float(1/𝕩)
bqndiv(𝕨::None, 𝕩::Array) = bqndiv.(Ref(none), 𝕩)
# ÷ bqndiv division
bqndiv(𝕨::Float64, 𝕩::Float64) = float(𝕨 / 𝕩)
bqndiv(𝕨::Float64, 𝕩::Array) = @along𝕩(bqndiv, 𝕨, 𝕩)
bqndiv(𝕨::Array, 𝕩::Float64) = @along𝕨(bqndiv, 𝕨, 𝕩)
bqndiv(𝕨::Array, 𝕩::Array) = @along𝕨𝕩(bqndiv, 𝕨, 𝕩)

@override(bqndiv)

# ⋆ bqnpow
bqnpow(𝕨::None, 𝕩::Float64) = float(ℯ^𝕩)
bqnpow(𝕨::None, 𝕩::Array) = bqnpow.(Ref(none), 𝕩)
# ⋆ bqnpow division
bqnpow(𝕨::Float64, 𝕩::Float64) = if 𝕩>=0; float(𝕨^𝕩) else 1/(𝕨^(-𝕩)) end
bqnpow(𝕨::Float64, 𝕩::Array) = @along𝕩(bqnpow, 𝕨, 𝕩)
bqnpow(𝕨::Array, 𝕩::Float64) = @along𝕨(bqnpow, 𝕨, 𝕩)
bqnpow(𝕨::Array, 𝕩::Array) = @along𝕨𝕩(bqnpow, 𝕨, 𝕩)

@override(bqnpow)

# √ bqnroot square root
bqnroot(root::None, 𝕩::Float64) = sqrt(𝕩)
bqnroot(root::None, 𝕩::Array) = sqrt.(𝕩)
# √ bqnroot root
bqnroot(𝕨::Float64, 𝕩::Float64) = 𝕩^(1/𝕨)
bqnroot(𝕨::Float64, 𝕩::Array) = @along𝕩(bqnroot, 𝕨, 𝕩)
bqnroot(𝕨::Array, 𝕩::Float64) = @along𝕨(bqnroot, 𝕨, 𝕩)
bqnroot(𝕨::Array, 𝕩::Array) = @along𝕨𝕩(bqnroot, 𝕨, 𝕩)

@override(bqnroot)

# ⌊ bqnmin floor
bqnmin(𝕨::None, 𝕩::Float64) = float(floor(𝕩))
bqnmin(𝕨::None, 𝕩::Array) = bqnmin.(Ref(𝕨), 𝕩)
# ⌊ bqnmin minimum
bqnmin(𝕨::Float64, 𝕩::Float64) = float(min(𝕨, 𝕩))
bqnmin(𝕨::Float64, 𝕩::Array) = @along𝕩(bqnmin, 𝕨, 𝕩)
bqnmin(𝕨::Array, 𝕩::Float64) = @along𝕨(bqnmin, 𝕨, 𝕩)
bqnmin(𝕨::Array, 𝕩::Array) = @along𝕨𝕩(bqnmin, 𝕨, 𝕩)

@override(bqnmin)

# ⌈ bqnmax ceil
bqnmax(𝕨::None, 𝕩::Float64) =  float(ceil(𝕩))
bqnmax(𝕨::None, 𝕩::Array) = bqnmax.(Ref(none), 𝕩)
# ⌈ bqnmax maximum
bqnmax(𝕨::Float64, 𝕩::Float64) = float(max(𝕨, 𝕩))
bqnmax(𝕨::Float64, 𝕩::Array) = @along𝕩(bqnmax, 𝕨, 𝕩)
bqnmax(𝕨::Array, 𝕩::Float64) = @along𝕨(bqnmax, 𝕨, 𝕩)
bqnmax(𝕨::Array, 𝕩::Array) = @along𝕨𝕩(bqnmax, 𝕨, 𝕩)

@override(bqnmax)

# | bqnabs absolute value
bqnabs(𝕨::None, 𝕩::Float64) = float(abs(𝕩))
bqnabs(𝕨::None, 𝕩::Array) = bqnabs.(Ref(none), 𝕩)
# | bqnabs modulus
bqnabs(𝕨::Float64, 𝕩::Float64) = float(mod(𝕩, 𝕨))
bqnabs(𝕨::Float64, 𝕩::Array) = @along𝕩(bqnabs, 𝕨, 𝕩)
bqnabs(𝕨::Array, 𝕩::Float64) = @along𝕨(bqnabs, 𝕨, 𝕩)
bqnabs(𝕨::Array, 𝕩::Array) = @along𝕨𝕩(bqnabs, 𝕨, 𝕩)

@override(bqnabs)

# ¬ bqnnot not
bqnnot(𝕨::None, 𝕩::Float64) = float(+(1 - 𝕩))
bqnnot(𝕨::None, 𝕩::Array) = bqnnot.(Ref(none), 𝕩)
bqnnot(𝕨, 𝕩) = bqnadd(1.0, bqnsub(𝕨, 𝕩))

@override(bqnnot)

# ≠ bqnneq length
bqnneq(𝕨::None, 𝕩::Vector) = float(length(𝕩))
bqnneq(𝕨::None, 𝕩::Array) = begin
  size𝕩 = size(𝕩)
  float(size𝕩 != () ? size𝕩[end] : 1)
end
bqnneq(𝕨::None, 𝕩::Union{Float64,Char}) = 1.0
# ≠ bqnneq not equals
bqnneq(𝕨, 𝕩) = float(𝕨 != 𝕩)
bqnneq(𝕨, 𝕩::Array) = @along𝕩(bqnneq, 𝕨, 𝕩)
bqnneq(𝕨::Array, 𝕩) = @along𝕨(bqnneq, 𝕨, 𝕩)
bqnneq(𝕨::Array, 𝕩::Array) = @along𝕨𝕩(bqnneq, 𝕨, 𝕩)

@override(bqnneq)

# < bqnlt box
bqnlt(𝕨::None, 𝕩) = fill(𝕩)
# < bqnlt less than
bqnlt(𝕨::Float64, 𝕩::Float64) = float(𝕨 < 𝕩)
bqnlt(𝕨::Char, 𝕩::Char) = float(𝕨 < 𝕩)
bqnlt(𝕨::Char, 𝕩::Float64) = 0.0
bqnlt(𝕨::Float64, 𝕩::Char) = 1.0
bqnlt(𝕨::Union{Float64,Char}, 𝕩::Array) = @along𝕩(bqnlt, 𝕨, 𝕩)
bqnlt(𝕨::Array, 𝕩::Union{Float64,Char}) = @along𝕨(bqnlt, 𝕨, 𝕩)
bqnlt(𝕨::Array, 𝕩::Array) = @along𝕨𝕩(bqnlt, 𝕨, 𝕩)

@override(bqnlt)

# ≤ bqnlte
bqnlte(𝕨::Float64, 𝕩::Float64) = float(𝕨 ≤ 𝕩)
bqnlte(𝕨::Char, 𝕩::Char) = float(𝕨 ≤ 𝕩)
bqnlte(𝕨::Char, 𝕩::Float64) = 0.0
bqnlte(𝕨::Float64, 𝕩::Char) = 1.0
bqnlte(𝕨::Union{Float64,Char}, 𝕩::Array) = @along𝕩(bqnlte, 𝕨, 𝕩)
bqnlte(𝕨::Array, 𝕩::Union{Float64,Char}) = @along𝕨(bqnlte, 𝕨, 𝕩)
bqnlte(𝕨::Array, 𝕩::Array) = @along𝕨𝕩(bqnlte, 𝕨, 𝕩)

@override(bqnlte)

# ≥ bqngte
bqngte(𝕨::Float64, 𝕩::Float64) = float(𝕨 ≥ 𝕩)
bqngte(𝕨::Char, 𝕩::Char) = float(𝕨 ≥ 𝕩)
bqngte(𝕨::Char, 𝕩::Float64) = 1.0
bqngte(𝕨::Float64, 𝕩::Char) = 0.0
bqngte(𝕨::Array, 𝕩) = float(𝕨 ≥ 𝕩)
bqngte(𝕨::Union{Float64,Char}, 𝕩::Array) = @along𝕩(bqngte, 𝕨, 𝕩)
bqngte(𝕨::Array, 𝕩::Union{Float64,Char}) = @along𝕨(bqngte, 𝕨, 𝕩)
bqngte(𝕨::Array, 𝕩::Array) = @along𝕨𝕩(bqngte, 𝕨, 𝕩)

@override(bqngte)

# > bqngt
bqngt(𝕨::None, 𝕩) = bqngt0(𝕨, 𝕩)
# > bqngt greater than
bqngt(𝕨::Float64, 𝕩::Float64) = float(𝕨 > 𝕩)
bqngt(𝕨::Char, 𝕩::Char) = float(𝕨 > 𝕩)
bqngt(𝕨::Char, 𝕩::Float64) = 1.0
bqngt(𝕨::Float64, 𝕩::Char) = 0.0
bqngt(𝕨::Union{Float64,Char}, 𝕩::Array) = @along𝕩(bqngt, 𝕨, 𝕩)
bqngt(𝕨::Array, 𝕩::Union{Float64,Char}) = @along𝕨(bqngt, 𝕨, 𝕩)
bqngt(𝕨::Array, 𝕩::Array) = @along𝕨𝕩(bqngt, 𝕨, 𝕩)

@override(bqngt)

# ≢ bqndepth depth
bqndepth(𝕨::None, 𝕩::Array) =
  isempty(𝕩) ? 1.0 : 1.0 + maximum(bqndepth.(Ref(none), 𝕩))
bqndepth(𝕨::None, 𝕩) = 0.0
# ≢ bqndepth match
bqndepth(𝕨, 𝕩) = float(𝕨 == 𝕩)

@override(bqndepth)

# ≢ bqnshape shape
bqnshape(𝕨::None, 𝕩::Array) = begin
  shape = Float64[x for x in size(𝕩)]
  reverse!(shape)
  shape
end
bqnshape(𝕨::None, 𝕩) = Float64[]
# ≢ bqnshape not match
bqnshape(𝕨, 𝕩) = float(𝕨 != 𝕩)

@override(bqnshape)

# ↕ bqnwindow
bqnwindow(𝕨::None, 𝕩::Float64) = begin
  if !isinteger(𝕩); throw(BQNError("Expected non-negative integer")); end
  Float64[0.0:(𝕩-1.0)...]
end
bqnwindow(𝕨, 𝕩) = bqnwindow0(𝕨, 𝕩) # TODO: ...

@override(bqnwindow)

# ⊏ bqnselect
bqnselect(𝕨::Vector{Float64}, 𝕩::Array) = begin
  size𝕩 = size(𝕩)
  collect(selectdim(𝕩, ndims(𝕩), makeidx.(𝕨, length(size𝕩), Ref(size𝕩))))
end
bqnselect(𝕨::SubArray{Float64}, 𝕩::Array) = begin
  size𝕩 = size(𝕩)
  collect(selectdim(𝕩, ndims(𝕩), makeidx.(𝕨, length(size𝕩), Ref(size𝕩))))
end
bqnselect(𝕨::Vector{Int}, 𝕩::Array) = begin
  size𝕩 = size(𝕩)
  collect(selectdim(𝕩, ndims(𝕩), makeidx.(𝕨, length(size𝕩), Ref(size𝕩))))
end
bqnselect(𝕨::SubArray{Int}, 𝕩::Array) = begin
  size𝕩 = size(𝕩)
  collect(selectdim(𝕩, ndims(𝕩), makeidx.(𝕨, length(size𝕩), Ref(size𝕩))))
end
bqnselect(𝕨::Array, 𝕩::Array) = begin
  length𝕨, size𝕩, ndims𝕩 = length(𝕨), size(𝕩), ndims(𝕩)
  if !isempty(𝕨) && isa(𝕨[1], Array)
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
        if !isa(inds𝕨, Array)
          throw(BQNError("𝕨⊏𝕩: 𝕨 must be an array of numbers or list of such arrays"))
        end
        @inbounds inds[dim𝕩] = makeidx.(𝕨[i𝕨], dim𝕩, Ref(size𝕩))
      else
        @inbounds inds[dim𝕩] = (:)
      end
    end
    getindex(𝕩, inds...)
  else
    collect(selectdim(𝕩, ndims𝕩, makeidx.(𝕨, length(size𝕩), Ref(size𝕩))))
  end
end
bqnselect(𝕨::Float64, 𝕩::Array) = begin
  size𝕩 = size(𝕩)
  collect(selectdim(𝕩, ndims(𝕩), makeidx(𝕨, length(size𝕩), size𝕩)))
end
bqnselect(𝕨, 𝕩) = bqnselect0(𝕨, 𝕩)

makeidx(idx::Float64, d::Int, size::Tuple) = begin
  idx′ = Int(idx)
  idx′ >= 0 ? idx′ + 1 : size[d] + idx′ + 1
end

@override(bqnselect)

# ∨ bqnor Sort Descending
bqnor(𝕨::None, 𝕩) = begin
  ndims𝕩 = ndims(𝕩)
  if ndims𝕩 == 0 || isa(𝕩, String)
    throw(BQNError("∨: Argrument cannot have rank 0"))
  end
  if ndims𝕩 == 1; sort(𝕩, lt=bqnarraylt, rev=true)
  else; sortslices(𝕩, dims=ndims𝕩, lt=bqnarraylt, rev=true)
  end
end
# ∨ bqnor Or
bqnor(𝕨::Float64, 𝕩::Float64) = float((𝕨+𝕩)-(𝕨*𝕩))
bqnor(𝕨::Float64, 𝕩::Array) = @along𝕩(bqnor, 𝕨, 𝕩)
bqnor(𝕨::Array, 𝕩::Float64) = @along𝕨(bqnor, 𝕨, 𝕩)
bqnor(𝕨::Array, 𝕩::Array) = @along𝕨𝕩(bqnor, 𝕨, 𝕩)

@override(bqnor)

# ∧ bqnand Sort Ascending
bqnand(𝕨::None, 𝕩) = begin
  ndims𝕩 = ndims(𝕩)
  if ndims𝕩 == 0
    throw(BQNError("∧: Argrument cannot have rank 0"))
  end
  if ndims𝕩 == 1; sort(𝕩, lt=bqnarraylt)
  else; sortslices(𝕩, dims=ndims𝕩, lt=bqnarraylt)
  end
end
# ∧ bqnand And
bqnand(𝕨::Float64, 𝕩::Float64) = float(𝕨*𝕩)
bqnand(𝕨::Float64, 𝕩::Array) = @along𝕩(bqnand, 𝕨, 𝕩)
bqnand(𝕨::Array, 𝕩::Float64) = @along𝕨(bqnand, 𝕨, 𝕩)
bqnand(𝕨::Array, 𝕩::Array) = @along𝕨𝕩(bqnand, 𝕨, 𝕩)

@override(bqnand)

# ⋈ bqnpair Enlist
# TODO: set fill element
bqnpair(𝕨::None, 𝕩::T) where T = T[𝕩]
# ⋈ bqnpair Pair
# TODO: set fill element
bqnpair(𝕨::T, 𝕩::T) where T = T[𝕨, 𝕩]
bqnpair(𝕨, 𝕩) = [𝕨, 𝕩]

@override(bqnpair)

# ⥊ bqndeshape
bqndeshape(𝕨::None, 𝕩::Array) = collect(reshape(𝕩, :))
# ⥊ bqndeshape
bqndeshape(𝕨::Float64, 𝕩::Array{T}) where T = begin
  len𝕩 = length(𝕩)
  if len𝕩 == 0;
    throw(BQNError("𝕨⥊𝕩: Can't produce non-empty array from empty 𝕩"))
  end
  if 𝕨 < 0
    throw(BQNError("𝕨⥊𝕩: Expected non-negative 𝕨"))
  end
  𝕨, 𝕩 = Int(𝕨), collect(reshape(𝕩, :))
  resize!(𝕩, 𝕨)
  if 𝕨 > len𝕩
    @simd for idx in (len𝕩 + 1):𝕨
      previdx = 1 + ((idx - 1) % len𝕩)
      @inbounds 𝕩[idx] = 𝕩[previdx]
    end
  end
  𝕩
end
bqndeshape(𝕨::Float64, 𝕩::Float64) = begin
  if 𝕨 < 0
    throw(BQNError("𝕨⥊𝕩: Expected non-negative 𝕨"))
  end
  fill(𝕩, (Int(𝕨),))
end
bqndeshape(𝕨, 𝕩) = bqndeshape0(𝕨, 𝕩)

@override(bqndeshape)

# ⊑ bqnpick
bqnpick(𝕨::None, 𝕩::Float64) = 𝕩
bqnpick(𝕨::None, 𝕩) = begin
  if ndims(𝕩) == 1
    if isempty(𝕩)
      throw(BQNError("⊑: Argument cannot be empty"))
    end
    𝕩[1]
  else
    bqnpick0(𝕨, 𝕩)
  end
end
bqnpick(𝕨::Float64, 𝕩::Vector) = 
  if 𝕨 >= 0; 𝕩[Int(𝕨) + 1] else 𝕩[end + (Int(𝕨) + 1)] end
bqnpick(𝕨, 𝕩) = begin
  bqnpick0(𝕨, 𝕩)
end

@override(bqnpick)

bqntake(𝕨::None, 𝕩) = bqntake0(𝕨, 𝕩)
bqntake(𝕨, 𝕩) = begin
  if 𝕨 isa Float64 && 𝕨 >= 0 && ndims(𝕩) == 1
    𝕨 = Int(𝕨)
    len𝕩 = length(𝕩)
    if 𝕨 > length(𝕩)
      𝕩fill = 0.0 # TODO: proper fill
      𝕩 = copy(𝕩)
      resize!(𝕩, 𝕨)
      for i in (len𝕩 + 1):𝕨
        @inbounds 𝕩[i] = 𝕩fill
      end
      𝕩
    else
      𝕩[1:Int(𝕨)]
    end
  else
    bqntake0(𝕨, 𝕩)
  end
end

@override(bqntake)

# = bqneq Rank
bqneq(𝕨::None, 𝕩) = if isa(𝕩, Array); float(ndims(𝕩)) else 0.0 end
# = bqneq Equality
bqneq(𝕨, 𝕩) = begin
  𝕨isarr = isa(𝕨, Array)
  𝕩isarr = isa(𝕩, Array)
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
bqnjoin(𝕨::None, 𝕩::Vector{Vector{T}}) where T = begin
  res = T[]
  for 𝕩e in 𝕩
    for 𝕩ee in 𝕩e
      push!(res, 𝕩ee)
    end
  end
  res
end
bqnjoin(𝕨::None, 𝕩::Vector{Vector}) = begin
  res = Any[]
  for 𝕩e in 𝕩
    for 𝕩ee in 𝕩e
      push!(res, 𝕩ee)
    end
  end
  res
end
bqnjoin(𝕨::Union{Float64,Char}, 𝕩::Union{Float64,Char}) =
  [𝕨, 𝕩]
bqnjoin(𝕨::Union{Float64,Char}, 𝕩::Array) =
  if ndims(𝕩) < 2; collect(vcat(𝕨, 𝕩))
  else bqnjoin0(𝕨, 𝕩) end
bqnjoin(𝕨::Array, 𝕩::Union{Float64,Char}) =
  if ndims(𝕨) < 2; collect(vcat(𝕨, 𝕩))
  else bqnjoin0(𝕨, 𝕩) end
bqnjoin(𝕨::Array, 𝕩::Array) = begin
  if ndims(𝕨) < 2 && ndims(𝕩) < 2; collect(vcat(𝕨, 𝕩))
  elseif length(𝕨) == 0; 𝕩
  elseif length(𝕩) == 0; 𝕨
  else collect(hcat(𝕨, 𝕩)) end
end
bqnjoin(𝕨, 𝕩) = bqnjoin0(𝕨, 𝕩)

@override(bqnjoin)

bqnrevselect(𝕨::None, 𝕩) = begin
  if ndims(𝕩) == 1
    map = Dict()
    Float64[get!(map, 𝕩e, length(map)) for 𝕩e in 𝕩]
  else
    bqnrevselect0(𝕨, 𝕩)
  end
end
bqnrevselect(𝕨, 𝕩) = begin
  bqnrevselect0(𝕨, 𝕩)
end
@override(bqnrevselect)

bqnrev(𝕨::None, @nospecialize(𝕩)) = begin
  if ndims(𝕩) == 1; reverse(𝕩)
  else; bqnrev0(𝕨, 𝕩)
  end
end
bqnrev(@nospecialize(𝕨), @nospecialize(𝕩)) =
  bqnrev0(𝕨, 𝕩)
@override(bqnrev)

bqngroup(𝕨::None, 𝕩) = bqngroup0(𝕨, 𝕩)
bqngroup(𝕨, 𝕩) = begin
  ndims𝕩 = ndims(𝕩)
  if ndims𝕩 == 1 && ndims(𝕨) == 1 && eltype(𝕨) <: Float64
    len𝕨, len𝕩 = length(𝕨), length(𝕩)
    if !(len𝕨 == len𝕩 || len𝕨 == len𝕩 + 1)
      throw(BQNError("⊔: ≠𝕨 must be either ≠𝕩 or one bigger"))
    end
    if len𝕨 == 0; return [] end
    len =
      if len𝕩 == 0 && len𝕨 == 1; 𝕨[end]
      elseif len𝕨 == len𝕩 + 1
        max(𝕨[end], maximum(collect(𝕨)[1:len𝕨-1]) + 1)
      else; maximum(collect(𝕨)) + 1
      end
    if len == 0; return [] end
    groups = [[] for _ in 1:len]
    for (𝕨e, 𝕩e) in zip(𝕨, 𝕩)
      if 𝕨e != -1
        push!(groups[Int(𝕨e) + 1], 𝕩e)
      end
    end
    groups
  else
    bqngroup0(𝕨, 𝕩)
  end
end
@override(bqngroup)

bqnmember(𝕨::None, 𝕩) = begin
  if ndims(𝕩) == 1
    len𝕩 = length(𝕩)
    map = Set()
    res = Float64[]
    resize!(res, len𝕩)
    for i in 1:len𝕩
      @inbounds 𝕩e = 𝕩[i]
      if 𝕩e in map
        @inbounds res[i] = 0.0
      else
        @inbounds res[i] = 1.0
        push!(map, 𝕩e)
      end
    end
    res
  else
    bqnmember0(𝕨, 𝕩)
  end
end
bqnmember(𝕨, 𝕩) = begin
  if ndims(𝕨) == 1 && ndims(𝕩) == 1
    # TODO: O(n×k)
    res = Float64[]
    for 𝕨e in 𝕨
      push!(res,
            findfirst(x -> x == 𝕨e, 𝕩) === nothing ? 0.0 : 1.0)
    end
    res
  else
    bqnmember0(𝕨, 𝕩)
  end
end
@override(bqnmember)

# / bqnreplicate
bqnreplicate(𝕨::Array, 𝕩::Array) = begin
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
bqnreplicate(𝕨::None, 𝕩::Array) = begin
  if ndims(𝕩) != 1
    throw(BQNError("/: Argument must have rank 1"))
  end
  bqnreplicate(𝕩, collect(0.0:(length(𝕩) - 1)))
end
bqnreplicate(𝕨, 𝕩) = bqnreplicate0(𝕨, 𝕩)

@override(bqnreplicate)

# ⍋ bqngradeup
bqngradeup(𝕨::None, 𝕩::Array) = begin
  ndims𝕩 = ndims(𝕩)
  if ndims𝕩 == 1
    float.(sortperm(𝕩, lt=bqnarraylt) .- 1)
  else
    float.(sortperm(collect(eachslice(𝕩, dims=ndims𝕩)), lt=bqnarraylt) .- 1)
  end
end
bqngradeup(𝕨, 𝕩) = begin
  if isa(𝕨, Array) && isa(𝕩, Array) && ndims(𝕨) == 1 && ndims(𝕩) == 1
    res = Float64[]
    for x in 𝕩
      c = 0.0
      for w in 𝕨
        if bqnarraylt(x, w); break end
        c = c + 1.0
      end
      push!(res, c)
    end
    res
  else
    bqngradeup0(𝕨, 𝕩)
  end
end

@override(bqngradeup)

# ⍒ bqngradedown
bqngradedown(𝕨::None, 𝕩::Array) = begin
  ndims𝕩 = ndims(𝕩)
  if ndims𝕩 == 1
    float.(sortperm(𝕩, lt=bqnarraylt, rev=true) .- 1)
  else
    float.(sortperm(collect(eachslice(𝕩, dims=ndims𝕩)), lt=bqnarraylt, rev=true) .- 1)
  end
end
bqngradedown(𝕨, 𝕩) =
  bqngradedown0(𝕨, 𝕩)

@override(bqngradedown)

bqnarraylt(𝕨, 𝕩) =
  bqnarrayord(𝕨, 𝕩) == -1

#  1 ←-→ 𝕨 > 𝕩
# -1 ←-→ 𝕨 < 𝕩
#  - ←-→ 𝕨 ≡ 𝕩
bqnarrayord(𝕨::Float64, 𝕩::Float64) =
  if 𝕨 == 𝕩; return 0
  elseif 𝕨 > 𝕩; return 1
  else; return -1 end
bqnarrayord(𝕨::Char, 𝕩::Char) =
  if 𝕨 == 𝕩; return 0
  elseif 𝕨 > 𝕩; return 1
  else; return -1 end
bqnarrayord(𝕨::Char, 𝕩::Float64) = 1
bqnarrayord(𝕨::Float64, 𝕩::Char) = -1
bqnarrayord(𝕨, 𝕩) = begin
  @nospecialize
  𝕨isarr, 𝕩isarr = isa(𝕨, AbstractArray), isa(𝕩, AbstractArray)
  if 𝕨isarr && 𝕩isarr
    𝕨size, 𝕩size = size(𝕨), size(𝕩)
    if 𝕨size == 𝕩size
      for idx in eachindex(𝕨)
        m = bqnarrayord(𝕨[idx], 𝕩[idx])
        if m != 0; return m end
      end
      return 0
    else
      return bqnarrayord2(𝕨, 𝕩)
    end
  elseif 𝕨isarr
    m = bqnarrayord(𝕨, fill(𝕩))
    return m == 0 ? 1 : m
  elseif 𝕩isarr
    m = bqnarrayord(fill(𝕨), 𝕩)
    return m == 0 ? -1 : m
  else
    throw(BQNError("Invalid comparison"))
  end
end

function bqnarrayord2(𝕨, 𝕩)
  @nospecialize
  𝕨size, 𝕩size = size(𝕨), size(𝕩)
  rankdiff = length(𝕨size) - length(𝕩size)
  𝕨, 𝕩 =
    if rankdiff < 0
      reshape(𝕨, (𝕨size..., fill(1, (-rankdiff,))...)), 𝕩
    elseif rankdiff > 0
      𝕨, reshape(𝕩, (𝕩size..., fill(1, (rankdiff,))...))
    else
      𝕨, 𝕩
    end
  𝕨keys, 𝕩keys = keys(𝕨), keys(𝕩)
  for (𝕨k, 𝕩k) in zip(𝕨keys, 𝕩keys)
    if 𝕨k[length(𝕨k)] != 𝕩k[length(𝕩k)]
      return 𝕨k[length(𝕨k)] > 𝕩k[length(𝕩k)] ? -1 : 1
    end
    m = bqnarrayord(𝕨[𝕨k], 𝕩[𝕩k])
    if m != 0; return m end
  end
  return 𝕨size > 𝕩size ? 1 : -1
end

# » bqnrshift
bqnrshift(𝕨::Union{Char,Float64}, 𝕩::Array) = begin
  if ndims(𝕩) == 1
    len𝕩 = length(𝕩)
    if len𝕩 == 0; 𝕩
    elseif len𝕩 == 1; [𝕨]
    else collect(vcat(𝕨, 𝕩[1:end-1]))
    end
  else
    bqnrshift0(𝕨, 𝕩)
  end
end
bqnrshift(𝕨::None, 𝕩::Array) =
  if ndims(𝕩) == 1
    # TODO: here we must use fill value
    bqnrshift(0.0, 𝕩)
  else
    bqnrshift0(𝕨, 𝕩)
  end
bqnrshift(𝕨, 𝕩) = begin
  bqnrshift0(𝕨, 𝕩)
end

@override(bqnrshift)

# « bqnlshift
bqnlshift(𝕨::Union{Char,Float64}, 𝕩::Vector) = begin
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
bqndrop(𝕨::Float64, 𝕩::Array) = begin
  if ndims(𝕩) == 1; bqndropone(Int(𝕨), 𝕩)
  else bqndrop0(𝕨, 𝕩) end
end
bqndrop(𝕨, 𝕩) = bqndrop0(𝕨, 𝕩)

bqndropone(𝕨::Int, 𝕩::Array) =
  if 𝕨 == 0; 𝕩
  elseif 𝕨 > 0; 𝕩[𝕨+1:end]
  else 𝕩[1:end+𝕨] end

@override(bqndrop)

# ¨ bqneach
bqneach(𝕘::Nothing, 𝕗) = FNEach(bqneach′, 𝕗, bqneach0(𝕘, 𝕗))
bqneach′ = M1N(bqneach)

struct FNEach <: BQNF
  𝕣::M1N
  𝕗::Any
  𝕗0::Any
end

(𝕣::FNEach)(𝕨::None, 𝕩::Array) =
  ndims(𝕩) == 0 ? fill(𝕣.𝕗(𝕨, 𝕩[1])) : 𝕣.𝕗.(Ref(𝕨), 𝕩)
(𝕣::FNEach)(𝕨::None, 𝕩::Float64) =
  fill(𝕣.𝕗(𝕨, 𝕩))
(𝕣::FNEach)(𝕨, 𝕩) =
  𝕣.𝕗0(𝕨, 𝕩)

type(𝕩::FNEach) = 3.0

@override(bqneach′)

# ´ bqnfold
bqnfold(𝕘::Nothing, 𝕗) = FNFold(bqnfold′, 𝕗)
bqnfold′ = M1N(bqnfold)

bqnidentity(𝕗) =
  if     𝕗 == bqnadd; 0.0
  elseif 𝕗 == bqnsub; 0.0
  elseif 𝕗 == bqnmul; 1.0
  elseif 𝕗 == bqndiv; 1.0
  elseif 𝕗 == bqnpow; 1.0
  elseif 𝕗 == bqnnot; 1.0
  elseif 𝕗 == bqnmin; Inf
  elseif 𝕗 == bqnmax; -Inf
  elseif 𝕗 == bqnor; 0.0
  elseif 𝕗 == bqnand; 1.0
  elseif 𝕗 == bqnneq; 0.0
  elseif 𝕗 == bqneq; 1.0
  elseif 𝕗 == bqngt; 0.0
  elseif 𝕗 == bqngte; 1.0
  else throw(BQNError("No identity found"))
  end

struct FNFold <: BQNF
  𝕣::M1N
  𝕗::Any
end

(𝕣::FNFold)(𝕨, 𝕩) = begin
  if ndims(𝕩) != 1
    throw(BQNError("´: Argument must be a list"))
  end
  if 𝕨 == none
    if isempty(𝕩); bqnidentity(𝕣.𝕗)
    else; foldr(𝕣.𝕗, 𝕩)
    end
  else
    foldr(𝕣.𝕗, 𝕩, init=𝕨)
  end
end

type(𝕩::FNFold) = 3.0

@override(bqnfold′)

@specialize

const _runtime_length = length(value)
const _runtime_indices = IdDict(𝕗 => float(idx - 1)
                                for (idx, 𝕗) in enumerate(value))

# _runtime_indices[Provide.bqntable´] = indices["bqntable"]
# _runtime_indices[Provide.bqnscan´] = indices["bqnscan"]

export runtime

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
