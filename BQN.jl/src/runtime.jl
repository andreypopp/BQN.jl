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
               "⊑" => "bqnpick"]

const indices = Dict{String, Int}(name.second => idx
                                  for (idx, name) in enumerate(names))

const value, set_prims, set_inv = run("<none>", R1.value...)

for (idx, name) in enumerate(names)
  name = Symbol("$(name.second)0")
  eval(quote       $(name) = $(value[idx]) end)
end

function set_override(func::Any; name=nothing)
  if name === nothing; name = string(Symbol(func)) end
  idx = indices[name]
  value[idx] = func
end
set_override(func::M1N) = set_override(func, name=string(Symbol(func.run)))
set_override(func::M2N) = set_override(func, name=string(Symbol(func.run)))

prim_ind(𝕨, 𝕩) = get(_runtime_indices, 𝕩, _runtime_length)

function decompose(𝕨, 𝕩)
  kind =
    if     𝕩 in value;                    [0, 𝕩]
    elseif isa(𝕩, F) && 𝕩.𝕘 !== nothing;  [5, 𝕩.𝕗, 𝕩.𝕣, 𝕩.𝕘]
    elseif isa(𝕩, FN) && 𝕩.𝕘 !== nothing; [5, 𝕩.𝕗, 𝕩.𝕣, 𝕩.𝕘]
    elseif isa(𝕩, Runtime0.FNChoose);     [5, 𝕩.𝕗, 𝕩.𝕣, 𝕩.𝕘]
    elseif isa(𝕩, Runtime0.FNAfter);      [5, 𝕩.𝕗, 𝕩.𝕣, 𝕩.𝕘]
    elseif isa(𝕩, Runtime0.FNBefore);     [5, 𝕩.𝕗, 𝕩.𝕣, 𝕩.𝕘]
    elseif isa(𝕩, Runtime0.FNRepeat);     [5, 𝕩.𝕗, 𝕩.𝕣, 𝕩.𝕘]
    elseif isa(𝕩, Runtime0.FNAtop);       [5, 𝕩.𝕗, 𝕩.𝕣, 𝕩.𝕘]
    elseif isa(𝕩, Runtime0.FNOver);       [5, 𝕩.𝕗, 𝕩.𝕣, 𝕩.𝕘]
    elseif isa(𝕩, F) && 𝕩.𝕗 !== nothing;  [4, 𝕩.𝕗, 𝕩.𝕣]
    elseif isa(𝕩, FN) && 𝕩.𝕗 !== nothing; [4, 𝕩.𝕗, 𝕩.𝕣]
    elseif isa(𝕩, F);                     [1, 𝕩]
    elseif isa(𝕩, FN);                    [1, 𝕩]
    elseif isa(𝕩, TR2D);                  [2, 𝕩.h, 𝕩.𝕘]
    elseif isa(𝕩, TR3D);                  [3, 𝕩.𝕘, 𝕩.h, 𝕩.𝕗]
    elseif isa(𝕩, TR3O);                  [3, 𝕩.𝕘, 𝕩.h, 𝕩.𝕗]
    else                                  [-1, 𝕩]
    end
  kind
end

set_prims(none, [decompose, prim_ind])

runtime(n::Int64) = value[n + 1]

""" Generate a function body which broadcasts 𝕗 along the leading axis."""
macro along_leading_axis(𝕗, 𝕨, 𝕩)
  quote
    𝕗, 𝕨, 𝕩 = $(esc(𝕗)), $(esc(𝕨)), $(esc(𝕩))
    size𝕨, size𝕩 = size(𝕨), size(𝕩)
    ndims𝕨, ndims𝕩 = ndims(𝕨), ndims(𝕩)
    if ndims𝕨 == 0
      𝕗(𝕨[1], 𝕩)
    elseif ndims𝕩 == 0
      𝕗(𝕨, 𝕩[1])
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
    else
      for n in 0:(ndims𝕨 - 1)
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

# + bqnadd plus
bqnadd(𝕨::None, 𝕩) = 𝕩
# + bqnadd addition
bqnadd(𝕨::Char, 𝕩::Number) = 𝕨 + Int(𝕩)
bqnadd(𝕨::Number, 𝕩::Char) = Int(𝕨) + 𝕩
bqnadd(𝕨::Number, 𝕩::Number) = 𝕨 + 𝕩
bqnadd(𝕨::Union{Number,Char}, 𝕩::AbstractArray) =
  size(𝕩) == () ? collect(bqnadd(𝕨, 𝕩[1])) : bqnadd.(𝕨, 𝕩)
bqnadd(𝕨::AbstractArray, 𝕩::Union{Number,Char}) =
  size(𝕨) == () ? collect(bqnadd(𝕨[1], 𝕩)) : bqnadd.(𝕨, 𝕩)
bqnadd(𝕨::AbstractArray, 𝕩::AbstractArray) = @along_leading_axis(bqnadd, 𝕨, 𝕩)
bqnadd(𝕨::String, 𝕩) = bqnadd(collect(𝕨), 𝕩)
bqnadd(𝕨, 𝕩::String) = bqnadd(𝕨, collect(𝕩))

set_override(bqnadd)

# - bqnsub minus
bqnsub(𝕨::None, 𝕩) = -𝕩
# + bqnsub substract
bqnsub(𝕨::Char, 𝕩::Number) = 𝕨 - Int(𝕩)
bqnsub(𝕨::Char, 𝕩::Char) = 𝕨 - 𝕩
bqnsub(𝕨::Number, 𝕩::Number) = 𝕨 - 𝕩
bqnsub(𝕨::Union{Number,Char}, 𝕩::AbstractArray) =
  size(𝕩) == () ? collect(bqnsub(𝕨, 𝕩[1])) : bqnsub.(𝕨, 𝕩)
bqnsub(𝕨::AbstractArray, 𝕩::Union{Number,Char}) =
  size(𝕨) == () ? collect(bqnsub(𝕨[1], 𝕩)) : bqnsub.(𝕨, 𝕩)
bqnsub(𝕨::AbstractArray, 𝕩::AbstractArray) = @along_leading_axis(bqnsub, 𝕨, 𝕩)
bqnsub(𝕨::String, 𝕩::String) = bqnsub(collect(𝕨), collect(𝕩))
bqnsub(𝕨::String, 𝕩) = bqnsub(collect(𝕨), 𝕩)
bqnsub(𝕨, 𝕩::String) = bqnsub(𝕨, collect(𝕩))

set_override(bqnsub)

# × bqnmul sign
bqnmul(𝕨::None, 𝕩::Number) = sign(𝕩)
bqnmul(𝕨::None, 𝕩::AbstractArray) = sign.(𝕩)
bqnmul(𝕨::None, 𝕩) = sign(𝕩)
# × bqnmul mulition
bqnmul(𝕨::Number, 𝕩::Number) = 𝕨 * 𝕩
bqnmul(𝕨::Number, 𝕩::AbstractArray) =
  size(𝕩) == () ? collect(bqnmul(𝕨, 𝕩[1])) : bqnmul.(𝕨, 𝕩)
bqnmul(𝕨::AbstractArray, 𝕩::Number) =
  size(𝕨) == () ? collect(bqnmul(𝕨[1], 𝕩)) : bqnmul.(𝕨, 𝕩)
bqnmul(𝕨::AbstractArray, 𝕩::AbstractArray) = @along_leading_axis(bqnmul, 𝕨, 𝕩)

set_override(bqnmul)

# ≠ bqnneq length
bqnneq(𝕨::None, 𝕩::Vector) = length(𝕩)
bqnneq(𝕨::None, 𝕩::AbstractArray) = begin
  size𝕩 = size(𝕩)
  size𝕩 != () ? size𝕩[end] : 1
end
bqnneq(𝕨::None, 𝕩::String) = length(𝕩)
bqnneq(𝕨::None, 𝕩::Union{Number,Char}) = 1
# ≠ bqnneq not equals
bqnneq(𝕨::Union{Number,Char}, 𝕩::Union{Number,Char}) = Int(𝕨 != 𝕩)
bqnneq(𝕨::AbstractArray, 𝕩::Union{Number,Char}) = 𝕨 .!= 𝕩
bqnneq(𝕨::Union{Number,Char}, 𝕩::AbstractArray) = 𝕨 .!= 𝕩
bqnneq(𝕨::AbstractArray, 𝕩::AbstractArray) = 𝕨 .!= 𝕩
bqnneq(𝕨::Union{Number,Char}, 𝕩::String) = 𝕨 .!= collect(𝕩)
bqnneq(𝕨::String, 𝕩::Union{Number,Char}) = collect(𝕨) .!= 𝕩
bqnneq(𝕨::String, 𝕩::String) = collect(𝕨) .!= collect(𝕩)
bqnneq(𝕨::AbstractArray, 𝕩::String) = 𝕨 .!= collect(𝕩)
bqnneq(𝕨::String, 𝕩::AbstractArray) = collect(𝕨) .!= 𝕩

set_override(bqnneq)

const _runtime_length = length(value)
const _runtime_indices = IdDict(𝕗 => idx - 1
                                for (idx, 𝕗) in enumerate(value))

export runtime

end
