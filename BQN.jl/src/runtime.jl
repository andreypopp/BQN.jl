module Runtime

module R1
import ....provide, ....runtime_0, ....str
include("./r1.jl")
end

import ..run
import ..none, ..F, ..FN, ..TR2D, ..TR3D, ..TR3O, ..Runtime0

const value, set_prims, set_inv = run("<none>", R1.value...)

const _runtime_length = length(value)
const _runtime_indices = IdDict(𝕗 => idx - 1
                                for (idx, 𝕗) in enumerate(value))

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

export runtime

end
