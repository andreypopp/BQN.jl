module Runtime0

module R0
import ....provide, ....str
include("./r0.jl")
end

import ..run

const value = run("<none>", R0.value...)

runtime_0(n::Int64) = Runtime0.value[n + 1]

export runtime_0

end
