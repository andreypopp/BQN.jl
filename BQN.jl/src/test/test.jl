using Test

function run_testsuite(cases; only=nothing)
  for (idx, (expected, code)) in enumerate(cases)
    if only !== nothing && !(idx in only); continue end
    @info "$(idx) $(code)"
    if isa(expected, DataType) && expected <: Exception
      Test.@test_throws expected bqneval(code)
    else
      Test.@test expected == bqneval(code)
    end
  end
end

include("./bytecode.jl")
include("./simple.jl")
include("./prim.jl")
include("./under.jl")
include("./identity.jl")

function test_all()
  @testset verbose=true begin
    test_bytecode()
    test_simple()
    test_prim()
    test_identity()
    test_under()
  end
  nothing
end
