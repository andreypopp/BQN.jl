using Test

function run_testsuite(cases; only=nothing)
  for (idx, (expected, code)) in enumerate(cases)
    if only !== nothing && !(idx in only); continue end
    @info "=== TEST@$(idx) $(code)"
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

function test_all()
  @testset verbose=true begin
    # pointless after we've tried to load the runtime but let's do it anyway
    @testset "bytecode" begin test_bytecode() end
    @testset "simple" begin test_simple() end
    @testset "prim, layer 0" begin test_prim_0() end
    @testset "prim, layer 1" begin test_prim_1() end
    @testset "prim, layer 2" begin test_prim_2() end
    @testset "prim, layer 3" begin test_prim_3() end
    @testset "prim, layer 4" begin test_prim_4() end
    @testset "prim, layer 5" begin test_prim_5() end
    @testset "prim, layer 6" begin test_prim_6() end
  end
  nothing
end
