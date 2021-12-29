gen0: BQN.jl/src/c.jl BQN.jl/src/r.jl

BQN.jl/src/%.jl:
	rm -f $@
	./cjl.bqn $(@:BQN.jl/src/%.jl=%) | sed -e 's/^quote/value = begin/g' > $@

