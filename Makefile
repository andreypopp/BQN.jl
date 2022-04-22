init:
	git submodule init
	git submodule update
	cd CBQN && make

test:
	julia -e 'using BQN; BQN.Tests.test_all()'

test0:
	julia -e 'using BQN; BQN.Tests0.test_all()'

gen0: BQN.jl/src/c.jl BQN.jl/src/r0.jl BQN.jl/src/r1.jl BQN.jl/src/f.jl

BQN.jl/src/%.jl: cjl.bqn
	rm -f $@
	./cjl.bqn $(@:BQN.jl/src/%.jl=%) | sed -e 's/^quote/value = begin/g' > $@

