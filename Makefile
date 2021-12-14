T = $(wildcard testcases/*.tig)

heap: sources.cm
	ml-build sources.cm Main.main tigerc

test: heap $(T)

$(T):
	./tigerc $@ >$(@:.tig=.out) 2>&1

.PHONY: heap test $(T)
