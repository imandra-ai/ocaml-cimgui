
all:
	@dune build @all
clean:
	@dune clean
watch:
	@dune build @all -w

.PHONY: all clean watch
