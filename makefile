build:
	mkdir -p obj
	gprbuild
check:
	gnatprove -P bitcoin.gpr --mode=check
flow:
	gnatprove -P bitcoin.gpr --mode=flow
prove:
	gnatprove -P bitcoin.gpr --mode=prove
clean:
	rm -rf *.o *.ali obj/
