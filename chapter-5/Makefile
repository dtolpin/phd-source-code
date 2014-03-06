.SUFFIXES: .lhs .o

SRC=BP.lhs Bridge.lhs Def.lhs Dibbuk.lhs FD.lhs SC.lhs PAC.lhs Test.lhs XO.lhs Perm.lhs
HFLAGS=-O2 

all: csp

csp: $(SRC) csp.lhs
	ghc $(HFLAGS) -o $@ --make csp.lhs

prof: $(SRC) csp.lhs
	ghc $(HFLAGS) -prof -auto-all -o csp csp.lhs

shallow:
	ghc $(HFLAGS) --make -fforce-recomp -DDEEP=0 -c FD.lhs

deep:
	ghc $(HFLAGS) --make -fforce-recomp -DDEEP=1 -c FD.lhs

debug:
	ghc $(HFLAGS) --make -fforce-recomp -DDEBUG=1 -c Dibbuk.lhs

nodebug:
	ghc $(HFLAGS) --make -fforce-recomp -DDEBUG=0 -c Dibbuk.lhs

spantree:
	ghc $(HFLAGS) --make -fforce-recomp -DSPANTREE=1 -c BP.lhs

eta:
	ghc $(HFLAGS) --make -fforce-recomp -DSPANTREE=0 -c BP.lhs

randomvoi:
	ghc $(HFLAGS) --make -fforce-recomp -DRANDOMSC=1 -c BP.lhs

voi:
	ghc $(HFLAGS) --make -fforce-recomp -DRANDOMSC=0 -c BP.lhs

clean:
	rm -f *.o *.hi csp

