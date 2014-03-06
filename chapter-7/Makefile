PACKAGES=-package batteries,base -linkpkg

OCC=ocamlfind ocamlc
OCFLAGS=-g $(PACKAGES)

XCC=ocamlfind ocamlopt
XCFLAGS=-noassert -unsafe $(PACKAGES)

OLFLAGS=$(OCFLAGS) 
XLFLAGS=$(XCFLAGS)

OFILES= \
	perm.cmo \
	stat.cmo \
	options.cmo \
	search.cmo \
	lookahead.cmo \
	astar.cmo \
	idastar.cmo \
	lida.cmo \
	lazyA.cmo \
	pzldef.cmo \
	pzlPDBFile.cmo \
	pzlHeurPDB.cmo pzlHeurManhattan.cmo pzlHeurConflict.cmo  \
	pzlProblem.cmo \
	pzlHeur.cmo \
	wpzHeurManhattan.cmo \
	wpzHeurConflict.cmo \
	wpzProblem.cmo \
	wpzHeur.cmo \
	mpzHeurManhattan.cmo \
	mpzHeurConflict.cmo \
	mpzProblem.cmo \
	mpzHeur.cmo \


XFILES= \
	perm.cmx \
	stat.cmx \
	options.cmx \
	lookahead.cmx \
	search.cmx \
	astar.cmx \
	idastar.cmx \
	lida.cmx \
	lazyA.cmx \
	pzldef.cmx \
	pzlPDBFile.cmx \
	pzlHeurPDB.cmx pzlHeurManhattan.cmx pzlHeurConflict.cmx \
	pzlProblem.cmx \
	pzlHeur.cmx \
	wpzHeurManhattan.cmx \
	wpzHeurConflict.cmx \
	wpzProblem.cmx \
	wpzHeur.cmx \
	mpzHeurManhattan.cmx \
	mpzHeurConflict.cmx \
	mpzProblem.cmx \
	mpzHeur.cmx \

.SUFFIXES: .mll .mly .ml .mli .cmi .cmo .cmx .1 .man .ps

.mll.ml:
	ocamllex $<

.mly.ml:
	ocamlyacc $<

.mly.mli:
	ocamlyacc $<

.mli.cmi:
	$(OCC) $(OCFLAGS) -c $<

.ml.cmo:
	$(OCC) $(OCFLAGS) -c $<

.ml.cmx:
	$(XCC) $(XCFLAGS) -c $<

.1.man:
	groff -man -Tascii $< > $@

.1.ps:
	groff -man -Tps $< > $@

all: genpdb genpdbx compdb compdbx cmpheu cmpheux \
	solvepzl solvepzlx \
	solvewpz solvewpzx \
	solvempz solvempzx 

genpdb: $(OFILES) pzlGenPDB.cmo
	$(OCC) $(OLFLAGS) -o $@ $^

genpdbx: $(XFILES) pzlGenPDB.cmx
	$(XCC) $(XLFLAGS) -o $@ $^

compdb: $(OFILES) pzlComPDB.cmo
	$(OCC) $(OLFLAGS) -o $@ $^

compdbx: $(XFILES) pzlComPDB.cmx
	$(XCC) $(XLFLAGS) -o $@ $^

cmpheu: $(OFILES) pzlCmpHeur.cmo
	$(OCC) $(OLFLAGS) -o $@ $^

cmpheux: $(XFILES) pzlCmpHeur.cmx
	$(XCC) $(XLFLAGS) -o $@ $^

solvepzl: $(OFILES) wpzlSolver.cmo
	$(OCC) $(OLFLAGS) -o $@ $^

solvepzlx: $(XFILES) wpzlSolver.cmx
	$(XCC) $(XLFLAGS) -o $@ $^

solvewpz: solvepzl
	ln -f $< $@

solvewpzx: solvepzlx
	ln -f $< $@

solvempz: solvepzl
	ln -f $< $@

solvempzx: solvepzlx
	ln -f $< $@

pdb555: 1-2-3-4-5.pdb 6-7-8-9-10.pdb 11-12-13-14-15.pdb
1-2-3-4-5.pdb:
	./genpdbx 1 2 3 4 5      | ./compdbx > 1-2-3-4-5.pdb
6-7-8-9-10.pdb:
	./genpdbx 6 7 8 9 10     | ./compdbx > 6-7-8-9-10.pdb
11-12-13-14-15.pdb:
	./genpdbx 11 12 13 14 15 | ./compdbx > 11-12-13-14-15.pdb

pdb78: 1-4-5-8-9-12-13.pdb 2-3-6-7-10-11-14-15.pdb
1-4-5-8-9-12-13.pdb:
	./genpdbx 1 4 5 8 9 12 13 | ./compdbx > 1-4-5-8-9-12-13.pdb
2-3-6-7-10-11-14-15.pdb:
	./genpdbx 2 3 6 7 10 11 14 15 | ./compdbx > 2-3-6-7-10-11-14-15.pdb

depend:
	-ed Makefile < remdep.ed
	ocamldep *.ml *.mli >> Makefile

clean: 
	rm -f *.cm? *.o gmon.out

distclean: clean
	rm -f ./#*# ./*~ ./.*.sw?

# dependencies
astar.cmo: stat.cmi search.cmo options.cmi astar.cmi
astar.cmx: stat.cmx search.cmx options.cmx astar.cmi
idastar.cmo: stat.cmi search.cmo options.cmi idastar.cmi
idastar.cmx: stat.cmx search.cmx options.cmx idastar.cmi
lida.cmo: stat.cmi search.cmo options.cmi lida.cmi
lida.cmx: stat.cmx search.cmx options.cmx lida.cmi
lazyA.cmo: stat.cmi search.cmo options.cmi lazyA.cmi
lazyA.cmx: stat.cmx search.cmx options.cmx lazyA.cmi
lookahead.cmo: search.cmo lookahead.cmi
lookahead.cmx: search.cmx lookahead.cmi
options.cmo: options.cmi
options.cmx: options.cmi
perm.cmo: perm.cmi
perm.cmx: perm.cmi
predict.cmo: predict.cmi
predict.cmx: predict.cmi
pzlCmpHeur.cmo: pzldef.cmi pzlHeur.cmi perm.cmi
pzlCmpHeur.cmx: pzldef.cmx pzlHeur.cmx perm.cmx
pzlComPDB.cmo: pzlPDBFile.cmi pzlComPDB.cmi
pzlComPDB.cmx: pzlPDBFile.cmx pzlComPDB.cmi
pzlGenPDB.cmo: pzldef.cmi pzlPDBFile.cmi pzlGenPDB.cmi
pzlGenPDB.cmx: pzldef.cmx pzlPDBFile.cmx pzlGenPDB.cmi
pzlHeur.cmo: pzlProblem.cmo pzlHeurPDB.cmi pzlHeurManhattan.cmi \
    pzlHeurConflict.cmi lookahead.cmi pzlHeur.cmi
pzlHeur.cmx: pzlProblem.cmx pzlHeurPDB.cmx pzlHeurManhattan.cmx \
    pzlHeurConflict.cmx lookahead.cmx pzlHeur.cmi
pzlHeurConflict.cmo: pzldef.cmi pzlHeurManhattan.cmi pzlHeurConflict.cmi
pzlHeurConflict.cmx: pzldef.cmx pzlHeurManhattan.cmx pzlHeurConflict.cmi
pzlHeurManhattan.cmo: pzldef.cmi pzlHeurManhattan.cmi
pzlHeurManhattan.cmx: pzldef.cmx pzlHeurManhattan.cmi
pzlHeurPDB.cmo: pzldef.cmi pzlPDBFile.cmi pzlHeurPDB.cmi
pzlHeurPDB.cmx: pzldef.cmx pzlPDBFile.cmx pzlHeurPDB.cmi
pzlPDBFile.cmo: pzlPDBFile.cmi
pzlPDBFile.cmx: pzlPDBFile.cmi
pzlProblem.cmo: pzldef.cmi
pzlProblem.cmx: pzldef.cmx
pzldef.cmo: perm.cmi pzldef.cmi
pzldef.cmx: perm.cmx pzldef.cmi
search.cmo:
search.cmx:
stat.cmo: stat.cmi
stat.cmx: stat.cmi
wpzHeur.cmo: wpzProblem.cmo wpzHeurManhattan.cmi wpzHeurConflict.cmi \
    lookahead.cmi wpzHeur.cmi
wpzHeur.cmx: wpzProblem.cmx wpzHeurManhattan.cmx wpzHeurConflict.cmx \
    lookahead.cmx wpzHeur.cmi
wpzHeurConflict.cmo: wpzHeurManhattan.cmi pzldef.cmi wpzHeurConflict.cmi
wpzHeurConflict.cmx: wpzHeurManhattan.cmx pzldef.cmx wpzHeurConflict.cmi
wpzHeurManhattan.cmo: pzldef.cmi wpzHeurManhattan.cmi
wpzHeurManhattan.cmx: pzldef.cmx wpzHeurManhattan.cmi
wpzProblem.cmo: pzldef.cmi
wpzProblem.cmx: pzldef.cmx
mpzHeur.cmo: mpzProblem.cmo mpzHeurManhattan.cmi mpzHeurConflict.cmi \
    lookahead.cmi mpzHeur.cmi
mpzHeur.cmx: mpzProblem.cmx mpzHeurManhattan.cmx mpzHeurConflict.cmx \
    lookahead.cmx mpzHeur.cmi
mpzHeurConflict.cmo: mpzHeurManhattan.cmi pzldef.cmi mpzHeurConflict.cmi
mpzHeurConflict.cmx: mpzHeurManhattan.cmx pzldef.cmx mpzHeurConflict.cmi
mpzHeurManhattan.cmo: pzldef.cmi mpzHeurManhattan.cmi
mpzHeurManhattan.cmx: pzldef.cmx mpzHeurManhattan.cmi
mpzProblem.cmo: pzldef.cmi
mpzProblem.cmx: pzldef.cmx
wpzlSolver.cmo: wpzProblem.cmo wpzHeur.cmi mpzProblem.cmo mpzHeur.cmi \
    stat.cmi search.cmo pzldef.cmi \
    pzlProblem.cmo pzlHeur.cmi options.cmi lida.cmi lazyA.cmi idastar.cmi astar.cmi \
    wpzlSolver.cmi
wpzlSolver.cmx: wpzProblem.cmx wpzHeur.cmx mpzProblem.cmx mpzHeur.cmx \
    stat.cmx search.cmx pzldef.cmx \
    pzlProblem.cmx pzlHeur.cmx options.cmx lida.cmx lazyA.cmx idastar.cmx astar.cmx \
	wpzlSolver.cmi
astar.cmi: search.cmo
idastar.cmi: search.cmo
lazyA.cmi: search.cmo
lida.cmi: search.cmo
lookahead.cmi: search.cmo
options.cmi:
perm.cmi:
predict.cmi:
pzlComPDB.cmi:
pzlGenPDB.cmi:
pzlHeur.cmi: pzldef.cmi
pzlHeurConflict.cmi: pzldef.cmi
pzlHeurManhattan.cmi: pzldef.cmi
pzlHeurPDB.cmi: pzldef.cmi
pzlPDBFile.cmi:
pzldef.cmi:
stat.cmi:
wpzHeur.cmi: pzldef.cmi
wpzHeurConflict.cmi: pzldef.cmi
wpzHeurManhattan.cmi: pzldef.cmi
mpzHeur.cmi: pzldef.cmi
mpzHeurConflict.cmi: pzldef.cmi
mpzHeurManhattan.cmi: pzldef.cmi
wpzlSolver.cmi:
