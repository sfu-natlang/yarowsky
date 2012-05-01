PROGS=evaluate comparelabellings abneymetrics applydl dl traindl yarowsky dlcotrain csem hsgraph yarowskyprop
OCAMLC=ocamlopt -I +str -g
LIB=str.cmxa

all: $(PROGS)

%.cmx: %.ml
	$(OCAMLC) -c $<

$(PROGS): utils.cmx data.cmx
	$(OCAMLC) -o $@ $(LIB) $^

utils.cmx: %.cmx: %.ml

data.cmx: %.cmx: utils.cmx %.ml

decision_list.cmx: %.cmx: utils.cmx %.ml

stats.cmx: %.cmx: utils.cmx %.ml

subramanya.cmx: %.cmx: utils.cmx %.ml

abney.cmx: %.cmx: utils.cmx %.ml

evaluate: %: %.cmx

comparelabellings: %: %.cmx

abneymetrics: %: decision_list.cmx stats.cmx %.cmx

dllogprobs: %: decision_list.cmx abney.cmx %.cmx

applydl: %: decision_list.cmx %.cmx

dl: %: decision_list.cmx %.cmx

traindl: %: decision_list.cmx abney.cmx %.cmx

trimdl: %: decision_list.cmx %.cmx

yarowsky: %: decision_list.cmx abney.cmx %.cmx

dlcotrain: %: decision_list.cmx abney.cmx %.cmx

csem: %: decision_list.cmx stats.cmx %.cmx

hsgraph: %: decision_list.cmx stats.cmx %.cmx

yarowskyprop: %: decision_list.cmx abney.cmx stats.cmx subramanya.cmx %.cmx

clean:
	rm -rf $(PROGS) *.cmi *.cmx *.cmo *.o
