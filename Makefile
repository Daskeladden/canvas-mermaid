# canvas-mermaid -- mermaid diagrams drawn on an Emacs 32 canvas, by
# canvas-graph and canvas-mindmap on canvas-diagram.

EMACS   ?= emacs
DIAGRAM ?= ../canvas-diagram
KEYS    ?= ../canvas-keys
GRAPH   ?= ../canvas-graph
MINDMAP ?= ../canvas-mindmap
SEQUENCE ?= ../canvas-sequence

.PHONY: test module clean

test: module
	$(EMACS) -Q --batch -L . -L $(DIAGRAM) -L $(KEYS) -L $(GRAPH) -L $(MINDMAP) -L $(SEQUENCE) -L tests \
	  -l tests/canvas-mermaid-tests.el \
	  --eval '(ert-run-tests-batch-and-exit)'

# The cairo and pango module lives with canvas-diagram.
module:
	$(MAKE) -C $(DIAGRAM)

clean:
	rm -f *.elc tests/*.elc
