# canvas-mermaid

canvas-mermaid draws [mermaid](https://mermaid.js.org) diagrams on an
Emacs 32 canvas. It reads the mermaid text and gives each diagram to the
layout that fits it: a flowchart or a state diagram goes to
[canvas-graph](https://github.com/Daskeladden/canvas-graph), and a mind map
goes to [canvas-mindmap](https://github.com/Daskeladden/canvas-mindmap).
It needs no mermaid program and no browser.

This is a prototype.

## What it reads

- `flowchart` and `graph`: nodes in every shape, links in every style,
  labels by pipe (`-->|yes|`) and inline (`-- yes -->`), chains
  (`A --> B --> C`) and fans (`A & B --> C`). A subgraph, from
  `subgraph kitchen [In the kitchen]` to `end`, draws as a frame round its nodes.
  `style kitchen stroke:#c60,fill:#fde` gives the frame its color and its
  background.
- `stateDiagram` and `stateDiagram-v2`: `[*]` as the start and the end,
  `state "Long name" as S`, labelled transitions, and composite states.
- `mindmap`: the tree by indentation, node shapes, and icons of the
  material set (`::icon(mdi mdi-book)`).

The front matter title names the diagram. Comments, directives, classes,
the styles of nodes and links, clicks and notes draw nothing.

Some parts of mermaid have no equivalent on the canvas:

- A node shape becomes an ordinary box, because canvas-graph has one
  shape of box.
- The layout always runs top down, so `LR` and the other directions have
  no effect.
- A link without an arrowhead (`---`) still gets one.
- Composite states become flat. A composite state leads to the state it
  starts at.
- A subgraph in another subgraph gets a frame of its own, next to the
  frame of the outer one and not in it. A node is in the first subgraph
  that closes round it, as in mermaid.
- Other rows of the layout can come between the nodes of a subgraph.
  Then each part of the subgraph gets a frame.

A sequence diagram, a class diagram, a gantt chart or another type
without a layout gives a clear error, and canvas-mermaid draws nothing
wrong.

## Where it finds diagrams

- A buffer that holds one mermaid diagram, such as a `.mmd` file.
- A mermaid fence in a markdown buffer (```` ```mermaid ````).
- A mermaid source block in an org buffer (`#+begin_src mermaid`).

## Requirements

- Emacs 32.0.50, built from source, with canvas images and modules.
- [canvas-diagram](https://github.com/Daskeladden/canvas-diagram) and its
  compiled `canvas-cairo` module, canvas-graph, and canvas-mindmap.

## Install

Clone the repositories side by side, and build the module once:

```sh
git clone https://github.com/Daskeladden/canvas-diagram.git
git clone https://github.com/Daskeladden/canvas-graph.git
git clone https://github.com/Daskeladden/canvas-mindmap.git
git clone https://github.com/Daskeladden/canvas-mermaid.git
make -C canvas-diagram
```

Then put the four directories on the load path:

```elisp
(use-package canvas-mermaid
  :load-path ("/path/to/canvas-diagram" "/path/to/canvas-graph"
              "/path/to/canvas-mindmap" "/path/to/canvas-mermaid")
  :commands (canvas-mermaid-show canvas-mermaid-show-file
             canvas-mermaid-export canvas-mermaid-demo))
```

## Use

Put point in a mermaid diagram and run `M-x canvas-mermaid-show`. If the
buffer has only one diagram, point can be anywhere. The drawing follows
the diagram. When you stop typing, the drawing shows the new text. While
the text is incomplete, the last good drawing stays, and the echo area
tells you what is wrong.

`M-x canvas-mermaid-show-file` draws the first diagram of a file.
`M-x canvas-mermaid-demo` opens a small flowchart that you can edit and
watch.

`canvas-mermaid-export` writes the first diagram of a file to a PNG,
SVG or PDF file. It needs no frame, so it works in batch:

```sh
emacs -Q --batch -L /path/to/canvas-diagram -L /path/to/canvas-graph \
  -L /path/to/canvas-mindmap -L /path/to/canvas-mermaid -l canvas-mermaid \
  --eval '(canvas-mermaid-export "flow.mmd" "flow.png")'
```

The keys in the drawing are the keys of canvas-graph or canvas-mindmap.

## Tests

```sh
make test
```

This command builds the module and runs the ERT suite in batch. The
suite expects canvas-diagram, canvas-graph and canvas-mindmap beside this
directory. If they are somewhere else, set `DIAGRAM`, `GRAPH` and
`MINDMAP` on the command line.

## License

GPL-3.0-or-later. See [LICENSE](LICENSE).
