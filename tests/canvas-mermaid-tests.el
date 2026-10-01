;;; canvas-mermaid-tests.el --- tests -*- lexical-binding: t -*-
(require 'ert)
(require 'cl-lib)
(require 'lisp-mnt)
(require 'package)
(require 'canvas-mermaid)

;; The drawing is canvas-graph's and canvas-mindmap's, tested there.
;; These tests cover the reading: mermaid text into their specs, the
;; diagrams found in a buffer, and following one as it is edited.

;;;; Helpers

(defun canvas-mermaid-test--read (text)
  "TEXT read as mermaid from position 1: (KIND . SPEC)."
  (canvas-mermaid-read text))

(defun canvas-mermaid-test--edges (spec)
  "SPEC's edges without their places: (FROM TO LABEL) each."
  (mapcar (lambda (e) (list (nth 0 e) (nth 1 e) (nth 2 e))) (plist-get spec :edges)))

(defun canvas-mermaid-test--labels (spec)
  "SPEC's nodes as (NAME . LABEL), a node without a label its own name."
  (mapcar (lambda (n) (cons (car n) (or (plist-get (cdr n) :label) (car n)))) (plist-get spec :nodes)))

(defun canvas-mermaid-test--unplaced (tree)
  "TREE, a mind map's nested spec, without its :pos options."
  (let ((out (list (car tree))) (rest (cdr tree)))
    (while rest
      (cond ((eq (car rest) :pos) (setq rest (cddr rest)))
            ((keywordp (car rest)) (setq out (append out (list (car rest) (cadr rest))) rest (cddr rest)))
            (t (setq out (append out (list (canvas-mermaid-test--unplaced (car rest)))) rest (cdr rest)))))
    out))

(defun canvas-mermaid-test--png-p (file)
  "Whether FILE starts with the PNG signature."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally file nil 0 8)
    (equal (buffer-string) "\x89PNG\r\n\x1a\n")))

(defconst canvas-mermaid-test--markdown
  "# Notes\n\nA flow:\n\n```mermaid\nflowchart TD\n  A --> B\n```\n\nA map:\n\n```mermaid\nmindmap\n  Root\n    Leaf\n```\n"
  "A markdown text with a flowchart and a mind map.")

;;;; Flowcharts

(ert-deftest canvas-mermaid-flowchart-nodes-show-the-text-of-their-shapes ()
  ;; GIVEN a flowchart with a node of every shape, one quoted with brackets
  ;;       inside, and links with labels by pipe and by inline text
  ;; WHEN it is read
  ;; THEN it is a graph, each node shows the text in its shape, the links
  ;;      join the nodes by name under their labels, AND the spec builds
  (pcase-let ((`(,kind . ,spec) (canvas-mermaid-test--read "flowchart TD
    A[Start] --> B(Round)
    B --> C{Is it?}
    C -->|Yes| D([Stadium])
    C -- No --> E[[Sub]]
    E --> F[(Db)]
    F --> G((Circle))
    G --> H>Flag]
    H --> I{{Hex}}
    I --> J[/Para/]
    J --> K[\\Alt\\]
    K --> L[/Trap\\]
    L --> M(((Double)))
    M --> N[\"quoted [text]\"]
")))
    (should (eq kind 'graph))
    (should (equal (plist-get spec :name) "flowchart"))
    (should (equal (canvas-mermaid-test--labels spec)
                   '(("A" . "Start") ("B" . "Round") ("C" . "Is it?") ("D" . "Stadium") ("E" . "Sub")
                     ("F" . "Db") ("G" . "Circle") ("H" . "Flag") ("I" . "Hex") ("J" . "Para")
                     ("K" . "Alt") ("L" . "Trap") ("M" . "Double") ("N" . "quoted [text]"))))
    (should (equal (seq-take (canvas-mermaid-test--edges spec) 4)
                   '(("A" "B" nil) ("B" "C" nil) ("C" "D" "Yes") ("C" "E" "No"))))
    (should (= (length (plist-get spec :edges)) 13))
    (should (canvas-graph-build spec))))

(ert-deftest canvas-mermaid-flowchart-links-chain-fan-out-and-go-both-ways ()
  ;; GIVEN links chained, fanned with &, both ways, dotted, thick, open,
  ;;       labelled inline in every style, invisible, and with circle,
  ;;       cross and long heads
  ;; WHEN the flowchart is read
  ;; THEN every link is an edge in order, a two-headed one an edge each way,
  ;;      the invisible one none, AND its nodes are nodes all the same
  (let ((spec (cdr (canvas-mermaid-test--read "graph LR
  A & B --> C --> D
  C <--> E
  D -.-> F
  F ==> G
  G --- H
  H -. maybe .-> I
  I == sure ==> J
  J ~~~ K
  K --o L
  L --x M
  M ---->|\"long\"| N
"))))
    (should (equal (canvas-mermaid-test--edges spec)
                   '(("A" "C" nil) ("B" "C" nil) ("C" "D" nil) ("C" "E" nil) ("E" "C" nil)
                     ("D" "F" nil) ("F" "G" nil) ("G" "H" nil) ("H" "I" "maybe") ("I" "J" "sure")
                     ("K" "L" nil) ("L" "M" nil) ("M" "N" "long"))))
    (should (equal (mapcar #'car (plist-get spec :nodes))
                   '("A" "B" "C" "D" "E" "F" "G" "H" "I" "J" "K" "L" "M" "N")))))

(ert-deftest canvas-mermaid-flowchart-leaves-out-what-draws-no-node ()
  ;; GIVEN a flowchart with front matter naming it, a directive, comments,
  ;;       a subgraph with its direction and end, classes, styles, a click,
  ;;       two statements on one line, a node of the new shape syntax, and
  ;;       a header with a statement after it
  ;; WHEN each is read
  ;; THEN the title names the graph, the rest draws only the nodes and
  ;;      links written, the new shape's label shown, AND the statement after
  ;;      the header is read too
  (let ((spec (cdr (canvas-mermaid-test--read "---
title: My flow
---
%%{init: {\"theme\": \"dark\"}}%%
flowchart TB
  %% a comment
  subgraph one [First]
    direction LR
    a1:::hot --> a2
  end
  a2 --> b1; b1 --> b2
  classDef hot fill:#f96
  class a1 hot
  style a2 fill:#bbf
  linkStyle 0 stroke:#ff3
  click a1 callback
  n@{ shape: rect, label: \"New shape\" }
  b2 --> n
"))))
    (should (equal (plist-get spec :name) "My flow"))
    (should (equal (canvas-mermaid-test--labels spec)
                   '(("a1" . "a1") ("a2" . "a2") ("b1" . "b1") ("b2" . "b2") ("n" . "New shape"))))
    (should (equal (canvas-mermaid-test--edges spec)
                   '(("a1" "a2" nil) ("a2" "b1" nil) ("b1" "b2" nil) ("b2" "n" nil)))))
  (should (equal (canvas-mermaid-test--edges (cdr (canvas-mermaid-test--read "graph TD; X-->Y\n")))
                 '(("X" "Y" nil)))))

(ert-deftest canvas-mermaid-nodes-are-placed-where-they-are-first-met ()
  ;; GIVEN a flowchart read as if it began at buffer position 100
  ;; WHEN its nodes' places are asked for
  ;; THEN each is where the node's name is first written in the buffer
  (let* ((text "graph TD\n  A --> B\n  B --> C\n")
         (spec (cdr (canvas-mermaid-read text 100)))
         (place (lambda (name) (plist-get (cdr (assoc name (plist-get spec :nodes))) :pos))))
    (should (= (funcall place "A") (+ 100 (string-search "A -->" text))))
    (should (= (funcall place "B") (+ 100 (string-search "B\n" text))))
    (should (= (funcall place "C") (+ 100 (string-search "C\n" text))))))

(ert-deftest canvas-mermaid-a-flowchart-it-cannot-read-is-an-error ()
  ;; GIVEN a link with nothing after it, and something that is no link
  ;; WHEN each is read
  ;; THEN each is an error saying what could not be read
  (should-error (canvas-mermaid-test--read "graph TD\n  A -->\n"))
  (let ((err (should-error (canvas-mermaid-test--read "graph TD\n  A ?? B\n"))))
    (should (string-search "A ?? B" (error-message-string err)))))

(ert-deftest canvas-mermaid-a-flowchart-labels-its-arrows ()
  ;; GIVEN the demo's flowchart, whose ways out of a question are labelled,
  ;;       and the labels on auto
  ;; WHEN the labels shown are asked for
  ;; THEN every arrow carries its label, yes and no beside their ways
  (let ((graph (canvas-graph-build (cdr (canvas-mermaid-read canvas-mermaid--demo))))
        (canvas-graph-show-labels 'auto))
    (should (eq (canvas-graph--labels graph) 'all))))

;;;; State diagrams

(ert-deftest canvas-mermaid-state-diagram-starts-ends-and-labels ()
  ;; GIVEN a state diagram with a start, ends, labelled transitions, a
  ;;       state with a long name, a description, notes on one line and
  ;;       on several, and a fork
  ;; WHEN it is read
  ;; THEN it is a graph starting at the first state entered, the ends one
  ;;      node labelled end, the long name shown, the notes and the
  ;;      description drawing nothing, AND the transitions in order
  (pcase-let ((`(,kind . ,spec) (canvas-mermaid-test--read "stateDiagram-v2
    [*] --> Still
    Still --> [*]
    Still --> Moving : push
    Moving --> Still
    Moving --> Crash: hit
    Crash --> [*]
    state \"Waiting a while\" as Wait
    Still --> Wait
    Moving : is moving
    note right of Still : a note
    note left of Crash
        multi
        line
    end note
    state fork_state <<fork>>
    Wait --> fork_state
")))
    (should (eq kind 'graph))
    (should (equal (plist-get spec :name) "state diagram"))
    (should (equal (plist-get spec :start) "Still"))
    (should (equal (canvas-mermaid-test--labels spec)
                   '(("Still" . "Still") ("[*]" . "end") ("Moving" . "Moving") ("Crash" . "Crash")
                     ("Wait" . "Waiting a while") ("fork_state" . "fork_state"))))
    (should (equal (canvas-mermaid-test--edges spec)
                   '(("Still" "[*]" nil) ("Still" "Moving" "push") ("Moving" "Still" nil) ("Moving" "Crash" "hit")
                     ("Crash" "[*]" nil) ("Still" "Wait" nil) ("Wait" "fork_state" nil))))))

(ert-deftest canvas-mermaid-composite-states-are-flattened ()
  ;; GIVEN composite states, one entered at an inner state and ending
  ;;       inside, one with concurrent regions
  ;; WHEN the diagram is read
  ;; THEN the inner states are states like the rest, a composite leads to
  ;;      the state it starts at, the inner end draws nothing, AND the
  ;;      region separator neither
  (let ((spec (cdr (canvas-mermaid-test--read "stateDiagram
  [*] --> First
  state First {
    [*] --> Inner
    Inner --> Second
    Inner --> [*]
  }
  state Second {
    [*] --> Deep
    --
    [*] --> Other
  }
"))))
    (should (equal (plist-get spec :start) "First"))
    (should (equal (canvas-mermaid-test--edges spec)
                   '(("First" "Inner" nil) ("Inner" "Second" nil) ("Second" "Deep" nil) ("Second" "Other" nil))))))

;;;; Mind maps

(ert-deftest canvas-mermaid-mindmap-nests-by-indent ()
  ;; GIVEN a mind map whose root is in a circle, with bare nodes, an icon
  ;;       of the material set and one of another, a line break, and nodes
  ;;       in shapes after names
  ;; WHEN it is read
  ;; THEN it is a mind map nested by indent, each node showing its text,
  ;;      the material icon kept and the other dropped, AND each node placed
  ;;      where its text begins
  (pcase-let* ((text "mindmap
  root((Mind map))
    Origins
      Long history
      ::icon(mdi mdi-book)
      Popularisation
        British author<br/>Tony Buzan
        ::icon(fa fa-user)
    Research
      id)On effectiveness(
      id2[Uses]
")
               (`(,kind . ,tree) (canvas-mermaid-test--read text)))
    (should (eq kind 'mindmap))
    (should (equal (canvas-mermaid-test--unplaced tree)
                   '("Mind map"
                     ("Origins" ("Long history" :icon "material:book")
                      ("Popularisation" ("British author Tony Buzan")))
                     ("Research" ("On effectiveness") ("Uses")))))
    (should (= (plist-get (cdr tree) :pos) (1+ (string-search "root((" text))))))

(ert-deftest canvas-mermaid-a-mindmap-has-one-root ()
  ;; GIVEN a mind map whose second node is as far left as its first
  ;; WHEN it is read
  ;; THEN that is an error, not a map missing a tree
  (should-error (canvas-mermaid-test--read "mindmap\n  One\n  Two\n")))

;;;; Sequence diagrams

(ert-deftest canvas-mermaid-a-sequence-diagram-is-read-as-parties-and-messages ()
  ;; GIVEN a sequence diagram with a participant, an actor, a participant
  ;;       named with `as', and messages of the arrow forms mermaid writes
  ;; WHEN it is read
  ;; THEN it is a sequence spec: the parties in the order they were named,
  ;;      each with its label, and the messages in order, a reply dashed
  (pcase-let ((`(,kind . ,spec)
               (canvas-mermaid-test--read
                (concat "sequenceDiagram\n"
                        "    participant host as Host\n"
                        "    actor toe\n"
                        "    host->>toe: open socket\n"
                        "    toe-->>host: ready\n"
                        "    toe->toe: start timer\n"))))
    (should (eq kind 'sequence))
    (should (equal (mapcar (lambda (line) (seq-take line 3))
                           (cl-remove-if-not (lambda (line) (eq (car line) 'party)) spec))
                   '((party "host" "Host") (party "toe" "toe"))))
    (should (equal (mapcar (lambda (line) (list (nth 1 line) (nth 2 line) (nth 3 line) (plist-get (nthcdr 4 line) :dashed)))
                           (cl-remove-if-not (lambda (line) (eq (car line) 'message)) spec))
                   '(("host" "toe" "open socket" nil)
                     ("toe" "host" "ready" t)
                     ("toe" "toe" "start timer" nil))))))

(ert-deftest canvas-mermaid-a-party-a-message-names-first-is-named-there ()
  ;; GIVEN a sequence diagram that names no participant of its own
  ;; WHEN it is read
  ;; THEN each party is named where a message first uses it, in that order,
  ;;      AND a statement that draws nothing yet is passed by
  (pcase-let ((`(,_ . ,spec)
               (canvas-mermaid-test--read
                (concat "sequenceDiagram\n"
                        "    autonumber\n"
                        "    B->>A: first\n"
                        "    Note over A,B: a note\n"
                        "    loop every minute\n"
                        "        A-)C: tick\n"
                        "    end\n"))))
    (should (equal (mapcar (lambda (line) (seq-take line 3))
                           (cl-remove-if-not (lambda (line) (eq (car line) 'party)) spec))
                   '((party "B" "B") (party "A" "A") (party "C" "C"))))
    (should (equal (mapcar (lambda (line) (nth 3 line))
                           (cl-remove-if-not (lambda (line) (eq (car line) 'message)) spec))
                   '("first" "tick")))))

(ert-deftest canvas-mermaid-a-message-of-a-sequence-knows-where-it-was-read ()
  ;; GIVEN a sequence diagram read from a place in a buffer
  ;; WHEN its lines are read
  ;; THEN each party and each message holds the position it was read from,
  ;;      so that the picture can go back to it
  (pcase-let ((`(,_ . ,spec) (canvas-mermaid-read "sequenceDiagram\n    A->>B: hi\n" 100)))
    (should (cl-every (lambda (line) (integerp (plist-get (nthcdr (if (eq (car line) 'party) 3 4) line) :pos)))
                      spec))
    (should (>= (plist-get (nthcdr 4 (car (last spec))) :pos) 100))))

;;;; Class diagrams

(ert-deftest canvas-mermaid-a-class-diagram-is-read-as-boxes-of-members ()
  ;; GIVEN a class diagram with a block of members, a member written on a
  ;;       line of its own, an annotation and a relation
  ;; WHEN it is read
  ;; THEN it is a graph whose boxes hold their members as rows, in the
  ;;      order they were written, and the annotation is passed by
  (pcase-let ((`(,kind . ,spec)
               (canvas-mermaid-test--read
                (concat "classDiagram\n"
                        "    class Animal {\n"
                        "        +int age\n"
                        "        +isMammal() bool\n"
                        "    }\n"
                        "    <<interface>> Animal\n"
                        "    Animal : +String name\n"
                        "    Animal <|-- Duck\n"))))
    (should (eq kind 'graph))
    (should (equal (plist-get (cdr (assoc "Animal" (plist-get spec :nodes))) :rows)
                   '("+int age" "+isMammal() bool" "+String name")))
    (should (member "Duck" (mapcar #'car (plist-get spec :nodes))))))

(ert-deftest canvas-mermaid-a-relation-of-a-class-diagram-points-the-right-way ()
  ;; GIVEN relations whose head is on the left and on the right, one of
  ;;       them labelled and one with cardinalities
  ;; WHEN they are read
  ;; THEN each edge runs towards the head, the label is kept, and the
  ;;      cardinalities are passed by
  (pcase-let ((`(,_ . ,spec)
               (canvas-mermaid-test--read
                (concat "classDiagram\n"
                        "    Animal <|-- Duck\n"
                        "    Duck --> Pond : swims in\n"
                        "    Owner \"1\" o-- \"many\" Duck\n"))))
    (should (equal (canvas-mermaid-test--edges spec)
                   '(("Duck" "Animal" nil) ("Duck" "Pond" "swims in") ("Owner" "Duck" nil))))))

;;;; What has no layout yet

(ert-deftest canvas-mermaid-a-diagram-without-a-layout-says-so ()
  ;; GIVEN an entity relationship diagram, a word mermaid does not know,
  ;;       and texts with no diagram at all
  ;; WHEN each is read
  ;; THEN the entity relationship diagram is a user error naming it, the
  ;;      others are
  ;;      errors, AND no picture is drawn wrong
  (let ((err (should-error (canvas-mermaid-test--read "erDiagram\n  A ||--o{ B : has\n") :type 'user-error)))
    (should (string-search "erDiagram" (error-message-string err))))
  (should-error (canvas-mermaid-test--read "nonsense\n  A --> B\n"))
  (should-error (canvas-mermaid-test--read ""))
  (should-error (canvas-mermaid-test--read "%% only a comment\n")))

;;;; Diagrams in a buffer

(ert-deftest canvas-mermaid-finds-diagrams-in-markdown-org-and-its-own-files ()
  ;; GIVEN a markdown buffer with two mermaid fences, an org buffer with a
  ;;       mermaid block, a buffer that is one diagram behind front matter,
  ;;       and a markdown buffer with none
  ;; WHEN their diagrams are asked for
  ;; THEN the fences and the block are found, the whole buffer is one, AND
  ;;      the last has none
  (with-temp-buffer
    (insert canvas-mermaid-test--markdown)
    (should (equal (mapcar (lambda (r) (buffer-substring-no-properties (car r) (cdr r))) (canvas-mermaid--regions))
                   '("flowchart TD\n  A --> B\n" "mindmap\n  Root\n    Leaf\n"))))
  (with-temp-buffer
    (insert "* Heading\n#+begin_src mermaid\ngraph TD\n  X --> Y\n#+end_src\n")
    (should (= (length (canvas-mermaid--regions)) 1)))
  (with-temp-buffer
    (insert "---\ntitle: T\n---\nflowchart LR\n  A --> B\n")
    (should (equal (canvas-mermaid--regions) (list (cons (point-min) (point-max))))))
  (with-temp-buffer
    (insert "---\ntitle: Post\n---\n# Heading\n\nText.\n")
    (should-not (canvas-mermaid--regions))))

(ert-deftest canvas-mermaid-draws-the-diagram-point-is-in ()
  ;; GIVEN a markdown buffer with a flowchart and a mind map, the layouts
  ;;       only noting what they are asked to follow
  ;; WHEN the diagram is drawn with point in each, then outside both, and
  ;;      in a buffer with no diagram
  ;; THEN the flowchart goes to the graph and the map to the mind map, each
  ;;      with a reader that reads its block, point outside both is a user
  ;;      error, AND so is a buffer without a diagram
  (let (followed)
    (cl-letf (((symbol-function 'canvas-graph-follow)
               (lambda (buffer read name) (push (list 'graph buffer (funcall read buffer) name) followed)))
              ((symbol-function 'canvas-mindmap-follow)
               (lambda (buffer read name) (push (list 'mindmap buffer (funcall read buffer) name) followed))))
      (with-temp-buffer
        (insert canvas-mermaid-test--markdown)
        (goto-char (point-min))
        (search-forward "A --> B")
        (canvas-mermaid-show)
        (search-forward "Leaf")
        (canvas-mermaid-show)
        (pcase-let ((`((mindmap ,b2 ,tree ,n2) (graph ,b1 ,spec ,n1)) followed))
          (should (eq b1 (current-buffer)))
          (should (eq b2 (current-buffer)))
          (should (equal (canvas-mermaid-test--edges spec) '(("A" "B" nil))))
          (should (equal (canvas-mermaid-test--unplaced tree) '("Root" ("Leaf"))))
          (should (equal (list n1 n2) '("*canvas-mermaid*" "*canvas-mermaid*"))))
        (goto-char (point-min))
        (should-error (canvas-mermaid-show) :type 'user-error))
      (with-temp-buffer
        (insert "# Nothing to draw\n")
        (should-error (canvas-mermaid-show) :type 'user-error)))))

(ert-deftest canvas-mermaid-follows-its-block-as-it-is-edited ()
  ;; GIVEN the reader of the flowchart block of a markdown buffer
  ;; WHEN text is added above the block, a link inside it, then the block
  ;;      is broken mid-edit, then made a mind map, then removed
  ;; THEN the reader still reads the block after the text above, sees the
  ;;      new link, reads nothing while the block is broken or of another
  ;;      kind, keeping the last picture and saying why, AND nothing once
  ;;      the block is gone
  (with-temp-buffer
    (insert canvas-mermaid-test--markdown)
    (let* ((region (car (canvas-mermaid--regions)))
           (read (canvas-diagram-reader-follower canvas-mermaid-reader (copy-marker (car region)) 'graph)))
      (goto-char (point-min))
      (insert "Some words above.\n\n")
      (should (equal (canvas-mermaid-test--edges (funcall read (current-buffer))) '(("A" "B" nil))))
      (search-forward "A --> B")
      (insert "\n  B --> C")
      (should (equal (canvas-mermaid-test--edges (funcall read (current-buffer))) '(("A" "B" nil) ("B" "C" nil))))
      (insert "\n  C -->")
      (let ((said nil))
        (cl-letf (((symbol-function 'message) (lambda (&rest args) (setq said (apply #'format args)))))
          (should-not (funcall read (current-buffer)))
          (should said)))
      (delete-region (line-beginning-position) (line-end-position))
      (goto-char (point-min))
      (search-forward "flowchart TD")
      (replace-match "mindmap")
      (cl-letf (((symbol-function 'message) #'ignore))
        (should-not (funcall read (current-buffer))))
      (erase-buffer)
      (should-not (funcall read (current-buffer))))))

;;;; Exporting

(ert-deftest canvas-mermaid-export-writes-a-png-of-each-kind ()
  ;; GIVEN a mermaid file with a flowchart, and a markdown file whose first
  ;;       diagram is a mind map
  ;; WHEN each is exported
  ;; THEN a PNG of some size is written with no buffer involved, AND a file
  ;;      without a diagram is a user error
  (let ((png (make-temp-file "canvas-mermaid-test" nil ".png"))
        (mmd (make-temp-file "canvas-mermaid-test" nil ".mmd" "flowchart TD\n  A[Start] -->|go| B{Done?}\n  B -- no --> A\n"))
        (md (make-temp-file "canvas-mermaid-test" nil ".md" "# Map\n```mermaid\nmindmap\n  Root\n    A\n    B\n```\n"))
        (empty (make-temp-file "canvas-mermaid-test" nil ".md" "# Nothing\n"))
        (canvas-diagram-font "Sans 12px"))
    (unwind-protect
        (progn
          (should (equal (canvas-mermaid-export mmd png) png))
          (should (canvas-mermaid-test--png-p png))
          (should (> (file-attribute-size (file-attributes png)) 500))
          (delete-file png)
          (canvas-mermaid-export md png)
          (should (canvas-mermaid-test--png-p png))
          (should-error (canvas-mermaid-export empty png) :type 'user-error))
      (dolist (f (list png mmd md empty)) (when (file-exists-p f) (delete-file f))))))

;;;; The package

(defun canvas-mermaid-test--library ()
  "The source file of the canvas-mermaid library."
  (let ((file (locate-library "canvas-mermaid.el")))
    (should file)
    file))

(ert-deftest canvas-mermaid-package-headers-describe-the-package ()
  ;; GIVEN the library as it is published
  ;; WHEN package.el reads its headers
  ;; THEN it finds the name, a version, a summary, a URL, AND the layouts
  ;;      and an Emacs this very Emacs satisfies as requirements
  (with-temp-buffer
    (insert-file-contents (canvas-mermaid-test--library))
    (let* ((info (package-buffer-info))
           (reqs (package-desc-reqs info))
           (wanted (cadr (assq 'emacs reqs))))
      (should (equal (package-desc-name info) 'canvas-mermaid))
      (should (version-list-<= '(0 1) (package-desc-version info)))
      (should-not (equal (package-desc-summary info) "No description available."))
      (should (string-prefix-p "https://" (cdr (assq :url (package-desc-extras info)))))
      (should (version-list-<= '(32) wanted))
      (should (version-list-<= wanted (version-to-list emacs-version)))
      (should (assq 'canvas-graph reqs))
      (should (assq 'canvas-mindmap reqs)))))

(ert-deftest canvas-mermaid-package-names-its-author-and-licence ()
  ;; GIVEN the library, published under the GPL alongside the other
  ;;       canvas packages
  ;; WHEN its headers, its comments and the LICENSE beside it are read
  ;; THEN it names an author, holds the copyright the way they all do,
  ;;      carries a Commentary section and the notice, AND the LICENSE is
  ;;      the whole GPL
  (with-temp-buffer
    (insert-file-contents (canvas-mermaid-test--library))
    (should (string-match-p "[^ ]" (or (lm-header "author") "")))
    (should (save-excursion
              (goto-char (point-min))
              (re-search-forward "^;; Copyright (C) [0-9-]+ canvas-mermaid contributors$" nil t)))
    (should (lm-commentary-start))
    (should (save-excursion (re-search-forward "GNU General Public License" nil t))))
  (with-temp-buffer
    (insert-file-contents (expand-file-name "LICENSE" (file-name-directory (canvas-mermaid-test--library))))
    (should (save-excursion (re-search-forward "TERMS AND CONDITIONS" nil t)))
    (should (save-excursion (re-search-forward "Version 3, 29 June 2007" nil t)))))

;;; canvas-mermaid-tests.el ends here
