;;; canvas-mermaid.el --- Mermaid diagrams drawn on an Emacs canvas -*- lexical-binding: t -*-

;; Copyright (C) 2026 canvas-mermaid contributors

;; Author: Daskeladden
;; Version: 0.1.0
;; Package-Requires: ((emacs "32.0.50") (canvas-graph "0.1.0") (canvas-mindmap "0.1.0") (canvas-sequence "0.1.0"))
;; Keywords: multimedia, tools, convenience
;; URL: https://github.com/Daskeladden/canvas-mermaid

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Reads mermaid diagrams and draws them on an Emacs 32 canvas with the
;; layout each needs: a flowchart or a state diagram as a graph by
;; canvas-graph, a mind map as a tree by canvas-mindmap.  A diagram is a
;; mermaid buffer, or a mermaid block in a markdown fence or an org
;; source block; its drawing follows it as it is edited, and keeps the
;; last good picture while the text is half written.  A diagram no
;; layout draws yet, a sequence diagram say, is refused by name rather
;; than drawn wrong.
;;
;; This is a prototype.

;;; Code:

(require 'cl-lib)
(require 'canvas-graph)
(require 'canvas-sequence)
(require 'canvas-mindmap)

;;;; The text

(defun canvas-mermaid--meaningful-p (line)
  "Whether LINE, (TEXT . POS), says something: it is no blank and no comment."
  (not (string-match-p "\\`[ \t]*\\(?:%%.*\\)?\\'" (car line))))

(defconst canvas-mermaid--fence-re "\\`[ \t]*---[ \t]*\\'"
  "A line that opens or closes the front matter.")

(defun canvas-mermaid--title (lines)
  "The title the front matter LINES give the diagram, or nil."
  (cl-some (lambda (line)
             (and (string-match "\\`[ \t]*title:[ \t]*\\(.*?\\)[ \t]*\\'" (car line))
                  (string-trim (match-string 1 (car line)) "[\"']" "[\"']")))
           lines))

(defun canvas-mermaid--front-matter (lines)
  "LINES less blanks, comments and the front matter, as (TITLE . LINES),
TITLE what the front matter calls the diagram, or nil.  Front matter
never closed is an error."
  (let ((lines (cl-remove-if-not #'canvas-mermaid--meaningful-p lines)))
    (if (not (and lines (string-match-p canvas-mermaid--fence-re (caar lines))))
        (cons nil lines)
      (let ((end (or (cl-position-if (lambda (l) (string-match-p canvas-mermaid--fence-re (car l)))
                                     lines :start 1)
                     (error "canvas-mermaid: the front matter is never closed"))))
        (cons (canvas-mermaid--title (cl-subseq lines 1 end)) (nthcdr (1+ end) lines))))))

(defun canvas-mermaid--clean (label)
  "LABEL as a box or an arrow shows it: unquoted, its line breaks spaces;
nil for none."
  (when label
    (let ((clean (string-trim (replace-regexp-in-string
                               "[ \t]*<br[ \t]*/?>[ \t]*" " "
                               (string-trim label "[ \t\"`]+" "[ \t\"`]+")))))
      (and (not (string-empty-p clean)) clean))))

(defun canvas-mermaid--unclassed (text)
  "TEXT without the classes, :::NAME, written on its nodes."
  (replace-regexp-in-string ":::[[:alnum:]_-]+" "" text))

;;;; The diagram types

(defconst canvas-mermaid--layouts
  '(("flowchart" . flowchart) ("graph" . flowchart)
    ("stateDiagram" . state) ("stateDiagram-v2" . state)
    ("mindmap" . mindmap) ("sequenceDiagram" . sequence)
    ("classDiagram" . class) ("classDiagram-v2" . class))
  "The diagrams a layout draws, by the word that opens them.")

(defconst canvas-mermaid--without-layout
  '("erDiagram" "gantt" "pie" "journey"
    "gitGraph" "requirementDiagram" "quadrantChart" "timeline" "zenuml" "sankey-beta"
    "xychart-beta" "block-beta" "packet-beta" "kanban" "architecture-beta" "radar-beta"
    "treemap-beta" "C4Context" "C4Container" "C4Component" "C4Dynamic" "C4Deployment")
  "The diagrams mermaid knows that no layout draws yet.")

(defun canvas-mermaid--word (line)
  "The first word of LINE, (TEXT . POS)."
  (car (split-string (car line) "[ \t;]+" t)))

(defun canvas-mermaid--type (word)
  "The layout's name for the diagram WORD opens.  A diagram no layout
draws yet is a user error; a word mermaid does not know, an error."
  (or (cdr (assoc word canvas-mermaid--layouts))
      (if (member word canvas-mermaid--without-layout)
          (user-error "canvas-mermaid: a %s has no layout to draw it yet" word)
        (error "canvas-mermaid: %s opens no mermaid diagram" word))))

(defun canvas-mermaid-read (text &optional offset)
  "TEXT, a mermaid diagram beginning at the buffer position OFFSET or 1,
as (KIND . SPEC): a flowchart or a state diagram as a `graph', its spec
for `canvas-graph-build'; a mind map as a `mindmap', its nested spec for
canvas-mindmap.  Each node is placed where it is first written."
  (pcase-let* ((`(,title . ,lines) (canvas-mermaid--front-matter (canvas-diagram-text-lines text (or offset 1))))
               (header (or (car lines) (error "canvas-mermaid: no diagram in the text"))))
    (pcase (canvas-mermaid--type (canvas-mermaid--word header))
      ('flowchart (cons 'graph (canvas-mermaid--flowchart header (cdr lines) (or title "flowchart"))))
      ('state (cons 'graph (canvas-mermaid--states (cdr lines) (or title "state diagram"))))
      ('mindmap (cons 'mindmap (canvas-mermaid--mindmap (cdr lines))))
      ('sequence (cons 'sequence (canvas-mermaid--sequence (cdr lines))))
      ('class (cons 'graph (canvas-mermaid--classes (cdr lines) (or title "class diagram")))))))

;;;; Class diagrams

(defconst canvas-mermaid--class-skip-re
  (concat "\\`\\(?:direction\\|namespace\\|note\\|click\\|style\\|classDef\\|cssClass"
          "\\|link\\|callback\\|accTitle\\|accDescr\\|title\\|<<\\)")
  "A statement of a class diagram that draws nothing yet.")

(defconst canvas-mermaid--class-open-re
  "\\`class[ \t]+\\([^ \t{:]+\\)\\(?:\\[\"\\([^]\"]*\\)\"]\\)?[ \t]*\\({\\)?[ \t]*\\'"
  "A class line: its name, a label in brackets, and a brace that opens a
block of members.")

(defconst canvas-mermaid--class-member-re
  "\\`\\([^ \t:]+\\)[ \t]*:[ \t]*\\(.+\\)\\'"
  "A member written on a line of its own: the class, then the member.")

(defconst canvas-mermaid--class-relation-re
  (concat "\\`\\([^ \t\"]+\\)[ \t]*\\(?:\"[^\"]*\"[ \t]*\\)?"
          "\\([<*o|]*\\(?:--\\|\\.\\.\\)[>*o|]*\\)"
          "[ \t]*\\(?:\"[^\"]*\"[ \t]*\\)?\\([^ \t:\"]+\\)[ \t]*\\(?::[ \t]*\\(.*\\)\\)?\\'")
  "A relation between two classes: the one on the left, the marks between
them, the one on the right, and the label after a colon.  A cardinality
in double quotes on either side is passed by.")

(defun canvas-mermaid--class-head-left-p (marks)
  "Whether MARKS, the middle of a relation, point at the class on the
left, as `<|--' and `<--' do.  A diamond, the `o' of an aggregation or
the `*' of a composition, marks the whole it belongs to rather than a
way to read the line, so it leaves the order as it was written."
  (string-match-p "\\`[<|]" marks))

(defun canvas-mermaid--class-relation (draft text pos)
  "Note in DRAFT the relation TEXT at POS holds; whether it held one.
The edge runs towards the head of the arrow."
  (when (string-match canvas-mermaid--class-relation-re text)
    (let ((left (match-string 1 text))
          (marks (match-string 2 text))
          (right (match-string 3 text))
          (label (match-string 4 text)))
      (if (canvas-mermaid--class-head-left-p marks)
          (canvas-graph-draft-edge draft right left (and label (string-trim label)) pos)
        (canvas-graph-draft-edge draft left right (and label (string-trim label)) pos))
      t)))

(defun canvas-mermaid--class-line (draft text pos inside)
  "Read TEXT at POS of a class diagram into DRAFT, INSIDE being the class
whose block is open, or nil; the class whose block stays open."
  (cond
   ((and inside (string-match-p "\\`}" text)) nil)
   (inside (canvas-graph-draft-row draft inside text pos) inside)
   ((string-match-p canvas-mermaid--class-skip-re text) nil)
   ((string-match canvas-mermaid--class-open-re text)
    (let ((name (match-string 1 text))
          (label (match-string 2 text))
          (block (match-string 3 text)))
      (canvas-graph-draft-node draft name (and label (string-trim label)) pos)
      (and block name)))
   ((canvas-mermaid--class-relation draft text pos) nil)
   ((string-match canvas-mermaid--class-member-re text)
    (let ((name (match-string 1 text))
          (member (match-string 2 text)))
      (canvas-graph-draft-row draft name (string-trim member) pos))
    nil)))

(defun canvas-mermaid--classes (lines name)
  "The spec of the class diagram called NAME, LINES its body.  A class is
a box, its members are the rows of that box, and a relation is an arrow
towards the head it is drawn with."
  (let ((draft (canvas-graph-draft-create))
        (inside nil))
    (pcase-dolist (`(,raw . ,pos) lines)
      (let ((text (string-trim raw)))
        (unless (string-empty-p text)
          (setq inside (canvas-mermaid--class-line draft text pos inside)))))
    (canvas-graph-draft-spec draft name)))

;;;; Sequence diagrams

(defconst canvas-mermaid--sequence-skip-re
  (concat "\\`\\(?:autonumber\\|activate\\|deactivate\\|[Nn]ote\\|loop\\|alt\\|else\\|opt"
          "\\|par\\|and\\|critical\\|option\\|break\\|rect\\|end\\|box\\|link\\|links"
          "\\|create\\|destroy\\|accTitle\\|accDescr\\|title\\)\\(?:[ \t:]\\|\\'\\)")
  "A statement of a sequence diagram that draws nothing yet: a note, a
block such as loop or alt, an activation, or a line that only numbers or
titles the diagram.")

(defconst canvas-mermaid--party-re
  "\\`\\(?:participant\\|actor\\)[ \t]+\\([^ \t]+\\)\\(?:[ \t]+as[ \t]+\\(.+\\)\\)?\\'"
  "A line that names a party: its name, and the label after `as'.")

(defconst canvas-mermaid--arrow-re
  "\\(-\\{1,2\\}\\)\\(?:>>\\|>\\|)\\|x\\)"
  "An arrow of a message: one or two dashes, then its head.  Two dashes
draw a dashed line, as mermaid draws a reply.")

(defconst canvas-mermaid--message-re
  (concat "\\`\\([^-+:]+?\\)[ \t]*" canvas-mermaid--arrow-re "[ \t]*\\([^:]+?\\)[ \t]*:[ \t]*\\(.*\\)\\'")
  "A message: the party it comes from, its arrow, the party it goes to,
and its text.")

(defun canvas-mermaid--sequence-party (text pos)
  "The party line that TEXT at POS names, or nil.  Every group is taken
before anything else runs, because `string-trim' matches on its own and
would leave the match data of another search behind."
  (when (string-match canvas-mermaid--party-re text)
    (let ((name (match-string 1 text))
          (label (match-string 2 text)))
      (list 'party name (string-trim (or label name)) :pos pos))))

(defun canvas-mermaid--sequence-message (text pos)
  "The message line that TEXT at POS holds, or nil.  A party is named
plainly here; mermaid's +, - and : marks on a name are for activations,
which nothing draws yet."
  (when (string-match canvas-mermaid--message-re text)
    (let ((from (match-string 1 text))
          (dashes (match-string 2 text))
          (to (match-string 3 text))
          (label (match-string 4 text)))
      (list 'message (string-trim from) (string-trim to) (string-trim label)
            :dashed (equal dashes "--") :pos pos))))

(defun canvas-mermaid--named-parties (lines)
  "The parties LINES name in messages but no party line names, as (NAME .
POS), in the order they first appear, POS being where that message was
read from."
  (let ((named (delq nil (mapcar (lambda (line) (nth 1 line))
                                 (cl-remove-if-not (lambda (line) (eq (car line) 'party)) lines))))
        found)
    (dolist (line lines (nreverse found))
      (when (eq (car line) 'message)
        (let ((pos (plist-get (nthcdr 4 line) :pos)))
          (dolist (name (list (nth 1 line) (nth 2 line)))
            (unless (or (member name named) (assoc name found))
              (push (cons name pos) found))))))))

(defun canvas-mermaid--sequence (lines)
  "LINES of a sequence diagram as a spec for canvas-sequence.
A party a message names first is named where it first appears.  A
statement that draws nothing yet is passed by."
  (let (out)
    (pcase-dolist (`(,raw . ,pos) lines)
      (let ((text (string-trim raw)))
        (unless (string-match-p canvas-mermaid--sequence-skip-re text)
          (when-let* ((line (or (canvas-mermaid--sequence-party text pos)
                                (canvas-mermaid--sequence-message text pos))))
            (push line out)))))
    (setq out (nreverse out))
    (append (mapcar (pcase-lambda (`(,name . ,pos)) (list 'party name name :pos pos))
                    (canvas-mermaid--named-parties out))
            out)))

;;;; Flowcharts

(defconst canvas-mermaid--skip-re
  (concat "\\`\\(?:classDef\\|class\\|style\\|linkStyle\\|click\\|direction"
          "\\|subgraph\\|end\\|accTitle\\|accDescr\\|title\\)\\(?:[ \t:]\\|\\'\\)")
  "A flowchart statement that draws nothing.")

(defconst canvas-mermaid--header-re
  "\\`[ \t]*\\(?:flowchart\\|graph\\)\\(?:[ \t]+\\(?:TB\\|TD\\|BT\\|RL\\|LR\\)\\)?[ \t]*;?"
  "A flowchart's header, up to what may follow it on its line.")

(defconst canvas-mermaid--shapes
  '(("(((" . ")))") ("((" . "))") ("([" . "])") ("[[" . "]]") ("[(" . ")]") ("{{" . "}}")
    ("[/" . "[/\\]]") ("[\\" . "[/\\]]") ("(" . ")") ("[" . "]") ("{" . "}") (">" . "]"))
  "A node's shapes: the text that opens each, and a regexp of what closes
it, the longest openers first.")

(defconst canvas-mermaid--text-link-re
  (concat "[ \t]*\\([<ox]\\)?\\(?:--\\|==\\|-\\.\\)[ \t]+\\(.+?\\)[ \t]+"
          "\\(?:-\\{2,\\}\\|=\\{2,\\}\\|\\.-\\)\\([>ox]\\)?")
  "A link with its label written inline, -- LABEL --> say: the head at its
tail, its label and its head.")

(defconst canvas-mermaid--link-re
  (concat "[ \t]*\\([<ox]\\)?\\(-\\.+-\\|-\\{2,\\}\\|=\\{2,\\}\\|~\\{3,\\}\\)\\([>ox]\\)?"
          "\\(?:[ \t]*|\\([^|]*\\)|\\)?")
  "A link, --> say, with its label between bars if it has one: the head
at its tail, its line, its head and its label.")

(defun canvas-mermaid--trimmed (text from to pos)
  "The part of TEXT FROM TO, trimmed, as (PART . WHERE), WHERE its buffer
position, TEXT beginning at POS; nil when it is blank."
  (let* ((part (substring text from to))
         (lead (progn (string-match "\\`[ \t]*" part) (match-end 0)))
         (trimmed (string-trim part)))
    (and (not (string-empty-p trimmed)) (cons trimmed (+ pos from lead)))))

(defun canvas-mermaid--statements (line)
  "The statements of LINE, (TEXT . POS), split at the semicolons outside
quotes and brackets, as (STATEMENT . POS) each, trimmed."
  (let ((text (car line)) (start 0) (depth 0) (quoted nil) pieces)
    (dotimes (i (length text))
      (pcase (aref text i)
        (?\" (setq quoted (not quoted)))
        ((or ?\[ ?\( ?\{) (unless quoted (cl-incf depth)))
        ((or ?\] ?\) ?\}) (unless quoted (cl-decf depth)))
        (?\; (when (and (not quoted) (<= depth 0))
               (push (cons start i) pieces)
               (setq start (1+ i))))))
    (push (cons start (length text)) pieces)
    (delq nil (mapcar (lambda (piece) (canvas-mermaid--trimmed text (car piece) (cdr piece) (cdr line)))
                      (nreverse pieces)))))

(defun canvas-mermaid--at (re text at)
  "Whether RE matches TEXT right at AT, the match data then set."
  (eq (string-match re text at) at))

(defun canvas-mermaid--shape-text (text at close)
  "The label written from AT in TEXT up to CLOSE, a regexp, quoted or not,
and where the shape ends: (LABEL . END).  One never closed is an error."
  (let ((quoted (and (< at (length text)) (eq (aref text at) ?\"))))
    (let ((from (if quoted (1+ at) at))
          (to (if quoted
                  (or (string-search "\"" text (1+ at))
                      (error "canvas-mermaid: a quote never closed in «%s»" text))
                (and (string-match close text at) (match-beginning 0)))))
      (unless (and to (canvas-mermaid--at close text (if quoted (1+ to) to)))
        (error "canvas-mermaid: a shape never closed in «%s»" text))
      (cons (substring text from to) (match-end 0)))))

(defun canvas-mermaid--shape (text at)
  "The shape opening at AT in TEXT as (LABEL . END), or nil when none does."
  (cl-loop for (open . close) in canvas-mermaid--shapes
           when (string-prefix-p open (substring text at))
           return (canvas-mermaid--shape-text text (+ at (length open)) close)))

(defun canvas-mermaid--extra (text at)
  "What follows a node's name and shape at AT in TEXT: a class, :::NAME,
or the new syntax, @{... label: LABEL ...}; as (LABEL . END), LABEL nil
for a class; nil for neither."
  (cond ((canvas-mermaid--at ":::[[:alnum:]_-]+" text at) (cons nil (match-end 0)))
        ((canvas-mermaid--at "@{\\([^}]*\\)}" text at)
         (let ((body (match-string 1 text)) (end (match-end 0)))
           (cons (and (string-match "label:[ \t]*\\(?:\"\\([^\"]*\\)\"\\|\\([^,}]*\\)\\)" body)
                      (or (match-string 1 body) (match-string 2 body)))
                 end)))))

(defun canvas-mermaid--node (draft text at pos)
  "Note in DRAFT the node written at AT in TEXT, a statement beginning at
POS, with the label its shape or the new syntax gives it; return its
name and where it ends, (NAME . END).  No node there is an error."
  (unless (canvas-mermaid--at "[ \t]*\\([[:alnum:]_]+\\)" text at)
    (error "canvas-mermaid: cannot read «%s»" text))
  (let* ((name (match-string 1 text))
         (name-at (match-beginning 1))
         (name-end (match-end 0))
         (shape (canvas-mermaid--shape text name-end))
         (after-shape (if shape (cdr shape) name-end))
         (extra (canvas-mermaid--extra text after-shape)))
    (canvas-graph-draft-node draft name (canvas-mermaid--clean (or (car shape) (car extra))) (+ pos name-at))
    (cons name (if extra (cdr extra) after-shape))))

(defun canvas-mermaid--group (draft text at pos)
  "Note in DRAFT the nodes written from AT in TEXT, a statement beginning
at POS, joined by &; return their names and where they end, (NAMES . END)."
  (let* ((node (canvas-mermaid--node draft text at pos))
         (names (list (car node)))
         (end (cdr node)))
    (while (canvas-mermaid--at "[ \t]*&" text end)
      (setq node (canvas-mermaid--node draft text (match-end 0) pos))
      (push (car node) names)
      (setq end (cdr node)))
    (cons (nreverse names) end)))

(defun canvas-mermaid--matched-link (text label-group)
  "The link the last match found in TEXT, its label in LABEL-GROUP, as
\(LABEL BOTH INVISIBLE END); the match data are read before the label is
cleaned, which changes them."
  (let ((label (match-string label-group text))
        (both (and (match-beginning 1) t))
        (invisible (string-prefix-p "~" (match-string 2 text)))
        (end (match-end 0)))
    (list (canvas-mermaid--clean label) both invisible end)))

(defun canvas-mermaid--link (text at)
  "The link written at AT in TEXT as (LABEL BOTH INVISIBLE END): its label,
whether it runs both ways, whether it is invisible, and where it ends;
nil when no link is there."
  (cond ((canvas-mermaid--at canvas-mermaid--text-link-re text at)
         (canvas-mermaid--matched-link text 2))
        ((canvas-mermaid--at canvas-mermaid--link-re text at)
         (canvas-mermaid--matched-link text 4))))

(defun canvas-mermaid--join (draft froms tos label both pos)
  "Note in DRAFT a link from each of FROMS to each of TOS under LABEL at
POS, and one back again when BOTH."
  (dolist (from froms)
    (dolist (to tos)
      (canvas-graph-draft-edge draft from to label pos)
      (when both
        (canvas-graph-draft-edge draft to from label pos)))))

(defun canvas-mermaid--statement (draft statement)
  "Note in DRAFT the nodes and links of STATEMENT, (TEXT . POS), a
flowchart's.  What cannot be read is an error."
  (pcase-let* ((`(,text . ,pos) statement)
               (`(,froms . ,at) (canvas-mermaid--group draft text 0 pos)))
    (while (string-match-p "[^ \t]" (substring text at))
      (pcase-let* ((`(,label ,both ,invisible ,after)
                    (or (canvas-mermaid--link text at) (error "canvas-mermaid: cannot read «%s»" text)))
                   (`(,tos . ,end) (canvas-mermaid--group draft text after pos)))
        (unless invisible
          (canvas-mermaid--join draft froms tos label both (+ pos at)))
        (setq froms tos at end)))))

(defun canvas-mermaid--after-header (header)
  "What follows the type and the direction on HEADER, a flowchart's first
line, as a line (TEXT . POS), or nil for nothing."
  (string-match canvas-mermaid--header-re (car header))
  (let* ((end (match-end 0))
         (rest (substring (car header) end)))
    (and (string-match-p "[^ \t]" rest) (cons rest (+ (cdr header) end)))))

(defun canvas-mermaid--flowchart (header lines name)
  "The spec of the flowchart called NAME: HEADER, its first line, perhaps
with a statement after the type, and LINES, the rest."
  (let ((draft (canvas-graph-draft-create)))
    (dolist (line (delq nil (cons (canvas-mermaid--after-header header) lines)))
      (dolist (statement (canvas-mermaid--statements line))
        (unless (string-match-p canvas-mermaid--skip-re (car statement))
          (canvas-mermaid--statement draft statement))))
    (canvas-graph-draft-spec draft name)))

;;;; State diagrams

(defconst canvas-mermaid--state-re "\\(\\[\\*\\]\\|[[:alnum:]_]+\\)"
  "A state, or the pseudo-state [*].")

(defconst canvas-mermaid--state-lines
  `((skip . "\\`\\(?:direction\\|classDef\\|class\\|accTitle\\|accDescr\\|title\\|--\\)\\(?:[ \t]\\|\\'\\)")
    (note-line . "\\`note[ \t].*:")
    (note . "\\`note[ \t]")
    (named . "\\`state[ \t]+\"\\([^\"]*\\)\"[ \t]+as[ \t]+\\([[:alnum:]_]+\\)\\'")
    (composite . "\\`state[ \t]+\\([[:alnum:]_]+\\)[ \t]*{\\'")
    (state . "\\`state[ \t]+\\([[:alnum:]_]+\\)\\(?:[ \t]*<<[[:alnum:]_]+>>\\)?\\'")
    (close . "\\`}\\'")
    (transition . ,(concat "\\`" canvas-mermaid--state-re "[ \t]*-->[ \t]*" canvas-mermaid--state-re
                           "[ \t]*\\(?::[ \t]*\\(.*\\)\\)?\\'"))
    (description . "\\`\\([[:alnum:]_]+\\)[ \t]*:"))
  "The lines of a state diagram: what each is, and the regexp that knows it.")

(defun canvas-mermaid--state-line (text)
  "What TEXT, a trimmed line of a state diagram, is: (KIND GROUP1 GROUP2
GROUP3), KIND a key of `canvas-mermaid--state-lines'.  None is an error."
  (or (cl-loop for (kind . re) in canvas-mermaid--state-lines
               when (string-match re text)
               return (cons kind (mapcar (lambda (i) (match-string i text)) '(1 2 3))))
      (error "canvas-mermaid: cannot read «%s»" text)))

(defun canvas-mermaid--pseudo (state)
  "STATE, or nil for the pseudo-state [*]."
  (unless (equal state "[*]") state))

(defun canvas-mermaid--states (lines name)
  "The spec of the state diagram called NAME, LINES its body.  Composite
states are flattened, their inner pseudo-states joined to them as
`canvas-graph-draft-transition' says; notes and descriptions draw nothing."
  (let ((draft (canvas-graph-draft-create)) (inside nil) (in-note nil))
    (pcase-dolist (`(,raw . ,pos) lines)
      (let ((text (canvas-mermaid--unclassed (string-trim raw)))
            (at (+ pos (progn (string-match "\\`[ \t]*" raw) (match-end 0)))))
        (if in-note
            (setq in-note (not (string-match-p "\\`end[ \t]+note\\'" text)))
          (pcase (canvas-mermaid--state-line text)
            (`(note . ,_) (setq in-note t))
            (`(named ,label ,state ,_) (canvas-graph-draft-node draft state label at))
            (`(composite ,state . ,_) (canvas-graph-draft-node draft state nil at) (push state inside))
            (`(close . ,_) (pop inside))
            (`(transition ,from ,to ,label)
             (canvas-graph-draft-transition draft (canvas-mermaid--pseudo from) (canvas-mermaid--pseudo to)
                                            label at (car inside)))
            (`(,(or 'state 'description) ,state . ,_) (canvas-graph-draft-node draft state nil at))))))
    (canvas-graph-draft-spec draft name)))

;;;; Mind maps

(defconst canvas-mermaid--mindmap-shapes
  '(("((" . "))") ("))" . "((") ("{{" . "}}") ("(" . ")") (")" . "(") ("[" . "]"))
  "A mind map node's shapes: the text that opens each and the text that
closes it, the longest first.")

(defun canvas-mermaid--mindmap-label (text)
  "The label of the mind map node written as TEXT: the text in its shape
after its name, else all of it."
  (let ((text (canvas-mermaid--unclassed text)))
    (or (cl-loop for (open . close) in canvas-mermaid--mindmap-shapes
                 when (string-match (concat "\\`[[:alnum:]_-]*" (regexp-quote open)
                                            "\\(.*\\)" (regexp-quote close) "\\'")
                                    text)
                 return (canvas-mermaid--clean (match-string 1 text)))
        (canvas-mermaid--clean text))))

(defun canvas-mermaid--icon (classes)
  "The icon canvas-diagram draws for CLASSES, a mermaid icon's: one of the
material set for mdi mdi-NAME, nil for any other."
  (let ((classes (string-trim classes)))
    (and (string-match "\\`mdi[ \t]+mdi-\\([[:alnum:]-]+\\)\\'" classes)
         (concat "material:" (match-string 1 classes)))))

(defun canvas-mermaid--give-icon (item classes)
  "Give ITEM, (INDENT LABEL OPTION...), the icon CLASSES name, if any.
An icon before any node is an error."
  (unless item
    (error "canvas-mermaid: an icon before any node"))
  (when-let* ((icon (canvas-mermaid--icon classes)))
    (nconc item (list :icon icon))))

(defun canvas-mermaid--mindmap (lines)
  "The nested spec of the mind map whose body is LINES, nested by indent,
each node placed where its text begins.  More than one root is an error."
  (let (items)
    (pcase-dolist (`(,raw . ,pos) lines)
      (let* ((lead (progn (string-match "\\`[ \t]*" raw) (match-string 0 raw)))
             (text (string-trim raw)))
        (if (string-match "\\`::icon(\\(.*\\))\\'" text)
            (canvas-mermaid--give-icon (car items) (match-string 1 text))
          (push (list (string-width (replace-regexp-in-string "\t" "    " lead))
                      (canvas-mermaid--mindmap-label text) :pos (+ pos (length lead)))
                items))))
    (let ((forest (canvas-mindmap-nest (nreverse items))))
      (unless (= (length forest) 1)
        (error "canvas-mermaid: a mind map has one root, not %d" (length forest)))
      (car forest))))

;;;; Diagrams in a buffer

(defconst canvas-mermaid--languages '("mermaid" "mmd")
  "What markdown fences and org source blocks call mermaid.")

(defun canvas-mermaid--diagram-p (text)
  "Whether TEXT is a mermaid diagram: after any front matter, it opens
with a word that opens one."
  (when-let* ((header (cadr (ignore-errors (canvas-mermaid--front-matter (canvas-diagram-text-lines text 1)))))
              (word (canvas-mermaid--word header)))
    (and (or (assoc word canvas-mermaid--layouts) (member word canvas-mermaid--without-layout)) t)))

(defun canvas-mermaid--regions ()
  "The mermaid diagrams of the current buffer as ((START . END)...): its
mermaid blocks, else all of it when it is one."
  (save-restriction
    (widen)
    (or (canvas-diagram-code-blocks canvas-mermaid--languages)
        (and (or (derived-mode-p 'mermaid-mode)
                 (canvas-mermaid--diagram-p (buffer-substring-no-properties (point-min) (point-max))))
             (list (cons (point-min) (point-max)))))))

(defun canvas-mermaid--regions-or-error (&optional name)
  "The mermaid diagrams of the current buffer; none is a user error,
naming the buffer, or NAME."
  (or (canvas-mermaid--regions)
      (user-error "canvas-mermaid: no mermaid diagram in %s" (or name (buffer-name)))))

(defun canvas-mermaid--read-region (region)
  "The diagram in REGION, (START . END) of the current buffer, as
`canvas-mermaid-read' reads it, placed in the buffer."
  (canvas-mermaid-read (buffer-substring-no-properties (car region) (cdr region)) (car region)))

(defun canvas-mermaid--spec-of (region kind)
  "The spec of the diagram in REGION of the current buffer, of KIND; a
diagram of another kind is a user error."
  (pcase-let ((`(,found . ,spec) (canvas-mermaid--read-region region)))
    (unless (eq found kind)
      (user-error "canvas-mermaid: the diagram is a %s now; draw it again to see it" found))
    spec))

(defun canvas-mermaid--reader (marker kind)
  "A function reading, from a buffer, the mermaid diagram at MARKER as a
spec of KIND, for following it; see `canvas-diagram-region-reader'."
  (canvas-diagram-region-reader marker #'canvas-mermaid--regions
                                (lambda (region) (canvas-mermaid--spec-of region kind))))

;;;; Showing and exporting

(defun canvas-mermaid--follow (kind buffer read)
  "Draw what READ finds in BUFFER with the layout for KIND, following BUFFER."
  (pcase kind
    ('graph (canvas-graph-follow buffer read "*canvas-mermaid*"))
    ('mindmap (canvas-mindmap-follow buffer read "*canvas-mermaid*"))
    ('sequence (canvas-sequence-follow buffer read "*canvas-mermaid*"))))

(defun canvas-mermaid--show-region (region)
  "Draw the mermaid diagram in REGION of the current buffer, following it."
  (let ((kind (car (canvas-mermaid--read-region region))))
    (canvas-mermaid--follow kind (current-buffer)
                            (canvas-mermaid--reader (copy-marker (car region)) kind))))

;;;###autoload
(defun canvas-mermaid-show ()
  "Draw the mermaid diagram point is in: a flowchart or a state diagram as
a graph, a mind map as a tree.  The current buffer is a mermaid one, or
holds mermaid in markdown fences or org source blocks; with a single
diagram, point may be anywhere.  The drawing follows the buffer: it is
drawn again once typing there pauses."
  (interactive)
  (canvas-mermaid--show-region (canvas-diagram-region-at (canvas-mermaid--regions-or-error) (point))))

;;;###autoload
(defun canvas-mermaid-show-file (file)
  "Draw the first mermaid diagram of FILE, following the buffer visiting it."
  (interactive "fMermaid, markdown or org file: ")
  (with-current-buffer (find-file-noselect file)
    (canvas-mermaid--show-region (car (canvas-mermaid--regions-or-error)))))

;;;###autoload
(defun canvas-mermaid-export (file out)
  "Draw the first mermaid diagram of FILE into OUT, a PNG, an SVG or a PDF
by its name, and return OUT.  FILE is a mermaid file, or a markdown or
org file with mermaid blocks.  No buffer or frame is needed, so this
works in batch."
  (pcase-let ((`(,kind . ,spec)
               (with-temp-buffer
                 (insert-file-contents file)
                 (canvas-mermaid--read-region
                  (car (canvas-mermaid--regions-or-error (file-name-nondirectory file)))))))
    (pcase kind
      ('graph (canvas-graph-export spec out))
      ('mindmap (canvas-mindmap-export spec out))
      ('sequence (canvas-sequence-export spec out)))))

;;;; Demo

(defconst canvas-mermaid--demo
  "---
title: Ordering coffee
---
flowchart TD
  start([Thirsty]) --> order[Order a coffee]
  order --> milk{Milk?}
  milk -->|yes| froth[Froth the milk]
  milk -->|no| pour[Pour]
  froth --> pour
  pour --> pay[Pay]
  pay -- card declined --> order
  pay --> drink((Drink))
"
  "A small flowchart to try the drawing on.")

;;;###autoload
(defun canvas-mermaid-demo ()
  "Show a small flowchart, with its mermaid in a buffer to edit and watch."
  (interactive)
  (with-current-buffer (get-buffer-create "*canvas-mermaid demo*")
    (erase-buffer)
    (insert canvas-mermaid--demo)
    (when (fboundp 'mermaid-mode)
      (mermaid-mode))
    (goto-char (point-min))
    (switch-to-buffer (current-buffer))
    (canvas-mermaid-show)))

(provide 'canvas-mermaid)
;;; canvas-mermaid.el ends here
