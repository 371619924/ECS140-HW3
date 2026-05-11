;; helper: get the label part of an edge like (a 0)
(defun edge-label (edge)
    (car edge)
)

;; helper: get the next-node part of an edge like (a 0)
(defun edge-node (edge)
    (car (cdr edge))
)

;; helper: check whether node exists in graph g.
;; In the handout, a node with no outgoing edges may return (nil),
;; while a node not in the graph returns nil.
(defun node-exists (g node)
    (if (null (funcall g node)) nil t)
)

;; helper: recursive membership check
(defun in-list (item lst)
    (cond
        ((null lst) nil)
        ((equal item (car lst)) t)
        (t (in-list item (cdr lst)))
    )
)

;; helper: recursive append, so we do not need loop
(defun join-lists (a b)
    (cond
        ((null a) b)
        (t (cons (car a) (join-lists (cdr a) b)))
    )
)

;; From one graph node's edge list, collect all next nodes reachable
;; by an edge whose label equals lab.
(defun next-nodes-from-edges (lab edges)
    (cond
        ((null edges) nil)
        ((null (car edges)) nil)
        ((equal lab (edge-label (car edges)))
            (cons (edge-node (car edges))
                  (next-nodes-from-edges lab (cdr edges))))
        (t (next-nodes-from-edges lab (cdr edges)))
    )
)

;; Given a list of possible current nodes in graph g, follow one label lab
;; and return all possible next nodes.
(defun next-nodes (g lab nodes)
    (cond
        ((null nodes) nil)
        (t (join-lists (next-nodes-from-edges lab (funcall g (car nodes)))
                       (next-nodes g lab (cdr nodes))))
    )
)

;; Try every outgoing edge in g1 from the current node.
(defun find-sequence-from-edges (edges g1 g2 target k g2nodes)
    (cond
        ((null edges) nil)
        ((null (car edges)) nil)
        (t (let ((result
                    (find-sequence-helper
                        g1
                        g2
                        (edge-node (car edges))
                        target
                        (- k 1)
                        (next-nodes g2 (edge-label (car edges)) g2nodes))))
                (if result
                    (cons (cons (edge-label (car edges)) (car result)) t)
                    (find-sequence-from-edges (cdr edges) g1 g2 target k g2nodes))))
    )
)

;; Main recursive search.
;; g2nodes means: after following the labels chosen so far,
;; these are all possible current nodes in g2.
(defun find-sequence-helper (g1 g2 current target k g2nodes)
    (cond
        ((zerop k)
            (if (and (equal current target)
                     (node-exists g1 current)
                     (null (in-list target g2nodes)))
                (cons nil t)
                nil))
        (t (find-sequence-from-edges (funcall g1 current) g1 g2 target k g2nodes))
    )
)

(defun find-sequence (g1 g2 start target k)
    (find-sequence-helper
        g1
        g2
        start
        target
        k
        (if (node-exists g2 start) (list start) nil))
)





