
(defun edge-label (edge)
    (car edge)
)

(defun edge-node (edge)
    (car (cdr edge))
)


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


(defun next-nodes (g lab nodes)
    (cond
        ((null nodes) nil)
        (t (join-lists (next-nodes-from-edges lab (funcall g (car nodes)))
                       (next-nodes g lab (cdr nodes))))
    )
)


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





