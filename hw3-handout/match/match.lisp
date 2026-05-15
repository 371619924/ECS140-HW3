; You may define helper functions here

; You may define helper functions here

(defun match (pattern assertion)
    (cond
        ((and (null pattern) (null assertion))
            t
        )

        ((null pattern)
            nil
        )

        ((null assertion)
            nil
        )

        ((equal (car pattern) '?)
            (match (cdr pattern) (cdr assertion))
        )

        ((equal (car pattern) '!)
            (or
                (match (cdr pattern) (cdr assertion))
                (match pattern (cdr assertion))
            )
        )

        ((equal (car pattern) (car assertion))
            (match (cdr pattern) (cdr assertion))
        )

        (t
            nil
        )
    )
)