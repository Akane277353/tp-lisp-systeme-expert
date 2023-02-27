(load "champions.cl")
(load "rule.cl")

; Base de faits
(defparameter champion "personne")
(defparameter CA '(not (null champion)))

; Fonctions

(defun estDansListe (liste element)
    (if (null liste)
        nil
        (if (equal (car liste) element)
            t
            (estDansListe (cdr liste) element)
        )
    )
)

(defun cancerland (lc c str)
    (cond 
        ((equal lc nil) (setq liste (list c)))
        ((equal (< (abs (- str (parse-integer (cancer  (car lc))))) (abs (- str (parse-integer (cancer c))))) t) (cancerland (cdr lc) (car lc) str))
        (t (cancerland (cdr lc) c str))
    )
)

(run 0)

(terpri)

