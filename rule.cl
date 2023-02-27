(defstruct rule 
  name
  conditions
  actions
)
(defparameter prix nil)
(defparameter poste nil)
(defparameter role nil)
(defparameter range nil)
(defparameter cancer nil)
(defparameter difficulte nil)

(defparameter liste nil)


(defun tri-post (lc lt)
    (cond
        ((equal (car lc) nil) (setq liste lt))
        ((equal (post (car lc)) poste) (push (car lc) lt) (tri-post (cdr lc) lt))
        (t (tri-post (cdr lc) lt))
    )
)
(defun tri-role (lc lt)
    (cond
        ((equal (car lc) nil) (setq liste lt))
        ((estDansListe (role (car lc)) role) (push (car lc) lt) (tri-role (cdr lc) lt))
        (t (tri-role (cdr lc) lt))
    )
)
(defun tri-range (lc lt)
    (cond
        ((equal (car lc) nil) (setq liste lt))
        ((equal (range (car lc)) range) (push (car lc) lt) (tri-range (cdr lc) lt))
        (t (tri-range (cdr lc) lt))
    )
)
(defun tri-difficulte (lc lt)
    (cond
        ((equal (car lc) nil) (setq liste lt))
        ((equal (difficulte (car lc)) difficulte) (push (car lc) lt) (tri-difficulte (cdr lc) lt))
        (t (tri-difficulte (cdr lc) lt))
    )
)
(defun tri-cancer (lc lt)
    (cond
        ((equal (car lc) nil) (setq liste lt))
        ((equal (cancer (car lc)) cancer) (push (car lc) lt) (tri-cancer (cdr lc) lt))
        (t (tri-cancer (cdr lc) lt))
    )
)



(defun displayN (lc)
    (cond
        ((equal lc nil) nil)
        (t (print (nom (car lc))) (displayN (cdr lc)))
    )
)



(defvar nice-post 
    (make-rule 
        :name 'nice-post
        :conditions (lambda () (equal poste nil)) 
        :actions (lambda ()
            (princ "La lane ? (Top | Jungle | Mid | Bot | Support) : ")
            (finish-output)
            (setq poste (read-line))
            (tri-post liste-de-champion '() )
            (displayN  liste)
            (terpri)
        )
    )
)

(defvar nice-role 
    (make-rule 
        :name 'nice-role
        :conditions (lambda () (equal role nil)) 
        :actions (lambda ()
            (princ "Le role ? (Combattant | Tank | Assassin | Mage | Suppport | Shooter) : ")
            (finish-output)
            (setq role (read-line))
            (tri-role liste '() )
            (displayN  liste)
            (terpri)
        )
    )
)

(defvar nice-range 
    (make-rule 
        :name 'nice-range
        :conditions (lambda () (equal range nil)) 
        :actions (lambda ()
            (princ "La range ? (Range | Melee) : ")
            (finish-output)
            (setq range (read-line))
            (tri-range liste '() )
            (displayN  liste)
            (terpri)
        )
    )
)

(defvar nice-difficulte 
    (make-rule 
        :name 'nice-difficulte
        :conditions (lambda () (equal difficulte nil)) 
        :actions (lambda ()
            (princ "La difficulté ? (1 | 2) : ")
            (finish-output)
            (setq difficulte (read-line))
            (tri-difficulte liste '() )
            (displayN  liste)
            (terpri)
        )
    )
)

(defvar nice-cancer 
    (make-rule 
        :name 'nice-cancer
        :conditions (lambda () (equal cancer nil)) 
        :actions (lambda ()
            (princ "Le cancer : ")
            (finish-output)
            (setq cancer (read-line))
            (if (not (equal (car liste) nil))
                 (cancerland liste (car liste) (parse-integer cancer))
                 (format t ""))
            
            (displayN  liste)
            (terpri)
        )
    )
)


(defvar finaly 
    (make-rule 
        :name 'nice-post
        :conditions (lambda () (and (not (equal range nil)) 
                                    (and (not (equal role nil))
                                        (and (not (equal difficulte nil))
                                            (and (not (equal cancer nil))
                                                 (not (equal poste nil))
                                                    ))))) 
        :actions (lambda ()
            ;;truc final
            (if (not (equal (car liste) nil))
                 (format t "vous allez jouer ~a.~%" (nom (car liste)))
                 (format t "aucun survivant"))
            
        )
    )
)

(defparameter lr (list finaly nice-cancer nice-difficulte nice-range nice-role nice-post ))

(defvar prochaineRegle (make-rule :name "prochaine-regle" :conditions nil :actions nil))


(defun moteurRun()
    (dolist (regle lr)
        (if (not(equal (rule-name regle) "prochaine-regle"))
            (if (funcall (rule-conditions regle))
                (setf prochaineRegle regle)
            )
        ) 
    )
    (if (not (eq (rule-name prochaineRegle) "prochaine-regle"))
        (funcall (rule-actions prochaineRegle))
        (error "Aucune regle ne peut etre appliquee")
    )    
)

(defun run (nb)
    (cond
        ((equal nb (length lr)) nil)
        (t (moteurRun) (run (+ nb 1)))
    )
)
