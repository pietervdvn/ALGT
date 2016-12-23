#lang racket
;
; .____          .__  .__          _________            .__        __    ._.
; |    |    ____ |  | |  | ___.__./   _____/ ___________|__|______/  |_  | |
; |    |   /  _ \|  | |  |<   |  |\_____  \_/ ___\_  __ \  \____ \   __\ | |
; |    |__(  <_> )  |_|  |_\___  |/        \  \___|  | \/  |  |_> >  |    \|
; |_______ \____/|____/____/ ____/_______  /\___  >__|  |__|   __/|__|    __
;         \/               \/            \/     \/         |__|           \/
;
; PLT-Redex model of the LollyScript Programming Language.
;
; Christophe.Scholliers@UGent.be &  noostvog@vub.ac.be
;

(require redex)
(provide (all-defined-out))

(define-language L
  ;Qualifiers 
  (q ::= un lin)
  ;Terms
  (e ::=
     x
     (q n)
     (q unit)
     (q (pair e e))
     (q (λ (x τ) e))
     (e e)
     (let (x x) e e)
     (Promise τ)
     (fork e)
     (wait e)
     (resolve e e)
     (q (Fut x τ))
     (q (Res x τ)))
  ;Pretypes
  (Pt ::= Int
          Unit
          (* τ τ)
          (-> τ τ)
          (Fut x τ)
          (Res x τ))
  ;Types
  (τ ::= (q Pt))
  ;Variables
  (x ::= variable-not-otherwise-mentioned)
  ;Type Environment
  (Γ ::= (x τ Γ) ·)
  ;Promise Environment
  (P :=  (x v P) ·)
  ;Numbers
  (n ::= natural)
  ;Values
  (v ::=
     (q n)
     (q unit)
     (q (pair v v))
     (q (λ (x τ) e))
     (q (Fut x τ))
     (q (Res x τ)))  
  ;Evaluation contexts
  (E ::= hole
        (q (pair E e))
        (q (pair v E))
        (let (x x) E e)
        (E e)
        (v E)
        (wait E)
        (resolve E e)
        (resolve v E))
  #:binding-forms
  (q (λ (x τ) e #:refers-to x))
  (let (x_1 x_2) e e #:refers-to (shadow x_1 x_2)))


; Lookup a variable in a store
(define-metafunction L
  lookup : Γ x -> τ or ⊥
  [(lookup · x) ⊥]
  [(lookup (x τ Γ) x) τ]
  [(lookup (x_1 τ Γ) x_2) (lookup Γ x_2)])

; Lookup a variable in the promise environment
(define-metafunction L
  find : P x -> v or ⊥
  [(find · x) ⊥]
  [(find (x v P) x) v]
  [(find (x_1 τ P) x_2) (lookup P x_2)])

(define red
  (reduction-relation
   L
   (-->_loc
    ((q (λ (x τ) e)) v)
    (substitute e x v)
    "E-App")
   
   (-->_loc
    (let (x_1 x_2) (q (pair v_1 v_2)) e_b)
    (substitute (substitute e_b x_1 v_1) x_2 v_2)
    "E-PMatchLet")

   (-->_loc
    (Promise (q Pt))
    (lin (pair (q (Fut l (q Pt))) (q (Res l (q Pt)))))
    (fresh l)
    "E-Promise")

   (-->
    (P           ((x_1 e_1)... (x_f (in-hole E (resolve (q (Res x_r τ)) v))) (x_n e_n)... )) 
    ((x_r v P)   ((x_1 e_1)... (x_f (in-hole E (un unit)))  (x_n e_n)... ))
    "E-Resolve")
  
   (-->
    (P   ((x_1 e_1)... (x_f (in-hole E  (wait (q (Fut x τ)))) (x_n e_n)... )))
    (P   ((x_1 e_1)... (x_f (in-hole E  v_res))  (x_n e_n)... ))
    (where v_res (find P x))
    (side-condition (not (equal? (term v_res) (term ⊥))))
    "E-Wait")

   (-->
    (P   ((x_1 e_1)... (x_f (in-hole E  (fork e)))   (x_n e_n) ... ))
    (P   ((x_new e)  (x_1 e_1)... (x_f (in-hole E  (un unit))) (x_n e_n)... ))
    (fresh x_new)
    "E-Fork")
   
   with
   ;E-Local
   [(--> (P           ((x_1 e_1)... (x_f (in-hole E a)) (x_n e_n)... ))
         (P           ((x_1 e_1)... (x_f (in-hole E b)) (x_n e_n)... )))
    (-->_loc a b)
    ]))

(test--> red
         ;Test App
         (term ( ·  ( (process1 ((un (λ (z (un Int)) z)) (un 3))  ) )))
         (term ( ·  ( (process1 (un 3)) ) )))

(test--> red
         ;Test Fork 
         (term ( ·  (   (process1 (fork (un 3)))  )))
         (term
          (·
           ((x_new (un 3))
            (process1 (un unit))))))

(test--> red
         ;Test Promise
         (term (·
                ((process1
                  (Promise (un Int))))))
         (term
          (·
           ((process1
             (lin
              (pair
               (un (Fut l (un Int)))
               (un (Res l (un Int))))))))))

(test-->> red
         (term
          ( ·  (   (process1 (let (x y) (Promise (un Int))  x))  )))
         (term
          (· ((process1
               (un (Fut l (un Int))))))))

(test-->> red
          (term  ( ·  (   (process1 (let (x y) (Promise (un Int))  (resolve y (un 3)) ))  )))
          (term 
           ((l (un 3) ·)
            ((process1 (un unit))))))

(test-->> red
          (term ( ·  (   (process1 (let (z y) (Promise (un Int))   ((un (λ (x (un Unit)) (wait z)))  (resolve y (un 3)))))  )))
          (term((l (un 3) ·)  ((process1 (un 3))))))

(traces red        
        (term
         ( ·  (   (process1 (let (z y) (Promise (un Int))   ((un (λ (x (un Unit)) (wait z)))  (resolve y (un 3)))))  )) ))