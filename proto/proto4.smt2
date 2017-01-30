(echo "Generating ARM V8 Constraints")

;(set-option :fixedpoint.engine datalog)
(set-option :fixedpoint.engine duality)
;(set-option :fixedpoint.engine pdr)

(define-sort State () (Array Int (_ BitVec 32)))
(define-sort Reg () Int)
(define-sort Flags () (_ BitVec 4))
(define-sort Cond () (_ BitVec 4))
(define-sort Opcode () Int)

(declare-var pre State)
(declare-var post State)

(declare-rel EqualAll (State State))
(rule (=> (EqualAll pre post) (= pre post)))

;EqualXpc is the relationship between two states that have identical registers from 0 to 14 and identical cpsr. So their Program counters may be different.
(declare-rel Z1 (State))
(rule (=> (Z1 pre) (= ((_ extract 1 1) (select pre 16)) #b1)))

(declare-rel EqualXpc (State State))
(rule (=> (EqualXpc pre post) (and (= (select pre 0) (select post 0)) (= (select pre 1) (select post 1)) (= (select pre 2) (select post 2)) (= (select pre 3) (select post 3)) (= (select pre 4) (select post 4)) (= (select pre 5) (select post 5)) (= (select pre 6) (select post 6)) (= (select pre 7) (select post 7)) (= (select pre 8) (select post 8)) (= (select pre 9) (select post 9)) (= (select pre 10) (select post 10)) (= (select pre 11) (select post 11)) (= (select pre 12) (select post 12)) (= (select pre 13) (select post 13)) (= (select pre 14) (select post 14)) (= (select pre 16) (select post 16)) ) ))

(declare-rel Execute (State Opcode Cond Reg Reg Reg State))
(rule (=> (and (Execute pre 0 #b0000 0 1 2 post) (= ((_ extract 1 1) (select pre 16)) #b1) ) (= (select post 0) (bvadd (select pre 1) (select pre 2))) ) )
(rule (=> (and (Execute pre 0 #b0000 0 1 2 post) (= ((_ extract 1 1) (select pre 16)) #b0)) (EqualXpc pre post)))

(rule (= #b00000000000000000000000000000001 (select pre 1)))
(rule (= #b00000000000000000000000000000001 (select pre 2)))
(rule (Z1 pre))
;(rule (= #b1 ((_ extract 1 1) (select pre 16))) )

(rule (Execute pre 0 #b0000 0 1 2 post))

(declare-rel Reg0Equal (State State))
(rule (=> (Reg0Equal pre post) (= (select pre 0) (select post 0))))


(declare-rel q1 ())
(rule (=> (EqualXpc pre post) q1))
;(eval post)
;(rule  (=> (= #b00000000000000000000000000000011 (select post 0)) q1))

(query q1)
;(check-sat)

;(get-model)

