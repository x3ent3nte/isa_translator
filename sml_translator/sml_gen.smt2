(echo "Generating ARM Constraints")
(set-option :smt.mbqi true)
(declare-datatypes () ((Operation  ADD SUB MOV MVN CMP AND)))
(declare-datatypes () ((Condition  EQ NE CS CC MI PL VS VC HI LS GE LT GT LE AL)))
(declare-datatypes () ((Flag  N S)))
(declare-datatypes () ((BarrelOp  LSL LSR ASR)))
(declare-datatypes () ((Register  R0 R1 R2 R3 R4 R5 R6 R7 R8 R9 R10 R11 R12 SP LR PC CPSR)))
(define-sort State () (Array Register (_ BitVec 32)))
(declare-datatypes () ((Instruction (mk-Instruction  (operx Operation) (condx Condition) (flagx Flag) (rdx Register) (rnx Register) (rox Register) (immx (_ BitVec 12)) (imm_usedx Bool) (barrel_opx BarrelOp) (barrel_numx (_ BitVec 32))))))
(define-sort Sequence () (Array Int State))
(define-sort Program () (Array Int Instruction))
(declare-const sequence Sequence)
(declare-const program Program)
(assert (let ( (num 0))(let ( (pre (select sequence num)) (post (select sequence (+ num 1))) (oper (operx (select program num))) (cond (condx (select program num))) (flag (flagx (select program num))) (rd (rdx (select program num))) (rn (rnx (select program num))) (ro (rox (select program num))) (imm (immx (select program num))) (imm_used (imm_usedx (select program num))) (barrel_op (barrel_opx (select program num))) (barrel_num (barrel_numx (select program num))))(and 
 (= (bvadd (select pre PC) #b00000000000000000000000000000100) (select post PC)) 
 (or 
 (and 
 (not (or 
 (and 
 (= cond EQ) (= ((_ extract 30 30) (select pre CPSR)) #b1)
) (and 
 (= cond NE) (= ((_ extract 30 30) (select pre CPSR)) #b0)
) (and 
 (= cond CS) (= ((_ extract 29 29) (select pre CPSR)) #b1)
) (and 
 (= cond CC) (= ((_ extract 29 29) (select pre CPSR)) #b0)
) (and 
 (= cond MI) (= ((_ extract 31 31) (select pre CPSR)) #b1)
) (and 
 (= cond PL) (= ((_ extract 31 31) (select pre CPSR)) #b0)
) (and 
 (= cond VS) (= ((_ extract 28 28) (select pre CPSR)) #b1)
) (and 
 (= cond VC) (= ((_ extract 28 28) (select pre CPSR)) #b0)
) (and 
 (= cond GT) (and 
 (= ((_ extract 30 30) (select pre CPSR)) #b0) (= (= ((_ extract 31 31) (select pre CPSR)) #b1) (= ((_ extract 28 28) (select pre CPSR)) #b1))
)
) (and 
 (= cond LE) (or 
 (= ((_ extract 30 30) (select pre CPSR)) #b1) (not (= (= ((_ extract 31 31) (select pre CPSR)) #b1) (= ((_ extract 28 28) (select pre CPSR)) #b1)))
)
) (= cond AL)
)) (and 
 (= (select pre R0) (select post R0)) (= (select pre R1) (select post R1)) (= (select pre R2) (select post R2)) (= (select pre R3) (select post R3)) (= (select pre R4) (select post R4)) (= (select pre R5) (select post R5)) (= (select pre R6) (select post R6)) (= (select pre R7) (select post R7)) (= (select pre R8) (select post R8)) (= (select pre R9) (select post R9)) (= (select pre R10) (select post R10)) (= (select pre R11) (select post R11)) (= (select pre R12) (select post R12)) (= (select pre SP) (select post SP)) (= (select pre LR) (select post LR))
)
) (let ( (flex_val (let ( (val_to_shift (ite imm_used ((_ rotate_right 0) (concat #b000000000000000000000000 ((_ extract 7 0) imm))) (select pre ro))))(ite (= barrel_op LSL) (bvshl val_to_shift barrel_num) (ite (= barrel_op LSR) (bvlshr val_to_shift barrel_num) (bvashr val_to_shift barrel_num))))))(and 
 (or 
 (and 
 (= cond EQ) (= ((_ extract 30 30) (select pre CPSR)) #b1)
) (and 
 (= cond NE) (= ((_ extract 30 30) (select pre CPSR)) #b0)
) (and 
 (= cond CS) (= ((_ extract 29 29) (select pre CPSR)) #b1)
) (and 
 (= cond CC) (= ((_ extract 29 29) (select pre CPSR)) #b0)
) (and 
 (= cond MI) (= ((_ extract 31 31) (select pre CPSR)) #b1)
) (and 
 (= cond PL) (= ((_ extract 31 31) (select pre CPSR)) #b0)
) (and 
 (= cond VS) (= ((_ extract 28 28) (select pre CPSR)) #b1)
) (and 
 (= cond VC) (= ((_ extract 28 28) (select pre CPSR)) #b0)
) (and 
 (= cond GT) (and 
 (= ((_ extract 30 30) (select pre CPSR)) #b0) (= (= ((_ extract 31 31) (select pre CPSR)) #b1) (= ((_ extract 28 28) (select pre CPSR)) #b1))
)
) (and 
 (= cond LE) (or 
 (= ((_ extract 30 30) (select pre CPSR)) #b1) (not (= (= ((_ extract 31 31) (select pre CPSR)) #b1) (= ((_ extract 28 28) (select pre CPSR)) #b1)))
)
) (= cond AL)
) (or 
 (and 
 (and 
 (= (select pre R0) (select post R0)) (= (select pre R1) (select post R1)) (= (select pre R2) (select post R2)) (= (select pre R3) (select post R3)) (= (select pre R4) (select post R4)) (= (select pre R5) (select post R5)) (= (select pre R6) (select post R6)) (= (select pre R7) (select post R7)) (= (select pre R8) (select post R8)) (= (select pre R9) (select post R9)) (= (select pre R10) (select post R10)) (= (select pre R11) (select post R11)) (= (select pre R12) (select post R12)) (= (select pre SP) (select post SP)) (= (select pre LR) (select post LR))
) (or 
 (= oper CMP)
) (and 
 (or 
 (not (= oper CMP)) (and 
 (or 
 (not (= (select pre rn) flex_val)) (= ((_ extract 30 30) (select post CPSR)) #b1)
) (or 
 (= (select pre rn) flex_val) (= ((_ extract 30 30) (select post CPSR)) #b0)
)
)
)
)
) (and 
 (and 
 (or 
 (= (select pre R0) (select post R0)) (= rd R0)
) (or 
 (= (select pre R1) (select post R1)) (= rd R1)
) (or 
 (= (select pre R2) (select post R2)) (= rd R2)
) (or 
 (= (select pre R3) (select post R3)) (= rd R3)
) (or 
 (= (select pre R4) (select post R4)) (= rd R4)
) (or 
 (= (select pre R5) (select post R5)) (= rd R5)
) (or 
 (= (select pre R6) (select post R6)) (= rd R6)
) (or 
 (= (select pre R7) (select post R7)) (= rd R7)
) (or 
 (= (select pre R8) (select post R8)) (= rd R8)
) (or 
 (= (select pre R9) (select post R9)) (= rd R9)
) (or 
 (= (select pre R10) (select post R10)) (= rd R10)
) (or 
 (= (select pre R11) (select post R11)) (= rd R11)
) (or 
 (= (select pre R12) (select post R12)) (= rd R12)
) (or 
 (= (select pre SP) (select post SP)) (= rd SP)
) (or 
 (= (select pre LR) (select post LR)) (= rd LR)
)
) (or 
 (and 
 (or 
 (= oper MOV) (= oper MVN)
) (or 
 (not (= oper MOV)) (= (select post rd) flex_val)
) (or 
 (not (= oper MVN)) (= (select post rd) (bvnot flex_val))
)
) (and 
 (or 
 (= oper ADD) (= oper SUB) (= oper AND)
) (or 
 (not (= oper ADD)) (= (select post rd) (bvadd (select pre rn) flex_val))
) (or 
 (not (= oper SUB)) (= (select post rd) (bvsub (select pre rn) flex_val))
) (or 
 (not (= oper AND)) (= (select post rd) (bvand (select pre rn) flex_val))
)
)
) (or 
 (and 
 (= flag N) (= (select pre CPSR) (select post CPSR))
) (and 
 (= flag S) (or 
 (not (= (select post rd) #b00000000000000000000000000000000)) (= ((_ extract 30 30) (select post CPSR)) #b1)
) (or 
 (= (select post rd) #b00000000000000000000000000000000) (= ((_ extract 30 30) (select post CPSR)) #b0)
)
)
)
)
)
))
) 

))))(assert (let ( (num 1))(let ( (pre (select sequence num)) (post (select sequence (+ num 1))) (oper (operx (select program num))) (cond (condx (select program num))) (flag (flagx (select program num))) (rd (rdx (select program num))) (rn (rnx (select program num))) (ro (rox (select program num))) (imm (immx (select program num))) (imm_used (imm_usedx (select program num))) (barrel_op (barrel_opx (select program num))) (barrel_num (barrel_numx (select program num))))(and 
 (= (bvadd (select pre PC) #b00000000000000000000000000000100) (select post PC)) 
 (or 
 (and 
 (not (or 
 (and 
 (= cond EQ) (= ((_ extract 30 30) (select pre CPSR)) #b1)
) (and 
 (= cond NE) (= ((_ extract 30 30) (select pre CPSR)) #b0)
) (and 
 (= cond CS) (= ((_ extract 29 29) (select pre CPSR)) #b1)
) (and 
 (= cond CC) (= ((_ extract 29 29) (select pre CPSR)) #b0)
) (and 
 (= cond MI) (= ((_ extract 31 31) (select pre CPSR)) #b1)
) (and 
 (= cond PL) (= ((_ extract 31 31) (select pre CPSR)) #b0)
) (and 
 (= cond VS) (= ((_ extract 28 28) (select pre CPSR)) #b1)
) (and 
 (= cond VC) (= ((_ extract 28 28) (select pre CPSR)) #b0)
) (and 
 (= cond GT) (and 
 (= ((_ extract 30 30) (select pre CPSR)) #b0) (= (= ((_ extract 31 31) (select pre CPSR)) #b1) (= ((_ extract 28 28) (select pre CPSR)) #b1))
)
) (and 
 (= cond LE) (or 
 (= ((_ extract 30 30) (select pre CPSR)) #b1) (not (= (= ((_ extract 31 31) (select pre CPSR)) #b1) (= ((_ extract 28 28) (select pre CPSR)) #b1)))
)
) (= cond AL)
)) (and 
 (= (select pre R0) (select post R0)) (= (select pre R1) (select post R1)) (= (select pre R2) (select post R2)) (= (select pre R3) (select post R3)) (= (select pre R4) (select post R4)) (= (select pre R5) (select post R5)) (= (select pre R6) (select post R6)) (= (select pre R7) (select post R7)) (= (select pre R8) (select post R8)) (= (select pre R9) (select post R9)) (= (select pre R10) (select post R10)) (= (select pre R11) (select post R11)) (= (select pre R12) (select post R12)) (= (select pre SP) (select post SP)) (= (select pre LR) (select post LR))
)
) (let ( (flex_val (let ( (val_to_shift (ite imm_used ((_ rotate_right 0) (concat #b000000000000000000000000 ((_ extract 7 0) imm))) (select pre ro))))(ite (= barrel_op LSL) (bvshl val_to_shift barrel_num) (ite (= barrel_op LSR) (bvlshr val_to_shift barrel_num) (bvashr val_to_shift barrel_num))))))(and 
 (or 
 (and 
 (= cond EQ) (= ((_ extract 30 30) (select pre CPSR)) #b1)
) (and 
 (= cond NE) (= ((_ extract 30 30) (select pre CPSR)) #b0)
) (and 
 (= cond CS) (= ((_ extract 29 29) (select pre CPSR)) #b1)
) (and 
 (= cond CC) (= ((_ extract 29 29) (select pre CPSR)) #b0)
) (and 
 (= cond MI) (= ((_ extract 31 31) (select pre CPSR)) #b1)
) (and 
 (= cond PL) (= ((_ extract 31 31) (select pre CPSR)) #b0)
) (and 
 (= cond VS) (= ((_ extract 28 28) (select pre CPSR)) #b1)
) (and 
 (= cond VC) (= ((_ extract 28 28) (select pre CPSR)) #b0)
) (and 
 (= cond GT) (and 
 (= ((_ extract 30 30) (select pre CPSR)) #b0) (= (= ((_ extract 31 31) (select pre CPSR)) #b1) (= ((_ extract 28 28) (select pre CPSR)) #b1))
)
) (and 
 (= cond LE) (or 
 (= ((_ extract 30 30) (select pre CPSR)) #b1) (not (= (= ((_ extract 31 31) (select pre CPSR)) #b1) (= ((_ extract 28 28) (select pre CPSR)) #b1)))
)
) (= cond AL)
) (or 
 (and 
 (and 
 (= (select pre R0) (select post R0)) (= (select pre R1) (select post R1)) (= (select pre R2) (select post R2)) (= (select pre R3) (select post R3)) (= (select pre R4) (select post R4)) (= (select pre R5) (select post R5)) (= (select pre R6) (select post R6)) (= (select pre R7) (select post R7)) (= (select pre R8) (select post R8)) (= (select pre R9) (select post R9)) (= (select pre R10) (select post R10)) (= (select pre R11) (select post R11)) (= (select pre R12) (select post R12)) (= (select pre SP) (select post SP)) (= (select pre LR) (select post LR))
) (or 
 (= oper CMP)
) (and 
 (or 
 (not (= oper CMP)) (and 
 (or 
 (not (= (select pre rn) flex_val)) (= ((_ extract 30 30) (select post CPSR)) #b1)
) (or 
 (= (select pre rn) flex_val) (= ((_ extract 30 30) (select post CPSR)) #b0)
)
)
)
)
) (and 
 (and 
 (or 
 (= (select pre R0) (select post R0)) (= rd R0)
) (or 
 (= (select pre R1) (select post R1)) (= rd R1)
) (or 
 (= (select pre R2) (select post R2)) (= rd R2)
) (or 
 (= (select pre R3) (select post R3)) (= rd R3)
) (or 
 (= (select pre R4) (select post R4)) (= rd R4)
) (or 
 (= (select pre R5) (select post R5)) (= rd R5)
) (or 
 (= (select pre R6) (select post R6)) (= rd R6)
) (or 
 (= (select pre R7) (select post R7)) (= rd R7)
) (or 
 (= (select pre R8) (select post R8)) (= rd R8)
) (or 
 (= (select pre R9) (select post R9)) (= rd R9)
) (or 
 (= (select pre R10) (select post R10)) (= rd R10)
) (or 
 (= (select pre R11) (select post R11)) (= rd R11)
) (or 
 (= (select pre R12) (select post R12)) (= rd R12)
) (or 
 (= (select pre SP) (select post SP)) (= rd SP)
) (or 
 (= (select pre LR) (select post LR)) (= rd LR)
)
) (or 
 (and 
 (or 
 (= oper MOV) (= oper MVN)
) (or 
 (not (= oper MOV)) (= (select post rd) flex_val)
) (or 
 (not (= oper MVN)) (= (select post rd) (bvnot flex_val))
)
) (and 
 (or 
 (= oper ADD) (= oper SUB) (= oper AND)
) (or 
 (not (= oper ADD)) (= (select post rd) (bvadd (select pre rn) flex_val))
) (or 
 (not (= oper SUB)) (= (select post rd) (bvsub (select pre rn) flex_val))
) (or 
 (not (= oper AND)) (= (select post rd) (bvand (select pre rn) flex_val))
)
)
) (or 
 (and 
 (= flag N) (= (select pre CPSR) (select post CPSR))
) (and 
 (= flag S) (or 
 (not (= (select post rd) #b00000000000000000000000000000000)) (= ((_ extract 30 30) (select post CPSR)) #b1)
) (or 
 (= (select post rd) #b00000000000000000000000000000000) (= ((_ extract 30 30) (select post CPSR)) #b0)
)
)
)
)
)
))
) 

))))(assert (let ( (num 2))(let ( (pre (select sequence num)) (post (select sequence (+ num 1))) (oper (operx (select program num))) (cond (condx (select program num))) (flag (flagx (select program num))) (rd (rdx (select program num))) (rn (rnx (select program num))) (ro (rox (select program num))) (imm (immx (select program num))) (imm_used (imm_usedx (select program num))) (barrel_op (barrel_opx (select program num))) (barrel_num (barrel_numx (select program num))))(and 
 (= (bvadd (select pre PC) #b00000000000000000000000000000100) (select post PC)) 
 (or 
 (and 
 (not (or 
 (and 
 (= cond EQ) (= ((_ extract 30 30) (select pre CPSR)) #b1)
) (and 
 (= cond NE) (= ((_ extract 30 30) (select pre CPSR)) #b0)
) (and 
 (= cond CS) (= ((_ extract 29 29) (select pre CPSR)) #b1)
) (and 
 (= cond CC) (= ((_ extract 29 29) (select pre CPSR)) #b0)
) (and 
 (= cond MI) (= ((_ extract 31 31) (select pre CPSR)) #b1)
) (and 
 (= cond PL) (= ((_ extract 31 31) (select pre CPSR)) #b0)
) (and 
 (= cond VS) (= ((_ extract 28 28) (select pre CPSR)) #b1)
) (and 
 (= cond VC) (= ((_ extract 28 28) (select pre CPSR)) #b0)
) (and 
 (= cond GT) (and 
 (= ((_ extract 30 30) (select pre CPSR)) #b0) (= (= ((_ extract 31 31) (select pre CPSR)) #b1) (= ((_ extract 28 28) (select pre CPSR)) #b1))
)
) (and 
 (= cond LE) (or 
 (= ((_ extract 30 30) (select pre CPSR)) #b1) (not (= (= ((_ extract 31 31) (select pre CPSR)) #b1) (= ((_ extract 28 28) (select pre CPSR)) #b1)))
)
) (= cond AL)
)) (and 
 (= (select pre R0) (select post R0)) (= (select pre R1) (select post R1)) (= (select pre R2) (select post R2)) (= (select pre R3) (select post R3)) (= (select pre R4) (select post R4)) (= (select pre R5) (select post R5)) (= (select pre R6) (select post R6)) (= (select pre R7) (select post R7)) (= (select pre R8) (select post R8)) (= (select pre R9) (select post R9)) (= (select pre R10) (select post R10)) (= (select pre R11) (select post R11)) (= (select pre R12) (select post R12)) (= (select pre SP) (select post SP)) (= (select pre LR) (select post LR))
)
) (let ( (flex_val (let ( (val_to_shift (ite imm_used ((_ rotate_right 0) (concat #b000000000000000000000000 ((_ extract 7 0) imm))) (select pre ro))))(ite (= barrel_op LSL) (bvshl val_to_shift barrel_num) (ite (= barrel_op LSR) (bvlshr val_to_shift barrel_num) (bvashr val_to_shift barrel_num))))))(and 
 (or 
 (and 
 (= cond EQ) (= ((_ extract 30 30) (select pre CPSR)) #b1)
) (and 
 (= cond NE) (= ((_ extract 30 30) (select pre CPSR)) #b0)
) (and 
 (= cond CS) (= ((_ extract 29 29) (select pre CPSR)) #b1)
) (and 
 (= cond CC) (= ((_ extract 29 29) (select pre CPSR)) #b0)
) (and 
 (= cond MI) (= ((_ extract 31 31) (select pre CPSR)) #b1)
) (and 
 (= cond PL) (= ((_ extract 31 31) (select pre CPSR)) #b0)
) (and 
 (= cond VS) (= ((_ extract 28 28) (select pre CPSR)) #b1)
) (and 
 (= cond VC) (= ((_ extract 28 28) (select pre CPSR)) #b0)
) (and 
 (= cond GT) (and 
 (= ((_ extract 30 30) (select pre CPSR)) #b0) (= (= ((_ extract 31 31) (select pre CPSR)) #b1) (= ((_ extract 28 28) (select pre CPSR)) #b1))
)
) (and 
 (= cond LE) (or 
 (= ((_ extract 30 30) (select pre CPSR)) #b1) (not (= (= ((_ extract 31 31) (select pre CPSR)) #b1) (= ((_ extract 28 28) (select pre CPSR)) #b1)))
)
) (= cond AL)
) (or 
 (and 
 (and 
 (= (select pre R0) (select post R0)) (= (select pre R1) (select post R1)) (= (select pre R2) (select post R2)) (= (select pre R3) (select post R3)) (= (select pre R4) (select post R4)) (= (select pre R5) (select post R5)) (= (select pre R6) (select post R6)) (= (select pre R7) (select post R7)) (= (select pre R8) (select post R8)) (= (select pre R9) (select post R9)) (= (select pre R10) (select post R10)) (= (select pre R11) (select post R11)) (= (select pre R12) (select post R12)) (= (select pre SP) (select post SP)) (= (select pre LR) (select post LR))
) (or 
 (= oper CMP)
) (and 
 (or 
 (not (= oper CMP)) (and 
 (or 
 (not (= (select pre rn) flex_val)) (= ((_ extract 30 30) (select post CPSR)) #b1)
) (or 
 (= (select pre rn) flex_val) (= ((_ extract 30 30) (select post CPSR)) #b0)
)
)
)
)
) (and 
 (and 
 (or 
 (= (select pre R0) (select post R0)) (= rd R0)
) (or 
 (= (select pre R1) (select post R1)) (= rd R1)
) (or 
 (= (select pre R2) (select post R2)) (= rd R2)
) (or 
 (= (select pre R3) (select post R3)) (= rd R3)
) (or 
 (= (select pre R4) (select post R4)) (= rd R4)
) (or 
 (= (select pre R5) (select post R5)) (= rd R5)
) (or 
 (= (select pre R6) (select post R6)) (= rd R6)
) (or 
 (= (select pre R7) (select post R7)) (= rd R7)
) (or 
 (= (select pre R8) (select post R8)) (= rd R8)
) (or 
 (= (select pre R9) (select post R9)) (= rd R9)
) (or 
 (= (select pre R10) (select post R10)) (= rd R10)
) (or 
 (= (select pre R11) (select post R11)) (= rd R11)
) (or 
 (= (select pre R12) (select post R12)) (= rd R12)
) (or 
 (= (select pre SP) (select post SP)) (= rd SP)
) (or 
 (= (select pre LR) (select post LR)) (= rd LR)
)
) (or 
 (and 
 (or 
 (= oper MOV) (= oper MVN)
) (or 
 (not (= oper MOV)) (= (select post rd) flex_val)
) (or 
 (not (= oper MVN)) (= (select post rd) (bvnot flex_val))
)
) (and 
 (or 
 (= oper ADD) (= oper SUB) (= oper AND)
) (or 
 (not (= oper ADD)) (= (select post rd) (bvadd (select pre rn) flex_val))
) (or 
 (not (= oper SUB)) (= (select post rd) (bvsub (select pre rn) flex_val))
) (or 
 (not (= oper AND)) (= (select post rd) (bvand (select pre rn) flex_val))
)
)
) (or 
 (and 
 (= flag N) (= (select pre CPSR) (select post CPSR))
) (and 
 (= flag S) (or 
 (not (= (select post rd) #b00000000000000000000000000000000)) (= ((_ extract 30 30) (select post CPSR)) #b1)
) (or 
 (= (select post rd) #b00000000000000000000000000000000) (= ((_ extract 30 30) (select post CPSR)) #b0)
)
)
)
)
)
))
) 

))))(assert (let ( (num 3))(let ( (pre (select sequence num)) (post (select sequence (+ num 1))) (oper (operx (select program num))) (cond (condx (select program num))) (flag (flagx (select program num))) (rd (rdx (select program num))) (rn (rnx (select program num))) (ro (rox (select program num))) (imm (immx (select program num))) (imm_used (imm_usedx (select program num))) (barrel_op (barrel_opx (select program num))) (barrel_num (barrel_numx (select program num))))(and 
 (= (bvadd (select pre PC) #b00000000000000000000000000000100) (select post PC)) 
 (or 
 (and 
 (not (or 
 (and 
 (= cond EQ) (= ((_ extract 30 30) (select pre CPSR)) #b1)
) (and 
 (= cond NE) (= ((_ extract 30 30) (select pre CPSR)) #b0)
) (and 
 (= cond CS) (= ((_ extract 29 29) (select pre CPSR)) #b1)
) (and 
 (= cond CC) (= ((_ extract 29 29) (select pre CPSR)) #b0)
) (and 
 (= cond MI) (= ((_ extract 31 31) (select pre CPSR)) #b1)
) (and 
 (= cond PL) (= ((_ extract 31 31) (select pre CPSR)) #b0)
) (and 
 (= cond VS) (= ((_ extract 28 28) (select pre CPSR)) #b1)
) (and 
 (= cond VC) (= ((_ extract 28 28) (select pre CPSR)) #b0)
) (and 
 (= cond GT) (and 
 (= ((_ extract 30 30) (select pre CPSR)) #b0) (= (= ((_ extract 31 31) (select pre CPSR)) #b1) (= ((_ extract 28 28) (select pre CPSR)) #b1))
)
) (and 
 (= cond LE) (or 
 (= ((_ extract 30 30) (select pre CPSR)) #b1) (not (= (= ((_ extract 31 31) (select pre CPSR)) #b1) (= ((_ extract 28 28) (select pre CPSR)) #b1)))
)
) (= cond AL)
)) (and 
 (= (select pre R0) (select post R0)) (= (select pre R1) (select post R1)) (= (select pre R2) (select post R2)) (= (select pre R3) (select post R3)) (= (select pre R4) (select post R4)) (= (select pre R5) (select post R5)) (= (select pre R6) (select post R6)) (= (select pre R7) (select post R7)) (= (select pre R8) (select post R8)) (= (select pre R9) (select post R9)) (= (select pre R10) (select post R10)) (= (select pre R11) (select post R11)) (= (select pre R12) (select post R12)) (= (select pre SP) (select post SP)) (= (select pre LR) (select post LR))
)
) (let ( (flex_val (let ( (val_to_shift (ite imm_used ((_ rotate_right 0) (concat #b000000000000000000000000 ((_ extract 7 0) imm))) (select pre ro))))(ite (= barrel_op LSL) (bvshl val_to_shift barrel_num) (ite (= barrel_op LSR) (bvlshr val_to_shift barrel_num) (bvashr val_to_shift barrel_num))))))(and 
 (or 
 (and 
 (= cond EQ) (= ((_ extract 30 30) (select pre CPSR)) #b1)
) (and 
 (= cond NE) (= ((_ extract 30 30) (select pre CPSR)) #b0)
) (and 
 (= cond CS) (= ((_ extract 29 29) (select pre CPSR)) #b1)
) (and 
 (= cond CC) (= ((_ extract 29 29) (select pre CPSR)) #b0)
) (and 
 (= cond MI) (= ((_ extract 31 31) (select pre CPSR)) #b1)
) (and 
 (= cond PL) (= ((_ extract 31 31) (select pre CPSR)) #b0)
) (and 
 (= cond VS) (= ((_ extract 28 28) (select pre CPSR)) #b1)
) (and 
 (= cond VC) (= ((_ extract 28 28) (select pre CPSR)) #b0)
) (and 
 (= cond GT) (and 
 (= ((_ extract 30 30) (select pre CPSR)) #b0) (= (= ((_ extract 31 31) (select pre CPSR)) #b1) (= ((_ extract 28 28) (select pre CPSR)) #b1))
)
) (and 
 (= cond LE) (or 
 (= ((_ extract 30 30) (select pre CPSR)) #b1) (not (= (= ((_ extract 31 31) (select pre CPSR)) #b1) (= ((_ extract 28 28) (select pre CPSR)) #b1)))
)
) (= cond AL)
) (or 
 (and 
 (and 
 (= (select pre R0) (select post R0)) (= (select pre R1) (select post R1)) (= (select pre R2) (select post R2)) (= (select pre R3) (select post R3)) (= (select pre R4) (select post R4)) (= (select pre R5) (select post R5)) (= (select pre R6) (select post R6)) (= (select pre R7) (select post R7)) (= (select pre R8) (select post R8)) (= (select pre R9) (select post R9)) (= (select pre R10) (select post R10)) (= (select pre R11) (select post R11)) (= (select pre R12) (select post R12)) (= (select pre SP) (select post SP)) (= (select pre LR) (select post LR))
) (or 
 (= oper CMP)
) (and 
 (or 
 (not (= oper CMP)) (and 
 (or 
 (not (= (select pre rn) flex_val)) (= ((_ extract 30 30) (select post CPSR)) #b1)
) (or 
 (= (select pre rn) flex_val) (= ((_ extract 30 30) (select post CPSR)) #b0)
)
)
)
)
) (and 
 (and 
 (or 
 (= (select pre R0) (select post R0)) (= rd R0)
) (or 
 (= (select pre R1) (select post R1)) (= rd R1)
) (or 
 (= (select pre R2) (select post R2)) (= rd R2)
) (or 
 (= (select pre R3) (select post R3)) (= rd R3)
) (or 
 (= (select pre R4) (select post R4)) (= rd R4)
) (or 
 (= (select pre R5) (select post R5)) (= rd R5)
) (or 
 (= (select pre R6) (select post R6)) (= rd R6)
) (or 
 (= (select pre R7) (select post R7)) (= rd R7)
) (or 
 (= (select pre R8) (select post R8)) (= rd R8)
) (or 
 (= (select pre R9) (select post R9)) (= rd R9)
) (or 
 (= (select pre R10) (select post R10)) (= rd R10)
) (or 
 (= (select pre R11) (select post R11)) (= rd R11)
) (or 
 (= (select pre R12) (select post R12)) (= rd R12)
) (or 
 (= (select pre SP) (select post SP)) (= rd SP)
) (or 
 (= (select pre LR) (select post LR)) (= rd LR)
)
) (or 
 (and 
 (or 
 (= oper MOV) (= oper MVN)
) (or 
 (not (= oper MOV)) (= (select post rd) flex_val)
) (or 
 (not (= oper MVN)) (= (select post rd) (bvnot flex_val))
)
) (and 
 (or 
 (= oper ADD) (= oper SUB) (= oper AND)
) (or 
 (not (= oper ADD)) (= (select post rd) (bvadd (select pre rn) flex_val))
) (or 
 (not (= oper SUB)) (= (select post rd) (bvsub (select pre rn) flex_val))
) (or 
 (not (= oper AND)) (= (select post rd) (bvand (select pre rn) flex_val))
)
)
) (or 
 (and 
 (= flag N) (= (select pre CPSR) (select post CPSR))
) (and 
 (= flag S) (or 
 (not (= (select post rd) #b00000000000000000000000000000000)) (= ((_ extract 30 30) (select post CPSR)) #b1)
) (or 
 (= (select post rd) #b00000000000000000000000000000000) (= ((_ extract 30 30) (select post CPSR)) #b0)
)
)
)
)
)
))
) 

))))(assert (let ( (num 4))(let ( (pre (select sequence num)) (post (select sequence (+ num 1))) (oper (operx (select program num))) (cond (condx (select program num))) (flag (flagx (select program num))) (rd (rdx (select program num))) (rn (rnx (select program num))) (ro (rox (select program num))) (imm (immx (select program num))) (imm_used (imm_usedx (select program num))) (barrel_op (barrel_opx (select program num))) (barrel_num (barrel_numx (select program num))))(and 
 (= (bvadd (select pre PC) #b00000000000000000000000000000100) (select post PC)) 
 (or 
 (and 
 (not (or 
 (and 
 (= cond EQ) (= ((_ extract 30 30) (select pre CPSR)) #b1)
) (and 
 (= cond NE) (= ((_ extract 30 30) (select pre CPSR)) #b0)
) (and 
 (= cond CS) (= ((_ extract 29 29) (select pre CPSR)) #b1)
) (and 
 (= cond CC) (= ((_ extract 29 29) (select pre CPSR)) #b0)
) (and 
 (= cond MI) (= ((_ extract 31 31) (select pre CPSR)) #b1)
) (and 
 (= cond PL) (= ((_ extract 31 31) (select pre CPSR)) #b0)
) (and 
 (= cond VS) (= ((_ extract 28 28) (select pre CPSR)) #b1)
) (and 
 (= cond VC) (= ((_ extract 28 28) (select pre CPSR)) #b0)
) (and 
 (= cond GT) (and 
 (= ((_ extract 30 30) (select pre CPSR)) #b0) (= (= ((_ extract 31 31) (select pre CPSR)) #b1) (= ((_ extract 28 28) (select pre CPSR)) #b1))
)
) (and 
 (= cond LE) (or 
 (= ((_ extract 30 30) (select pre CPSR)) #b1) (not (= (= ((_ extract 31 31) (select pre CPSR)) #b1) (= ((_ extract 28 28) (select pre CPSR)) #b1)))
)
) (= cond AL)
)) (and 
 (= (select pre R0) (select post R0)) (= (select pre R1) (select post R1)) (= (select pre R2) (select post R2)) (= (select pre R3) (select post R3)) (= (select pre R4) (select post R4)) (= (select pre R5) (select post R5)) (= (select pre R6) (select post R6)) (= (select pre R7) (select post R7)) (= (select pre R8) (select post R8)) (= (select pre R9) (select post R9)) (= (select pre R10) (select post R10)) (= (select pre R11) (select post R11)) (= (select pre R12) (select post R12)) (= (select pre SP) (select post SP)) (= (select pre LR) (select post LR))
)
) (let ( (flex_val (let ( (val_to_shift (ite imm_used ((_ rotate_right 0) (concat #b000000000000000000000000 ((_ extract 7 0) imm))) (select pre ro))))(ite (= barrel_op LSL) (bvshl val_to_shift barrel_num) (ite (= barrel_op LSR) (bvlshr val_to_shift barrel_num) (bvashr val_to_shift barrel_num))))))(and 
 (or 
 (and 
 (= cond EQ) (= ((_ extract 30 30) (select pre CPSR)) #b1)
) (and 
 (= cond NE) (= ((_ extract 30 30) (select pre CPSR)) #b0)
) (and 
 (= cond CS) (= ((_ extract 29 29) (select pre CPSR)) #b1)
) (and 
 (= cond CC) (= ((_ extract 29 29) (select pre CPSR)) #b0)
) (and 
 (= cond MI) (= ((_ extract 31 31) (select pre CPSR)) #b1)
) (and 
 (= cond PL) (= ((_ extract 31 31) (select pre CPSR)) #b0)
) (and 
 (= cond VS) (= ((_ extract 28 28) (select pre CPSR)) #b1)
) (and 
 (= cond VC) (= ((_ extract 28 28) (select pre CPSR)) #b0)
) (and 
 (= cond GT) (and 
 (= ((_ extract 30 30) (select pre CPSR)) #b0) (= (= ((_ extract 31 31) (select pre CPSR)) #b1) (= ((_ extract 28 28) (select pre CPSR)) #b1))
)
) (and 
 (= cond LE) (or 
 (= ((_ extract 30 30) (select pre CPSR)) #b1) (not (= (= ((_ extract 31 31) (select pre CPSR)) #b1) (= ((_ extract 28 28) (select pre CPSR)) #b1)))
)
) (= cond AL)
) (or 
 (and 
 (and 
 (= (select pre R0) (select post R0)) (= (select pre R1) (select post R1)) (= (select pre R2) (select post R2)) (= (select pre R3) (select post R3)) (= (select pre R4) (select post R4)) (= (select pre R5) (select post R5)) (= (select pre R6) (select post R6)) (= (select pre R7) (select post R7)) (= (select pre R8) (select post R8)) (= (select pre R9) (select post R9)) (= (select pre R10) (select post R10)) (= (select pre R11) (select post R11)) (= (select pre R12) (select post R12)) (= (select pre SP) (select post SP)) (= (select pre LR) (select post LR))
) (or 
 (= oper CMP)
) (and 
 (or 
 (not (= oper CMP)) (and 
 (or 
 (not (= (select pre rn) flex_val)) (= ((_ extract 30 30) (select post CPSR)) #b1)
) (or 
 (= (select pre rn) flex_val) (= ((_ extract 30 30) (select post CPSR)) #b0)
)
)
)
)
) (and 
 (and 
 (or 
 (= (select pre R0) (select post R0)) (= rd R0)
) (or 
 (= (select pre R1) (select post R1)) (= rd R1)
) (or 
 (= (select pre R2) (select post R2)) (= rd R2)
) (or 
 (= (select pre R3) (select post R3)) (= rd R3)
) (or 
 (= (select pre R4) (select post R4)) (= rd R4)
) (or 
 (= (select pre R5) (select post R5)) (= rd R5)
) (or 
 (= (select pre R6) (select post R6)) (= rd R6)
) (or 
 (= (select pre R7) (select post R7)) (= rd R7)
) (or 
 (= (select pre R8) (select post R8)) (= rd R8)
) (or 
 (= (select pre R9) (select post R9)) (= rd R9)
) (or 
 (= (select pre R10) (select post R10)) (= rd R10)
) (or 
 (= (select pre R11) (select post R11)) (= rd R11)
) (or 
 (= (select pre R12) (select post R12)) (= rd R12)
) (or 
 (= (select pre SP) (select post SP)) (= rd SP)
) (or 
 (= (select pre LR) (select post LR)) (= rd LR)
)
) (or 
 (and 
 (or 
 (= oper MOV) (= oper MVN)
) (or 
 (not (= oper MOV)) (= (select post rd) flex_val)
) (or 
 (not (= oper MVN)) (= (select post rd) (bvnot flex_val))
)
) (and 
 (or 
 (= oper ADD) (= oper SUB) (= oper AND)
) (or 
 (not (= oper ADD)) (= (select post rd) (bvadd (select pre rn) flex_val))
) (or 
 (not (= oper SUB)) (= (select post rd) (bvsub (select pre rn) flex_val))
) (or 
 (not (= oper AND)) (= (select post rd) (bvand (select pre rn) flex_val))
)
)
) (or 
 (and 
 (= flag N) (= (select pre CPSR) (select post CPSR))
) (and 
 (= flag S) (or 
 (not (= (select post rd) #b00000000000000000000000000000000)) (= ((_ extract 30 30) (select post CPSR)) #b1)
) (or 
 (= (select post rd) #b00000000000000000000000000000000) (= ((_ extract 30 30) (select post CPSR)) #b0)
)
)
)
)
)
))
) 

))))(assert (let ( (num 5))(let ( (pre (select sequence num)) (post (select sequence (+ num 1))) (oper (operx (select program num))) (cond (condx (select program num))) (flag (flagx (select program num))) (rd (rdx (select program num))) (rn (rnx (select program num))) (ro (rox (select program num))) (imm (immx (select program num))) (imm_used (imm_usedx (select program num))) (barrel_op (barrel_opx (select program num))) (barrel_num (barrel_numx (select program num))))(and 
 (= (bvadd (select pre PC) #b00000000000000000000000000000100) (select post PC)) 
 (or 
 (and 
 (not (or 
 (and 
 (= cond EQ) (= ((_ extract 30 30) (select pre CPSR)) #b1)
) (and 
 (= cond NE) (= ((_ extract 30 30) (select pre CPSR)) #b0)
) (and 
 (= cond CS) (= ((_ extract 29 29) (select pre CPSR)) #b1)
) (and 
 (= cond CC) (= ((_ extract 29 29) (select pre CPSR)) #b0)
) (and 
 (= cond MI) (= ((_ extract 31 31) (select pre CPSR)) #b1)
) (and 
 (= cond PL) (= ((_ extract 31 31) (select pre CPSR)) #b0)
) (and 
 (= cond VS) (= ((_ extract 28 28) (select pre CPSR)) #b1)
) (and 
 (= cond VC) (= ((_ extract 28 28) (select pre CPSR)) #b0)
) (and 
 (= cond GT) (and 
 (= ((_ extract 30 30) (select pre CPSR)) #b0) (= (= ((_ extract 31 31) (select pre CPSR)) #b1) (= ((_ extract 28 28) (select pre CPSR)) #b1))
)
) (and 
 (= cond LE) (or 
 (= ((_ extract 30 30) (select pre CPSR)) #b1) (not (= (= ((_ extract 31 31) (select pre CPSR)) #b1) (= ((_ extract 28 28) (select pre CPSR)) #b1)))
)
) (= cond AL)
)) (and 
 (= (select pre R0) (select post R0)) (= (select pre R1) (select post R1)) (= (select pre R2) (select post R2)) (= (select pre R3) (select post R3)) (= (select pre R4) (select post R4)) (= (select pre R5) (select post R5)) (= (select pre R6) (select post R6)) (= (select pre R7) (select post R7)) (= (select pre R8) (select post R8)) (= (select pre R9) (select post R9)) (= (select pre R10) (select post R10)) (= (select pre R11) (select post R11)) (= (select pre R12) (select post R12)) (= (select pre SP) (select post SP)) (= (select pre LR) (select post LR))
)
) (let ( (flex_val (let ( (val_to_shift (ite imm_used ((_ rotate_right 0) (concat #b000000000000000000000000 ((_ extract 7 0) imm))) (select pre ro))))(ite (= barrel_op LSL) (bvshl val_to_shift barrel_num) (ite (= barrel_op LSR) (bvlshr val_to_shift barrel_num) (bvashr val_to_shift barrel_num))))))(and 
 (or 
 (and 
 (= cond EQ) (= ((_ extract 30 30) (select pre CPSR)) #b1)
) (and 
 (= cond NE) (= ((_ extract 30 30) (select pre CPSR)) #b0)
) (and 
 (= cond CS) (= ((_ extract 29 29) (select pre CPSR)) #b1)
) (and 
 (= cond CC) (= ((_ extract 29 29) (select pre CPSR)) #b0)
) (and 
 (= cond MI) (= ((_ extract 31 31) (select pre CPSR)) #b1)
) (and 
 (= cond PL) (= ((_ extract 31 31) (select pre CPSR)) #b0)
) (and 
 (= cond VS) (= ((_ extract 28 28) (select pre CPSR)) #b1)
) (and 
 (= cond VC) (= ((_ extract 28 28) (select pre CPSR)) #b0)
) (and 
 (= cond GT) (and 
 (= ((_ extract 30 30) (select pre CPSR)) #b0) (= (= ((_ extract 31 31) (select pre CPSR)) #b1) (= ((_ extract 28 28) (select pre CPSR)) #b1))
)
) (and 
 (= cond LE) (or 
 (= ((_ extract 30 30) (select pre CPSR)) #b1) (not (= (= ((_ extract 31 31) (select pre CPSR)) #b1) (= ((_ extract 28 28) (select pre CPSR)) #b1)))
)
) (= cond AL)
) (or 
 (and 
 (and 
 (= (select pre R0) (select post R0)) (= (select pre R1) (select post R1)) (= (select pre R2) (select post R2)) (= (select pre R3) (select post R3)) (= (select pre R4) (select post R4)) (= (select pre R5) (select post R5)) (= (select pre R6) (select post R6)) (= (select pre R7) (select post R7)) (= (select pre R8) (select post R8)) (= (select pre R9) (select post R9)) (= (select pre R10) (select post R10)) (= (select pre R11) (select post R11)) (= (select pre R12) (select post R12)) (= (select pre SP) (select post SP)) (= (select pre LR) (select post LR))
) (or 
 (= oper CMP)
) (and 
 (or 
 (not (= oper CMP)) (and 
 (or 
 (not (= (select pre rn) flex_val)) (= ((_ extract 30 30) (select post CPSR)) #b1)
) (or 
 (= (select pre rn) flex_val) (= ((_ extract 30 30) (select post CPSR)) #b0)
)
)
)
)
) (and 
 (and 
 (or 
 (= (select pre R0) (select post R0)) (= rd R0)
) (or 
 (= (select pre R1) (select post R1)) (= rd R1)
) (or 
 (= (select pre R2) (select post R2)) (= rd R2)
) (or 
 (= (select pre R3) (select post R3)) (= rd R3)
) (or 
 (= (select pre R4) (select post R4)) (= rd R4)
) (or 
 (= (select pre R5) (select post R5)) (= rd R5)
) (or 
 (= (select pre R6) (select post R6)) (= rd R6)
) (or 
 (= (select pre R7) (select post R7)) (= rd R7)
) (or 
 (= (select pre R8) (select post R8)) (= rd R8)
) (or 
 (= (select pre R9) (select post R9)) (= rd R9)
) (or 
 (= (select pre R10) (select post R10)) (= rd R10)
) (or 
 (= (select pre R11) (select post R11)) (= rd R11)
) (or 
 (= (select pre R12) (select post R12)) (= rd R12)
) (or 
 (= (select pre SP) (select post SP)) (= rd SP)
) (or 
 (= (select pre LR) (select post LR)) (= rd LR)
)
) (or 
 (and 
 (or 
 (= oper MOV) (= oper MVN)
) (or 
 (not (= oper MOV)) (= (select post rd) flex_val)
) (or 
 (not (= oper MVN)) (= (select post rd) (bvnot flex_val))
)
) (and 
 (or 
 (= oper ADD) (= oper SUB) (= oper AND)
) (or 
 (not (= oper ADD)) (= (select post rd) (bvadd (select pre rn) flex_val))
) (or 
 (not (= oper SUB)) (= (select post rd) (bvsub (select pre rn) flex_val))
) (or 
 (not (= oper AND)) (= (select post rd) (bvand (select pre rn) flex_val))
)
)
) (or 
 (and 
 (= flag N) (= (select pre CPSR) (select post CPSR))
) (and 
 (= flag S) (or 
 (not (= (select post rd) #b00000000000000000000000000000000)) (= ((_ extract 30 30) (select post CPSR)) #b1)
) (or 
 (= (select post rd) #b00000000000000000000000000000000) (= ((_ extract 30 30) (select post CPSR)) #b0)
)
)
)
)
)
))
) 

))))(assert (let ( (num 6))(let ( (pre (select sequence num)) (post (select sequence (+ num 1))) (oper (operx (select program num))) (cond (condx (select program num))) (flag (flagx (select program num))) (rd (rdx (select program num))) (rn (rnx (select program num))) (ro (rox (select program num))) (imm (immx (select program num))) (imm_used (imm_usedx (select program num))) (barrel_op (barrel_opx (select program num))) (barrel_num (barrel_numx (select program num))))(and 
 (= (bvadd (select pre PC) #b00000000000000000000000000000100) (select post PC)) 
 (or 
 (and 
 (not (or 
 (and 
 (= cond EQ) (= ((_ extract 30 30) (select pre CPSR)) #b1)
) (and 
 (= cond NE) (= ((_ extract 30 30) (select pre CPSR)) #b0)
) (and 
 (= cond CS) (= ((_ extract 29 29) (select pre CPSR)) #b1)
) (and 
 (= cond CC) (= ((_ extract 29 29) (select pre CPSR)) #b0)
) (and 
 (= cond MI) (= ((_ extract 31 31) (select pre CPSR)) #b1)
) (and 
 (= cond PL) (= ((_ extract 31 31) (select pre CPSR)) #b0)
) (and 
 (= cond VS) (= ((_ extract 28 28) (select pre CPSR)) #b1)
) (and 
 (= cond VC) (= ((_ extract 28 28) (select pre CPSR)) #b0)
) (and 
 (= cond GT) (and 
 (= ((_ extract 30 30) (select pre CPSR)) #b0) (= (= ((_ extract 31 31) (select pre CPSR)) #b1) (= ((_ extract 28 28) (select pre CPSR)) #b1))
)
) (and 
 (= cond LE) (or 
 (= ((_ extract 30 30) (select pre CPSR)) #b1) (not (= (= ((_ extract 31 31) (select pre CPSR)) #b1) (= ((_ extract 28 28) (select pre CPSR)) #b1)))
)
) (= cond AL)
)) (and 
 (= (select pre R0) (select post R0)) (= (select pre R1) (select post R1)) (= (select pre R2) (select post R2)) (= (select pre R3) (select post R3)) (= (select pre R4) (select post R4)) (= (select pre R5) (select post R5)) (= (select pre R6) (select post R6)) (= (select pre R7) (select post R7)) (= (select pre R8) (select post R8)) (= (select pre R9) (select post R9)) (= (select pre R10) (select post R10)) (= (select pre R11) (select post R11)) (= (select pre R12) (select post R12)) (= (select pre SP) (select post SP)) (= (select pre LR) (select post LR))
)
) (let ( (flex_val (let ( (val_to_shift (ite imm_used ((_ rotate_right 0) (concat #b000000000000000000000000 ((_ extract 7 0) imm))) (select pre ro))))(ite (= barrel_op LSL) (bvshl val_to_shift barrel_num) (ite (= barrel_op LSR) (bvlshr val_to_shift barrel_num) (bvashr val_to_shift barrel_num))))))(and 
 (or 
 (and 
 (= cond EQ) (= ((_ extract 30 30) (select pre CPSR)) #b1)
) (and 
 (= cond NE) (= ((_ extract 30 30) (select pre CPSR)) #b0)
) (and 
 (= cond CS) (= ((_ extract 29 29) (select pre CPSR)) #b1)
) (and 
 (= cond CC) (= ((_ extract 29 29) (select pre CPSR)) #b0)
) (and 
 (= cond MI) (= ((_ extract 31 31) (select pre CPSR)) #b1)
) (and 
 (= cond PL) (= ((_ extract 31 31) (select pre CPSR)) #b0)
) (and 
 (= cond VS) (= ((_ extract 28 28) (select pre CPSR)) #b1)
) (and 
 (= cond VC) (= ((_ extract 28 28) (select pre CPSR)) #b0)
) (and 
 (= cond GT) (and 
 (= ((_ extract 30 30) (select pre CPSR)) #b0) (= (= ((_ extract 31 31) (select pre CPSR)) #b1) (= ((_ extract 28 28) (select pre CPSR)) #b1))
)
) (and 
 (= cond LE) (or 
 (= ((_ extract 30 30) (select pre CPSR)) #b1) (not (= (= ((_ extract 31 31) (select pre CPSR)) #b1) (= ((_ extract 28 28) (select pre CPSR)) #b1)))
)
) (= cond AL)
) (or 
 (and 
 (and 
 (= (select pre R0) (select post R0)) (= (select pre R1) (select post R1)) (= (select pre R2) (select post R2)) (= (select pre R3) (select post R3)) (= (select pre R4) (select post R4)) (= (select pre R5) (select post R5)) (= (select pre R6) (select post R6)) (= (select pre R7) (select post R7)) (= (select pre R8) (select post R8)) (= (select pre R9) (select post R9)) (= (select pre R10) (select post R10)) (= (select pre R11) (select post R11)) (= (select pre R12) (select post R12)) (= (select pre SP) (select post SP)) (= (select pre LR) (select post LR))
) (or 
 (= oper CMP)
) (and 
 (or 
 (not (= oper CMP)) (and 
 (or 
 (not (= (select pre rn) flex_val)) (= ((_ extract 30 30) (select post CPSR)) #b1)
) (or 
 (= (select pre rn) flex_val) (= ((_ extract 30 30) (select post CPSR)) #b0)
)
)
)
)
) (and 
 (and 
 (or 
 (= (select pre R0) (select post R0)) (= rd R0)
) (or 
 (= (select pre R1) (select post R1)) (= rd R1)
) (or 
 (= (select pre R2) (select post R2)) (= rd R2)
) (or 
 (= (select pre R3) (select post R3)) (= rd R3)
) (or 
 (= (select pre R4) (select post R4)) (= rd R4)
) (or 
 (= (select pre R5) (select post R5)) (= rd R5)
) (or 
 (= (select pre R6) (select post R6)) (= rd R6)
) (or 
 (= (select pre R7) (select post R7)) (= rd R7)
) (or 
 (= (select pre R8) (select post R8)) (= rd R8)
) (or 
 (= (select pre R9) (select post R9)) (= rd R9)
) (or 
 (= (select pre R10) (select post R10)) (= rd R10)
) (or 
 (= (select pre R11) (select post R11)) (= rd R11)
) (or 
 (= (select pre R12) (select post R12)) (= rd R12)
) (or 
 (= (select pre SP) (select post SP)) (= rd SP)
) (or 
 (= (select pre LR) (select post LR)) (= rd LR)
)
) (or 
 (and 
 (or 
 (= oper MOV) (= oper MVN)
) (or 
 (not (= oper MOV)) (= (select post rd) flex_val)
) (or 
 (not (= oper MVN)) (= (select post rd) (bvnot flex_val))
)
) (and 
 (or 
 (= oper ADD) (= oper SUB) (= oper AND)
) (or 
 (not (= oper ADD)) (= (select post rd) (bvadd (select pre rn) flex_val))
) (or 
 (not (= oper SUB)) (= (select post rd) (bvsub (select pre rn) flex_val))
) (or 
 (not (= oper AND)) (= (select post rd) (bvand (select pre rn) flex_val))
)
)
) (or 
 (and 
 (= flag N) (= (select pre CPSR) (select post CPSR))
) (and 
 (= flag S) (or 
 (not (= (select post rd) #b00000000000000000000000000000000)) (= ((_ extract 30 30) (select post CPSR)) #b1)
) (or 
 (= (select post rd) #b00000000000000000000000000000000) (= ((_ extract 30 30) (select post CPSR)) #b0)
)
)
)
)
)
))
) 

))))(assert (let ( (num 7))(let ( (pre (select sequence num)) (post (select sequence (+ num 1))) (oper (operx (select program num))) (cond (condx (select program num))) (flag (flagx (select program num))) (rd (rdx (select program num))) (rn (rnx (select program num))) (ro (rox (select program num))) (imm (immx (select program num))) (imm_used (imm_usedx (select program num))) (barrel_op (barrel_opx (select program num))) (barrel_num (barrel_numx (select program num))))(and 
 (= (bvadd (select pre PC) #b00000000000000000000000000000100) (select post PC)) 
 (or 
 (and 
 (not (or 
 (and 
 (= cond EQ) (= ((_ extract 30 30) (select pre CPSR)) #b1)
) (and 
 (= cond NE) (= ((_ extract 30 30) (select pre CPSR)) #b0)
) (and 
 (= cond CS) (= ((_ extract 29 29) (select pre CPSR)) #b1)
) (and 
 (= cond CC) (= ((_ extract 29 29) (select pre CPSR)) #b0)
) (and 
 (= cond MI) (= ((_ extract 31 31) (select pre CPSR)) #b1)
) (and 
 (= cond PL) (= ((_ extract 31 31) (select pre CPSR)) #b0)
) (and 
 (= cond VS) (= ((_ extract 28 28) (select pre CPSR)) #b1)
) (and 
 (= cond VC) (= ((_ extract 28 28) (select pre CPSR)) #b0)
) (and 
 (= cond GT) (and 
 (= ((_ extract 30 30) (select pre CPSR)) #b0) (= (= ((_ extract 31 31) (select pre CPSR)) #b1) (= ((_ extract 28 28) (select pre CPSR)) #b1))
)
) (and 
 (= cond LE) (or 
 (= ((_ extract 30 30) (select pre CPSR)) #b1) (not (= (= ((_ extract 31 31) (select pre CPSR)) #b1) (= ((_ extract 28 28) (select pre CPSR)) #b1)))
)
) (= cond AL)
)) (and 
 (= (select pre R0) (select post R0)) (= (select pre R1) (select post R1)) (= (select pre R2) (select post R2)) (= (select pre R3) (select post R3)) (= (select pre R4) (select post R4)) (= (select pre R5) (select post R5)) (= (select pre R6) (select post R6)) (= (select pre R7) (select post R7)) (= (select pre R8) (select post R8)) (= (select pre R9) (select post R9)) (= (select pre R10) (select post R10)) (= (select pre R11) (select post R11)) (= (select pre R12) (select post R12)) (= (select pre SP) (select post SP)) (= (select pre LR) (select post LR))
)
) (let ( (flex_val (let ( (val_to_shift (ite imm_used ((_ rotate_right 0) (concat #b000000000000000000000000 ((_ extract 7 0) imm))) (select pre ro))))(ite (= barrel_op LSL) (bvshl val_to_shift barrel_num) (ite (= barrel_op LSR) (bvlshr val_to_shift barrel_num) (bvashr val_to_shift barrel_num))))))(and 
 (or 
 (and 
 (= cond EQ) (= ((_ extract 30 30) (select pre CPSR)) #b1)
) (and 
 (= cond NE) (= ((_ extract 30 30) (select pre CPSR)) #b0)
) (and 
 (= cond CS) (= ((_ extract 29 29) (select pre CPSR)) #b1)
) (and 
 (= cond CC) (= ((_ extract 29 29) (select pre CPSR)) #b0)
) (and 
 (= cond MI) (= ((_ extract 31 31) (select pre CPSR)) #b1)
) (and 
 (= cond PL) (= ((_ extract 31 31) (select pre CPSR)) #b0)
) (and 
 (= cond VS) (= ((_ extract 28 28) (select pre CPSR)) #b1)
) (and 
 (= cond VC) (= ((_ extract 28 28) (select pre CPSR)) #b0)
) (and 
 (= cond GT) (and 
 (= ((_ extract 30 30) (select pre CPSR)) #b0) (= (= ((_ extract 31 31) (select pre CPSR)) #b1) (= ((_ extract 28 28) (select pre CPSR)) #b1))
)
) (and 
 (= cond LE) (or 
 (= ((_ extract 30 30) (select pre CPSR)) #b1) (not (= (= ((_ extract 31 31) (select pre CPSR)) #b1) (= ((_ extract 28 28) (select pre CPSR)) #b1)))
)
) (= cond AL)
) (or 
 (and 
 (and 
 (= (select pre R0) (select post R0)) (= (select pre R1) (select post R1)) (= (select pre R2) (select post R2)) (= (select pre R3) (select post R3)) (= (select pre R4) (select post R4)) (= (select pre R5) (select post R5)) (= (select pre R6) (select post R6)) (= (select pre R7) (select post R7)) (= (select pre R8) (select post R8)) (= (select pre R9) (select post R9)) (= (select pre R10) (select post R10)) (= (select pre R11) (select post R11)) (= (select pre R12) (select post R12)) (= (select pre SP) (select post SP)) (= (select pre LR) (select post LR))
) (or 
 (= oper CMP)
) (and 
 (or 
 (not (= oper CMP)) (and 
 (or 
 (not (= (select pre rn) flex_val)) (= ((_ extract 30 30) (select post CPSR)) #b1)
) (or 
 (= (select pre rn) flex_val) (= ((_ extract 30 30) (select post CPSR)) #b0)
)
)
)
)
) (and 
 (and 
 (or 
 (= (select pre R0) (select post R0)) (= rd R0)
) (or 
 (= (select pre R1) (select post R1)) (= rd R1)
) (or 
 (= (select pre R2) (select post R2)) (= rd R2)
) (or 
 (= (select pre R3) (select post R3)) (= rd R3)
) (or 
 (= (select pre R4) (select post R4)) (= rd R4)
) (or 
 (= (select pre R5) (select post R5)) (= rd R5)
) (or 
 (= (select pre R6) (select post R6)) (= rd R6)
) (or 
 (= (select pre R7) (select post R7)) (= rd R7)
) (or 
 (= (select pre R8) (select post R8)) (= rd R8)
) (or 
 (= (select pre R9) (select post R9)) (= rd R9)
) (or 
 (= (select pre R10) (select post R10)) (= rd R10)
) (or 
 (= (select pre R11) (select post R11)) (= rd R11)
) (or 
 (= (select pre R12) (select post R12)) (= rd R12)
) (or 
 (= (select pre SP) (select post SP)) (= rd SP)
) (or 
 (= (select pre LR) (select post LR)) (= rd LR)
)
) (or 
 (and 
 (or 
 (= oper MOV) (= oper MVN)
) (or 
 (not (= oper MOV)) (= (select post rd) flex_val)
) (or 
 (not (= oper MVN)) (= (select post rd) (bvnot flex_val))
)
) (and 
 (or 
 (= oper ADD) (= oper SUB) (= oper AND)
) (or 
 (not (= oper ADD)) (= (select post rd) (bvadd (select pre rn) flex_val))
) (or 
 (not (= oper SUB)) (= (select post rd) (bvsub (select pre rn) flex_val))
) (or 
 (not (= oper AND)) (= (select post rd) (bvand (select pre rn) flex_val))
)
)
) (or 
 (and 
 (= flag N) (= (select pre CPSR) (select post CPSR))
) (and 
 (= flag S) (or 
 (not (= (select post rd) #b00000000000000000000000000000000)) (= ((_ extract 30 30) (select post CPSR)) #b1)
) (or 
 (= (select post rd) #b00000000000000000000000000000000) (= ((_ extract 30 30) (select post CPSR)) #b0)
)
)
)
)
)
))
) 

))))
(assert (= (select (select sequence 0) PC) #b00000000000000000000000000000001))
(assert (= (select (select sequence 2) PC) #b00000000000000000000000000001001))
(check-sat)
(get-model)
