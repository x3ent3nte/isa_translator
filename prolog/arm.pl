/* 
TODO

*/

xor(true, false).
xor(false, true).

nxor(true, true).
nxor(false, false).

zero([0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]).
zero([0,0,0,0]).

four([0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0]).
four([0,1,0,0]).

%pcPlus4(Pre, Post) :- getReg(Pre, pc, PrePC), getReg(Post, pc, PostPC), bvadd(PrePC, [0,1,0,0], PrePCplus4), areEqual(PrePCplus4, PostPC).
pcPlus4(Pre, Post) :- getReg(Pre, pc, PrePC), getReg(Post, pc, PostPC), PrePCplus4 is PrePC + 4, areEqual(PrePCplus4, PostPC).

bvadd(B1, B2, Res) :- reverseList(B1, B1Rev), reverseList(B2,B2Rev), bvadd(B1Rev, B2Rev, 0, [], Res).
bvadd([],[], 1, Acc, Acc).
bvadd([],[], 0, Acc, Acc).
bvadd([0|XT], [0|YT], 1, Acc, Res) :- bvadd(XT, YT, 0, [1|Acc], Res).
bvadd([1|XT], [0|YT], 1, Acc, Res) :- bvadd(XT, YT, 1, [0|Acc], Res).
bvadd([0|XT], [1|YT], 1, Acc, Res) :- bvadd(XT, YT, 1, [0|Acc], Res).
bvadd([1|XT], [1|YT], 1, Acc, Res) :- bvadd(XT, YT, 1, [1|Acc], Res).
bvadd([0|XT], [0|YT], 0, Acc, Res) :- bvadd(XT, YT, 0, [0|Acc], Res).
bvadd([1|XT], [0|YT], 0, Acc, Res) :- bvadd(XT, YT, 0, [1|Acc], Res).
bvadd([0|XT], [1|YT], 0, Acc, Res) :- bvadd(XT, YT, 0, [1|Acc], Res).
bvadd([1|XT], [1|YT], 0, Acc, Res) :- bvadd(XT, YT, 1, [0|Acc], Res).

bvor(B1, B2, Res) :- reverseList(B1, B1Rev), reverseList(B2, B2Rev), bvor(B1Rev, B2Rev, [], Res).
bvor([],[], Acc, Acc).
bvor([0|XT], [0|YT], Acc, Res) :- bvor(XT, YT, [0|Acc], Res).
bvor([1|XT], [0|YT], Acc, Res) :- bvor(XT, YT, [1|Acc], Res).
bvor([0|XT], [1|YT], Acc, Res) :- bvor(XT, YT, [1|Acc], Res).
bvor([1|XT], [1|YT], Acc, Res) :- bvor(XT, YT, [1|Acc], Res).

bvand(B1, B2, Res) :- reverseList(B1, B1Rev), reverseList(B2, B2Rev), bvand(B1Rev, B2Rev, [], Res).
bvand([],[], Acc, Acc).
bvand([0|XT], [0|YT], Acc, Res) :- bvand(XT, YT, [0|Acc], Res).
bvand([1|XT], [0|YT], Acc, Res) :- bvand(XT, YT, [0|Acc], Res).
bvand([0|XT], [1|YT], Acc, Res) :- bvand(XT, YT, [0|Acc], Res).
bvand([1|XT], [1|YT], Acc, Res) :- bvand(XT, YT, [1|Acc], Res).

bvxor(B1, B2, Res) :- reverseList(B1, B1Rev), reverseList(B2, B2Rev), bvxor(B1Rev, B2Rev, [], Res).
bvxor([],[], Acc, Acc).
bvxor([0|XT], [0|YT], Acc, Res) :- bvxor(XT, YT, [0|Acc], Res).
bvxor([1|XT], [0|YT], Acc, Res) :- bvxor(XT, YT, [1|Acc], Res).
bvxor([0|XT], [1|YT], Acc, Res) :- bvxor(XT, YT, [1|Acc], Res).
bvxor([1|XT], [1|YT], Acc, Res) :- bvxor(XT, YT, [0|Acc], Res).

reverseList(List, Rev) :- reverseList(List, [], Rev).
reverseList([], Acc, Acc).
reverseList([H|T], Acc, Rev) :- reverseList(T, [H|Acc], Rev).

allReg([r0,r1,r2,r3,r4,r5,r6,r7,r8,r9,r10,r11,r12,sp,lr,pc,cpsr]).

areEqual(R, R).

registersEqual(Pre, PreR,  Post, PostR) :- getReg(Pre, PreR, PreReg), getReg(Post, PostR, PostReg), areEqual(PreReg, PostReg).
registersNotEqual(Pre, PreR,  Post, PostR) :- getReg(Pre, PreR, PreReg), getReg(Post, PostR, PostReg), dif(PreReg, PostReg).

statesEqual([],[], _, _).
statesEqual([_RX|R1], [_RY|R2], [X|XT], [X|YT]) :- statesEqual(R1, R2, XT, YT).
statesEqual([RX|R1], [RX|R2], [], [_Y|YT]) :- statesEqual(R1, R2, [], YT).
statesEqual([RX|R1], [RX|R2], [X|XT], [_Y|YT]) :- statesEqual(R1, R2, [X|XT], YT).

getIndex([H|_T], 0, H).
getIndex([_H|T], N, Reg) :- NM1 is N - 1, getIndex(T, NM1, Reg). 

getReg(State, r0, Reg) :- getIndex(State, 0, Reg).
getReg(State, r1, Reg) :- getIndex(State, 1, Reg).
getReg(State, r2, Reg) :- getIndex(State, 2, Reg).
getReg(State, r3, Reg) :- getIndex(State, 3, Reg).
getReg(State, r4, Reg) :- getIndex(State, 4, Reg).
getReg(State, r5, Reg) :- getIndex(State, 5, Reg).
getReg(State, r6, Reg) :- getIndex(State, 6, Reg).
getReg(State, r7, Reg) :- getIndex(State, 7, Reg).
getReg(State, r8, Reg) :- getIndex(State, 8, Reg).
getReg(State, r9, Reg) :- getIndex(State, 9, Reg).
getReg(State, r10, Reg) :- getIndex(State, 10, Reg).
getReg(State, r11, Reg) :- getIndex(State, 11, Reg).
getReg(State, r12, Reg) :- getIndex(State, 12, Reg).
getReg(State, sp, Reg) :- getIndex(State, 13, Reg).
getReg(State, lr, Reg) :- getIndex(State, 14, Reg).
getReg(State, pc, Reg) :- getIndex(State, 15, Reg).
getReg(State, cpsr, Reg) :- getIndex(State, 16, Reg).

n1(State) :- getReg(State, cpsr, Cpsr), nOne(Cpsr).
nOne([1,_,_,_]).

z1(State) :- getReg(State, cpsr, Cpsr), zOne(Cpsr).
zOne([_,1,_,_]).

c1(State) :- getReg(State, cpsr, Cpsr), cOne(Cpsr).
cOne([_,_,1,_]).

v1(State) :- getReg(State, cpsr, Cpsr), vOne(Cpsr).
vOne([_,_,_,1]).

n0(State) :- getReg(State, cpsr, Cpsr), nZero(Cpsr).
nZero([0,_,_,_]).

z0(State) :- getReg(State, cpsr, Cpsr), zZero(Cpsr).
zZero([_,0,_,_]).

c0(State) :- getReg(State, cpsr, Cpsr), cZero(Cpsr).
cZero([_,_,0,_]).

v0(State) :- getReg(State, cpsr, Cpsr), vZero(Cpsr).
vZero([_,_,_,0]).

evalCond(State, eq) :- z1(State).
evalCond(State, ne) :- not(z1(State)).
evalCond(State, cs) :- c1(State).
evalCond(State, cc) :- not(c1(State)).
evalCond(State, mi) :- n1(State).
evalCond(State, pl) :- not(n1(State)).
evalCond(State, vs) :- v1(State).
evalCond(State, vc) :- not(v1(State)).
evalCond(State, hi) :- c1(State), not(z1(State)).
evalCond(State, ls) :- not(c1(State)); z1(State).
evalCond(State, ge) :- nxor(n1(State), v1(State)).
evalCond(State, lt) :- xor(n1(State), v1(State)).
evalCond(State, gt) :- not(z1(State)), nxor(n1(State), v1(State)).
evalCond(State, le) :- z1(State); xor(n1(State), v1(State)).
evalCond(_State, al).

%execute(Pre, Op, Cond, X, Y, Z, Post) :- execute3(Pre, Op, Cond, X, Y, Z, Post).
%execute(Pre, Op, Cond, X, Y, Post) :- execute2(Pre, Op, Cond, X, Y, Post).

execute(Pre, add, Cond, X, Y, Z, Post) :- evalCond(Pre, Cond), allReg(All), statesEqual(Pre, Post, [X, pc], All), getReg(Pre, Y, YReg), getReg(Pre, Z, ZReg), bvadd(YReg, ZReg, Added), getReg(Post, X, XReg), areEqual(XReg, Added), pcPlus4(Pre, Post).
execute(Pre, add, Cond, _X, _Y, _Z, Post) :- not(evalCond(Pre, Cond)), allReg(All), statesEqual(Pre, Post, [pc], All), pcPlus4(Pre, Post).

execute(Pre, mov, Cond, X, Y, none, Post) :- evalCond(Pre,Cond), allReg(All), statesEqual(Pre, Post, [X, pc], All), registersEqual(Pre, Y, Post,  X), pcPlus4(Pre, Post).
execute(Pre, mov, Cond, _X, _Y, none, Post) :- not(evalCond(Pre, Cond)), allReg(All), statesEqual(Pre, Post, [pc], All), pcPlus4(Pre, Post).

execute(Pre, cmp, Cond, X, Y, none, Post) :- evalCond(Pre, Cond), allReg(All), statesEqual(Pre, Post, [pc, cpsr], All), registersEqual(Pre, X, Pre, Y), z1(Post), pcPlus4(Pre, Post).
execute(Pre, cmp, Cond, X, Y, none, Post) :- evalCond(Pre, Cond), allReg(All), statesEqual(Pre, Post, [pc, cpsr], All), registersNotEqual(Pre, X, Pre, Y), z0(Post), pcPlus4(Pre, Post).
execute(Pre, cmp, Cond, _X, _Y, none, Post) :- not(evalCond(Pre,Cond)), allReg(All), statesEqual(Pre, Post, [pc], All), pcPlus4(Pre, Post).

triple(Pre, Post, [[O, C, X, Y, Z]|Rest]) :- execute(Pre, O, C, X, Y, Z, Mid), triple(Mid, Post, Rest).
triple(Pre, Post, [[O, C, X, Y, none]|Rest]) :- execute(Pre, O, C, X, Y, none, Mid), triple(Mid, Post, Rest).
triple(Post, Post, []).

transition(Pre, Pre, []).
transition(Pre, Post, Prog) :- transition(Pre, Post, [], ProgRev), reverseList(ProgRev, Prog).
transition(Pre, Post, Acc, [[O, C, X, Y, Z]| Acc]) :- execute(Pre, O, C, X, Y, Z, Post).
transition(Pre, Post, Acc, [[O2, C2, X2, Y2, Z2],[O1, C1, X1, Y1, Z1]|Acc]) :- execute(Pre, O1, C1, X1, Y1, Z1, S1), execute(S1, O2, C2, X2, Y2, Z2, Post).
transition(Pre, Post, Acc, [[O3, C3, X3, Y3, Z3],[O2, C2, X2, Y2, Z2],[O1, C1, X1, Y1, Z1]|Acc]) :- execute(Pre, O1, C1, X1, Y1, Z1, S1), execute(S1, O2, C2, X2, Y2, Z2, S2), execute(S2, O3, C3, X3, Y3, Z3, Post).
transition(Pre, Post, Acc, [[O4, C4, X4, Y4, Z4],[O3, C3, X3, Y3, Z3],[O2, C2, X2, Y2, Z2],[O1, C1, X1, Y1, Z1]|Acc]) :- execute(Pre, O1, C1, X1, Y1, Z1, S1), execute(S1, O2, C2, X2, Y2, Z2, S2), execute(S2, O3, C3, X3, Y3, Z3, S3), execute(S3, O4, C4, X4, Y4, Z4, Post).

% triple([[0,0,0,1],[0,0,0,1],2,3,4,5,6,7,8,9,10,11,12,13,14,0,[0,1,0,0]],[[0,0,1,1],[0,0,0,1],3,3,4,5,6,7,8,9,10,11,12,13,14,12,[0,1,0,0]],[[mov, eq, r2, r3, none],[add, eq, r0, r1, r1], [add,eq, r0,r0,r1]]). 
% true

% transition([[0,0,0,1],[0,0,0,1],2,3,4,5,6,7,8,9,10,11,12,13,14,0,[0,1,0,0]],[[0,0,1,1],[0,0,0,1],3,3,4,5,6,7,8,9,10,11,12,13,14,12,[0,1,0,0]],Prog).
% Prog = [[add, eq, r0, r0, r0], [add, eq, r0, r0, r1], [mov, eq, r2, r3, none]]

% reg32([_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_]).
% [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16]
% N [1,0,0,0]
% Z [0,1,0,0]
% C [0,0,1,0]
% V [0,0,0,1]

















