s(S) --> sentences(S).

sentences(S) --> sentence(S).
sentences(ss(S, Ss)) --> sentence(S), sentences(Ss).

sentence(s(If)) --> if_statement(If).
sentence(s(Assign)) --> assignment_statement(Assign).
sentence(s(Loop)) --> loop_statement(Loop).


if_statement(ifstat(Cond, Then)) --> ['if'], ['('], rel_Expr(Cond), [')'], sentence(Then).
if_statement(ifelseStat(Cond, Then, Else)) --> ['if'], ['('], rel_Expr(Cond), [')'], sentence(Then), ['else'], sentence(Else).

loop_statement(loop(Cond,Then)) --> ['while','('], rel_Expr(Cond), [')'], sentence(Then).

rel_Expr(relExpr(A,B,C)) --> arith_Expr(A), relOp(B), arith_Expr(C).

relOp(eq('==')) --> ['=='].
relOp(goe('>=')) --> ['>='].
relOp(ltoe('<=')) --> ['<='].
relOp(nq('!=')) --> ['!='].
relOp(lt('<')) --> ['<'].
relOp(gt('>')) --> ['>'].


assignment_statement(assignstat(Id, EQsign,Expr, Semicolon)) --> identifier(Id), equalSign(EQsign), arith_Expr(Expr), semicolon(Semicolon).

arith_Expr(arithExpr(X,Y))-->term(X),continue(Y).

factor(int(X)) --> unsigned_int(X).
factor(id(X)) --> identifier(X).
factor(factor(X)) --> ['('], arith_Expr(X), [')'].

term(term(X,Y))-->factor(X),term2(Y).

continue(add('+',X,Y))-->['+'],term(X),continue(Y).
continue(min('-',X,Y))-->['-'],term(X),continue(Y).
continue([])-->[].

term2(div('/',X,Y))-->['/'],factor(X),term2(Y).
term2(mul('*',X,Y))-->['*'],factor(X),term2(Y).
term2(mod('%',X,Y))-->['%'],factor(X),term2(Y).
term2([])-->[].


equalSign(equal('=')) --> ['='].
semicolon(semicolon(';')) --> [';'].
unsigned_int(X) --> [X], {integer(X), X >= 0}.


identifier([Letter|Tail]) --> [Letter], {letter(Letter)}, identifier_tail(Tail).
identifier([Letter]) --> [Letter], {letter(Letter)}.

identifier_tail([Alphanumeric|Tail]) --> [Alphanumeric], {alphanumeric(Alphanumeric)}, identifier_tail(Tail).
identifier_tail([Alphanumeric]) --> [Alphanumeric], {alphanumeric(Alphanumeric)}.

letter(Letter) :-
 member(Letter, ['A','B','C','D','E','F','G','H','I','J','K','L','M','N','O','P','Q','R','S','T','U','V','W','X','Y','Z',a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,'$','_']).


alphanumeric(Alphanumeric) :-
 member(Alphanumeric, ['A','B','C','D','E','F','G','H','I','J','K','L','M','N','O','P','Q','R','S','T','U','V','W','X','Y','Z',a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,'$','_',1,2,3,4,5,6,7,8,9,0]).

