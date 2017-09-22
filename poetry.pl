/* 
 * Poetry generator
 *
 * Uses old probdcg to run grammar with rphrase
 */

:- use_module(library(rphrase)).
:- use_module(library(plrand),    [is_rnd_state/1, set_rnd_state/1]).
:- use_module(library(prob/meta), [sample/2]).
:- use_module(library(dcg_core),  [get//1, set//1, rep//2, iso//1, (//)//2]).
:- use_module(library(dcg_shell)).
:- use_module(library(dcg_pair)).
:- use_module(library(dcg_macros)).
:- use_module(library(lex/biglex)).
:- use_module(library(trender)).
:- use_module(library(callutils)).
:- use_module(grammar).

rphrase:mapquery(A,B) :- grammar:mapquery(A,B).
rphrase:mapgoal(A,B)  :- grammar:mapgoal(A,B).
rphrase:(A--->B)      :- grammar:(A--->B).
rphrase:(A+-->B)      :- grammar:(A+-->B).


%! rphrase_nofail(P:rphrase(T), L:list(T))// is semidet.
%  Runs in =|pair(_,pdcg(T))|= DCG.
rphrase_nofail(T,L) --> \> sample_rphrase(fac(T),L).

voice(V) --> sample_rphrase(#voice(V),_).

render(Action,Prog) --> 
	run_left(Prog,L,[]), 
	{	(	select(title(Tit),L,L1) % float title to top
		->	L2=[title(Tit)|L1]
		;	L2=L
		),
		render(Action,L2)
	}.

blank_line  --> \< [nl].

line(T)     --> rphrase_nofail(T>>fs,L), \< [line(L)].
line(Len,T) --> {length(L,Len)}, rphrase_nofail(T,L), \< [line(L)].

title(T)    --> rphrase_nofail(T,L), \< [title(L)].
title(L,T)  --> {length(S,L)}, rphrase_nofail(T,S), \< [title(S)].

stanza(0,_) --> blank_line.
stanza(N,T) --> {succ(M,N)}, line(T), stanza(M,T).

stanza([]) --> blank_line.
stanza([O:S|LX]) --> pwith(O,line(S)), stanza(LX).
stanza([line(O,N,S)|LX]) --> pwith(O,line(N,S)), stanza(LX).
stanza([line(N,S)|LX]) --> line(N,S), stanza(LX).


stanza2 -->
	rep(2,line(np(_,_,_,nominative,_)>>fs) >> line(adj(_,_,[],_)>>fs)),
	line(vp(_,_,_,_,_,gerund) >> fs),
	blank_line.

poem(Al,Mult,N) --> 
	resample(dirichlet(Al)), pget(U), store_to(poem),
	pwith(U, (resample(specialise(Mult)), pget(A), store_to(poem(1)))),
	pwith(U, (resample(specialise(Mult)), pget(B), store_to(poem(2)))),
	% poem_body2(4,A,B).
	poem_body(2,N,A,B).

last_poem(M,N) --> 
	get_from(poem(1)), pget(A),
	get_from(poem(2)), pget(B),
	poem_body(M,N,A,B).

poem_body(M,N,A,B) -->
	rphrase_nofail(#(T=a;T=b),_),
	{T=a,TT=A;T=b,TT=B},
	pwith(TT, title(9,np(_,_,_,_,_))),
	rep(M,(
		pwith(A, sstanza(N,s)),
		pwith(A, sstanza(N,s)),
		pwith(B, sstanza(N,s)),
		pwith(A, sstanza(N,s))
	)).
sstanza(N,S) --> resample(specialise(2)), stanza(N,S).

poem_body2(M,A,B) -->
	rphrase_nofail(#(T=a;T=b),_),
	{T=a,TT=A;T=b,TT=B},
	pwith(TT, title(9,np(_,_,_,_,_))),
	rep(M,timeout_retry(10, sstanza_form(s,[A,A,B,A]), [])).

sstanza_form(T,Form) --> foldl(sline(T),Form), blank_line.
sline(T,A) --> pwith(A, (resample(specialise(2)), line(T))).


poem2(old) -->
	get_from(poem2(1)), pget(D),
	get_from(poem2(2)), pget(A),
	get_from(poem2(3)), pget(B),
	pwith(D, title(9,s)),
	rep(3,piso((stanza([A:s,B:s,A:s,B:s,D:s])))).

poem2(new,Mult) -->
	pget(C),
	pwith(C, (resample(specialise(Mult)),pget(D),store_to(poem2(1)))),
	pwith(C, (resample(specialise(Mult)),pget(A),store_to(poem2(2)))),
	pwith(C, (resample(specialise(Mult)),pget(B),store_to(poem2(3)))),
	pwith(D, title(9,s)),
	rep(3,piso((stanza([A:s,B:s,A:s,B:s,D:s])))).

poem1(Alpha,Alpha2,TO) --> 
	foldl(resample_get(dirichlet(Alpha)),[A,B,C]), % per-line models
	{maplist(sample(3+binomial(0.25,32)),[L1,L2,L3])}, % mean line lengths
   {writeln(L1:L2:L1:L3)},

	% make up a title using products of base model and line model C
	render(fmt, pwith(C, title(title))),
	% iso(
	% 	timeout_retry(TO,
	% 		(product(C), render(fmt,title(title))),
	% 		resample(diffuse(0.01,Alpha2)))),

	rep(4, % for each of 4 stanzas
		( get(Z1), % get the current models
		  render(fmt, foldl(poem1_folder(TO,Z1,Alpha2),
							     [L1:A,L2:B,L1:A,L3:C])),	 	% apply to line lengths and models
		  resample_probs(diffuse(0.05,Alpha2)),  % drift the base model a bit
		  {nl})).

resample_get(R,S) --> resample_probs(R), get(S).
poem1_folder(TO,Z1,Alpha2,_L:K) -->
	timeout_retry(TO,
		(product(K), line(s), pset(Z1)),
		resample(diffuse(0.01,Alpha2))).


% -- tools for working with pair(list, state) DCG.
pget(X) --> \> get(X).
pset(X) --> \> set(X).
piso(G) --> \> get(S), \< run_right(G,S,_).
pwith(S,G) --> \< run_right(G,S,_).

cool(DT) --> \> cool_probs(DT).
resample(M) --> \> resample_probs(M).
product(M) --> \> product_probs(M).

:- meta_predicate replay(:,?,?).
replay(M:log(S1,RndState,Goal),_,S2) :-
	set_rnd_state(RndState),
	call_dcg(M:Goal,S1,S2).

main :- main(200,s).

main(N,T) :- 
   empty_state(S0),
   rep(N,sample_rphrase(fac(T),_),S0,S1),
	dcgshell(S1,_).

:- dynamic state/2.

get_from(Key) --> {state(Key,S)}, pset(S).
store_to(Key) --> pget(S), {store_state(Key,S)}.
store_state(Key,State) :-
   retractall(state(Key,_)),
   assert(state(Key,State)).
