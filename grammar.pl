/* 
	natural language grammar (English)

	Plan

	X made Y Z

	compound sentence conjuctions and tenses

	questions

	gaps

	more adverbs
	optimise sampling using eg BLOB for discrete dist
	sample verb before subject
	places
*/

:- module(grammar, [ 
		(--->)/2
	,	(+-->)/2
	,	mapgoal/2
	,	maprule/2
	,	pron/5
	,	person/1
	,	gender/2, gender/1
	,	multiplicity/1
	,	agency/1
	,	nform/1
	,	vform/1
	,	tense/1
	,	check_mod/2
	,	det/3
	,	ord/1
	,	quant/3
	,	quant_form/2
	,	modal/2
	,	verb_adverb/2
	,	edit_args/3
	,	vpseq/2, vpseq/3
	,	being_type/1
	,	modal_vform/2
	,	gtype/3
	
	,	op(1200,xfx,--->)
	,	op(1200,xfx,+-->)
	]).

:- op(1200,xfx,--->).
:- op(1200,xfx,+-->).
:- op(1180,xfx,\\).
:- op(1190,xfx,#).
:- op(900,fx,#).

:- style_check(-discontiguous).


   mapquery(_,_) :- fail.

	mapgoal( pname(_,_), pname(_,_)).
	mapgoal( gender(_,_), gender(_,_)).
	mapgoal( det(_,_,_), det(_,_,_)).
	mapgoal( adj(_,Ag,_,Cl), adj(_,Ag,_,Cl)).
	mapgoal( noun(_,_,_,_), noun(_,_,_,_)).
	mapgoal( pron(_,_,_,NF,_), pron(_,_,_,NF,_)).
	mapgoal( verb(T,_,_,_), verb(T,_,_,_)).



 maprule(maybe(A),maybe(A)).
maybe(_) ---> [].
maybe(A) ---> A.


% punctuation
fs ---> ['.'].
ex ---> ['!'].
qm ---> ['?'].
cm ---> [','].
sc ---> [';'].
cl ---> [':'].

title ---> s.
title ---> ss(6).
title ---> np(_,_,_,accusative,_).
title ---> vp(_,_,_,_,_,gerund).

% -------- Sentence 

ss ---> fac(s).
ss ---> cs.

ss(_) ---> fac(s).
ss(K) ---> iso(resample(specialise(K)) >> cs).

cs ---> fac(s(Tense)), #conj(Conj), conj_comma(Conj), [Conj], s(Tense).
% more constraints on conjunction sentences

 maprule(conj_comma(C),conj_comma(C)).
conj_comma(_) ---> [].
conj_comma(_) ---> [','].

sfs ---> fac(s), fs.

imp ---> vp(second,_,_,active,_,imperative), ex.

% question
qs ---> 
	aux(P,M), 
	np(P,M,G,nominative,A), 
	vp(P,M,G,A,_,bare), qm.

whqs ---> [how], fac(qs).
whqs ---> [why], fac(qs).

1 # s0 ---> whqs.
2 # s0 ---> qs.
3 # s0 ---> fac(imp).
6 # s0 ---> fac(ss), fs. 

aux(_,_) ---> #modal(W,_), [W]. 
aux(P,M) ---> vb(emphatic,P,M,tensed(_),_). 


bio(Gen,Sur,Middles) --->
	#(Gen=male;Gen=female),
	rep(Middles,fac((#pname(Gen,B),[B]))),
	[Sur], cl,
	rep(2,fac(adj(_,_,_,_))>>cm),
	fac(adj(_,_,_,_)).

	
% what question
%  whnp, vp.
% whnp, aux, np, vp.

s            ---> s(_).
s(Tense)     ---> #tense(Tense), np(P,N,G,nominative,A), vp(P,N,G,A,_,tensed(Tense)).
%s(Tense)     ---> cp(N,G,A), np(P,N,G,nominative,A), vp(P,N,G,A,_,tensed(Tense)).
s(Tense,Gap) ---> #tense(Tense), np(P,N,G,nominative,A), vp(P,N,G,A,_,tensed(Tense),Gap).

vpargs([]) ---> [].
vpargs([A|AX]) ---> A, vpargs(AX).

edit_args([],Args,Args).
edit_args([np(P,N,G,_,A)],[np(P,N,G,_,A)|Args],Args).
edit_args([object(P,N,G,A,P1,N1,G1,A1)],[object(P,N,G,A,P1,N1,G1,A1)|Args],Args).


% -------- Verb phrase  
%
% vp(Person,Multiplicity,Gender,Agency,VerbGroup,VerbForm).
% vp(Person,Multiplicity,Gender,Agency,VerbGroup,VerbForm,Gap).
%
% !!!TODOO
% look at prepositional phrases again
% absorb maybe([not]) into verb phrase
% need to incorporate adverbs in to verb phrases


% adverb classes for different verb types
verb_adverb(tensed(_),tensed).
verb_adverb(infinitive,infinitive).
verb_adverb(gerund,tensed).
verb_adverb(past_part,tensed).

 maprule( vp(_,_,_,_,_,_), vp(_,_,_,_,_,_)).
 maprule( vp(_,_,_,_,_,VF,_), vp(_,_,_,_,_,VF,_)).

vp(P,N,G,A,TD,VF) ---> vp(P,N,G,A,TD,VF,[]).

% General rule for ordinary verbs with any arguments
8 # vp(Pers,Num,Gen,Agency,[ordinary],VForm,Gap) --->
	({verb_adverb(VForm,Adv)} -> maybe_adv(Adv); []),
	vb(v(Type),Pers,Num,VForm,Agency),
	#verb_type(Type,[np(Pers,Num,Gen,nominative,Agency)|Args]),
	{edit_args(Gap,Args,Args1)},
	vpargs(Args1).


% ------------ based on TO BE -------------------------------

vp(Pers,Num,Gen,Agency,[being],VForm,Gap) ---> 
	vb(bv(Type),Pers,Num,VForm,Agency),
	#verb_type(Type,[np(Pers,Num,Gen,nominative,Agency)|Args]),
	({VForm=tensed(_)}->maybe([not])),
	{edit_args(Gap,Args,Args1)},
	vpargs(Args1).

% subject BEGINS/GOES/BEGAN/WENT/IS BEGINNING TO/IS GOING TO to verb_infinitive
vp(Per,Num,Gen,Agency,[modal(infinitive,Class)|TD],VForm,Gap) --->
	#multiplicity(Num), 
	maybe_adv(modal),
	vb(modal(infinitive,Class),Per,Num,VForm,Agency), 
	{modal_vform(Class,VForm)},
	vp(Per,Num,Gen,Agency,TD,infinitive,Gap),
	{\+member(modal(infinitive,Class),TD)}.


% --------- Compound Tense variations ---------------------------------------

% EMPHATIC:  subject DOES/DID verb_infinitive 
vp(Per,Num,Gen,Agency,[emphatic|TD],tensed(T),Gap) --->
	#multiplicity(Num), 
	#vpseq(emphatic,NextVP),
	vb(emphatic,Per,Num,tensed(T),_), maybe([not]),
	vp(Per,Num,Gen,Agency,[NextVP|TD],bare,Gap).


% MODAL: eg subject WILL/CAN verb_inifinitive
vp(Per,Num,Gen,Agency,[modal|TD],tensed(_),Gap) --->
	#multiplicity(Num), 
	#modal(W,_), [W], 
	vp(Per,Num,Gen,Agency,TD,bare,Gap).

% PERFECT: subject HAS/HAD verb_past participle 
vp(Per,Num,Gen,Agency,[perfect|TD],VForm,Gap) --->
	dif(VForm,past_part), dif(VForm,gerund) \\
	#multiplicity(Num), 
	vb(perfect,Per,Num,VForm,_), 
	vp(Per,Num,Gen,Agency,TD,past_part,Gap).


% PROGRESSIVE: subject IS gerund (eg is running)
vp(Pers,Num,Gen,Agency,[progressive|TD],VForm,Gap) ---> 
	dif(VForm,gerund) \\
	vb(progressive,Pers,Num,VForm,_),
	#vpseq(VForm,progressive,NextVP), {TD=[NextVP|_]},
%%	({VForm=tensed(_)}->maybe([not])),
	vp(Pers,Num,Gen,Agency,TD,gerund,Gap).

% ---------------  Derived forms

% imperative form
7 # vp(Pers,Num,Gen,Agency,TD,imperative,Gap) ---> 
	vp(Pers,Num,Gen,Agency,TD,bare,Gap),
	{\+member(not(_),TD)}. 

1 # vp(Pers,Num,Gen,Agency,[not(V)|TD],imperative,Gap) ---> 
	[do], vp(Pers,Num,Gen,Agency,[not(V)|TD],bare,Gap).

% negation of verb
vp(Pers,Num,Gen,Ag,[not(V)|TD],VForm,Gap) --->
	[not],
	{when(nonvar(VForm),(VForm\=tensed(_),VForm\=imperative))},
	vp(Pers,Num,Gen,Ag,[V|TD],VForm,Gap),
	{V\=not(_), \+member(not(_),TD)}.


vpseq(_,progressive,ordinary).
vpseq(tensed(_),progressive,modal(infinitive,_)).
vpseq(tensed(_),progressive,not(ordinary)).
vpseq(tensed(_),progressive,not(modal(infinitive,_))).
vpseq(tensed(_),progressive,being).
vpseq(tensed(_),progressive,not(being)).

vpseq(emphatic,modal(infinitive,_)).
vpseq(emphatic,ordinary).
vpseq(perfect,progressive).
vpseq(perfect,ordinary).
vpseq(perfect,modal(infinitive,_)).


% -------- Verbs

 maprule( vb(v(_),_,_,tensed(_),_), vb(v(_),_,_,tensed(_),_)).
 maprule( vb(v(_),_,_,T,_), vb(v(_),_,_,T,_)).
 maprule( vb(VF,_,_,tensed(_),_), vb(VF,_,_,tensed(_),_)).
 maprule( vb(VF,_,_,T,_), vb(VF,_,_,T,_)).

vb(v(T),P,N,infinitive,A) ---> [to], vb(v(T),P,N,bare,A).
vb(emphatic,P,N,infinitive,A) ---> [to], vb(emphatic,P,N,bare,A).
vb(perfect,P,N,infinitive,A) ---> [to], vb(perfect,P,N,bare,A).
vb(modal(infinitive,C),P,N,infinitive,A) ---> [to], vb(modal(infinitive,C),P,N,bare,A).


% standard verb form pattern 

vb(C,P,N,bare,A) ---> dif(C,progressive) \\   		
	#verb(C,A,W,_), #person(P), #multiplicity(N), [W].

vb(C,P,N,past_part,A) ---> dif(C,progressive) \\
	#verb(C,A,R,Mo), {verb_morph(Mo,R,_,_,_,W)},
	#person(P), #multiplicity(N), [W].


vb(C,P,N,gerund,A) 	\\ dif(C,progressive) --->       
	#verb(C,A,R,Mo), {verb_morph(Mo,R,_,_,W,_)},
	#person(P), #multiplicity(N), [W].

vb(C,P,N,tensed(past),A)	\\ dif(C,progressive) ---> 
	#verb(C,A,R,Mo), {verb_morph(Mo,R,_,W,_,_)},
	#person(P), #multiplicity(N), [W].

vb(C,P,N,tensed(present),A) \\ dif(C,progressive) ---> 
	#verb(C,A,R,Mo), {verb_morph(Mo,R,V,_,_,_)},
	#person(P), #multiplicity(N),
	( {P/N=third/singular} -> [V]; [R]).


% forms fo the verb to be
0.0625 # vb(progressive,P,N,VForm,A) ---> 
	#agency(A), #person(P), #multiplicity(N), 
	being_inflection(P,N,VForm). 

0.0625 # vb(bv(Type),P,N,VForm,A) ---> 
	#agency(A), #person(P), #multiplicity(N), #being_type(Type),
	being_inflection(P,N,VForm).

pred_adj(N,A) ---> adj(N,A,[],_).
pred_adj(N,A) ---> [W], #adj(N,A,W,predicative).

being_type([np(_,_,_,_,A), pred_adj(_,A)]).
being_type([np(P,N,G,_,A), cp(P,N,G,A)]).
being_type([np(P,N,G,_,A), object(P,N,G,A,_,N,G,_)]).
being_type([np(P,N,G,_,A), being_prep, object(P,N,G,A,_,_,_,_)]).

being_prep ---> [Prep], #((prep(Prep),Prep\=to,Prep\=at)).

	maprule( being_inflection(P,N,_), being_inflection(P,N,_)).
being_inflection(_,_,infinitive) ---> [to,be].
being_inflection(_,_,bare)       ---> [be].
being_inflection(_,_,gerund)     ---> [being].
being_inflection(_,_,past_part)  ---> [been].
being_inflection(P,N,tensed(present)) --->
	{ P/N=first/singular -> W=am 
	; P/N=third/singular -> W=is
	; W=are
	}, [W].

being_inflection(P,N,tensed(past)) ---> 
	{	P/N=first/singular -> W=was 
	;	P/N=third/singular -> W=was
	;	W=were
	}, [W].




% -------- Determiners

% determiner phrase - can include superlatives

	maprule(detp(_,_),detp(_,_)).

detp(continuous,_) ---> [].
detp(discrete(plural),_) ---> [].
detp(M,_) ---> det(M,D,W), predet(M,D), [W].
detp(M,A) ---> superdet(M), number(M), maybe(sp(M,A,[])).


% pre determiners
% all of, some of, N of, none of, no, the majority of ...
15 # predet(_,_) ---> [].
predet(discrete(plural),definite) ---> [some,of].
predet(discrete(plural),definite) ---> [all].
predet(discrete(plural),definite) ---> [all,of].
predet(continuous,definite) ---> [all].
predet(continuous,definite) ---> [all,of].
predet(discrete(plural),definite) ---> [none,of].
predet(continuous,definite) ---> [none,of].

% determiners that can be followed by numerals, ordinals and superlative phrase
4 # superdet(M) ---> det(M,definite,W), predet(M,definite), [W].
1 # superdet(M) ---> predet(M,definite), possessive(_).

	mapgoal(card(_,_),card(_,_)).
2  # number(discrete(plural)) ---> [W], #card(plural,W).
1  # number(discrete(singular)) ---> [W], #ord(W).
9  # number(_) ---> [].

possessive(N) ---> #pname(_,N), {atom_concat(N,'\'s',W)}, [W].

% superlative phrase
2 # sp(M,A,In) ---> #adj(M,A,W,normal(_)), {\+member(W,In)}, superlative(W).
1 # sp(M,A,In) +--> #adj(M,A,W,normal(_)), {\+member(W,In)}, superlative(W), 
	[','], sp(M,A,[W|In]).


% -------- Nominals

 maprule( nn(_,_,_), nn(_,_,_)). 

% !! could freeze computation of gender until needed
nn(discrete(singular),G,A) ---> [S], #noun(GT,A,S,_), #gtype(singular,GT,G).
nn(discrete(plural),G,A)   ---> [P], #noun(GT,A,_,P),  #gtype(plural,GT,G).
nn(continuous,neuter,passive) ---> [W], #substance(W).

/*
nn(discrete(singular),G,A) ---> [S], #noun(SS,A,S,_), {freeze(G,ss_gen(SS,singular,G)}.
nn(discrete(plural),G,A)   ---> [P], #noun(SS,A,_,P), {freeze(G,ss_gen(SS,plural,G)}.

ss_gen(SS,M,G) :-
	ss_gtype(SS,GT),
	gtype(M,GT,G).

:- index(gtype(1,1,0)).
*/

gtype(_,only(G),G).
gtype(_,neuter,neuter).
gtype(singular,either,male).
gtype(singular,either,female).
gtype(plural,either,male).
gtype(plural,either,female).
gtype(plural,either,mixed).

% -------- Noun phrase  


pron(P,M,G,C) ---> #nform(C), #pron(P,M,G,C,W), [W].

 maprule( np(_,_,_,NF,_), np(_,_,_,NF,_)).

2 # np(P,M,G,F,A) +--> np(P,M,G,F,A), rel(P,M,G,A).
1 # np(P,M,G,F,A) +--> np(P,M,G,F,A), cm, vp(P,M,G,A,_,gerund,[]), cm.
3 # np(P,M,G,C,active) ---> pron(P,M,G,C).
5 # np(third,M,G,F,A) ---> #nform(F), #multiplicity(M), nnp(discrete(M),G,A).
3 # np(third,singular,G,F,A) ---> #nform(F), nnp(continuous,G,A).
1 # np(third,singular,_,NF,A) ---> #nform(NF), qp(NF,A).
3 # np(third,singular,G,NF,active) --->   
	#nform(NF), #pname(G,Name), 
	ap(proper,discrete(singular),active), [Name].

nnp(M,G,A) ---> detp(M,A), ap(ordinary,M,A), nn(M,G,A).

% Quantified nouns like everything, something, nothing etc
	maprule(qp(NF,_),qp(NF,_)).
qp(NF,A) ---> #quant_form(NF,Q), #quant(Q,A,W), #agency(A), [W].

quant_form(_,some).
quant_form(_,every).
quant_form(_,no).
%quant_form(accusative,any).




% -- Pronouns
pron(first,singular,_,nominative,     'I').
pron(second,singular,_,nominative,    you).
pron(third,singular,male,nominative,  he).
pron(third,singular,female,nominative,she).
pron(third,singular,neuter,nominative,it).
pron(first,plural,_,nominative,       we).
pron(second,plural,_,nominative,      you).
pron(third,plural,_,nominative,       they).

pron(first,singular,_,accusative,     me).
pron(second,singular,_,accusative,    you).
pron(third,singular,male,accusative,  him).
pron(third,singular,female,accusative,her).
pron(third,singular,neuter,accusative,it).
pron(first,plural,_,accusative,     us).
pron(second,plural,_,accusative,    you).
pron(third,plural,_,accusative,     them).

pron(first,singular,_,reflexive,     myself).
pron(second,singular,_,reflexive,    yourself).
pron(third,singular,male,reflexive,  himself).
pron(third,singular,female,reflexive,herself).
pron(third,singular,neuter,reflexive,itself).
pron(first,plural,_,reflexive,     ourselves).
pron(second,plural,_,reflexive,    yourselves).
pron(third,plural,_,reflexive,     themselves).

% Noun phrases including possible reflexive nouns 
% that agree with subject of verb
	maprule(object(P,_,_,A,_,_,_,_),object(P,_,_,A,_,_,_,_)).
9 # object(_,_,_,_,P,N,G,A) ---> np(P,N,G,accusative,A).
1 # object(P,N,G,A,P,N,G,A) ---> [W], #pron(P,N,G,reflexive,W).

% -------- Adjectives

 maprule( ap(T,_,_), ap(T,_,_)).
 maprule( apx(_,_,_), apx(_,_,_)).

% adjectival phrase, first arg to segregate probability tables
% either no adjective or some..
ap(_,_,_) ---> [].
ap(_,M,A) ---> apx(M,A,[]-_).

% ap helper - recursive, keeps track of adjectives used so far
% number of adjectives has exponential distribution
apx(M,A,_) ---> [W], #adj(M,A,W,attributive).
apx(M,A,Adj1-_) ---> adj(M,A,Adj1,_).
apx(M,A,Adj1-T) +--> adj(M,A,Adj1,Adj2), adj_comma(T), apx(M,A,Adj2-T).


	maprule(adj_comma(_),adj_comma(_)).
1 # adj_comma(false) ---> [].
3 # adj_comma(true) ---> [','].

1 # rel_comma(false) ---> [].
3 # rel_comma(true) ---> [','].

% -------- comparative phrase
cp(P,M,G,A) ---> #adj(_,A,D,normal(_)), comparative(D), [than], object(P,M,G,A,_,M,G,A).

% -------- Relative clause
rel(P,M,G,A) ---> 
	rel_comma(C), 
	wh(nominative,A), 
	vp(P,M,G,A,_,tensed(_),[]), 
	rel_comma(C).

rel(P,M,G,A) ---> 
	rel_comma(C), 
	wh(accusative,A), 
	s(_,np(P,M,G,_,A)), 
	rel_comma(C).


% -------- Adjectival phrases

 maprule( adj(_,_,_,_), adj(_,_,_,_)).

adj(M,A,In,Out) ---> #agency(A), adj(M,A,In,Out,[]).
adj(M,A,In,[W:Mods|In],Mods) ---> [W], #adj(M,A,W,normal(_)), check_adj(W:Mods,In). 
adj(M,A,In,Out,Mods) +--> mod(Mods,Mods2), adj(M,A,In,Out,Mods2).
0.125 # adj(_,active,In,In,[]) ---> vb(v(wn(2)),_,_,gerund,active). 

% adjective modifier
 maprule( mod(_,_), mod(_,_)).
mod(In,[Mod|In]) ---> #mod(Mod), {check_mod(Mod,In)}, [Mod]. 

% allow/disallow particular chains of adjectives
%% small prob of allowing double adjective
8 # check_adj(_,[]) ---> [].
%1 # check_adj(X:_,[_|Z]) ---> {\+member(X:_,Z)}.
7 # check_adj(X:_,[Y|Z]) ---> {\+member(X:_,[Y|Z])}.

% allow/disallow certain chains of modifiers
check_mod(_,[]).
check_mod(very,[very|_]). % allowed to repeat very
check_mod(slightly,[very|_]). % allowed to follow very with slightly


comparative(A)  ---> 
	{adj(_,_,A,normal(Rel))}, 
	(	{Rel=rel(B,_)} -> [B] 
	;	{Rel=prefix}   -> [more,A]
	).

superlative(A)  ---> 
	{adj(_,_,A,normal(Rel))}, 
	(	{Rel=rel(_,B)} -> [B] 
	;	{Rel=prefix}   -> [most,A]
	).

% --------- Adverbs

   maprule(maybe_adv(A),maybe_adv(A)).
4 # maybe_adv(modal) ---> [].
1 # maybe_adv(modal) ---> adv.
maybe_adv(tensed) ---> [].
maybe_adv(tensed) ---> adv.
4 # maybe_adv(infinitive) ---> [].
1 # maybe_adv(infinitive) ---> adv.

adv ---> #adv(A), [A].

% -------- Wh*

 maprule( wh(_,_), wh(_,_)).

wh(nominative,active) ---> [who].
wh(accusative,active) ---> [whom].
wh(_,_) ---> [that].
wh(_,passive) ---> [which].

% lists
	maprule(comma_list(_),comma_list(_)).

comma_list(C) ---> C.
comma_list(C) +--> C, [','], comma_list(C).

	maprule(and_list(_),and_list(_)).
and_list(C) ---> C.
and_list(C) ---> comma_list(C), ['and'], C.

person(first).
person(second).
person(third).

gender(G) :- gender(_,G).
gender(singular,male).
gender(singular,female).
gender(singular,neuter).
gender(plural,mixed).
gender(plural,G) :- gender(singular,G).

	

agency(active).
agency(passive).

multiplicity(singular).
multiplicity(plural).

nform(accusative).
nform(nominative).

% these will affect noun usage wrt verbs and prepositions
nclass(countable).
nclass(continuous).
nclass(place).
nclass(time).
nclass(abstract).
nclass(concrete).

% these will affect verb usage wrt tenses and auxilaries and modals
vclass(punctual).
vclass(process).
vclass(countable).


vform(infinitive).
vform(bare).
vform(imperative).
vform(gerund).
vform(past_part).
vform(tensed(Tense)) :- tense(Tense).

tense(past).
tense(present).
%tense(future).

modal(can,future).
modal('can\'t',future).
modal(could,future).
modal('couldn\'t',future).
modal(will,future).
modal('won\'t',future).
modal(would,future).
%modal('wouldn\'t',future).
modal(may,future).
modal(shall,future).
modal(should,future).
% modal('shouldn\'t',future).
modal(must,future).
modal(might,future).
% modal('mustn\'t',future).
% modal('needn\'t',future).

det(_,indefinite,some).
det(_,indefinite,no).
det(continuous,indefinite,all).
det(discrete(plural),indefinite,all).

det(continuous,definite,this).
det(discrete(singular),definite,this).
det(discrete(plural),definite,these).
det(continuous,definite,that).
det(discrete(singular),definite,that).
det(discrete(plural),definite,those).

det(discrete(singular),indefinite,a).
det(discrete(singular),indefinite,another).
det(discrete(singular),indefinite,every).
det(discrete(singular),indefinite,one).

det(_,definite,the).
det(_,definite,my).
det(_,definite,your).
det(_,definite,his).
det(_,definite,her).
det(_,definite,our).
det(_,definite,their).

20 # det(A,B,C) ---> #det(A,B,C).
det(discrete(plural),indefinite,Num) ---> #card(plural,Num).

ord(Num) :- nth1(_N,[first,second,third,fourth,fifth,sixth,seventh,eighth],Num).
ord(W) :- member(W,[next,last,other]).



%noun_phrase(passive,here).
%noun_phrase(passive,there).

% quantifiers - don't know how to handle these properly yet
%quant(some,		passive, somewhere).
%quant(every,	passive, everywhere).
%quant(no,  		passive, nowhere).
%quant(any,   	passive,	anywhere).
quant(every,	passive, everything).
quant(no,  		passive, nothing).
quant(some,		passive, something).
quant(any,  	passive,	anything).
quant(every,	active, everyone).
quant(no,  		active, 'no one').
quant(some,		active, someone).
quant(any,		active, anyone).
quant(every,	active, everybody).
quant(no,  		active, nobody).
quant(some,		active, somebody).
quant(any,   	active, anybody).



prep ---> [P], #prep(P).

modal_vform(need,F) :- !, F\=gerund. 
modal_vform(seem,F) :- !, F\=gerund. 
modal_vform(go,F)   :- !, F=gerund.
modal_vform(_,_).
