/* 
	natural language grammar (English)
	Feature grammar With attributes and graph unification
*/

:- module(grammar_att, [ 
		(--->)/2
	,	(+-->)/2
	,	pron/5
	,	person/1
	,	gender/2, gender/1
	,	multiplicity/1
	,	agency/1
	,	nform/1
	,	vform/1
	,	tense/1
	,	check_mod/2
	,	substance/1
	,	quant_form/2
	,	verb_type/1, verb_type/2
	,	vpseq/2, vpseq/3
	,	being_type/1
	,	pathval/4
	,	unify/2
	,	denotes/2
	,	edit_args/3
	,	modal/2
	
	,	op(1200,xfx,--->)
	,	op(1200,xfx,+-->)
	,	op(950,xfx,includes)
	,	op(800,xfy,&)
	]).
	
:- op(1200,xfx,--->).
:- op(1200,xfx,+-->).
:- op(1190,xfx,#).
:- op(900,fx,#).
:- op(1180,xfx,\\).
:- op(950,xfx,includes).
:- op(800,xfy,&).
:- op(700,xfx,:=:).

:- dynamic substance/1.
:- dynamic emotion/1.

:- style_check(-discontiguous).

% lexicon

:- consult(dagunify).

	mapgoal( pname(_,_), pname(_,_)).
	mapgoal( gender(_,_), gender(_,_)).
	mapgoal( det(_,_,_), det(_,_,_)).
	mapgoal( adj(_,Ag,_), adj(_,Ag,_)).
	mapgoal( noun(_,_,_,_), noun(_,_,_,_)).
	mapgoal( pron(_,_,_,NF,_), pron(_,_,_,NF,_)).
	mapgoal( verb(T,_,_,_), verb(T,_,_,_)).


%att(Name,Value,F) :- pathval(Name,Value,F,_).
F includes Name=Value :- pathval(Name,Value,F,_).
F includes Name=Value & Z :- pathval(Name,Value,F,_), F includes Z.
A :=: B ---> {denotes(A,AA), denotes(B,BB), unify(AA,BB)}.

%att(Name,Value,F) ---> {pathval(Name,Value,F,_)}.
F includes Name=Value ---> {pathval(Name,Value,F,_)}.
F includes Name=Value & Z ---> {pathval(Name,Value,F,_)}, F includes Z.
%A :=: B ---> {denotes(A,AA), denotes(B,BB), unify(AA,BB)}.


denotes(A,A) :- var(A), !.
denotes(Dag:Path,Value) :- !, pathval(Path,Value,Dag,_).
denotes(K,K).

edit_args([],Args,Args).
edit_args([np(_,NP)],[np(_,NP)|Args],Args).
edit_args([object(NP,NP1)],[object(NP,NP1)|Args],Args).

vpargs([]) ---> [].
vpargs([A|AX]) ---> A, vpargs(AX).


 mapquery(maybe(A),maybe(A)).
maybe(_) ---> [].
maybe(A) ---> A.


% punctuation
fs ---> ['.'].
ex ---> ['!'].
qm ---> ['?'].
cm ---> [','].
sc ---> [';'].
cl ---> [':'].


	
s(S) ---> 
	#tense(Tense),
	NP:agreement :=: VP:agreement, 
	S:agreement :=: VP:agreement,
	S:tense :=: Tense,
	S:derivation :=: TD,
	np(nominative,NP),
	vp(tensed(Tense),TD,VP).

% what question
%  whnp, vp.
% whnp, aux, np, vp.


% -------- Verb phrase  
%
% !!!TODOO
% look at prepositional phrases again
% absorb maybe([not]) into verb phrase
% need to incorporate adverbs in to verb phrases

% map verb types to argument list
verb_type([A|AX],[A|AX]).
verb_type(nil,      [np(_,_) ]).
verb_type(p(Part),  [np(_,_), maybe([Part]) ]).
verb_type(s,        [np(_,_), maybe([that]),s]).
verb_type(np     ,  [np(_,NP), object(NP,_) ]).
verb_type(ap     ,  [
	np(_,NP), 
	NP includes agreement:number=N & agreement:agency=A, 
	adj(N,A,[],_) 
	]).

verb_type(ppp(Prep), [np(_,NP), [Prep], object(NP,_) ]).
verb_type(pp(Prep), [np(_,NP), [Prep], object(NP,_) ]).
verb_type(pp(_),    [np(_,_) ]).
verb_type(np-np  ,  [
	np(_,NP), 
	NP2:agreement:agency :=: active, object(NP,NP2), 
	NP3:agreement:agency :=: passive, object(NP,NP3) 
	]).

verb_type(np-pp(Prep),[ np(_,NP), object(NP,_), [Prep], object(NP,_)]).

verb_type(np(A2)-vp(VF),[
	np(_,NP),
	NP2:agreement:agency:=:A2, object(NP,NP2), 
	VP :=: NP, vp(VF,_,VP)]).

verb_type(np(A2)-s,[
	np(_,NP),
	NP2:agreement:agency:=:A2, object(NP,NP2), 
	maybe([that]),
	s]).


% for sampling verb types
verb_type(nil).
verb_type(np).
verb_type(p(_)).
verb_type(pp(_)).
verb_type(ppp(_)).
verb_type(np-np).
verb_type(np-pp(_)).
verb_type(np(_)-vp(_)).
verb_type(np(_)-s).
verb_type(s).


 mapquery( vp(_,_,_), vp(_,_,_)).
 mapquery( vp(VF,_,_,_), vp(VF,_,_,_)).

vp(VF,TD,VP) ---> vp(VF,TD,VP,[]).

% General rule for ordinary verbs with any arguments
4 # vp(VForm,[ordinary],VP,Gap) --->
	VP:agreement :=: NP:agreement,
	#verb_type(Type),
	vb(v(Type),VForm,VP),
	#verb_type(Type,[np(nominative,NP)|Args]),
	{edit_args(Gap,Args,Args1)},
	vpargs(Args1).


% ------------ based on TO BE -------------------------------

1 # vp(VForm,[being],VP,Gap) ---> 
	VP:agreement :=: NP:agreement,
	vb(bv(Type),VForm,VP),
	#verb_type(Type,[np(nominative,NP)|Args]),
	({VForm=tensed(_)}->maybe([not])),
	{edit_args(Gap,Args,Args1)},
	vpargs(Args1).

% subject BEGINS/GOES/BEGAN/WENT/IS BEGINNING TO/IS GOING TO to verb_infinitive
vp(VForm,[modal(infinitive)|TD],VP,Gap) --->
	vb(modal(infinitive),VForm,VP), 
	vp(infinitive,TD,VP,Gap).


% --------- Compound Tense variations ---------------------------------------

% EMPHATIC:  subject DOES/DID verb_infinitive 
vp(tensed(T),[emphatic|TD],VP,Gap) --->
	#vpseq(emphatic,NextVP),
	vb(emphatic,tensed(T),VP), maybe([not]),
	vp(bare,[NextVP|TD],VP,Gap).


% MODAL: eg subject WILL/CAN verb_inifinitive
vp(tensed(_),[modal|TD],VP,Gap) --->
	#modal(W,_), [W], 
	vp(bare,TD,VP,Gap).

% PERFECT: subject HAS/HAD verb_past participle 
vp(VForm,[perfect|TD],VP,Gap) --->
	{dif(VForm,past_part), dif(VForm,gerund)},
	vb(perfect,VForm,VP), 
	vp(past_part,TD,VP,Gap).


% PROGRESSIVE: subject IS gerund (eg is running)
vp(VForm,[progressive|TD],VP,Gap) ---> 
	{dif(VForm,gerund)},
	vb(progressive,VForm,VP),
	#vpseq(VForm,progressive,NextVP), {TD=[NextVP|_]},
	{writeln(TD:VP)},
%%	({VForm=tensed(_)}->maybe([not])),
	vp(gerund,TD,VP,Gap).

% negation of verb
vp(VForm,[not(V)|TD],VP,Gap) --->
	{when(nonvar(VForm),(VForm\=tensed(_),VForm\=imperative))},
	[not], vp(VForm,[V|TD],VP,Gap),
	{V\=not(_), \+member(not(_),TD)}.
/*
% ---------------  Derived forms

% imperative form
7 # vp(imperative,TD,VP,Gap) ---> 
	vp(bare,TD,VP,Gap),
	{\+member(not(_),TD)}. 

1 # vp(imperative,[not(V)|TD],VP,Gap) ---> 
	[do], vp(bare,[not(V)|TD],VP,Gap).


*/
vpseq(_,progressive,ordinary).
vpseq(tensed(_),progressive,modal(infinitive)).
vpseq(tensed(_),progressive,not(ordinary)).
vpseq(tensed(_),progressive,not(modal(infinitive))).
vpseq(tensed(_),progressive,being).
vpseq(tensed(_),progressive,not(being)).

vpseq(emphatic,modal(infinitive)).
vpseq(emphatic,ordinary).
vpseq(perfect,progressive).
vpseq(perfect,ordinary).
vpseq(perfect,modal(infinitive)).


agency(D,A) ---> D includes agreement:agency=A.
pna(D,P,N,A) --->
	D includes
		agreement:person=P &
		agreement:number=N &
		agreement:agency=A.

% -------- Verbs

 mapquery( vb(VF,tensed(_),_), vb(VF,tensed(_),_)).

vb(v(T),infinitive,A) ---> [to], vb(v(T),bare,A).
vb(emphatic,infinitive,A) ---> [to], vb(emphatic,bare,A).
vb(perfect,infinitive,A) ---> [to], vb(perfect,bare,A).



% standard verb form pattern 
vb(C,bare,D) ---> 			[W], agency(D,A), #verb(C,A,W,_).
vb(C,past_part,D) --->		[W], agency(D,A), #verb(C,A,R,M), {verb_morph(M,R,_,_,_,W)}.
vb(C,gerund,D) ---> 			[W], agency(D,A), #verb(C,A,R,M), {verb_morph(M,R,_,_,W,_)}.
vb(C,tensed(past),D) --->	[W], agency(D,A), #verb(C,A,R,M), {verb_morph(M,R,_,W,_,_)}.
vb(C,tensed(present),D) ---> 	pna(D,P,N,A), 
	#person(P), #multiplicity(N),
	#verb(C,A,R,_), {verb_morph(_,R,V,_,_,_)}, 
	( {P/N=third/singular} -> [V]; [R]).


% forms fo the verb to be
0.125 # vb(progressive,VForm,VB) ---> 
	pna(VB,P,N,_),
	#person(P), #multiplicity(N), 
	being_inflection(P,N,VForm). 

0.125 # vb(bv(Type),VForm,VB) ---> 
	pna(VB,P,N,_),
	#person(P), #multiplicity(N), #being_type(Type),
	being_inflection(P,N,VForm).


being_type([np(_,NP), being_prep, object(NP,_)]).
being_type([
	np(_,NP), 
	NP:agreement:number:=:N, 
	NP:agreement:agency:=:A, 
	adj(N,A,[],_)]).

being_type([
	np(_,NP), 
	NP2:agreement:number:=:NP:agreement:number,
	NP2:agreement:gender:=:NP:agreement:gender,
	object(NP,NP2)]).


being_prep ---> [Prep], #((prep(Prep),Prep\=to,Prep\=at)).

	mapquery( being_inflection(P,N,_), being_inflection(P,N,_)).
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

	mapquery(detp(M,_),detp(M,_)).

detp(continuous,_) ---> [].
detp(plural,_) ---> [].
detp(M,_) ---> #det(M,D,W), predet(M,D), [W].
detp(M,_) ---> superdet(M), number(M).
detp(M,A) ---> superdet(M), number(M), sp(M,A,[]).


% pre determiners
% all of, some of, N of, none of, no, the majority of ...
predet(_,_) ---> [].
predet(plural,definite) ---> [some,of].
predet(plural,definite) ---> [all].
predet(plural,definite) ---> [all,of].
predet(continuous,definite) ---> [all].
predet(continuous,definite) ---> [all,of].
predet(plural,definite) ---> [none,of].
predet(continuous,definite) ---> [none,of].

% determiners that can be followed by numerals, ordinals and superlative phrase
3 # superdet(M) ---> #det(M,definite,W), predet(M,definite), [W].
1 # superdet(M) ---> predet(M,definite), possessive(_).

	mapgoal(num(_,_),num(_,_)).
2  # number(plural) ---> [W], #num(N,W), {N>1}.
1  # number(singular) ---> [W], #ord(W).
9  # number(_) ---> [].

possessive(N) ---> #pname(_,N), {atom_concat(N,'\'s',W)}, [W].

% superlative phrase
sp(M,A,In) ---> #adj(M,A,W), {\+member(W,In)}, superlative(W).
sp(M,A,In) +--> #adj(M,A,W), {\+member(W,In)}, superlative(W), 
	[','], sp(M,A,[W|In]).


% -------- Nominals

1 # nn(NN) \\
	Ag includes number=singular & gender=G & agency=passive, 
	NN includes agreement=Ag ---> [N], 
	#noun(3,G,N,_).

1 # nn(NN)   ---> [N], 
	Ag includes number=plural & gender=G & agency=passive, 
	NN includes agreement=Ag,
	#noun(3,G,_,N).

1 # nn(NN) ---> [N], 
	Ag includes number=singular & gender=G & agency=active, 
	NN includes agreement=Ag,
	#noun(1,G,N,_).

1 # nn(NN)   ---> [N], 
	Ag includes number=plural & gender=G & agency=active, 
	NN includes agreement=Ag,
	#noun(1,G,_,N).

3 # nn(NN) ---> [S], 
	Ag includes number=singular & gender=G & agency=active, 
	NN includes agreement=Ag,
	#noun(active,2,_,S,_), 
	#gender(singular,G).

3 # nn(NN)  ---> [P], 
	Ag includes number=plural & gender=G & agency=active, 
	NN includes agreement=Ag,
	#noun(active,2,_,_,P), 
	#gender(plural,G).

3 # nn(NN) ---> [W], 
	Ag includes number=continuous & gender=neuter & agency=passive, 
	NN includes agreement=Ag,
	#substance(W).



% -------- Noun phrase  

2 # np(NF,NP) ---> [W],
	Ag includes person=P & number=M & gender=G & agency=active,
	NP includes agreement=Ag,
	#gender(M,G), 
	#pron(P,M,G,NF,W). 
	
5 # np(_,NP) ---> 
	NP includes agreement:person=third & agreement:number=M,
	NP :=: NNP,
	nnp(M,NNP).

5 # np(_,NP) --->
	NP includes agreement:person=third & agreement:number=singular,
	NP :=: NNP,
	nnp(continuous,NNP).

3 # np(_,NP) --->   
	NP includes
		agreement:person=third &
		agreement:number=singular &
		agreement:gender=G &
		agreement:agency=active,
	#pname(G,Name), 
	ap(proper,singular,active), [Name].

2 # np(NF,NP) ---> 
	NP includes
		agreement:person=third &
		agreement:number=singular & 
		agreement:agency=A, 
	#nform(NF), qp(NF,A).

nnp(M,NNP) ---> 
	NNP :=: NN,
	NNP:agreement:agency :=: A,
	detp(M,A), ap(ordinary,M,A), nn(NN).



% Quantified nouns like everything, something, nothing etc
	mapquery(qp(NF,_),qp(NF,_)).
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
	mapquery(object(P,_,_,A,_,_,_,_),object(P,_,_,A,_,_,_,_)).
9 # object(_,NP) ---> np(accusative,NP).
1 # object(NP1,NP2) ---> [W], 
	NP1:=:NP2, 
	NP2 includes
		agreement:person=P &
		agreement:gender=G &
		agreement:number=N,
	#pron(P,N,G,reflexive,W).

% -------- Adjectives

 mapquery( ap(T,_,_), ap(T,_,_)).
 mapquery( apx(_,_,_), apx(_,_,_)).

% adjectival phrase, first arg to segregate probability tables
% either no adjective or some..
ap(_,_,_) ---> [].
ap(_,M,A) ---> apx(M,A,[]-_).

% ap helper - recursive, keeps track of adjectives used so far
% number of adjectives has exponential distribution
apx(M,A,Adj1-_) ---> adj(M,A,Adj1,_).
apx(M,A,Adj1-T) +--> adj(M,A,Adj1,Adj2), adj_comma(T), apx(M,A,Adj2-T).


	mapquery(adj_comma(_),adj_comma(_)).
1 # adj_comma(false) ---> [].
3 # adj_comma(true) ---> [','].

1 # rel_comma(false) ---> [].
3 # rel_comma(true) ---> [','].


% -------- Adjectival phrases

 mapquery( adj(_,_,_,_), adj(_,_,_,_)).

adj(M,A,In,Out) ---> #agency(A), adj(M,A,In,Out,[]).
adj(M,A,In,[W:Mods|In],Mods) ---> [W], #adj(M,A,W), check_adj(W:Mods,In). 
adj(M,A,In,Out,Mods) +--> mod(Mods,Mods2), adj(M,A,In,Out,Mods2).
0.125 # adj(M,active,In,In,[]) ---> vb(v(nil),_,M,gerund,active). 

% adjective modifier
 mapquery( mod(_,_), mod(_,_)).
mod(In,[Mod|In]) ---> #mod(Mod), {check_mod(Mod,In)}, [Mod]. 

% allow/disallow particular chains of adjectives
% small prob of allowing double adjective
8 # check_adj(_,[]) ---> [].
1 # check_adj(X:_,[_|Z]) ---> {\+member(X:_,Z)}.
7 # check_adj(X:_,[Y|Z]) ---> {\+member(X:_,[Y|Z])}.

% allow/disallow certain chains of modifiers
check_mod(_,[]).
check_mod(very,[very|_]). % allowed to repeat very
check_mod(slightly,[very|_]). % allowed to follow very with slightly


20 # comparative(A)  ---> {adj(_,A,rel(B,_))}, [B].
1 # comparative(A)  ---> [more], [A].
20 # superlative(A)  ---> {adj(_,A,rel(_,B))}, [B].
1 # superlative(A)  ---> [most], [A].



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
