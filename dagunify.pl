:- module(dagunify, [ unify/2, pathval/4 ]).
/** <module> - Unification of DAGS
  originally written by Bob Carpenter

  A path is a colon separated sequence of features.
  a dag is a list of Feature:Value pairs, where a value
  can be another dag.
*/

%% unify(D1:dag, D2:dag) is det.
%  unify two side-ways open DAGs, encoded as open-ended lists.
unify(Dag,Dag) :- !.
unify([Path:Value|Dags1],Dag) :-
	pathval(Path,Value,Dag,Dags2),
	unify(Dags1,Dags2).

%% pathval(P:path, V:val, D1:dag, Ds:dag) is det.
%
% Find the value of a DAG at a path, returning the remainder of
% the dag (without that feature and value).  Note that you can
% provide a partially specified DAG as Value and the actual value
% will be unified with it (this fact is used in the clause for
% unification above).
pathval(Feature:Path,Value,Dag1,Dags) :-
	!, pathval(Feature,Dag2,Dag1,Dags),
	pathval(Path,Value,Dag2,_).
	
pathval(Feature,Value2,[Feature:Value1|Dags],Dags) :-
	!, unify(Value1,Value2).

pathval(Feature,Value,[Dag|Dags1],[Dag|Dags2]) :-
	pathval(Feature,Value,Dags1,Dags2).

