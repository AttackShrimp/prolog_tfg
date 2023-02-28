% Example from Justifications for Goal-Directed Constraint Answer Set Programming
% by Joaquín Arias, Manuel Carro, Zhuo Chen and Gopal Gupta
% https://arxiv.org/abs/2009.10238

%!read intraocularLens as: intraocular lens are recommended
%!read correctiveLens as: needs corrective lenses
%!read glasses as: wears glasses
%!read not(glasses) as: does not wear glasses
%!read contactLens as: wears contact lenses
%!read not(contactLens) as: does not wear contact lenses
%!read laserSurgery as: underwent laser surgery
%!read not(laserSurgery) as: did not undergo laser surgery
%!read shortSighted as: is short sighted
%!read not(shortSighted) as: is not short sighted
%!read tightOnMoney as: is tight on money
%!read not(tightOnMoney) as: is not tight on money
%!read caresPracticality as: cares about practicality
%!read not(caresPracticality) as: does not care about practicality
%!read afraidToTouchEyes as: is afraid to touch eyes
%!read not(afraidToTouchEyes) as: is not afraid to touch eyes
%!read longSighted as: is long sighted
%!read student as: is a student
%!read richParents as: has rich parents
%!read not(richParents) as: has not rich parents
%!read likesSports as: likes sports


intraocularLens :- 
  correctiveLens,
  not glasses,
  not contactLens.

laserSurgery :-
  shortSighted,
  not tightOnMoney.
  %not correctiveLens.  %%deleted to avoid circular calls

glasses :-
  correctiveLens,
  not caresPracticality,
  not contactLens.

contactLens :-
  correctiveLens,
  not afraidToTouchEyes,
  not longSighted,
  not glasses.

correctiveLens :- 
  shortSighted,
  not laserSurgery.

tightOnMoney :-
  student,
  not richParents.

caresPracticality :-
  likesSports.

%Peter, a patient
shortSighted.
student.
likesSports.
afraidToTouchEyes.

% query: intraocularLens