% visible: throws/1

0.5::lucky(suzy).

throws(suzy) :- lucky(suzy).
throws(billy).

%0.8::broken; 0.2::miss :- throws(suzy).
%0.6::broken; 0.4::miss :- throws(billy).

0.8::broken :- throws(suzy).
0.6::broken :- throws(billy).

query(broken).
