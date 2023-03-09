## Instrumenter
### Use 

To use:

```
Welcome to SWI-Prolog (threaded, 64 bits, version 7.6.4)
SWI-Prolog comes with ABSOLUTELY NO WARRANTY. This is free software.
Please run ?- license. for legal details.

For online help and background, visit http://www.swi-prolog.org
For built-in help, use ?- help(Topic). or ?- apropos(Word).

?- [tfg].
true.

?- load('test.pl').
true.

?- ancestor(tim,fanny).

____________________ CODE COVERAGE ____________________

  0  mother/2               # 1       G 0.11    L 0.50
  1  mother/2               # 1       G 0.11    L 0.50
  2  mother/2               # 0       G 0.00    L 0.00
  3  mother/2               # 0       G 0.00    L 0.00
  4  father/2               # 0       G 0.00    L 0.00
  5  father/2               # 0       G 0.00    L 0.00
  6  father/2               # 0       G 0.00    L 0.00
  7  father/2               # 0       G 0.00    L 0.00
  8  parent/2               # 3       G 0.33    L 0.75
  9  parent/2               # 1       G 0.11    L 0.25
 10  ancestor/2             # 2       G 0.22    L 0.67
 11  ancestor/2             # 1       G 0.11    L 0.33
true.

```

It is also possible to chain multiple commands:

```
Welcome to SWI-Prolog (threaded, 64 bits, version 7.6.4)
SWI-Prolog comes with ABSOLUTELY NO WARRANTY. This is free software.
Please run ?- license. for legal details.

For online help and background, visit http://www.swi-prolog.org
For built-in help, use ?- help(Topic). or ?- apropos(Word).

?- [tfg].
true.

?- load('test.pl', []).
true.

?- ancestor(tim,fanny).
true .

?- ancestor(tim,anna).
true .

?- coverage.

____________________ CODE COVERAGE ____________________

  0  mother/2               # 2       G 0.17    L 0.67
  1  mother/2               # 1       G 0.08    L 0.33
  2  mother/2               # 0       G 0.00    L 0.00
  3  mother/2               # 0       G 0.00    L 0.00
  4  father/2               # 0       G 0.00    L 0.00
  5  father/2               # 0       G 0.00    L 0.00
  6  father/2               # 0       G 0.00    L 0.00
  7  father/2               # 0       G 0.00    L 0.00
  8  parent/2               # 4       G 0.33    L 0.80
  9  parent/2               # 1       G 0.08    L 0.20
 10  ancestor/2             # 3       G 0.25    L 0.75
 11  ancestor/2             # 1       G 0.08    L 0.25
true.
```
These commands can also be entered in a file:

```
Welcome to SWI-Prolog (threaded, 64 bits, version 7.6.4)
SWI-Prolog comes with ABSOLUTELY NO WARRANTY. This is free software.
Please run ?- license. for legal details.

For online help and background, visit http://www.swi-prolog.org
For built-in help, use ?- help(Topic). or ?- apropos(Word).

?- [tfg].
true.

?- load('test.pl', []).
true.

?- query('test_queries.plt').

____________________ CODE COVERAGE ____________________

  0  mother/2               # 2       G 0.17    L 0.67
  1  mother/2               # 1       G 0.08    L 0.33
  2  mother/2               # 0       G 0.00    L 0.00
  3  mother/2               # 0       G 0.00    L 0.00
  4  father/2               # 0       G 0.00    L 0.00
  5  father/2               # 0       G 0.00    L 0.00
  6  father/2               # 0       G 0.00    L 0.00
  7  father/2               # 0       G 0.00    L 0.00
  8  parent/2               # 4       G 0.33    L 0.80
  9  parent/2               # 1       G 0.08    L 0.20
 10  ancestor/2             # 3       G 0.25    L 0.75
 11  ancestor/2             # 1       G 0.08    L 0.25
true.
```
Where test_queries.plt is:
```
ancestor(tim,fanny).
ancestor(tim,anna).
```