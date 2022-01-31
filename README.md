## PrologT: A simple Prolog tracer

### Instalation

You only need to download the files to a local folder.
The tool requires 
[SWI Prolog](https://www.swi-prolog.org/).

### Use 

A typical session looks like this:

```
swipl
Welcome to SWI-Prolog (threaded, 64 bits, version 8.2.4)
SWI-Prolog comes with ABSOLUTELY NO WARRANTY. This is free software.
Please run ?- license. for legal details.

For online help and background, visit https://www.swi-prolog.org
For built-in help, use ?- help(Topic). or ?- apropos(Word).

?- [tracer].
true.

?- load('examples/ancestor.pl').
true.

?- run([ancestor(A,fanny)]).
ancestor(anna,fanny)
   parent(anna,fanny)
      mother(anna,fanny)
;
ancestor(daniel,fanny)
   parent(daniel,fanny)
      mother(daniel,fanny)
;
ancestor(tim,fanny)
   parent(tim,anna)
      mother(tim,anna)
   ancestor(anna,fanny)
      parent(anna,fanny)
         mother(anna,fanny)
;
ancestor(celine,fanny)
   parent(celine,daniel)
      father(celine,daniel)
   ancestor(daniel,fanny)
      parent(daniel,fanny)
         mother(daniel,fanny)

true.
```
By default, the output is also saved in file ```temp.txt``` as follows:

```
[[call(0,ancestor(anna,fanny)),call(1,parent(anna,fanny)),call(2,mother(anna,fanny))],[call(0,ancestor(daniel,fanny)),call(1,parent(daniel,fanny)),call(2,mother(daniel,fanny))],[call(0,ancestor(tim,fanny)),call(1,parent(tim,anna)),call(2,mother(tim,anna)),call(1,ancestor(anna,fanny)),call(2,parent(anna,fanny)),call(3,mother(anna,fanny))],[call(0,ancestor(celine,fanny)),call(1,parent(celine,daniel)),call(2,father(celine,daniel)),call(1,ancestor(daniel,fanny)),call(2,parent(daniel,fanny)),call(3,mother(daniel,fanny))]]
```
The tracer can also be used as a shell command:

```
$ ./tracer_shell.pl 'examples/ancestor.pl' -g '[ancestor(A,fanny)]'
```

By default, the output in file ```temp.txt``` is the same as above. Alternatively, one can add ```-o text``` to get a textual representation instead (the same representation that is shown on the screen).

Comments or questions are welcome: [gvidal@dsic.upv.es](mailto:gvidal@dsic.upv.es).