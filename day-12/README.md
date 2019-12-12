Run as:
```
octave-cli --eval "[en, res] = prg.tasks('input', 1000)"
```

MATLAB, or Octave seemed to be a good choice, however as it turned out, performance was needed here.
I have a couple of remarks about the language:

* the language is rather slow for iterative tasks;
* printing being turned on/off by a semicolon is interesting;
* one function per file is very restricting;
* I could list destructing in for loops work.
