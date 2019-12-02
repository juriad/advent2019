Run as:
```
xsltproc prog.xslt prog.xslt
```

Yes, the stylesheet is applied to itself. It reads input using an entity.

It transforms the input into:
```
<i0 op="">1</i0><i1>0</i1>...
```

Then it recursively applies loop which steps through the program always producing a new state of memory.

Second task builds upon the first one, trying all options by looping over a string of size 10000 split at every charater.
It takes it pretty long to finish.
