To compile the markdown files to html and run them locally you should only need

```
git clone https://github.com/epsilonhalbe/Talks
git submodule init
git submodule update
```

to compile the slides by yourself get

- [pandoc](http://pandoc.org/) best if you clone it from github and 
    - `git submodule init`
    - `git submodule update`
    - `stack install`

Note: the `deb` packages of pandoc are way outdated and the templates are even
older
