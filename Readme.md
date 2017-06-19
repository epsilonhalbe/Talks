To compile the markdown files to html and run them locally you should only need

```
git clone https://github.com/epsilonhalbe/Talks
git submodule init
git submodule update
cd <talk i want to build>
pandoc -o index.html -t revealjs -s Readme.md
```

Of course you need [pandoc](http://pandoc.org/) for that.

Note: the `deb` packages in the debian repository of pandoc are way outdated and the templates for revealjs are even older
