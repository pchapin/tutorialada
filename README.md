TutorialAda
===========

An Ada programming language tutorial with samples. This tutorial covers a variety of topics in
varying levels of depth. As with most things on GitHub, it is a work in progress.

You can download snapshots of this tutorial below.

+ A [PDF of the tutorial](http://www.pchapin.org/VTC/TutorialAda/AdaCrash.pdf) itself.
+ A zip archive of the [source code of the
  samples](http://www.pchapin.org/VTC/TutorialAda/samples.zip).

Pull requests against this repository are also welcome.

To build the document, you will need a suitable TeX installation. On Windows I recommend
[MiKTeX](https://miktex.org/), but other TeX installations would probably be fine. In the root
folder use the following commands:

    $ pdflatex main
    $ bibtex main
    $ pdflatex main
    $ pdflatex main

Yes, it is correct and necessary to issue the same command multiple times. LaTeX relies on this
to correctly build cross-references, the table of contents, and similar things. The result of
the commands above is the file `main.pdf` which contains the tutorial. You may have to install
some supporting packages into your TeX installation. Some TeX installations can do this
automatically; others may need manual assistance. Let the error messages be your guide.

To run the samples, install the latest version of [GNAT
Community](https://www.adacore.com/community). Start the GPS integrated development environment
and load the project file `samples.gpr` in the `samples` folder. You can then use the various
menu items in GPS to build and run the sample programs.

Peter C. Chapin  
chapinp@acm.org  
