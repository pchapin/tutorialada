TutorialAda
===========

An Ada programming language tutorial with samples. This tutorial covers a variety of topics in
varying levels of depth. As with most things on GitHub, it is a work in progress.

You can download snapshots of this tutorial below.

+ A [PDF of the tutorial](http://www.pchapin.org/Ada/AdaCrash.pdf) itself.
+ A zip archive of the [source code of the
  samples](http://www.pchapin.org/Ada/samples.zip).

Pull requests against this repository are also welcome.

To build the document, you will need a suitable TeX installation. On Windows I recommend
[MiKTeX](https://miktex.org/), but other TeX installations would probably be fine. In the root
folder use the following commands:

    $ pdflatex main
    $ bibtex main
    $ pdflatex main
    $ pdflatex main

Yes, it is necessary to issue the same command multiple times. LaTeX relies on this to correctly
build cross-references, the table of contents, and similar things. The result of the commands
above is the file `main.pdf` which contains the tutorial. You may have to install some
supporting packages into your TeX installation. Some TeX installations can do this
automatically; others may need manual assistance. Let the error messages be your guide.

To run the samples, install the latest version of [GNAT
Community](https://www.adacore.com/community). Start the GPS integrated development environment
and load the project file `samples.gpr` in the `samples` folder. You can then use the various
menu items in GPS to build and run the sample programs.

In addition to the samples, this repository contains several larger example programs that
demonstrate Ada more fully or that show how a particular technology/library can be used in Ada.
These larger programs stand alone, meaning they each have their own GNAT project file. Currently
the tutorial document does not describe these additional programs, although that might change in
the future. The following examples are included:

* `ASIS`. The ASIS library allows you to write analysis programs for Ada without having to
  create your own parser and semantic analyzer. This example program is merely a skeleton.
  (Note: it might be more appropriate to demonstrate libadalang rather than ASIS).
  
* `Channel`. This program creates a simulated noisy channel and sends random data over that
  channel using different error detection and correction methods. It is very old code written by
  me to support a computer communications class I taught many years ago. The program is mature
  enough to be a little interesting, but it could benefit from more elaboration.
  
* `GtkAda`. This program is intended to demonstrate the GtkAda graphical library. Right now it
  is little more than a placeholder (it displays a single button). A more interesting
  application (like a graphical version of the Hexdump example?) would be welcome.

* `Hexdump`. This program is a simple command line hex viewer. It displays the contents of a
  file as a sequence of bytes in hex, along with the offset of each byte and its ASCII
  interpretation.
  
* `Huffman`. This program does a huffman encoding of the specified input file. It also displays
  some interesting information about the encoding process. The program is incomplete.

* `Win32Ada`. This program is intended to demonstrate the Win32Ada library that allows Ada to
  interface directly to the Windows OS. Right now it is little more than a placeholder. A more
  interesting application (again: a graphical version of the Hexdump example?) would be welcome.
  
* `XMLAda`. This program is nothing more than one of the samples in the XMLAda documentation. A
  more interesting application would be welcome.


Peter C. Chapin  
chapinp@acm.org  
