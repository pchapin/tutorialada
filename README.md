TutorialAda
===========

This repository contains an Ada programming language tutorial with samples, covering a variety
of topics in varying levels of depth. As with most things on GitHub, it is a work in progress.

You can download a [snapshot of this tutorial](http://www.pchapin.org/Ada/AdaCrash.pdf) as a
single PDF file. Note that the snapshot might be relatively out of date compared to the contents
of this repository.

Pull requests against this repository are also welcome.

To build the document, you will need a suitable TeX installation. On Windows I recommend
[MiKTeX](https://miktex.org/), but other TeX installations should also work. In the root folder
use the following commands:

    $ pdflatex AdaCrash
    $ bibtex AdaCrash
    $ pdflatex AdaCrash
    $ pdflatex AdaCrash

It is necessary to issue the same command multiple times so LaTeX will correctly build the
cross-references, the table of contents, and similar things. The result of the commands above is
the file `AdaCrash.pdf`. You may have to install some supporting packages into your TeX
installation. Some TeX installations can do this automatically; others may need manual
assistance.

To run the samples, install the latest version of [Alire](https://alire.ada.dev/). More detailed
instructions can be found in the ``Hello, Ada`` section of the tutorial.

What follows is a brief description of the samples. See the README files in the individual
sample folders for more information.

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

* `hexdump_sample`. This program is a simple command line hex viewer. It displays the contents
  of a file as a sequence of bytes in hex, along with the offset of each byte and its ASCII
  interpretation.
  
* `huffman_sample`. This program does a Huffman encoding of the specified input file. It also
  displays some interesting information about the encoding process. The program is incomplete.

* `Interfacing`. This program demonstrates using Ada to interface to low-level code written in C
  and to hardware. The program is incomplete.

* `Networking`. This program demonstrates using Ada to write a simple client/server application.

* `Tasks`. This program demonstrates using Ada tasks to implement a simple producer/consumer
  application. The program is incomplete.

* `Win32Ada`. This program is intended to demonstrate the Win32Ada library that allows Ada to
  interface directly to the Windows OS. Right now it is little more than a placeholder. A more
  interesting application (again: a graphical version of the Hexdump example?) would be welcome.
  
* `XMLAda`. This program is nothing more than one of the samples in the XMLAda documentation. A
  more interesting application would be welcome.


Peter Chapin  
spicacality@kelseymountain.org  
