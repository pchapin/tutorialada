TutorialAda
===========

This repository contains an Ada programming language tutorial with samples, covering a variety
of topics in varying levels of depth. As with most things on GitHub, it is a work in progress.

You can download a [snapshot of this tutorial](http://www.pchapin.org/Ada/AdaCrash.pdf) as a
single PDF file. Note that the snapshot might be relatively out of date compared to the contents
of this repository since it is only refreshed occasionally.

Pull requests against this repository are welcome.

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
installation. Some TeX distributions can do this automatically; others may need manual
assistance.

To run the samples, install the latest version of [Alire](https://alire.ada.dev/), change to the
folder containing the sample of interest, and do `alr run`. More detailed instructions can be
found in the `Hello, Ada` section of the tutorial.

If you want to edit the samples, install [Visual Studio Code](https://code.visualstudio.com/),
along with the Ada extension from AdaCore. Use the command `alr edit` in each sample folder to
start VS Code on that sample with the environment configured appropriately.

The samples are intended to complement the tutorial by demonstrating Ada in a more realistic
context than found in typical tutorial examples. The samples are chosen to showcase some aspect
of Ada or some popular Ada library. They are written in an educational style, meaning they
contain more documentation about Ada itself than one typically finds in production code. The
samples are wildly variable in terms of their complexity, scope, and degree of completeness.

Stylistically, the samples follow the Ada open source community standard set by the default
configuration of Alire. However, in most cases there have been some (minor) adjustments made to
that standard. Each sample includes a sample-specific GNAT project file containing additional
compiler options reflecting the adjustments. Although one should avoid deviating from the
community standard set by Alire, in some cases adjustments seem reasonable and necessary,
particularly if done in moderation.

What follows is a brief description of each sample. See the README files in the individual
sample folders for more detailed and specific information.

* `channel_sample`. This program creates a simulated noisy channel and sends random data over
  that channel using different error detection and correction methods. It is very old code
  written by me to support a computer communications class I taught many years ago. The program
  is mature enough to be a little interesting, but it could benefit from more elaboration.
  
* `gtkada_sample`. This program is intended to demonstrate the GtkAda graphical library. Right
  now it is little more than a placeholder (it displays a single button). A more interesting
  application (like a graphical version of the Hexdump sample?) would be better. This sample is
  known to work on Windows, but has not been checked on macOS or Linux.

* `hexdump_sample`. This program is a simple command line hex viewer. It displays the contents
  of a file as a sequence of bytes in hex, along with the offset of each byte and its ASCII
  interpretation.
  
* `huffman_sample`. This program does a Huffman encoding (compression) of the specified input
  file. It also displays some interesting information about the encoding process.

* `interfacing_sample`. This program demonstrates using Ada to interface to low-level code
  written in C and to hardware.

* `tasks_sample`. This program demonstrates using Ada tasks to implement a simple
  producer/consumer application.
  
* `tutorial_samples`. This program is a collection of unrelated samples, many of which are
  presented in the text of the tutorial. It is menu-driven so you can pick which sample you want
  to run. It includes a demonstration of the AUnit testing framework.

* `win32ada_sample`. This program is intended to demonstrate the Win32Ada library that allows
  Ada to interface directly to the Windows OS. Right now it is just a placeholder. A more
  interesting application (again: a graphical version of the Hexdump sample?) would be better.
  This program is only expected to work on Windows.
  
* `xmlada_sample`. This program is intended to demonstrate the XMLAda library for processing XML
  documents. Right now it is nothing more than one of the samples in the XMLAda documentation.


Peter Chapin  
spicacality@kelseymountain.org  
