
GtkAda
======

This folder contains a basic "Hello World!" sample program for GtkAda. Before loading the
hello.gpr project file (for example, into GPS), be sure your `GPR_PROJECT_PATH` environment
variable includes the folder in which gtkada.gpr is located. In the case of the Windows version
of GtkAda this should be C:\GtkAda\lib\gnat.

If you are using Windows GtkAda v2.8.0 you will need to make some adjustments to the GtkAda
installation. The provided gtkada.gpr file is faulty. Do the following.

    1. Edit gtkada.gpr to include the line

           for Object_Dir use "../../include/gtkada";

       Also change the linker option -ljpeg to -ljpeg6b.
   
    2. Copy the following three files from C:\GtkAda\bin to C:\GtkAda\include\gtkada

        iconv.dll
        libjpeg6b.dll
        libpng.dll
    
Without the first change, the compiler will not be able to locate the compiled package
specifications. Without the second change, the linker will complain about missing libraries.

Finally note that you may need to have C:\GtkAda\bin in your PATH so that your running program
can find the DLLs it needs (how necessary this is may depend on exactly what features of GtkAda
you use).
