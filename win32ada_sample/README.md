
README
======

This sample demonstrates the Win32Ada library. Win32Ada is a thin Ada binding to the Win32 API.
The binding is thin in the sense that it exposed many low-level, C-like aspects of the API
without trying to wrap them in a more idiomatically Ada interface. It could be used as the bases
for a thick binding. It can also be used at the back-end for packages providing various
Windows-specific services.

To use Win32Ada, you need to do:

    $ alr with win32ada
    
This has already been done in this sample, of course.

It is also necessary to adjust the project GPR file to "with" `shared.gpr` and to include
certain additional complier switches. See `win32ada_sample.gpr` for the specifics.
