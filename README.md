# NBC/NXT/NXC compiler

Adapted from [Bricxcc](https://sourceforge.net/projects/bricxcc/) by John Hansen (see <http://bricxcc.sourceforge.net/nbc/>).
This is based on the latest available release, [1.2.1.r4](https://sourceforge.net/projects/bricxcc/files/NBC_NXC/NBC%20release%201.2.1%20r4/).
I just tried to compile it on a up-to-date Linux distribution (because this is the target language of my compiler project for a CS lesson), because the provided Linux binary was not working anymore.
I also moved and edited a few stuff, because the file organization was a mess.

**I do not plan on fixing bugs or anything** ;)
If you are a CS student in the University of Namur, you probably know where to find me for (some kind of) support, though.

Also, I'm not supporting **Windows** and **Mac** compilation at the moment (but PR are welcomed if someone is motivated enough, the tricky part is probably to get the pascal compiler and `libusb`).
[There are already binaries](https://sourceforge.net/projects/bricxcc/files/NBC_NXC/NBC%20release%201.2.1%20r4/) for those operating systems, but I didn't test them.

## Compile that in Linux

+ Check your package distribution for something that looks like `fpc` or `fp-compiler` and install it, or go to the page for the [latest version of the compiler](https://sourceforge.net/projects/freepascal/files/Linux/3.0.4/), download and install what corresponds to your distribution ;
+ Check your package distribution for something that looks like `libusb-dev` and install it ;
+ Clone this repository ;
+ Compile the compiler: `make all`. If everything goes well, there should be a `nbc` executable file in your folder.
+ Test that everything goes well, by using `./nbc tests/bools.nbc`. The output should look like

```
# Status: NBC compilation begins
# Status: Compiling for firmware version 128, NBC/NXC enhanced = FALSE
# Status: Loading NBC system files
# Status: Running NBC Preprocessor
# Status: Include path = /home/pierre/code/nbc-compiler/;/usr/local/include/nbc/;tests/
# Status: Processing include: NBCCommon.h
# Status: Compiling NBC source code
# Status: Finished compiling NBC source code
# Status: Finalizing dependencies
# Status: Optimizing at level 1
# Status: Build codespace references
# Status: Optimize mutexes
# Status: Compact the codespace
# Status: Remove unused labels
# Status: Compact the dataspace
# Status: Sort the dataspace
# Status: Generate raw dataspace data
# Status: Fill clump and codespace arrays
# Status: Update executable file header
# Status: Write file header to executable
# Status: Write dataspace to executable
# Status: Write clump data to executable
# Status: Write code to executable
# Status: Write optimized source to compiler output
# Status: Finished
```
+ If so, you are good :)


## Using it

There is some help:

```
$ ./nbc -help
Next Byte Codes Compiler version 1.2 (1.2.1.r4, built Sat 04 Apr 2020 12:07:06 PM CEST)
     Copyright (c) 2006-2010, John Hansen
Syntax: nbc [options] filename [options]

   -S=<portname>: specify port name (usb), brick resource name, or alias
   -d: download program
   -r: download and run program
   -b: treat input file as a binary file (don't compile it)
   -q: quiet
   -n: prevent the system file from being included
   -D=<sym>[=<value>]: define macro <sym>
   -x: decompile program
   -Z[1|2]: turn on compiler optimizations
   -ER=n: set maximum errors before aborting (0 == no limit)
   -PD=n: set max preprocessor recursion depth (default == 10)
   -O=<outfile> : specify output file
   -E=<filename> : write compiler messages to <filename>
   -I=<path>: search <path> for include files
   -nbc=<filename> : save NXC intermediate NBC code to <filename>
   -L=<filename> : generate code listing to <filename>
   -Y=<filename> : generate symbol table to <filename>
   -w[-|+] : warnings off or on (default is on)
   -sm[-|+] : status messages off or on (default is on)
   -EF : enhanced firmware
   -safecall: NXC will wrap all function calls in Acquire/Release
   -api: dump the API to stdout
   -v=n: set the targeted firmware version (default == 128, NXT 1.1 == 105)
   -help : display command line options
```

... But basically, the only thing you need is

```
/path/to/nbc /path/to/input.nbc -O=/path/to/output.rxe
```

The `-O=` part is only required if you wan to execute your program latter in a simulator, like [this one](http://schuelerlabor.informatik.rwth-aachen.de/roboter-simulator), I may plan a Linux port of that one, since [the code source is available](https://github.com/InfoSphereAC/RoboSim).

Also note that right after the execution of this command, `echo $?` is either `0` if the compilation went well, `1` if it is not the case, so that should allow for some CI/testing procedure.
