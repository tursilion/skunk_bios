JCP is the main program used for communication with the Skunkboard.

It requires LibUSB 0.1 to be available. For Windows, the .lib and header file are provided. 
For Linux or OSX, you must get these files yourself and set up LibUSB on your system correctly.

http://libusb.sourceforge.net/

The jcp.vcproj and libusb.lib files are used for people building under Windows.
The Makefile is used for people building under Linux or OSX.

For getting the libUSB drivers on modern Windows operating systems, this tool has been useful:
https://zadig.akeo.ie/

JCP and all original SKUNKBOARD properties (source, binary, design, etc) are released to the PUBLIC DOMAIN. No support is available, no warranty is possible, and you can do whatever you like with it, except claim ownership. :)

---

12 Sep 2008 - Added support for ABS binaries
21 Sep 2008 - Added patches to enable OSX to build (Thanks to Belboz)
 4 Oct 2008 - Added word write option for machines with flakey power. Refactored the code to help shell authors with better helper functions.
16 Oct 2008 - Moved word write into the ROM for BIOS 1.1.0, tweaked skunkCONSOLECLOSE to wait before closing, added BIOS upgrade code
21 Oct 2008 - Patch to above release - make file loading more safe, fix flash memory access error
25 Jan 2009 - Updated code for Skunkboard rev 2, fixes to Skunklib thanks to SebRmv
 4 Mar 2009 - Many more updates for SB revision 2, external console, additional safety tests
 6 Apr 2009 - BIOS 1.02.02 added. Improved -b parameter parsing. Implemented inline serial # check. RomDumper waits for console to close.


 