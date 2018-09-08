#====================================================================
#       Macro & Assembler flags
#====================================================================

STADDR = 80000
#MACFLAGS = -fb -i../../include -llist.txt
MACFLAGS = -fb -i../../include
#ALNFLAGS = -w -v -v -m -e -rd -g -l -a $(STADDR) x 4000
ALNFLAGS = -w -v -v -e -rd -a $(STADDR) x 4000


#====================================================================
#       Default Rules
#====================================================================
.SUFFIXES:      .o .s

.s.o:
	mac $(MACFLAGS) $<

#====================================================================
#       EXECUTABLES
#====================================================================

OBJ = startup.o

jagmand.cof: $(OBJ)
	aln $(ALNFLAGS) -o jagmand.cof $(OBJ)
	
jagbjl.cof: startbjl.o
	aln -w -v -v -e -rd -a 1400 x x -o startbjl.cof startbjl.o


#############################################################################

startup.o: startup.s

clean:
	$(RM) $(OBJ) jagmand.cof