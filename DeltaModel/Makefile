include make.inc

$(EXE) : $(OBJS)
	$(LD) -o $(EXE) $(OBJS) 

$(OBJS): $(SRC)
	$(FC) $(INC) -o deltamodel16.o $(SRC) $(OFLG)

$(LIB): force_look
	$(ECHO) looking into lib : $(MAKE) $(MFLAGS)
	cd lib; $(MAKE) $(MFLAGS)

omp:		
	$(PFC) -openmp $(INC) -o deltamodel16.o $(SRC)
	$(LD) -openmp -o $(EXE) $(OBJS)

# Possibly add the flag -fpe0 to compilation and linking lines to increase floating point precission. If it does not work, delete it.
noomp:		
	
	$(PFCNOOMP) -fpp $(INC) -o deltamodel16.o $(SRC)
	$(LDNOOMP) -o $(EXE) $(OBJS)

cleanout: 
	rm -f *.out out_* *.sh.e*

ompnofast:	
	rm -f *.out out_*
	$(PFCNF) $(INC) -o deltamodel16.o $(SRC)
	$(LDNF) -openmp -o $(EXE) $(OBJS)

debug: 
	#cd lib; $(MAKE) debug $(MFLAGS)
	$(DFC) $(INC) -o deltamodel16.o $(SRC)
	$(DLD) -g -o $(EXE) $(OBJS) 

test:  force_look 		
	cd test/scripts; qsub test1.pbs; qsub test2.pbs

clean:
	echo $(NOOMPMESSAGE)
	rm -f *.mod *.o $(EXE)
	#cd lib; $(MAKE) clean $(MFLAGS)

veryclean:
	echo $(NOOMPMESSAGE)
	rm -f *.mod *.o *.pdb *.hpcstruct $(EXE)
	cd lib; $(MAKE) clean $(MFLAGS)
	rm -f test/scripts/*.e
	rm -f test/scripts/*.o

force_look :
	true

