export MODELS=ex_001 ex_002 ex_003

model:
	for MNAME in $$MODELS; do \
	    cp $$MNAME.json model.json ; \
	    node compile.js > $$MNAME.c; \
	    gcc $$MNAME.c -w -lm -lpopulation -lgsl -lgslcblas -shared -fPIC -o $$MNAME.dylib; \
	    rm model.json; \
	done

test:
	for MNAME in $$MODELS; do \
		gcc $$MNAME.c -Wall -lm -lpopulation -lgsl -lgslcblas -shared -fPIC -o $$MNAME.dylib; \
	done
