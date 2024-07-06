TARGET = ./galaxiasm
SRCDIR = ./src
OBJDIR = ./obj
OUTDIR = ./out
COMMON_FLAGS = -O2 -i$(SRCDIR) -odir $(OBJDIR) -hidir $(OBJDIR) -W

# $(TARGET) is a phony target: ghc can already recompile based on dependencies
# changing, so defer to it.
.PHONY: clean $(TARGET) profiling test tests prettify

$(TARGET): $(OBJDIR)
		ghc $(COMMON_FLAGS) -o $(TARGET) $(SRCDIR)/main 

profiling: $(OBJDIR) 
		ghc $(COMMON_FLAGS) $(TARGET) $(SRCDIR)/main -o -prof -fprof-late $(SRCDIR)/main

test:
		ghc -i$(SRCDIR) $(SRCDIR)/test -W --run

tests: test
	@echo "All tests passed"

$(OBJDIR):
		mkdir -p obj

clean:
		rm -f $(TARGET)
		rm -rf $(OBJDIR)/*
		rm -rf $(OUTDIR)/*

# stylish-haskell can help draw attention to problems, but doesn't currently
# have an option for aligning the imports quite as we want them.
# (See https://github.com/haskell/stylish-haskell/issues/472)
pretty:
	stylish-haskell -r -i $(SRCDIR)
