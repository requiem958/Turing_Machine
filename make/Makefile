# Michaël PÉRIN, Verimag / Université Grenoble-Alpes, Février 2017
#
# Part of the project TURING MACHINES FOR REAL
#
#
SOL =
#_{
#

PROJECT = MT2019

MIC = LC_by_MT Emulator Action Band Turing_Machine main
PRJ = $(MIC)
SOL = Exercise

tar_dir = _tar
prj_dir = _prj
web_dir = _web

SKL_FILES = $(addsuffix .ml.skl, $(PRJ))
PRJ_FILES = $(addprefix $(prj_dir)/, $(addsuffix .ml, $(PRJ)))
SOL_FILES = $(addprefix $(prj_dir)/, $(addsuffix .ml, $(SOL)))

GENERATED_FILES = $(addsuffix .ml, $(MIC))

# The DEMARKUP tool for processing .mic. files (mic = Mark In Code)
demarkup = $(MYBIN)/demarkup

#}_


MODULES = lib/Fresh lib/Option lib/Tricks lib/MyList lib/MyString lib/File lib/Dot lib/Pretty lib/MySet lib/Bit_Vector lib/Binary_Tree lib/Date lib/Logger lib/Color lib/Html lib/Pattern \
Symbol State Alphabet Band Action Turing_Machine Configuration Execution \
Turing_Machines/TM_Basic Turing_Machines/TM_2Bands Turing_Machines/TM_Busy_Beaver Universal/UTM Demo \
Emulator 
#Lambda_Calculus/Lambda_Calcul \
#Lambda_Calculus/LC_by_MT \

ML  = $(addsuffix  .ml, $(MODULES))
CMO = $(addsuffix .cmo, $(MODULES))
CMA = unix.cma
#/!\ graphics.cma is not available on all distribution

# The ocaml compiler with options
OCAMLC = ocamlc -w -8 -w -26 -I lib -I Turing_Machines -I Lambda_Calculus -I Universal

#The ocaml interpreter with options
OCAMLI = ledit -h .ledit_history ocaml -I lib -I Turing_Machines -I Lambda_Calculus -I Universal

# directory needed for logging execution traces
log_dir = _log
dot_dir = _dot

#_{
help_teacher:
	@make help
	@echo "Here are additional commands for teachers:"
	@echo "  make project_run"
	@echo "  make project_src"
	@echo "  make project_web"
	@echo "  make project_tar"
	@echo "  make clean_all"
#}_

help:
	@echo ""
	@echo "Here are the possible commands:"
	@echo "  make cmo ...... produces .cmo files. This command is used by"
	@echo "  make run ...... produces an executable called \"run\" and executes it"
	@echo "  make play ..... load the .cmo files in the ocaml interpreter. Type #use \"main.ml\";;  in the interpreter to execute the main function."
	@echo "  make clean .... delete compilation files"

cmo: $(ML) main.ml
	@$(OCAMLC) $(CMA) $(ML) main.ml -o run


play: $(log_dir) $(dot_dir)
	@make cmo
	@echo "#use \"main.ml\";;" > .ledit_history
	@echo "#use \"Universal/UTM.ml\";;" >> .ledit_history
	@echo "open Demo;;" >> .ledit_history
	@echo "demo();;" >> .ledit_history
	@$(OCAMLI) $(CMA) $(CMO)

run: $(log_dir) $(dot_dir)
	@make cmo
	@$(OCAMLC) $(CMA) $(CMO) main.ml -o run
	@./run

clean:
	@rm -f a.out run *.cm* *~ lib/*.cm* Universal/*.cm* Turing_Machines/*.cm*

#_{
# THE TEACHER MAKEFILE 

clean_all:
	@make clean
	@rm -fr $(prj_dir) $(tar_dir) $(log_dir) $(dot_dir) $(web_dir) *.skl


# SOURCE from .mic. files

%.ml: %.mic.ml
	@echo "$< ...[demarkup]... $@"
	@$(demarkup) "..{" "%c" "}.." $< > $<.mic_tmp.1
	@$(demarkup) "!{/(*" " /!\ DONT MODIFY THIS FILE ! modify the .mic.ml version instead " "}!/*)" $<.mic_tmp.1 > $<.mic_tmp.2
	@$(demarkup) "__{" "%c" "}__" $<.mic_tmp.2 > $<.mic_tmp.3
	@$(demarkup) "_{" "" "}_" $<.mic_tmp.3 > $@
	@rm -f *.mic_tmp.*


# SKELETON from .mic. files

skl: $(SKL_FILES)

%.ml.skl: %.mic.ml
	@echo "$< ...[demarkup]... $@"
	@$(demarkup) "..{"   ".."  "}.." $< > $<.mic_tmp.1
	@$(demarkup) "!{"   ""    "}!" $<.mic_tmp.1 > $<.mic_tmp.2
	@$(demarkup) "__{" "" "}__" $<.mic_tmp.2 > $<.mic_tmp.3
	@$(demarkup) "_{/(* PROJET: modifiez ce code -> *)"  "%c"   "}_" $<.mic_tmp.3 > $@
	@rm -f *.mic_tmp.*


# PROJECT FILES

project_run:
	@make project_src
	@echo "\nTo execute: cd $(prj_dir)/ ; make run"

project_web:
	@make project_src
	@mv $(prj_dir) $(web_dir)

project_tar:
	@make project_src
	@make prj_tar
	@echo "The archive is here: $(tar_dir)/$(PROJECT).tar"

project_src: $(prj_dir) $(prj_dir)/Makefile
	@echo "1) Completez les fichiers .ml: $(PRJ) \n2) Tapez: make" > $(prj_dir)/README
	@make clean
	@make prj_lib
	@make prj_skl


# RULES

$(prj_dir):
	@if test ! -d $(prj_dir); then mkdir $(prj_dir)/; fi

$(prj_dir)/Makefile: Makefile
	@echo "$< ...[demarkup]... $@"
	@$(demarkup) "!{"   ""    "}!" $< > $<.mic_tmp.1
	@$(demarkup) "__{" "" "}__" $<.mic_tmp.1 > $<.mic_tmp.2
	@$(demarkup) "_{" "" "}_" $<.mic_tmp.2 > $@


prj_lib: $(ML) main.ml
	cp $(ML) main.ml $(prj_dir)/
	rm $(PRJ_FILES)
	rm $(SOL_FILES)


prj_skl: $(PRJ_FILES)

$(prj_dir)/%.ml: %.mic.ml
	@echo "$< ...[demarkup]... $@"
	@$(demarkup) "..{"   ".."  "}.." $< > $<.mic_tmp.1
	@$(demarkup) "!{"   ""    "}!" $<.mic_tmp.1 > $<.mic_tmp.2
	@$(demarkup) "__{" "" "}__" $<.mic_tmp.2 > $<.mic_tmp.3
	@$(demarkup) "_{/(* PROJET: modifiez ce code -> *)"  "%c"   "}_" $<.mic_tmp.3 > $@
	@rm -f *.mic_tmp.*


prj_tar: $(tar_dir)
	@mv $(prj_dir)/ $(tar_dir)/$(PROJECT)/
	@cd $(tar_dir)/ ; tar -cf $(PROJECT).tar $(PROJECT)/

$(tar_dir):
	@if test ! -d $(tar_dir); then mkdir $(tar_dir)/; fi
#}_


%.ml: %.cp4
	@camlp4o -impl $< -o $@

#%.cmo: Turing_Machines/%.cmo
#	cp Turing_Machines/%< $@

%.cmo: %.ml
	@ocamlc *.cmo $<

$(log_dir):
	@if test ! -d $(log_dir); then mkdir $(log_dir)/; fi

$(dot_dir):
	@if test ! -d $(dot_dir); then mkdir $(dot_dir)/; fi

.PHONY: help cmo play run clean
