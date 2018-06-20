PROJECT = janga_core
DIALYZER = dialyzer
REBAR = rebar
REPO = ../repository
REPOSRC = ../repository
TARGET = ~/projects/erlang/janga/deps
DATE = `date +%Y-%m-%d`
CRASH_DIR = ../../crasher

all: app

tar: 
	cd ..; tar cv --exclude='.git' --exclude='.DS_Store' --exclude='logs' --exclude='Mnesia*' -f $(REPO)/$(PROJECT).$(VERSION).tar  $(PROJECT) 

dist: tar
	 cd ..;scp $(REPO)/$(PROJECT).$(VERSION).tar $(USR)@$(HOST):$(TARGET)
	 ssh $(USR)@$(HOST) 'cd $(TARGET); tar xf $(TARGET)/$(PROJECT).$(VERSION).tar; rm $(TARGET)/$(PROJECT).$(VERSION).tar'

app: deps
	@$(REBAR) compile

deps:
	@$(REBAR) get-deps

clean:	
	rm -f test/*.beam
	rm -f erl_crash.dump
	rm -rf log/*
	rmdir log

tests: 
	$(REBAR) skip_deps=true eunit

eunit:
	@$(REBAR) eunit skip_deps=true
