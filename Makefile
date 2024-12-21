PROJECT = eschat
PROJECT_DESCRIPTION = Erlang Simple Chat
PROJECT_VERSION = 0.1.0

DEPS += cowboy
DEPS += jsone
DEPS += epgsql
DEPS += lager
DEPS += sherlock

dep_cowboy   = git https://github.com/ninenines/cowboy 		2.9.0
dep_jsone	 = git https://github.com/sile/jsone 			master
dep_epgsql	 = git https://github.com/epgsql/epgsql			master
dep_lager	 = git https://github.com/erlang-lager/lager	master
dep_sherlock = git https://github.com/andranat-b/sherlock 	master

REL_DEPS += relx

ERLC_OPTS += '+{parse_transform, lager_transform}'
ERLC_OPTS += '+inline'

include erlang.mk
