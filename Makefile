PROJECT = stampede
PROJECT_DESCRIPTION = Application resilience testing
PROJECT_VERSION = 0.4.0

TEST_DEPS = ct_helper
dep_ct_helper = git https://github.com/ninenines/ct_helper master

CT_OPTS += -pa ebin -ct_hooks stampede_ct_hook []

DOC_DEPS = asciideck

include erlang.mk
