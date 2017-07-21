PROJECT = tlbot
PROJECT_DESCRIPTION = New project
PROJECT_VERSION = 0.1.0

DEPS = jsx epgsql sync
dep_epgsql = git https://github.com/epgsql/epgsql.git 3.2.0
LOCAL_DEPS = inets crypto ssl asn1 public_key


include erlang.mk
