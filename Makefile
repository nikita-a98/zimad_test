PROJECT = zimad_test
PROJECT_DESCRIPTION = Test task for zimad company
PROJECT_VERSION = 0.0.1
PROJECT_MOD = zimad_app
PROJECT_REGISTERED = zimad_sup

ERLC_OPTS = +warn_missing_spec
ERLC_OPTS += +warn_unused_vars +warn_unused_function
ERLC_OPTS += +warn_unused_import +warn_unused_record
ERLC_OPTS += +warn_deprecated_function +warn_deprecated_type
ERLC_OPTS += +warn_export_vars +warn_shadow_vars +warn_obsolete_guard
ERLC_OPTS += +warn_export_all +warn_untyped_record
#ERLC_OPTS += +debug_info +bin_opt_info
#ERLC_OPTS += +warnings_as_errors

LOCAL_DEPS = kernel stdlib crypto public_key inets ssl asn1 mnesia os_mon
LOCAL_DEPS += runtime_tools observer wx sasl tools common_test debugger et

DEPS += cowboy jiffy uuid lager #observer_cli
dep_cowboy = git https://github.com/ninenines/cowboy.git 2.6.1
dep_jiffy = git https://github.com/davisp/jiffy.git 0.15.2
dep_uuid = git https://github.com/okeuday/uuid.git v1.7.4
dep_lager = git https://github.com/erlang-lager/lager.git 3.6.7
#dep_observer_cli = git https://github.com/zhongwencool/observer_cli.git 1.4.0

include erlang.mk
