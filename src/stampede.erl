%% Copyright (c) 2020, Jan Uhlig <j.uhlig@mailingwork.de>
%%
%% Permission to use, copy, modify, and/or distribute this software for any
%% purpose with or without fee is hereby granted, provided that the above
%% copyright notice and this permission notice appear in all copies.
%%
%% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
%% WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
%% MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
%% ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
%% WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
%% ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
%% OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

-module(stampede).

-export([start_herd/2, start_herd/3]).
-export([stop_herd/1]).
-export([stop_all/0]).
-export([activate/1]).
-export([deactivate/1]).
-export([is_active/1]).
-export([default_opts/0]).
-export([set_opts/2]).
-export([get_opts/1]).

-type ref() :: term().
-export_type([ref/0]).

-type target() :: {application, atom()}
	        | {supervisor, pid() | atom()}.
-export_type([target/0]).

-type interval() :: {non_neg_integer(), non_neg_integer()}.
-export_type([interval/0]).

-type callback() :: fun((pid() | port()) -> boolean()).
-export_type([callback/0]).

-type opts() :: #{
		interval => interval(),
		before_kill => callback()
	}.
-export_type([opts/0]).

-spec start_herd(ref(), target()) -> supervisor:startchild_ret().
start_herd(Ref, AppOrSup) ->
	start_herd(Ref, AppOrSup, #{}).

-spec start_herd(ref(), target(), opts()) -> supervisor:startchild_ret().
start_herd(Ref, {application, App}, Opts) ->
	AppInfo=application:info(),
	{running, RunningApps}=lists:keyfind(running, 1, AppInfo),
	case lists:keyfind(App, 1, RunningApps) of
		false ->
			error(badarg);
		{App, undefined} ->
			error(badarg);
		{App, AppMaster} ->
			{TopSup, _}=application_master:get_child(AppMaster),
			start_herd(Ref, {supervisor, TopSup}, Opts)
	end;
start_herd(Ref, {supervisor, TopSupName}, Opts) when is_atom(TopSupName) ->
	case whereis(TopSupName) of
		TopSup when is_pid(TopSup) ->
			start_herd(Ref, {supervisor, TopSup}, Opts);
		_ ->
			error(badarg)
	end;
start_herd(Ref, {supervisor, TopSup}, Opts) when is_pid(TopSup) ->
	stampede_sup:start_herd(Ref, TopSup, validate_opts(Opts));
start_herd(_, _, _) ->
	error(badarg).

-spec stop_herd(ref()) -> ok | {error, term()}.
stop_herd(Ref) ->
	stampede_sup:stop_herd(Ref).

-spec stop_all() -> ok.
stop_all() ->
	stampede_sup:stop_all().

-spec activate(ref()) -> ok.
activate(Ref) ->
	stampede_sup:activate(Ref).

-spec deactivate(ref()) -> ok.
deactivate(Ref) ->
	stampede_sup:deactivate(Ref).

-spec is_active(ref()) -> boolean().
is_active(Ref) ->
	stampede_sup:is_active(Ref).

-spec default_opts() -> opts().
default_opts() ->
	#{
		interval => {5000, 5000},
		before_kill => stampede_callbacks:if_any()
	}.

-spec set_opts(ref(), opts()) -> ok.
set_opts(Ref, Opts) ->
	stampede_sup:set_opts(Ref, validate_opts(Opts)).

-spec get_opts(ref()) -> opts().
get_opts(Ref) ->
	stampede_sup:get_opts(Ref).

validate_opts(Opts) when is_map(Opts) ->
	case lists:all(fun validate_opt/1, maps:to_list(Opts)) of
		true ->
			maps:merge(default_opts(), Opts);
		false ->
			error(badarg)
	end;
validate_opts(_) ->
	error(badarg).

validate_opt({interval, {Min, Max}}) ->
	is_integer(Min) andalso Min>=0
	andalso is_integer(Max) andalso Max>=0
	andalso Min=<Max;
validate_opt({before_kill, Fun}) ->
	is_function(Fun, 1);
validate_opt(_) ->
	false.
