%% Copyright (c) 2019, Jan Uhlig <j.uhlig@mailingwork.de>
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

-type ref() :: term().
-export_type([ref/0]).

-type target() :: {application, atom()}
	        | {supervisor, pid() | atom()}.
-export_type([target/0]).

-type opts() :: #{
		interval => {non_neg_integer(), non_neg_integer()},
		before_kill => {fun((pid(), term()) -> boolean()), term()}}.
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
start_herd(Ref, {supervisor, TopSup}, Opts0) when is_pid(TopSup) ->
	DefaultOpts=#{interval => {5000, 5000},
		before_kill => stampede_callbacks:if_any()},
	Opts1=maps:merge(DefaultOpts, Opts0),
	stampede_sup:start_herd(Ref, TopSup, Opts1);
start_herd(_, _, _) ->
	error(badarg).

-spec stop_herd(ref()) -> ok | {error, term()}.
stop_herd(App) ->
	stampede_sup:stop_herd(App).

-spec stop_all() -> ok.
stop_all() ->
	stampede_sup:stop_all().
