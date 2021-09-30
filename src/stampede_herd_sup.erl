%% Copyright (c) 2020-2021, Jan Uhlig <juhlig@hnc-agency.org>
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

-module(stampede_herd_sup).

-behavior(supervisor).

-export([start_link/2]).
-export([init/1]).
-export([get_top_sup/1]).

-spec start_link(pid(), stampede:opts()) -> {ok, pid()}.
start_link(TopSup, Opts) ->
	supervisor:start_link(?MODULE, {TopSup, Opts}).

init({TopSup, Opts}) ->
	Tab = ets:new(?MODULE, [public]),
	{
		ok,
		{
			#{
				strategy => one_for_all,
				intensity => 0
			},
			[
				#{
					id => stampede_tracer,
					start => {stampede_tracer, start_link, [TopSup, Tab]},
					shutdown => brutal_kill
				},
				#{
					id => stampede_herd,
					start => {stampede_herd, start_link, [Tab, Opts]},
					shutdown => brutal_kill
				}
			]
		}
	}.

get_top_sup(App) ->
        AppInfo=application:info(),
        {running, RunningApps}=lists:keyfind(running, 1, AppInfo),
        case lists:keyfind(App, 1, RunningApps) of
                false ->
                        {error, {not_started, App}};
                {App, undefined} ->
                        {error, {library, App}};
                {App, AppMaster} ->
                        {TopSup, _}=application_master:get_child(AppMaster),
                        {ok, TopSup}
        end.
