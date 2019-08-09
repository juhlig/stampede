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

-module(stampede_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([start_herd/3]).
-export([stop_herd/1, stop_all/0]).
-export([init/1]).

-spec start_link() -> {ok, pid()}.
start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

-spec start_herd(stampede:ref(), pid(), stampede:opts()) -> supervisor:startchild_ret().
start_herd(Ref, TopSup, Opts) ->
	supervisor:start_child(
		?MODULE,
		#{
			id => {stampede_herd, Ref},
			start => {stampede_herd_sup, start_link, [TopSup, Opts]},
			restart => temporary,
			type => supervisor
		}
	).

-spec stop_herd(stampede:ref()) -> ok | {error, term()}.
stop_herd(Ref) ->
	supervisor:terminate_child(?MODULE, {stampede_herd, Ref}).

-spec stop_all() -> ok.
stop_all() ->
	_=[catch supervisor:terminate_child(?MODULE, Id) || {Id={stampede_herd, _}, _, supervisor, _} <- supervisor:which_children(?MODULE)],
	ok.

init([]) ->
	{ok, {#{}, []}}.
