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

-export([start_herd/1, start_herd/2]).
-export([stop_herd/1]).
-export([stop_all/0]).

-type opts() :: #{
		interval => {non_neg_integer(), non_neg_integer()},
		before_kill => {fun((pid(), term()) -> boolean()), term()}}.
-export_type([opts/0]).

-spec start_herd(atom()) -> supervisor:startchild_ret().
start_herd(App) ->
	start_herd(App, #{}).

-spec start_herd(atom(), opts()) -> supervisor:startchild_ret().
start_herd(App, Opts0) ->
	DefaultOpts=#{interval => {5000, 5000},
		before_kill => stampede_callbacks:if_any()},
	Opts1=maps:merge(DefaultOpts, Opts0),
	stampede_sup:start_herd(App, Opts1).

-spec stop_herd(atom()) -> ok | {error, term()}.
stop_herd(App) ->
	stampede_sup:stop_herd(App).

-spec stop_all() -> ok.
stop_all() ->
	stampede_sup:stop_all().
