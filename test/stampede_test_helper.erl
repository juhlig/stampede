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

-module(stampede_test_helper).

-export([collect_pids/0]).

collect_pids() ->
	Children=supervisor:which_children(stampede_test_sup_sup),
	First={stampede_test_sup_sup, stampede_test_sup_sup, supervisor, whereis(stampede_test_sup_sup)},
	collect_pids(Children, [First]).

collect_pids([], Acc) ->
	Acc;
collect_pids([{Id, Pid, supervisor, [Mod]}|Children], Acc0) ->
	Acc1=collect_pids(supervisor:which_children(Pid), [{Id, Mod, supervisor, Pid}|Acc0]),
	collect_pids(Children, Acc1);
collect_pids([{Id, Pid, worker, [Mod]}|Children], Acc) ->
	collect_pids(Children, [{Id, Mod, worker, Pid}|Acc]).
