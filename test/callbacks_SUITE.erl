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

-module(callbacks_SUITE).

-compile(export_all).
-compile(nowarn_export_all).

-import(ct_helper, [doc/1]).
-import(stampede_test_helper, [collect_pids/0]).

all() ->
	ct_helper:all(?MODULE).

init_per_suite(Config) ->
	application:start(stampede_test),
	Config.

end_per_suite(_) ->
	application:stop(stampede_test).

cb_if_not(_) ->
	doc(""),
	Cb=stampede_callbacks:if_not(fun (_) -> false end),
	Pids=collect_pids(),
	true=lists:all(
		fun
			({_, _, _, Pid}) ->
				Cb(Pid)
		end,
		Pids
	),
	ok.

cb_if_any(_) ->
	doc("Ensure that the if_any callback works."),
	Cb=stampede_callbacks:if_any(),
	Pids=collect_pids(),
	true=lists:all(
		fun
			({_, _, _, Pid}) ->
				Cb(Pid)
		end,
		Pids
	),
	ok.

cb_if_supervisor(_) ->
	doc("Ensure that the if_supervisor callback works."),
	Cb=stampede_callbacks:if_supervisor(),
	Pids=collect_pids(),
	true=lists:all(
		fun
			({_, _, supervisor, Pid}) ->
				Cb(Pid);
			({_, _, worker, Pid}) ->
				not Cb(Pid)
		end,
		Pids
	),
	ok.

cb_if_module(_) ->
	doc("Ensure that the if_module callback works."),
	Module=stampede_test_wrk,
	Cb=stampede_callbacks:if_module([Module]),
	Pids=collect_pids(),
	true=lists:all(
		fun
			({_, Mod, _, Pid}) when Mod=:=Module ->
				Cb(Pid);
			({_, _, _, Pid}) ->
				not Cb(Pid)
		end,
		Pids
	),
	ok.

cb_if_child_infinite(_) ->
	doc("Ensure that the if_child callback works on infinite depth."),
	Pids=collect_pids(),
	Cb=stampede_callbacks:if_child(infinity, stampede_test_sup_sup),
	true=lists:all(
		fun
			({stampede_test_sup_sup, _, _, Pid}) ->
				not Cb(Pid);
			({_, _, _, Pid}) ->
				Cb(Pid)
		end,
		Pids
	),
	ok.

cb_if_child_direct(_) ->
	doc("Ensure that the if_child callback works on direct children."),
	Pids=collect_pids(),
	Cb=stampede_callbacks:if_child(1, stampede_test_sup_sup),
	true=lists:all(
		fun
			({{stampede_test_sup_sup, 1}, _, _, Pid}) ->
				Cb(Pid);
			({{stampede_test_sup_sup, 2}, _, _, Pid}) ->
				Cb(Pid);
			({_, _, _, Pid}) ->
				not Cb(Pid)
		end,
		Pids
	),
	ok.

cb_if_child_subsup(_) ->
	doc("Ensure that the if_child callback works on a sub-tree."),
	Pids=collect_pids(),
	{_, _, supervisor, Supervisor}=lists:keyfind({stampede_test_sup_sup, 1}, 1, Pids),
	Cb=stampede_callbacks:if_child(infinity, Supervisor),
	true=lists:all(
		fun
			({{stampede_test_sup, 1}, _, _, Pid}) ->
				Cb(Pid);
			({_, _, _, Pid}) ->
				not Cb(Pid)
		end,
		Pids
	),
	ok.
