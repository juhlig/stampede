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

-module(stampede_SUITE).

-compile(export_all).
-compile(nowarn_export_all).

-import(ct_helper, [doc/1]).
-import(stampede_test_helper, [collect_pids/0]).

all() ->
	ct_helper:all(?MODULE).

init_per_suite(Config) ->
	Config.

end_per_suite(_) ->
	ok.

init_per_testcase(_, Config) ->
	application:start(stampede_test),
	Config.

end_per_testcase(_, _) ->
	application:stop(stampede_test),
	ok.

stampede(_) ->
	doc("Ensure that an application is stampeded and all processes are dead."),
	Self=self(),
	ReportFun=fun (Pid) -> Self ! {killing, Pid}, true end,
	{ok, _}=stampede:start_herd(stampede_test, #{interval => {100, 100}, before_kill => ReportFun}),
	timer:sleep(10000),
	ok=stampede:stop_herd(stampede_test),
	Killed=do_receive_loop(),
	true=length(Killed)>0,
	false=lists:any(fun (Pid) -> erlang:is_process_alive(Pid) end, Killed),
	ok.

do_receive_loop() ->
	do_receive_loop([]).

do_receive_loop(Acc) ->
	receive
		{killing, Pid} ->
			do_receive_loop([Pid|Acc])
	after 1000 ->
		Acc
	end.
