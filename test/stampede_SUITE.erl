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

init_per_testcase(stampede_supervisor, Config) ->
	{ok, Pid}=stampede_test_sup_sup:start_link(),
	unlink(Pid),
	Config;
init_per_testcase(_, Config) ->
	application:start(stampede_test),
	Config.

end_per_testcase(stampede_supervisor, _) ->
	catch exit(whereis(stampede_test_sup_sup), shutdown),
	ok;
end_per_testcase(_, _) ->
	application:stop(stampede_test),
	ok.

stampede_application(_) ->
	doc("Ensure that an application is stampeded and all processes are dead."),
	Self=self(),
	ReportFun=fun (Pid) -> Self ! {killing, Pid}, true end,
	{ok, _}=stampede:start_herd(stampede_test, {application, stampede_test}, #{interval => {100, 100}, before_kill => ReportFun}),
	ok=stampede:activate(stampede_test),
	timer:sleep(10000),
	ok=stampede:stop_herd(stampede_test),
	Killed=do_receive_loop(),
	true=length(Killed)>0,
	false=lists:any(fun (Pid) when is_pid(Pid) -> erlang:is_process_alive(Pid); (Port) when is_port(Port) -> undefined=/=erlang:port_info(Port) end, Killed),
	ok.

stampede_supervisor(_) ->
	doc("Ensure that a supervisor is stampeded and all processes are dead."),
	Self=self(),
	ReportFun=fun (Pid) -> Self ! {killing, Pid}, true end,
	{ok, _}=stampede:start_herd(stampede_test, {supervisor, stampede_test_sup_sup}, #{interval => {100, 100}, before_kill => ReportFun}),
	ok=stampede:activate(stampede_test),
	timer:sleep(10000),
	ok=stampede:stop_herd(stampede_test),
	Killed=do_receive_loop(),
	true=length(Killed)>0,
	false=lists:any(fun (Pid) when is_pid(Pid) -> erlang:is_process_alive(Pid); (Port) when is_port(Port) -> undefined=/=erlang:port_info(Port) end, Killed),
	ok.

stampede_ports(_) ->
	doc("Ensure that the ports of an application are stampeded and all ports are dead."),
	Self=self(),
	PortsOnlyFun=stampede_callbacks:if_port(),
	ReportFun=fun (Port) -> Self ! {killing, Port}, true end,
	BeforeKillFun=stampede_callbacks:if_allof([PortsOnlyFun, ReportFun]),
	{ok, _}=stampede:start_herd(stampede_test, {application, stampede_test}, #{interval => {100, 100}, before_kill => BeforeKillFun}),
	ok=stampede:activate(stampede_test),
	timer:sleep(10000),
	ok=stampede:stop_herd(stampede_test),
	Killed=do_receive_loop(),
	true=length(Killed)>0,
	false=lists:any(fun (Port) -> undefined=/=erlang:port_info(Port) end, Killed),
	ok.

activate_deactivate(_) ->
	doc("Ensure that a stampede can be activated and deactivated."),
	Self=self(),
	ReportFun=fun (Pid) -> Self ! {killing, Pid}, true end,
	{ok, _}=stampede:start_herd(stampede_test, {application, stampede_test}, #{interval => {100, 100}, before_kill => ReportFun}),
	timer:sleep(1000),
	[]=do_receive_loop(),
	ok=stampede:activate(stampede_test),
	timer:sleep(1000),
	ok=stampede:deactivate(stampede_test),
	true=length(do_receive_loop())>0,
	timer:sleep(1000),
	[]=do_receive_loop(),
	ok=stampede:stop_herd(stampede_test),
	ok.

change_opts(_) ->
	Opts1=#{
		interval => {5000, 5000},
		before_kill => fun (_) -> _=1, false end
	},
	{ok, _}=stampede:start_herd(stampede_test, {application, stampede_test}, Opts1),
	ok=stampede:activate(stampede_test),
	Opts1=stampede:get_opts(stampede_test),
	Opts2=#{
		interval => {1000, 1000},
		before_kill => fun (_) -> _=2, false end
	},
	stampede:set_opts(stampede_test, Opts2),
	Opts2=stampede:get_opts(stampede_test),
	ok=stampede:stop_herd(stampede_test),
	ok.

do_receive_loop() ->
	do_receive_loop([]).

do_receive_loop(Acc) ->
	receive
		{killing, PidOrPort} ->
			do_receive_loop([PidOrPort|Acc])
	after 1000 ->
		Acc
	end.
