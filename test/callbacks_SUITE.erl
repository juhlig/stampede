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

-module(callbacks_SUITE).

-compile(export_all).
-compile(nowarn_export_all).

-import(ct_helper, [doc/1]).
-import(stampede_test_helper, [collect_pids/0]).

-ifndef(OTP_RELEASE).
-define(OTP_RELEASE, 0).
-endif.

all() ->
	ct_helper:all(?MODULE).

init_per_suite(Config) ->
	application:start(stampede_test),
	Config.

end_per_suite(_) ->
	application:stop(stampede_test).

cb_if_anyof(_) ->
	doc("Ensure that the if_anyof callback works"),
	CbGen=fun (Results) -> stampede_callbacks:if_anyof([fun (_) -> Result end || Result <- Results]) end,
	Pid=self(),
	false=(CbGen([]))(Pid),
	true=(CbGen([true]))(Pid),
	false=(CbGen([false]))(Pid),
	true=(CbGen([true, true]))(Pid),
	true=(CbGen([true, false]))(Pid),
	true=(CbGen([false, true]))(Pid),
	false=(CbGen([false, false]))(Pid),
	true=(CbGen([true, true, true]))(Pid),
	true=(CbGen([true, true, false]))(Pid),
	true=(CbGen([true, false, true]))(Pid),
	true=(CbGen([true, false, false]))(Pid),
	true=(CbGen([false, true, true]))(Pid),
	true=(CbGen([false, true, false]))(Pid),
	true=(CbGen([false, false, true]))(Pid),
	false=(CbGen([false, false, false]))(Pid),
	ok.

cb_if_allof(_) ->
	doc("Ensure that the if_allof callback works"),
	CbGen=fun (Results) -> stampede_callbacks:if_allof([fun (_) -> Result end || Result <- Results]) end,
	Pid=self(),
	true=(CbGen([]))(Pid),
	true=(CbGen([true]))(Pid),
	false=(CbGen([false]))(Pid),
	true=(CbGen([true, true]))(Pid),
	false=(CbGen([true, false]))(Pid),
	false=(CbGen([false, true]))(Pid),
	false=(CbGen([false, false]))(Pid),
	true=(CbGen([true, true, true]))(Pid),
	false=(CbGen([true, true, false]))(Pid),
	false=(CbGen([true, false, true]))(Pid),
	false=(CbGen([true, false, false]))(Pid),
	false=(CbGen([false, true, true]))(Pid),
	false=(CbGen([false, true, false]))(Pid),
	false=(CbGen([false, false, true]))(Pid),
	false=(CbGen([false, false, false]))(Pid),
	ok.

cb_if_not(_) ->
	doc("Ensure that the if_not callback works."),
	Pid=self(),
	true=(stampede_callbacks:if_not(fun (_) -> false end))(Pid),
	false=(stampede_callbacks:if_not(fun (_) -> true end))(Pid),
	ok.

cb_if_process(_) ->
	doc("Ensure that the if_process callback works."),
	true=(stampede_callbacks:if_process())(self()),
	false=(stampede_callbacks:if_process())(do_make_port()),
	ok.

cb_if_port(_) ->
	doc("Ensure that the if_port callback works."),
	true=(stampede_callbacks:if_port())(do_make_port()),
	false=(stampede_callbacks:if_port())(self()),
	ok.

cb_if_any(_) ->
	doc("Ensure that the if_any callback works."),
	true=(stampede_callbacks:if_any())(self()),
	true=(stampede_callbacks:if_any())(do_make_port()),
	ok.

cb_if_supervisor(_) ->
	doc("Ensure that the if_supervisor callback works."),
	Cb=stampede_callbacks:if_supervisor(),
	true=lists:all(
		fun
			({_, _, supervisor, Pid}) ->
				Cb(Pid);
			({_, _, worker, Pid}) ->
				not Cb(Pid)
		end,
		collect_pids()
	),
	ok.

cb_if_module(_) ->
	doc("Ensure that the if_module callback works."),
	Module=stampede_test_wrk,
	Cb=stampede_callbacks:if_module([Module]),
	true=lists:all(
		fun
			({_, Mod, _, Pid}) when Mod=:=Module ->
				Cb(Pid);
			({_, _, _, Pid}) ->
				not Cb(Pid)
		end,
		collect_pids()
	),
	ok.

cb_if_child_infinite(_) ->
	doc("Ensure that the if_child callback works on infinite depth."),
	Cb=stampede_callbacks:if_child(infinity, stampede_test_sup_sup),
	true=lists:all(
		fun
			({stampede_test_sup_sup, _, _, Pid}) ->
				not Cb(Pid);
			({_, _, _, Pid}) ->
				Cb(Pid)
		end,
		collect_pids()
	),
	ok.

cb_if_child_direct(_) ->
	doc("Ensure that the if_child callback works on direct children."),
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
		collect_pids()
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

cb_if_portname(_) ->
	doc("Ensure that the if_portname callback works"),
	{ok, TcpPort}=gen_tcp:listen(0, [{active, false}]),
	true=(stampede_callbacks:if_portname("tcp_inet"))(TcpPort),
	ok=gen_tcp:close(TcpPort),
	{ok, UdpPort}=gen_udp:open(0, [{active, false}]),
	false=(stampede_callbacks:if_portname("tcp_inet"))(UdpPort),
	ok=gen_udp:close(UdpPort),
	ok.

cb_if_portinfo(_) ->
	doc("Ensure that the if_portinfo callback works"),
	{ok, TcpPort}=gen_tcp:listen(0, [{active, false}]),
	true=(stampede_callbacks:if_portinfo(name, "tcp_inet"))(TcpPort),
	ok=gen_tcp:close(TcpPort),
	{ok, UdpPort}=gen_udp:open(0, [{active, false}]),
	false=(stampede_callbacks:if_portinfo(name, "tcp_inet"))(UdpPort),
	ok=gen_udp:close(UdpPort),
	ok.

cb_if_tcp(_) ->
	doc("Ensure that the if_tcp callback works."),
	{ok, TcpPort}=gen_tcp:listen(0, [{active, false}]),
	true=(stampede_callbacks:if_tcp())(TcpPort),
	ok=gen_tcp:close(TcpPort),
	%% The socket backend for gen_tcp/inet was introduced as an
	%% EXPERIMENTAL feature in OTP/23. erlang:atom_to_binary/1
	%% appears in the same release and is used as an indicator
	%% if this part of the test can be performed.
	case erlang:function_exported(erlang, atom_to_binary, 1) of
		true ->
			{ok, TcpSocket}=gen_tcp:listen(0, [{inet_backend, socket}, {active, false}]),
			{'$inet', gen_tcp_socket, {TcpSocketCtrl, _}}=TcpSocket,
			true=(stampede_callbacks:if_tcp())(TcpSocketCtrl),
			ok=gen_tcp:close(TcpSocket);
		false ->
			ok
	end,
	{ok, UdpPort}=gen_udp:open(0, [{active, false}]),
	false=(stampede_callbacks:if_tcp())(UdpPort),
	ok=gen_udp:close(UdpPort),
	ok.

cb_if_udp(_) ->
	doc("Ensure that the if_udp callback works."),
	{ok, UdpPort}=gen_udp:open(0, [{active, false}]),
	true=(stampede_callbacks:if_udp())(UdpPort),
	ok=gen_udp:close(UdpPort),
	{ok, TcpPort}=gen_tcp:listen(0, [{active, false}]),
	false=(stampede_callbacks:if_udp())(TcpPort),
	ok=gen_tcp:close(TcpPort),
	ok.

do_make_port() ->
	{ok, Port}=gen_tcp:listen(0, [{active, false}]),
	ok=gen_tcp:close(Port),
	Port.
