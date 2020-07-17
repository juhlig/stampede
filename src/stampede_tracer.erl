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

-module(stampede_tracer).

-behavior(gen_server).

-export([start_link/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {top_sup, tab}).

-spec start_link(pid(), ets:tab()) -> {ok, pid()}.
start_link(TopSup, Tab) ->
	gen_server:start_link(?MODULE, {TopSup, Tab}, []).

init({TopSup, Tab}) ->
	Monitor=monitor(process, TopSup),
	ok=collect(TopSup, Tab),
	{ok, #state{top_sup={TopSup, Monitor}, tab=Tab}}.

handle_call(_, _, State) ->
	{noreply, State}.

handle_cast(_, State) ->
	{noreply, State}.

handle_info({'DOWN', Ref, process, Pid, _}, State=#state{top_sup={Pid, Ref}}) ->
	{stop, {top_supervisor_exited, Pid}, State};
handle_info({trace, Pid, spawned, _, _}, State=#state{tab=Tab}) when is_pid(Pid) ->
	ets:insert(Tab, {Pid}),
	{noreply, State};
handle_info({trace, Pid, exit, _}, State=#state{tab=Tab}) when is_pid(Pid) ->
	ets:delete(Tab, Pid),
	{noreply, State};
handle_info({trace, _, link, Port}, State=#state{tab=Tab}) when is_port(Port) ->
	case erlang:port_info(Port, connected) of
		{connected, Pid} when Pid=:=self() ->
			collect_trace(Port, Tab);
		_ ->
			ok
	end,
	{noreply, State};
handle_info({trace, Port, closed, _}, State=#state{tab=Tab}) when is_port(Port) ->
	ets:delete(Tab, Port),
	{noreply, State};
handle_info(_, State) ->
	{noreply, State}.

terminate(_, _) ->
	ok.

code_change(_, State, _) ->
	{ok, State}.

collect(TopSup, Tab) ->
	true = trace(TopSup),
	collect_trace_children(TopSup, Tab),
	collect_trace_ports(TopSup, Tab),
	collect_trace_sockets(TopSup, Tab),
	ok.

collect_trace_children(Sup, Tab) ->
	try
		supervisor:which_children(Sup)
	of
		Children ->
			lists:foreach(
				fun
					({_, undefined, _, _}) ->
						ok;
					({_, restarting, _, _}) ->
						ok;
					({_, Pid, supervisor, _}) ->
						collect_trace(Pid, Tab),
						collect_trace_children(Pid, Tab);
					({_, Pid, worker, _}) ->
						collect_trace(Pid, Tab)
				end,
				Children
			)
	catch
		_:_ ->
			ok
	end.

collect_trace_sockets(TopSup, Tab) ->
	collect_trace_process_sockets(TopSup, Tab),
	lists:foreach(
		fun
			({Pid}) when is_pid(Pid) ->
				collect_trace_process_sockets(Pid, Tab);
			(_) ->
				ok
		end,
		ets:tab2list(Tab)
	),
	ok.

collect_trace_process_sockets(Pid, Tab) ->
	{monitored_by, MonitoredBy}=erlang:process_info(Pid, monitored_by),
	lists:foreach(
		fun
			(MonitoringPid) when is_pid(MonitoringPid) ->
				case proc_lib:initial_call(MonitoringPid) of
					{gen_tcp_socket, _, _} ->
						collect_trace(MonitoringPid, Tab);
					_ ->
						ok
				end;
			(_) ->
				ok
		end,
		MonitoredBy
	),
	ok.

collect_trace_ports(TopSup, Tab) ->
	lists:foreach(
		fun (Port) ->
			case erlang:port_info(Port, connected) of
				undefined ->
					ok;
				{connected, Pid} when Pid=:=TopSup ->
					collect_trace(Port, Tab);
				{connected, Pid} ->
					case ets:member(Tab, Pid) of
						true ->
							collect_trace(Port, Tab);
						false ->
							ok
					end
			end
		end,
		erlang:ports()
	),
	ok.

collect_trace(PidOrPort, Tab) ->
	case trace(PidOrPort) of
		true ->
			ets:insert(Tab, {PidOrPort});
		false ->
			ok
	end,
	ok.

trace(PidOrPort) when is_pid(PidOrPort); is_port(PidOrPort) ->
	try
		1=erlang:trace(PidOrPort, true, [procs, ports, set_on_spawn]),
		true
	catch
		error:badarg ->
			false
	end.
