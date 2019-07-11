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

-module(stampede_tracer).

-behavior(gen_server).

-export([start_link/3]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {app, top_sup, tab}).

-spec start_link(atom(), pid(), ets:tab()) -> {ok, pid()}.
start_link(App, TopSup, Tab) ->
	{ok, Pid}=gen_server:start_link(?MODULE, {App, TopSup}, []),
	ets:give_away(Tab, Pid, stampede),
	{ok, Pid}.

init({App, TopSup}) ->
	Monitor=monitor(process, TopSup),
	{ok, #state{app=App, top_sup={TopSup, Monitor}}}.

handle_call(_, _, State) ->
	{noreply, State}.

handle_cast(_, State) ->
	{noreply, State}.

handle_info({'ETS-TRANSFER', Tab, _, stampede}, State=#state{app=App, top_sup={TopSup, _}}) ->
	ok=collect(App, TopSup, Tab),
	gen_server:cast(self(), kill_something),
	{noreply, State#state{tab=Tab}};
handle_info({'DOWN', Ref, process, Pid, _}, State=#state{top_sup={Pid, Ref}}) ->
	{stop, {top_supervisor_exited, Pid}, State};
handle_info({trace, Pid, spawned, _, _}, State=#state{tab=Tab}) ->
	ets:insert(Tab, {Pid}),
	{noreply, State};
handle_info({trace, Pid, exit, _}, State=#state{tab=Tab}) ->
	ets:delete_object(Tab, {Pid}),
	{noreply, State};
handle_info(_, State) ->
	{noreply, State}.

terminate(_, _) ->
	ok.

code_change(_, State, _) ->
	{ok, State}.

collect(App, TopSup, Tab) ->
	true = trace_proc(TopSup),
	collect_trace_hierarchy(TopSup, Tab),
	collect_trace_sequence(App, TopSup, Tab),
	ok.

collect_trace_hierarchy(Sup, Tab) ->
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
						collect_trace_proc(Pid, Tab),
						collect_trace_hierarchy(Pid, Tab);
					({_, Pid, worker, _}) ->
						collect_trace_proc(Pid, Tab)
				end,
				Children
			)
	catch
		exit:{noproc, _} ->
			ok
	end.

collect_trace_sequence(App, TopSup, Tab) ->
	_ = [collect_trace_proc(Pid, Tab) || Pid <- erlang:processes(), Pid>TopSup, {ok, App}=:=application:get_application(Pid)],
	ok.

collect_trace_proc(Pid, Tab) ->
	case trace_proc(Pid) of
		true ->
			ets:insert(Tab, {Pid});
		false ->
			ok
	end,
	ok.

trace_proc(Pid) ->
	try
		1=erlang:trace(Pid, true, [procs, set_on_spawn]),
		true
	catch
		error:badarg ->
			false
	end.
