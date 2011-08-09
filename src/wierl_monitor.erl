%% Copyright (c) 2011, Michael Santos <michael.santos@gmail.com>
%% All rights reserved.
%%
%% Redistribution and use in source and binary forms, with or without
%% modification, are permitted provided that the following conditions
%% are met:
%%
%% Redistributions of source code must retain the above copyright
%% notice, this list of conditions and the following disclaimer.
%%
%% Redistributions in binary form must reproduce the above copyright
%% notice, this list of conditions and the following disclaimer in the
%% documentation and/or other materials provided with the distribution.
%%
%% Neither the name of the author nor the names of its contributors
%% may be used to endorse or promote products derived from this software
%% without specific prior written permission.
%%
%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
%% "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
%% LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
%% FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
%% COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
%% INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
%% BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
%% LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
%% CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
%% LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
%% ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
%% POSSIBILITY OF SUCH DAMAGE.

%%
%% Put an 802.11 interface into monitor mode
%%
-module(wierl_monitor).
-behaviour(gen_server).

-export([
        open/1,
        close/1,
        frame/2,

        read/1, read/2,
        write/2,

        mode/2
    ]).
-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
        terminate/2, code_change/3]).

-include("wierl.hrl").
-include("wierl_frame.hrl").

-define(ETH_P_ALL, 16#0003).
-define(PF_PACKET, 17).
-define(SIZEOF_STRUCT_SOCKADDR_LL, 20).

-record(state, {
        pid,
        socket,
        ifname,
        ifindex,
        header        % header format
    }).


%%--------------------------------------------------------------------
%%% Exports
%%--------------------------------------------------------------------
open(Ifname) ->
    start_link(Ifname).

close(Ref) ->
    gen_server:call(Ref, close).

read(Ref) ->
    read(Ref, 16#FFFF).
read(Ref, Size) ->
    gen_server:call(Ref, {read, Size}).

write(Ref, Frame) ->
    gen_server:call(Ref, {write, Frame}).

% Encode a complete frame
frame(_Ref, {Header, #ieee802_11_fc{} = FC, FB}) when is_tuple(FB) ->

    Type = case element(1, Header) of
        ieee802_11_radiotap -> wierl_radiotap;
        ieee802_11_prism -> wierl_prism
    end,

    list_to_binary([
            Type:header(Header),
            wierl_frame:control(FC),
            wierl_frame:type(FC, FB)
        ]);

% Decode a complete frame
frame(Ref, Frame) when is_binary(Frame) ->
    Type = gen_server:call(Ref, header),

    % Get the radio header
    {Radio, Data1} = Type:header(Frame),

    % Frame control header
    {FC, Data2} = wierl_frame:control(Data1),

    % Frame control body
    FB = wierl_frame:type(FC, Data2),

    {Radio, FC, FB}.

mode(Ref, Mode) when is_atom(Mode) ->
    Ifname = gen_server:call(Ref, ifname),
    wireless_mode(Ifname, Mode).


start_link(Ifname) when byte_size(Ifname) < ?IFNAMSIZ ->
    Pid = self(),
    gen_server:start_link(?MODULE, [Pid, Ifname], []).


%%--------------------------------------------------------------------
%%% Callbacks
%%--------------------------------------------------------------------
init([Pid, Ifname]) ->
    process_flag(trap_exit, true),

    ok = wierl_config:down(Ifname),
    ok = wireless_mode(Ifname, monitor),
    ok = wierl_config:up(Ifname),

    {ok, Socket} = packet:socket(?ETH_P_ALL),
    Ifindex = packet:ifindex(Socket, binary_to_list(Ifname)),
    ok = packet:bind(Socket, Ifindex),

    {ok, #state{
            pid = Pid,
            ifname = Ifname,
            socket = Socket,
            ifindex = Ifindex
    }}.


handle_call(header, _From, #state{header = Header} = State) ->
    {reply, Header, State};
handle_call(ifname, _From, #state{ifname = Ifname} = State) ->
    {reply, Ifname, State};

handle_call({read, Size}, _From, #state{socket = Socket} = State) ->
    case procket:recvfrom(Socket, Size, 0, ?SIZEOF_STRUCT_SOCKADDR_LL) of
        {ok, Buf, <<
            _Family:?UINT16,
            _Protocol:?UINT16,
            _Ifindex:?UINT32,
            Hatype:?UINT16,
            _/binary
            >>} ->
            {reply, {ok, Buf}, State#state{header = dlt(Hatype)}};
        {error, _} = Error ->
            {reply, Error, State};
        Unknown ->
            {reply, {unknown, Unknown}, State}
    end;
handle_call({write, Frame}, _From, #state{socket = Socket} = State) ->
    Reply = procket:write(Socket, Frame),
    {reply, Reply, State};
handle_call(close, _From, State) ->
    {stop, normal, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

% WTF?
handle_info(Info, State) ->
    error_logger:error_report([wtf, Info]),
    {noreply, State}.

terminate(_Reason, #state{socket = Socket}) ->
    % XXX Reset back to infra mode?
    procket:close(Socket),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

% Most drivers require the device to be down before
% changing the mode. Other wireless devices need to be
% up before changing the mode.
%
% XXX Possible to end up in an infinite loop here?
wireless_mode(Ifname, Mode) ->
    N = wierl:mode(Mode),
    case wierl_config:param(Ifname, {mode, N}) of
        {ok, <<N, 0:(15*8)>>} ->
            ok;
        {error, enetdown} ->
            ok = wierl_config:up(Ifname),
            wireless_mode(Ifname, Mode);
        Error ->
            Error
    end.

dlt(802) -> wierl_prism;
dlt(803) -> wierl_radiotap;
dlt(N) when is_integer(N) -> {unsupported, N};

dlt(wierl_prism) -> 802;
dlt(wierl_radiotap) -> 803.
