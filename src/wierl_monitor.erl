%% Copyright (c) 2011-2016, Michael Santos <michael.santos@gmail.com>
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
        open/1, open/2,
        close/1,
        frame/2,

        read/1, read/2,
        write/2,

        mode/2, controlling_process/2,

        dlt/1
    ]).
-export([start_link/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
        terminate/2, code_change/3]).

-include("wierl.hrl").
-include("wierl_frame.hrl").

-define(ETH_P_ALL, 16#0003).
-define(PF_PACKET, 17).
-define(SIZEOF_STRUCT_SOCKADDR_LL, 20).

-record(state, {
        port,
        pid,
        socket,
        ifname,
        ifindex,
        dlt,            % radio header format
        fcs = false     % FCS is present in frame
    }).


%%--------------------------------------------------------------------
%%% Exports
%%--------------------------------------------------------------------
open(Ifname) ->
    open(Ifname, []).
open(Ifname, Flags) ->
    start_link(Ifname, Flags).

close(Ref) ->
    gen_server:call(Ref, close).

read(Ref) ->
    read(Ref, 16#FFFF).
read(Ref, Size) ->
    gen_server:call(Ref, {read, Size}).

write(Ref, Frame) ->
    gen_server:call(Ref, {write, Frame}).

% Encode a complete frame
frame(Ref, {Header, #ieee802_11_fc{} = FC, FB}) when is_tuple(FB) ->
    try frame_encode(Ref, {Header, FC, FB})
    catch
        error:_ ->
            {error, bad_frame}
    end;
% Decode a complete frame
frame(Ref, Frame) when is_binary(Frame) ->
    try frame_decode(Ref, Frame)
    catch
        error:_ ->
            {error, bad_frame}
    end.

mode(Ref, Mode) when is_atom(Mode) ->
    Ifname = gen_server:call(Ref, ifname),
    wireless_mode(Ifname, Mode).


% FIXME: race condition: events can be delivered out of order
controlling_process(Ref, Pid) when is_pid(Ref), is_pid(Pid) ->
    flush_events(Ref, Pid),
    gen_server:call(Ref, {controlling_process, Pid}),
    flush_events(Ref, Pid).


start_link(Ifname, Flags) when byte_size(Ifname) < ?IFNAMSIZ, is_list(Flags) ->
    Pid = self(),
    gen_server:start_link(?MODULE, [Pid, Ifname, Flags], [{timeout, 5000}]).


%%--------------------------------------------------------------------
%%% Callbacks
%%--------------------------------------------------------------------
init([Pid, Ifname, Flags]) ->
    process_flag(trap_exit, true),

    ok = wierl_config:down(Ifname),
    ok = wireless_mode(Ifname, monitor),
    ok = wierl_config:up(Ifname),

    {ok, Socket} = packet:socket(?ETH_P_ALL),
    Ifindex = packet:ifindex(Socket, binary_to_list(Ifname)),
    ok = packet:bind(Socket, Ifindex),

    FCS = proplists:get_value(fcs, Flags, false),

    case datalinktype(Socket) of
        {ok, DLT} ->
            Active = proplists:get_value(active, Flags, false),

            Port = case Active of
                true -> set_active(Socket);
                false -> false
            end,

            {ok, #state{
                dlt = DLT,
                port = Port,
                pid = Pid,
                ifname = Ifname,
                socket = Socket,
                ifindex = Ifindex,
                fcs = FCS
            }};
        {error, Error} ->
            procket:close(Socket),
            {stop, Error}
    end.


handle_call(dlt, _From, #state{dlt = DLT} = State) ->
    {reply, DLT, State};
handle_call(fcs, _From, #state{fcs = FCS} = State) ->
    {reply, FCS, State};
handle_call({fcs, FCS}, _From, State) ->
    {reply, ok, State#state{fcs = FCS}};
handle_call(ifname, _From, #state{ifname = Ifname} = State) ->
    {reply, Ifname, State};
handle_call({controlling_process, Pid}, {Owner,_}, #state{pid = Owner} = State) ->
    {reply, ok, State#state{pid = Pid}};

handle_call({read, Size}, _From, #state{socket = Socket} = State) ->
    case procket:recvfrom(Socket, Size, 0, ?SIZEOF_STRUCT_SOCKADDR_LL) of
        {ok, Buf, <<
            ?UINT16(_Family),
            ?UINT16(_Protocol),
            ?UINT32(_Ifindex),
            ?UINT16(Hatype),
            _/binary
            >>} ->
            {reply, {ok, Buf}, State#state{dlt = dlt(Hatype)}};
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


%% {active, true} mode
handle_info({Port, {data, Data}}, #state{port = Port, pid = Pid} = State) ->
    Pid ! {wierl_monitor, self(), Data},
    {noreply, State};

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
% changing the mode. Legacy drivers like rt2870sta
% need the interface to be up before changing the mode.
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

% Get the datalink type of the interface by reading 0 bytes
% from the socket. The interface may not be ready, so spin
% here until it comes up.
%
% In some cases the interface will never come up. tcpdump
% doesn't display any frames being received by the interface.
% Only fix is to unload/load the driver kernel module or
% (more reliable) reboot.
datalinktype(Socket) ->
    case procket:recvfrom(Socket, 0, 0, ?SIZEOF_STRUCT_SOCKADDR_LL) of
        {error,eagain} ->
            timer:sleep(10),
            datalinktype(Socket);
        {ok, <<>>, <<
            ?UINT16(_Family),
            ?UINT16(_Protocol),
            ?UINT32(_Ifindex),
            ?UINT16(Hatype),
            _/binary
            >>} ->
            {ok, dlt(Hatype)};
        {error, _} = Error ->
            Error
    end.


frame_encode(_Ref, {Header, FC, FB}) ->
    Type = case element(1, Header) of
        ieee802_11_radiotap -> wierl_radiotap;
        ieee802_11_prism -> wierl_prism
    end,

    list_to_binary([
            Type:header(Header),
            wierl_frame:control(FC),
            wierl_frame:type(FC, FB)
        ]).

frame_decode(Ref, Frame) ->
    Type = gen_server:call(Ref, dlt),

    % Get the radio header
    {Radio, Data1} = Type:header(Frame),

    % Frame control header
    {FC, Data2} = wierl_frame:control(Data1),

    % Frame control body
    % XXX check the FCS is correct
    {FB, FCS} = wierl_frame:type(FC, Data2),

    {FB1, FCS1} = case FCS of
        <<>> ->
            % Driver does not provide FCS or frame type
            % does not use FCS
            {FB, 0};
        <<N:4/unsigned-integer-unit:8>> ->
            % Driver provides FCS
            % XXX flag is set in state on EVERY frame
            ok = gen_server:call(Ref, {fcs, true}),
            {FB, N};
        false ->
            % Ambiguous frame type, check our state if
            % FCS is provided
            Include = gen_server:call(Ref, fcs),

            case Include of
                true ->
                    % Remove 4 bytes from the frame body and re-parse the frame
                    Len = byte_size(Data2) - 4,
                    <<Body:Len/bytes, RealFCS:4/unsigned-integer-unit:8>> = Data2,
                    {FrameBody, _} = wierl_frame:type(FC, Body),
                    {FrameBody, RealFCS};
                false ->
                    {FB, 0}
            end
    end,

    {Radio, FC, FB1, FCS1}.


%% active mode
set_active(FD) ->
    open_port({fd, FD, FD}, [stream, binary]).

flush_events(Ref, Pid) ->
    receive
        {wierl_monitor, Ref, _} = Event ->
            Pid ! Event,
            flush_events(Ref, Pid)
    after
        0 -> ok
    end.
