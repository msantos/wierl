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
-export([
        open/1,
        close/1, close/2, close/3,
        frame/1,

        read/1, read/2,
        write/2
    ]).

-include("wierl.hrl").
-include("wierl_frame.hrl").

-define(ETH_P_ALL, 16#0003).
-define(PF_PACKET, 17).


open(Ifname) when byte_size(Ifname) < ?IFNAMSIZ ->
    ok = wierl_config:down(Ifname),

    Mode = wierl:mode(monitor),
    {ok, <<Mode, 0:(15*8)>>} = wierl_config:param(Ifname, {mode, Mode}),

    ok = wierl_config:up(Ifname),

    {ok, Socket} = packet:socket(?ETH_P_ALL),
    Ifindex = packet:ifindex(Socket, binary_to_list(Ifname)),
    ok = packet:bind(Socket, Ifindex),
    {ok, Socket}.

close(Socket) when is_integer(Socket) ->
    procket:close(Socket).

close(Ifname, Socket) ->
    close(Ifname, Socket, infra).

close(Ifname, Socket, Mode) when is_integer(Socket), is_atom(Mode) ->
    wierl_config:down(Ifname),
    wierl_config:param(Ifname, {mode, Mode}),
    wierl_config:up(Ifname),

    procket:close(Socket).

read(Socket) ->
    read(Socket, 16#FFFF).
read(Socket, Size) ->
    procket:read(Socket, Size).

write(Socket, Frame) ->
    procket:write(Socket, Frame).


% Encode a complete frame
frame({#ieee802_11_radiotap{} = Radio,
        #ieee802_11_fc{} = FC,
        FB}) when is_tuple(FB) ->
    list_to_binary([
            wierl_radiotap:header(Radio),
            wierl_frame:frame_control(FC),
            wierl_frame:frame_type(FC, FB)
        ]);

frame({#ieee802_11_fc{} = FC,
        FB}) when is_tuple(FB) ->
    list_to_binary([
            wierl_frame:frame_control(FC),
            wierl_frame:frame_type(FC, FB)
        ]);

% Decode a complete frame
frame(Frame) when is_binary(Frame) ->
    % Get the radiotap header
    {Radiotap, Data1} = wierl_radiotap:header(Frame),

    % Frame control header
    {FC, Data2} = wierl_frame:frame_control(Data1),

    % Frame control body
    FB = wierl_frame:frame_type(FC, Data2),

    {Radiotap, FC, FB}.
