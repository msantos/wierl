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
%% Dump and generate 802.11 frames
%%
-module(wierl_frame).
-export([
        open/1, close/1
    ]).

-include("wierl.hrl").

-define(ETH_P_ALL, 16#0003).
-define(PF_PACKET, 17).


open(Ifname) when byte_size(Ifname) < ?IFNAMSIZ ->
    {ok, Socket} = packet:socket(?ETH_P_ALL),
    Ifindex = packet:ifindex(Socket, binary_to_list(Ifname)),

    Sockaddr_ll = <<
        ?PF_PACKET:16/native,   % sll_family: PF_PACKET
        0:16,                   % sll_protocol: Physical layer protocol
        Ifindex:32/native,      % sll_ifindex: Interface number
        0:16,                   % sll_hatype: Header type
        0:8,                    % sll_pkttype: Packet type
        0:8,                    % sll_halen: address length
        0:8,                    % sll_addr[8]: physical layer address
        0:8,                    % sll_addr[8]: physical layer address
        0:8,                    % sll_addr[8]: physical layer address
        0:8,                    % sll_addr[8]: physical layer address
        0:8,                    % sll_addr[8]: physical layer address
        0:8,                    % sll_addr[8]: physical layer address
        0:8,                    % sll_addr[8]: physical layer address
        0:8                     % sll_addr[8]: physical layer address
        >>,

    ok = procket:bind(Socket, Sockaddr_ll),
    {ok, Socket, Ifindex}.

close(Socket) when is_integer(Socket) ->
    procket:close(Socket).
