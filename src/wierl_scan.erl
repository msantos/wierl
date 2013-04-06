%% Copyright (c) 2011-2013, Michael Santos <michael.santos@gmail.com>
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
-module(wierl_scan).
-export([
        list/0, list/1, list/2
    ]).

-include("wierl.hrl").

-record(state, {
        ap,
        aps
    }).


%%
%% Open an unprivileged, datagram socket using
%% procket
%%
list() ->
    {ok, Devs} = inet:getifaddrs(),
    [ begin
                N = list_to_binary(Dev),
                {N, list(N)}
        end || {Dev,_} <- Devs ].

list(Dev) ->
    list(Dev, []).

list(Dev, Opt) when is_binary(Dev), is_list(Opt) ->
    ESSID = proplists:get_value(essid, Opt),
    {ok, Socket} = procket:socket(inet, dgram, 0),
    Result = scan(Socket, Dev, ESSID),
    procket:close(Socket),
    Result.


%%
%% Initiate the scan
%%
scan(Socket, Dev, ESSID) when byte_size(Dev) < ?IFNAMSIZ ->
    Req = essid(Dev, ESSID),
    case procket:ioctl(Socket, ?SIOCSIWSCAN, Req) of
        {ok, _Req} ->
            ap(Socket, Dev);
        {error, ebusy} ->
            timer:sleep(1000),
            scan(Socket, Dev, ESSID);
        {error, _} = Error ->
            Error
    end.


%%
%% Retrieve the scan results by specifying a buffer for the
%% kernel to return the results
%%
ap(Socket, Dev) ->
    {ok, Req, [Res]} = procket:alloc([
            <<Dev/bytes, 0:( (?IFNAMSIZ - byte_size(Dev))*8)>>,
            {ptr, 4096},
            <<?UINT16(4096), ?UINT16(0)>>
        ]),

    Pointer = erlang:system_info({wordsize, external}) * 8,

    % struct iw_point
    % {
    %   void *pointer;
    %   __u16 length;
    %   __u16 flags;
    % };
    case procket:ioctl(Socket, ?SIOCGIWSCAN, Req) of
        {ok, <<_Ifname:16/bytes, _Ptr:Pointer, ?UINT16(Len), ?UINT16(_Flag)>>} ->
            Pad = (4096 - Len) * 8,
            {ok, <<Stream:Len/bytes, 0:Pad>>} = procket:buf(Res),
            event(Stream);

        % Poll the socket for the results
        {error, eagain} ->
            timer:sleep(1000),
            ap(Socket, Dev);

        {error, _} = Error ->
            Error
    end.


essid(Dev, undefined) ->
    <<Dev/binary, 0:((?IFNAMSIZ - byte_size(Dev))*8), 0:(16*8)>>;
essid(Dev, ESSID) when is_binary(ESSID), byte_size(ESSID) < ?IW_ESSID_MAX_SIZE ->
    Len = byte_size(ESSID),

    ScanReq = <<
    0:8,                        % scan_type: IW_SCAN_TYPE_ACTIVE
    Len:8,                      % essid_len
    0:8,                        % num entries in channel list: scan all
    0:8,                        % flags: unused

    % struct sockaddr bssid
    ?ARPHRD_ETHER:16/native,    % family
    255,255,255,255,255,255,    % sa_data: ff:ff:ff:ff:ff:ff
    0:64,                       % sa_data: zero remainder

    ESSID/bytes, 0:((?IW_ESSID_MAX_SIZE - Len)*8),

    0:32/native,                % min_channel_time: use defaults
    0:32/native                 % max_channel_time: use defaults
    >>,

    Freq = binary:copy(<<
        % struct iw_freq channel_list[IW_MAX_FREQUENCIES]
        0:32/native,                % mantissa
        0:16/native,                % exponent
        0:8,                        % list index
        0:8                         % flags
        >>, 32),

    Struct = list_to_binary([ScanReq, Freq]),

    % XXX This is probably dangerous. Since we throw away the
    % XXX return value of the resource, the garbage collector
    % XXX may destroy the resource and free the buffer.
    {ok, Req, [_Res]} = procket:alloc([
        <<Dev/bytes, 0:( (?IFNAMSIZ - byte_size(Dev))*8)>>,
        {ptr, Struct},
        <<?UINT16((byte_size(Struct))), ?UINT16(?IW_SCAN_THIS_ESSID)>>
    ]),
    Req.


% The events are returned in the form: length, command, data
% (length bytes)
%
% We believe the length returned in the packet and print out
% the binary data. If the length is wrong, we'll just crash.
event(Buf) ->
    event(Buf, #state{
            aps = gb_trees:empty()
        }).

% struct iw_event {
%    __u16       len;
%    __u16       cmd;
%    union iwreq_data    u;
% }
event(<<>>, #state{aps = APs}) ->
    [ {AP, orddict:to_list(N)} || {AP, N} <- gb_trees:to_list(APs) ];
event(<<?UINT16(EventLen), ?UINT16(Cmd), Buf/binary>>, #state{ap = AP, aps = APs}) ->
    Pad = procket:wordalign(2+2) * 8,
    Len = EventLen - 4 - Pad div 8,
    <<0:Pad, Event:Len/bytes, Rest/binary>> = Buf,

    case wierl:cmd(Cmd) of
        ap ->
            event(Rest, #state{
                    ap = Event,
                    aps = gb_trees:enter(Event, orddict:new(), APs)
                });
        Type ->
            Info = gb_trees:get(AP, APs),
            Info1 = orddict:store(Type, Event, Info),
            event(Rest, #state{
                    ap = AP,
                    aps = gb_trees:enter(AP, Info1, APs)
                })
    end.
