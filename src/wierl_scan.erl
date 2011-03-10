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
-module(wierl_scan).
-export([
        start/0, start/1, start/2,
        format/1,
        decode/1
    ]).

-include("wierl_scan.hrl").

-record(state, {
        ap,
        aps
    }).


%%
%% Open an unprivileged, datagram socket using
%% procket
%%
start() ->
    {ok, Devs} = inet:getifaddrs(),
    [ begin
                N = list_to_binary(Dev),
                {N, start(N)}
        end || {Dev,_} <- Devs ].

start(Dev) ->
    start(Dev, []).

start(Dev, Opt) when is_binary(Dev), is_list(Opt) ->
    ESSID = proplists:get_value(essid, Opt),
    {ok, Socket} = procket:socket(inet, dgram, 0),
    Result = scan(Dev, Socket, ESSID),
    procket:close(Socket),
    Result.


%%
%% Initiate the scan
%%
scan(Dev, Socket, ESSID) when byte_size(Dev) < ?IFNAMSIZ ->
    Req = essid(Dev, ESSID),
    case procket:ioctl(Socket, ?SIOCSIWSCAN, Req) of
        {ok, _Req} ->
            result(Dev, Socket);
        {error, ebusy} ->
            timer:sleep(1000),
            scan(Dev, Socket, ESSID);
        {error, _} = Error ->
            Error
    end.


%%
%% Retrieve the scan results by specifying a buffer for the
%% kernel to return the results
%%
result(Dev, Socket) ->
    {ok, Req, [Res]} = procket:alloc([
            <<Dev/bytes, 0:( (?IFNAMSIZ - byte_size(Dev))*8)>>,
            {ptr, 4096},
            <<4096:?UINT16, 0:?UINT16>>
        ]),
    case procket:ioctl(Socket, ?SIOCGIWSCAN, Req) of
        {ok, <<_Ifname:16/bytes, _Ptr:?UINT32, Len:?UINT16, _Flag:?UINT16>>} ->
            {ok, <<Stream:Len/bytes, _/binary>>} = procket:buf(Res),
            event(Stream);

        % Poll the socket for the results
        {error, eagain} ->
            timer:sleep(1000),
            result(Dev, Socket);

        Error ->
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
        <<(byte_size(Struct)):?UINT16, ?IW_SCAN_THIS_ESSID:?UINT16>>
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

event(<<>>, #state{aps = APs}) ->
    [ {AP, orddict:to_list(N)} || {AP, N} <- gb_trees:to_list(APs) ];
event(<<EventLen:?UINT16, Cmd:?UINT16, Buf/binary>>, #state{ap = AP, aps = APs}) ->
    Len = EventLen - 4,
    <<Event:Len/bytes, Rest/binary>> = Buf,

    case cmd(Cmd) of
        ap ->
            event(Rest, #state{
                    ap = Event,
                    aps = gb_trees:enter(Event, orddict:new(), APs)
                });
        Type ->
            Info = gb_trees:get(AP, APs),
            Info1 = orddict:append(Type, Event, Info),
            event(Rest, #state{
                    ap = AP,
                    aps = gb_trees:enter(AP, Info1, APs)
                })
    end.


% Convert the integer command values to almost human readable atoms
cmd(?SIOCGIWNAME) -> name;
cmd(?SIOCGIWNWID) -> nwid;
cmd(?SIOCGIWFREQ) -> freq;
cmd(?SIOCGIWMODE) -> mode;
cmd(?SIOCGIWSENS) -> sens;
cmd(?SIOCGIWRANGE) -> range;
cmd(?SIOCGIWPRIV) -> priv;
cmd(?SIOCGIWSTATS) -> stats;
cmd(?SIOCGIWSPY) -> spy;
cmd(?SIOCGIWTHRSPY) -> thrspy;
cmd(?SIOCGIWAP) -> ap;
cmd(?SIOCGIWAPLIST) -> aplist;
cmd(?SIOCGIWESSID) -> essid;
cmd(?SIOCGIWNICKN) -> nickn;
cmd(?SIOCGIWRATE) -> rate;
cmd(?SIOCGIWRTS) -> rts;
cmd(?SIOCGIWFRAG) -> frag;
cmd(?SIOCGIWTXPOW) -> txpow;
cmd(?SIOCGIWRETRY) -> retry;
cmd(?SIOCGIWENCODE) -> encode;
cmd(?SIOCGIWPOWER) -> power;
cmd(?SIOCGIWMODUL) -> modul;
cmd(?SIOCGIWGENIE) -> genie;
cmd(?SIOCGIWAUTH) -> auth;
cmd(?SIOCGIWENCODEEXT) -> encodeext;
cmd(?IWEVGENIE) -> genie;
cmd(?IWEVQUAL) -> qual;
cmd(?IWEVCUSTOM) -> custom;
cmd(Unknown) -> {unknown, Unknown}.

mode(?IW_MODE_AUTO) -> auto;
mode(?IW_MODE_ADHOC) -> adhoc;
mode(?IW_MODE_INFRA) -> infra;
mode(?IW_MODE_MASTER) -> master;
mode(?IW_MODE_REPEAT) -> repeat;
mode(?IW_MODE_SECOND) -> second;
mode(?IW_MODE_MONITOR) -> monitor.


% Pretty print the scan list
format(Info) when is_list(Info) ->
    [ {decode({bssid, N}), attr(L)} || {N,L} <- Info ].

attr(Attr) ->
    [ decode(N) || N <- Attr ].

decode({Key, List}) when is_list(List) ->
    {Key, [ decode({Key, N}) || N <- List ]};

% What is 1?
decode({essid, <<Len:?UINT16, 1:?UINT16, Bin/binary>>}) ->
    <<ESSID:Len/bytes>> = Bin,
    ESSID;
decode({bssid, <<1,0, Bytes:6/bytes, 0,0,0,0,0,0,0,0>>}) ->
    lists:flatten(string:join([ io_lib:format("~.16b", [N]) || <<N:8>> <= Bytes ], ":"));
decode({mode, <<Mode:?UINT32>>}) ->
    mode(Mode);
decode({qual, <<Qual:8, Level:8/signed, Noise:8, Updated:8>>}) ->
    [{quality, Qual}, {level, Level}, {noise, Noise}, {updated, decode({updated, Updated})}];

decode({updated, Status}) when Status band ?IW_QUAL_QUAL_UPDATED == 1 ->
    true;
decode({updated, _Status}) ->
    false;

% How do we distinguish a channel from a frequency?
decode({freq, <<Channel:?UINT64>>}) when Channel < 32 ->
    {channel, Channel};
decode({freq, <<Freq:?UINT64>>}) ->
    {frequency, Freq};

decode({_Key, Val}) ->
    Val.
