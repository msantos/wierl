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
        list/0, list/1, list/2,
        format/1,
        decode/1
    ]).

-include("wierl.hrl").

-record(state, {
        ap,
        aps
    }).

-define(IW_MAX_BITRATES, 32).
-define(IW_MAX_ENCODING_SIZES, 8).
-define(IW_MAX_TXPOWER, 8).

-define(IW_POWER_ON, 16#0000).
-define(IW_POWER_TYPE, 16#F000).
-define(IW_POWER_PERIOD, 16#1000).
-define(IW_POWER_TIMEOUT, 16#2000).
-define(IW_POWER_SAVING, 16#4000).
-define(IW_POWER_MODE, 16#0F00).
-define(IW_POWER_UNICAST_R, 16#0100).
-define(IW_POWER_MULTICAST_R, 16#0200).
-define(IW_POWER_ALL_R, 16#0300).
-define(IW_POWER_FORCE_S, 16#0400).
-define(IW_POWER_REPEATER, 16#0800).
-define(IW_POWER_MODIFIER, 16#000F).
-define(IW_POWER_MIN, 16#0001).
-define(IW_POWER_MAX, 16#0002).
-define(IW_POWER_RELATIVE, 16#0004).



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
            <<4096:?UINT16, 0:?UINT16>>
        ]),
    case procket:ioctl(Socket, ?SIOCGIWSCAN, Req) of
        {ok, <<_Ifname:16/bytes, _Ptr:?UINT32, Len:?UINT16, _Flag:?UINT16>>} ->
            {ok, <<Stream:Len/bytes, _/binary>>} = procket:buf(Res),
            event(Stream);

        % Poll the socket for the results
        {error, eagain} ->
            timer:sleep(1000),
            ap(Socket, Dev);

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
    <<ESSID:Len/bytes, _/binary>> = Bin,
    ESSID;
decode({bssid, <<1,0, Bytes:6/bytes, 0,0,0,0,0,0,0,0, _/binary>>}) ->
    lists:flatten(string:join([ io_lib:format("~.16b", [N]) || <<N:8>> <= Bytes ], ":"));
decode({ap, AP}) ->
    decode({bssid, AP});
decode({mode, <<Mode:?UINT32>>}) ->
    mode(Mode);
decode({qual, <<Qual:8, Level:8/signed, Noise:8, Updated:8, _/binary>>}) ->
    [{quality, Qual}, {level, Level}, {noise, Noise}, {updated, decode({updated, Updated})}];

decode({updated, Status}) when Status band ?IW_QUAL_QUAL_UPDATED == 1 ->
    true;
decode({updated, _Status}) ->
    false;

decode({freq, <<Channel:?UINT64, _/binary>>}) when Channel < 1000 ->
    {channel, Channel};
decode({freq, <<M:?INT32, E:?INT16, _I:8, _Flags:8, _/binary>>}) ->
    {frequency, M*math:pow(10, E)};

decode({power, Power}) ->
    decode({param, Power});

decode({param, <<Value:?INT32, Fixed:8, Disabled:8, Flags:?UINT32, _/binary>>}) ->
    [{value, Value}, {fixed, Fixed}, {disabled, Disabled}, {flags, Flags}];

decode({range, <<
        Throughput:?UINT32,
        Min_nwid:?UINT32,
        Max_nwid:?UINT32,
        Old_num_channels:?UINT16,
        Old_num_frequency:8,
        Scan_capa:8,
        Event_capa:(6*4)/bytes,
        Sensitivity:?INT32,
        Max_qual:4/bytes,
        Avg_qual:4/bytes,
        Num_bitrates:8,
        Bitrate:(?IW_MAX_BITRATES*4)/bytes,
        Min_rts:?INT32,
        Max_rts:?INT32,
        Min_frag:?INT32,
        Max_frag:?INT32,
        Min_pmp:?INT32,
        Max_pmp:?INT32,
        Min_pmt:?INT32,
        Max_pmt:?INT32,
        Pmp_flags:?UINT16,
        Pmt_flags:?UINT16,
        Pm_capa:?UINT16,
        Encoding_size:(?IW_MAX_ENCODING_SIZES*2)/bytes,
        Num_encoding_sizes:8,
        Max_encoding_tokens:8,
        Encoding_login_index:8,
        Txpower_capa:?UINT16,
        Num_txpower:8,
        Txpower:(?IW_MAX_TXPOWER*4)/bytes,
        We_version_compiled:8,
        We_version_source:8,
        Retry_capa:?UINT16,
        Retry_flags:?UINT16,
        R_time_flags:?UINT16,
        Min_retry:?INT32,
        Max_retry:?INT32,
        Min_r_time:?INT32,
        Max_r_time:?INT32,
        Num_channels:?UINT16,
        Num_frequency:8,
        Freq:(?IW_MAX_FREQUENCIES*8)/bytes,
        Enc_capa:?UINT32,
        Min_pms:?INT32,
        Max_pms:?INT32,
%        Pms_flags:?UINT16,
%        Modul_capa:?INT32,
%        Bitrate_capa:?UINT32,
        _/binary
        >>}) ->
    [
        {throughput, Throughput},
        {min_nwid, Min_nwid},
        {max_nwid, Max_nwid},
        {old_num_channels, Old_num_channels},
        {old_num_frequency, Old_num_frequency},
        {scan_capa, Scan_capa},
        {event_capa, Event_capa},
        {sensitivity, Sensitivity},
        {max_qual, decode({qual, Max_qual})},
        {avg_qual, decode({qual, Avg_qual})},
        {num_bitrates, Num_bitrates},
        {bitrate, Bitrate},
        {min_rts, Min_rts},
        {max_rts, Max_rts},
        {min_frag, Min_frag},
        {max_frag, Max_frag},
        {min_pmp, Min_pmp},
        {max_pmp, Max_pmp},
        {min_pmt, Min_pmt},
        {max_pmt, Max_pmt},
        {pmp_flags, Pmp_flags},
        {pmt_flags, Pmt_flags},
        {pm_capa, Pm_capa},
        {encoding_size, Encoding_size},
        {num_encoding_sizes, Num_encoding_sizes},
        {max_encoding_tokens, Max_encoding_tokens},
        {encoding_login_index, Encoding_login_index},
        {txpower_capa, Txpower_capa},
        {num_txpower, Num_txpower},
        {txpower, Txpower},
        {we_version_compiled, We_version_compiled},
        {we_version_source, We_version_source},
        {retry_capa, Retry_capa},
        {retry_flags, Retry_flags},
        {r_time_flags, R_time_flags},
        {min_retry, Min_retry},
        {max_retry, Max_retry},
        {min_r_time, Min_r_time},
        {max_r_time, Max_r_time},
        {num_channels, Num_channels},
        {num_frequency, Num_frequency},
        {freq, decode({freq, Freq})},
        {enc_capa, Enc_capa},
        {min_pms, Min_pms},
        {max_pms, Max_pms}
%        {max_pms, Max_pms},
%        {pms_flags, Pms_flags},
%        {modul_capa, Modul_capa},
%        {bitrate_capa, Bitrate_capa}
    ];

decode({_Key, Val}) ->
    Val.
