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
-module(wierl).
-export([
        format/1,
        decode/1,
        cmd/1,
        mode/1
    ]).

-include("wierl.hrl").


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
mode(?IW_MODE_MONITOR) -> monitor;

mode(auto) -> ?IW_MODE_AUTO;
mode(adhoc) -> ?IW_MODE_ADHOC;
mode(infra) -> ?IW_MODE_INFRA;
mode(master) -> ?IW_MODE_MASTER;
mode(repeat) -> ?IW_MODE_REPEAT;
mode(second) -> ?IW_MODE_SECOND;
mode(monitor) -> ?IW_MODE_MONITOR.

qual(<<Qual:8, Level:8/signed, Noise:8, Updated:8>>) ->
    [{quality, Qual}, {level, Level}, {noise, Noise}, decode({updated, Updated})].

% Pretty print the scan list
format(Info) when is_list(Info) ->
    [ {decode({bssid, N}), param(L)} || {N,L} <- Info ].

param(Param) ->
    [ decode(N) || N <- Param ].

decode({Key, List}) when is_list(List) ->
    {Key, [ decode({Key, N}) || N <- List ]};

decode({essid, <<Len:?UINT16, _Cmd:?UINT16, Rest/binary>>}) ->
    Pad = procket:wordalign(2+2) * 8,
    <<0:Pad, ESSID:Len/bytes>> = Rest,
    {essid, ESSID};

% struct sockadddr
decode({bssid, <<
        ?ARPHRD_ETHER:?UINT16,  % sa_family_t
        Bytes:6/bytes, 0:64     % sa_data: 14 bytes
        >>}) ->
    {bssid, lists:flatten(string:join([ io_lib:format("~.16b", [N]) || <<N:8>> <= Bytes ], ":"))};

decode({ap, AP}) ->
    decode({bssid, AP});

decode({mode, <<Mode:?UINT32>>}) ->
    {mode, mode(Mode)};

decode({qual, Qual}) ->
    {qual, qual(Qual)};

decode({updated, Status}) when Status band ?IW_QUAL_QUAL_UPDATED == 1 ->
    {updated, true};
decode({updated, _Status}) ->
    {updated, false};

decode({freq, <<Channel:?UINT64, _/binary>>}) when Channel < 1000 ->
    {channel, Channel};
decode({freq, <<M:?INT32, E:?INT16, _I:8, _Flags:8, _/binary>>}) ->
    {frequency, M*math:pow(10, E)};

decode({power, Power}) ->
    {power, decode({param, Power})};

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
    {range, [
        {throughput, Throughput},
        {min_nwid, Min_nwid},
        {max_nwid, Max_nwid},
        {old_num_channels, Old_num_channels},
        {old_num_frequency, Old_num_frequency},
        {scan_capa, Scan_capa},
        {event_capa, Event_capa},
        {sensitivity, Sensitivity},
        {max_qual, qual(Max_qual)},
        {avg_qual, qual(Avg_qual)},
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
    ]};

decode({_Key,_Val} = Unknown) ->
    Unknown.
