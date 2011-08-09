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
%% Decode 802.11 radiotap headers
%%
-module(wierl_radiotap).
-export([
        header/1
    ]).

-include("wierl.hrl").
-include("wierl_frame.hrl").

-define(BMAP(X,Y), X bor (1 bsl Y)).

%%-------------------------------------------------------------------------
%%% Radiotap header
%%-------------------------------------------------------------------------

%% See:
%% http://netbsd.gw.com/cgi-bin/man-cgi?ieee80211_radiotap+9+NetBSD-current

header(<<Version:8, Pad:8, Len:?UINT16LE,
    Present:?UINT32LE,
    Frame/binary>>) ->

    Size = Len-8,
    <<Header:Size/bytes, Data/binary>> = Frame,

    {Extensions, Rest} = extension(Present, Header),

    {#ieee802_11_radiotap{
        version = Version,
        pad = Pad,
        len = Len,
        present = Extensions,
        rest = Rest
    }, Data};
header(#ieee802_11_radiotap{
        version = Version,
        pad = Pad,
        len = Len,
        present = Extensions,
        rest = Rest
    }) ->

    {Bitmap, Header} = extension(Extensions),

    <<Version:8, Pad:8, Len:?UINT16LE,
    Bitmap:?UINT32LE,
    Header/bytes, Rest/binary>>.


extension(Bitmap, Extensions) when is_integer(Bitmap), is_binary(Extensions) ->

    <<Ext:1,
    Vendor_namespace:1,
    _Namespace:1,
    _:9,
    Mcs:1,
    Xchannel:1,
    _Data_retries:1,
    _Rts_retries:1,
    _Tx_flags:1,
    Rx_flags:1,
    Db_antnoise:1,
    Db_antsignal:1,
    Antenna:1,
    Dbm_tx_power:1,
    Db_tx_attenuation:1,
    Tx_attenuation:1,
    Lock_quality:1,
    Dbm_antnoise:1,
    Dbm_antsignal:1,
    Fhss:1,
    Channel:1,
    Rate:1,
    Flags:1,
    Tsft:1>> = <<Bitmap:32>>,

    Present = [ K || {K,V} <-
        [
            {tsft, Tsft},
            {flags, Flags},
            {rate, Rate},
            {channel, Channel},
            {fhss, Fhss},
            {dbm_antsignal, Dbm_antsignal},
            {dbm_antnoise, Dbm_antnoise},
            {lock_quality, Lock_quality},
            {tx_attenuation, Tx_attenuation},
            {db_tx_attenuation, Db_tx_attenuation},
            {dbm_tx_power, Dbm_tx_power},
            {antenna, Antenna},
            {db_antsignal, Db_antsignal},
            {db_antnoise, Db_antnoise},
            {rx_flags, Rx_flags},
            {xchannel, Xchannel},
            {mcs, Mcs},
            {vendor_namepsace, Vendor_namespace},
            {ext, Ext}
        ], V == 1 ],

    {Header, Rest} = lists:foldl(
        fun (Type, {Field, Data}) ->
                {Decoded, Rest} = field(Type, Data),
                {[Decoded|Field], Rest}
        end,
        {[], Extensions}, Present),

    {lists:reverse(Header), Rest}.

extension(Extensions) when is_list(Extensions) ->

    Bitmap = lists:foldl(
        fun (tsft, N) -> ?BMAP(N,0);
            (flags, N) -> ?BMAP(N,1);
            (rate, N) -> ?BMAP(N,2);
            (channel, N) -> ?BMAP(N,3);
            (fhss, N) -> ?BMAP(N,4);
            (dbm_antsignal, N) -> ?BMAP(N,5);
            (dbm_antnoise, N) -> ?BMAP(N,6);
            (lock_quality, N) -> ?BMAP(N,7);
            (tx_attenuation, N) -> ?BMAP(N,8);
            (db_tx_attenuation, N) -> ?BMAP(N,9);
            (dbm_tx_power, N) -> ?BMAP(N,10);
            (antenna, N) -> ?BMAP(N,11);
            (db_antsignal, N) -> ?BMAP(N,12);
            (db_antnoise, N) -> ?BMAP(N,13);
            (rx_flags, N) -> ?BMAP(N,14);
            (xchannel, N) -> ?BMAP(N,18);
            (mcs, N) -> ?BMAP(N,19);
            (vendor_namepsace, N) -> ?BMAP(N,30);
            (ext, N) -> ?BMAP(N,31)
        end,
        0, proplists:get_keys(Extensions)),

    Present = lists:foldl(
        fun (Field, Fields) ->
                [field(Field)|Fields]
        end,
        [], lists:reverse(Extensions)),

    {Bitmap, list_to_binary(Present)}.


%% See:
%% https://github.com/mcr/tcpdump/blob/master/ieee802_11_radio.h
field(tsft, <<Microsec:?UINT64, Data/binary>>) ->
    {{tsft, Microsec}, Data};

field(channel, <<Channel:?UINT16, Flags:?UINT16, Data/binary>>) ->
    {{channel, Channel, Flags}, Data};

field(fhss, <<Hop:8, Pattern:8, Data/binary>>) ->
    {{fhss, Hop, Pattern}, Data};

field(rate, <<1, Index:8, Data/binary>>) ->
    {{mcs_index, Index}, Data};
field(rate, <<Rate:8, Data/binary>>) ->
    {{rate, Rate}, Data};

field(dbm_antsignal, <<Signal:8, Data/binary>>) ->
    {{dbm_antsignal, Signal}, Data};

field(dbm_antnoise, <<Noise:8, Data/binary>>) ->
    {{dbm_antnoise, Noise}, Data};

field(db_antsignal, <<Signal:8, Data/binary>>) ->
    {{db_antsignal, Signal}, Data};

field(db_antnoise, <<Noise:8, Data/binary>>) ->
    {{db_antnoise, Noise}, Data};

field(lock_quality, <<Qual:8, Data/binary>>) ->
    {{lock_quality, Qual}, Data};

field(tx_attenuation, <<Power:?UINT16, Data/binary>>) ->
    {{tx_attenuation, Power}, Data};

field(db_tx_attenuation, <<Power:?UINT16, Data/binary>>) ->
    {{db_tx_attenuation, Power}, Data};

field(dbm_tx_power, <<Power:8, Data/binary>>) ->
    {{dbm_tx_power, Power}, Data};

field(flags, <<Bitmap:8, Data/binary>>) ->
    {{flags, Bitmap}, Data};

field(antenna, <<Index:8, Data/binary>>) ->
    {{antenna, Index}, Data};

field(rx_flags, <<Bitmap:8, Data/binary>>) ->
    {{rx_flags, Bitmap}, Data};

field(xchannel, <<Bitmap:?UINT32, Mhz:?UINT16, Channel:8, Max_power:8, Data/binary>>) ->
    {{xchannel, Bitmap, Mhz, Channel, Max_power}, Data};

field(mcs, <<Known:8, Flags:8, Mcs:8, Data/binary>>) ->
    {{mcs, Known, Flags, Mcs}, Data};

field(vendor_namespace, <<OUI1:8, OUI2:8, OUI3:8, Subspace:8, Len:?UINT16, Data/binary>>) ->
    {{vendor_namespace, {OUI1, OUI2, OUI3}, Subspace, Len}, Data}.

field({tsft, Microsec}) ->
    <<Microsec:?UINT64>>;

field({channel, Channel, Flags}) ->
    <<Channel:?UINT16, Flags:?UINT16>>;

field({fhss, Hop, Pattern}) ->
    <<Hop:8, Pattern:8>>;

field({mcs_index, Index}) ->
    <<1, Index:8>>;
field({rate, Rate}) ->
    <<Rate:8>>;

field({dbm_antsignal, Signal}) ->
    <<Signal:8>>;

field({dbm_antnoise, Noise}) ->
    <<Noise:8>>;

field({db_antsignal, Signal}) ->
    <<Signal:8>>;

field({db_antnoise, Noise}) ->
    <<Noise:8>>;

field({lock_quality, Qual}) ->
    <<Qual:8>>;

field({tx_attenuation, Power}) ->
    <<Power:?UINT16>>;

field({db_tx_attenuation, Power}) ->
    <<Power:?UINT16>>;

field({dbm_tx_power, Power}) ->
    <<Power:8>>;

field({flags, Bitmap}) ->
    <<Bitmap:8>>;

field({antenna, Index}) ->
    <<Index:8>>;

field({rx_flags, Bitmap}) ->
    <<Bitmap:8>>;

field({xchannel, Bitmap, Mhz, Channel, Max_power}) ->
    <<Bitmap:?UINT32, Mhz:?UINT16, Channel:8, Max_power:8>>;

field({mcs, Known, Flags, Mcs}) ->
    <<Known:8, Flags:8, Mcs:8>>;

field({vendor_namespace, {OUI1, OUI2, OUI3}, Subspace, Len}) ->
    <<OUI1:8, OUI2:8, OUI3:8, Subspace:8, Len:?UINT16>>.

%%-------------------------------------------------------------------------
%%% Internal functions
%%-------------------------------------------------------------------------
