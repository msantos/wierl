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
        open/1, close/1,
        decode/1, decode/2,
        header/2, frame/1
    ]).

-include("wierl.hrl").
-include("wierl_frame.hrl").

-define(ETH_P_ALL, 16#0003).
-define(PF_PACKET, 17).


open(Ifname) when byte_size(Ifname) < ?IFNAMSIZ ->
    {ok, Socket} = packet:socket(?ETH_P_ALL),
    Ifindex = packet:ifindex(Socket, binary_to_list(Ifname)),
    ok = packet:bind(Socket, Ifindex),
    {ok, Socket, Ifindex}.

close(Socket) when is_integer(Socket) ->
    procket:close(Socket).


decode(<<Version:8, Pad:8, Len:16/native, Present:?UINT32, _/binary>> = Frame) ->
    Len1 = Len-8,
    <<_:8/bytes, Header:Len1/bytes, Data/binary>> = Frame,

    <<Ext:1, Vendor_namespace:1, Namespace:1, _:9, Mcs:1, Xchannel:1, _:3, Rx_flags:1,
    Db_antnoise:1, Db_antsignal:1, Antenna:1, Dbm_tx_power:1, Db_tx_attenuation:1,
    Tx_attenuation:1, Lock_quality:1, Dbm_antnoise:1, Dbm_antsignal:1, Fhss:1, Channel:1, Rate:1,
    Flags:1, Tsft:1>> = <<Present:32>>,

    {#ieee802_11_radio{
        version = Version,
        pad = Pad,
        len = Len,
        present = [ {K, bool(V)} || {K,V} <-
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
                    {namepsace, Namespace},
                    {vendor_namepsace, Vendor_namespace},
                    {ext, Ext}
                ] ]
    }, Header, Data}.


% IEEE80211_RADIOTAP_TSFT              u_int64_t       microseconds
decode(tsft, <<Microsec:?UINT64, Data/binary>>) ->
    {{tsft, Microsec}, Data};

% IEEE80211_RADIOTAP_CHANNEL           2 x u_int16_t   MHz, bitmap
decode(channel, <<Channel:?UINT16, Flags:?UINT16, Data/binary>>) ->
    {{channel, Channel, Flags}, Data};

% IEEE80211_RADIOTAP_FHSS              u_int16_t       see below
decode(fhss, <<Hop:8, Pattern:8, Data/binary>>) ->
    {{fhss, Hop, Pattern}, Data};

% IEEE80211_RADIOTAP_RATE              u_int8_t        500kb/s or index
decode(rate, <<1, Index:8, Data/binary>>) ->
    {{mcs_index, Index}, Data};
decode(rate, <<Rate:8, Data/binary>>) ->
    {{rate, Rate}, Data};

% IEEE80211_RADIOTAP_DBM_ANTSIGNAL     int8_t          decibels from
decode(dbm_antsignal, <<Signal:8, Data/binary>>) ->
    {{dbm_antsignal, Signal}, Data};

% IEEE80211_RADIOTAP_DBM_ANTNOISE      int8_t          decibels from
decode(dbm_antnoise, <<Noise:8, Data/binary>>) ->
    {{dbm_antnoise, Noise}, Data};

% IEEE80211_RADIOTAP_LOCK_QUALITY      u_int16_t       unitless
decode(lock_quality, <<Qual:8, Data/binary>>) ->
    {{lock_quality, Qual}, Data};

% IEEE80211_RADIOTAP_TX_ATTENUATION    u_int16_t       unitless
decode(tx_attenuation, <<Power:?UINT16, Data/binary>>) ->
    {{tx_attenuation, Power}, Data};

% IEEE80211_RADIOTAP_DBM_TX_POWER      int8_t          decibels from
%                                                      one milliwatt (dBm)
decode(tx_power, <<Power:8, Data/binary>>) ->
    {{tx_power, Power}, Data};

% IEEE80211_RADIOTAP_FLAGS             u_int8_t        bitmap
decode(flags, <<Bitmap:8, Data/binary>>) ->
    {{flags, Bitmap}, Data};

% IEEE80211_RADIOTAP_ANTENNA           u_int8_t        antenna index
decode(antenna, <<Index:8, Data/binary>>) ->
    {{antenna, Index}, Data};

% IEEE80211_RADIOTAP_RX_FLAGS          u_int16_t       bitmap
decode(rx_flags, <<Bitmap:8, Data/binary>>) ->
    {{rx_flags, Bitmap}, Data};

% IEEE80211_RADIOTAP_XCHANNEL          u_int32_t   bitmap
%                  u_int16_t   MHz
%                  u_int8_t    channel number
%                  u_int8_t    .5 dBm
decode(xchannel, <<Bitmap:?UINT32, Mhz:?UINT16, Channel:8, Max_power:8, Data/binary>>) ->
    {{xchannel, Bitmap, Mhz, Channel, Max_power}, Data};

% IEEE80211_RADIOTAP_MCS       u_int8_t    known
%                  u_int8_t    flags
%                  u_int8_t    mcs
decode(mcs, <<Known:8, Flags:8, Mcs:8, Data/binary>>) ->
    {{mcs, Known, Flags, Mcs}, Data};

% IEEE80211_RADIOTAP_VENDOR_NAMESPACE
%                                   u_int8_t  OUI[3]
%                                   u_int8_t  subspace
%                                   u_int16_t length
decode(vendor_namespace, <<OUI1:8, OUI2:8, OUI3:8, Subspace:8, Len:?UINT16, Data/binary>>) ->
    {{vendor_namespace, {OUI1, OUI2, OUI3}, Subspace, Len}, Data}.


header(#ieee802_11_radio{} = R, Bin) when is_binary(Bin) ->
    {Header, Unknown} = lists:foldl(
        fun ({Type, true}, {Present, Data}) ->
                {Decoded, Rest} = wierl_frame:decode(Type, Data),
                {[Decoded|Present], Rest};
            ({_Type, false}, Present) -> Present
        end,
        {[], Bin},
        R#ieee802_11_radio.present),
    {lists:reverse(Header), Unknown}.


% struct mgmt_header_t {
%         u_int16_t   fc;
%         u_int16_t   duration;
%         u_int8_t    da[6];
%         u_int8_t    sa[6];
%         u_int8_t    bssid[6];
%         u_int16_t   seq_ctrl;
% };
frame(<<Fc:?UINT16, Rest/binary>>) when ?FC_TYPE(Fc) == ?T_MGMT ->
    {Duration, DA, SA, BSSID, Seq_ctl, Data} = mgmt(?FC_SUBTYPE(Fc), Rest),
    {{mgmt_header, Fc, Duration, DA, SA, BSSID, Seq_ctl}, Data}.

mgmt(?ST_BEACON, <<Duration:?UINT16,
    DA1,DA2,DA3,DA4,DA5,DA6,
    SA1,SA2,SA3,SA4,SA5,SA6,
    BS1,BS2,BS3,BS4,BS5,BS6,
    Seq_ctl:?UINT16, Rest/binary>>) ->
    {Duration, {DA1,DA2,DA3,DA4,DA5,DA6}, {SA1,SA2,SA3,SA4,SA5,SA6},
        {BS1,BS2,BS3,BS4,BS5,BS6}, Seq_ctl, Rest}.


type(tsft) -> 0;
type(flags) -> 1;
type(rate) -> 2;
type(channel) -> 3;
type(fhss) -> 4;
type(dbm_antsignal) -> 5;
type(dbm_antnoise) -> 6;
type(lock_quality) -> 7;
type(tx_attenuation) -> 8;
type(db_tx_attenuation) -> 9;
type(dbm_tx_power) -> 10;
type(antenna) -> 11;
type(db_antsignal) -> 12;
type(db_antnoise) -> 13;
type(ext) -> 31.


% IEEE80211_RADIOTAP_FLAGS
%flags(16#01) -> cfp;
%flags(16#02) -> shortpre;
%flags(16#04) -> wep;
%flags(16#08) -> frag;
%flags(16#10) -> fcs;
%flags(16#20) -> datapad;
%flags(16#40) -> badfcs;
%
%flags(cfp) -> 16#01;
%flags(shortpre) -> 16#02;
%flags(wep) -> 16#04;
%flags(frag) -> 16#08;
%flags(fcs) -> 16#10;
%flags(datapad) -> 16#20;
%flags(badfcs) -> 16#40.

bool(0) -> false;
bool(1) -> true.
