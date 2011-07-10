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
        header/2, frame_control/1, frame_type/2
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


%%
%% Radiotap header
%%
%% See: http://netbsd.gw.com/cgi-bin/man-cgi?ieee80211_radiotap+9+NetBSD-current
%%
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


%%
%% Frames
%%
%% See: Section 7
%% http://standards.ieee.org/getieee802/download/802.11-2007.pdf

%% Frame control header
%% 2 bytes, little endian
%%
%% Match frame control in big-endian format
frame_control(<<
    Subtype:4, Type:2, Version:2,

    Order:1, Protected:1, MoreData:1, PwrMgmt:1,
    Retry:1, MoreFrag:1, FromDS:1, ToDS:1,
    Data/binary>>) ->

    {#ieee802_11_fc{
        version = Version,
        type = Type,
        subtype = Subtype,
        to_ds = ToDS,
        from_ds = FromDS,
        more_frag = MoreFrag,
        retry = Retry,
        power_management = PwrMgmt,
        more_data = MoreData,
        protected = Protected,
        order = Order
    }, Data};
frame_control(#ieee802_11_fc{
        version = Version,
        type = Type,
        subtype = Subtype,
        to_ds = ToDS,
        from_ds = FromDS,
        more_frag = MoreFrag,
        retry = Retry,
        power_management = PwrMgmt,
        more_data = MoreData,
        protected = Protected,
        order = Order
    }) ->
    <<Subtype:4, Type:2, Version:2,
    Order:1, Protected:1, MoreData:1, PwrMgmt:1,
    Retry:1, MoreFrag:1, FromDS:1, ToDS:1>>.


%%
%% Management
%%
%% Reserved: 0110-0111, 1110-1111
frame_type(#ieee802_11_fc{type = 0, subtype = Subtype},
    <<Duration:?UINT16LE, DA:6/bytes, SA:6/bytes, BSSID:6/bytes,
    SeqCtl:?UINT16LE, Body/binary>>) when Subtype band 6 /= 6 ->
    {#ieee802_11_management{
            duration = Duration,
            da = DA,
            sa = SA,
            bssid = BSSID,
            seq_ctl = SeqCtl
        }, Body};
frame_type(#ieee802_11_fc{type = 0},
    #ieee802_11_management{
            duration = Duration,
            da = DA,
            sa = SA,
            bssid = BSSID,
            seq_ctl = SeqCtl
        }) ->
    <<Duration:?UINT16LE,
    DA:6/bytes, SA:6/bytes, BSSID:6/bytes,
    SeqCtl:?UINT16LE>>;


%%
%% Control
%%

% Request to send (RTS)
frame_type(#ieee802_11_fc{type = 1,
        subtype = 16#B},
    <<Duration:?UINT16LE,
    RA:6/bytes, TA:6/bytes>>) ->
    {#ieee802_11_cf_rts{
            duration = Duration,
            ra = RA,
            ta = TA
        }, <<>>};
frame_type(#ieee802_11_fc{type = 1,
        subtype = 16#B},
    #ieee802_11_cf_rts{
            duration = Duration,
            ra = RA,
            ta = TA
        }) ->
    <<Duration:?UINT16LE,
    RA:6/bytes, TA:6/bytes>>;

% Clear to send (CTS)
frame_type(#ieee802_11_fc{type = 1,
        subtype = 16#C},
    <<Duration:?UINT16LE, RA:6/bytes>>) ->
    {#ieee802_11_cf_cts{
            duration = Duration,
            ra = RA
        }, <<>>};
frame_type(#ieee802_11_fc{type = 1,
        subtype = 16#C},
    #ieee802_11_cf_cts{
            duration = Duration,
            ra = RA
        }) ->
    <<Duration:?UINT16LE, RA:6/bytes>>;

% Acknowledgement (ACK)
frame_type(#ieee802_11_fc{type = 1,
        subtype = 16#D},
    <<Duration:?UINT16LE, RA:6/bytes>>) ->
    {#ieee802_11_cf_ack{
            duration = Duration,
            ra = RA
        }, <<>>};
frame_type(#ieee802_11_fc{type = 1,
        subtype = 16#D},
    #ieee802_11_cf_ack{
            duration = Duration,
            ra = RA
        }) ->
    <<Duration:?UINT16LE, RA:6/bytes>>;

% Power save poll (PS)
frame_type(#ieee802_11_fc{type = 1,
        subtype = 16#A},
    <<AID:?UINT16LE, BSSID:6/bytes, TA:6/bytes>>) ->
    {#ieee802_11_cf_ps{
            aid = AID,
            bssid = BSSID,
            ta = TA
        }, <<>>};
frame_type(#ieee802_11_fc{type = 1,
        subtype = 16#A},
    #ieee802_11_cf_ps{
            aid = AID,
            bssid = BSSID,
            ta = TA
        }) ->
    <<AID:?UINT16LE, BSSID:6/bytes, TA:6/bytes>>;

% Contention free end; CF end + CF ACK
frame_type(#ieee802_11_fc{type = 1,
        subtype = Subtype},
    <<Duration:?UINT16LE, RA:6/bytes, BSSID:6/bytes>>)
    when Subtype == 16#E; Subtype == 16#F ->
    {#ieee802_11_cf_cfend{
            duration = Duration,
            ra = RA,
            bssid = BSSID
        }, <<>>};
frame_type(#ieee802_11_fc{type = 1,
        subtype = Subtype},
    #ieee802_11_cf_cfend{
            duration = Duration,
            ra = RA,
            bssid = BSSID
        })
    when Subtype == 16#E; Subtype == 16#F ->
    <<Duration:?UINT16LE, RA:6/bytes, BSSID:6/bytes>>;

% Block Ack Request (BlockAckReq)
frame_type(#ieee802_11_fc{type = 1,
        subtype = 8},
    <<Duration:?UINT16LE, RA:6/bytes, TA:6/bytes,
    BAR:?UINT16LE, SeqCtl:?UINT16LE>>) ->
    {#ieee802_11_cf_bar{
            duration = Duration,
            ra = RA,
            ta = TA,
            bar = BAR,
            seq_ctl = SeqCtl
        }, <<>>};
frame_type(#ieee802_11_fc{type = 1,
        subtype = 8},
    #ieee802_11_cf_bar{
            duration = Duration,
            ra = RA,
            ta = TA,
            bar = BAR,
            seq_ctl = SeqCtl
        }) ->
    <<Duration:?UINT16LE, RA:6/bytes, TA:6/bytes,
    BAR:?UINT16LE, SeqCtl:?UINT16LE>>;

% Block Ack (BlockAck)
frame_type(#ieee802_11_fc{type = 1,
        subtype = 9},
    <<Duration:?UINT16LE, BA:?UINT16LE, SeqCtl:?UINT16LE,
    Bitmap:128/bytes>>) ->
    {#ieee802_11_cf_ba{
            duration = Duration,
            ba = BA,
            seq_ctl = SeqCtl,
            bitmap = Bitmap
        }, <<>>};
frame_type(#ieee802_11_fc{type = 1,
        subtype = 8},
    #ieee802_11_cf_ba{
            duration = Duration,
            ba = BA,
            seq_ctl = SeqCtl,
            bitmap = Bitmap
        }) ->
    <<Duration:?UINT16LE, BA:?UINT16LE, SeqCtl:?UINT16LE,
    Bitmap:(128*8)>>;

%%
%% Data
%%
frame_type(#ieee802_11_fc{type = 2,
    to_ds = 0, from_ds = 0},
    <<Duration:?UINT16LE, DA:6/bytes, SA:6/bytes, BSSID:6/bytes, Body/binary>>) ->
    {#ieee802_11_data{
            duration = Duration,
            da = DA,
            sa = SA,
            bssid = BSSID
        }, Body};
frame_type(#ieee802_11_fc{type = 2,
    to_ds = 0, from_ds = 0},
    #ieee802_11_data{
            duration = Duration,
            da = DA,
            sa = SA,
            bssid = BSSID
        }) ->
    <<Duration:?UINT16LE, DA:6/bytes, SA:6/bytes, BSSID:6/bytes>>;

frame_type(#ieee802_11_fc{type = 2,
    to_ds = 0, from_ds = 1},
    <<Duration:?UINT16LE, DA:6/bytes, BSSID:6/bytes, SA:6/bytes, Body/binary>>) ->
    {#ieee802_11_data{
            duration = Duration,
            da = DA,
            sa = SA,
            bssid = BSSID
        }, Body};
frame_type(#ieee802_11_fc{type = 2,
    to_ds = 0, from_ds = 1},
    #ieee802_11_data{
            duration = Duration,
            da = DA,
            sa = SA,
            bssid = BSSID
        }) ->
    <<Duration:?UINT16LE, DA:6/bytes, BSSID:6/bytes, SA:6/bytes>>;

frame_type(#ieee802_11_fc{type = 2,
    to_ds = 1, from_ds = 0},
    <<Duration:?UINT16LE, BSSID:6/bytes, SA:6/bytes, DA:6/bytes, Body/binary>>) ->
    {#ieee802_11_data{
            duration = Duration,
            da = DA,
            sa = SA,
            bssid = BSSID
        }, Body};
frame_type(#ieee802_11_fc{type = 2,
    to_ds = 1, from_ds = 0},
    #ieee802_11_data{
            duration = Duration,
            da = DA,
            sa = SA,
            bssid = BSSID
        }) ->
    <<Duration:?UINT16LE, BSSID:6/bytes, SA:6/bytes, DA:6/bytes>>;

frame_type(#ieee802_11_fc{type = 2,
    to_ds = 1, from_ds = 1},
    <<Duration:?UINT16LE, RA:6/bytes, TA:6/bytes, DA:6/bytes,
    SA:6/bytes, Body/binary>>) ->
    {#ieee802_11_data{
            duration = Duration,
            ra = RA,
            ta = TA,
            da = DA,
            sa = SA
        }, Body};
frame_type(#ieee802_11_fc{type = 2,
    to_ds = 1, from_ds = 1},
    #ieee802_11_data{
            duration = Duration,
            ra = RA,
            ta = TA,
            da = DA,
            sa = SA
        }) ->
    <<Duration:?UINT16LE, RA:6/bytes, TA:6/bytes, DA:6/bytes,
    SA:6/bytes>>;

%%
%% Reserved
%%
frame_type(#ieee802_11_fc{}, _Body) ->
    reserved.


%%-------------------------------------------------------------------------
%%% Internal functions
%%-------------------------------------------------------------------------
bool(0) -> false;
bool(1) -> true.
