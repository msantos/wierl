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
%% Encode/decode 802.11 frames
%%
-module(wierl_frame).
-export([
        frame_control/1, frame_type/2,
        field/2
    ]).

-include("wierl.hrl").
-include("wierl_frame.hrl").


%% See: Section 7
%% http://standards.ieee.org/getieee802/download/802.11-2007.pdf

%%-------------------------------------------------------------------------
%%% Frame control
%%-------------------------------------------------------------------------

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


%%-------------------------------------------------------------------------
%%% Frame types
%%-------------------------------------------------------------------------

%%
%% Management
%%

% Association request
frame_type(#ieee802_11_fc{type = 0, subtype = 0},
    <<Duration:?UINT16LE,
    DA:6/bytes, SA:6/bytes, BSSID:6/bytes,
    SeqCtl:?UINT16LE,

    Capability:?UINT16LE,
    Listen:?UINT16LE,

    Body/binary>>) ->
    {#ieee802_11_management{
            duration = Duration,
            da = DA,
            sa = SA,
            bssid = BSSID,
            seq_ctl = field(seq_ctl, SeqCtl)
        },
        [{capability, Capability}, {listen_interval, Listen}] ++
        management_body(Body)
    };

% Association response
frame_type(#ieee802_11_fc{type = 0, subtype = 1},
    <<Duration:?UINT16LE,
    DA:6/bytes, SA:6/bytes, BSSID:6/bytes,
    SeqCtl:?UINT16LE,

    Capability:?UINT16LE,
    Status:?UINT16LE,
    Aid:?UINT16LE,

    Body/binary>>) ->
    {#ieee802_11_management{
            duration = Duration,
            da = DA,
            sa = SA,
            bssid = BSSID,
            seq_ctl = field(seq_ctl, SeqCtl)
        },
        [{capability, Capability}, {status_code, Status, {aid, Aid}}] ++
        management_body(Body)
    };

% Beacon
frame_type(#ieee802_11_fc{type = 0, subtype = 8},
    <<Duration:?UINT16LE,
    DA:6/bytes, SA:6/bytes, BSSID:6/bytes,
    SeqCtl:?UINT16LE,

    % Frame body
    Timestamp:8/bytes,
    Interval:?UINT16LE,
    Capability:?UINT16LE,

    Body/binary>>) ->
    {#ieee802_11_management{
            duration = Duration,
            da = DA,
            sa = SA,
            bssid = BSSID,
            seq_ctl = field(seq_ctl, SeqCtl)
        },
        [{timestamp, Timestamp}, {interval, Interval},
            {capability, Capability}] ++
        management_body(Body)
    };

% IBSS ATIM
frame_type(#ieee802_11_fc{type = 0, subtype = 9},
    <<Duration:?UINT16LE,
    DA:6/bytes, SA:6/bytes, BSSID:6/bytes,
    SeqCtl:?UINT16LE>>) ->
    {#ieee802_11_management{
            duration = Duration,
            da = DA,
            sa = SA,
            bssid = BSSID,
            seq_ctl = field(seq_ctl, SeqCtl)
        }, []};

% Disassociation
frame_type(#ieee802_11_fc{type = 0, subtype = 10},
    <<Duration:?UINT16LE,
    DA:6/bytes, SA:6/bytes, BSSID:6/bytes,
    SeqCtl:?UINT16LE,

    Reason:?UINT16LE, Vendor/binary
    >>) ->
    {#ieee802_11_management{
            duration = Duration,
            da = DA,
            sa = SA,
            bssid = BSSID,
            seq_ctl = field(seq_ctl, SeqCtl)
        }, [{reason_code, Reason}, {vendor, Vendor}]};

% Unhandled, valid management frames
% Reserved: 0110-0111, 1110-1111
frame_type(#ieee802_11_fc{type = 0, subtype = Subtype},
    <<Duration:?UINT16LE, DA:6/bytes, SA:6/bytes, BSSID:6/bytes,
    SeqCtl:?UINT16LE, Body/binary>>) when Subtype band 6 /= 6 ->
    {#ieee802_11_management{
            duration = Duration,
            da = DA,
            sa = SA,
            bssid = BSSID,
            seq_ctl = field(seq_ctl, SeqCtl)
        }, Body};
frame_type(#ieee802_11_fc{type = 0},
    #ieee802_11_management{
            duration = Duration,
            da = DA,
            sa = SA,
            bssid = BSSID,
            seq_ctl = {FragNum, SeqCtl}
        }) ->
    <<Duration:?UINT16LE,
    DA:6/bytes, SA:6/bytes, BSSID:6/bytes,
    SeqCtl:12, FragNum:4>>;


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
            seq_ctl = field(seq_ctl, SeqCtl)
        }, <<>>};
frame_type(#ieee802_11_fc{type = 1,
        subtype = 8},
    #ieee802_11_cf_bar{
            duration = Duration,
            ra = RA,
            ta = TA,
            bar = BAR,
            seq_ctl = {FragNum, SeqNum}
        }) ->
    <<Duration:?UINT16LE, RA:6/bytes, TA:6/bytes,
    BAR:?UINT16LE, SeqNum:12, FragNum:4>>;

% Block Ack (BlockAck)
frame_type(#ieee802_11_fc{type = 1,
        subtype = 9},
    <<Duration:?UINT16LE, BA:?UINT16LE, SeqCtl:?UINT16LE,
    Bitmap:128/bytes>>) ->
    {#ieee802_11_cf_ba{
            duration = Duration,
            ba = BA,
            seq_ctl = field(seq_ctl, SeqCtl),
            bitmap = Bitmap
        }, <<>>};
frame_type(#ieee802_11_fc{type = 1,
        subtype = 8},
    #ieee802_11_cf_ba{
            duration = Duration,
            ba = BA,
            seq_ctl = {FragNum, SeqNum},
            bitmap = Bitmap
        }) ->
    <<Duration:?UINT16LE, BA:?UINT16LE, SeqNum:12, FragNum:4,
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
%%% Fields
%%-------------------------------------------------------------------------
field(seq_ctl, N) ->
    {N band 16#f, N bsr 4};
field(_, N) ->
    N.

%%-------------------------------------------------------------------------
%%% Internal functions
%%-------------------------------------------------------------------------

% Management body elements
element_to_atom(16#00) -> ssid;
element_to_atom(16#01) -> rates;
element_to_atom(16#02) -> fh;
element_to_atom(16#03) -> ds;
element_to_atom(16#04) -> cf;
element_to_atom(16#05) -> tim;
element_to_atom(16#06) -> ibss;
element_to_atom(16#07) -> country;
element_to_atom(16#08) -> fh_hop_param;
element_to_atom(16#09) -> fh_hop_table;
element_to_atom(16#0a) -> request;
element_to_atom(16#0b) -> qbss_load;
element_to_atom(16#0c) -> edca_param;
element_to_atom(16#0d) -> tspec;
element_to_atom(16#0e) -> tclas;
element_to_atom(16#0f) -> schedule;
element_to_atom(16#10) -> challenge;
element_to_atom(16#20) -> power_constraint;
element_to_atom(16#21) -> power_capability;
element_to_atom(16#22) -> tpc_request;
element_to_atom(16#23) -> tpc_report;
element_to_atom(16#24) -> supported_channels;
element_to_atom(16#25) -> channel_switch_ann;
element_to_atom(16#26) -> measure_req;
element_to_atom(16#27) -> measure_rep;
element_to_atom(16#28) -> quiet;
element_to_atom(16#29) -> ibss_dfs;
element_to_atom(16#2a) -> erp_info;
element_to_atom(16#2b) -> ts_delay;
element_to_atom(16#2c) -> tclass_process;
element_to_atom(16#2d) -> ht_capability;
element_to_atom(16#2e) -> qos_capability;
element_to_atom(16#2f) -> erp_info_old;
element_to_atom(16#30) -> rsn_ie;
element_to_atom(16#31) -> reserved;
element_to_atom(16#32) -> ext_supp_rates;
element_to_atom(16#34) -> neighor_report;
element_to_atom(16#3d) -> ht_info;
element_to_atom(16#3e) -> secondary_channel_offset;
element_to_atom(16#45) -> wsie;
element_to_atom(16#48) -> bss_co_ex_20_40;
element_to_atom(16#49) -> bss_intol_ch_rep_20_40;
element_to_atom(16#7f) -> extended_capabilities;
element_to_atom(16#80) -> agere_proprietrary;
element_to_atom(16#85) -> cisco_ccx1_ckip;
element_to_atom(16#88) -> cisco_unknown_88;
element_to_atom(16#95) -> cisco_unknown_95;
element_to_atom(16#96) -> cisco_unknown_96;
element_to_atom(16#dd) -> vendor_specific_ie;
element_to_atom(16#ad) -> symbol_proprietrary;

element_to_atom(ssid)-> 16#00;
element_to_atom(rates)-> 16#01;
element_to_atom(fh)-> 16#02;
element_to_atom(ds)-> 16#03;
element_to_atom(cf)-> 16#04;
element_to_atom(tim)-> 16#05;
element_to_atom(ibss)-> 16#06;
element_to_atom(country)-> 16#07;
element_to_atom(fh_hop_param)-> 16#08;
element_to_atom(fh_hop_table)-> 16#09;
element_to_atom(request)-> 16#0a;
element_to_atom(qbss_load)-> 16#0b;
element_to_atom(edca_param)-> 16#0c;
element_to_atom(tspec)-> 16#0d;
element_to_atom(tclas)-> 16#0e;
element_to_atom(schedule)-> 16#0f;
element_to_atom(challenge)-> 16#10;
element_to_atom(power_constraint)-> 16#20;
element_to_atom(power_capability)-> 16#21;
element_to_atom(tpc_request)-> 16#22;
element_to_atom(tpc_report)-> 16#23;
element_to_atom(supported_channels)-> 16#24;
element_to_atom(channel_switch_ann)-> 16#25;
element_to_atom(measure_req)-> 16#26;
element_to_atom(measure_rep)-> 16#27;
element_to_atom(quiet)-> 16#28;
element_to_atom(ibss_dfs)-> 16#29;
element_to_atom(erp_info)-> 16#2a;
element_to_atom(ts_delay)-> 16#2b;
element_to_atom(tclass_process)-> 16#2c;
element_to_atom(ht_capability)-> 16#2d;
element_to_atom(qos_capability)-> 16#2e;
element_to_atom(erp_info_old)-> 16#2f;
element_to_atom(rsn_ie)-> 16#30;
element_to_atom(reserved)-> 16#31;
element_to_atom(ext_supp_rates)-> 16#32;
element_to_atom(neighor_report)-> 16#34;
element_to_atom(ht_info)-> 16#3d;
element_to_atom(secondary_channel_offset)-> 16#3e;
element_to_atom(wsie)-> 16#45;
element_to_atom(bss_co_ex_20_40)-> 16#48;
element_to_atom(bss_intol_ch_rep_20_40)-> 16#49;
element_to_atom(extended_capabilities)-> 16#7f;
element_to_atom(agere_proprietrary)-> 16#80;
element_to_atom(cisco_ccx1_ckip)-> 16#85;
element_to_atom(cisco_unknown_88)-> 16#88;
element_to_atom(cisco_unknown_95)-> 16#95;
element_to_atom(cisco_unknown_96)-> 16#96;
element_to_atom(vendor_specific_ie)-> 16#dd;
element_to_atom(symbol_proprietrary)-> 16#ad;

element_to_atom(N) -> {unsupported, N}.


management_body(Body) ->
    management_body(Body, []).

management_body(<<>>, Acc) ->
    lists:reverse(Acc);

management_body(<<?E_FH, _Len, Dwell:?UINT16LE, HopSet, HopPattern,
    HopIndex, Rest/binary>>, Acc) ->
    management_body(Rest, [{fh, Dwell, HopSet, HopPattern, HopIndex}|Acc]);

management_body(<<?E_CF, _Len, Count, Period, MaxDuration:?UINT16LE,
    DurationRemaining:?UINT16LE, Rest/binary>>, Acc) ->
    management_body(Rest, [{cf, Count, Period, MaxDuration, DurationRemaining}|Acc]);

management_body(<<?E_TIM, Len, Count, Period, BitmapControl, Rest/binary>>, Acc) ->
    Size = Len-3,
    <<Bitmap:Size/bytes, Rest1/binary>> = Rest,
    management_body(Rest1, [{tim, Count, Period, BitmapControl, Bitmap}|Acc]);

management_body(<<Type, Len, Element:Len/bytes, Rest/binary>>, Acc) ->
    management_body(Rest, [{element_to_atom(Type), Element}|Acc]).
