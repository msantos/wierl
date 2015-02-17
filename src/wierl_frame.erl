%% Copyright (c) 2011-2015, Michael Santos <michael.santos@gmail.com>
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
        control/1, type/2,
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
control(<<
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
control(#ieee802_11_fc{
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
type(#ieee802_11_fc{type = 0, subtype = 0},
    <<?UINT16LE(Duration),
    DA:6/bytes, SA:6/bytes, BSSID:6/bytes,
    ?UINT16LE(SeqCtl),

    ?UINT16LE(Capability),
    ?UINT16LE(Listen),

    Data/binary>>) ->

    {Body, FCS} = management_body(Data),

    {#ieee802_11_management{
            duration = Duration,
            da = DA,
            sa = SA,
            bssid = BSSID,
            seq_ctl = field(seq_ctl, SeqCtl),

            body = [{capability, Capability}, {listen_interval, Listen}] ++
                Body
    }, FCS};

% Association response
% Re-association response
type(#ieee802_11_fc{type = 0, subtype = Subtype},
    <<?UINT16LE(Duration),
    DA:6/bytes, SA:6/bytes, BSSID:6/bytes,
    ?UINT16LE(SeqCtl),

    ?UINT16LE(Capability),
    ?UINT16LE(Status),
    ?UINT16LE(Aid),

    Data/binary>>) when Subtype == 1; Subtype == 3 ->

    {Body, FCS} = management_body(Data),

    {#ieee802_11_management{
            duration = Duration,
            da = DA,
            sa = SA,
            bssid = BSSID,
            seq_ctl = field(seq_ctl, SeqCtl),

            body = [{capability, Capability}, {status_code, Status}, {aid, Aid}] ++
                Body
    }, FCS};

% Re-association request
type(#ieee802_11_fc{type = 0, subtype = 2},
    <<?UINT16LE(Duration),
    DA:6/bytes, SA:6/bytes, BSSID:6/bytes,
    ?UINT16LE(SeqCtl),

    ?UINT16LE(Capability),
    ?UINT16LE(Listen),
    AP:6/bytes,

    Data/binary>>) ->

    {Body, FCS} = management_body(Data),

    {#ieee802_11_management{
            duration = Duration,
            da = DA,
            sa = SA,
            bssid = BSSID,
            seq_ctl = field(seq_ctl, SeqCtl),

            body = [{capability, Capability}, {listen_interval, Listen}, {ap, AP}] ++
                Body
    }, FCS};

% Probe request
type(#ieee802_11_fc{type = 0, subtype = 4},
    <<?UINT16LE(Duration),
    DA:6/bytes, SA:6/bytes, BSSID:6/bytes,
    ?UINT16LE(SeqCtl),

    Data/binary>>) ->

    {Body, FCS} = management_body(Data),

    {#ieee802_11_management{
            duration = Duration,
            da = DA,
            sa = SA,
            bssid = BSSID,
            seq_ctl = field(seq_ctl, SeqCtl),

            body = Body
    }, FCS};

% Probe response
type(#ieee802_11_fc{type = 0, subtype = 5},
    <<?UINT16LE(Duration),
    DA:6/bytes, SA:6/bytes, BSSID:6/bytes,
    ?UINT16LE(SeqCtl),

    Timestamp:8/bytes,
    ?UINT16LE(Beacon),
    ?UINT16LE(Capability),

    Data/binary>>) ->

    {Body, FCS} = management_body(Data),

    {#ieee802_11_management{
            duration = Duration,
            da = DA,
            sa = SA,
            bssid = BSSID,
            seq_ctl = field(seq_ctl, SeqCtl),

            body = [{timestamp, Timestamp}, {beacon_interval, Beacon},
                {capability, Capability}] ++ Body
    }, FCS};

% Beacon
type(#ieee802_11_fc{type = 0, subtype = 8},
    <<?UINT16LE(Duration),
    DA:6/bytes, SA:6/bytes, BSSID:6/bytes,
    ?UINT16LE(SeqCtl),

    Timestamp:8/bytes,
    ?UINT16LE(Interval),
    ?UINT16LE(Capability),

    Data/binary>>) ->

    {Body, FCS} = management_body(Data),

    {#ieee802_11_management{
            duration = Duration,
            da = DA,
            sa = SA,
            bssid = BSSID,
            seq_ctl = field(seq_ctl, SeqCtl),

            body = [{timestamp, Timestamp}, {interval, Interval},
                {capability, Capability}] ++ Body
    }, FCS};

% IBSS ATIM
type(#ieee802_11_fc{type = 0, subtype = 9},
    <<?UINT16LE(Duration),
    DA:6/bytes, SA:6/bytes, BSSID:6/bytes,
    ?UINT16LE(SeqCtl)>>) ->
    {#ieee802_11_management{
            duration = Duration,
            da = DA,
            sa = SA,
            bssid = BSSID,
            seq_ctl = field(seq_ctl, SeqCtl)
        }, <<>>};
type(#ieee802_11_fc{type = 0, subtype = 9},
    <<?UINT16LE(Duration),
    DA:6/bytes, SA:6/bytes, BSSID:6/bytes,
    ?UINT16LE(SeqCtl), FCS:4/bytes>>) ->
    {#ieee802_11_management{
            duration = Duration,
            da = DA,
            sa = SA,
            bssid = BSSID,
            seq_ctl = field(seq_ctl, SeqCtl)
        }, FCS};

% Disassociation
type(#ieee802_11_fc{type = 0, subtype = 10},
    <<?UINT16LE(Duration),
    DA:6/bytes, SA:6/bytes, BSSID:6/bytes,
    ?UINT16LE(SeqCtl),

    ?UINT16LE(Reason), Vendor/binary
    >>) ->
    {#ieee802_11_management{
            duration = Duration,
            da = DA,
            sa = SA,
            bssid = BSSID,
            seq_ctl = field(seq_ctl, SeqCtl),

            body = [{reason_code, Reason}, {vendor, Vendor}]
        }, false};

% Authentication
type(#ieee802_11_fc{type = 0, subtype = 11},
    <<?UINT16LE(Duration),
    DA:6/bytes, SA:6/bytes, BSSID:6/bytes,
    ?UINT16LE(SeqCtl),

    ?UINT16LE(Alg),
    ?UINT16LE(TransSeqNum),
    ?UINT16LE(Status),

    Data/binary>>) ->

    {Body, FCS} = management_body(Data),

    {#ieee802_11_management{
            duration = Duration,
            da = DA,
            sa = SA,
            bssid = BSSID,
            seq_ctl = field(seq_ctl, SeqCtl),

            body = [{auth_alg, Alg}, {auth_trans_seq_num, TransSeqNum},
                {status_code, Status}] ++ Body
    }, FCS};

% Deauthentication
type(#ieee802_11_fc{type = 0, subtype = 12},
    <<?UINT16LE(Duration),
    DA:6/bytes, SA:6/bytes, BSSID:6/bytes,
    ?UINT16LE(SeqCtl),

    ?UINT16LE(Reason), Vendor/binary>>) ->
    {#ieee802_11_management{
            duration = Duration,
            da = DA,
            sa = SA,
            bssid = BSSID,
            seq_ctl = field(seq_ctl, SeqCtl),

            body = [{reason_code, Reason}, {vendor, Vendor}]
    }, false};

% Action
type(#ieee802_11_fc{type = 0, subtype = 11},
    <<?UINT16LE(Duration),
    DA:6/bytes, SA:6/bytes, BSSID:6/bytes,
    ?UINT16LE(SeqCtl),

    Category, Action/binary>>) ->
    {#ieee802_11_management{
            duration = Duration,
            da = DA,
            sa = SA,
            bssid = BSSID,
            seq_ctl = field(seq_ctl, SeqCtl),

            body = [{category, Category}, {action_detail, Action}]
        }, false};

% Unhandled, valid management frames
% Reserved: 0110-0111, 1110-1111
type(#ieee802_11_fc{type = 0, subtype = Subtype},
    <<?UINT16LE(Duration), DA:6/bytes, SA:6/bytes, BSSID:6/bytes,
    ?UINT16LE(SeqCtl), Body/binary>>) when Subtype band 2#0110 /= 2#0110 ->
    {#ieee802_11_management{
            duration = Duration,
            da = DA,
            sa = SA,
            bssid = BSSID,
            seq_ctl = field(seq_ctl, SeqCtl),

            body = Body
    }, false};
type(#ieee802_11_fc{type = 0},
    #ieee802_11_management{
            duration = Duration,
            da = DA,
            sa = SA,
            bssid = BSSID,
            seq_ctl = {FragNum, SeqCtl},

            body = Body
        }) ->
    <<?UINT16LE(Duration),
    DA:6/bytes, SA:6/bytes, BSSID:6/bytes,
    SeqCtl:12, FragNum:4, (elements_to_bin(Body))/binary>>;


%%
%% Control
%%

% Request to send (RTS)
type(#ieee802_11_fc{type = 1,
        subtype = 16#B},
    <<?UINT16LE(Duration),
    RA:6/bytes, TA:6/bytes>>) ->
    {#ieee802_11_cf_rts{
            duration = Duration,
            ra = RA,
            ta = TA
        }, <<>>};
type(#ieee802_11_fc{type = 1,
        subtype = 16#B},
    <<?UINT16LE(Duration),
    RA:6/bytes, TA:6/bytes, ?UINT32(FCS)>>) ->
    {#ieee802_11_cf_rts{
            duration = Duration,
            ra = RA,
            ta = TA
        }, FCS};
type(#ieee802_11_fc{type = 1,
        subtype = 16#B},
    #ieee802_11_cf_rts{
            duration = Duration,
            ra = RA,
            ta = TA
        }) ->
    <<?UINT16LE(Duration),
    RA:6/bytes, TA:6/bytes>>;

% Clear to send (CTS)
type(#ieee802_11_fc{type = 1,
        subtype = 16#C},
    <<?UINT16LE(Duration), RA:6/bytes>>) ->
    {#ieee802_11_cf_cts{
            duration = Duration,
            ra = RA
    }, <<>>};
type(#ieee802_11_fc{type = 1,
        subtype = 16#C},
    <<?UINT16LE(Duration), RA:6/bytes, FCS:4/bytes>>) ->
    {#ieee802_11_cf_cts{
            duration = Duration,
            ra = RA
    }, FCS};
type(#ieee802_11_fc{type = 1,
        subtype = 16#C},
    #ieee802_11_cf_cts{
            duration = Duration,
            ra = RA
        }) ->
    <<?UINT16LE(Duration), RA:6/bytes>>;

% Acknowledgement (ACK)
type(#ieee802_11_fc{type = 1,
        subtype = 16#D},
    <<?UINT16LE(Duration), RA:6/bytes>>) ->
    {#ieee802_11_cf_ack{
            duration = Duration,
            ra = RA
    }, <<>>};
type(#ieee802_11_fc{type = 1,
        subtype = 16#D},
    <<?UINT16LE(Duration), RA:6/bytes, FCS:4/bytes>>) ->
    {#ieee802_11_cf_ack{
            duration = Duration,
            ra = RA
    }, FCS};
type(#ieee802_11_fc{type = 1,
        subtype = 16#D},
    #ieee802_11_cf_ack{
            duration = Duration,
            ra = RA
        }) ->
    <<?UINT16LE(Duration), RA:6/bytes>>;

% Power save poll (PS)
type(#ieee802_11_fc{type = 1,
        subtype = 16#A},
    <<?UINT16LE(AID), BSSID:6/bytes, TA:6/bytes>>) ->
    {#ieee802_11_cf_ps{
            aid = AID,
            bssid = BSSID,
            ta = TA
        }, <<>>};
type(#ieee802_11_fc{type = 1,
        subtype = 16#A},
    <<?UINT16LE(AID), BSSID:6/bytes, TA:6/bytes, FCS:4/bytes>>) ->
    {#ieee802_11_cf_ps{
            aid = AID,
            bssid = BSSID,
            ta = TA
    }, FCS};
type(#ieee802_11_fc{type = 1,
        subtype = 16#A},
    #ieee802_11_cf_ps{
            aid = AID,
            bssid = BSSID,
            ta = TA
        }) ->
    <<?UINT16LE(AID), BSSID:6/bytes, TA:6/bytes>>;

% Contention free end; CF end + CF ACK
type(#ieee802_11_fc{type = 1,
        subtype = Subtype},
    <<?UINT16LE(Duration), RA:6/bytes, BSSID:6/bytes>>)
    when Subtype == 16#E; Subtype == 16#F ->
    {#ieee802_11_cf_cfend{
            duration = Duration,
            ra = RA,
            bssid = BSSID
        }, <<>>};
type(#ieee802_11_fc{type = 1,
        subtype = Subtype},
    <<?UINT16LE(Duration), RA:6/bytes, BSSID:6/bytes, FCS:4/bytes>>)
    when Subtype == 16#E; Subtype == 16#F ->
    {#ieee802_11_cf_cfend{
            duration = Duration,
            ra = RA,
            bssid = BSSID
    }, FCS};
type(#ieee802_11_fc{type = 1,
        subtype = Subtype},
    #ieee802_11_cf_cfend{
            duration = Duration,
            ra = RA,
            bssid = BSSID
        })
    when Subtype == 16#E; Subtype == 16#F ->
    <<?UINT16LE(Duration), RA:6/bytes, BSSID:6/bytes>>;

% Block Ack Request (BlockAckReq)
type(#ieee802_11_fc{type = 1,
        subtype = 8},
    <<?UINT16LE(Duration), RA:6/bytes, TA:6/bytes,
    ?UINT16LE(BAR), ?UINT16LE(SeqCtl)>>) ->
    {#ieee802_11_cf_bar{
            duration = Duration,
            ra = RA,
            ta = TA,
            bar = {BAR band 16#0fff, BAR bsr 12},
            seq_ctl = field(seq_ctl, SeqCtl)
        }, <<>>};
type(#ieee802_11_fc{type = 1,
        subtype = 8},
    <<?UINT16LE(Duration), RA:6/bytes, TA:6/bytes,
    ?UINT16LE(BAR), ?UINT16LE(SeqCtl), FCS:4/bytes>>) ->
    {#ieee802_11_cf_bar{
            duration = Duration,
            ra = RA,
            ta = TA,
            bar = {BAR band 16#0fff, BAR bsr 12},
            seq_ctl = field(seq_ctl, SeqCtl)
    }, FCS};
type(#ieee802_11_fc{type = 1,
        subtype = 8},
    #ieee802_11_cf_bar{
            duration = Duration,
            ra = RA,
            ta = TA,
            bar = {Reserved, TID},
            seq_ctl = {FragNum, SeqNum}
        }) ->
    <<?UINT16LE(Duration), RA:6/bytes, TA:6/bytes,
    TID:4, Reserved:12, SeqNum:12, FragNum:4>>;

% Block Ack (BlockAck)
type(#ieee802_11_fc{type = 1,
        subtype = 9},
    <<?UINT16LE(Duration), ?UINT16LE(BA), ?UINT16LE(SeqCtl),
    Bitmap:128/bytes>>) ->
    {#ieee802_11_cf_ba{
            duration = Duration,
            ba = {BA band 16#0fff, BA bsr 12},
            seq_ctl = field(seq_ctl, SeqCtl),
            bitmap = Bitmap
    }, <<>>};
type(#ieee802_11_fc{type = 1,
        subtype = 9},
    <<?UINT16LE(Duration), ?UINT16LE(BA), ?UINT16LE(SeqCtl),
    Bitmap:128/bytes, FCS:4/bytes>>) ->
    {#ieee802_11_cf_ba{
            duration = Duration,
            ba = {BA band 16#0fff, BA bsr 12},
            seq_ctl = field(seq_ctl, SeqCtl),
            bitmap = Bitmap
    }, FCS};
type(#ieee802_11_fc{type = 1,
        subtype = 8},
    #ieee802_11_cf_ba{
            duration = Duration,
            ba = {Reserved, TID},
            seq_ctl = {FragNum, SeqNum},
            bitmap = Bitmap
        }) ->
    <<?UINT16LE(Duration), TID:4, Reserved:12, SeqNum:12,
    FragNum:4, Bitmap:(128*8)>>;

%%
%% Data
%%
type(#ieee802_11_fc{type = 2,
    to_ds = 0, from_ds = 0},
    <<?UINT16LE(Duration), DA:6/bytes, SA:6/bytes, BSSID:6/bytes, Body/binary>>) ->
    {#ieee802_11_data{
            duration = Duration,
            da = DA,
            sa = SA,
            bssid = BSSID,

            body = Body
        }, false};
type(#ieee802_11_fc{type = 2,
    to_ds = 0, from_ds = 0},
    #ieee802_11_data{
            duration = Duration,
            da = DA,
            sa = SA,
            bssid = BSSID,

            body = Body
        }) ->
    <<?UINT16LE(Duration), DA:6/bytes, SA:6/bytes, BSSID:6/bytes,
    (elements_to_bin(Body))/binary>>;

type(#ieee802_11_fc{type = 2,
    to_ds = 0, from_ds = 1},
    <<?UINT16LE(Duration), DA:6/bytes, BSSID:6/bytes, SA:6/bytes, Body/binary>>) ->
    {#ieee802_11_data{
            duration = Duration,
            da = DA,
            sa = SA,
            bssid = BSSID,

            body = Body
    }, false};
type(#ieee802_11_fc{type = 2,
    to_ds = 0, from_ds = 1},
    #ieee802_11_data{
            duration = Duration,
            da = DA,
            sa = SA,
            bssid = BSSID,

            body = Body
        }) ->
    <<?UINT16LE(Duration), DA:6/bytes, BSSID:6/bytes, SA:6/bytes,
    (elements_to_bin(Body))/binary>>;

type(#ieee802_11_fc{type = 2,
    to_ds = 1, from_ds = 0},
    <<?UINT16LE(Duration), BSSID:6/bytes, SA:6/bytes, DA:6/bytes, Body/binary>>) ->
    {#ieee802_11_data{
            duration = Duration,
            da = DA,
            sa = SA,
            bssid = BSSID,

            body = Body
    }, false};
type(#ieee802_11_fc{type = 2,
    to_ds = 1, from_ds = 0},
    #ieee802_11_data{
            duration = Duration,
            da = DA,
            sa = SA,
            bssid = BSSID,

            body = Body
        }) ->
    <<?UINT16LE(Duration), BSSID:6/bytes, SA:6/bytes, DA:6/bytes,
    (elements_to_bin(Body))/binary>>;

type(#ieee802_11_fc{type = 2,
    to_ds = 1, from_ds = 1},
    <<?UINT16LE(Duration), RA:6/bytes, TA:6/bytes, DA:6/bytes,
    SA:6/bytes, Body/binary>>) ->
    {#ieee802_11_data{
            duration = Duration,
            ra = RA,
            ta = TA,
            da = DA,
            sa = SA,

            body = Body
    }, false};
type(#ieee802_11_fc{type = 2,
    to_ds = 1, from_ds = 1},
    #ieee802_11_data{
            duration = Duration,
            ra = RA,
            ta = TA,
            da = DA,
            sa = SA,

            body = Body
        }) ->
    <<?UINT16LE(Duration), RA:6/bytes, TA:6/bytes, DA:6/bytes,
    SA:6/bytes, (elements_to_bin(Body))/binary>>;

%%
%% Reserved
%%
% FIXME handle FCS
type(#ieee802_11_fc{}, _Body) ->
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
element_type(16#00) -> ssid;
element_type(16#01) -> rates;
element_type(16#02) -> fh;
element_type(16#03) -> ds;
element_type(16#04) -> cf;
element_type(16#05) -> tim;
element_type(16#06) -> ibss;
element_type(16#07) -> country;
element_type(16#08) -> fh_hop_param;
element_type(16#09) -> fh_hop_table;
element_type(16#0a) -> request;
element_type(16#0b) -> qbss_load;
element_type(16#0c) -> edca_param;
element_type(16#0d) -> tspec;
element_type(16#0e) -> tclas;
element_type(16#0f) -> schedule;
element_type(16#10) -> challenge;
element_type(16#20) -> power_constraint;
element_type(16#21) -> power_capability;
element_type(16#22) -> tpc_request;
element_type(16#23) -> tpc_report;
element_type(16#24) -> supported_channels;
element_type(16#25) -> channel_switch_ann;
element_type(16#26) -> measure_req;
element_type(16#27) -> measure_rep;
element_type(16#28) -> quiet;
element_type(16#29) -> ibss_dfs;
element_type(16#2a) -> erp_info;
element_type(16#2b) -> ts_delay;
element_type(16#2c) -> tclass_process;
element_type(16#2d) -> ht_capability;
element_type(16#2e) -> qos_capability;
element_type(16#2f) -> erp_info_old;
element_type(16#30) -> rsn_ie;
element_type(16#31) -> reserved;
element_type(16#32) -> ext_supp_rates;
element_type(16#34) -> neighor_report;
element_type(16#3d) -> ht_info;
element_type(16#3e) -> secondary_channel_offset;
element_type(16#45) -> wsie;
element_type(16#48) -> bss_co_ex_20_40;
element_type(16#49) -> bss_intol_ch_rep_20_40;
element_type(16#7f) -> extended_capabilities;
element_type(16#80) -> agere_proprietrary;
element_type(16#85) -> cisco_ccx1_ckip;
element_type(16#88) -> cisco_unknown_88;
element_type(16#95) -> cisco_unknown_95;
element_type(16#96) -> cisco_unknown_96;
element_type(16#dd) -> vendor_specific_ie;
element_type(16#ad) -> symbol_proprietrary;

element_type(ssid)-> 16#00;
element_type(rates)-> 16#01;
element_type(fh)-> 16#02;
element_type(ds)-> 16#03;
element_type(cf)-> 16#04;
element_type(tim)-> 16#05;
element_type(ibss)-> 16#06;
element_type(country)-> 16#07;
element_type(fh_hop_param)-> 16#08;
element_type(fh_hop_table)-> 16#09;
element_type(request)-> 16#0a;
element_type(qbss_load)-> 16#0b;
element_type(edca_param)-> 16#0c;
element_type(tspec)-> 16#0d;
element_type(tclas)-> 16#0e;
element_type(schedule)-> 16#0f;
element_type(challenge)-> 16#10;
element_type(power_constraint)-> 16#20;
element_type(power_capability)-> 16#21;
element_type(tpc_request)-> 16#22;
element_type(tpc_report)-> 16#23;
element_type(supported_channels)-> 16#24;
element_type(channel_switch_ann)-> 16#25;
element_type(measure_req)-> 16#26;
element_type(measure_rep)-> 16#27;
element_type(quiet)-> 16#28;
element_type(ibss_dfs)-> 16#29;
element_type(erp_info)-> 16#2a;
element_type(ts_delay)-> 16#2b;
element_type(tclass_process)-> 16#2c;
element_type(ht_capability)-> 16#2d;
element_type(qos_capability)-> 16#2e;
element_type(erp_info_old)-> 16#2f;
element_type(rsn_ie)-> 16#30;
element_type(reserved)-> 16#31;
element_type(ext_supp_rates)-> 16#32;
element_type(neighor_report)-> 16#34;
element_type(ht_info)-> 16#3d;
element_type(secondary_channel_offset)-> 16#3e;
element_type(wsie)-> 16#45;
element_type(bss_co_ex_20_40)-> 16#48;
element_type(bss_intol_ch_rep_20_40)-> 16#49;
element_type(extended_capabilities)-> 16#7f;
element_type(agere_proprietrary)-> 16#80;
element_type(cisco_ccx1_ckip)-> 16#85;
element_type(cisco_unknown_88)-> 16#88;
element_type(cisco_unknown_95)-> 16#95;
element_type(cisco_unknown_96)-> 16#96;
element_type(vendor_specific_ie)-> 16#dd;
element_type(symbol_proprietrary)-> 16#ad;

element_type(N) -> {unsupported, N}.


% Managment frame bodies can be passed as proplists
% or as preconstructed binaries
elements_to_bin(N) when is_binary(N) ->
    N;
elements_to_bin(N) when is_list(N) ->
    % Remove unhandled types (weak check, element_type returns a
    % tuple ({unsupported, Key} on failure)
    Elements = [ {K,V} || {K,V} <- N, is_integer(element_type(K)) ],
    management_body(Elements).


management_body(Body) ->
    management_body(Body, []).

management_body(<<>>, Acc) ->
    {lists:reverse(Acc), <<>>};
% Driver includes the frame checksum
management_body(<<FCS:4/bytes>>, Acc) ->
    {lists:reverse(Acc), FCS};
management_body([], Acc) ->
    list_to_binary(lists:reverse(Acc));

management_body(<<?E_FH, _Len, ?UINT16LE(Dwell), HopSet, HopPattern,
    HopIndex, Rest/binary>>, Acc) ->
    management_body(Rest, [{fh, Dwell, HopSet, HopPattern, HopIndex}|Acc]);
management_body([{fh, Dwell, HopSet, HopPattern, HopIndex}|T], Acc) ->
    management_body(T, [<<?E_FH, 5, ?UINT16LE(Dwell), HopSet, HopPattern, HopIndex>>|Acc]);

management_body(<<?E_CF, _Len, Count, Period, ?UINT16LE(MaxDuration),
    ?UINT16LE(DurationRemaining), Rest/binary>>, Acc) ->
    management_body(Rest, [{cf, Count, Period, MaxDuration, DurationRemaining}|Acc]);
management_body([{cf, Count, Period, MaxDuration, DurationRemaining}|T], Acc) ->
    management_body(T, [<<?E_CF, 6, Count, Period, ?UINT16LE(MaxDuration), ?UINT16LE(DurationRemaining)>>|Acc]);

management_body(<<?E_TIM, Len, Count, Period, BitmapControl, Rest/binary>>, Acc) ->
    Size = Len-3,
    <<Bitmap:Size/bytes, Rest1/binary>> = Rest,
    management_body(Rest1, [{tim, Count, Period, BitmapControl, Bitmap}|Acc]);
management_body([{tim, Count, Period, BitmapControl, Bitmap}|T], Acc) ->
    Len = byte_size(Bitmap)+3,
    management_body(T, [<<?E_TIM, Len, Count, Period, BitmapControl, Bitmap/binary>>|Acc]);

management_body(<<Type, Len, Element:Len/bytes, Rest/binary>>, Acc) ->
    management_body(Rest, [{element_type(Type), Element}|Acc]);
management_body([{Type, Element}|T], Acc) ->
    N = element_type(Type),
    Len = byte_size(Element),
    management_body(T, [<<N, Len, Element/binary>>|Acc]).
