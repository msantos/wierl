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
%% Reserved: 0110-0111, 1110-1111
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
