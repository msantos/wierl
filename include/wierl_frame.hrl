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

-define(IEEE802_11_TSTAMP_LEN, 8).
-define(IEEE802_11_AP_LEN, 6).

-define(IEEE802_11_PROTOCOL_VERSION, 0).

-define(FC_VERSION(Fc), ((Fc) band 16#3)).
-define(FC_TYPE(Fc), (((Fc) bsr 2) band 16#3)).
-define(FC_SUBTYPE(Fc), (((Fc) bsr 4) band 16#F)).
-define(FC_TO_DS(Fc), ((Fc) band 16#0100)).
-define(FC_FROM_DS(Fc), ((Fc) band 16#0200)).
-define(FC_MORE_FLAG(Fc), ((Fc) band 16#0400)).
-define(FC_RETRY(Fc), ((Fc) band 16#0800)).
-define(FC_POWER_MGMT(Fc), ((Fc) band 16#1000)).
-define(FC_MORE_DATA(Fc), ((Fc) band 16#2000)).
-define(FC_WEP(Fc), ((Fc) band 16#4000)).
-define(FC_ORDER(Fc), ((Fc) band 16#8000)).

-define(T_MGMT, 16#0).
-define(T_CTRL, 16#1).
-define(T_DATA, 16#2).
-define(T_RESV, 16#3).

-define(ST_ASSOC_REQUEST, 16#0).
-define(ST_ASSOC_RESPONSE, 16#1).
-define(ST_REASSOC_REQUEST, 16#2).
-define(ST_REASSOC_RESPONSE, 16#3).
-define(ST_PROBE_REQUEST, 16#4).
-define(ST_PROBE_RESPONSE, 16#5).
-define(ST_BEACON, 16#8).
-define(ST_ATIM, 16#9).
-define(ST_DISASSOC, 16#A).
-define(ST_AUTH, 16#B).
-define(ST_DEAUTH, 16#C).

-define(CTRL_PS_POLL, 16#A).
-define(CTRL_RTS, 16#B).
-define(CTRL_CTS, 16#C).
-define(CTRL_ACK, 16#D).
-define(CTRL_CF_END, 16#E).
-define(CTRL_END_ACK, 16#F).

-define(DATA_DATA, 16#0).
-define(DATA_DATA_CF_ACK, 16#1).
-define(DATA_DATA_CF_POLL, 16#2).
-define(DATA_DATA_CF_ACK_POLL, 16#3).
-define(DATA_NODATA, 16#4).
-define(DATA_NODATA_CF_ACK, 16#5).
-define(DATA_NODATA_CF_POLL, 16#6).
-define(DATA_NODATA_CF_ACK_POLL, 16#7).

-define(DATA_QOS_DATA, 16#8).
-define(DATA_QOS_DATA_CF_ACK, 16#9).
-define(DATA_QOS_DATA_CF_POLL, 16#A).
-define(DATA_QOS_DATA_CF_ACK_POLL, 16#B).
-define(DATA_QOS_NODATA, 16#C).
-define(DATA_QOS_CF_POLL_NODATA, 16#E).
-define(DATA_QOS_CF_ACK_POLL_NODATA, 16#F).

-define(DATA_FRAME_IS_CF_ACK(X), ((X) band 16#01)).
-define(DATA_FRAME_IS_CF_POLL(X), ((X) band 16#02)).
-define(DATA_FRAME_IS_NULL(X), ((X) band 16#04)).
-define(DATA_FRAME_IS_QOS(X), ((X) band 16#08)).

-define(E_SSID, 0).
-define(E_RATES, 1).
-define(E_FH, 2).
-define(E_DS, 3).
-define(E_CF, 4).
-define(E_TIM, 5).
-define(E_IBSS, 6).
-define(E_CHALLENGE, 16).


-record(ieee802_11_radiotap, {
        version = 0,
        pad = 0,
        len = 0,
        present = []
    }).

-record(ieee802_11_fc, {
        version = ?IEEE802_11_PROTOCOL_VERSION,
        type = <<>>,
        subtype = <<>>,
        to_ds = 0,
        from_ds = 0,
        more_frag = 0,
        retry = 0,
        power_management = 0,
        more_data = 0,
        protected = 0,
        order = 0
    }).

-record(ieee802_11_management, {
        duration = 0,
        seq_ctl = 0,
        sa = <<0,0,0,0,0,0>>,
        da = <<0,0,0,0,0,0>>,
        bssid = <<0,0,0,0,0,0>>
    }).

-record(ieee802_11_cf_rts, {
        duration = 0,
        ra = <<0,0,0,0,0,0>>,
        ta = <<0,0,0,0,0,0>>
    }).

-record(ieee802_11_cf_cts, {
        duration = 0,
        ra = <<0,0,0,0,0,0>>
    }).

-record(ieee802_11_cf_ack, {
        duration = 0,
        ra = <<0,0,0,0,0,0>>
    }).

-record(ieee802_11_cf_ps, {
        aid = 0,
        bssid = <<0,0,0,0,0,0>>,
        ta = <<0,0,0,0,0,0>>
    }).

-record(ieee802_11_cf_cfend, {
        duration = 0,
        ra = <<0,0,0,0,0,0>>,
        bssid = <<0,0,0,0,0,0>>
    }).

-record(ieee802_11_cf_bar, {
        duration = 0,
        ra = <<0,0,0,0,0,0>>,
        ta = <<0,0,0,0,0,0>>,
        bar = 0,
        seq_ctl = 0
    }).

-record(ieee802_11_cf_ba, {
        duration = 0,
        ba = 0,
        seq_ctl = 0,
        bitmap = <<>>
    }).

-record(ieee802_11_data, {
        duration = 0,
        da = <<0,0,0,0,0,0>>,
        sa = <<0,0,0,0,0,0>>,
        ra = <<0,0,0,0,0,0>>,
        ta = <<0,0,0,0,0,0>>,
        bssid = <<0,0,0,0,0,0>>
    }).
