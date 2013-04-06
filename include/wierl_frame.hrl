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

-define(IEEE802_11_TSTAMP_LEN, 8).
-define(IEEE802_11_AP_LEN, 6).

-define(IEEE802_11_PROTOCOL_VERSION, 0).

-define(E_SSID, 0).
-define(E_RATES, 1).
-define(E_FH, 2).
-define(E_DS, 3).
-define(E_CF, 4).
-define(E_TIM, 5).
-define(E_IBSS, 6).
-define(E_CHALLENGE, 16).


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
        bssid = <<0,0,0,0,0,0>>,
        body = []
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
        bssid = <<0,0,0,0,0,0>>,
        body = []
    }).


%%-------------------------------------------------------------------------
%%% Radiotap header
%%-------------------------------------------------------------------------
-record(ieee802_11_radiotap, {
        version = 0,
        pad = 0,
        len = 0,
        present = [],
        rest = <<>>
    }).


%%-------------------------------------------------------------------------
%%% PRISM header
%%-------------------------------------------------------------------------
-define(PRISM_MSGCODE, 16#0041).

-record(prism_value, {
        did = 0,
        status = 0,
        len = 0,
        data = 0
    }).

-record(ieee802_11_prism, {
        msgcode = ?PRISM_MSGCODE,
        msglen = 144,
        devname = <<0:(16*8)>>,
        hosttime = #prism_value{},
        mactime = #prism_value{},
        channel = #prism_value{},
        rssi = #prism_value{},
        sq = #prism_value{},
        signal = #prism_value{},
        noise = #prism_value{},
        rate = #prism_value{},
        istx = #prism_value{},
        frmlen = #prism_value{}
    }).
