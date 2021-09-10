%% Copyright (c) 2012-2021, Michael Santos <michael.santos@gmail.com>
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

-define(RFKILL_STATE_SOFT_BLOCKED, 0).
-define(RFKILL_STATE_UNBLOCKED, 1).
-define(RFKILL_STATE_HARD_BLOCKED, 2).

-define(RFKILL_OP_ADD, 0).
-define(RFKILL_OP_DEL, 1).
-define(RFKILL_OP_CHANGE, 2).
-define(RFKILL_OP_CHANGE_ALL, 3).

-define(RFKILL_TYPE_ALL, 0).
-define(RFKILL_TYPE_WLAN, 1).
-define(RFKILL_TYPE_BLUETOOTH, 2).
-define(RFKILL_TYPE_UWB, 3).
-define(RFKILL_TYPE_WIMAX, 4).
-define(RFKILL_TYPE_WWAN, 5).
-define(RFKILL_TYPE_GPS, 6).
-define(RFKILL_TYPE_FM, 7).

-define(BLOCK, 1).
-define(UNBLOCK, 0).

%% struct rfkill_event {
%%     __u32 idx;
%%     __u8  type;
%%     __u8  op;
%%     __u8  soft, hard;
%% } __packed;
-record(rfkill_event, {
    idx = 0,
    type = 0,
    op = 0,
    soft = 0,
    hard = 0
}).
