
# Erlang interface for manipulating 802.11 wireless interfaces

## NOTE ON PRIVILEGES

To run this code, Erlang will have to either run as root or have
CAP\_NET\_ADMIN privileges:

    setcap cap_net_admin=ep /path/to/beam


## QUICK SETUP

    cd wierl
    make

    sudo setcap cap_net_admin=ep /path/to/beam

    ./start.sh
    % Scan using the "wlan0" interface 
    wierl_scan:list(<<"wlan0">>).

    # If you want to remove the privs
    sudo setcap -r /path/to/beam


## USAGE

### wierl_scan

    wierl_scan:list() ->  [{Interface, AccessPoints}]
    wierl_scan:list(Interface) ->  AccessPoints
    wierl_scan:list(Interface, Options) ->  AccessPoints

        Types   Interface = binary()
                AccessPoints = [AccessPoint]
                AccessPoint = {BSSID, ScanInfo}
                BSSID = binary()
                ScanInfo = [Info]
                Info = {Key, binary()}
                Key = [ custom
                    | encode
                    | essid
                    | freq
                    | genie
                    | mode
                    | qual
                    | rate ]
                Options = [{essid, binary()}]

    Initiate a wireless scan and return the scan list.

    wierl:format(AccessPoints) -> proplist()

    Decode some of the binary data values returned in the list of
    access points.

### wierl_config

    param(Ifname) -> Parameters
    param(Socket, Ifname, Attr) -> binary() | {error, unsupported}
            | {error, posix()}

        Types   Ifname = binary()
                Socket = int()
                Attr = {Key,Value} | Key
                Key = [ name
                    | nwid
                    | freq
                    | mode
                    | essid
                    | encode
                    | range
                    | ap
                    | rate
                    | power ]
                Value = binary() | integer()
                Parameters = [Parameter]
                Parameter = [ {name, binary()}
                    | {nwid, binary()}
                    | {freq, binary()}
                    | {mode, binary()}
                    | {essid, binary()}
                    | {encode, binary()}
                    | {range, binary()}
                    | {ap, binary()}
                    | {rate, binary()}
                    | {power, binary()} ]

    Query or set a wireless parameter.

    Use the wierl module to decode the values, e.g.,

        1> wierl:decode({freq,<<133,9,0,0,6,0,0,0,0,0,0,0,0,0,0,0>>}).
        {frequency,2.437e9}

    To set a parameter, a key/value as the attribute, e.g.,

        1> {ok,Socket} = wierl_config:open().
        {ok,7}
        2> wierl_config:param(Socket, <<"wlan0">>, {essid, <<"MY ESSID">>}).
        <<"MY ESSID">>

    Depending on the parameter, the value can be either an integer or a
    binary (which will be converted to a pointer to an iw_point struct
    and may require assigning a value to the flag field of the structure).

    To change some parameters, the interface must first be brought
    down. For example, to put the interface into monitor mode:

        wierl_config:down(<<"wlan0">>),
        {ok, Socket} = wierl_config:open(),
        wierl_config:param(Socket, <<"wlan0">>, {mode, wierl:mode(monitor)}),
        wierl_config:up(<<"wlan0">>),
        wierl_config:close(Socket).


    up(Ifname) -> ok

        Types   Ifname = binary()

    Configure the interface as up and running.


    down(Ifname) -> ok

        Types   Ifname = binary()

    Bring down the interface.


    open() -> {ok, FD}

        Types   FD = integer()

    Obtain a netlink socket file descriptor.


    close(FD) -> ok

        Types   FD = integer()

    Close the file descriptor.

### wierl

### rfkill

rfkill is a wireless soft kill switch.

    block() -> ok | {error, posix()}

    Disable wireless devices.

    unblock() -> ok | {error, posix()}

    Enable wireless devices.

    list() -> ok | {error, posix()}

    Monitor rfkill device events.


## TODO

* convert to gen_fsm/gen_server

* put interfaces into monitor mode

* dump 802.11 frames

