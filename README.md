
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

    wierl_scan:list() ->  [{Interface, AccessPoints}]
    wierl_scan:list(Interface) ->  AccessPoints
    wierl_scan:list(Interface, Options) ->  AccessPoints

        Types   Interface = binary()
                AccessPoints = [AccessPoint]
                AccessPoint = {BSSID, ScanInfo}
                BSSID = binary()
                ScanInfo = [Info]
                Info = {Key, binary()}
                Key = [ custom | encode | essid | freq | genie | mode |
                    qual | rate ]
                Options = [{essid, binary()}]

    Initiate a wireless scan and return the scan list.

    wierl_scan:format(AccessPoints) -> proplist()

    Decode some of the binary data values returned in the list of
    access points.


## TODO

* convert to gen_fsm/gen_server

* put interfaces into monitor mode

* dump 802.11 frames

