{application, wierl,
    [
    {description, "Erlang interface for manipulating 802.11 devices"},
    {vsn, "0.01"},
    {modules, [
        wierl,
        wierl_scan,
        wierl_config,
        rfkill
            ]},
    {registered, []},
    {applications, [
        kernel,
        stdlib
            ]},
    {env, []}
    ]}.

