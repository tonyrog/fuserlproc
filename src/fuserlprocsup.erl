-module (fuserlprocsup).
-behaviour (supervisor).

-export ([ start_link/2, start_link/3, init/1 ]).

%-=====================================================================-
%-                                Public                               -
%-=====================================================================-

start_link (LinkedIn, MountPoint) ->
  start_link (LinkedIn, MountPoint, "").

start_link (LinkedIn, MountPoint, MountOpts) ->
  supervisor:start_link (?MODULE, [ LinkedIn, MountPoint, MountOpts ]).


%-=====================================================================-
%-                         supervisor callbacks                        -
%-=====================================================================-

%% @hidden

init ([ LinkedIn, MountPoint, MountOpts ]) ->
  { ok,
    { { one_for_one, 3, 10 },
      [
        { fuserlprocsrv,
          { fuserlprocsrv, start_link, [ LinkedIn, MountPoint, MountOpts ] },
          permanent,
          10000,
          worker,
          [ fuserlprocsrv ]
        }
      ]
    }
  }.
