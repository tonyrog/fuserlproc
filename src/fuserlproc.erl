-module (fuserlproc).
-behaviour (application).
-export ([ start/0,
           start/2,
           stop/0,
           stop/1 ]).

%-=====================================================================-
%-                        application callbacks                        -
%-=====================================================================-

%% @hidden

start () ->
  application:start (fuserl),
  application:start (fuserlproc).

%% @hidden

start (_Type, _Args) ->
  io:format("fuserlproc: start called\n", []),
  { ok, LinkedIn } = application:get_env (fuserlproc, linked_in),
  { ok, MountPoint } = application:get_env (fuserlproc, mount_point),
  { ok, MountOpts } = application:get_env (fuserlproc, mount_opts),
  case application:get_env (fuserlproc, make_mount_point) of
    { ok, false } -> 
      ok;
    _ ->
      io:format("make_dir: ~p\n", [MountPoint]),
      case file:make_dir (MountPoint) of
        ok ->
          ok;
        { error, eexist } ->
          ok
      end
  end,

  fuserlprocsup:start_link (LinkedIn, MountPoint, MountOpts).

%% @hidden

stop () ->
  application:stop (fuserlproc).

%% @hidden

stop (_State) ->
  ok.
