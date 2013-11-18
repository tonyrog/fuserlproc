%% @doc Proc filesystem for Erlang.
%% @end

-module (fuserlprocsrv).
-export ([ start_link/2, start_link/3 ]).
%-behaviour (fuserl).
-export ([ code_change/3,
           handle_info/2,
           init/1,
           terminate/2,
           getattr/4,
           lookup/5,
           open/5,
           read/7,
           readdir/7,
           readlink/4 ]).

-include_lib ("fuserl/include/fuserl.hrl").

-ifdef (HAVE_EUNIT).
-include_lib ("eunit/include/eunit.hrl").
-include_lib ("kernel/include/file.hrl").
-endif.

-record (fuserlprocsrv, { inodes, names }).

%-=====================================================================-
%-                                Public                               -
%-=====================================================================-

start_link (LinkedIn, Dir) ->
  start_link (LinkedIn, Dir, "").

start_link (LinkedIn, Dir, MountOpts) ->
  fuserlsrv:start_link (?MODULE, LinkedIn, MountOpts, Dir, [], []).

%-=====================================================================-
%-                           fuserl callbacks                          -
%-=====================================================================-

init ([]) ->
  State = #fuserlprocsrv{ inodes = gb_trees:from_orddict ([ { 6, [] } ]),
                          names = gb_trees:empty () },
  % cheesy: pre-alloc system info inodes
  { _, NewState } = readdir (void, 4, 100000, 0, void, void, State),
  { ok, NewState }.

code_change (_OldVsn, State, _Extra) -> { ok, State }.
handle_info (_Msg, State) -> { noreply, State }.
terminate (_Reason, _State) -> ok.

-define (DIRATTR (X), #stat{ st_ino = (X), 
                             st_mode = ?S_IFDIR bor 8#0555, 
                             st_nlink = 1 }).
-define (LINKATTR, #stat{ st_mode = ?S_IFLNK bor 8#0555, st_nlink = 1 }).

% / -> 1
% /ports -> 2
% /processes -> 3
% /system -> 4
% /nodes -> 5
% /environment -> 6
% 
% /ports/XXX -> inodes table
% /processses/XXX -> inodes table
% /system/XXX -> inodes table
% /nodes/XXX -> inodes table
% /environment/XXX -> inodes table

getattr (_, 1, _, State) ->
  { #fuse_reply_attr{ attr = ?DIRATTR (1), attr_timeout_ms = 1000 }, State };
getattr (_, 2, _, State) ->
  { #fuse_reply_attr{ attr = ?DIRATTR (2), attr_timeout_ms = 1000 }, State };
getattr (_, 3, _, State) ->
  { #fuse_reply_attr{ attr = ?DIRATTR (3), attr_timeout_ms = 1000 }, State };
getattr (_, 4, _, State) ->
  { #fuse_reply_attr{ attr = ?DIRATTR (4), attr_timeout_ms = 1000 }, State };
getattr (_, 5, _, State) ->
  { #fuse_reply_attr{ attr = ?DIRATTR (5), attr_timeout_ms = 1000 }, State };
getattr (_, 6, _, State) ->
  { #fuse_reply_attr{ attr = ?DIRATTR (6), attr_timeout_ms = 1000 }, State };
getattr (_, X, _, State) ->
  case gb_trees:lookup (X, State#fuserlprocsrv.inodes) of
    { value, { Type, _ } } when (Type =:= port) or
                                (Type =:= pid) or
                                (Type =:= nodes) ->
      { #fuse_reply_attr{ attr = ?DIRATTR (X), attr_timeout_ms = 1000 }, 
        State };
    { value, { Type, _ } } when (Type =:= registered) ->
      { #fuse_reply_attr{ attr = ?LINKATTR, attr_timeout_ms = 1000 }, State };
    { value, { env_var, Name } } ->
      Attr = env_var_attr (Name, State),
      { #fuse_reply_attr{ attr = Attr, attr_timeout_ms = 1000 }, State };
    { value, { nodes_item, NodesItem } } ->
      Attr = nodes_item_attr (NodesItem, State),
      { #fuse_reply_attr{ attr = Attr, attr_timeout_ms = 1000 }, State };
    { value, { pid_item, PidItem } } ->
      Attr = pid_item_attr (PidItem, State),
      { #fuse_reply_attr{ attr = Attr, attr_timeout_ms = 1000 }, State };
    { value, { port_item, PortItem } } ->
      { #fuse_reply_attr{ attr = port_item_attr (PortItem, State),
                          attr_timeout_ms = 1000 },
        State };
    { value, { system_item, Name } } ->
      Attr = system_item_attr (Name, State),
      { #fuse_reply_attr{ attr = Attr, attr_timeout_ms = 1000 }, State };
    none ->
      { #fuse_reply_err{ err = enoent }, State }
  end.

lookup (_, 1, <<"ports">>, _, State) ->
  { #fuse_reply_entry{ 
      fuse_entry_param = #fuse_entry_param{ ino = 2,
                                            generation = 1,  % (?)
                                            attr_timeout_ms = 1000,
                                            entry_timeout_ms = 1000,
                                            attr = ?DIRATTR (2) } },
    State };
lookup (_, 1, <<"processes">>, _, State) ->
  { #fuse_reply_entry{ 
      fuse_entry_param = #fuse_entry_param{ ino = 3,
                                            generation = 1,  % (?)
                                            attr_timeout_ms = 1000,
                                            entry_timeout_ms = 1000,
                                            attr = ?DIRATTR (3) } },
    State };
lookup (_, 1, <<"system">>, _, State) ->
  { #fuse_reply_entry{ 
      fuse_entry_param = #fuse_entry_param{ ino = 4,
                                            generation = 1,  % (?)
                                            attr_timeout_ms = 1000,
                                            entry_timeout_ms = 1000,
                                            attr = ?DIRATTR (4) } },
    State };
lookup (_, 1, <<"nodes">>, _, State) ->
  { #fuse_reply_entry{ 
      fuse_entry_param = #fuse_entry_param{ ino = 5,
                                            generation = 1,  % (?)
                                            attr_timeout_ms = 1000,
                                            entry_timeout_ms = 1000,
                                            attr = ?DIRATTR (5) } },
    State };
lookup (_, 1, <<"environment">>, _, State) ->
  { #fuse_reply_entry{ 
      fuse_entry_param = #fuse_entry_param{ ino = 6,
                                            generation = 1,  % (?)
                                            attr_timeout_ms = 1000,
                                            entry_timeout_ms = 1000,
                                            attr = ?DIRATTR (6) } },
    State };
lookup (_, 2, BinName, _, State) ->
  Name = erlang:binary_to_list (BinName),
  case gb_trees:lookup ({ port, Name }, State#fuserlprocsrv.names) of
    { value, { Ino, _ } } ->
      { #fuse_reply_entry{ 
          fuse_entry_param = #fuse_entry_param{ ino = Ino,
                                                generation = 1,  % (?)
                                                attr_timeout_ms = 1000,
                                                entry_timeout_ms = 1000,
                                                attr = ?DIRATTR (Ino) } },
        State };
    none ->
      % unfortunately, no erlang:list_to_port (Name) ...
      { #fuse_reply_err{ err = enoent }, State }
  end;
lookup (_, 3, BinName, _, State) ->
  Name = erlang:binary_to_list (BinName),
  case gb_trees:lookup ({ pid, Name }, State#fuserlprocsrv.names) of
    { value, { Ino, _ } } ->
      { #fuse_reply_entry{ 
          fuse_entry_param = #fuse_entry_param{ ino = Ino,
                                                generation = 1,  % (?)
                                                attr_timeout_ms = 1000,
                                                entry_timeout_ms = 1000,
                                                attr = ?DIRATTR (Ino) } },
        State };
    none ->
      case maybe_pid (Name) of
        { pid, Pid } ->
          { Ino, NewState } = make_inode ({ pid, Name }, Pid, State),
          { #fuse_reply_entry{ 
              fuse_entry_param = #fuse_entry_param{ ino = Ino,
                                                    generation = 1,  % (?)
                                                    attr_timeout_ms = 1000,
                                                    entry_timeout_ms = 1000,
                                                    attr = ?DIRATTR (Ino) } },
            NewState };
        { registered, Pid } ->
          { Ino, NewState } = make_inode ({ registered, Name }, Pid, State),
          { #fuse_reply_entry{
              fuse_entry_param = #fuse_entry_param{ ino = Ino,
                                                    generation = 1,  % (?)
                                                    attr_timeout_ms = 1000,
                                                    entry_timeout_ms = 1000,
                                                    attr = ?LINKATTR } },
              NewState };
        _ ->
          { #fuse_reply_err{ err = enoent }, State }
      end
  end;
lookup (_, 4, BinName, _, State) ->
  Name = erlang:binary_to_list (BinName),
  case gb_trees:lookup ({ system_item, Name }, State#fuserlprocsrv.names) of
    { value, { Ino, _ } } ->
      { #fuse_reply_entry{ 
          fuse_entry_param = 
            #fuse_entry_param{ ino = Ino,
                               generation = 1,  % (?)
                               attr_timeout_ms = 1000,
                               entry_timeout_ms = 1000,
                               attr = system_item_attr (Name, State) } },
        State };
    none ->
      { #fuse_reply_err{ err = enoent }, State }
  end;
lookup (_, 5, BinName, _, State) ->
  Name = erlang:binary_to_list (BinName),
  case gb_trees:lookup ({ nodes, Name }, State#fuserlprocsrv.names) of
    { value, { Ino, _ } } ->
      { #fuse_reply_entry{ 
          fuse_entry_param = #fuse_entry_param{ ino = Ino,
                                                generation = 1,  % (?)
                                                attr_timeout_ms = 1000,
                                                entry_timeout_ms = 1000,
                                                attr = ?DIRATTR (Ino) } },
        State };
    none ->
      case maybe_node_type (Name) of
        { node_type, Item } ->
          { Ino, NewState } = make_inode ({ nodes, Name }, Item, State),
          { #fuse_reply_entry{ 
              fuse_entry_param = #fuse_entry_param{ ino = Ino,
                                                    generation = 1,  % (?)
                                                    attr_timeout_ms = 1000,
                                                    entry_timeout_ms = 1000,
                                                    attr = ?DIRATTR (Ino) } },
            NewState };
        _ ->
          { #fuse_reply_err{ err = enoent }, State }
      end
  end;
lookup (_, 6, BinName, _, State) ->
  Name = erlang:binary_to_list (BinName),
  case gb_trees:lookup ({ env_var, Name }, State#fuserlprocsrv.names) of
    { value, { Ino, _ } } ->
      { #fuse_reply_entry{ 
          fuse_entry_param = #fuse_entry_param{ ino = Ino,
                                                generation = 1,  % (?)
                                                attr_timeout_ms = 1000,
                                                entry_timeout_ms = 1000,
                                                attr = env_var_attr (Name,
                                                                     State) } },
        State };
    none ->
      case maybe_env_var (Name) of
        true ->
          { Ino, NewState } = make_inode ({ env_var, Name }, void, State),
          { #fuse_reply_entry{ 
              fuse_entry_param = #fuse_entry_param{ ino = Ino,
                                                    generation = 1,  % (?)
                                                    attr_timeout_ms = 1000,
                                                    entry_timeout_ms = 1000,
                                                    attr = env_var_attr (Name,
                                                                         NewState) } },
            NewState };
        _ ->
          { #fuse_reply_err{ err = enoent }, State }
      end
  end;
lookup (_, X, Name, _, State) ->
  case gb_trees:lookup (X, State#fuserlprocsrv.inodes) of
    { value, { port, PortName } } ->
      lookup_port_item (PortName, 
                        list_to_atom (binary_to_list (Name)),
                        State);
    { value, { pid, PidName } } ->
      lookup_pid_item (PidName, 
                       list_to_atom (binary_to_list (Name)),
                       State);
    { value, { nodes, NodesName } } ->
      lookup_nodes_item (list_to_atom (NodesName),
                         list_to_atom (binary_to_list (Name)),
                         State);
    none ->
      { #fuse_reply_err{ err = enoent }, State }
  end.

open (_, X, Fi = #fuse_file_info{}, _, State) when X >= 1, X =< 6 ->
  case (Fi#fuse_file_info.flags band ?O_ACCMODE) =/= ?O_RDONLY of
    true ->
      { #fuse_reply_err{ err = eacces }, State };
    false ->
      { #fuse_reply_open{ fuse_file_info = Fi }, State }
  end;
open (_, X, Fi = #fuse_file_info{}, _, State) ->
  case gb_trees:lookup (X, State#fuserlprocsrv.inodes) of
    { value, { Type, _ } } when (Type =:= env_var) or
                                (Type =:= port) or
                                (Type =:= pid) or
                                (Type =:= port_item) or
                                (Type =:= pid_item) or
                                (Type =:= registered) or 
                                (Type =:= system_item) or
                                (Type =:= nodes) or
                                (Type =:= nodes_item) ->
      case (Fi#fuse_file_info.flags band ?O_ACCMODE) =/= ?O_RDONLY of
        true ->
          { #fuse_reply_err{ err = eacces }, State };
        false ->
          { #fuse_reply_open{ fuse_file_info = Fi }, State }
      end;
    none ->
      { #fuse_reply_err{ err = enoent }, State }
  end.

read (_, X, Size, Offset, _Fi, _, State) ->
  case gb_trees:lookup (X, State#fuserlprocsrv.inodes) of
    { value, { Type, _ } } when (Type =:= port) or
                                (Type =:= pid) or
                                (Type =:= nodes) ->
      { #fuse_reply_err{ err = eisdir }, State };
    { value, { env_var, Name } } ->
      read_env_var (Name, Size, Offset, State);
    { value, { nodes_item, NodesItem } } ->
      read_nodes_item (NodesItem, Size, Offset, State);
    { value, { pid_item, PidItem } } ->
      read_pid_item (PidItem, Size, Offset, State);
    { value, { port_item, PortItem } } ->
      read_port_item (PortItem, Size, Offset, State);
    { value, { registered, _ } } ->
      { #fuse_reply_err{ err = einval }, State };
    { value, { system_item, Name } } ->
      read_system_item (Name, Size, Offset, State);
    none ->
      { #fuse_reply_err{ err = enoent }, State }
  end.

% Well, these strategies really suck if the number of ports or processes
% gets into the thousands.

readdir (_, 1, Size, Offset, _Fi, _, State) ->
  DirEntryList = 
    take_while 
      (fun (E, { Total, Max }) -> 
         Cur = fuserlsrv:dirent_size (E),
         if 
           Total + Cur =< Max ->
             { continue, { Total + Cur, Max } };
           true ->
             stop
         end
       end,
       { 0, Size },
       lists:nthtail 
         (Offset,
          [ #direntry{ name = ".", offset = 1, stat = ?DIRATTR (1) },
            #direntry{ name = "..", offset = 2, stat = ?DIRATTR (1) },
            #direntry{ name = "ports", offset = 3, stat = ?DIRATTR (2) },
            #direntry{ name = "processes", offset = 4, stat = ?DIRATTR (3) },
            #direntry{ name = "system", offset = 5, stat = ?DIRATTR (4) },
            #direntry{ name = "nodes", offset = 6, stat = ?DIRATTR (5) },
            #direntry{ name = "environment", offset = 7, stat = ?DIRATTR (6) }
          ])),

  { #fuse_reply_direntrylist{ direntrylist = DirEntryList }, State };
readdir (_, 2, Size, Offset, _Fi, _, State) ->
  DirEntryList = 
    take_while 
      (fun (R, { Total, Max }) -> 
         Cur = case R of { E, _ } -> fuserlsrv:dirent_size (E);
                         E -> fuserlsrv:dirent_size (E)
               end,
         if 
           Total + Cur =< Max ->
             { continue, { Total + Cur, Max } };
           true ->
             stop
         end
       end,
       { 0, Size },
       safe_nthtail
         (Offset,
          [ #direntry{ name = ".", offset = 1, stat = ?DIRATTR (2) },
            #direntry{ name = "..", offset = 2, stat = ?DIRATTR (1) } ] ++
          [ { #direntry{ name = erlang:port_to_list (P), 
                         offset = N + 2 }, 
              P } ||
            { N, P } <- index (erlang:ports ()) ])),

  { MappedList, NewState } = 
    lists:mapfoldl (fun ({ E = #direntry{ name = Name }, Port }, Acc) ->
                      { Ino, NewAcc } = 
                        make_inode ({ port, Name }, Port, Acc),
                      { E#direntry{ stat = #stat{ st_ino = Ino } }, NewAcc };
                        (E = #direntry{}, Acc) ->
                      { E, Acc }
                    end,
                    State,
                    DirEntryList),
                      
  { #fuse_reply_direntrylist{ direntrylist = MappedList }, NewState };
readdir (_, 3, Size, Offset, _Fi, _, State) ->
  Processes = erlang:processes (),
  NumProcesses = erlang:length (Processes),

  DirEntryList = 
    take_while 
      (fun (R, { Total, Max }) -> 
         Cur = case R of { E, _ } -> fuserlsrv:dirent_size (E);
                         E -> fuserlsrv:dirent_size (E)
               end,
         if 
           Total + Cur =< Max ->
             { continue, { Total + Cur, Max } };
           true ->
             stop
         end
       end,
       { 0, Size },
       safe_nthtail 
         (Offset,
          [ #direntry{ name = ".", offset = 1, stat = ?DIRATTR (3) },
            #direntry{ name = "..", offset = 2, stat = ?DIRATTR (1) } ] ++
          [ { #direntry{ name = erlang:pid_to_list (P), offset = N + 2 }, 
              { pid, P } } ||
            { N, P } <- index (Processes) ] ++
          [ { #direntry{ name = atom_to_list (R), offset = N + 2 + NumProcesses }, 
              { registered, P } } ||
            { N, { P, R } }
              <- index 
                   ([ { P, R } 
                      || P <- Processes, 
                         { registered_name, R } <-
                           [ erlang:process_info (P, registered_name) ] ]) ])),

  { MappedList, NewState } = 
    lists:mapfoldl (fun ({ E = #direntry{ name = Name }, { pid, Pid } }, Acc) ->
                      { Ino, NewAcc } = 
                        make_inode ({ pid, Name }, Pid, Acc),
                      { E#direntry{ stat = #stat{ st_ino = Ino } }, NewAcc };
                        ({ E = #direntry{ name = Name }, { registered, Pid } }, Acc) ->
                      { Ino, NewAcc } = 
                        make_inode ({ registered, Name }, Pid, Acc),
                      { E#direntry{ stat = #stat{ st_ino = Ino } }, NewAcc };
                        (E = #direntry{}, Acc) ->
                      { E, Acc }
                    end,
                    State,
                    DirEntryList),
                      
  { #fuse_reply_direntrylist{ direntrylist = MappedList }, NewState };
readdir (_, 4, Size, Offset, _Fi, _, State) ->
  Items = [ allocated_areas, allocator, c_compiler_used, 
            check_io, compat_rel, creation, dist, dist_ctrl,
            driver_version, elib_malloc, fullsweep_after, 
            garbage_collection, global_heaps_size, heap_sizes,
            heap_type, info, kernel_poll, loaded, machine,
            modified_timing_level, multi_scheduling, 
            multi_scheduling_blockers, otp_release, process_count,
            process_limit, procs, scheduler_id, schedulers,
            smp_support, system_version, system_architecture,
            threads, thread_pool_size, trace_control_word, 
            version, wordsize ],

  DirEntryList = 
    take_while 
      (fun (R, { Total, Max }) -> 
       Cur = case R of { E, _ } -> fuserlsrv:dirent_size (E);
                       E -> fuserlsrv:dirent_size (E)
             end,
         if 
           Total + Cur =< Max ->
             { continue, { Total + Cur, Max } };
           true ->
             stop
         end
       end,
       { 0, Size },
       lists:nthtail 
         (Offset,
          [ #direntry{ name = ".", offset = 1, stat = ?DIRATTR (4) },
            #direntry{ name = "..", offset = 2, stat = ?DIRATTR (1) } ] ++
          [ { #direntry{ name = atom_to_list (Item),
                         offset = N + 2,
                         stat = #stat{ st_mode = ?S_IFREG bor 8#0444,
                                       st_size = StSize } },
              Item } ||
            { N, { Item, Info } } 
              <- index ([ { I, erlang:system_info (I) } || 
                            I <- Items,
                            is_valid_system_info (I) ]),
            StSize 
              <- [ erlang:iolist_size (io_lib:format ("~p.~n", [ Info ])) ] ])),

  { MappedList, NewState } = 
    lists:mapfoldl (fun ({ E = #direntry{ name = Name }, Item }, Acc) ->
                      { Ino, NewAcc } = 
                        make_inode ({ system_item, Name }, Item, Acc),
                      { E#direntry{ stat = #stat{ st_ino = Ino } }, NewAcc };
                        (E = #direntry{}, Acc) ->
                      { E, Acc }
                    end,
                    State,
                    DirEntryList),
                      
  { #fuse_reply_direntrylist{ direntrylist = MappedList }, NewState };
readdir (_, 5, Size, Offset, _Fi, _, State) ->
  Items = [ visible, hidden, connected, this, known ],

  DirEntryList = 
    take_while 
      (fun (R, { Total, Max }) -> 
         Cur = case R of { E, _ } -> fuserlsrv:dirent_size (E);
                          E -> fuserlsrv:dirent_size (E)
               end,
         if 
           Total + Cur =< Max ->
             { continue, { Total + Cur, Max } };
           true ->
             stop
         end
       end,
       { 0, Size },
       lists:nthtail 
         (Offset,
          [ #direntry{ name = ".", offset = 1, stat = ?DIRATTR (5) },
            #direntry{ name = "..", offset = 2, stat = ?DIRATTR (1) } ] ++
          [ { #direntry{ name = atom_to_list (Item), 
                         offset = N + 2, 
                         stat = ?DIRATTR (undefined) }, Item }
            || { N, Item } <- index (Items) ])),

  { MappedList, NewState } = 
    lists:mapfoldl (fun ({ E = #direntry{ name = Name }, Item }, Acc) ->
                      { Ino, NewAcc } = 
                        make_inode ({ nodes, Name }, Item, Acc),
                      { E#direntry{ stat = #stat{ st_ino = Ino } }, NewAcc };
                        (E = #direntry{}, Acc) ->
                      { E, Acc }
                    end,
                    State,
                    DirEntryList),
                      
  { #fuse_reply_direntrylist{ direntrylist = MappedList }, NewState };
readdir (_, 6, Size, Offset, _Fi, _, State) ->
  DirEntryList = 
    take_while 
      (fun (E, { Total, Max }) -> 
         Cur = fuserlsrv:dirent_size (E),
         if 
           Total + Cur =< Max ->
             { continue, { Total + Cur, Max } };
           true ->
             stop
         end
       end,
       { 0, Size },
       safe_nthtail
         (Offset,
          [ #direntry{ name = ".", offset = 1, stat = ?DIRATTR (6) },
            #direntry{ name = "..", offset = 2, stat = ?DIRATTR (1) } ] ++
          [ #direntry{ name = Var, 
                       offset = N + 2, 
                       stat = #stat{ st_mode = ?S_IFREG bor 8#0444,
                                     st_size = StSize } }
            || { N, { Var, Value } } <- index (env_vars ()),
               StSize 
                <- [ erlang:iolist_size (io_lib:format ("~p.~n", [ Value ])) ] ])),

  { MappedList, NewState } = 
    lists:mapfoldl (fun (E = #direntry{ name = Name }, Acc) ->
                      { Ino, NewAcc } = 
                        make_inode ({ env_var, Name }, void, Acc),
                      { E#direntry{ stat = #stat{ st_ino = Ino } }, NewAcc }
                    end,
                    State,
                    DirEntryList),
                      
  { #fuse_reply_direntrylist{ direntrylist = MappedList }, NewState };
readdir (_, X, Size, Offset, _Fi, _, State) ->
  case gb_trees:lookup (X, State#fuserlprocsrv.inodes) of
    { value, { env_var, _ } } ->
      { #fuse_reply_err{ err = enotdir }, State };
    { value, { nodes, Name } } ->
      read_nodes_directory (X, Name, Size, Offset, State);
    { value, { nodes_item, _ } } ->
      { #fuse_reply_err{ err = enotdir }, State };
    { value, { pid, Name } } ->
      read_pid_directory (X, Name, Size, Offset, State);
    { value, { port, Name } } ->
      read_port_directory (X, Name, Size, Offset, State);
    { value, { port_item, _ } } ->
      { #fuse_reply_err{ err = enotdir }, State };
    { value, { registered, _ } } ->
      { #fuse_reply_err{ err = enotdir }, State };
    { value, { system_item, _ } } ->
      { #fuse_reply_err{ err = enotdir }, State };
    none ->
      { #fuse_reply_err{ err = enotdir }, State }
  end.

readlink (_, X, _, State) ->
  case gb_trees:lookup (X, State#fuserlprocsrv.inodes) of
    { value, { registered, Name } } ->
      case gb_trees:lookup ({ registered, Name }, State#fuserlprocsrv.names) of
        { value, { _, Pid } } ->
          { #fuse_reply_readlink{ link = erlang:pid_to_list (Pid) }, State };
        _ ->
          { #fuse_reply_err{ err = enoent }, State }
      end;
    _ ->
      { #fuse_reply_err{ err = einval }, State }
  end.

%-=====================================================================-
%-                               Private                               -
%-=====================================================================-

env_var_attr (Name, State) ->
  { Ino, _ } = gb_trees:get ({ env_var, Name }, State#fuserlprocsrv.names),
  StSize = erlang:iolist_size (io_lib:format ("~p.~n", [ os:getenv (Name) ])),
  #stat{ st_ino = Ino, st_mode = ?S_IFREG bor 8#0444, st_size = StSize }.

env_vars () ->
  [ { Var, tl (Value) }
    || S <- os:getenv (),
       { Var, Value } <- [ lists:splitwith (fun (X) -> X =/= $= end, S) ] ].

index ([]) ->
  [];
index (L) ->
  lists:zip (lists:seq (1, length (L)), L).

is_valid_system_info (Item) ->
  try
    erlang:system_info (Item),
    true
  catch
    _ : _ ->
      false
  end.

lookup_nodes_item (Item, Node, State) ->
  case lists:member (Node, erlang:nodes (Item)) of
    true ->
      { Ino, NewState } = make_inode ({ nodes_item, { Item, Node } }, 
                                      void, 
                                      State),
      { #fuse_reply_entry{ 
          fuse_entry_param = 
            #fuse_entry_param{ ino = Ino,
                               generation = 1,  % (?)
                               attr_timeout_ms = 1000,
                               entry_timeout_ms = 1000,
                               attr = nodes_item_attr ({ Item, Node }, 
                                                       NewState) } },
        NewState };
    false ->
      { #fuse_reply_err{ err = enoent }, State }
  end.

lookup_pid_item (Name, Item, State) ->
  { Ino, NewState } = make_inode ({ pid_item, { Name, Item } }, void, State),
  { #fuse_reply_entry{ 
      fuse_entry_param = 
        #fuse_entry_param{ ino = Ino,
                           generation = 1,  % (?)
                           attr_timeout_ms = 1000,
                           entry_timeout_ms = 1000,
                           attr = pid_item_attr ({ Name, Item }, NewState) } },
    NewState }.

lookup_port_item (Name, Item, State) ->
  { Ino, NewState } = make_inode ({ port_item, { Name, Item } }, void, State),
  { #fuse_reply_entry{ 
      fuse_entry_param = 
        #fuse_entry_param{ ino = Ino,
                           generation = 1,  % (?)
                           attr_timeout_ms = 1000,
                           entry_timeout_ms = 1000,
                           attr = port_item_attr ({ Name, Item }, NewState) } },
    NewState }.

make_inode (Name, Extra, State) ->
  case gb_trees:lookup (Name, State#fuserlprocsrv.names) of
    { value, { Ino, Extra } } ->
      { Ino, State };
    none ->
      Inodes = State#fuserlprocsrv.inodes,
      { Max, _ } = gb_trees:largest (Inodes),
      NewInodes = gb_trees:insert (Max + 1, Name, Inodes),
      Names = State#fuserlprocsrv.names,
      NewNames = gb_trees:insert (Name, { Max + 1, Extra }, Names),
      { Max + 1, State#fuserlprocsrv{ inodes = NewInodes, names = NewNames } }
  end.

maybe_env_var (Name) ->
  os:getenv (Name) =/= false.

maybe_node_type (Name) ->
  case lists:member (Name, 
                     [ "visible", "hidden", "connected", "this", "known" ]) of
    true ->
      { node_type, list_to_atom (Name) };
    false ->
      false
  end.

maybe_pid (Name) ->
  case catch erlang:list_to_pid (Name) of
    Pid when is_pid (Pid) ->
      { pid, Pid };
    _ ->
      case erlang:whereis (list_to_atom (Name)) of
        Pid when is_pid (Pid) ->
          { registered, Pid };
        _ ->
          false
      end
  end.

nodes_item_attr ({ Item, Node }, State) ->
  { Ino, _ } = gb_trees:get ({ nodes_item, { Item, Node } }, 
                             State#fuserlprocsrv.names),

  #stat{ st_ino = Ino, st_mode = ?S_IFREG bor 8#0444, st_size = 0 }.

pid_item_attr ({ Name, Item }, State) ->
  { _, Pid } = gb_trees:get ({ pid, Name }, State#fuserlprocsrv.names),
  try
    case erlang:process_info (Pid, Item) of
      { Item, Info } ->
        { Ino, _ } = gb_trees:get ({ pid_item, { Name, Item } }, 
                                   State#fuserlprocsrv.names),
        StSize = erlang:iolist_size (io_lib:format ("~p.~n", [ Info ])),
        #stat{ st_ino = Ino, st_mode = ?S_IFREG bor 8#0444, st_size = StSize };
      [] when Item =:= registered_name ->
        #stat{ st_ino = 0, st_mode = ?S_IFREG bor 8#0444, st_size = 0 };
      undefined ->
        #stat{ st_ino = 0, st_mode = ?S_IFREG bor 8#0444, st_size = 0 }
    end
  catch
    _ : _ ->
      #stat{ st_ino = 0, st_mode = ?S_IFREG bor 8#0444, st_size = 0 }
  end.

port_item_attr ({ Name, Item }, State) ->
  { _, Port } = gb_trees:get ({ port, Name }, State#fuserlprocsrv.names),
   try
    case erlang:port_info (Port, Item) of
      { Item, Info } ->
        { Ino, _ } = gb_trees:get ({ port_item, { Name, Item } }, 
                                   State#fuserlprocsrv.names),
        StSize = erlang:iolist_size (io_lib:format ("~p.~n", [ Info ])),
        #stat{ st_ino = Ino, st_mode = ?S_IFREG bor 8#0444, st_size = StSize };
      undefined ->
        #stat{ st_ino = 0, st_mode = ?S_IFREG bor 8#0444, st_size = 0 }
    end
  catch
    _ : _ ->
      #stat{ st_ino = 0, st_mode = ?S_IFREG bor 8#0444, st_size = 0 }
  end.

read_env_var (Name, Size, Offset, State) ->
  IoList = io_lib:format ("~p.~n", [ os:getenv (Name) ]),
  Len = erlang:iolist_size (IoList),
  
  if
    Offset < Len ->
      if
        Offset + Size > Len ->
          Take = Len - Offset,
          <<_:Offset/binary, Data:Take/binary, _/binary>> = 
            erlang:iolist_to_binary (IoList);
        true ->
          <<_:Offset/binary, Data:Size/binary, _/binary>> = 
            erlang:iolist_to_binary (IoList)
      end;
    true ->
      Data = <<>>
  end,

  { #fuse_reply_buf{ buf = Data, size = erlang:size (Data) }, State }.

read_nodes_directory (Dot, Name, Size, Offset, State) ->
  { _, Item } = gb_trees:get ({ nodes, Name }, State#fuserlprocsrv.names),
  DirEntryList = 
    take_while 
      (fun (R, { Total, Max }) -> 
         Cur = case R of { E, _ } -> fuserlsrv:dirent_size (E);
                         E -> fuserlsrv:dirent_size (E)
               end,
         if 
           Total + Cur =< Max ->
             { continue, { Total + Cur, Max } };
           true ->
             stop
         end
       end,
       { 0, Size },
       safe_nthtail
         (Offset,
          [ #direntry{ name = ".", offset = 1, stat = ?DIRATTR (Dot) },
            #direntry{ name = "..", offset = 2, stat = ?DIRATTR (5) } ] ++
          [ { #direntry{ name = atom_to_list (Node),
                         offset = N + 2,
                         stat = #stat{ st_mode = ?S_IFREG bor 8#0444,
                                       st_size = 0 } },
              Node } ||
            { N, Node } <- index (erlang:nodes (Item)) ])),

  { MappedList, NewState } = 
    lists:mapfoldl (fun ({ E = #direntry{ stat = Stat }, Node }, Acc) ->
                      { Ino, NewAcc } = 
                        make_inode ({ nodes_item, { Item, Node } }, void, Acc),
                      { E#direntry{ stat = Stat#stat{ st_ino = Ino } },
                        NewAcc };
                        (E = #direntry{}, Acc) ->
                      { E, Acc }
                    end,
                    State,
                    DirEntryList),
  { #fuse_reply_direntrylist{ direntrylist = MappedList }, NewState }.

read_nodes_item (_, _, _, State) ->
  { #fuse_reply_buf{ buf = <<>>, size = 0 }, State }.

read_pid_directory (Dot, Name, Size, Offset, State) ->
  { _, Pid } = gb_trees:get ({ pid, Name }, State#fuserlprocsrv.names),
  DirEntryList = 
    take_while 
      (fun (R, { Total, Max }) -> 
         Cur = case R of { E, _ } -> fuserlsrv:dirent_size (E);
                         E -> fuserlsrv:dirent_size (E)
               end,
         if 
           Total + Cur =< Max ->
             { continue, { Total + Cur, Max } };
           true ->
             stop
         end
       end,
       { 0, Size },
       safe_nthtail
         (Offset,
          [ #direntry{ name = ".", offset = 1, stat = ?DIRATTR (Dot) },
            #direntry{ name = "..", offset = 2, stat = ?DIRATTR (3) } ] ++
          [ { #direntry{ name = atom_to_list (Item),
                         offset = N + 2,
                         stat = #stat{ st_mode = ?S_IFREG bor 8#0444,
                                       st_size = StSize } },
              Item } ||
            { N, { Item, Info } } 
              <- index (erlang:process_info (Pid)),
            StSize 
              <- [ erlang:iolist_size (io_lib:format ("~p.~n", [ Info ])) ] ])),

  { MappedList, NewState } = 
    lists:mapfoldl (fun ({ E = #direntry{ stat = Stat }, Item }, Acc) ->
                      { Ino, NewAcc } = 
                        make_inode ({ pid_item, { Name, Item } }, void, Acc),
                      { E#direntry{ stat = Stat#stat{ st_ino = Ino } },
                        NewAcc };
                        (E = #direntry{}, Acc) ->
                      { E, Acc }
                    end,
                    State,
                    DirEntryList),
  { #fuse_reply_direntrylist{ direntrylist = MappedList }, NewState }.

read_port_directory (Dot, Name, Size, Offset, State) ->
  { _, Port } = gb_trees:get ({ port, Name }, State#fuserlprocsrv.names),
  DirEntryList = 
    take_while 
      (fun (R, { Total, Max }) -> 
         Cur = case R of { E, _ } -> fuserlsrv:dirent_size (E);
                         E -> fuserlsrv:dirent_size (E)
               end,
         if 
           Total + Cur =< Max ->
             { continue, { Total + Cur, Max } };
           true ->
             stop
         end
       end,
       { 0, Size },
       safe_nthtail 
         (Offset,
          [ #direntry{ name = ".", offset = 1, stat = ?DIRATTR (Dot) },
            #direntry{ name = "..", offset = 2, stat = ?DIRATTR (2) } ] ++
          [ { #direntry{ name = atom_to_list (Item),
                         offset = N + 2,
                         stat = #stat{ st_mode = ?S_IFREG bor 8#0444,
                                       st_size = StSize } },
              Item } ||
            { N, { Item, Info } } 
              <- index (erlang:port_info (Port)),
            StSize 
              <- [ erlang:iolist_size (io_lib:format ("~p.~n", [ Info ])) ] ])),

  { MappedList, NewState } = 
    lists:mapfoldl (fun ({ E = #direntry{ stat = Stat }, Item }, Acc) ->
                      { Ino, NewAcc } = 
                        make_inode ({ port_item, { Name, Item } }, void, Acc),
                      { E#direntry{ stat = Stat#stat{ st_ino = Ino } },
                        NewAcc };
                        (E = #direntry{}, Acc) ->
                      { E, Acc }
                    end,
                    State,
                    DirEntryList),
  { #fuse_reply_direntrylist{ direntrylist = MappedList }, NewState }.

read_pid_item ({ Name, Item }, Size, Offset, State) ->
  { _, Pid } = gb_trees:get ({ pid, Name }, State#fuserlprocsrv.names),
  case erlang:process_info (Pid, Item) of
    { Item, Info } ->
      IoList = io_lib:format ("~p.~n", [ Info ]),
      Len = erlang:iolist_size (IoList),

      if
        Offset < Len ->
          if
            Offset + Size > Len ->
              Take = Len - Offset,
              <<_:Offset/binary, Data:Take/binary, _/binary>> = 
                erlang:iolist_to_binary (IoList);
            true ->
              <<_:Offset/binary, Data:Size/binary, _/binary>> = 
                erlang:iolist_to_binary (IoList)
          end;
        true ->
          Data = <<>>
      end,

      { #fuse_reply_buf{ buf = Data, size = erlang:size (Data) }, State };
    [] when Item =:= registered_name ->
      { #fuse_reply_buf{ buf = <<>>, size = 0 }, State };
    undefined ->
      { #fuse_reply_buf{ buf = <<>>, size = 0 }, State }
  end.

read_port_item ({ Name, Item }, Size, Offset, State) ->
  { _, Port } = gb_trees:get ({ port, Name }, State#fuserlprocsrv.names),
  case erlang:port_info (Port, Item) of
    { Item, Info } ->
      IoList = io_lib:format ("~p.~n", [ Info ]),
      Len = erlang:iolist_size (IoList),

      if
        Offset < Len ->
          if
            Offset + Size > Len ->
              Take = Len - Offset,
              <<_:Offset/binary, Data:Take/binary, _/binary>> = 
                erlang:iolist_to_binary (IoList);
            true ->
              <<_:Offset/binary, Data:Size/binary, _/binary>> = 
                erlang:iolist_to_binary (IoList)
          end;
        true ->
          Data = <<>>
      end,

      { #fuse_reply_buf{ buf = Data, size = erlang:size (Data) }, State };
    undefined ->
      { #fuse_reply_buf{ buf = <<>>, size = 0 }, State }
  end.

read_system_item (Name, Size, Offset, State) ->
  { _, Item } = gb_trees:get ({ system_item, Name }, State#fuserlprocsrv.names),
  try
    Info = erlang:system_info (Item),
    IoList = io_lib:format ("~p.~n", [ Info ]),
    Len = erlang:iolist_size (IoList),
    if
      Offset < Len ->
        if
          Offset + Size > Len ->
            Take = Len - Offset,
            <<_:Offset/binary, Data:Take/binary, _/binary>> = 
              erlang:iolist_to_binary (IoList);
          true ->
            <<_:Offset/binary, Data:Size/binary, _/binary>> = 
              erlang:iolist_to_binary (IoList)
        end;
      true ->
        Data = <<>>
    end,

    { #fuse_reply_buf{ buf = Data, size = erlang:size (Data) }, State }
  catch
    _ : _ ->
      { #fuse_reply_buf{ buf = <<>>, size = 0 }, State }
  end.

% This may look like a cheat, but there are some cases where the entity
% being indexed is changing (e.g., erlang:processes ()) and so 
% readdir can crash between two lookups.

safe_nthtail (_, []) -> 
  [];
safe_nthtail (N, L) when N =< 0 ->
  L;
safe_nthtail (N, L) ->
  safe_nthtail (N - 1, tl (L)).

system_item_attr (Name, State) ->
  { Ino, Item } = gb_trees:get ({ system_item, Name },
                                State#fuserlprocsrv.names),

  try
    Info = erlang:system_info (Item),
    StSize = erlang:iolist_size (io_lib:format ("~p.~n", [ Info ])),
    #stat{ st_ino = Ino, st_mode = ?S_IFREG bor 8#0444, st_size = StSize }
  catch
    _ : _ ->
      #stat{ st_ino = 0, st_mode = ?S_IFREG bor 8#0444, st_size = 0 }
  end.

take_while (_, _, []) -> 
  [];
take_while (F, Acc, [ H | T ]) ->
  case F (H, Acc) of
    { continue, NewAcc } ->
      [ H | take_while (F, NewAcc, T) ];
    stop ->
      []
  end.

-ifdef (EUNIT).

%-=====================================================================-
%-                                Tests                                -
%-=====================================================================-

test_proc (LinkedIn) ->
  F = fun (Dir) ->
        fun () ->
          ?assert (erlang:system_info (thread_pool_size) > 0), % avoid deadlock

          G = fun (F, T, N) when N > 0 -> 
                               case T () of true -> ok; 
                                            false -> 
                                              receive after 1000 -> ok end,
                                              F (F, T, N - 1)
                               end
              end,
          
          G (G, 
             fun () -> 
               { ok, Filenames } = file:list_dir (Dir),
               [ "environment", "nodes", "ports", "processes", "system" ] =:= lists:sort (Filenames) 
             end, 
             10),

          true
        end
      end,

  { setup,
    fun () ->
      ok = application:start (fuserl),
      { MegaSec, Sec, MicroSec } = erlang:now (),
      ok = application:load (fuserlproc),
      Dir = "mount.tmp." ++
            integer_to_list (MegaSec) ++ "." ++
            integer_to_list (Sec) ++ "." ++
            integer_to_list (MicroSec),

      application:set_env (fuserlproc, linked_in, LinkedIn),
      application:set_env (fuserlproc, mount_point, Dir),
      application:set_env (fuserlproc, mount_opts, ""),
      ok = application:start (fuserlproc),
      Dir
    end,
    fun (Dir) -> 
      application:stop (fuserlproc),
      application:unload (fuserlproc),
      application:stop (fuserl),
      application:unload (fuserl),
      H = fun (G) -> 
            case file:del_dir (Dir) of
              ok -> ok;
              { error, ebusy } -> receive after 1000 -> ok end, G (G)
            end
          end,

      ok = H (H)
    end,
    F }.

proc_pipe_test_ () ->
  test_proc (false).

proc_linkedin_test_ () ->
  test_proc (true).

-endif.
