%% This is the application resource file (.app file) for the rserve_server,
%% application.
{application, rserve_server, 
  [{description, "Your Desc HERE"},
   {vsn, "0.1.0"},
   {modules, [ ]},
   {registered,[ ]},
   {applications, [kernel, stdlib]},
   {mod, {rserve_server_app,[]}},
   {start_phases, []}]}.

