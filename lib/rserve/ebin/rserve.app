%% This is the application resource file (.app file) for the rserve_server,
%% application.
{application, rserve, 
  [
   {description, "Erlang interface to Rserve- the binary R server."},
   {vsn, "0.1.0"},
   {modules, [ 
     rserve_server,
     rserve_sup
   ]},
   {registered,[rserve_sup]},
   {applications, [kernel, stdlib]},
   {env, []}
  ]
}.
