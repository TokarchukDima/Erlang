{application, my_db,
  [{description, "My Database"},
    {vsn, "1.0.0"},
    {modules, [db_server, db_server_app, db_server_gen, db_server_obs]},
    {registered, [db_server, db_server_obs]},
    {applications, [kernel, stdlib]},
    {env, []},
    {mod, {db_server_app,[]}}]}.