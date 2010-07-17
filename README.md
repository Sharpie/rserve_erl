# Rserve: Erlang

This is an Erlang application that handles communication with the
Rserve- the binary R server. 

This package is currently a prototype- error handling is non-existant,
only one Rserve command is implemented and the (ab)use of logging is
pretty pathetic.  However, the following commands should work:

    rserve_server:start_link( 6311 ). % Connect to Rserve on port 6311
    rserve_server:eval( "1+1" ). % Execute the R command 1 + 1 
    "[1] 2"

    rserve_server:stop().

This application can be compiled using the [Sinan][2] build tool and
installed with the [Faxien][3] package manager:

    sinan build
    sinan dist
    faxien ir

There may also be some whining by the build tools if you are not using
Erlang R14A.

  [1]: http://www.rforge.net/Rserve/
  [2]: http://github.com/erlware/sinan
  [3]: http://github.com/erlware/faxien
