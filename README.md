# Erollbar

Error handler that posts crash reports to Rollbar.com.

## Usage

``` erlang
-type access_token() :: binary().
-type opt() :: {modules, [module()]}|
               {applications, [atom()]}|
               {environment, binary()}|
               {platform, binary()}|
               {batch_max, pos_integer()}|
               {time_max, ms()}|
               {endpoint, iolist()}|
               {info_fun, info_fun()|undefined}.
-type opts() :: [opt()]|[].
-spec start(access_token()) -> ok.
-spec start(access_token(), opts()) -> ok.
```

## Running the tests

``` bash
$ rebar ct skip_deps=true
```

## Todo

Add server block support, moar tests

## License

See `LICENSE`.
