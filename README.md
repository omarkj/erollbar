# Erollbar

[![Build Status](https://travis-ci.org/omarkj/erollbar.svg?branch=master)](https://travis-ci.org/omarkj/erollbar)

Error handler that posts crash reports to Rollbar.com.

## Usage

``` erlang
-type access_token() :: binary().
-type opt() :: {modules, [module()]}|
               {environment, binary()}|
               {platform, binary()}|
               {batch_max, pos_integer()}|
               {time_max, ms()}|
               {endpoint, binary()}|
               {host, binary()}|
               {root, binary()}|
               {branch, binary()}|
               {sha, binary()}|
               send_args.
-type opts() :: [opt()]|[].
-spec start(access_token()) -> ok.
-spec start(access_token(), opts()) -> ok.
-spec stop() -> ok.
```

## Running the tests

``` bash
$ rebar get-deps compile
$ ct_run -dir test/ -logdir logs -pa ebin -pa deps/*/ebin
```

## License

See `LICENSE`.
