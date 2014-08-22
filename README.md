# Erollbar

[![Build Status](https://travis-ci.org/omarkj/erollbar.svg?branch=master)](https://travis-ci.org/omarkj/erollbar)

Error handler that posts reports to Rollbar.com.

## Usage

Erollbar will by default submit crash reports and error messages to Rollbar.com, other messages
are ignored. You can easily extend it by writing your own handler which erollbar will run. How to
write a handler is documented in [doc/writing_a_handler.md](doc/writing_a_handler.md).

### Starting it without customization

```
erollbar:start(access_token()).
```

Where `access_token()` is a `write` access token to your Rollbar.com account.

### Starting it with customization

You can customize Erollbar's behaviour by passing in options which are documented below.

```
erollbar:start(access_token(), [option()]).
```

The possible options are

* `environment`: The environment to which the messages are related. This could be `production`, `staging`
  etc. Defaults to `default`;
* `platform`: The platform from where the message originates. Defaults to `beam`;
* `batch_max`: The maximum number of messages erollbar should wait before submitting a report to Rollbar.com.
  Defaults to 10;
* `time_max`: The maximum number of milliseconds erollbar should wait before submitting a report to Rollbar.com.
  No default (no timer);
* `endpoint`: Submit reports to another API endpoint. Defaults to `https://api.rollbar.com/api/1`;
* `host`: Host of the node where the message originates. Defaults to the servers' hostname;
* `root`: Path of the application code root. No default;
* `branch`: Currently running branch. No default;
* `report_handlers`: List of handlers to run messages through. Defaults to handlers for crash reports and error messages;
* `send_args`: If this is set arguments are not scrubbed from frames.

## Running the tests

``` bash
$ rebar get-deps compile
$ ct_run -dir test/ -logdir logs -pa ebin -pa deps/*/ebin
```

## License

See `LICENSE`.
