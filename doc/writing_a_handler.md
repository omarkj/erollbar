# Writing a erollbar handler

Erollbar handlers are functions that parse arbitrary messages that pass through Erlang's
`error_logger` into Rollbar reports. The library comes with handlers for crash reports
and error messages, as well as handlers that ignore erollbar's own info reports as well
as the error messages sent through error_logger when a crash happens as those are already
reported via the crash report.

Writing a erollbar handler is simple but to do so the datastructure needs to be converted
into either a `trace()` or `message()`, the only two rollbar message types supported at this
moment.

Handlers are a function of arity 1 that return either `pass`, which will let the message
pass on to the next handler for parsing, `ignore` which will cause the message to be
dropped or `erollbar_message:msg()` which is a datastructure constructed with the helpers
in the `erollbar_message` module.

## Writing a simple handler to ignore all info_reports that have the tag `ignore_me`

```
Handler = fun ({info_report, _, {_, ignore_me, _}}) -> ignore;
              (_) -> pass
              end.
```

Handler can also be defined in a module as a "regular" function.

## Creating a Trace

A trace is a type of a message Rollbar.com understands. It has an exceptions and frames. Since
we are writing Erlang this will have to contain more than just exception, like exits etc.

The first thing to do is to create a new Trace message with the exception class (in our case
this could be the ExceptionExit).

```
Trace = erollbar_message:trace(badmatch),
```

A trace will contain Frames. Frames are stack frames.

```
Frame = erollbar_message:frame(bad_file),
```

A frame has to have a Filename. According to Erollbar this should be the full module name, but
at this time erollbar sets this to the module name only.

Further details about the stack trace can also be filled in, like `lineno`.

```
Frame1 = erollbar_message:lineno(10, Frame),
```

Once all the frames have been set, they can be added to the Trace

```
Trace1 = erollbar_message:frames([Frame1], Trace),
{ok, Trace1}.
```

## Using the new handler

When the new handler is ready, erollbar needs to be configured to run it on a new message,
handlers can be set with the `report_handlers` option when starting erollbar.

`erollbar_handlers` exports four handlers `filter_doubles`, `filter_erollbar`, `crash_report`
and `error_msg`. When configuring erollbar it's recommended that you at least keep `filter_erollbar`
in your list of handlers, as it prevents erollbar to submit it's own messages to Rollbar.com.

### To start erollbar with custom handlers

```
erollbar:start(access_token(), [{report_handlers, [fun erollbar_handlers:filter_erollbar/1,
                                                   Handler]}]).
```

Handlers are run in order.