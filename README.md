# The Great Rouclere!

Welcome, welcome, gentlefolk of all kinds! Welcome to the show of The Great Rouclere, in which we shall attempt to meet your expectations, all of your expectations, only your expectations, nothing more, nothing less!

All over mostly-standard HTTP!

Come, come! Watch the show of The Great Rouclere!

## Basics

The Great Rouclere is a programmable HTTP mock written in Common Lisp. It allows the programmer to set "if-then" expectations for incoming HTTP requests, letting the programmer specify responses in a declarative way. For instance:

```lisp
(with-magic-show (port)
  (expect (:post "/ping")
    (with :body "I would like one magic, please!")
    (with :header "Magic-Dust" "Imagination")
    (with :accept "application/magic-show")
    (answer (200)
      (with :body "That's perfect!!!")
      (with :content-type "text/magical")
      (with :header "Magic-Dust" "Prestidigitation")))
  ...)
```

This snippet starts a new *magic show* - a HTTP localhost server - and returns the port it's bound to as `port`.

Then, The Great Rouclere proceeds to expect a single `POST` request of `/ping`, one that contains headers `Accept: application/magic-show` and `Magic-Dust: Imagination`, as well as `I would like one magic, please!` in the body.

If such a request is made, The Great Rouclere fulfils the expectation by responding with a `HTTP 200` response, containing headers `Content-Type: text/magical` and `Magic-Dust: Prestidigitation`, as well as `That's perfect!!!` in the body.

If any other request comes in instead, The Great Rouclere responds with `HTTP 444` - a code that signifies a *surprise*, which is a request that did not match any expectation.

If an expectation is unmet by any incoming request, The Great Rouclere marks it as a *letdown*.

Both of these, signifying unwanted behaviour, are reported before the HTTP server is torn down at the end of the magic show.

The Great Rouclere can do more than this:
* call predicates and side-effecting code during expectation matching and response building, with a possibility to use variables from URL templates,
* create a magic show with multiple HTTP servers, with the possibility of setting expectations for each of them separately,
* create expectations that can be triggered multiple times, including permanent expectations that never time out,
* provide the current list of expectations and letdowns to user code as introspection,
* call user-provides code for all surprises and letdowns at the end of the magic show,
* signal a variety of errors for ensuring expectation consistency.

See [the test file](tests.lisp) for the capabilities of the current API.

## License

MIT. © Michał "phoe" Herda 2026.
