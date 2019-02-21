# Overview

Deli is a Haskell DSL which allows you to model a system, and then run
simulations to understand performance. Deli borrows from [queueing
theory](https://en.wikipedia.org/wiki/Queueing_theory), and allows us to model
and understand how work is performed with a limited set of resources. Deli can
be used to model everything from elevator scheduling, thread and disk
schedulers in operating systems, to how to design checkout lines at a grocery
store.

To model your system, Deli gives you a concurrency and message passing API
similar to the Go programming language, and allows you to model input into your
system either statistically, or from a production log (like a CSV file).

Once you have modeled your system and its inputs, you run Deli, which runs as
quickly as it can, simulating wall-clock time, and then returns with statistics
about how your system performed.

Before digging in further, let's start with why you *should not* use Deli.

## Why shouldn't you use Deli?

* It may take longer to learn to use Deli than to fix your problem another way
* Uses Deli requires a small but existent knowledge of the Haskell programming
  language
* Using Deli doesn't obviate the need to understand basic statistics and the
  distribution of your input data
* Deli currently is light on documentation, we hope that the community can assist here

## Why was this built?

Deli was built to explore improvements to GitHub's webhook infrastructure,
where the HTTP response time distribution can vary dramatically when a single
popular webhook target is unavailable.

## How does it work?

Deli is a Haskell library and DSL, implemented as a [discrete event
simulation](https://en.wikipedia.org/wiki/Discrete_event_simulation). It
presents a
[CSP](https://en.wikipedia.org/wiki/Communicating_sequential_processes)
concurrency API which is used to model systems.

## What next?

If you'd like to start using Deli, head over to our [user
guide](user-guide.md).
