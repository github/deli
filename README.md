# deli

## What is Deli?

Deli is a tool for modeling and improving software systems that use queues.
After modeling a system with Deli, input is simulated, and you're given
statistics about performance and utilization.

For example, let's say you have a distributed system that sends several classes
of jobs through the same queue, with a fixed set of workers. You begin
experiences problems with slow jobs exhausting all of the workers, and
increasing response times for quick jobs. You would model this system using
Deli's Haskell DSL, and then be able to make predictions about proposed
solutions.


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

## Why shouldn't you use it?

* It may take longer to learn to use Deli than to fix your problem another way
* Uses Deli requires a small but existant knowledge of Haskell
* Using Deli doesn't obviate the need to understand basic statistics and the
  distribution of your input data
* Deli currently is light on documentation

---

```
stack build && stack exec deli-exe
```
