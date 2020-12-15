# deli

## What is Deli?

Deli is a performance modeling tool, allowing you to understand and experiment
with new designs at several orders of magnitude faster than wall-clock time.
Specifically, Deli is designed to help you understand how long it takes for
'jobs' or 'requests' to complete in your system. Deli borrows concepts and
terminology from [queuing
theory](https://en.wikipedia.org/wiki/Queueing_theory), and is implemented as a
Haskell DSL, exposing a [Go (language)](https://golang.org/) -like concurrency
and message-passing API.

Deli's documentation is divided into the following three sections, depending on
your interest.

## First time readers

If you're looking for an overview on Deli, and whether it may be appropriate
for your problem, head over to our [overview documentation](docs/overview.md).

## Using Deli

If you've decided you want to use Deli, or are already using it in a project,
then our [user guide](docs/user-guide.md) is a great resource.

## Contributing

If you'd like to contribute to Deli, start with our [contributing
documentation](docs/contributing.md).

## License

Deli is BSD3 licensed. More information is available in [LICENSE](LICENSE).
