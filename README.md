# fit_parser

fit_parser is an Erlang library for parsing FIT files, the kind of files your activity devices (Garmin, Polar, etc) generates after recording an activity, workout, etc.

This is still a WIP.

## Requirements

Erlang 19
Rebar3

## Installation

```bash
brew install erlang

brew install rebar3

git clone

rebar3 compile
```

## Usage

To parse:

```erlang
FilePath = "some path to your .FIT file"

% records is a map.
{ok, records} = fit_parser:parse(FilePath)
```

For developement testing, you can use `rebar3 shell` in your terminal to open up a shell with the library and dependencies already loaded. From there you can interact with the libarary. See `Usage` above.

## Contributing

Pull requests are welcome. For major changes, please open an issue first to discuss what you would like to change.

Please make sure to update tests as appropriate.

## License

[MIT](https://choosealicense.com/licenses/mit/)

## TODO

- [ ] Implement unit tests.
- [ ] Provide a clear interface for interacting with the parsed file. In other words make it easier to extract attributes of the messages in a file.
- [ ] When extracting values return a struct that contains the value of the attribute you want and any meta data ( like units, etc) with it.
- [ ] Improve the readme with examples and explanations.
- [ ] Code cleanup.
