# `localize`

[![Github CI](https://github.com/eunikolsky/localize/workflows/CI/badge.svg)](https://github.com/eunikolsky/localize/actions)

## Purpose

`localize` helps with "fake localization". That is if your program uses English and you need to support other languages (say, German), but you don't have translated strings immediately available, you need a way to see which strings are not localized yet.

The fake localization approach is to reverse the string and flip the case of the characters, thus the result stands out visually, yet is still readable English. For example:

> "Hello, world!" => "!DLROW ,OLLEh"

### Nuances

This is easily done in `vim` with a macro, however the string should not be reversed in a simple way. There are atomic tokens that should be preserved as is, those are:

* escaped characters in JSON strings: `\"`, `\n`, `\t`, etc.; see <https://www.json.org/json-en.html>;
* various placeholders:
  * `{{foo_bar}}`;
  * `$foo_bar`;
  * `%count%` — only `count` for now.

This requires a more advanced program, which is IMHO more convenient to write in a better language than vimscript. The result is `localize` here.

## Building

[Haskell `stack`](https://docs.haskellstack.org/en/stable/README/) is used to manage the project.

* Build the project with `stack build`.

* Install the compiled executable with `stack install localize:localize`.

The program is installed into `~/.local/bin/` by default, so make sure it's added to your `$PATH`. Verify it with:

```bash
$ localize --version
0.3.0.0
```

### Testing

Run the tests with `stack test`. Unit testing is done with `tasty` and `HUnit`.

## Usage

The program has the simplest possible interface: it reads the entire `stdin` and prints the result to `stdout`. Sample usage:

```bash
$ echo -n 'One {{foo}} case|Multiple $foo %count% cases|Else' | localize
ESAC {{foo}} ENo|SESAC %count% $foo ELPITLUm|ESLe

$ echo -n 'ёHello\\nworld \\r\\"$xyz\\" ёё {{foo_BAR}} ❓🚜 й ❄' | localize
❄ Й 🚜❓ {{foo_BAR}} ЁЁ \"$xyz\"\r DLROW\nOLLEhЁ
```

### vim

A sample `vim` macro `s` to localize a json string (within quotes `"`):

```vim
let @s='vi""0y:let @0=system("localize", @0)vi""0pn'
```

(There is a `CR` character between `@0)` and `vi""` in the command above, not `LF`, which may be incorrectly rendered by a newline in some software. You can type it with [`<C-V><C-M>` in `vim`](https://stackoverflow.com/a/1585463), it looks like `^M`.)

To use it, place the cursor inside a string within `"` and press `@s`.

### Daemon mode

`localize` has a daemon mode when it watches for changes in JSON files in the given directories, automatically localizes all string values and writes the output JSON file into a corresponding directory. All the found JSON files in those directories are also localized at startup. The output JSON files are pretty-printed and object keys are sorted — the same style as when piping through [`jq --sort-keys .`](https://stedolan.github.io/jq/manual/).

The directories to watch are specified with a config file; you can use [`config.json.sample`](config.json.sample) as a sample config:

```json
{
  "watchDirs": {
    "lang/en": "lang/fake",
    "src/locales/en": "src/locales/fake"
  }
}
```

Here the `lang/en` and `src/locales/en` directories are watched (non-recursively) and a localized JSON file from from `lang/en/` is saved into `lang/fake/` with the original name, the same for `src/locales/en/` => `src/locales/fake/`. Note that relative paths are resolved relative to the current directory. If the source file changes in a way that wouldn't change the already localized output file (e.g., only formatting was updated), then the output file isn't rewritten.

Localization of files is done sequentially for now because it's simpler to implement (no need to keep track if another thread is localizing the same file at the moment). This is fine for occasionally changing, medium-sized files.

To start this mode, use the `-d` option:

```bash
$ localize -d config.json

# or run in the background:
$ localize -d config.json &
```

True to the [Unix philosophy](https://en.wikipedia.org/wiki/Unix_philosophy) ("Don't clutter output with extraneous information"), the program doesn't print anything to `stdout` when everything goes smoothly. If there is any error (for example, a failure to parse a json file), then it's printed to `stderr`.

## Known issues

* Unicode grapheme clusters aren't processed correctly; the constituent codepoints are reversed instead of staying as a single cluster:

    ```bash
    $ echo -n '❄️  á' | localize
    A  ️❄
    $ echo -n '❄️  á' | xxd
    00000000: e29d 84ef b88f 2020 61cc 81              ......  a..
    $ echo -n '❄️  á' | localize | xxd
    00000000: cc81 4120 20ef b88f e29d 84              ..A  ......
    ```

## Technical details

The program is written in [Haskell](https://www.haskell.org/), which is an excellent choice for text processing. Specifically, it has multiple parser combinator libraries and I'm using [`megaparsec`](https://markkarpov.com/tutorial/megaparsec.html) to parse the tokens correctly. I initially tried to do manual parsing in swift, but parsing of even one kind of placeholders quickly got complicated. This task becomes much easier with a proper parser combinator library.
