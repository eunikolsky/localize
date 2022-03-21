# `localize`

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

* Install the compiled executable with `stack install localizehs:localize`.

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

To use it, place the curson inside a string within `"` and press `@s`.

## Technical details

The program is written in [Haskell](https://www.haskell.org/), which is an excellent choice for text processing. Specifically, it has multiple parser combinator libraries and I'm using [`megaparsec`](https://markkarpov.com/tutorial/megaparsec.html) to parse the tokens correctly. I initially tried to do manual parsing in swift, but parsing of even one kind of placeholders quickly got complicated. This task becomes much easier with a proper parser combinator library.
