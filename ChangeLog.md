# Changelog for `localize`

## 0.5.0

* Fix processing Unicode grapheme clusters (user-perceived characters): they are now kept as a unit (w/o reversing their codepoints) and also case-flipped if applicable.
* Add a runtime cache for localized strings in the daemon mode for faster repetitive processing.

## 0.4.1

* Fix a rare bug when the daemon would stop processing any file events after some time. Apparently this was caused by two events for the same file in quick succession and trying to write the same file from two threads. This is fixed by making the event handling single-threaded.

## 0.4.0

* Add the `-d` option to start a daemon watching for changes in JSON files in given directories and automatically localizing them into other directories.

## 0.3.0

* Support the `%count%` placeholder.
* Preserve the position of groups separated with `|`.

## 0.2.0

* Support a subset of escaped characters in JSON strings.
* Support PHP-style (`$foo`) and React-style (`{{foo}}`) placeholders.
