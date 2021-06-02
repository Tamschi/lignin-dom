# lignin-dom Changelog

<!-- markdownlint-disable no-trailing-punctuation -->

## next

TODO: Date

* **Breaking:**
  * Upgraded `lignin` dependency from 0.0.3 to 0.1.0
    > to support fallible allocation/bump object initialisation downstream.
  * Increased minimum supported Rust version from 1.42.0 to 1.46.0
    > required because of `lignin` upgrade.
* Features:
  * `lignin-dom` is now feature-complete for the current version of `lignin`,
    > and as such should never panic as long as the implementation contract specified by `lignin` is observed.
* Revisions:
  * `lignin-dom` now avoids memory leaks when it fails to locate event bindings in the DOM and warns about the latter.
  * Duplicate `ReorderableFragment::dom_key`s now more often lead to panics, especially when `cfg!(debug_assertions) == true`.
    > This was disallowed before, but `lignin_dom` was more lenient in this regard than necessary.
  * Updated to newer version of `rust-template` project base, which comes with CI improvements and a newly added SECURITY.md file.

## 0.0.3

2021-01-03

* **Breaking:**
  * Upgraded `lignin` dependency from 0.0.2 to 0.0.3

## 0.0.2

2020-11-30

* **Breaking:**
  * Removed "remnants" feature (now always enabled)
  * Upgraded `lignin` dependency from 0.0.1 to 0.0.2
* Revisions:
  * Adjusted README

## 0.0.1

2020-10-02

Initial unstable release
