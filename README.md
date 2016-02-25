Assess
========

Assess provides additional support for testing Emacs packages.

It provides:
  - a set of predicates for comparing strings, buffers and file contents.
  - explainer functions for all predicates giving useful output
  - macros for creating many temporary buffers at once, and for restoring the
    buffer list.
  - methods for testing indentation, by comparision or "roundtripping".
  - methods for testing fontification.

Assess aims to be a stateless as possible, leaving Emacs unchanged whether
the tests succeed or fail, with respect to buffers, open files and so on; this
helps to keep tests independent from each other.

Documentation
-------------

Assess is fully documented using the `lentic-doc` documentation system.


Status
------

It is now a 0.1 release; for those parts that already exist I do not expected
to change the interface, but it could happen. It is ready for careful use,
therefore.


[![Build Status](https://travis-ci.org/phillord/assess.svg)](https://travis-ci.org/phillord/assess)
