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

Assess is fully
[documented](http://homepages.cs.ncl.ac.uk/phillip.lord/lentic/assess-doc.html).
Documentation is written and generating using the `lentic-doc` documentation
system. It is also possible to generate the documentation locally:

    M-x package-install lentic-server
    M-x lentic-server-browse


Status
------

The core of assess should now be considered stable and may be actively used.

Assess supports runs all of the Emacs-24 series, Emacs-25 and Emacs-26 (to
be). I will maintain support for older Emacs as far back as I am easily able
to compile or run older versions; currently this is Emacs-24.1.

Roadmap
-------

I plan to move this to core Emacs, as ert-assess. This will happen after
Emacs-25.1 release.

Release
-------

## Version 0.4

This release features the first feature added by an external contributor
(thanks to Damien Cassou). Assess now also supports the entire Emacs-24
series, after several requests; that this was possible was largely, if
indirectly, due to Nicolas Petton's seq.el supporting all these versions

### Features

- All of Emacs-24 series now supported.
- `assess-with-filesystem` enables creation of a temporary file hierarchy.

### Bug Fixes
 - `assess-with-preserved-buffer-list` now kills even file associated buffers
   at the end of the form.

## Version 0.3.2

Fix Version Number

## Version 0.3.1

Add test, fix keybinding

## Version 0.3

Add assess-robot.el

## Version 0.2

Add assess-call.el

## Version 0.1

First Release

[![Build Status](https://travis-ci.org/phillord/assess.svg)](https://travis-ci.org/phillord/assess)
