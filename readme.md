Description
===========

I can only admit this toy project is a little geeky...

Anyway, if you have ever dreamt of playing EDIFACT injection files right from Emacs, this is for you!

This module allows you to define injection files in a Test Tool Server fashion. (Not exactly the same syntax, though. We are here in a Lisp world).

No documentation... Just check the file 'example.el'.
It gives a pretty complete overview of the whole thing.

It is probably better if you know 'some' elisp. You don't have to, though.

Installing
==========

You'll need emacs 24.x.
(Could also work with emacs 23.x but it has not been tested.)

Copy 'ttemacs.el' in your load-path and you're done for the install.

Then, you probably want to check 'example.el' to have an example to start with:

 * start a Test Tool Server in mode echo (the scenario is completely empty) on 127.0.0.1:40000 in ERPLv2

 * eval the buffer of 'example.el' in Emacs (M-x eval-buffer)

The output should come in a new buffer.

Test?
=====

No unit tests... :-(

Want to contribute?
===================

Feedbacks, comments and bug fixes are more than welcome.