Description
===========

Have ever dreamt of injecting Interactive EDIFACT (IEDI) messages right from Emacs?
Here is a way to do that.

This module allows you to define injection files using very little elisp.

No documentation... Just check the file 'example.el'.
It gives a pretty complete overview of the whole thing.

It is probably better if you know elisp and Edifact.
No need to know much, though.

The only supported transport protocol is ERPLv2.

Installing
==========

You'll need emacs 24.x.
(Could also work with emacs 23.x but it has not been tested.)

Copy 'ttemacs.el' in your load-path and you're done for the install.

Then, you probably want to check 'example.el' to have an example to start with:

 * start a Test Tool Server in echo mode (or whatever you have at hand that just echo what is receives on a TCP connection) on 127.0.0.1:40000 (in ERPLv2).

 * eval the buffer of 'example.el' in Emacs (M-x eval-buffer)

The output should come in a new buffer.

Test?
=====

No unit tests... :-(

Want to contribute?
===================

Feedbacks, comments and bug fixes are more than welcome.
