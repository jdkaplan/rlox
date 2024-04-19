# rlox

A Rust implementation of the Lox bytecode VM from [Crafting Interpreters]

[Crafting Interpreters]: https://craftinginterpreters.com

This passes the test suite from the book repo! But wouldn't be surprised if I
introduced some bugs somehow 😅

## Why is it so `unsafe`?

Almost every interesting line in this interpreter is wrapped in `unsafe`
because it's a function-by-function port from my clox code.

I tried to make some things safer, but I don't (yet?) have the knowledge/skills
I need to safe-ify the whole thing. Although maybe it makes sense that the
garbage collector ~has no shoes~ can't be made safe?
