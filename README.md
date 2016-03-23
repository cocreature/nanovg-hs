# NanoVG Haskell bindings

Currently only the GL3 backend is supported.

A large part of the example bundled with NanoVG is translated into
Haskell and bundled as `example00`.

Most of the bindings are just small wrappers around the raw C API. I’m
open to higher level abstractions, see
`textBreak[ines` in `NanoVG.hs`, but I don’t want to reinvent diagrams.
Instead I’d like to write a diagrams backend that uses these bindings once I find time.

Feel free to open issues if you have any ideas for improvements (or even better PRs :)).
