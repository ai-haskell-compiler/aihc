Changelog
=========

Version 0.2.4.0
---------------

*December 15, 2023*

<https://github.com/mstksg/conduino/releases/tag/v0.2.4.0>

*   Remove the "morally incorrect" `MonadTrans` instance for `ZipSource` and
    `ZipSink`, and replace them with `liftZipSource` and `liftZipSink`.  This
    also adds compatibility with *transformers-0.6*.

Version 0.2.3.0
---------------

*March 25, 2022*

<https://github.com/mstksg/conduino/releases/tag/v0.2.3.0>

*   Add `sourceHandleLinesText` for `Text` output
*   `sourceHandleLines` now continues through blank lines, but `stdinLines`
    retains the same behavior
*   `passthrough` pipe manipulation
*   More efficient `runExceptP`, `runCatchP`
*   More explicit inlining
*   `yield` is strict by default.  Use `yieldLazy` for original lazy behavior.

Version 0.2.2.0
---------------

*January 7, 2020*

<https://github.com/mstksg/conduino/releases/tag/v0.2.2.0>

*   Added `feedbackEither` for more flexibility on top of `feedback`
*   Some documentation cleanup

Version 0.2.1.0
---------------

*January 7, 2020*

<https://github.com/mstksg/conduino/releases/tag/v0.2.1.0>

*   `hoistPipe` exported from *Data.Conduit*
*   A handful of pipe primitive combinators added to *Data.Conduit*, including:
    *   `feedbackPipe`
    *   `zipPipe`
    *   `&|` / `fuseBoth`
    *   `|.` / `fuseUpstream`
    *   `fuseBothMaybe`
*   Some pipe runners added to *Data.Conduit*, including:
    *   `squeezePipe` / `squeezePipeEither`
    *   `feedPipe` / `feedPipeEither`
*   *Data.Conduit.Lift* module, for working with monad transformers
*   `iterM` added to *Data.Conduit.Combinators*

Version 0.2.0.0
---------------

*October 30, 2019*

<https://github.com/mstksg/conduino/releases/tag/v0.2.0.0>

*   Initial release

Version 0.1.0.0
---------------

(Accidental incomplete release made by mistake)

