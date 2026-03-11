# Revision history for simple-prompt

## 0.2.4 (2025-12-28)
- yesNo[Default]: only append "?" if missing

## 0.2.3 (2024-06-13)
- getGenericPrompt: adapt prompt suffix for final newline or colon
- clearedInput no longer prints short duration for any buffered input

## 0.2.2 (2023-10-27)
- add promptKeyPress

## 0.2.1 (2023-08-09)
- do not trim spaces for yesNo and yesNoDefault
- Internal: timedInput renamed to clearedInput

## 0.2.0 (2023-05-28)
- new API using haskeline and MonadIO
- prompt ignores buffered stdin lines if it returns in milliseconds
- promptEnter replaces prompt_
- new functions: promptEnter, promptInitial, promptNonEmpty, promptPassword
- yesNo and yesNoDefault replace yesno
- internal haskeline functions in SimplePrompt.Internal: including
  getPrompt*, runPrompt, untilInput, mapInput, nonEmptyInput, timedInput

## 0.1.0 (2023-04-02)
- initial release with prompt, prompt_, yesno functions
