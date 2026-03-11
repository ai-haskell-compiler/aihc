# Version history for simple-cmd

## 0.2.7 (2022-06-20)
- allow building on Windows without unix (no sudo*)
- add 'newline' to output newline
- rename 'sudo_' to 'sudoLog' ('sudo_' no longer outputs log)
- export 'sudoInternal' helper
- provide 'cmdLog_' to be used instead of 'cmdLog'

## 0.2.6 (2022-05-18)
- timeIO: print the duration in hours and minutes, not just seconds

## 0.2.5 (2022-04-24)
- cmdN: quote arguments with show
- add timeIO: runs action and prints the time it took

## 0.2.4 (2022-03-27)
- error' and warning are now strict
- pipeBool: return False if either end fails
- add filesWithExtension and fileWithExtension
- initial basic testsuite

## 0.2.3 (2020-12-20)
- most commands now use removeTrailingNewline
- cmdFull: wrapper of readProcessWithExitCode
- cmdStderrToStdoutIn: like cmdStderrToStdout but reads a String
- pipe3 connects 3 commands

## 0.2.2 (2020-06-17)
- grep: no longer errors for no match
- add cmdStderrToStdout: redirects stderr to stdout
- add gitBool
- define gitBranch using "git rev-parse" (internal change)
- now export ifM and whenM from extra

## 0.2.1 (2019-12-12)
- add cmdTry_: only runs if available
- add ifM and whenM
- add needProgram

## 0.2.0 (2019-06-03)
- add warning command
- API change: sudo and sudo_
- add shellBool
- add pipe, pipe_, pipeBool, pipeFile_, pipe3
- quoteCmd uses showCommandForUser

## 0.1.4 (2019-04-08)
- export error'
- add cmdLog (deprecates cmdlog)

## 0.1.3.1 (2019-03-15)
- sudo: ignored for root or when no sudo installed

## 0.1.3 -- 2019-02-20
- gitDiffQuiet
- fix rwGitDir regexp
- use errorWithoutStackTrace

## 0.1.2 -- 2018-10-28
- grep
- improve haddock documentation

## 0.1.1 -- 2018-10-02
- cmdLines
- Git: grepGitConfig
- new Rpm: rpmspec

## 0.1.0.0  -- 2018-09-13

- Initial release, providing:
  cmd, cmd_, cmdBool, cmdMaybe, cmdStdIn, cmdlog, cmdN,
  cmdIgnoreErr, cmdQuiet, cmdSilent, cmdStdErr,
  egrep_, grep_, logMsg,
  removePrefix, removeStrictPrefix, removeSuffix,
  shell, shell_, sudo, (+-+)
- A few git commands

# Local Variables:
# mode: text
# End:
