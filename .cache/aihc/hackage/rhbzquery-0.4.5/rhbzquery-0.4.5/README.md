# rhbzquery

A cli tool for querying bugzilla.redhat.com.

The tool outputs the bugzilla query url,
and if xdg-open is available will try to open the url
(unless --dryrun is given).

rhbzquery is distributed under the GPL license version 2 or later.

## Usage examples

`rhbzquery f41 xyz` : Fedora 41 bugs for package xyz

`rhbzquery closed rawhide xyz` : closed rawhide bugs for package xyz

`rhbzquery --mine` : your open bugs (gets userid from `~/.bugzillarc`)

`rhbzquery rhel9.4 bash` : RHEL 9.4 bash bugs

`rhbzquery "Package Review" reporter_realname="Your Name"` : open package reviews you reported

`rhbzquery --mine all flag~fedora-review+` : all open and closed approved reviews you reviewed

`rhbzquery component~bugzilla summary~bugzilla` : open bugs with component and summary containing "bugzilla"

`rhbzquery component~python summary~something` : open bugs with component including "python" and summary containing "something"

`rhbzquery --file f40 xyz` : file a bug against the xyz package in F40

`rhbzquery --query rhel8 ...`: open an advanced bugzilla search for RHEL 8

`rhbzquery xyz '<modified'`: xyz bugs not yet in MODIFIED or ON_QA, etc.

### Help
Help describes the arguments:
`$ rhbzquery --help`

```
Bugzilla query tool

Usage: rhbzquery [--version] [-n|--dryrun] [-m|--mine] [-s|--server SERVER]
                 [(-l|--list-fields) | (-o|--list-operators) | (-f|--file) |
                   (-q|--query) | (-r|--reverse) | (-w|--api)]
                 [[COMPONENT|STATUS|PRODUCTVERSION|FIELD=VALUE|FIELDopVALUE]...]


  Tool for generating bugzilla queries

  STATUS = {new,assigned,post,modified,on_qa,verified,release_pending,closed,
            all (open and closed),'<STATE','>STATE'}
  PRODUCTVERSION = {rawhide,fedora,fXY,epel,epelX,rhel8,rhel7,rhelX.Z}
  op = search operator (eg '~' for substring: "summary~akeyword")

  See https://github.com/juhp/rhbzquery#readme for examples

Available options:
  -h,--help                Show this help text
  --version                Show version
  -n,--dryrun              Do not open url
  -m,--mine                My bugs
  -s,--server SERVER       Bugzilla server [default: bugzilla.redhat.com]
  -l,--list-fields         List query FIELDs
  -o,--list-operators      List op search operator types
  -f,--file                File a bug
  -q,--query               Open advanced query page
  -r,--reverse             Convert url query to args
  -w,--api                 Web API query
```

`rhbzquery --list-fields` lists the many fields (not all well supported yet, eg timestamps)

`$ rhbzquery --list-operators`

```
         '=' : equals (is equal to)
        '!=' : notequals (is not equal to)
         '~' : substring (contains the string)
        '!~' : notsubstring (does not contain the string)
        '=~' : regexp (matches regular expression)
       '!=~' : notregexp (does not match regular expression)
    '~case~' : casesubstring (contains the string (exact case))
     '~any~' : anywordssubstr (contains any of the strings)
     '~all~' : allwordssubstr (contains all of the strings)
      '~no~' : nowordssubstr (contains none of the strings)
'~anyexact=' : anyexact (is equal to any of the strings)
'~anywords~' : anywords (contains any of the words)
'~allwords~' : allwords (contains all of the words)
 '~nowords~' : nowords (contains none of the words)
   '~empty~' : isempty (is empty)
'~notempty~' : isnotempty (is not empty)
    '~noop~' : noop (ignore (comment out query field))

content~ uses matches
content!~ uses notmatches
```
lists the search operator types

## Installation
Run `stack install` or `cabal install`.

Binary packages for Fedora are available from <https://copr.fedorainfracloud.org/coprs/petersen/rhbzquery/>.

## Requests and feedback
Feature requests, bug reports and contributions are welcome.

Please open an issue at <https://github.com/juhp/rhbzquery>.
