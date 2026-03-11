# Changelog

## 0.2.3 (2026-02-11)
- hide output of empty lists and object by default (behind --show-empty)
- add 'contributors' command
- use pagureGroupRepos from pagure-0.2.1

## 0.2.2 (2024-05-06)
- new add group and groupinfo commands
- help: order commands with better grouping
- update to Haskell pagure-0.2.0

## 0.2.1 (2022-02-17)
- new commands: 'project', 'issue', 'userinfo'
- drop --url and use aeson-pretty
- use new pagure library
- 'user': new '--fork" option
- support aeson-2.0 and drop lens dependency
- 'branches' and 'username' now error if entity does not exist
- errors for non-existent API paths (eg unknown repo or user)

## 0.2 (2020-04-06)
- new commands: 'branches', 'issues', 'users', 'username', 'groups', 'git-url'
- 'list': new options --namespace, --username, --include-forks, --only-forks
- switch to http-conduit and default to lens instead of microlens for aeson
- -U is now the short option for --url, which displays the API url

## 0.1 (2019-12-11)
- initial release with list and user commands
