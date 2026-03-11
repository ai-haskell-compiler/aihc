# A Haskell library for the GitLab web API

This library lifts the GitLab REST API into Haskell. It supports
queries about and updates to:

* Branches
* Commits
* Groups
* Issues
* Jobs
* Members
* Merge requests
* Pipelines
* Projects
* Repositories
* Repository files
* Users
* Discussions
* Tags
* Todos
* Version
* Notes
* Boards

## gitlab-haskell API

The library parses JSON results into Haskell data types in the
`GitLab.Types` module, allowing you to work with statically typed
GitLab data with data types and functions that the library
provides. E.g.

    searchUser   :: Text -> GitLab (Maybe User)
    userProjects :: User -> ProjectSearchAttrs -> GitLab (Maybe [Project])

## Server-side GitLab file hooks

This library can also be used to implement rule based GitLab file
system hooks that, when deployed a GitLab server, react in real time
to GitLab events like project creation, new users, merge requests etc.

The rule based API for implementing file hooks is:

    receive :: [Rule] -> GitLab ()

    class (FromJSON a) => SystemHook a where
      match   :: String -> (a -> GitLab ()) -> Rule
      matchIf :: String -> (a -> GitLab Bool) -> (a -> GitLab ()) -> Rule

For more details about the file system hooks support, see post:
[GitLab automation with file hook rules](https://www.macs.hw.ac.uk/~rs46/posts/2020-06-06-gitlab-system-hooks.html).

This library has almost 100% coverage of the GitLab REST API. For the complete
`gitlab-haskell` API, see the [hackage
documentation](https://hackage.haskell.org/package/gitlab-haskell).

## Example

Run all GitLab actions with `runGitLab`: 

    runGitLab ::
       => GitLabServerConfig
       -> GitLab a
       -> IO a

For example the following project returns all GitLab projects for the
user "joe".

    myProjects <-
      runGitLab
        (defaultGitLabServer
           { url = "https://gitlab.example.com"
           , token = AuthMethodToken "my_token"} )
        (searchUser "joe" >>=  \usr -> userProjects (fromJust usr) defaultProjectSearchAttrs)

## Library use

It was initially developed to automate and support computer science
education. See our ICSE-SEET 2024 paper for the details: [_"Integrating Canvas
and GitLab to Enrich Learning
Processes"_](https://doi.org/10.1145/3639474.3640056).

An example of an application using this library is `gitlab-tools`,
which is a command line tool for bulk GitLab transactions [link](https://gitlab.com/robstewart57/gitlab-tools).

Unsurprisingly, this library is maintained on GitLab: [GitLab project](https://gitlab.com/robstewart57/gitlab-haskell).
