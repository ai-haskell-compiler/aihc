{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import API.BoardsTests
import API.BranchesTests
import API.CommitsTests
import API.DiscussionsTests
import API.EventsTests
import API.GroupsTests
import API.IssuesTests
import API.JobsTests
import API.MembersTests
import API.MergeRequestsTests
import API.NotesTests
import API.PipelinesTests
import API.ProjectsTests
import API.RepositoriesTests
import API.RepositoryFilesTests
import API.TagsTests
import API.TodosTests
import API.UsersTests
import API.VersionTests
import SystemHookTests
import Test.Tasty

main :: IO ()
main = do
  defaultMain
    ( testGroup
        "gitlab-haskell"
        [ testGroup
            "gitlab system hook tests"
            systemHookTests,
          testGroup
            "api-boards"
            boardsTests,
          testGroup
            "api-branches"
            branchesTests,
          testGroup
            "api-commits"
            commitsTests,
          testGroup
            "api-discussions"
            discussionsTests,
          testGroup
            "api-events"
            eventsTests,
          testGroup
            "api-groups"
            groupsTests,
          testGroup
            "api-issues"
            issuesTests,
          testGroup
            "api-jobs"
            jobsTests,
          testGroup
            "api-members"
            membersTests,
          testGroup
            "api-merge-requests"
            mergeRequestsTests,
          testGroup
            "api-notes"
            notesTests,
          testGroup
            "api-pipelines"
            pipelinesTests,
          testGroup
            "api-projects"
            projectsTests,
          testGroup
            "api-repositories"
            repositoriesTests,
          testGroup
            "api-repository-files"
            repositoryFilesTests,
          testGroup
            "api-tags"
            tagsTests,
          testGroup
            "api-todos"
            todosTests,
          testGroup
            "api-users"
            usersTests,
          testGroup
            "api-version"
            versionTests
        ]
    )
