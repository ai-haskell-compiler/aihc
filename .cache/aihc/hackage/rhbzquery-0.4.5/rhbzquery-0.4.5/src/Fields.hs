-- SPDX-License-Identifier: GPL-2.0-or-later

module Fields (
  allBzFields,
  BzFields(..),
  argToFields,
  argToSimpleField,
  fieldAliases
  )
where

import Data.Maybe
import Data.List.Extra as L
import Data.Version
import Numeric.Natural

import Common
import ParseArg (ArgType(..), Operator(..), ProductVersion(..), showOp,
                 statusList)

data BzFields = BzProduct
              | BzVersion
              | BzComponent
              -- FIXME should be NonEmpty String
              | BzParameter String
              | BzMeta Char Natural
  deriving Eq

instance Show BzFields where
  show BzProduct = "product"
  show BzVersion = "version"
  show BzComponent = "component"
  show (BzParameter f) = f
  show (BzMeta c n) = c: show n

argToSimpleField :: ArgType -> [(BzFields,String)]
argToSimpleField (ArgProdVer prodver) =
  productVersionQuery prodver
argToSimpleField (ArgParameter p Equals v) =
  [(BzParameter (mapField p), v)]
argToSimpleField (ArgParameter p _ v) =
  error' $ "Only '" ++ p ++ "=" ++ v ++ "'allowed here"
argToSimpleField (ArgComponent c) =
  [(BzComponent,c)]
-- FIXME or error for "all"?
argToSimpleField _ = []

argToFields :: Natural -> ArgType -> (Natural,[(BzFields,String)])
argToFields i arg =
  case arg of
    ArgProdVer prodver -> (i,productVersionQuery prodver)
    ArgStatusAll -> (i,[])
    ArgStatusBefore st -> (i, [(BzParameter "bug_status", s) | s <- takeWhile (/= st) statusList])
    ArgStatusAfter st -> (i, [(BzParameter "bug_status", s) | s <- (tail . dropWhile (/= st)) statusList])
    ArgParameter p op v ->
      let param = mapField p
          val = if p == "sst"
                then "sst_" ++ v
                else v
          oper = if p == "content"
                 then case op of
                        Equals -> "equals"
                        Substring -> "matches"
                        NotSubstring -> "notmatches"
                        _ -> error' "'content' only supports matches and notmatches"
                 else showOp op
      in if '.' `elem` param || op /= Equals
         then (i+1,[(BzMeta 'f' i, param)
                   ,(BzMeta 'o' i, oper)
                   ,(BzMeta 'v' i, val)])
         else (i,[(BzParameter param, val)])
    ArgParameterEmpty p e ->
      let param = mapField p
      in (i+1,[(BzMeta 'f' i, param)
              ,(BzMeta 'o' i, if e then "isempty" else "isnotempty")])
    ArgComponent c -> (i,[(BzComponent,c)])

productVersionQuery :: ProductVersion -> [(BzFields,String)]
productVersionQuery (Fedora Nothing) = [(BzProduct, "Fedora")]
productVersionQuery Rawhide = [(BzProduct, "Fedora")
                              ,(BzVersion, "rawhide")]
productVersionQuery (Fedora (Just n)) = [(BzProduct, "Fedora")
                                        ,(BzVersion, show n)]
productVersionQuery (EPEL Nothing) = [(BzProduct, "Fedora EPEL")]
productVersionQuery (EPEL (Just n)) = [(BzProduct, "Fedora EPEL")
                                      ,(BzVersion, "epel" ++ show n)]
productVersionQuery (RHEL ver) =
  case versionBranch ver of
    [] -> error' "Can't search RHEL without version"
    [major] -> [(BzProduct, "Red Hat Enterprise Linux " ++ show major)]
    (major:_) ->  [(BzProduct, "Red Hat Enterprise Linux " ++ show major)
                  ,(BzVersion, showVersion ver)]

fieldAliases :: [(String,String)]
fieldAliases =
  [ ("arch", "rep_platform")
  , ("platform", "rep_platform")
  , ("flag", "flagtypes.name")
  , ("flags", "flagtypes.name")
  , ("itm", "cf_internal_target_milestone")
  , ("itr", "cf_internal_target_release")
  , ("sst", "agile_team.name")
  , ("status", "bug_status")
  , ("summary", "short_desc")
  ]

mapField :: String -> String
mapField f =
  let longname =
        let norm = L.replace "-" "_" f
        in fromMaybe norm $ lookup norm fieldAliases
  in if longname `elem` ("format":allBzFields)
     then longname
     else if "cf_" ++ longname `elem` allBzFields
          then "cf_" ++ longname
          else error' $ "unknown field: " ++ show f

-- https://bugzilla.redhat.com/query.cgi?query_format=advanced
-- https://bugzilla.redhat.com/rest/field/bug
-- or: bugzilla query --json -b bug#
allBzFields :: [String]
allBzFields =
  ["agile_pool.name",
   "agile_team.name",
   "alias",
   "assigned_to",
   "assigned_to_realname",
   "attach_data.thedata",
   "attachments.description",
   "attachments.filename",
   "attachments.isobsolete",
   "attachments.ispatch",
   "attachments.isprivate",
   "attachments.mimetype",
   "attachments.submitter",
   "blocked",
   "bug_agile_pool.pool_id",
   "bug_agile_pool.pool_order",
   "bug_file_loc",
   "bug_group",
   "bug_id",
   "bug_severity",
   "bug_status",
   "cc",
   "cclist_accessible",
   "cf_approved_release",
   "cf_atomic",
   "cf_build_id",
   "cf_business_market_problem",
   "cf_category",
   "cf_clone_of",
   "cf_cloudforms_team",
   "cf_compliance_control_group",
   "cf_compliance_level",
   "cf_conditional_nak",
   "cf_crm",
   "cf_cust_facing",
   "cf_deadline",
   "cf_deadline_type",
   "cf_devdoctest",
   "cf_devel_whiteboard",
   "cf_doc_type",
   "cf_docs_score",
   "cf_documentation_action",
   "cf_environment",
   "cf_epm_cdp",
   "cf_epm_phd",
   "cf_epm_prf_state",
   "cf_epm_pri",
   "cf_epm_ptl",
   "cf_epm_put",
   "cf_final_deadline",
   "cf_fixed_in",
   "cf_internal_target_milestone",
   "cf_internal_target_release",
   "cf_internal_whiteboard",
   "cf_last_closed",
   "cf_mount_type",
   "cf_ovirt_team",
   "cf_pgm_internal",
   "cf_pm_score",
   "cf_qa_whiteboard",
   "cf_qe_conditional_nak",
   "cf_regression_status",
   "cf_release_notes",
   "cf_srtnotes",
   "cf_story_points",
   "cf_target_upstream_version",
   "cf_type",
   "cf_verified_branch",
   "cf_zstream_target_release",
   "classification",
   "comment_tag",
   "commenter",
   "component",
   "content",
   "creation_ts",
   "days_elapsed",
   "deadline",
   "delta_ts",
   "dependent_products",
   "dependson",
   "docs_contact",
   "docs_contact_realname",
   "estimated_time",
   "everconfirmed",
   "ext_bz_bug_map.ext_bz_bug_id",
   "ext_bz_bug_map.ext_status",
   "external_bugzilla.description",
   "external_bugzilla.url",
   "extra_components",
   "extra_versions",
   "flagtypes.name",
   "keywords",
   "last_visit_ts",
   "longdesc",
   "longdescs.count",
   "longdescs.isprivate",
   "op_sys",
   "owner_idle_time",
   "percentage_complete",
   "priority",
   "product",
   "qa_contact",
   "qa_contact_realname",
   "remaining_time",
   "rep_platform",
   "reporter",
   "reporter_accessible",
   "reporter_realname",
   "requestees.login_name",
   "resolution",
   "rh_rule",
   "rh_sub_components",
   "see_also",
   "setters.login_name",
   "short_desc",
   "status_whiteboard",
   "tag",
   "target_milestone",
   "target_release",
   "version",
   "view",
   "votes",
   "work_time"]
