{-# LANGUAGE OverloadedStrings #-}

--------------------------------------------------------------------------------
--  See end of this file for licence information.
--------------------------------------------------------------------------------
-- |
--  Module      :  Swish.RDF.Vocabulary.SIOC
--  Copyright   :  (c) 2011 Douglas Burke
--  License     :  GPL V2
--
--  Maintainer  :  Douglas Burke
--  Stability   :  experimental
--  Portability :  OverloadedStrings
--
--  This module defines some commonly used vocabulary terms from the SIOC
--  project (<http://sioc-project.org/>).
--
--------------------------------------------------------------------------------

module Swish.RDF.Vocabulary.SIOC
    ( 
      -- | The version used for this module is Revison 1.35 of the
      -- \"SIOC Core Ontology Specification\", dated 25 March 2010,
      -- <http://rdfs.org/sioc/spec/>.
      namespaceSIOC
      
      -- * Classes
      , siocCommunity
      , siocContainer
      , siocForum
      , siocItem
      , siocPost
      , siocRole
      , siocSite
      , siocSpace
      , siocThread
      , siocUserAccount
      , siocUsergroup
        
      -- * Properties
      , siocabout
      , siocaccount_of
      , siocaddressed_to
      , siocadministrator_of
      , siocattachment
      , siocavatar
      , sioccontainer_of
      , sioccontent
      , sioccreator_of
      , siocearlier_version
      , siocemail
      , siocemail_sha1
      , siocembeds_knowledge
      , siocfeed
      , siocfollows
      , siocfunction_of
      , siochas_administrator
      , siochas_container
      , siochas_creator
      , siochas_discussion
      , siochas_function
      , siochas_host
      , siochas_member
      , siochas_moderator
      , siochas_modifier
      , siochas_owner
      , siochas_parent
      , siochas_reply
      , siochas_scope
      , siochas_space
      , siochas_subscriber
      , siochas_usergroup
      , siochost_of
      , siocid
      , siocip_address
      , sioclast_activity_date
      , sioclast_item_date
      , sioclast_reply_date
      , sioclater_version
      , sioclatest_version
      , sioclink
      , sioclinks_to
      , siocmember_of
      , siocmoderator_of
      , siocmodifier_of
      , siocname
      , siocnext_by_date
      , siocnext_version
      , siocnote
      , siocnum_authors
      , siocnum_items
      , siocnum_replies
      , siocnum_threads
      , siocnum_views
      , siocowner_of
      , siocparent_of
      , siocprevious_by_date
      , siocprevious_version
      , siocrelated_to
      , siocreply_of
      , siocscope_of
      , siocsibling
      , siocspace_of
      , siocsubscriber_of
      , sioctopic
      , siocusergroup_of 
    )
where

import Swish.Namespace (Namespace, makeNamespace, ScopedName, makeNSScopedName)
import Swish.QName (LName)

import Data.Maybe (fromMaybe)
import Network.URI (URI, parseURI)

------------------------------------------------------------
--  Namespace
------------------------------------------------------------

siocURI :: URI
siocURI = fromMaybe (error "Internal error processing SIOC URI") $ parseURI "http://rdfs.org/sioc/ns#"

-- | Maps @sioc@ to <http://rdfs.org/sioc/ns#>.
namespaceSIOC :: Namespace
namespaceSIOC = makeNamespace (Just "sioc") siocURI

------------------------------------------------------------
--  Terms
------------------------------------------------------------

toS :: LName -> ScopedName
toS  = makeNSScopedName namespaceSIOC

-- Classes

-- | @sioc:Community@ from <http://rdfs.org/sioc/spec/#term_Community>. 
siocCommunity :: ScopedName
siocCommunity = toS "Community"

-- | @sioc:Container@ from <http://rdfs.org/sioc/spec/#term_Container>. 
siocContainer :: ScopedName
siocContainer = toS "Container"

-- | @sioc:Forum@ from <http://rdfs.org/sioc/spec/#term_Forum>. 
siocForum :: ScopedName
siocForum = toS "Forum"

-- | @sioc:Item@ from <http://rdfs.org/sioc/spec/#term_Item>. 
siocItem :: ScopedName
siocItem = toS "Item"

-- | @sioc:Post@ from <http://rdfs.org/sioc/spec/#term_Post>. 
siocPost :: ScopedName
siocPost = toS "Post"

-- | @sioc:Role@ from <http://rdfs.org/sioc/spec/#term_Role>. 
siocRole :: ScopedName
siocRole = toS "Role"

-- | @sioc:Site@ from <http://rdfs.org/sioc/spec/#term_Site>. 
siocSite :: ScopedName
siocSite = toS "Site"

-- | @sioc:Space@ from <http://rdfs.org/sioc/spec/#term_Space>. 
siocSpace :: ScopedName
siocSpace = toS "Space"

-- | @sioc:Thread@ from <http://rdfs.org/sioc/spec/#term_Thread>. 
siocThread :: ScopedName
siocThread = toS "Thread"

-- | @sioc:UserAccount@ from <http://rdfs.org/sioc/spec/#term_UserAccount>. 
siocUserAccount :: ScopedName
siocUserAccount = toS "UserAccount"

-- | @sioc:Usergroup@ from <http://rdfs.org/sioc/spec/#term_Usergroup>. 
siocUsergroup :: ScopedName
siocUsergroup = toS "Usergroup"

-- Properties

-- | @sioc:about@ from <http://rdfs.org/sioc/spec/#term_about>. 
siocabout :: ScopedName
siocabout = toS "about"

-- | @sioc:account_of@ from <http://rdfs.org/sioc/spec/#term_account_of>. 
siocaccount_of :: ScopedName
siocaccount_of = toS "account_of"

-- | @sioc:addressed_to@ from <http://rdfs.org/sioc/spec/#term_addressed_to>. 
siocaddressed_to :: ScopedName
siocaddressed_to = toS "addressed_to"

-- | @sioc:administrator_of@ from <http://rdfs.org/sioc/spec/#term_administrator_of>. 
siocadministrator_of :: ScopedName
siocadministrator_of = toS "administrator_of"

-- | @sioc:attachment@ from <http://rdfs.org/sioc/spec/#term_attachment>. 
siocattachment :: ScopedName
siocattachment = toS "attachment"

-- | @sioc:avatar@ from <http://rdfs.org/sioc/spec/#term_avatar>. 
siocavatar :: ScopedName
siocavatar = toS "avatar"

-- | @sioc:container_of@ from <http://rdfs.org/sioc/spec/#term_container_of>. 
sioccontainer_of :: ScopedName
sioccontainer_of = toS "container_of"

-- | @sioc:content@ from <http://rdfs.org/sioc/spec/#term_content>. 
sioccontent :: ScopedName
sioccontent = toS "content"

-- | @sioc:creator_of@ from <http://rdfs.org/sioc/spec/#term_creator_of>. 
sioccreator_of :: ScopedName
sioccreator_of = toS "creator_of"

-- | @sioc:earlier_version@ from <http://rdfs.org/sioc/spec/#term_earlier_version>. 
siocearlier_version :: ScopedName
siocearlier_version = toS "earlier_version"

-- | @sioc:email@ from <http://rdfs.org/sioc/spec/#term_email>. 
siocemail :: ScopedName
siocemail = toS "email"

-- | @sioc:email_sha1@ from <http://rdfs.org/sioc/spec/#term_email_sha1>. 
siocemail_sha1 :: ScopedName
siocemail_sha1 = toS "email_sha1"

-- | @sioc:embeds_knowledge@ from <http://rdfs.org/sioc/spec/#term_embeds_knowledge>. 
siocembeds_knowledge :: ScopedName
siocembeds_knowledge = toS "embeds_knowledge"

-- | @sioc:feed@ from <http://rdfs.org/sioc/spec/#term_feed>. 
siocfeed :: ScopedName
siocfeed = toS "feed"

-- | @sioc:follows@ from <http://rdfs.org/sioc/spec/#term_follows>. 
siocfollows :: ScopedName
siocfollows = toS "follows"

-- | @sioc:function_of@ from <http://rdfs.org/sioc/spec/#term_function_of>. 
siocfunction_of :: ScopedName
siocfunction_of = toS "function_of"

-- | @sioc:has_administrator@ from <http://rdfs.org/sioc/spec/#term_has_administrator>. 
siochas_administrator :: ScopedName
siochas_administrator = toS "has_administrator"

-- | @sioc:has_container@ from <http://rdfs.org/sioc/spec/#term_has_container>. 
siochas_container :: ScopedName
siochas_container = toS "has_container"

-- | @sioc:has_creator@ from <http://rdfs.org/sioc/spec/#term_has_creator>. 
siochas_creator :: ScopedName
siochas_creator = toS "has_creator"

-- | @sioc:has_discussion@ from <http://rdfs.org/sioc/spec/#term_has_discussion>. 
siochas_discussion :: ScopedName
siochas_discussion = toS "has_discussion"

-- | @sioc:has_function@ from <http://rdfs.org/sioc/spec/#term_has_function>. 
siochas_function :: ScopedName
siochas_function = toS "has_function"

-- | @sioc:has_host@ from <http://rdfs.org/sioc/spec/#term_has_host>. 
siochas_host :: ScopedName
siochas_host = toS "has_host"

-- | @sioc:has_member@ from <http://rdfs.org/sioc/spec/#term_has_member>. 
siochas_member :: ScopedName
siochas_member = toS "has_member"

-- | @sioc:has_moderator@ from <http://rdfs.org/sioc/spec/#term_has_moderator>. 
siochas_moderator :: ScopedName
siochas_moderator = toS "has_moderator"

-- | @sioc:has_modifier@ from <http://rdfs.org/sioc/spec/#term_has_modifier>. 
siochas_modifier :: ScopedName
siochas_modifier = toS "has_modifier"

-- | @sioc:has_owner@ from <http://rdfs.org/sioc/spec/#term_has_owner>. 
siochas_owner :: ScopedName
siochas_owner = toS "has_owner"

-- | @sioc:has_parent@ from <http://rdfs.org/sioc/spec/#term_has_parent>. 
siochas_parent :: ScopedName
siochas_parent = toS "has_parent"

-- | @sioc:has_reply@ from <http://rdfs.org/sioc/spec/#term_has_reply>. 
siochas_reply :: ScopedName
siochas_reply = toS "has_reply"

-- | @sioc:has_scope@ from <http://rdfs.org/sioc/spec/#term_has_scope>. 
siochas_scope :: ScopedName
siochas_scope = toS "has_scope"

-- | @sioc:has_space@ from <http://rdfs.org/sioc/spec/#term_has_space>. 
siochas_space :: ScopedName
siochas_space = toS "has_space"

-- | @sioc:has_subscriber@ from <http://rdfs.org/sioc/spec/#term_has_subscriber>. 
siochas_subscriber :: ScopedName
siochas_subscriber = toS "has_subscriber"

-- | @sioc:has_usergroup@ from <http://rdfs.org/sioc/spec/#term_has_usergroup>. 
siochas_usergroup :: ScopedName
siochas_usergroup = toS "has_usergroup"

-- | @sioc:host_of@ from <http://rdfs.org/sioc/spec/#term_host_of>. 
siochost_of :: ScopedName
siochost_of = toS "host_of"

-- | @sioc:id@ from <http://rdfs.org/sioc/spec/#term_id>. 
siocid :: ScopedName
siocid = toS "id"

-- | @sioc:ip_address@ from <http://rdfs.org/sioc/spec/#term_ip_address>. 
siocip_address :: ScopedName
siocip_address = toS "ip_address"

-- | @sioc:last_activity_date@ from <http://rdfs.org/sioc/spec/#term_last_activity_date>. 
sioclast_activity_date :: ScopedName
sioclast_activity_date = toS "last_activity_date"

-- | @sioc:last_item_date@ from <http://rdfs.org/sioc/spec/#term_last_item_date>. 
sioclast_item_date :: ScopedName
sioclast_item_date = toS "last_item_date"

-- | @sioc:last_reply_date@ from <http://rdfs.org/sioc/spec/#term_last_reply_date>. 
sioclast_reply_date :: ScopedName
sioclast_reply_date = toS "last_reply_date"

-- | @sioc:later_version@ from <http://rdfs.org/sioc/spec/#term_later_version>. 
sioclater_version :: ScopedName
sioclater_version = toS "later_version"

-- | @sioc:latest_version@ from <http://rdfs.org/sioc/spec/#term_latest_version>. 
sioclatest_version :: ScopedName
sioclatest_version = toS "latest_version"

-- | @sioc:link@ from <http://rdfs.org/sioc/spec/#term_link>. 
sioclink :: ScopedName
sioclink = toS "link"

-- | @sioc:links_to@ from <http://rdfs.org/sioc/spec/#term_links_to>. 
sioclinks_to :: ScopedName
sioclinks_to = toS "links_to"

-- | @sioc:member_of@ from <http://rdfs.org/sioc/spec/#term_member_of>. 
siocmember_of :: ScopedName
siocmember_of = toS "member_of"

-- | @sioc:moderator_of@ from <http://rdfs.org/sioc/spec/#term_moderator_of>. 
siocmoderator_of :: ScopedName
siocmoderator_of = toS "moderator_of"

-- | @sioc:modifier_of@ from <http://rdfs.org/sioc/spec/#term_modifier_of>. 
siocmodifier_of :: ScopedName
siocmodifier_of = toS "modifier_of"

-- | @sioc:name@ from <http://rdfs.org/sioc/spec/#term_name>. 
siocname :: ScopedName
siocname = toS "name"

-- | @sioc:next_by_date@ from <http://rdfs.org/sioc/spec/#term_next_by_date>. 
siocnext_by_date :: ScopedName
siocnext_by_date = toS "next_by_date"

-- | @sioc:next_version@ from <http://rdfs.org/sioc/spec/#term_next_version>. 
siocnext_version :: ScopedName
siocnext_version = toS "next_version"

-- | @sioc:note@ from <http://rdfs.org/sioc/spec/#term_note>. 
siocnote :: ScopedName
siocnote = toS "note"

-- | @sioc:num_authors@ from <http://rdfs.org/sioc/spec/#term_num_authors>. 
siocnum_authors :: ScopedName
siocnum_authors = toS "num_authors"

-- | @sioc:num_items@ from <http://rdfs.org/sioc/spec/#term_num_items>. 
siocnum_items :: ScopedName
siocnum_items = toS "num_items"

-- | @sioc:num_replies@ from <http://rdfs.org/sioc/spec/#term_num_replies>. 
siocnum_replies :: ScopedName
siocnum_replies = toS "num_replies"

-- | @sioc:num_threads@ from <http://rdfs.org/sioc/spec/#term_num_threads>. 
siocnum_threads :: ScopedName
siocnum_threads = toS "num_threads"

-- | @sioc:num_views@ from <http://rdfs.org/sioc/spec/#term_num_views>. 
siocnum_views :: ScopedName
siocnum_views = toS "num_views"

-- | @sioc:owner_of@ from <http://rdfs.org/sioc/spec/#term_owner_of>. 
siocowner_of :: ScopedName
siocowner_of = toS "owner_of"

-- | @sioc:parent_of@ from <http://rdfs.org/sioc/spec/#term_parent_of>. 
siocparent_of :: ScopedName
siocparent_of = toS "parent_of"

-- | @sioc:previous_by_date@ from <http://rdfs.org/sioc/spec/#term_previous_by_date>. 
siocprevious_by_date :: ScopedName
siocprevious_by_date = toS "previous_by_date"

-- | @sioc:previous_version@ from <http://rdfs.org/sioc/spec/#term_previous_version>. 
siocprevious_version :: ScopedName
siocprevious_version = toS "previous_version"

-- | @sioc:related_to@ from <http://rdfs.org/sioc/spec/#term_related_to>. 
siocrelated_to :: ScopedName
siocrelated_to = toS "related_to"

-- | @sioc:reply_of@ from <http://rdfs.org/sioc/spec/#term_reply_of>. 
siocreply_of :: ScopedName
siocreply_of = toS "reply_of"

-- | @sioc:scope_of@ from <http://rdfs.org/sioc/spec/#term_scope_of>. 
siocscope_of :: ScopedName
siocscope_of = toS "scope_of"

-- | @sioc:sibling@ from <http://rdfs.org/sioc/spec/#term_sibling>. 
siocsibling :: ScopedName
siocsibling = toS "sibling"

-- | @sioc:space_of@ from <http://rdfs.org/sioc/spec/#term_space_of>. 
siocspace_of :: ScopedName
siocspace_of = toS "space_of"

-- | @sioc:subscriber_of@ from <http://rdfs.org/sioc/spec/#term_subscriber_of>. 
siocsubscriber_of :: ScopedName
siocsubscriber_of = toS "subscriber_of"

-- | @sioc:topic@ from <http://rdfs.org/sioc/spec/#term_topic>. 
sioctopic :: ScopedName
sioctopic = toS "topic"

-- | @sioc:usergroup_of@ from <http://rdfs.org/sioc/spec/#term_usergroup_of>. 
siocusergroup_of :: ScopedName
siocusergroup_of = toS "usergroup_of"

--------------------------------------------------------------------------------
--
--  Copyright (c) 2011 Douglas Burke
--  All rights reserved.
--
--  This file is part of Swish.
--
--  Swish is free software; you can redistribute it and/or modify
--  it under the terms of the GNU General Public License as published by
--  the Free Software Foundation; either version 2 of the License, or
--  (at your option) any later version.
--
--  Swish is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU General Public License for more details.
--
--  You should have received a copy of the GNU General Public License
--  along with Swish; if not, write to:
--    The Free Software Foundation, Inc.,
--    59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
--
--------------------------------------------------------------------------------
