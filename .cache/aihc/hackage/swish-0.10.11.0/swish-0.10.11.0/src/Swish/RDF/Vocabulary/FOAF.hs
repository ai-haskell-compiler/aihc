{-# LANGUAGE OverloadedStrings #-}

--------------------------------------------------------------------------------
--  See end of this file for licence information.
--------------------------------------------------------------------------------
-- |
--  Module      :  Swish.RDF.Vocabulary.FOAF
--  Copyright   :  (c) 2011 Douglas Burke
--  License     :  GPL V2
--
--  Maintainer  :  Douglas Burke
--  Stability   :  experimental
--  Portability :  OverloadedStrings
--
--  This module defines some commonly used vocabulary terms from the FOAF
--  vocabulary (<http://xmlns.com/foaf/spec/>).
--
--  Note that unlike some of the existing vocabularies in Swish, the FOAF
--  one matches the case and spelling of the RDF terms; so we
--  use 'foafbased_near'
--  rather than @foafBasedNear@. This is partly because some terms would
--  end up with the same Haskell label if a conversion to camel-case wer
--  used.
--
--------------------------------------------------------------------------------

module Swish.RDF.Vocabulary.FOAF
    ( 
      -- | The version used for this module is 
      -- \"FOAF Vocabulary Specification 0.98 Namespace Document 9 August 2010 - /Marco Polo Edition/\",
      -- <http://xmlns.com/foaf/spec/20100809.html>.
      namespaceFOAF
      
      -- * Classes
      , foafAgent
      , foafDocument
      , foafGroup
      , foafImage
      , foafLabelProperty
      , foafOnlineAccount
      , foafOnlineChatAccount
      , foafOnlineEcommerceAccount
      , foafOnlineGamingAccount
      , foafOrganization
      , foafPerson
      , foafPersonalProfileDocument
      , foafProject
        
      -- * Properties
      , foafaccount
      , foafaccountName
      , foafaccountServiceHomepage
      , foafage
      , foafaimChatID
      , foafbased_near
      , foafbirthday
      , foafcurrentProject
      , foafdepiction
      , foafdepicts
      , foafdnaChecksum
      , foaffamilyName
      , foaffamily_name
      , foaffirstName
      , foaffocus
      , foaffundedBy
      , foafgeekcode
      , foafgender
      , foafgivenName
      , foafgivenname
      , foafholdsAccount
      , foafhomepage
      , foaficqChatID
      , foafimg
      , foafinterest
      , foafisPrimaryTopicOf
      , foafjabberID
      , foafknows
      , foaflastName
      , foaflogo
      , foafmade
      , foafmaker
      , foafmbox
      , foafmbox_sha1sum
      , foafmember
      , foafmembershipClass
      , foafmsnChatID
      , foafmyersBriggs
      , foafname
      , foafnick
      , foafopenid
      , foafpage
      , foafpastProject
      , foafphone
      , foafplan
      , foafprimaryTopic
      , foafpublications
      , foafschoolHomepage
      , foafsha1
      , foafskypeID
      , foafstatus
      , foafsurname
      , foaftheme
      , foafthumbnail
      , foaftipjar
      , foaftitle
      , foaftopic
      , foaftopic_interest
      , foafweblog
      , foafworkInfoHomepage
      , foafworkplaceHomepage
      , foafyahooChatID
    )
where

import Swish.Namespace (Namespace, makeNamespace, ScopedName, makeNSScopedName)
import Swish.QName (LName)

import Data.Maybe (fromMaybe)
import Network.URI (URI, parseURI)

------------------------------------------------------------
--  Namespace
------------------------------------------------------------

foafURI :: URI
foafURI = fromMaybe (error "Internal error processing FOAF URI") $ parseURI "http://xmlns.com/foaf/0.1/"

-- | Maps @foaf@ to <http://xmlns.com/foaf/0.1/>.
namespaceFOAF :: Namespace
namespaceFOAF = makeNamespace (Just "foaf") foafURI

------------------------------------------------------------
--  Terms
------------------------------------------------------------

toF :: LName -> ScopedName
toF  = makeNSScopedName namespaceFOAF

-- Classes

-- | @foaf:Agent@ from <http://xmlns.com/foaf/spec/#term_Agent>.
foafAgent :: ScopedName
foafAgent = toF "Agent"

-- | @foaf:Document@ from <http://xmlns.com/foaf/spec/#term_Document>.
foafDocument :: ScopedName
foafDocument = toF "Document"

-- | @foaf:Group@ from <http://xmlns.com/foaf/spec/#term_Group>.
foafGroup :: ScopedName
foafGroup = toF "Group"

-- | @foaf:Image@ from <http://xmlns.com/foaf/spec/#term_Image>.
foafImage :: ScopedName
foafImage = toF "Image"

-- | @foaf:LabelProperty@ from <http://xmlns.com/foaf/spec/#term_LabelProperty>.
foafLabelProperty :: ScopedName
foafLabelProperty = toF "LabelProperty"

-- | @foaf:OnlineAccount@ from <http://xmlns.com/foaf/spec/#term_OnlineAccount>.
foafOnlineAccount :: ScopedName
foafOnlineAccount = toF "OnlineAccount"

-- | @foaf:OnlineChatAccount@ from <http://xmlns.com/foaf/spec/#term_OnlineChatAccount>.
foafOnlineChatAccount :: ScopedName
foafOnlineChatAccount = toF "OnlineChatAccount"

-- | @foaf:OnlineEcommerceAccount@ from <http://xmlns.com/foaf/spec/#term_OnlineEcommerceAccount>.
foafOnlineEcommerceAccount :: ScopedName
foafOnlineEcommerceAccount = toF "OnlineEcommerceAccount"

-- | @foaf:OnlineGamingAccount@ from <http://xmlns.com/foaf/spec/#term_OnlineGamingAccount>.
foafOnlineGamingAccount :: ScopedName
foafOnlineGamingAccount = toF "OnlineGamingAccount"

-- | @foaf:Organization@ from <http://xmlns.com/foaf/spec/#term_Organization>.
foafOrganization :: ScopedName
foafOrganization = toF "Organization"

-- | @foaf:Person@ from <http://xmlns.com/foaf/spec/#term_Person>.
foafPerson :: ScopedName
foafPerson = toF "Person"

-- | @foaf:PersonalProfileDocument@ from <http://xmlns.com/foaf/spec/#term_PersonalProfileDocument>.
foafPersonalProfileDocument :: ScopedName
foafPersonalProfileDocument = toF "PersonalProfileDocument"

-- | @foaf:Project@ from <http://xmlns.com/foaf/spec/#term_Project>.
foafProject :: ScopedName
foafProject = toF "Project"

-- Properties

-- | @foaf:account@ from <http://xmlns.com/foaf/spec/#term_account>. 
foafaccount :: ScopedName
foafaccount = toF "account"
  
-- | @foaf:accountName@ from <http://xmlns.com/foaf/spec/#term_accountName>. 
foafaccountName :: ScopedName
foafaccountName = toF "accountName"
  
-- | @foaf:accountServiceHomepage@ from <http://xmlns.com/foaf/spec/#term_accountServiceHomepage>. 
foafaccountServiceHomepage :: ScopedName
foafaccountServiceHomepage = toF "accountServiceHomepage"
  
-- | @foaf:age@ from <http://xmlns.com/foaf/spec/#term_age>. 
foafage :: ScopedName
foafage = toF "age"
  
-- | @foaf:aimChatID@ from <http://xmlns.com/foaf/spec/#term_aimChatID>. 
foafaimChatID :: ScopedName
foafaimChatID = toF "aimChatID"
  
-- | @foaf:based_near@ from <http://xmlns.com/foaf/spec/#term_based_near>. 
foafbased_near :: ScopedName
foafbased_near = toF "based_near"
  
-- | @foaf:birthday@ from <http://xmlns.com/foaf/spec/#term_birthday>. 
foafbirthday :: ScopedName
foafbirthday = toF "birthday"
  
-- | @foaf:currentProject@ from <http://xmlns.com/foaf/spec/#term_currentProject>. 
foafcurrentProject :: ScopedName
foafcurrentProject = toF "currentProject"
  
-- | @foaf:depiction@ from <http://xmlns.com/foaf/spec/#term_depiction>. 
foafdepiction :: ScopedName
foafdepiction = toF "depiction"
  
-- | @foaf:depicts@ from <http://xmlns.com/foaf/spec/#term_depicts>. 
foafdepicts :: ScopedName
foafdepicts = toF "depicts"
  
-- | @foaf:dnaChecksum@ from <http://xmlns.com/foaf/spec/#term_dnaChecksum>. 
foafdnaChecksum :: ScopedName
foafdnaChecksum = toF "dnaChecksum"
  
-- | @foaf:familyName@ from <http://xmlns.com/foaf/spec/#term_familyName>. 
foaffamilyName :: ScopedName
foaffamilyName = toF "familyName"
  
-- | @foaf:family_name@ from <http://xmlns.com/foaf/spec/#term_family_name>. 
foaffamily_name :: ScopedName
foaffamily_name = toF "family_name"
  
-- | @foaf:firstName@ from <http://xmlns.com/foaf/spec/#term_firstName>. 
foaffirstName :: ScopedName
foaffirstName = toF "firstName"
  
-- | @foaf:focus@ from <http://xmlns.com/foaf/spec/#term_focus>. 
foaffocus :: ScopedName
foaffocus = toF "focus"
  
-- | @foaf:fundedBy@ from <http://xmlns.com/foaf/spec/#term_fundedBy>. 
foaffundedBy :: ScopedName
foaffundedBy = toF "fundedBy"
  
-- | @foaf:geekcode@ from <http://xmlns.com/foaf/spec/#term_geekcode>. 
foafgeekcode :: ScopedName
foafgeekcode = toF "geekcode"
  
-- | @foaf:gender@ from <http://xmlns.com/foaf/spec/#term_gender>. 
foafgender :: ScopedName
foafgender = toF "gender"
  
-- | @foaf:givenName@ from <http://xmlns.com/foaf/spec/#term_givenName>. 
foafgivenName :: ScopedName
foafgivenName = toF "givenName"
  
-- | @foaf:givenname@ from <http://xmlns.com/foaf/spec/#term_givenname>. 
foafgivenname :: ScopedName
foafgivenname = toF "givenname"
  
-- | @foaf:holdsAccount@ from <http://xmlns.com/foaf/spec/#term_holdsAccount>. 
foafholdsAccount :: ScopedName
foafholdsAccount = toF "holdsAccount"
  
-- | @foaf:homepage@ from <http://xmlns.com/foaf/spec/#term_homepage>. 
foafhomepage :: ScopedName
foafhomepage = toF "homepage"
  
-- | @foaf:icqChatID@ from <http://xmlns.com/foaf/spec/#term_icqChatID>. 
foaficqChatID :: ScopedName
foaficqChatID = toF "icqChatID"
  
-- | @foaf:img@ from <http://xmlns.com/foaf/spec/#term_img>. 
foafimg :: ScopedName
foafimg = toF "img"
  
-- | @foaf:interest@ from <http://xmlns.com/foaf/spec/#term_interest>. 
foafinterest :: ScopedName
foafinterest = toF "interest"
  
-- | @foaf:isPrimaryTopicOf@ from <http://xmlns.com/foaf/spec/#term_isPrimaryTopicOf>. 
foafisPrimaryTopicOf :: ScopedName
foafisPrimaryTopicOf = toF "isPrimaryTopicOf"
  
-- | @foaf:jabberID@ from <http://xmlns.com/foaf/spec/#term_jabberID>. 
foafjabberID :: ScopedName
foafjabberID = toF "jabberID"
  
-- | @foaf:knows@ from <http://xmlns.com/foaf/spec/#term_knows>. 
foafknows :: ScopedName
foafknows = toF "knows"
  
-- | @foaf:lastName@ from <http://xmlns.com/foaf/spec/#term_lastName>. 
foaflastName :: ScopedName
foaflastName = toF "lastName"
  
-- | @foaf:logo@ from <http://xmlns.com/foaf/spec/#term_logo>. 
foaflogo :: ScopedName
foaflogo = toF "logo"
  
-- | @foaf:made@ from <http://xmlns.com/foaf/spec/#term_made>. 
foafmade :: ScopedName
foafmade = toF "made"
  
-- | @foaf:maker@ from <http://xmlns.com/foaf/spec/#term_maker>. 
foafmaker :: ScopedName
foafmaker = toF "maker"
  
-- | @foaf:mbox@ from <http://xmlns.com/foaf/spec/#term_mbox>. 
foafmbox :: ScopedName
foafmbox = toF "mbox"
  
-- | @foaf:mbox_sha1sum@ from <http://xmlns.com/foaf/spec/#term_mbox_sha1sum>. 
foafmbox_sha1sum :: ScopedName
foafmbox_sha1sum = toF "mbox_sha1sum"
  
-- | @foaf:member@ from <http://xmlns.com/foaf/spec/#term_member>. 
foafmember :: ScopedName
foafmember = toF "member"
  
-- | @foaf:membershipClass@ from <http://xmlns.com/foaf/spec/#term_membershipClass>. 
foafmembershipClass :: ScopedName
foafmembershipClass = toF "membershipClass"

-- | @foaf:msnChatID@ from <http://xmlns.com/foaf/spec/#term_msnChatID>.  
foafmsnChatID :: ScopedName
foafmsnChatID = toF "msnChatID"

-- | @foaf:myersBriggs@ from <http://xmlns.com/foaf/spec/#term_myersBriggs>. 
foafmyersBriggs :: ScopedName
foafmyersBriggs = toF "myersBriggs"

-- | @foaf:name@ from <http://xmlns.com/foaf/spec/#term_name>. 
foafname :: ScopedName
foafname = toF "name"

-- | @foaf:nick@ from <http://xmlns.com/foaf/spec/#term_nick>. 
foafnick :: ScopedName
foafnick = toF "nick"

-- | @foaf:openid@ from <http://xmlns.com/foaf/spec/#term_openid>. 
foafopenid :: ScopedName
foafopenid = toF "openid"
 
-- | @foaf:page@ from <http://xmlns.com/foaf/spec/#term_page>. 
foafpage :: ScopedName
foafpage = toF "page"
  
-- | @foaf:pastProject@ from <http://xmlns.com/foaf/spec/#term_pastProject>. 
foafpastProject :: ScopedName
foafpastProject = toF "pastProject"
  
-- | @foaf:phone@ from <http://xmlns.com/foaf/spec/#term_phone>. 
foafphone :: ScopedName
foafphone = toF "phone"
  
-- | @foaf:plan@ from <http://xmlns.com/foaf/spec/#term_plan>. 
foafplan :: ScopedName
foafplan = toF "plan"

-- | @foaf:primaryTopic@ from <http://xmlns.com/foaf/spec/#term_primaryTopic>.  
foafprimaryTopic :: ScopedName
foafprimaryTopic = toF "primaryTopic"

-- | @foaf:publications@ from <http://xmlns.com/foaf/spec/#term_publications>. 
foafpublications :: ScopedName
foafpublications = toF "publications"

-- | @foaf:schoolHomepage@ from <http://xmlns.com/foaf/spec/#term_schoolHomepage>. 
foafschoolHomepage :: ScopedName
foafschoolHomepage = toF "schoolHomepage"

-- | @foaf:sha1@ from <http://xmlns.com/foaf/spec/#term_sha1>. 
foafsha1 :: ScopedName
foafsha1 = toF "sha1"

-- | @foaf:skypeID@ from <http://xmlns.com/foaf/spec/#term_skypeID>. 
foafskypeID :: ScopedName
foafskypeID = toF "skypeID"
 
-- | @foaf:status@ from <http://xmlns.com/foaf/spec/#term_status>. 
foafstatus :: ScopedName
foafstatus = toF "status"
  
-- | @foaf:surname@ from <http://xmlns.com/foaf/spec/#term_surname>. 
foafsurname :: ScopedName
foafsurname = toF "surname"
  
-- | @foaf:theme@ from <http://xmlns.com/foaf/spec/#term_theme>. 
foaftheme :: ScopedName
foaftheme = toF "theme"
  
-- | @foaf:thumbnail@ from <http://xmlns.com/foaf/spec/#term_thumbnail>. 
foafthumbnail :: ScopedName
foafthumbnail = toF "thumbnail"

-- | @foaf:tipjar@ from <http://xmlns.com/foaf/spec/#term_tipjar>.  
foaftipjar :: ScopedName
foaftipjar = toF "tipjar"

-- | @foaf:title@ from <http://xmlns.com/foaf/spec/#term_title>. 
foaftitle :: ScopedName
foaftitle = toF "title"

-- | @foaf:topic@ from <http://xmlns.com/foaf/spec/#term_topic>. 
foaftopic :: ScopedName
foaftopic = toF "topic"
 
-- | @foaf:topic_interest@ from <http://xmlns.com/foaf/spec/#term_topic_interest>. 
foaftopic_interest :: ScopedName
foaftopic_interest = toF "topic_interest"
  
-- | @foaf:weblog@ from <http://xmlns.com/foaf/spec/#term_weblog>. 
foafweblog :: ScopedName
foafweblog = toF "weblog"
  
-- | @foaf:workInfoHomepage@ from <http://xmlns.com/foaf/spec/#term_workInfoHomepage>. 
foafworkInfoHomepage :: ScopedName
foafworkInfoHomepage = toF "workInfoHomepage"
  
-- | @foaf:workplaceHomepage@ from <http://xmlns.com/foaf/spec/#term_workplaceHomepage>. 
foafworkplaceHomepage :: ScopedName
foafworkplaceHomepage = toF "workplaceHomepage"
  
-- | @foaf:yahooChatID@ from <http://xmlns.com/foaf/spec/#term_yahooChatID>. 
foafyahooChatID :: ScopedName
foafyahooChatID = toF "yahooChatID"
  
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
