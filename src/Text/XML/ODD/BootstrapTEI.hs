-- ODD-XML - An ODD Parser for Haskell
--
-- Copyright (C) 2015-2017 Richard Lewis
-- Author: Richard Lewis <richard@rjlewis.me.uk>

-- This file is part of ODD-XML

-- ODD-XML is free software: you can redistribute it and/or modify it
-- under the terms of the GNU Lesser General Public License as
-- published by the Free Software Foundation, either version 3 of the
-- License, or (at your option) any later version.

-- ODD-XML is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.

-- You should have received a copy of the GNU General Public License
-- along with ODD-XML.  If not, see <http://www.gnu.org/licenses/>.

module Text.XML.ODD.BootstrapTEI where

data GlobalAttrs = GlobalAttrs
data GlobalRenditionAttrs = GlobalRenditionAttrs
data GlobalLinkingAttrs = GlobalLinkingAttrs
data GlobalAnalyticAttrs = GlobalAnalyticAttrs
data GlobalFacsAttrs = GlobalFacsAttrs
data GlobalChangeAttrs = GlobalChangeAttrs
data TranslatableAttrs = TranslatableAttrs
data GlobalIdentifiedAttrs = GlobalIdentifiedAttrs
data CombinableAttrs = CombinableAttrs
data DeprecatedAttrs = DeprecatedAttrs
data GlobalNamespaceableAttrs = GlobalNamespaceableAttrs
data ReadFromAttrs = ReadFromAttrs
data GlobalResponsibilityAttrs = GlobalResponsibilityAttrs
data TypedAttrs = TypedAttrs
data RepeatableAttrs = RepeatableAttrs
data SourceAttrs = SourceAttrs

newtype DataName = DataName String -- Or maybe type DataName = XSDString ?
newtype DataXMLName = DataXMLName String
newtype DataLanguage = DataLanguage String -- Or maybe type DataLanguage = XSDLanguage ?
newtype DataNamespace = DataNamespace String -- Or maybe type DataNamespace = XSDAnyURI ?
newtype DataPointer = DataPointer String -- Or maybe type DataPointer = XSDAnyURI ?
newtype DataToken = DataToken String
newtype DataText = DataText String
newtype DataEnumerated = DataEnumerated String
newtype DataRegex = DataRegex String
newtype DataCount = DataCount Int
newtype DataUnboundedInt = DataUnboundedInt Int
newtype DataTruth = DataTruth Bool
newtype DataNumeric = DataNumeric Int
newtype DataXPath = DataXPath String
