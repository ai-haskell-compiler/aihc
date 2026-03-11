-- | DOT AST. See <http://www.graphviz.org/doc/info/lang.html>.

module Language.Dot.Syntax where

data Graph
  = Graph GraphStrictness GraphDirectedness (Maybe Id) [Statement]
  deriving (Eq, Ord, Show)

data GraphStrictness
  = StrictGraph
  | UnstrictGraph
  deriving (Eq, Ord, Show, Enum, Bounded)

data GraphDirectedness
  = DirectedGraph
  | UndirectedGraph
  deriving (Eq, Ord, Show, Enum, Bounded)

data Id
  = NameId    String
  | StringId  String
  | IntegerId Integer
  | FloatId   Float
  | XmlId     Xml
  deriving (Eq, Ord, Show)

data Statement
  = NodeStatement       NodeId [Attribute]
  | EdgeStatement       [Entity] [Attribute]
  | AttributeStatement  AttributeStatementType [Attribute]
  | AssignmentStatement Id Id
  | SubgraphStatement   Subgraph
  deriving (Eq, Ord, Show)

data AttributeStatementType
  = GraphAttributeStatement
  | NodeAttributeStatement
  | EdgeAttributeStatement
  deriving (Eq, Ord, Show, Enum, Bounded)

data Attribute
  = AttributeSetTrue  Id
  | AttributeSetValue Id Id
  deriving (Eq, Ord, Show)

data NodeId
  = NodeId Id (Maybe Port)
  deriving (Eq, Ord, Show)

data Port
  = PortI Id (Maybe Compass)
  | PortC Compass
  deriving (Eq, Ord, Show)

data Compass
  = CompassN  | CompassE  | CompassS  | CompassW
  | CompassNE | CompassNW | CompassSE | CompassSW
  deriving (Eq, Ord, Show)

data Subgraph
  = NewSubgraph (Maybe Id) [Statement]
  | SubgraphRef Id
  deriving (Eq, Ord, Show)

data Entity
  = ENodeId   EdgeType NodeId
  | ESubgraph EdgeType Subgraph
  deriving (Eq, Ord, Show)

data EdgeType
  = NoEdge
  | DirectedEdge
  | UndirectedEdge
  deriving (Eq, Ord, Show, Enum, Bounded)

data Xml
  = XmlEmptyTag XmlName [XmlAttribute]
  | XmlTag      XmlName [XmlAttribute] [Xml]
  | XmlText     String
  deriving (Eq, Ord, Show)

data XmlName
  = XmlName String
  deriving (Eq, Ord, Show)

data XmlAttribute
  = XmlAttribute XmlName XmlAttributeValue
  deriving (Eq, Ord, Show)

data XmlAttributeValue
  = XmlAttributeValue String
  deriving (Eq, Ord, Show)
