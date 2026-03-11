module CMark.Lucid
(
  renderNode,
  renderNodes,
)
where


import CMark
import qualified Lucid.Base as Lucid


renderNode
  :: Monad m
  => [CMarkOption]       -- ^ Will usually be @[]@ or @['optSafe']@
  -> Node
  -> Lucid.HtmlT m ()
renderNode opts node = Lucid.toHtmlRaw (nodeToHtml opts node)

renderNodes
  :: Monad m
  => [CMarkOption]       -- ^ Will usually be @[]@ or @['optSafe']@
  -> [Node]
  -> Lucid.HtmlT m ()
renderNodes opts nodes = renderNode opts node
  where node = Node Nothing DOCUMENT nodes
