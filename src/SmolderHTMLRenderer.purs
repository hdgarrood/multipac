module SmolderHTMLRenderer
  ( render
  ) where

import Prelude
import Control.Monad.Eff.Unsafe (unsafePerformEff)
import DOM.Node.Types as DOM
import DOM.HTML.Types (HTMLElement)
import DOM.HTML.Window (document)
import DOM.Node.Document (createTextNode, createElement)
import DOM.Node.Element (setAttribute)
import Text.Smolder.Markup (Markup)
import Text.Smolder.Renderer.Util (renderMarkup, Node(..))

render :: Markup e -> DOM.Node
render = renderMarkup >>> buildElement >>> unsafePerformEff

buildElement :: Node e -> DOM.Node
buildElement (Element name attrs _ children) = unit
buildElement (Text str) = createTextNode document str
