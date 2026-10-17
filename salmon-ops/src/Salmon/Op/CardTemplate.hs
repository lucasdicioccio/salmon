{- | An optional Tramaj template a node carries for the web UI: when it is
there, the page draws the node's card (the descriptive part of the panel a
selected node opens) from the template, evaluated in the browser against the
node's own public fields, instead of the default sections.

It rides in 'Salmon.Builtin.Extension.Extension''s @dynamics@ as a
'CardTemplate', so a node without one carries nothing new and renders
exactly what it did. It is presentation only: "Salmon.Op.Dag"'s
@representative@ leaves it out, so adding, changing or removing a template
never changes a node's representative (no conflict, no @Stale@, no restart
of a held action); a re-declaration still replaces it in the magma like
every other field, since the newest declaration's 'Act' wins there.

The source is public, like @help@ and @notes@: it is served on @\/dag@ as the
node's @template@. It is author-written text, never read from a secret.
The contract a template is written against is @salmon-ops\/ui\/card-template.js@.
-}
module Salmon.Op.CardTemplate (
    CardTemplate (..),
    cardTemplateOf,
    isCardTemplate,
    setCardTemplate,
) where

import Data.Dynamic (Dynamic, fromDynamic, toDyn)
import Data.Maybe (isJust, mapMaybe)
import Data.Text (Text)

-- | A Tramaj program, as source text.
newtype CardTemplate = CardTemplate {cardTemplateSource :: Text}
    deriving (Show, Eq)

-- | The template among a node's dynamics, the last one if several.
cardTemplateOf :: [Dynamic] -> Maybe Text
cardTemplateOf ds =
    case mapMaybe fromDynamic ds of
        [] -> Nothing
        ts -> Just (cardTemplateSource (last ts))

isCardTemplate :: Dynamic -> Bool
isCardTemplate d = isJust (fromDynamic d :: Maybe CardTemplate)

-- | Replace any template among the dynamics with this one.
setCardTemplate :: Text -> [Dynamic] -> [Dynamic]
setCardTemplate src ds = filter (not . isCardTemplate) ds <> [toDyn (CardTemplate src)]
