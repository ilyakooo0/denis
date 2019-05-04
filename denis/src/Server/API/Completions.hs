{-# LANGUAGE
    DataKinds ,
    DeriveGeneric ,
    OverloadedLabels,
    OverloadedStrings ,
    TypeApplications ,
    TypeOperators,
    LambdaCase #-}

module Server.API.Completions (
    TagsAPI,
    tagsServer
    ) where

import Servant.Server
import Servant
import Data.Tags.Completions
import Server.App
import Data.Connection
import Data.Query
import qualified Data.Text as T
import qualified Data.Post as P
import Data.PostElement
import Data.Tags.Suggestions
import Data.List

type CompletionsDescription = Description "\
    \Gets tag completion tree\n\n\
    \```haskell\n\
    \data CompletionTree = CompletionTreeNode {\n\
    \    value :: Maybe String,\n\
    \    subtree :: Map Char CompletionTree\n\
    \}\n\
    \```\n\n\
    \The psuedocode for retrieving all completions from the tree:\n\n\
    \```swift\n\
    \func getCompletion(tree: CompletionTree, word: String) -> [String] {\n\
    \    if word == \"\" {\n\
    \        return getValues(tree: tree)\n\
    \    }\n\
    \\n\
    \    let character = word.first\n\
    \\n\
    \    if tree.subtree[character] == null {\n\
    \        return []\n\
    \    } else {\n\
    \        return getCompletion(tree: tree.subtree[character], word.withoutFirst)\n\
    \    }\n\
    \}\n\
    \\n\
    \func getValues(tree: CompletionTree) -> [String] {\n\
    \    var out = []\n\
    \\n\
    \    if tree.value != null {\n\
    \        out += tree.value\n\
    \    }\n\
    \\n\
    \    for (key, subtree) in tree {\n\
    \        out += getValues(tree: tree)\n\
    \    }\n\
    \\n\
    \    return out\n\
    \}\n\
    \```"

type SuggestionDescription = Description "Retuens suggested tags for given post element objects."

type TagsAPI = "completions" :> CompletionsDescription :> Get '[JSON] (CompletionTree Char)
    :<|> "suggest" :> SuggestionDescription :> ReqBody '[JSON] [PostElement P.Post] :> Post '[JSON] [T.Text]

tagsServer :: ServerT TagsAPI App
tagsServer = completionHandler :<|> suggestionHandler

completionHandler :: App (CompletionTree Char)
completionHandler = runQerror $ mkCompletionTree <$> getTags

suggestionHandler :: [PostElement P.Post] -> App [T.Text]
suggestionHandler = return . suggestTags . T.concat . intersperse " " . concatMap (\case
    Markdown t -> T.words t
    _ -> [])