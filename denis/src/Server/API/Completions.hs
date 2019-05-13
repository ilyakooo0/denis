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
import Data.Text (Text)
import Data.Tags.Validation
import Server.Error
import qualified Data.Text as T
import Data.Char

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

type SearchDescription = Description "Returns top 1000 tag completions for given user input."

type TagsAPI = "completions" :> CompletionsDescription :> Get '[JSON] CompletionTree :<|>
    "search" :> SearchDescription :> ReqBody '[JSON, PlainText] Text :> Post '[JSON] [Text]

tagsServer :: ServerT TagsAPI App
tagsServer = completionHandler :<|> tagSearch

completionHandler :: App CompletionTree
completionHandler = runQerror $ mkCompletionTree <$> getTags 1000

tagSearch :: Text -> App [Text]
tagSearch query' =
    if T.null . T.filter (not . isSpace) . T.filter isPrint $ query'
        then return []
        else do
            query <- fromMaybeThrow formatError . return $ validateTag query'
            runQerror . searchTags $ query