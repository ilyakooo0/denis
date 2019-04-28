{-# LANGUAGE
    DataKinds ,
    DeriveGeneric ,
    OverloadedLabels,
    OverloadedStrings ,
    TypeApplications ,
    TypeOperators #-}

module Server.API.Completions (
    CompletionsAPI,
    completionServer
    ) where

import Servant.Server
import Servant
import Data.Completions
import Server.App
import Data.Connection
import Data.Query

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

type CompletionsAPI = "tagCompletions" :> CompletionsDescription :> Get '[JSON] (CompletionTree Char)

completionServer :: ServerT CompletionsAPI App
completionServer = completionHandler

completionHandler :: ServerT CompletionsAPI App
completionHandler = runQerror $ mkCompletionTree <$> getTags