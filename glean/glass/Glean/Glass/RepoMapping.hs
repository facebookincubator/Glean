module Glean.Glass.RepoMapping where

import qualified Data.Map.Strict as Map

import Glean.Glass.Base ( GleanDBAttrName, GleanDBName(..) )
import Glean.Glass.Types ( Language(..), RepoName(..) )

-- E.g. example: the open source react repo.
gleanIndices :: Map.Map RepoName [(GleanDBName, Language)]
gleanIndices = Map.fromList
    [ (RepoName "react",
        [ ("react", Language_JavaScript) ])
    ]

-- repos that contain symbol attributes
gleanAttrIndices :: Map.Map GleanDBName [GleanDBAttrName]
gleanAttrIndices = Map.empty
