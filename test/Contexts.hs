{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
module Contexts where

import NanoVG
import           Language.C.Inline.HaskellIdentifier
import qualified Language.C.Inline.Context as C
import qualified Data.Map as M
import qualified Language.C.Types as C

nanoVGCtx :: C.Context
nanoVGCtx = mempty { C.ctxTypesTable = M.fromList [(C.TypeName "NVGcolor",[t|Color|])
                                                  ,(C.TypeName "NVGpaint",[t|Paint|])
                                                  ,(C.TypeName "NVGglyphPosition",[t|GlyphPosition|])
                                                  ,(C.TypeName "NVGtextRow",[t|TextRow|])] }
