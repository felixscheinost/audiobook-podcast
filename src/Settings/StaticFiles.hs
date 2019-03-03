{-# LANGUAGE CPP             #-}
{-# LANGUAGE TemplateHaskell #-}
module Settings.StaticFiles where

import           Prelude                         (Bool (..))
import           Yesod.EmbeddedStatic            (mkEmbeddedStatic)
import           Yesod.EmbeddedStatic.Generators (embedDirAt, embedFileAt)

-- This generates easy references to files in the static directory at compile time,
-- giving you compile-time verification that referenced files exist.
-- Warning: any files added to your static directory during run-time can't be
-- accessed this way. You'll have to use their FilePath or URL to access them.
--
-- For example, to refer to @static/js/script.js@ via an identifier, you'd use:
--
--     js_script_js
--
-- If the identifier is not available, you may use:
--
--     StaticFile ["js", "script.js"] []
-- staticFiles "static"

#ifdef DEVELOPMENT
#define DEV_BOOL True
#else
#define DEV_BOOL False
#endif
mkEmbeddedStatic DEV_BOOL "myStatic"
    [ embedDirAt "css" "static/css"
    , embedDirAt "webfonts" "static/webfonts"
    , embedDirAt "img" "static/img"
    , embedFileAt "js/dist/bundle.js" "static/js/dist/bundle.js"
    ]
