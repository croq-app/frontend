module Croq.Dbg exposing (..)

import Debug



-------------------------------------------------------------------------------
-- Comment this in production builds
-------------------------------------------------------------------------------


log : String -> a -> a
log =
    Debug.log


repr : a -> String
repr =
    Debug.toString


todo : String -> a
todo =
    Debug.todo



-------------------------------------------------------------------------------
-- Use these in production builds
-------------------------------------------------------------------------------
-- log : a -> b -> b
-- log _ y =
--     y
--
-- repr : a -> String
-- repr _ =
--     "<???>"
--
-- {-| It fails in an infinte loop
-- -}
-- todo : String -> a
-- todo st =
--     todo ("" ++ st)
-------------------------------------------------------------------------------
-- More functions...
-------------------------------------------------------------------------------


dbg : a -> a
dbg =
    log "debug"
