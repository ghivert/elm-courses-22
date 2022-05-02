module Utils.Lists exposing (..)

import Expect exposing (Expectation)

-- This module exposes helpers. Don't bother about it.

filter : (List a -> List a) -> List a -> List a -> () -> Expectation
filter fun list result () = Expect.equal result <| fun list
