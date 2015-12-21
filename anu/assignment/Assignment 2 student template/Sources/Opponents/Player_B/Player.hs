module Opponents.Player_B.Player (
   select_move -- :: Board -> Lookahead -> Pond_Ix
) where

import Data.Board

select_move :: Board -> Lookahead -> Pond_Ix
select_move _ _ = error " This player has no idea yet what to do - please help him!"