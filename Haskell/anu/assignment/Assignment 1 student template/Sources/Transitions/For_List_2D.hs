--
-- Uwe R. Zimmer
-- Australia 2012
--

module Transitions.For_List_2D (
   transition_world -- :: List_2D Cell -> List_2D Cell
) where

import Data.Cell (Cell)
-- import Data.Cell (Cell (Head, Tail, Conductor, Empty))
-- import Data.Coordinates
import Data.List_2D


-- Replace this function with something more meaningful:

transition_world :: List_2D Cell -> List_2D Cell
transition_world world = world