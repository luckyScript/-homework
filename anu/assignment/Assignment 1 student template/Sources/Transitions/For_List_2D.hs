--
-- Uwe R. Zimmer
-- Australia 2012
--

module Transitions.For_List_2D (
   transition_world -- :: List_2D Cell -> List_2D Cell
) where

import Data.Cell (Cell)
--import Data.Coordinates
import Data.List_2D


-- Replace this function with something more meaningful:

transition_world :: List_2D Cell -> List_2D Cell
transition_world world = world
--    [] -> []
--    x:xs-> transition_cell x world : transition_world xs



--transition_cell :: Element_w_Coord Cell -> List_2D Cell -> Element_w_Coord Cell
--transition_cell c world = case c of
--    (Empty,(x_coord,y_coord)) -> (Empty,(x_coord,y_coord))
--    (Head,(x_coord,y_coord)) -> (Tail,(x_coord,y_coord))
--    (Tail,(x_coord,y_coord)) -> (Conductor,(x_coord,y_coord))
--    (Conductor,(x_coord,y_coord)) -> occurence_of (x_coord,y_coord) world
--    where
--        occurence_of :: Coord -> List_2D Cell -> Element_w_Coord Cell
--        occurence_of coord w
--            | (element_occurrence Head (local_elements coord w) == 1) || (element_occurrence Head (local_elements coord w) == 2) 
--                = (Head,coord)
--            | otherwise = (Conductor,coord)

--change_state:: Element_w_Coord e -> List_2D Cell -> Element_w_Coord Cell
--change_state conductor world  = case conductor of
--   (_, (x_e, y_e))
--             |element_occurrence Head (local_elements (x_e, y_e) world) == 1 -> (Head,(x_e, y_e))
--             |element_occurrence Head (local_elements (x_e, y_e) world) == 2 -> (Head,(x_e, y_e))
--             |otherwise -> (Conductor, (x_e, y_e))

--move_world :: List_2D Cell -> List_2D Cell -> List_2D Cell
--move_world world backgorund = case world of
--    (element, (x_e, y_e)) : cs
--      | element == Conductor -> (change_state (element, (x_e, y_e)) backgorund) : move_world cs backgorund
--      | element == Empty -> (Empty, (x_e, y_e)) : move_world cs backgorund
--      | element == Head -> ( Tail, (x_e, y_e)) : move_world cs backgorund
--      | element == Tail -> ( Conductor, (x_e, y_e)) : move_world cs backgorund
--      | otherwise -> []
--    [] -> []
-- transition_world is a process, it is always reapeating
