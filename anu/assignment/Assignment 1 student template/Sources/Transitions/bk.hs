--
-- Uwe R. Zimmer
-- Australia 2012
--

module Transitions.For_Ordered_Lists_2D (
   transition_world -- :: Ordered_Lists_2D Cell -> Ordered_Lists_2D Cell
) where

import Data.Cell (Cell)
import Data.Cell (Cell (Head, Tail, Conductor, Empty))
import Data.Coordinates
import Data.Ordered_Lists_2D
import Data.Integer_Subtypes (Nat)


-- Replace this function with something more meaningful:


cell_change :: Nat -> Cell
cell_change num
    | (num == 2)||(num == 1) = Head
    | otherwise = Conductor

transition_cell :: Placed_Element Cell -> Ordered_Lists_2D Cell -> Placed_Element Cell
transition_cell cell local = case cell of
    Placed_Element {x_pos = x, entry = Head} -> Placed_Element {x_pos = x, entry = Tail}
    Placed_Element {x_pos = x, entry = Empty} -> Placed_Element {x_pos = x, entry = Empty}
    Placed_Element {x_pos = x, entry = Tail} -> Placed_Element {x_pos = x, entry = Conductor}
    Placed_Element {x_pos = x, entry = Conductor} -> Placed_Element {x_pos = x, entry = (cell_change (element_occurrence Head local))}

transition_elements :: Placed_Elements Cell -> Y_Coord -> Ordered_Lists_2D Cell -> Placed_Elements Cell
transition_elements elems y_coord world = case elems of
    x:xs    -> (transition_cell (Placed_Element {x_pos = x_pos x, entry = entry x}) (local_elements (x_pos x, y_coord) world) ) : transition_elements xs y_coord world
    []      -> []

transition_world :: Ordered_Lists_2D Cell -> Ordered_Lists_2D Cell
transition_world world = case world of
    y:ys        -> (Sparse_Line {y_pos = y_pos y, entries = (transition_elements (entries y) (y_pos y) world)}) : transition_world ys
    []          -> []
