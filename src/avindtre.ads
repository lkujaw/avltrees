-----------------------------------------------------------------------
--  Copyright 2021 Lev Kujawski                                      --
--                                                                   --
--                  This file is part of AVLTREES.                   --
--                                                                   --
--  AVLTREES is free software: you can redistribute it and/or modify --
--  it under the terms of the GNU Lesser General Public License as   --
--  published by the Free Software Foundation, either version 3 of   --
--       the License, or (at your option) any later version.         --
--                                                                   --
--   AVLTREES is distributed in the hope that it will be useful,     --
--  but WITHOUT ANY WARRANTY; without even the implied warranty of   --
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the    --
--       GNU Lesser General Public License for more details.         --
--                                                                   --
--              You should have received a copy of the               --
--      GNU Lesser General Public License along with AVLTREES.       --
--           If not, see <https://www.gnu.org/licenses/>.            --
--                                                                   --
--  SPDX-License-Identifier: LGPL-3.0-or-later                       --
--                                                                   --
--  File:          avindtre.ads (Ada Package Specification)          --
--  Language:      Ada (1995) [1]                                    --
--  Author:        Lev Kujawski                                      --
--  Description:                                                     --
--    Self-balancing binary trees, based upon the algorithms         --
--    developed by G. M. Adelson-Velsky and E. M. Landis [2].        --
--                                                                   --
--  References:                                                      --
--  [1] Information technology - Programming languages - Ada,        --
--      ISO/IEC 8652:1995(E), 15 Feb. 1995.                          --
--  [2] G. M. Adelson-Velsky and E. M. Landis                        --
--      Doklady Akademii Nauk SSSR 146 (1962), 263-266               --
--      English translation in                                       --
--      "An algorithm for the organization of information",          --
--      Soviet Math. Doklady 3 (1962) 1259-1263.                     --
-----------------------------------------------------------------------

with AVL_Tree_Heights;

generic
   type Key_T (<>) is private;
   type Element_T (<>) is private;

   with function Is_Less_Than
     (X : in Key_T;
      Y : in Key_T)
      return Boolean;
   --  The use of Is_Less_Than mitigates a GNAT hiding warning that
   --  would otherwise occur with "<".

package AVL_Indefinite_Trees is
   pragma Preelaborate;

   Key_Not_Found  : exception;
   Pool_Exhausted : exception;

   type T is limited private;

   --  Removes the node associated with the given key.  The space it
   --  occupies will be reclaimed if Unchecked_Deallocation reclaims
   --  dynamically allocated memory on the target platform.
   --
   --  If the key is not present within the tree, the exception
   --  Key_Not_Found will be raised.
   procedure Remove
     (From_The_Tree : in out T;
      The_Key       : in     Key_T);

   --  Ada (2007) Containers library compatibility name.
   procedure Delete
     (From_The_Tree : in out T;
      The_Key       : in     Key_T) renames Remove;

   --  Irreversibly purges the given tree of all its nodes.
   --
   --  If the tree is already empty, the tree will not be modified.
   procedure Clear
     (The_Tree : in out T);

   --  Returns the least key within the tree.
   --
   --  If the tree is empty, the Key_Not_Found exception will be
   --  raised.
   function Least_Key
     (Within_The_Tree : in T)
      return Key_T;

   --  Returns the greatest key within the tree.
   --
   --  If the tree is empty, the Key_Not_Found exception will be
   --  raised.
   function Greatest_Key
     (Within_The_Tree : in T)
      return Key_T;

   --  Returns the height of the tree, where height is defined as the
   --  longest path (in nodes) from (and including) the root to a node
   --  within.
   function Height
     (Of_The_Tree : in T)
      return AVL_Tree_Heights.T;
   pragma Inline (Height);

   --  Inserts a new key/element pair into the tree.
   --
   --  If the same element is inserted twice, the tree will be not
   --  be modified.  To replace the element associated with a key, use
   --  Replace or Insert_Or_Replace.
   --
   --  Raises the Pool_Exhausted exception when there is insufficient
   --  memory to allocate a new node within the tree.
   procedure Insert
     (Within_The_Tree : in out T;
      The_Key         : in     Key_T;
      With_Element    : in     Element_T);

   --  Inserts a new key/element pair into the tree or replaces the
   --  element associated with the given key if it is already present
   --  in the tree.
   --
   --  Raises the Pool_Exhausted exception when there is insufficient
   --  memory to allocate a new node within the tree.
   procedure Insert_Or_Replace
     (Within_The_Tree : in out T;
      The_Key         : in     Key_T;
      With_Element    : in     Element_T);

   --  Query whether there any nodes within the given tree.
   function Is_Empty
     (The_Tree : in T)
      return Boolean;
   pragma Inline (Is_Empty);

   --  Query whether the given key exists within a tree.
   function Is_Present
     (Within_The_Tree : in T;
      The_Key         : in Key_T)
      return Boolean;

   --  Ada (2007) Containers library compatibility name.
   function Contains
     (Within_The_Tree : in T;
      The_Key         : in Key_T)
      return Boolean renames Is_Present;

   --  Returns the total number of nodes present within the tree.
   function Nodes
     (Within_The_Tree : in T)
      return Natural;
   pragma Inline (Nodes);

   --  Ada (2007) Containers library compatibility name.
   function Length
     (Of_The_Tree : in T)
      return Natural renames Nodes;

   --  Finds and returns the key that is the predecessor to that
   --  given.
   --
   --  If the predecessor key is not present within the tree, the
   --  Key_Not_Found exception will be raised.
   function Predecessor_Key
     (Within_The_Tree : in T;
      Of_The_Key      : in Key_T)
      return Key_T;

   --  Finds and returns the key that is the successor to that given.
   --
   --  If the successor key is not present within the tree, the
   --  Key_Not_Found exception will be raised.
   function Successor_Key
     (Within_The_Tree : in T;
      Of_The_Key      : in Key_T)
      return Key_T;

   --  Replaces the element associated with the given key.
   --
   --  If the key is not present within the tree, the Key_Not_Found
   --  exception will be raised.
   procedure Replace
     (Within_The_Tree : in out T;
      The_Key         : in     Key_T;
      With_Element    : in     Element_T);

   --  Replace the element associated with the greatest key in the
   --  tree.
   --
   --  If the tree is empty, the Key_Not_Found exception will be
   --  raised.
   procedure Replace_Greatest
     (Within_The_Tree : in out T;
      With_Element    : in     Element_T);

   --  Replace the element associated with the least key in the tree.
   --
   --  If the tree is empty, the Key_Not_Found exception will be
   --  raised.
   procedure Replace_Least
     (Within_The_Tree : in out T;
      With_Element    : in     Element_T);

   --  Replace the element associated with the predecessor of the
   --  given key.
   --
   --  If the predecessor of the key is not present within the tree,
   --  the Key_Not_Found exception will be raised.
   procedure Replace_Predecessor
     (Within_The_Tree : in out T;
      Of_The_Key      : in     Key_T;
      With_Element    : in     Element_T);

   --  Replace the element associated with the successor of the given
   --  key.
   --
   --  If the predecessor of the key is not present within the tree,
   --  the Key_Not_Found exception will be raised.
   procedure Replace_Successor
     (Within_The_Tree : in out T;
      Of_The_Key      : in     Key_T;
      With_Element    : in     Element_T);

   --  Retrieves the element associated with the given key.
   --
   --  If the key is not present within the tree, the Key_Not_Found
   --  exception will be raised.
   procedure Retrieve
     (From_The_Tree    : in     T;
      The_Key          : in     Key_T;
      Into_The_Element :    out Element_T);

   --  Retrieves the element associated with the greatest key in the
   --  tree.
   --
   --  If the tree is empty, the Key_Not_Found exception will be
   --  raised.
   procedure Retrieve_Greatest
     (From_The_Tree    : in     T;
      Into_The_Element :    out Element_T);

   --  Retrieves the element associated with the least key in the
   --  tree.
   --
   --  If the tree is empty, the Key_Not_Found exception will be
   --  raised.
   procedure Retrieve_Least
     (From_The_Tree    : in     T;
      Into_The_Element :    out Element_T);

   --  Retrieves the element associated with the predecessor of the
   --  given key.
   --
   --  If the predecessor of the key is not present within the tree,
   --  the Key_Not_Found exception will be raised.
   procedure Retrieve_Predecessor
     (Within_The_Tree  : in     T;
      Of_The_Key       : in     Key_T;
      Into_The_Element : in out Element_T);

   --  Retrieves the element associated with the successor of the
   --  given key.
   --
   --  If the successor of the key is not present within the tree, the
   --  Key_Not_Found exception will be raised.
   procedure Retrieve_Successor
     (Within_The_Tree  : in     T;
      Of_The_Key       : in     Key_T;
      Into_The_Element : in out Element_T);

   --  Swaps the element associated with the given key with the
   --  given element.
   --
   --  If the key is not present within the tree, the Key_Not_Found
   --  exception will be raised.
   procedure Swap
     (Within_The_Tree  : in out T;
      The_Key          : in     Key_T;
      With_The_Element : in out Element_T);

   --  Swaps the element associated with the greatest key within the
   --  tree with the given element.
   --
   --  If the tree is empty, the Key_Not_Found exception will be
   --  raised.
   procedure Swap_Greatest
     (Within_The_Tree  : in out T;
      With_The_Element : in out Element_T);

   --  Swaps the element associated with the least key within the tree
   --  with the given element.
   --
   --  If the tree is empty, the Key_Not_Found exception will be
   --  raised.
   procedure Swap_Least
     (Within_The_Tree  : in out T;
      With_The_Element : in out Element_T);

   --  Swaps the element associated with the predecessor of the given
   --  key with the given element.
   --
   --  If the predecessor of the given key is not found within the
   --  tree, the Key_Not_Found exception will be raised.
   procedure Swap_Predecessor
     (Within_The_Tree  : in out T;
      Of_The_Key       : in     Key_T;
      With_The_Element : in out Element_T);

   --  Swaps the element associated with the successor of the given
   --  key with the given element.
   --
   --  If the successor of the given key is not found within the
   --  tree, the Key_Not_Found exception will be raised.
   procedure Swap_Successor
     (Within_The_Tree  : in out T;
      Of_The_Key       : in     Key_T;
      With_The_Element : in out Element_T);

   generic
      with procedure Process (The_Key     : in     Key_T;
                              The_Element : in     Element_T;
                              Continue    :    out Boolean);
   procedure Traverse_In_Order (The_Tree : in T);

private  --  AVL_Indefinite_Trees  ------------------------------------

   type AVL_Node_T;
   type AVL_Node_A is access AVL_Node_T;
   --  Deallocation of nodes is always performed manually, so the type
   --  is controlled to prevent any pools from attempting garbage
   --  collection.
   pragma Controlled (AVL_Node_A);

   --  Although for clients it is labeled 'Tree', for internal
   --  subprograms, it is often referred to as 'Head'.
   type T is
      record
         Root   : AVL_Node_A         := null;
         Nodes  : Natural            := 0;
         Height : AVL_Tree_Heights.T := 0;
      end record;

end AVL_Indefinite_Trees;
