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
--  File:          avltrees.adb (Ada Package Body)                   --
--  Language:      Ada (1987) [1]                                    --
--  Author:        Lev Kujawski                                      --
--  Description:                                                     --
--    Self-balancing binary trees, based upon the algorithms         --
--    developed by G. M. Adelson-Velsky and E. M. Landis [2].        --
--                                                                   --
--  References:                                                      --
--  [1] Programming languages - Ada, ISO/IEC 8652:1987,              --
--      15 Jun. 1987.                                                --
--  [2] G. M. Adelson-Velsky and E. M. Landis                        --
--      Doklady Akademii Nauk SSSR 146 (1962), 263-266               --
--      English translation in                                       --
--      "An algorithm for the organization of information",          --
--      Soviet Math. Doklady 3 (1962) 1259-1263.                     --
-----------------------------------------------------------------------

with System;
with Unchecked_Conversion;
with Unchecked_Deallocation;

package body AVL_Trees is

   Alignment_Error : exception;

   subtype Address_Index_T is Positive range 1 .. System.Address'Size;

   type Address_Booleans_T is array (Address_Index_T) of Boolean;
   pragma Pack (Address_Booleans_T);

   Address_Mask : constant Address_Booleans_T :=
     Address_Booleans_T'(1 .. 2 => False, others => True);

   Types_Mask : constant Address_Booleans_T :=
     Address_Booleans_T'(1 .. 2 => True, others => False);

   --  Denotes left-heavy (-1), balanced (0), or right-heavy (+1)
   --  subtrees.
   type Balance_T is range -1 .. +1;
   for Balance_T'Size use 2;

   --  For threaded AVL tree implementions, a link can either point to
   --  a child of the node, or to the predecessor or successor of the
   --  node within the tree.
   --
   --  As a special case, for nodes that have no predecessor or
   --  successor, the corresponding link will be null.
   type Link_T is (Child, Thread);

   type Direction_T is (Left, Right);

   type AVL_Node_T is
      record
         Key       : Key_T;
         Left_Link : System.Address;
         RL        : AVL_Node_A;  --  Right Link
         Element   : Element_T;
         Balance   : Balance_T;
      end record;
   pragma Pack (AVL_Node_T);
   for AVL_Node_T'Alignment use 4;

   function Access_To_Address is new Unchecked_Conversion
     (Source => AVL_Node_A, Target => System.Address);
   pragma Inline (Access_To_Address);

   function Access_To_Booleans is new Unchecked_Conversion
     (Source => AVL_Node_A, Target => Address_Booleans_T);
   pragma Inline (Access_To_Booleans);

   function Address_To_Booleans is new Unchecked_Conversion
     (Source => System.Address, Target => Address_Booleans_T);
   pragma Inline (Address_To_Booleans);

   procedure Allocate
     (Key        : in     Key_T;
      Element    : in     Element_T;
      Left_Link  : in     AVL_Node_A;
      Left_Type  : in     Link_T;
      Right_Link : in     AVL_Node_A;
      Right_Type : in     Link_T;
      Balance    : in     Balance_T;
      The_Node   :    out AVL_Node_A);

   function Balance_Of_Direction
     (Direction : in Direction_T)
      return Balance_T;
   pragma Inline (Balance_Of_Direction);

   procedure Balance_Post_Deletion
     (Direction   : in     Direction_T;
      Parent_Node : in     AVL_Node_A;
      Head        : in out T);

   function Booleans_To_Address is new Unchecked_Conversion
     (Source => Address_Booleans_T, Target => System.Address);
   pragma Inline (Booleans_To_Address);

   procedure Deallocate
     (The_Node : in out AVL_Node_A);
   pragma Inline (Deallocate);

   procedure Deallocate_Link
     (Of_Node   : in AVL_Node_A;
      Direction : in Direction_T);
   pragma Inline (Deallocate_Link);

   function Element_Of
     (The_Node : in AVL_Node_A)
      return Element_T;
   pragma Inline (Element_Of);

   function Is_Equal_To
     (X : in Key_T;
      Y : in Key_T)
      return Boolean;
   pragma Inline (Is_Equal_To);

   function Is_Key_Equal
     (X : in Key_T;
      Y : in AVL_Node_A)
      return Boolean;
   pragma Inline (Is_Key_Equal);

   function Is_Key_Less
     (X : in Key_T;
      Y : in AVL_Node_A)
      return Boolean;
   pragma Inline (Is_Key_Less);

   function Is_Node_Less
     (X : in AVL_Node_A;
      Y : in Key_T)
      return Boolean;
   pragma Inline (Is_Node_Less);

   function Key_Of
     (The_Node : in AVL_Node_A)
      return Key_T;
   pragma Inline (Key_Of);

   function Least_Node
     (Within_The_Tree : in T)
      return AVL_Node_A;

   function Link
     (Of_Node   : in AVL_Node_A;
      Direction : in Direction_T)
      return AVL_Node_A;
   pragma Inline (Link);

   function Link_Type
     (Of_Node   : in AVL_Node_A;
      Direction : in Direction_T)
      return Link_T;
   pragma Inline (Link_Type);

   function Node
     (Within_The_Tree : in T;
      That_Has_Key    : in Key_T)
      return AVL_Node_A;

   function Opposite
     (Direction : in Direction_T)
      return Direction_T;
   pragma Inline (Opposite);

   --  Provide a pointer to the parent of the given node.
   --
   --  If the parent is the root of the tree, null is returned.
   function Parent
     (Of_Node : in AVL_Node_A)
      return AVL_Node_A;

   --  NNode:
   --    New Node. The newly inserted node that triggered the balance.
   --  NZRT:
   --    Non-Zero Rooted Tree. Tree.Root or one of its subtrees.
   --  PNZRT:
   --    Parent of NZRT. PNZRT = null iff NZRT = Tree.Root, in which
   --    case 'Head' is used instead.
   --  TMR:
   --    Tree Movement Record, an array of the moves necessary to
   --    reach Node from NZRT.

   --  Called exclusively by Remove. Should be inlined.
   procedure Remove_Node
     (Key  : in     Key_T;
      Head : in out T);
   pragma Inline (Remove_Node);

   --  left child-right child (left child has right thread)
   procedure Remove_Node_LCRC_LCT
     (PNode : in     AVL_Node_A;
      DNode : in out AVL_Node_A;
      Head  : in out T);
   pragma Inline (Remove_Node_LCRC_LCT);

   --  left child-right child (replace root with predecessor)
   procedure Remove_Node_LCRC_Predecessor
     (PNode : in     AVL_Node_A;
      DNode : in out AVL_Node_A;
      Head  : in out T);
   pragma Inline (Remove_Node_LCRC_Predecessor);

   --  left child-right child (right child has left thread)
   procedure Remove_Node_LCRC_RCT
     (PNode : in     AVL_Node_A;
      DNode : in out AVL_Node_A;
      Head  : in out T);
   pragma Inline (Remove_Node_LCRC_RCT);

   --  left child-right child (replace root with successor)
   procedure Remove_Node_LCRC_Successor
     (PNode : in     AVL_Node_A;
      DNode : in out AVL_Node_A;
      Head  : in out T);
   pragma Inline (Remove_Node_LCRC_Successor);

   --  left child-right thread
   procedure Remove_Node_LCRT
     (PNode : in     AVL_Node_A;
      DNode : in out AVL_Node_A;
      Head  : in out T);
   pragma Inline (Remove_Node_LCRT);

   --  left thread-right child
   procedure Remove_Node_LTRC
     (PNode : in     AVL_Node_A;
      DNode : in out AVL_Node_A;
      Head  : in out T);
   pragma Inline (Remove_Node_LTRC);

   --  left child-right-child (left child has right thread)
   procedure Remove_Root_LCRC_LCT
     (Head : in out T);
   pragma Inline (Remove_Root_LCRC_LCT);

   --  left child-right child (replace root with predecessor)
   procedure Remove_Root_LCRC_Predecessor
     (Head : in out T);
   pragma Inline (Remove_Root_LCRC_Predecessor);

   --  left child-right-child (right child has left thread)
   procedure Remove_Root_LCRC_RCT
     (Head : in out T);
   pragma Inline (Remove_Root_LCRC_RCT);

   --  left child-right child (replace root with successor)
   procedure Remove_Root_LCRC_Successor
     (Head : in out T);
   pragma Inline (Remove_Root_LCRC_Successor);

   --  left child-right thread
   procedure Remove_Root_LCRT
     (Head : in out T);
   pragma Inline (Remove_Root_LCRT);

   --  left thread-right child
   procedure Remove_Root_LTRC
     (Head : in out T);
   pragma Inline (Remove_Root_LTRC);

   procedure Replace_Element
     (Within_The_Node  : in AVL_Node_A;
      With_The_Element : in Element_T);

   procedure Set_Link
     (Of_Node   : in AVL_Node_A;
      Direction : in Direction_T;
      To_Node   : in AVL_Node_A);
   pragma Inline (Set_Link);

   procedure Set_Link_Type
     (Of_Node   : in AVL_Node_A;
      Direction : in Direction_T;
      To_Type   : in Link_T);
   pragma Inline (Set_Link_Type);

   function Successor_Node
     (Of_Node : in AVL_Node_A)
      return AVL_Node_A;

   procedure Allocate
     (Key        : in     Key_T;
      Element    : in     Element_T;
      Left_Link  : in     AVL_Node_A;
      Left_Type  : in     Link_T;
      Right_Link : in     AVL_Node_A;
      Right_Type : in     Link_T;
      Balance    : in     Balance_T;
      The_Node   :    out AVL_Node_A)
   is
      New_Node_Pointer  : AVL_Node_A := null;
      New_Node_Booleans : Address_Booleans_T;
   begin
      New_Node_Pointer := new AVL_Node_T'
        (Key       => Key,
         Left_Link => Access_To_Address (Left_Link),
         RL        => Right_Link,
         Element   => Element,
         Balance   => Balance);

      New_Node_Booleans := Access_To_Booleans (New_Node_Pointer);

      if New_Node_Booleans (1) or else New_Node_Booleans (2) then
         raise Alignment_Error;
      end if;

      Set_Link_Type (New_Node_Pointer, Left, Left_Type);
      Set_Link_Type (New_Node_Pointer, Right, Right_Type);
      The_Node := New_Node_Pointer;
   exception
      when Storage_Error =>
         raise Pool_Exhausted;
   end Allocate;

   function Balance_Of_Direction
     (Direction : in Direction_T)
      return Balance_T
   is
      Result : Balance_T;
   begin
      case Direction is
         when Left =>
            Result := -1;
         when Right =>
            Result := +1;
      end case;

      return Result;
   end Balance_Of_Direction;

   procedure Balance_Post_Deletion
     (Direction   : in     Direction_T;
      Parent_Node : in     AVL_Node_A;
      Head        : in out T)
   is
      procedure Double_Rotation;

      Parent_Direction  : Direction_T := Direction;
      Current_Direction : Direction_T;
      PNode             : AVL_Node_A  := Parent_Node;
      SNode             : AVL_Node_A  := null;
      TNode             : AVL_Node_A  := null;

      procedure Double_Rotation
      is
         MN_C : constant AVL_Node_A := SNode;
      begin
         SNode := Link (MN_C, Current_Direction);

         case Link_Type (SNode, (Opposite (Current_Direction))) is
            when Child =>
               Set_Link
                 (MN_C, Current_Direction,
                  Link (SNode, Opposite (Current_Direction)));
               Set_Link (SNode, Opposite (Current_Direction), MN_C);
            when Thread =>
               Set_Link_Type
                 (SNode, Opposite (Current_Direction), Child);
               Set_Link_Type (MN_C, Current_Direction, Thread);
         end case;

         case Link_Type (SNode, Current_Direction) is
            when Child =>
               Set_Link
                 (TNode, Opposite (Current_Direction),
                  Link (SNode, Current_Direction));
               Set_Link (SNode, Current_Direction, TNode);
            when Thread =>
               Set_Link_Type (SNode, Current_Direction, Child);
               Set_Link (TNode, Opposite (Current_Direction), SNode);
               Set_Link_Type
                 (TNode, Opposite (Current_Direction), Thread);
         end case;

         if SNode.all.Balance
           = Balance_Of_Direction (Current_Direction)
         then
            TNode.all.Balance := 0;
            SNode.all.Balance := 0;
            MN_C.all.Balance  :=
              Balance_Of_Direction (Opposite (Current_Direction));
         elsif SNode.all.Balance
           = Balance_Of_Direction (Opposite (Current_Direction))
         then
            TNode.all.Balance :=
              Balance_Of_Direction (Current_Direction);
            SNode.all.Balance := 0;
            MN_C.all.Balance  := 0;
         else
            TNode.all.Balance := 0;
            MN_C.all.Balance  := 0;
         end if;
      end Double_Rotation;

   begin  --  Balance_Post_Deletion
      loop
         TNode             := PNode;
         PNode             := Parent (Of_Node => TNode);
         Current_Direction := Parent_Direction;

         if PNode /= null
           and then Link (PNode, Parent_Direction) /= TNode
         then
            Parent_Direction := Opposite (Parent_Direction);
         end if;

         if TNode.all.Balance
           = Balance_Of_Direction (Current_Direction)
         then
            TNode.all.Balance := 0;

            if TNode = Head.Root then
               Head.Height := Head.Height - 1;
            end if;
         elsif TNode.all.Balance
           = Balance_Of_Direction (Opposite (Current_Direction))
         then
            SNode := Link (TNode, Opposite (Current_Direction));

            if Balance_Of_Direction (Current_Direction)
              = SNode.all.Balance
            then
               Double_Rotation;
            elsif Balance_Of_Direction (Opposite (Current_Direction))
              = SNode.all.Balance
            then
               case Link_Type (SNode, Current_Direction) is
                  when Child =>
                     Set_Link
                       (TNode, Opposite (Current_Direction),
                        Link (SNode, Current_Direction));
                     Set_Link (SNode, Current_Direction, TNode);
                  when Thread =>
                     Set_Link_Type
                       (TNode, Opposite (Current_Direction), Thread);
                     Set_Link_Type (SNode, Current_Direction, Child);
               end case;

               TNode.all.Balance := 0;
               SNode.all.Balance := 0;
            else
               Set_Link
                 (TNode, Opposite (Current_Direction),
                  Link (SNode, Current_Direction));
               Set_Link (SNode, Current_Direction, TNode);

               TNode.all.Balance :=
                 Balance_Of_Direction (Opposite (Current_Direction));
               SNode.all.Balance :=
                 Balance_Of_Direction (Current_Direction);
            end if;

            if PNode = null then
               Head.Root := SNode;

               if Head.Root.all.Balance = 0 then
                  Head.Height := Head.Height - 1;
               end if;

               exit;
            end if;

            Set_Link (PNode, Parent_Direction, SNode);

            exit when SNode.all.Balance /= 0;
         else
            TNode.all.Balance :=
              Balance_Of_Direction (Opposite (Current_Direction));
            exit;
         end if;

         exit when PNode = null;
      end loop;
   end Balance_Post_Deletion;

   procedure Clear
     (The_Tree : in out T)
   is
      Node_Pointer : AVL_Node_A := The_Tree.Root;
   begin  --  Clear
      if Node_Pointer /= null then
         loop
            while Link_Type (Node_Pointer, Left) = Child loop
               Node_Pointer := Link (Node_Pointer, Left);
            end loop;

            while Link_Type (Node_Pointer, Right) = Thread
              and then Node_Pointer.all.RL /= null
            loop
               Node_Pointer := Link (Node_Pointer, Right);

               if Link_Type (Link (Node_Pointer, Left), Right) = Child
               then
                  Set_Link
                    (Link (Node_Pointer, Left), Left, Node_Pointer);
                  Node_Pointer := Link (Node_Pointer, Left);

                  while Link_Type (Node_Pointer, Right) = Child loop
                     Set_Link
                       (Link (Node_Pointer, Right), Left, Node_Pointer);
                     Node_Pointer := Link (Node_Pointer, Right);
                  end loop;

                  while Link (Link (Node_Pointer, Left), Left)
                    /= Node_Pointer
                  loop
                     Node_Pointer := Link (Node_Pointer, Left);
                     Deallocate_Link (Node_Pointer, Right);
                  end loop;

                  Node_Pointer := Link (Node_Pointer, Left);
               end if;

               Deallocate_Link (Node_Pointer, Left);
            end loop;

            exit when Link (Node_Pointer, Right) = null;

            Node_Pointer := Link (Node_Pointer, Right);
         end loop;

         Node_Pointer := The_Tree.Root;

         while Link_Type (Node_Pointer, Right) = Child loop
            Set_Link (Link (Node_Pointer, Right), Left, Node_Pointer);
            Node_Pointer := Link (Node_Pointer, Right);
         end loop;

         while Node_Pointer /= The_Tree.Root loop
            Node_Pointer := Link (Node_Pointer, Left);
            Deallocate_Link (Node_Pointer, Right);
         end loop;

         --  According to ARM 13.11.2, Tree.Root does not need to be
         --  explicitly set to null after deallocation.
         Deallocate (The_Node => The_Tree.Root);
         The_Tree.Nodes  := 0;
         The_Tree.Height := 0;
      end if;
   end Clear;

   procedure Deallocate
     (The_Node : in out AVL_Node_A)
   is
      procedure Unchecked_Deallocate_Node is new Unchecked_Deallocation
        (Object => AVL_Node_T, Name => AVL_Node_A);
   begin
      pragma Assert (The_Node /= null);
      Unchecked_Deallocate_Node (X => The_Node);
   end Deallocate;

   procedure Deallocate_Link
     (Of_Node   : in AVL_Node_A;
      Direction : in Direction_T)
   is
      Pointer_To_Deallocate : AVL_Node_A := null;
   begin
      pragma Assert (Of_Node /= null);
      Pointer_To_Deallocate := Link (Of_Node, Direction);
      pragma Assert (Pointer_To_Deallocate /= null);
      Deallocate (Pointer_To_Deallocate);
      Set_Link (Of_Node, Direction, null);
   end Deallocate_Link;

   function Element_Of
     (The_Node : in AVL_Node_A)
      return Element_T
   is
   begin
      pragma Assert (The_Node /= null);
      return The_Node.all.Element;
   end Element_Of;

   function Greatest_Key
     (Within_The_Tree : in T)
      return Key_T
   is
      Node_Pointer : AVL_Node_A := Within_The_Tree.Root;
   begin
      if Node_Pointer = null then
         raise Key_Not_Found;
      end if;

      while Link_Type (Node_Pointer, Right) = Child loop
         Node_Pointer := Link (Node_Pointer, Right);
      end loop;

      return Key_Of (The_Node => Node_Pointer);
   end Greatest_Key;

   function Height
     (Of_The_Tree : in T)
      return AVL_Tree_Heights.T
   is
   begin
      return Of_The_Tree.Height;
   end Height;

   procedure Insert
     (Within_The_Tree : in out T;
      The_Key         : in     Key_T;
      With_Element    : in     Element_T)
   is
      procedure Insert_In_Tree;

      procedure Insert_In_Tree
      is
         PNode : AVL_Node_A := Within_The_Tree.Root;
         CNode : AVL_Node_A := Within_The_Tree.Root;
         NZRT  : AVL_Node_A := Within_The_Tree.Root;
         PNZRT : AVL_Node_A := null;
      begin
         loop
            if Is_Key_Less (The_Key, CNode) then
               case Link_Type (CNode, Left) is
                  when Child =>
                     CNode := Link (CNode, Left);

                     if CNode.all.Balance /= 0 then
                        PNZRT := PNode;
                        NZRT  := CNode;
                     end if;

                     PNode := CNode;
                  when Thread =>
                     Allocate
                       (Key        => The_Key,
                        Element    => With_Element,
                        Left_Link  => Link (CNode, Left),
                        Left_Type  => Thread,
                        Right_Link => CNode,
                        Right_Type => Thread,
                        Balance    => 0,
                        The_Node   => PNode);

                     Set_Link (CNode, Left, PNode);
                     Set_Link_Type (CNode, Left, Child);

                     if Is_Key_Less (The_Key, NZRT) then
                        CNode := Link (NZRT, Left);

                        loop
                           --  While moving down the tree towards the
                           --  new node, adjust the balance factors
                           --  along the way.
                           if Is_Key_Less (The_Key, CNode) then
                              --  The left subtree of the node has
                              --  become heavy due to the new
                              --  addition.
                              CNode.all.Balance := -1;
                              CNode             := Link (CNode, Left);
                           elsif Is_Node_Less (CNode, The_Key) then
                              --  The right subtree of the node has
                              --  become heavy due to the new
                              --  addition.
                              CNode.all.Balance := +1;
                              CNode             := Link (CNode, Right);
                           else
                              --  CNode is again equal to the new node.
                              exit;
                           end if;
                        end loop;

                        case NZRT.all.Balance is
                           when +1 =>
                              NZRT.all.Balance := 0;
                           when 0 =>
                              NZRT.all.Balance       := -1;
                              Within_The_Tree.Height :=
                                Within_The_Tree.Height + 1;
                           when -1 =>
                              CNode := Link (NZRT, Left);

                              if CNode.all.Balance = -1 then
                                 case Link_Type (CNode, Right) is
                                    when Child =>
                                       Set_Link
                                         (NZRT, Left,
                                          Link (CNode, Right));
                                       CNode.all.RL := NZRT;
                                    when Thread =>
                                       Set_Link_Type
                                         (NZRT, Left, Thread);
                                       Set_Link_Type
                                         (CNode, Right, Child);
                                 end case;

                                 NZRT.all.Balance  := 0;
                                 CNode.all.Balance := 0;
                              else
                                 PNode        := CNode;
                                 CNode        := PNode.all.RL;
                                 PNode.all.RL := Link (CNode, Left);
                                 Set_Link (CNode, Left, PNode);

                                 case Link_Type (CNode, Right) is
                                    when Child =>
                                       Set_Link
                                         (NZRT, Left,
                                          Link (CNode, Right));
                                       CNode.all.RL := NZRT;
                                    when Thread =>
                                       Set_Link_Type
                                         (CNode, Right, Child);
                                       Set_Link (NZRT, Left, CNode);
                                       Set_Link_Type
                                         (NZRT, Left, Thread);
                                 end case;

                                 if CNode.all.Balance = -1 then
                                    NZRT.all.Balance  := +1;
                                    CNode.all.Balance := 0;
                                    PNode.all.Balance := 0;
                                 else
                                    NZRT.all.Balance  := 0;
                                    CNode.all.Balance := 0;
                                    PNode.all.Balance := -1;
                                 end if;
                              end if;

                              if PNZRT = null then
                                 Within_The_Tree.Root := CNode;
                              elsif Link (PNZRT, Left) = NZRT then
                                 Set_Link (PNZRT, Left, CNode);
                              else
                                 PNZRT.all.RL := CNode;
                              end if;
                        end case;
                     else
                        CNode := NZRT.all.RL;

                        loop
                           --  While moving down the tree towards the
                           --  new node, adjust the balance factors
                           --  along the way.
                           if Is_Key_Less (The_Key, CNode) then
                              --  The left subtree of the node has
                              --  become heavy due to the new
                              --  addition.
                              CNode.all.Balance := -1;
                              CNode             := Link (CNode, Left);
                           elsif Is_Node_Less (CNode, The_Key) then
                              --  The right subtree of the node has
                              --  become heavy due to the new
                              --  addition.
                              CNode.all.Balance := +1;
                              CNode             := Link (CNode, Right);
                           else
                              --  CNode is again equal to the new node.
                              exit;
                           end if;
                        end loop;

                        case NZRT.all.Balance is
                           when -1 =>
                              NZRT.all.Balance := 0;
                           when 0 =>
                              NZRT.all.Balance       := +1;
                              Within_The_Tree.Height :=
                                Within_The_Tree.Height + 1;
                           when +1 =>
                              CNode := NZRT.all.RL;

                              if CNode.all.Balance = +1 then
                                 NZRT.all.RL := Link (CNode, Left);
                                 Set_Link (CNode, Left, NZRT);
                                 NZRT.all.Balance  := 0;
                                 CNode.all.Balance := 0;
                              else
                                 PNode := CNode;
                                 CNode := Link (PNode, Left);

                                 case Link_Type (CNode, Right) is
                                    when Child =>
                                       Set_Link
                                         (PNode, Left,
                                          Link (CNode, Right));
                                       CNode.all.RL := PNode;
                                    when Thread =>
                                       Set_Link_Type
                                         (CNode, Right, Child);
                                       Set_Link_Type
                                         (PNode, Left, Thread);
                                 end case;

                                 case Link_Type (CNode, Left) is
                                    when Child =>
                                       NZRT.all.RL :=
                                         Link (CNode, Left);
                                       Set_Link (CNode, Left, NZRT);
                                    when Thread =>
                                       Set_Link_Type
                                         (CNode, Left, Child);
                                       NZRT.all.RL := CNode;
                                       Set_Link_Type
                                         (NZRT, Right, Thread);
                                 end case;

                                 case CNode.all.Balance is
                                    when +1 =>
                                       NZRT.all.Balance  := -1;
                                       CNode.all.Balance := 0;
                                       PNode.all.Balance := 0;
                                    when 0 =>
                                       NZRT.all.Balance  := 0;
                                       PNode.all.Balance := 0;
                                    when -1 =>
                                       NZRT.all.Balance  := 0;
                                       CNode.all.Balance := 0;
                                       PNode.all.Balance := +1;
                                 end case;
                              end if;

                              if PNZRT = null then
                                 Within_The_Tree.Root := CNode;
                              elsif Link (PNZRT, Left) = NZRT then
                                 Set_Link (PNZRT, Left, CNode);
                              else
                                 PNZRT.all.RL := CNode;
                              end if;
                        end case;
                     end if;

                     Within_The_Tree.Nodes :=
                       Within_The_Tree.Nodes + 1;
                     exit;
               end case;
            elsif Is_Node_Less (CNode, The_Key) then
               case Link_Type (CNode, Right) is
                  when Child =>
                     CNode := Link (CNode, Right);

                     if CNode.all.Balance /= 0 then
                        PNZRT := PNode;
                        NZRT  := CNode;
                     end if;

                     PNode := CNode;
                  when Thread =>
                     Allocate
                       (Key        => The_Key,
                        Element    => With_Element,
                        Left_Link  => CNode,
                        Left_Type  => Thread,
                        Right_Link => CNode.all.RL,
                        Right_Type => Thread,
                        Balance    => 0,
                        The_Node   => PNode);

                     CNode.all.RL := PNode;
                     Set_Link_Type (CNode, Right, Child);

                     if Is_Key_Less (The_Key, NZRT) then
                        CNode := Link (NZRT, Left);

                        loop
                           --  While moving down the tree towards the
                           --  new node, adjust the balance factors
                           --  along the way.
                           if Is_Key_Less (The_Key, CNode) then
                              --  The left subtree of the node has
                              --  become heavy due to the new
                              --  addition.
                              CNode.all.Balance := -1;
                              CNode             := Link (CNode, Left);
                           elsif Is_Node_Less (CNode, The_Key) then
                              --  The right subtree of the node has
                              --  become heavy due to the new
                              --  addition.
                              CNode.all.Balance := +1;
                              CNode             := Link (CNode, Right);
                           else
                              --  CNode is again equal to the new node.
                              exit;
                           end if;
                        end loop;

                        case NZRT.all.Balance is
                           when +1 =>
                              NZRT.all.Balance := 0;
                           when 0 =>
                              NZRT.all.Balance       := -1;
                              Within_The_Tree.Height :=
                                Within_The_Tree.Height + 1;
                           when -1 =>
                              CNode := Link (NZRT, Left);

                              if CNode.all.Balance = -1 then
                                 Set_Link
                                   (NZRT, Left, Link (CNode, Right));
                                 CNode.all.RL      := NZRT;
                                 NZRT.all.Balance  := 0;
                                 CNode.all.Balance := 0;
                              else
                                 PNode := CNode;
                                 CNode := PNode.all.RL;

                                 case Link_Type (CNode, Left) is
                                    when Child =>
                                       PNode.all.RL :=
                                         Link (CNode, Left);
                                       Set_Link (CNode, Left, PNode);
                                    when Thread =>
                                       Set_Link_Type
                                         (CNode, Left, Child);
                                       Set_Link_Type
                                         (PNode, Right, Thread);
                                 end case;

                                 case Link_Type (CNode, Right) is
                                    when Child =>
                                       Set_Link
                                         (NZRT, Left,
                                          Link (CNode, Right));
                                       CNode.all.RL := NZRT;
                                    when Thread =>
                                       Set_Link_Type
                                         (CNode, Right, Child);
                                       Set_Link (NZRT, Left, CNode);
                                       Set_Link_Type
                                         (NZRT, Left, Thread);
                                 end case;

                                 case CNode.all.Balance is
                                    when -1 =>
                                       NZRT.all.Balance  := +1;
                                       CNode.all.Balance := 0;
                                       PNode.all.Balance := 0;
                                    when 0 =>
                                       NZRT.all.Balance  := 0;
                                       PNode.all.Balance := 0;
                                    when +1 =>
                                       NZRT.all.Balance  := 0;
                                       CNode.all.Balance := 0;
                                       PNode.all.Balance := -1;
                                 end case;
                              end if;

                              if PNZRT = null then
                                 Within_The_Tree.Root := CNode;
                              elsif Link (PNZRT, Left) = NZRT then
                                 Set_Link (PNZRT, Left, CNode);
                              else
                                 PNZRT.all.RL := CNode;
                              end if;
                        end case;
                     else
                        CNode := NZRT.all.RL;

                        loop
                           --  While moving down the tree towards the
                           --  new node, adjust the balance factors
                           --  along the way.
                           if Is_Key_Less (The_Key, CNode) then
                              --  The left subtree of the node has
                              --  become heavy due to the new
                              --  addition.
                              CNode.all.Balance := -1;
                              CNode             := Link (CNode, Left);
                           elsif Is_Node_Less (CNode, The_Key) then
                              --  The right subtree of the node has
                              --  become heavy due to the new
                              --  addition.
                              CNode.all.Balance := +1;
                              CNode             := Link (CNode, Right);
                           else
                              --  CNode is again equal to the new node.
                              exit;
                           end if;
                        end loop;

                        case NZRT.all.Balance is
                           when -1 =>
                              NZRT.all.Balance := 0;
                           when 0 =>
                              NZRT.all.Balance       := +1;
                              Within_The_Tree.Height :=
                                Within_The_Tree.Height + 1;
                           when +1 =>
                              CNode := NZRT.all.RL;

                              if CNode.all.Balance = +1 then
                                 case Link_Type (CNode, Left) is
                                    when Child =>
                                       NZRT.all.RL :=
                                         Link (CNode, Left);
                                       Set_Link (CNode, Left, NZRT);
                                    when Thread =>
                                       Set_Link_Type
                                         (NZRT, Right, Thread);
                                       Set_Link_Type
                                         (CNode, Left, Child);
                                 end case;

                                 NZRT.all.Balance  := 0;
                                 CNode.all.Balance := 0;
                              else
                                 PNode := CNode;
                                 CNode := Link (PNode, Left);
                                 Set_Link
                                   (PNode, Left, Link (CNode, Right));
                                 CNode.all.RL := PNode;

                                 case Link_Type (CNode, Left) is
                                    when Child =>
                                       NZRT.all.RL :=
                                         Link (CNode, Left);
                                       Set_Link (CNode, Left, NZRT);
                                    when Thread =>
                                       Set_Link_Type
                                         (CNode, Left, Child);
                                       NZRT.all.RL := CNode;
                                       Set_Link_Type
                                         (NZRT, Right, Thread);
                                 end case;

                                 if CNode.all.Balance = +1 then
                                    NZRT.all.Balance  := -1;
                                    CNode.all.Balance := 0;
                                    PNode.all.Balance := 0;
                                 else
                                    NZRT.all.Balance  := 0;
                                    CNode.all.Balance := 0;
                                    PNode.all.Balance := +1;
                                 end if;
                              end if;

                              if PNZRT = null then
                                 Within_The_Tree.Root := CNode;
                              elsif Link (PNZRT, Left) = NZRT then
                                 Set_Link (PNZRT, Left, CNode);
                              else
                                 PNZRT.all.RL := CNode;
                              end if;
                        end case;
                     end if;

                     Within_The_Tree.Nodes :=
                       Within_The_Tree.Nodes + 1;
                     exit;
               end case;
            else
               exit;
            end if;
         end loop;
      end Insert_In_Tree;

   begin  --  Insert
      if Within_The_Tree.Root = null then
         Allocate
           (Key        => The_Key,
            Element    => With_Element,
            Left_Link  => null,
            Left_Type  => Thread,
            Right_Link => null,
            Right_Type => Thread,
            Balance    => 0,
            The_Node   => Within_The_Tree.Root);

         Within_The_Tree.Height := 1;
         Within_The_Tree.Nodes  := 1;
      else
         Insert_In_Tree;
      end if;
   end Insert;

   procedure Insert_Or_Replace
     (Within_The_Tree : in out T;
      The_Key         : in     Key_T;
      With_Element    : in     Element_T)
   is
      procedure Insert_Or_Replace_In_Tree;

      procedure Insert_Or_Replace_In_Tree
      is
         procedure Adjust_Balance;

         procedure Attach_Tree;

         PNode : AVL_Node_A := Within_The_Tree.Root;
         CNode : AVL_Node_A := Within_The_Tree.Root;
         NZRT  : AVL_Node_A := Within_The_Tree.Root;
         PNZRT : AVL_Node_A := null;

         procedure Adjust_Balance
         is
         begin
            loop
               --  While moving down the tree towards the new node,
               --  adjust the balance factors along the way.
               if Is_Key_Less (The_Key, CNode) then
                  --  The left subtree of the node has become
                  --  heavy due to the new addition.
                  CNode.all.Balance := -1;
                  CNode             := Link (CNode, Left);
               elsif Is_Node_Less (CNode, The_Key) then
                  --  The right subtree of the node has become
                  --  heavy due to the new addition.
                  CNode.all.Balance := +1;
                  CNode             := Link (CNode, Right);
               else
                  --  CNode is again equal to the new node.
                  exit;
               end if;
            end loop;
         end Adjust_Balance;

         procedure Attach_Tree
         is
         begin
            if PNZRT = null then
               Within_The_Tree.Root := CNode;
            elsif Link (PNZRT, Left) = NZRT then
               Set_Link (PNZRT, Left, CNode);
            else
               PNZRT.all.RL := CNode;
            end if;
         end Attach_Tree;

      begin  --  Insert_Or_Replace_In_Tree
         loop
            if Is_Key_Less (The_Key, CNode) then
               case Link_Type (CNode, Left) is
                  when Child =>
                     CNode := Link (CNode, Left);

                     if CNode.all.Balance /= 0 then
                        PNZRT := PNode;
                        NZRT  := CNode;
                     end if;

                     PNode := CNode;
                  when Thread =>
                     Allocate
                       (Key        => The_Key,
                        Element    => With_Element,
                        Left_Link  => Link (CNode, Left),
                        Left_Type  => Thread,
                        Right_Link => CNode,
                        Right_Type => Thread,
                        Balance    => 0,
                        The_Node   => PNode);

                     Set_Link (CNode, Left, PNode);
                     Set_Link_Type (CNode, Left, Child);

                     if Is_Key_Less (The_Key, NZRT) then
                        CNode := Link (NZRT, Left);

                        Adjust_Balance;

                        case NZRT.all.Balance is
                           when +1 =>
                              NZRT.all.Balance := 0;
                           when 0 =>
                              NZRT.all.Balance       := -1;
                              Within_The_Tree.Height :=
                                Within_The_Tree.Height + 1;
                           when -1 =>
                              CNode := Link (NZRT, Left);

                              if CNode.all.Balance = -1 then
                                 case Link_Type (CNode, Right) is
                                    when Child =>
                                       Set_Link
                                         (NZRT, Left,
                                          Link (CNode, Right));
                                       CNode.all.RL := NZRT;
                                    when Thread =>
                                       Set_Link_Type
                                         (NZRT, Left, Thread);
                                       Set_Link_Type
                                         (CNode, Right, Child);
                                 end case;

                                 NZRT.all.Balance  := 0;
                                 CNode.all.Balance := 0;
                              else
                                 PNode        := CNode;
                                 CNode        := PNode.all.RL;
                                 PNode.all.RL := Link (CNode, Left);
                                 Set_Link (CNode, Left, PNode);

                                 case Link_Type (CNode, Right) is
                                    when Child =>
                                       Set_Link
                                         (NZRT, Left,
                                          Link (CNode, Right));
                                       CNode.all.RL := NZRT;
                                    when Thread =>
                                       Set_Link_Type
                                         (CNode, Right, Child);
                                       Set_Link (NZRT, Left, CNode);
                                       Set_Link_Type
                                         (NZRT, Left, Thread);
                                 end case;

                                 if CNode.all.Balance = -1 then
                                    NZRT.all.Balance  := +1;
                                    CNode.all.Balance := 0;
                                    PNode.all.Balance := 0;
                                 else
                                    NZRT.all.Balance  := 0;
                                    CNode.all.Balance := 0;
                                    PNode.all.Balance := -1;
                                 end if;
                              end if;

                              Attach_Tree;
                        end case;
                     else
                        CNode := NZRT.all.RL;

                        Adjust_Balance;

                        case NZRT.all.Balance is
                           when -1 =>
                              NZRT.all.Balance := 0;
                           when 0 =>
                              NZRT.all.Balance       := +1;
                              Within_The_Tree.Height :=
                                Within_The_Tree.Height + 1;
                           when +1 =>
                              CNode := NZRT.all.RL;

                              if CNode.all.Balance = +1 then
                                 NZRT.all.RL := Link (CNode, Left);
                                 Set_Link (CNode, Left, NZRT);
                                 NZRT.all.Balance  := 0;
                                 CNode.all.Balance := 0;
                              else
                                 PNode := CNode;
                                 CNode := Link (PNode, Left);

                                 case Link_Type (CNode, Right) is
                                    when Child =>
                                       Set_Link
                                         (PNode, Left,
                                          Link (CNode, Right));
                                       CNode.all.RL := PNode;
                                    when Thread =>
                                       Set_Link_Type
                                         (CNode, Right, Child);
                                       Set_Link_Type
                                         (PNode, Left, Thread);
                                 end case;

                                 case Link_Type (CNode, Left) is
                                    when Child =>
                                       NZRT.all.RL :=
                                         Link (CNode, Left);
                                       Set_Link (CNode, Left, NZRT);
                                    when Thread =>
                                       Set_Link_Type
                                         (CNode, Left, Child);
                                       NZRT.all.RL := CNode;
                                       Set_Link_Type
                                         (NZRT, Right, Thread);
                                 end case;

                                 case CNode.all.Balance is
                                    when +1 =>
                                       NZRT.all.Balance  := -1;
                                       CNode.all.Balance := 0;
                                       PNode.all.Balance := 0;
                                    when 0 =>
                                       NZRT.all.Balance  := 0;
                                       PNode.all.Balance := 0;
                                    when -1 =>
                                       NZRT.all.Balance  := 0;
                                       CNode.all.Balance := 0;
                                       PNode.all.Balance := +1;
                                 end case;
                              end if;

                              Attach_Tree;
                        end case;
                     end if;

                     Within_The_Tree.Nodes :=
                       Within_The_Tree.Nodes + 1;
                     exit;
               end case;
            elsif Is_Node_Less (CNode, The_Key) then
               case Link_Type (CNode, Right) is
                  when Child =>
                     CNode := Link (CNode, Right);

                     if CNode.all.Balance /= 0 then
                        PNZRT := PNode;
                        NZRT  := CNode;
                     end if;

                     PNode := CNode;
                  when Thread =>
                     Allocate
                       (Key        => The_Key,
                        Element    => With_Element,
                        Left_Link  => CNode,
                        Left_Type  => Thread,
                        Right_Link => CNode.all.RL,
                        Right_Type => Thread,
                        Balance    => 0,
                        The_Node   => PNode);

                     CNode.all.RL := PNode;
                     Set_Link_Type (CNode, Right, Child);

                     if Is_Key_Less (The_Key, NZRT) then
                        CNode := Link (NZRT, Left);

                        Adjust_Balance;

                        case NZRT.all.Balance is
                           when +1 =>
                              NZRT.all.Balance := 0;
                           when 0 =>
                              NZRT.all.Balance       := -1;
                              Within_The_Tree.Height :=
                                Within_The_Tree.Height + 1;
                           when -1 =>
                              CNode := Link (NZRT, Left);

                              if CNode.all.Balance = -1 then
                                 Set_Link
                                   (NZRT, Left, Link (CNode, Right));
                                 CNode.all.RL      := NZRT;
                                 NZRT.all.Balance  := 0;
                                 CNode.all.Balance := 0;
                              else
                                 PNode := CNode;
                                 CNode := PNode.all.RL;

                                 case Link_Type (CNode, Left) is
                                    when Child =>
                                       PNode.all.RL :=
                                         Link (CNode, Left);
                                       Set_Link (CNode, Left, PNode);
                                    when Thread =>
                                       Set_Link_Type
                                         (CNode, Left, Child);
                                       Set_Link_Type
                                         (PNode, Right, Thread);
                                 end case;

                                 case Link_Type (CNode, Right) is
                                    when Child =>
                                       Set_Link
                                         (NZRT, Left,
                                          Link (CNode, Right));
                                       CNode.all.RL := NZRT;
                                    when Thread =>
                                       Set_Link_Type
                                         (CNode, Right, Child);
                                       Set_Link (NZRT, Left, CNode);
                                       Set_Link_Type
                                         (NZRT, Left, Thread);
                                 end case;

                                 case CNode.all.Balance is
                                    when -1 =>
                                       NZRT.all.Balance  := +1;
                                       CNode.all.Balance := 0;
                                       PNode.all.Balance := 0;
                                    when 0 =>
                                       NZRT.all.Balance  := 0;
                                       PNode.all.Balance := 0;
                                    when +1 =>
                                       NZRT.all.Balance  := 0;
                                       CNode.all.Balance := 0;
                                       PNode.all.Balance := -1;
                                 end case;
                              end if;

                              Attach_Tree;
                        end case;
                     else
                        CNode := NZRT.all.RL;

                        Adjust_Balance;

                        case NZRT.all.Balance is
                           when -1 =>
                              NZRT.all.Balance := 0;
                           when 0 =>
                              NZRT.all.Balance       := +1;
                              Within_The_Tree.Height :=
                                Within_The_Tree.Height + 1;
                           when +1 =>
                              CNode := NZRT.all.RL;

                              if CNode.all.Balance = +1 then
                                 case Link_Type (CNode, Left) is
                                    when Child =>
                                       NZRT.all.RL :=
                                         Link (CNode, Left);
                                       Set_Link (CNode, Left, NZRT);
                                    when Thread =>
                                       Set_Link_Type
                                         (NZRT, Right, Thread);
                                       Set_Link_Type
                                         (CNode, Left, Child);
                                 end case;

                                 NZRT.all.Balance  := 0;
                                 CNode.all.Balance := 0;
                              else
                                 PNode := CNode;
                                 CNode := Link (PNode, Left);
                                 Set_Link
                                   (PNode, Left, Link (CNode, Right));
                                 CNode.all.RL := PNode;

                                 case Link_Type (CNode, Left) is
                                    when Child =>
                                       NZRT.all.RL :=
                                         Link (CNode, Left);
                                       Set_Link (CNode, Left, NZRT);
                                    when Thread =>
                                       Set_Link_Type
                                         (CNode, Left, Child);
                                       NZRT.all.RL := CNode;
                                       Set_Link_Type
                                         (NZRT, Right, Thread);
                                 end case;

                                 if CNode.all.Balance = +1 then
                                    NZRT.all.Balance  := -1;
                                    CNode.all.Balance := 0;
                                    PNode.all.Balance := 0;
                                 else
                                    NZRT.all.Balance  := 0;
                                    CNode.all.Balance := 0;
                                    PNode.all.Balance := +1;
                                 end if;
                              end if;

                              Attach_Tree;
                        end case;
                     end if;

                     Within_The_Tree.Nodes :=
                       Within_The_Tree.Nodes + 1;
                     exit;
               end case;
            else
               Replace_Element (CNode, With_Element);
               exit;
            end if;
         end loop;
      end Insert_Or_Replace_In_Tree;

   begin  --  Insert_Or_Replace
      if Within_The_Tree.Root = null then
         Allocate
           (Key        => The_Key,
            Element    => With_Element,
            Left_Link  => null,
            Left_Type  => Thread,
            Right_Link => null,
            Right_Type => Thread,
            Balance    => 0,
            The_Node   => Within_The_Tree.Root);

         Within_The_Tree.Height := 1;
         Within_The_Tree.Nodes  := 1;
      else
         Insert_Or_Replace_In_Tree;
      end if;
   end Insert_Or_Replace;

   function Is_Empty
     (The_Tree : in T)
      return Boolean
   is
   begin
      return The_Tree.Nodes = 0;
   end Is_Empty;

   function Is_Equal_To
     (X : in Key_T;
      Y : in Key_T)
      return Boolean
   is
      Result : Boolean := True;
   begin
      if Is_Less_Than (X => X, Y => Y)
         or else
         Is_Less_Than (X => Y, Y => X)
      then
         Result := False;
      end if;

      return Result;
   end Is_Equal_To;

   function Is_Key_Equal
     (X : in Key_T;
      Y : in AVL_Node_A)
      return Boolean
   is
   begin
      return Is_Equal_To (X, Key_Of (Y));
   end Is_Key_Equal;

   function Is_Key_Less
     (X : in Key_T;
      Y : in AVL_Node_A)
      return Boolean
   is
   begin
      return Is_Less_Than (X, Key_Of (Y));
   end Is_Key_Less;

   function Is_Node_Less
     (X : in AVL_Node_A;
      Y : in Key_T)
      return Boolean
   is
   begin
      return Is_Less_Than (Key_Of (X), Y);
   end Is_Node_Less;

   function Is_Present
     (Within_The_Tree : in T;
      The_Key         : in Key_T)
      return Boolean
   is
   begin
      return Node (Within_The_Tree, The_Key) /= null;
   end Is_Present;

   function Key_Of
     (The_Node : in AVL_Node_A)
      return Key_T
   is
   begin
      pragma Assert (The_Node /= null);
      return The_Node.all.Key;
   end Key_Of;

   function Least_Key
     (Within_The_Tree : in T)
      return Key_T
   is
      Node_Pointer : constant AVL_Node_A :=
        Least_Node (Within_The_Tree);
   begin
      if Node_Pointer = null then
         raise Key_Not_Found;
      end if;

      return Key_Of (The_Node => Node_Pointer);
   end Least_Key;

   function Least_Node
     (Within_The_Tree : in T)
      return AVL_Node_A
   is
      Node_Pointer : AVL_Node_A := Within_The_Tree.Root;
   begin
      if Node_Pointer /= null then
         while Link_Type (Node_Pointer, Left) = Child loop
            Node_Pointer := Link (Node_Pointer, Left);
         end loop;
      end if;

      return Node_Pointer;
   end Least_Node;

   function Link
     (Of_Node   : in AVL_Node_A;
      Direction : in Direction_T)
      return AVL_Node_A
   is
      pragma Warnings (Off);
      function Booleans_To_Access is new Unchecked_Conversion
        (Source => Address_Booleans_T, Target => AVL_Node_A);
      pragma Warnings (On);

      Result : AVL_Node_A;
   begin
      pragma Assert (Of_Node /= null);

      case Direction is
         when Left =>
            Result :=
              Booleans_To_Access
                (Address_To_Booleans (Of_Node.all.Left_Link)
                 and Address_Mask);
         when Right =>
            Result := Of_Node.all.RL;
      end case;

      return Result;
   end Link;

   function Link_Type
     (Of_Node   : in AVL_Node_A;
      Direction : in Direction_T)
      return Link_T
   is
      Address_Booleans : Address_Booleans_T;
      Bit              : Address_Index_T;
      Result           : Link_T;
   begin
      pragma Assert (Of_Node /= null);

      Address_Booleans := Address_To_Booleans (Of_Node.all.Left_Link);

      case Direction is
         when Left =>
            Bit := 1;
         when Right =>
            Bit := 2;
      end case;

      case Address_Booleans (Bit) is
         when False =>
            Result := Thread;
         when True =>
            Result := Child;
      end case;

      return Result;
   end Link_Type;

   function Node
     (Within_The_Tree : in T;
      That_Has_Key    : in Key_T)
      return AVL_Node_A
   is
      Node_Pointer : AVL_Node_A := Within_The_Tree.Root;
   begin
      if Node_Pointer /= null then
         loop
            if Is_Key_Less (That_Has_Key, Node_Pointer) then
               if Link_Type (Node_Pointer, Left) = Child then
                  Node_Pointer := Link (Node_Pointer, Left);
               else
                  Node_Pointer := null;
                  exit;
               end if;
            elsif Is_Node_Less (Node_Pointer, That_Has_Key) then
               if Link_Type (Node_Pointer, Right) = Child then
                  Node_Pointer := Link (Node_Pointer, Right);
               else
                  Node_Pointer := null;
                  exit;
               end if;
            else
               exit;
            end if;
         end loop;
      end if;

      return Node_Pointer;
   end Node;

   function Nodes
     (Within_The_Tree : in T)
      return Natural
   is
   begin
      return Within_The_Tree.Nodes;
   end Nodes;

   function Opposite
     (Direction : in Direction_T)
      return Direction_T
   is
      Result : Direction_T;
   begin
      case Direction is
         when Left =>
            Result := Right;
         when Right =>
            Result := Left;
      end case;

      return Result;
   end Opposite;

   function Parent
     (Of_Node : in AVL_Node_A)
      return AVL_Node_A
   is
      --  Place the result in LNode, avoiding the use of another
      --  variable or multiple return statements.
      LNode : AVL_Node_A := Of_Node;
      RNode : AVL_Node_A := Of_Node;
   begin
      --  The chain of children to the left or right may end far
      --  sooner than that of the opposite chain, so stop as soon
      --  as possible to avoid unnecessary effort.
      loop
         if Link_Type (LNode, Left) = Thread then
            LNode := Link (LNode, Left);

            if LNode = null or else LNode.all.RL /= Of_Node then
               while Link_Type (RNode, Right) = Child loop
                  RNode := RNode.all.RL;
               end loop;

               LNode := RNode.all.RL;
            end if;

            exit;
         end if;

         if Link_Type (RNode, Right) = Thread then
            RNode := RNode.all.RL;

            if RNode = null or else Link (RNode, Left) /= Of_Node then
               while Link_Type (LNode, Left) = Child loop
                  LNode := Link (LNode, Left);
               end loop;

               --  These two breaks in symmetry are to satisfy the
               --  above demand that the result be placed in LNode.
               LNode := Link (LNode, Left);
            else
               LNode := RNode;
            end if;

            exit;
         end if;

         LNode := Link (LNode, Left);
         RNode := RNode.all.RL;
      end loop;

      return LNode;
   end Parent;

   function Predecessor_Key
     (Within_The_Tree : in T;
      Of_The_Key      : in Key_T)
      return Key_T
   is
      Node_Pointer : AVL_Node_A := Node (Within_The_Tree, Of_The_Key);
   begin
      if Node_Pointer = null then
         raise Key_Not_Found;
      else
         case Link_Type (Node_Pointer, Left) is
            when Child =>
               Node_Pointer := Link (Node_Pointer, Left);

               while Link_Type (Node_Pointer, Right) /= Thread loop
                  Node_Pointer := Link (Node_Pointer, Right);
               end loop;

               return Key_Of (Node_Pointer);
            when Thread =>
               if Link (Node_Pointer, Left) = null then
                  raise Key_Not_Found;
               end if;

               Node_Pointer := Link (Node_Pointer, Left);

               return Key_Of (Node_Pointer);
         end case;
      end if;
   end Predecessor_Key;

   procedure Remove
     (From_The_Tree : in out T;
      The_Key       : in     Key_T)
   is
   begin
      if From_The_Tree.Root /= null then
         if Is_Key_Equal (The_Key, From_The_Tree.Root) then
            case Link_Type (From_The_Tree.Root, Left) is
               when Child =>
                  case Link_Type (From_The_Tree.Root, Right) is
                     when Child =>
                        if From_The_Tree.Root.all.Balance = -1 then
                           if Link_Type
                               (Link (From_The_Tree.Root, Left), Right)
                             = Thread
                           then
                              Remove_Root_LCRC_LCT
                                (Head => From_The_Tree);
                           elsif Link_Type
                               (From_The_Tree.Root.all.RL, Left)
                             = Thread
                           then
                              Remove_Root_LCRC_RCT
                                (Head => From_The_Tree);
                           else
                              Remove_Root_LCRC_Predecessor
                                (Head => From_The_Tree);
                           end if;
                        else
                           --  In a completely arbitrary fashion,
                           --  check the successor first.
                           if Link_Type
                               (From_The_Tree.Root.all.RL, Left)
                             = Thread
                           then
                              Remove_Root_LCRC_RCT
                                (Head => From_The_Tree);
                           elsif Link_Type
                               (Link (From_The_Tree.Root, Left), Right)
                             = Thread
                           then
                              Remove_Root_LCRC_LCT
                                (Head => From_The_Tree);
                           else
                              Remove_Root_LCRC_Successor
                                (Head => From_The_Tree);
                           end if;
                        end if;
                     when Thread =>
                        Remove_Root_LCRT (Head => From_The_Tree);
                  end case;
               when Thread =>
                  case Link_Type (From_The_Tree.Root, Right) is
                     when Child =>
                        Remove_Root_LTRC (Head => From_The_Tree);
                     when Thread =>
                        Deallocate (From_The_Tree.Root);
                        From_The_Tree.Height := 0;
                  end case;
            end case;

            From_The_Tree.Nodes := From_The_Tree.Nodes - 1;
         else
            Remove_Node
              (Key  => The_Key,
               Head => From_The_Tree);
         end if;
      end if;
   end Remove;

   procedure Remove_Node
     (Key  : in     Key_T;
      Head : in out T)
   is
      PNode : AVL_Node_A := null;
      DNode : AVL_Node_A := Head.Root;
   begin
      loop
         if Is_Key_Less (Key, DNode) then
            if Link_Type (DNode, Left) = Thread then
               raise Key_Not_Found;
            end if;

            PNode := DNode;
            DNode := Link (DNode, Left);
         elsif Is_Node_Less (DNode, Key) then
            if Link_Type (DNode, Right) = Thread then
               raise Key_Not_Found;
            end if;

            PNode := DNode;
            DNode := DNode.all.RL;
         else
            case Link_Type (DNode, Left) is
               when Child =>
                  case Link_Type (DNode, Right) is
                     when Child =>
                        if DNode.all.Balance = -1 then
                           if Link_Type (Link (DNode, Left), Right)
                             = Thread
                           then
                              Remove_Node_LCRC_LCT
                                (PNode => PNode,
                                 DNode => DNode,
                                 Head  => Head);
                           elsif Link_Type (DNode.all.RL, Left)
                             = Thread
                           then
                              Remove_Node_LCRC_RCT
                                (PNode => PNode,
                                 DNode => DNode,
                                 Head  => Head);
                           else
                              Remove_Node_LCRC_Predecessor
                                (PNode => PNode,
                                 DNode => DNode,
                                 Head  => Head);
                           end if;
                        else
                           --  In a completely arbitrary fashion,
                           --  check the successor first.
                           if Link_Type (DNode.all.RL, Left) = Thread
                           then
                              Remove_Node_LCRC_RCT
                                (PNode => PNode,
                                 DNode => DNode,
                                 Head  => Head);
                           elsif Link_Type (Link (DNode, Left), Right)
                             = Thread
                           then
                              Remove_Node_LCRC_LCT
                                (PNode => PNode,
                                 DNode => DNode,
                                 Head  => Head);
                           else
                              Remove_Node_LCRC_Successor
                                (PNode => PNode,
                                 DNode => DNode,
                                 Head  => Head);
                           end if;
                        end if;
                     when Thread =>
                        Remove_Node_LCRT
                          (PNode => PNode,
                           DNode => DNode,
                           Head  => Head);
                  end case;
               when Thread =>
                  case Link_Type (DNode, Right) is
                     when Child =>
                        Remove_Node_LTRC
                          (PNode => PNode,
                           DNode => DNode,
                           Head  => Head);
                     when Thread =>
                        --  If DNode is not root and only has threads
                        --  as links, then PNode will inherit the
                        --  thread from the DNode that is of the same
                        --  direction as the PNode - DNode link.
                        if Link (PNode, Left) = DNode then
                           Set_Link (PNode, Left, Link (DNode, Left));
                           Set_Link_Type (PNode, Left, Thread);

                           Deallocate (The_Node => DNode);

                           Balance_Post_Deletion
                             (Direction   => Left,
                              Parent_Node => PNode,
                              Head        => Head);
                        else
                           PNode.all.RL := DNode.all.RL;
                           Set_Link_Type (PNode, Right, Thread);

                           Deallocate (The_Node => DNode);

                           Balance_Post_Deletion
                             (Direction   => Right,
                              Parent_Node => PNode,
                              Head        => Head);
                        end if;
                  end case;
            end case;

            Head.Nodes := Head.Nodes - 1;
            exit;
         end if;
      end loop;
   end Remove_Node;

   procedure Remove_Node_LCRC_LCT
     (PNode : in     AVL_Node_A;
      DNode : in out AVL_Node_A;
      Head  : in out T)
   is
      RNode : AVL_Node_A := Link (DNode, Left);
   begin
      RNode.all.RL := DNode.all.RL;
      Set_Link_Type (RNode, Right, Child);

      RNode := RNode.all.RL;

      while Link_Type (RNode, Left) = Child loop
         RNode := Link (RNode, Left);
      end loop;

      Set_Link (RNode, Left, Link (DNode, Left));
      RNode             := Link (DNode, Left);
      RNode.all.Balance := DNode.all.Balance;

      Set_Link (DNode, Left, null);
      DNode.all.RL := null;

      if Link (PNode, Left) = DNode then
         Set_Link (PNode, Left, RNode);
      else
         PNode.all.RL := RNode;
      end if;

      Deallocate (The_Node => DNode);

      Balance_Post_Deletion
        (Direction   => Left,
         Parent_Node => RNode,
         Head        => Head);
   end Remove_Node_LCRC_LCT;

   procedure Remove_Node_LCRC_Predecessor
     (PNode : in     AVL_Node_A;
      DNode : in out AVL_Node_A;
      Head  : in out T)
   is
      --  Parent of the replacement node
      QNode : AVL_Node_A := DNode;
      --  Replacement node
      RNode : AVL_Node_A := Link (DNode, Left);
   begin
      --  Start at the node to the left of the root and keep moving
      --  right to find the ceiling node.
      while Link_Type (RNode, Right) = Child loop
         QNode := RNode;
         RNode := RNode.all.RL;
      end loop;

      RNode := DNode.all.RL;

      while Link_Type (RNode, Left) = Child loop
         RNode := Link (RNode, Left);
      end loop;

      Set_Link (RNode, Left, Link (QNode, Right));
      RNode := QNode.all.RL;

      case Link_Type (RNode, Left) is
         when Child =>
            QNode.all.RL := Link (RNode, Left);
         when Thread =>
            Set_Link_Type (QNode, Right, Thread);
            Set_Link_Type (RNode, Left, Child);
      end case;

      Set_Link (RNode, Left, Link (DNode, Left));
      RNode.all.RL := DNode.all.RL;
      Set_Link_Type (RNode, Right, Child);

      DNode.all.RL := null;
      Set_Link (DNode, Left, null);

      if Link (PNode, Left) = DNode then
         Set_Link (PNode, Left, RNode);
      else
         PNode.all.RL := RNode;
      end if;

      RNode.all.Balance := DNode.all.Balance;

      Deallocate (The_Node => DNode);

      Balance_Post_Deletion
        (Direction   => Right,
         Parent_Node => QNode,
         Head        => Head);
   end Remove_Node_LCRC_Predecessor;

   procedure Remove_Node_LCRC_RCT
     (PNode : in     AVL_Node_A;
      DNode : in out AVL_Node_A;
      Head  : in out T)
   is
      RNode : AVL_Node_A := DNode.all.RL;
   begin
      Set_Link (RNode, Left, Link (DNode, Left));
      Set_Link_Type (RNode, Left, Child);

      RNode := Link (RNode, Left);

      while Link_Type (RNode, Right) = Child loop
         RNode := RNode.all.RL;
      end loop;

      RNode.all.RL      := DNode.all.RL;
      RNode             := DNode.all.RL;
      RNode.all.Balance := DNode.all.Balance;

      DNode.all.RL := null;
      Set_Link (DNode, Left, null);

      if PNode.all.RL = DNode then
         PNode.all.RL := RNode;
      else
         Set_Link (PNode, Left, RNode);
      end if;

      Deallocate (The_Node => DNode);

      Balance_Post_Deletion
        (Direction   => Right,
         Parent_Node => RNode,
         Head        => Head);
   end Remove_Node_LCRC_RCT;

   procedure Remove_Node_LCRC_Successor
     (PNode : in     AVL_Node_A;
      DNode : in out AVL_Node_A;
      Head  : in out T)
   is
   --  Parent of the replacement node
      QNode : AVL_Node_A := DNode;
      --  Replacement node
      RNode : AVL_Node_A := DNode.all.RL;
   begin
      --  Start at the node to the right of the root and keep moving
      --  left to find the floor node.
      while Link_Type (RNode, Left) = Child loop
         QNode := RNode;
         RNode := Link (RNode, Left);
      end loop;

      RNode := Link (DNode, Left);

      while Link_Type (RNode, Right) = Child loop
         RNode := RNode.all.RL;
      end loop;

      RNode.all.RL := Link (QNode, Left);
      RNode        := Link (QNode, Left);

      case Link_Type (RNode, Right) is
         when Child =>
            Set_Link (QNode, Left, Link (RNode, Right));
         when Thread =>
            Set_Link_Type (QNode, Left, Thread);
            Set_Link_Type (RNode, Right, Child);
      end case;

      RNode.all.RL := DNode.all.RL;
      Set_Link (RNode, Left, Link (DNode, Left));
      Set_Link_Type (RNode, Left, Child);

      Set_Link (DNode, Left, null);
      DNode.all.RL := null;

      if PNode.all.RL = DNode then
         PNode.all.RL := RNode;
      else
         Set_Link (PNode, Left, RNode);
      end if;

      RNode.all.Balance := DNode.all.Balance;

      Deallocate (The_Node => DNode);

      Balance_Post_Deletion
        (Direction   => Left,
         Parent_Node => QNode,
         Head        => Head);
   end Remove_Node_LCRC_Successor;

   procedure Remove_Node_LCRT
     (PNode : in     AVL_Node_A;
      DNode : in out AVL_Node_A;
      Head  : in out T)
   is
      RNode : AVL_Node_A := Link (DNode, Left);
   begin
      case Link_Type (RNode, Right) is
         when Child =>
            while Link_Type (RNode, Right) = Child loop
               RNode := RNode.all.RL;
            end loop;

            --  Set the right link of this node, a thread,
            --  to point to the same node as the right link
            --  of the DNode (also a thread).
            RNode.all.RL := DNode.all.RL;
            RNode        := Link (DNode, Left);
         when Thread =>
            RNode.all.RL := DNode.all.RL;
      end case;

      DNode.all.RL := null;
      Set_Link (DNode, Left, null);

      if PNode.all.RL = DNode then
         --  PNode's left link now points to the left
         --  child of the DNode.
         PNode.all.RL := RNode;
      else
         Set_Link (PNode, Left, RNode);
      end if;

      RNode.all.Balance := DNode.all.Balance;

      Deallocate (The_Node => DNode);

      Balance_Post_Deletion
        (Direction   => Left,
         Parent_Node => RNode,
         Head        => Head);
   end Remove_Node_LCRT;

   procedure Remove_Node_LTRC
     (PNode : in     AVL_Node_A;
      DNode : in out AVL_Node_A;
      Head  : in out T)
   is
      RNode : AVL_Node_A := DNode.all.RL;
   begin
      case Link_Type (RNode, Left) is
         when Child =>
            while Link_Type (RNode, Left) = Child loop
               RNode := Link (RNode, Left);
            end loop;

            Set_Link (RNode, Left, Link (DNode, Left));
            RNode := DNode.all.RL;
         when Thread =>
            Set_Link (RNode, Left, Link (DNode, Left));
      end case;

      Set_Link (DNode, Left, null);
      DNode.all.RL := null;

      if Link (PNode, Left) = DNode then
         Set_Link (PNode, Left, RNode);
      else
         PNode.all.RL := RNode;
      end if;

      RNode.all.Balance := DNode.all.Balance;

      Deallocate (The_Node => DNode);

      Balance_Post_Deletion
        (Direction   => Right,
         Parent_Node => RNode,
         Head        => Head);
   end Remove_Node_LTRC;

   procedure Remove_Root_LCRC_LCT
     (Head : in out T)
   is
      RNode : AVL_Node_A := Link (Head.Root, Left);
   begin
      RNode.all.RL := Head.Root.all.RL;
      Set_Link_Type (RNode, Right, Child);

      RNode := RNode.all.RL;

      while Link_Type (RNode, Left) = Child loop
         RNode := Link (RNode, Left);
      end loop;

      Set_Link (RNode, Left, Link (Head.Root, Left));
      RNode             := Link (Head.Root, Left);
      RNode.all.Balance := Head.Root.all.Balance;

      Set_Link (Head.Root, Left, null);
      Head.Root.all.RL := null;

      Deallocate (The_Node => Head.Root);

      Head.Root := RNode;

      Balance_Post_Deletion
        (Direction   => Left,
         Parent_Node => RNode,
         Head        => Head);
   end Remove_Root_LCRC_LCT;

   procedure Remove_Root_LCRC_Predecessor
     (Head : in out T)
   is
      --  Parent of the replacement node
      PNode : AVL_Node_A := Head.Root;
      --  Replacement node
      RNode : AVL_Node_A := Link (Head.Root, Left);
   begin
      --  Start at the node to the left of the root and keep moving
      --  right to find the ceiling node.
      while Link_Type (RNode, Right) = Child loop
         PNode := RNode;
         RNode := RNode.all.RL;
      end loop;

      RNode := Head.Root.all.RL;

      while Link_Type (RNode, Left) = Child loop
         RNode := Link (RNode, Left);
      end loop;

      Set_Link (RNode, Left, PNode.all.RL);
      RNode := PNode.all.RL;

      case Link_Type (RNode, Left) is
         when Child =>
            PNode.all.RL := Link (RNode, Left);
         when Thread =>
            Set_Link_Type (PNode, Right, Thread);
            Set_Link_Type (RNode, Left, Child);
      end case;

      Set_Link (RNode, Left, Link (Head.Root, Left));
      RNode.all.RL := Head.Root.all.RL;
      Set_Link_Type (RNode, Right, Child);
      RNode.all.Balance := Head.Root.all.Balance;

      Head.Root.all.RL := null;
      Set_Link (Head.Root, Left, null);

      Deallocate (The_Node => Head.Root);

      Head.Root := RNode;

      Balance_Post_Deletion
        (Direction   => Right,
         Parent_Node => PNode,
         Head        => Head);
   end Remove_Root_LCRC_Predecessor;

   procedure Remove_Root_LCRC_RCT
     (Head : in out T)
   is
      RNode : AVL_Node_A := Head.Root.all.RL;
   begin
      Set_Link (RNode, Left, Link (Head.Root, Left));
      Set_Link_Type (RNode, Left, Child);

      RNode := Link (RNode, Left);

      while Link_Type (RNode, Right) = Child loop
         RNode := RNode.all.RL;
      end loop;

      RNode.all.RL      := Head.Root.all.RL;
      RNode             := Head.Root.all.RL;
      RNode.all.Balance := Head.Root.all.Balance;

      Head.Root.all.RL := null;
      Set_Link (Head.Root, Left, null);

      Deallocate (The_Node => Head.Root);

      Head.Root := RNode;

      Balance_Post_Deletion
        (Direction   => Right,
         Parent_Node => RNode,
         Head        => Head);
   end Remove_Root_LCRC_RCT;

   procedure Remove_Root_LCRC_Successor
     (Head : in out T)
   is
      --  Parent of the replacement node
      PNode : AVL_Node_A := Head.Root;
      --  Replacement node
      RNode : AVL_Node_A := Head.Root.all.RL;
   begin
      --  Start at the node to the right of the root and keep moving
      --  left to find the floor node.
      while Link_Type (RNode, Left) = Child loop
         PNode := RNode;
         RNode := Link (RNode, Left);
      end loop;

      RNode := Link (Head.Root, Left);

      while Link_Type (RNode, Right) = Child loop
         RNode := RNode.all.RL;
      end loop;

      RNode.all.RL := Link (PNode, Left);
      RNode        := Link (PNode, Left);

      case Link_Type (RNode, Right) is
         when Child =>
            Set_Link (PNode, Left, Link (RNode, Right));
         when Thread =>
            Set_Link_Type (PNode, Left, Thread);
            Set_Link_Type (RNode, Right, Child);
      end case;

      RNode.all.RL := Head.Root.all.RL;
      Set_Link (RNode, Left, Link (Head.Root, Left));
      Set_Link_Type (RNode, Left, Child);

      Set_Link (Head.Root, Left, null);
      Head.Root.all.RL := null;

      RNode.all.Balance := Head.Root.all.Balance;

      Deallocate (The_Node => Head.Root);

      Head.Root := RNode;

      Balance_Post_Deletion
        (Direction   => Left,
         Parent_Node => PNode,
         Head        => Head);
   end Remove_Root_LCRC_Successor;

   procedure Remove_Root_LCRT
     (Head : in out T)
   is
      --  First, find the descendant of DNode with a thread on the
      --  right.  This thread points to DNode. Begin the search at
      --  left child of DNode.
      DNode : AVL_Node_A := Link (Head.Root, Left);
   begin
      case Link_Type (DNode, Right) is
         when Child =>
            while Link_Type (DNode, Right) = Child loop
               DNode := DNode.all.RL;
            end loop;

            DNode.all.RL := null;
            DNode        := Link (Head.Root, Left);
         when Thread =>
            DNode.all.RL := null;
      end case;

      Set_Link (Head.Root, Left, null);

      Deallocate (The_Node => Head.Root);

      Head.Root   := DNode;
      Head.Height := Head.Height - 1;
   end Remove_Root_LCRT;

   procedure Remove_Root_LTRC
     (Head : in out T)
   is
      --  First, find the descendant of DNode with a thread on the
      --  left.  This thread points to DNode.  Begin the search at
      --  the left child of DNode.
      DNode : AVL_Node_A := Head.Root.all.RL;
   begin
      case Link_Type (DNode, Left) is
         when Child =>
            while Link_Type (DNode, Left) = Child loop
               DNode := Link (DNode, Left);
            end loop;

            Set_Link (DNode, Left, null);
            DNode := Head.Root.all.RL;
         when Thread =>
            Set_Link (DNode, Left, null);
      end case;

      Head.Root.all.RL := null;

      Deallocate (The_Node => Head.Root);

      Head.Root   := DNode;
      Head.Height := Head.Height - 1;
   end Remove_Root_LTRC;

   procedure Replace
     (Within_The_Tree : in out T;
      The_Key         : in     Key_T;
      With_Element    : in     Element_T)
   is
      Node_Pointer : constant AVL_Node_A :=
        Node (Within_The_Tree, The_Key);
   begin
      if Node_Pointer = null then
         raise Key_Not_Found;
      else
         Replace_Element (Node_Pointer, With_Element);
      end if;
   end Replace;

   procedure Replace_Element
     (Within_The_Node  : in AVL_Node_A;
      With_The_Element : in Element_T)
   is
   begin
      pragma Assert (Within_The_Node /= null);
      Within_The_Node.all.Element := With_The_Element;
   end Replace_Element;

   procedure Replace_Greatest
     (Within_The_Tree : in out T;
      With_Element    : in     Element_T)
   is
      Node_Pointer : AVL_Node_A := Within_The_Tree.Root;
   begin
      if Node_Pointer = null then
         raise Key_Not_Found;
      end if;

      while Link_Type (Node_Pointer, Right) = Child loop
         Node_Pointer := Link (Node_Pointer, Right);
      end loop;

      Replace_Element (Node_Pointer, With_Element);
   end Replace_Greatest;

   procedure Replace_Least
     (Within_The_Tree : in out T;
      With_Element    : in     Element_T)
   is
      Node_Pointer : constant AVL_Node_A :=
        Least_Node (Within_The_Tree);
   begin
      if Node_Pointer = null then
         raise Key_Not_Found;
      else
         Replace_Element (Node_Pointer, With_Element);
      end if;
   end Replace_Least;

   procedure Replace_Predecessor
     (Within_The_Tree : in out T;
      Of_The_Key      : in     Key_T;
      With_Element    : in     Element_T)
   is
      Node_Pointer : AVL_Node_A := Node (Within_The_Tree, Of_The_Key);
   begin
      if Node_Pointer = null then
         raise Key_Not_Found;
      else
         case Link_Type (Node_Pointer, Left) is
            when Child =>
               Node_Pointer := Link (Node_Pointer, Left);

               while Link_Type (Node_Pointer, Right) /= Thread loop
                  Node_Pointer := Link (Node_Pointer, Right);
               end loop;

               Replace_Element (Node_Pointer, With_Element);
            when Thread =>
               if Link (Node_Pointer, Left) /= null then
                  Node_Pointer := Link (Node_Pointer, Left);
                  Replace_Element (Node_Pointer, With_Element);
               end if;
         end case;
      end if;
   end Replace_Predecessor;

   procedure Replace_Successor
     (Within_The_Tree : in out T;
      Of_The_Key      : in     Key_T;
      With_Element    : in     Element_T)
   is
      Node_Pointer : AVL_Node_A := Node (Within_The_Tree, Of_The_Key);
   begin
      if Node_Pointer = null then
         raise Key_Not_Found;
      else
         Node_Pointer := Successor_Node (Node_Pointer);

         if Node_Pointer = null then
            raise Key_Not_Found;
         else
            Replace_Element (Node_Pointer, With_Element);
         end if;
      end if;
   end Replace_Successor;

   procedure Retrieve
     (From_The_Tree    : in     T;
      The_Key          : in     Key_T;
      Into_The_Element :    out Element_T)
   is
      Node_Pointer : constant AVL_Node_A :=
        Node (From_The_Tree, The_Key);
   begin
      if Node_Pointer = null then
         raise Key_Not_Found;
      else
         Into_The_Element := Element_Of (The_Node => Node_Pointer);
      end if;
   end Retrieve;

   procedure Retrieve_Greatest
     (From_The_Tree    : in     T;
      Into_The_Element :    out Element_T)
   is
      Node_Pointer : AVL_Node_A := From_The_Tree.Root;
   begin
      if Node_Pointer = null then
         raise Key_Not_Found;
      else
         while Link_Type (Node_Pointer, Right) = Child loop
            Node_Pointer := Link (Node_Pointer, Right);
         end loop;

         Into_The_Element := Element_Of (The_Node => Node_Pointer);
      end if;
   end Retrieve_Greatest;

   procedure Retrieve_Least
     (From_The_Tree    : in     T;
      Into_The_Element :    out Element_T)
   is
      Node_Pointer : constant AVL_Node_A := Least_Node (From_The_Tree);
   begin
      if Node_Pointer = null then
         raise Key_Not_Found;
      else
         Into_The_Element := Element_Of (The_Node => Node_Pointer);
      end if;
   end Retrieve_Least;

   procedure Retrieve_Predecessor
     (Within_The_Tree  : in     T;
      Of_The_Key       : in     Key_T;
      Into_The_Element : in out Element_T)
   is
      Node_Pointer : AVL_Node_A := Node (Within_The_Tree, Of_The_Key);
   begin
      if Node_Pointer = null then
         raise Key_Not_Found;
      else
         case Link_Type (Node_Pointer, Left) is
            when Child =>
               Node_Pointer := Link (Node_Pointer, Left);

               while Link_Type (Node_Pointer, Right) /= Thread loop
                  Node_Pointer := Link (Node_Pointer, Right);
               end loop;

               Into_The_Element :=
                 Element_Of (The_Node => Node_Pointer);
            when Thread =>
               if Link (Node_Pointer, Left) /= null then
                  Node_Pointer     := Link (Node_Pointer, Left);
                  Into_The_Element :=
                    Element_Of (The_Node => Node_Pointer);
               end if;
         end case;
      end if;
   end Retrieve_Predecessor;

   procedure Retrieve_Successor
     (Within_The_Tree  : in     T;
      Of_The_Key       : in     Key_T;
      Into_The_Element : in out Element_T)
   is
      Node_Pointer : AVL_Node_A := Node (Within_The_Tree, Of_The_Key);
   begin
      if Node_Pointer = null then
         raise Key_Not_Found;
      else
         Node_Pointer := Successor_Node (Node_Pointer);

         if Node_Pointer = null then
            raise Key_Not_Found;
         else
            Into_The_Element :=
              Element_Of (The_Node => Node_Pointer);
         end if;
      end if;
   end Retrieve_Successor;

   procedure Set_Link
     (Of_Node   : in AVL_Node_A;
      Direction : in Direction_T;
      To_Node   : in AVL_Node_A)
   is
   begin
      pragma Assert (Of_Node /= null);

      case Direction is
         when Left =>
            declare
               Saved : constant Address_Booleans_T :=
                 Types_Mask
                 and Address_To_Booleans (Of_Node.all.Left_Link);
            begin
               Of_Node.all.Left_Link :=
                 Booleans_To_Address
                   (Access_To_Booleans (To_Node) or Saved);
            end;
         when Right =>
            Of_Node.all.RL := To_Node;
      end case;
   end Set_Link;

   procedure Set_Link_Type
     (Of_Node   : in AVL_Node_A;
      Direction : in Direction_T;
      To_Type   : in Link_T)
   is
      Address_Booleans : Address_Booleans_T;
      Bit              : Address_Index_T;
   begin
      pragma Assert (Of_Node /= null);

      Address_Booleans := Address_To_Booleans (Of_Node.all.Left_Link);

      case Direction is
         when Left =>
            Bit := 1;
         when Right =>
            Bit := 2;
      end case;

      case To_Type is
         when Thread =>
            Address_Booleans (Bit) := False;
         when Child =>
            Address_Booleans (Bit) := True;
      end case;

      Of_Node.all.Left_Link := Booleans_To_Address (Address_Booleans);
   end Set_Link_Type;

   function Successor_Key
     (Within_The_Tree : in T;
      Of_The_Key      : in Key_T)
      return Key_T
   is
      Node_Pointer : AVL_Node_A := Node (Within_The_Tree, Of_The_Key);
   begin
      if Node_Pointer = null then
         --  Could not find the original key.
         raise Key_Not_Found;
      else
         Node_Pointer := Successor_Node (Node_Pointer);

         if Node_Pointer = null then
            --  Could not find the successor key.
            raise Key_Not_Found;
         end if;
      end if;

      return Key_Of (The_Node => Node_Pointer);
   end Successor_Key;

   function Successor_Node
     (Of_Node : in AVL_Node_A)
      return AVL_Node_A
   is
      Node_Pointer : AVL_Node_A := null;
   begin
      pragma Assert (Of_Node /= null);
      Node_Pointer := Of_Node;

      case Link_Type (Node_Pointer, Right) is
         when Child =>
            Node_Pointer := Link (Node_Pointer, Right);

            while Link_Type (Node_Pointer, Left) /= Thread loop
               Node_Pointer := Link (Node_Pointer, Left);
            end loop;
         when Thread =>
            Node_Pointer := Link (Node_Pointer, Right);
      end case;

      return Node_Pointer;
   end Successor_Node;

   procedure Swap
     (Within_The_Tree  : in out T;
      The_Key          : in     Key_T;
      With_The_Element : in out Element_T)
   is
      Swap_C       : constant Element_T  := With_The_Element;
      Node_Pointer : constant AVL_Node_A :=
        Node (Within_The_Tree, The_Key);
   begin
      if Node_Pointer = null then
         raise Key_Not_Found;
      else
         With_The_Element := Element_Of (The_Node => Node_Pointer);
         Replace_Element (Node_Pointer, Swap_C);
      end if;
   end Swap;

   procedure Swap_Greatest
     (Within_The_Tree  : in out T;
      With_The_Element : in out Element_T)
   is
      Swap_C       : constant Element_T := With_The_Element;
      Node_Pointer : AVL_Node_A         := Within_The_Tree.Root;
   begin
      if Node_Pointer = null then
         raise Key_Not_Found;
      end if;

      while Link_Type (Node_Pointer, Right) = Child loop
         Node_Pointer := Link (Node_Pointer, Right);
      end loop;

      With_The_Element := Element_Of (The_Node => Node_Pointer);
      Replace_Element (Node_Pointer, Swap_C);
   end Swap_Greatest;

   procedure Swap_Least
     (Within_The_Tree  : in out T;
      With_The_Element : in out Element_T)
   is
      Swap_C       : constant Element_T  := With_The_Element;
      Node_Pointer : constant AVL_Node_A :=
        Least_Node (Within_The_Tree);
   begin
      if Node_Pointer = null then
         raise Key_Not_Found;
      end if;

      With_The_Element := Element_Of (The_Node => Node_Pointer);
      Replace_Element (Node_Pointer, Swap_C);
   end Swap_Least;

   procedure Swap_Predecessor
     (Within_The_Tree  : in out T;
      Of_The_Key       : in     Key_T;
      With_The_Element : in out Element_T)
   is
      Swap_C       : constant Element_T := With_The_Element;
      Node_Pointer : AVL_Node_A := Node (Within_The_Tree, Of_The_Key);
   begin
      if Node_Pointer = null then
         raise Key_Not_Found;
      else
         case Link_Type (Node_Pointer, Left) is
            when Child =>
               Node_Pointer := Link (Node_Pointer, Left);

               while Link_Type (Node_Pointer, Right) /= Thread loop
                  Node_Pointer := Link (Node_Pointer, Right);
               end loop;

               With_The_Element :=
                 Element_Of (The_Node => Node_Pointer);
               Replace_Element (Node_Pointer, Swap_C);
            when Thread =>
               if Link (Node_Pointer, Left) /= null then
                  Node_Pointer     := Link (Node_Pointer, Left);
                  With_The_Element :=
                    Element_Of (The_Node => Node_Pointer);
                  Replace_Element (Node_Pointer, Swap_C);
               end if;
         end case;
      end if;
   end Swap_Predecessor;

   procedure Swap_Successor
     (Within_The_Tree  : in out T;
      Of_The_Key       : in     Key_T;
      With_The_Element : in out Element_T)
   is
      Swap_C       : constant Element_T := With_The_Element;
      Node_Pointer : AVL_Node_A := Node (Within_The_Tree, Of_The_Key);
   begin
      if Node_Pointer = null then
         raise Key_Not_Found;
      else
         Node_Pointer := Successor_Node (Node_Pointer);

         if Node_Pointer = null then
            raise Key_Not_Found;
         else
            With_The_Element := Element_Of (The_Node => Node_Pointer);
            Replace_Element (Node_Pointer, Swap_C);
         end if;
      end if;
   end Swap_Successor;

   procedure Traverse_In_Order
     (The_Tree : in T)
   is
      Node_Pointer : AVL_Node_A := Least_Node (The_Tree);
      Continue     : Boolean    := False;
   begin
      while Node_Pointer /= null loop
         Process
           (Key_Of (Node_Pointer), Element_Of (Node_Pointer),
            Continue);
         exit when not Continue;
         Node_Pointer := Successor_Node (Node_Pointer);
      end loop;
   end Traverse_In_Order;

end AVL_Trees;
