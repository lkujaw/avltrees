-----------------------------------------------------------------------
--  Copyright 2021 Lev Kujawski
--
--  This file is part of AVLTREES.
--
--  AVLTREES is free software: you can redistribute it and/or modify
--  it under the terms of the GNU Lesser General Public License as
--  published by the Free Software Foundation, either version 3 of
--  the License, or (at your option) any later version.
--
--  AVLTREES is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU Lesser General Public License for more details.
--
--  You should have received a copy of the
--  GNU Lesser General Public License along with AVLTREES.
--  If not, see <https://www.gnu.org/licenses/>.
--
--  SPDX-License-Identifier: LGPL-3.0-or-later
--
--  File:          avltrees.adb (Ada Package Body)
--  Language:      Ada (1987) [1]
--  Author:        Lev Kujawski
--  Description:
--    Self-balancing binary trees, based upon the algorithms developed
--    by G. M. Adelson-Velsky and E. M. Landis [2].
--
--  References:
--  [1] Programming languages - Ada, ISO/IEC 8652:1987, 15 Jun. 1987.
--  [2] G. M. Adelson-Velsky and E. M. Landis
--      Doklady Akademii Nauk SSSR 146 (1962), 263-266
--      English translation in
--      "An algorithm for the organization of information",
--      Soviet Math. Doklady 3 (1962) 1259-1263.
--
-----------------------------------------------------------------------

with Unchecked_Deallocation;

package body AVL_Trees is

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

   type Links_T is array (Direction_T) of Link_T;
   pragma Pack (Links_T);

   --  LL and RL correspond to 'Left Link' and 'Right Link',
   --  respectively.
   type AVL_Node_T is
      record
         Balance   : Balance_T;
         Link_Kind : Links_T;
         LL        : AVL_Node_A;
         RL        : AVL_Node_A;
         Key       : Key_T;
         Data      : Element_T;
      end record;
   pragma Pack (AVL_Node_T);

   --  Using an array indexed by Direction_T for the left and right
   --  links seems to be impossible without implicit looping or
   --  'pragma Suppress_Initialization', a consequence of having
   --  access types as array elements, as they must be initialized to
   --  null.  This is unfortunate, as a great deal of duplication
   --  could be avoided were this not the case.

   procedure Balance_Post_Deletion
     (Direction   : in     Direction_T;
      Parent_Node : in     AVL_Node_A;
      Head        : in out T);

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

   --  left thread-right child
   procedure Remove_Node_LTRC
     (PNode : in     AVL_Node_A;
      DNode : in out AVL_Node_A;
      Head  : in out T);
   pragma Inline (Remove_Node_LTRC);

   --  left child-right thread
   procedure Remove_Node_LCRT
     (PNode : in     AVL_Node_A;
      DNode : in out AVL_Node_A;
      Head  : in out T);
   pragma Inline (Remove_Node_LCRT);

   --  left child-right thread
   procedure Remove_Root_LCRT
     (Head : in out T);
   pragma Inline (Remove_Root_LCRT);

   --  left thread-right child
   procedure Remove_Root_LTRC
     (Head : in out T);
   pragma Inline (Remove_Root_LTRC);

   --  left child-right child (replace root with predecessor)
   procedure Remove_Root_LCRC_Predecessor
     (Head : in out T);
   pragma Inline (Remove_Root_LCRC_Predecessor);

   --  left child-right child (replace root with successor)
   procedure Remove_Root_LCRC_Successor
     (Head : in out T);
   pragma Inline (Remove_Root_LCRC_Successor);

   --  left child-right-child (left child has right thread)
   procedure Remove_Root_LCRC_LCT
     (Head : in out T);
   pragma Inline (Remove_Root_LCRC_LCT);

   --  left child-right-child (right child has left thread)
   procedure Remove_Root_LCRC_RCT
     (Head : in out T);
   pragma Inline (Remove_Root_LCRC_RCT);

   --  Called exclusively by Balance_Post_Deletion. Should be inlined.
   procedure Double_Rotation_Left
     (NZRT : in     AVL_Node_A;
      Node : in out AVL_Node_A);
   pragma Inline (Double_Rotation_Left);

   --  Called exclusively by Balance_Post_Deletion. Should be inlined.
   procedure Double_Rotation_Right
     (NZRT : in     AVL_Node_A;
      Node : in out AVL_Node_A);
   pragma Inline (Double_Rotation_Right);

   --  Provide a pointer to the parent of the given node.
   --
   --  If the parent is the root of the tree, null is returned.
   function Parent
     (Node : in AVL_Node_A)
      return AVL_Node_A;

   procedure Unchecked_Deallocate_Node is new Unchecked_Deallocation
     (Object => AVL_Node_T, Name => AVL_Node_A);

   function Is_Equal_To
     (X : in Key_T;
      Y : in Key_T)
      return Boolean
   is
      Result : Boolean := True;
   begin
      if
        Is_Less_Than (X => X, Y => Y)
        or else
        Is_Less_Than (X => Y, Y => X)
      then
         Result := False;
      end if;

      return Result;
   end Is_Equal_To;
   pragma Inline (Is_Equal_To);

   procedure Balance_Post_Deletion
     (Direction   : in     Direction_T;
      Parent_Node : in     AVL_Node_A;
      Head        : in out T)
   is
      T_Dir : Direction_T := Direction;
      PNode : AVL_Node_A  := Parent_Node;
      SNode : AVL_Node_A  := null;
      TNode : AVL_Node_A  := null;
   begin
      loop
         TNode := PNode;
         PNode := Parent (Node => TNode);

         case T_Dir is
            when Left =>
               if PNode /= null and then PNode.all.LL /= TNode then
                  T_Dir := Right;
               end if;

               case TNode.all.Balance is
                  when -1 =>
                     TNode.all.Balance := 0;

                     if TNode = Head.Root then
                        Head.Height := Head.Height - 1;
                     end if;
                  when 0 =>
                     TNode.all.Balance := +1;
                     exit;
                  when +1 =>
                     SNode := TNode.all.RL;

                     case SNode.all.Balance is
                        when +1 =>
                           case SNode.all.Link_Kind (Left) is
                              when Child =>
                                 TNode.all.RL := SNode.all.LL;
                                 SNode.all.LL := TNode;
                              when Thread =>
                                 TNode.all.Link_Kind (Right) := Thread;
                                 SNode.all.Link_Kind (Left)  := Child;
                           end case;

                           TNode.all.Balance := 0;
                           SNode.all.Balance := 0;
                        when 0 =>
                           TNode.all.RL := SNode.all.LL;
                           SNode.all.LL := TNode;

                           TNode.all.Balance := +1;
                           SNode.all.Balance := -1;
                        when -1 =>
                           Double_Rotation_Right
                             (NZRT => TNode,
                              Node => SNode);
                     end case;

                     if PNode = null then
                        Head.Root := SNode;

                        if Head.Root.all.Balance = 0 then
                           Head.Height := Head.Height - 1;
                        end if;

                        exit;
                     end if;

                     case T_Dir is
                        when Left =>
                           PNode.all.LL := SNode;
                        when Right =>
                           PNode.all.RL := SNode;
                     end case;

                     exit when SNode.all.Balance /= 0;
               end case;
            when Right =>
               if PNode /= null and then PNode.all.RL /= TNode then
                  T_Dir := Left;
               end if;

               case TNode.all.Balance is
                  when +1 =>
                     TNode.all.Balance := 0;

                     if TNode = Head.Root then
                        Head.Height := Head.Height - 1;
                     end if;
                  when 0 =>
                     TNode.all.Balance := -1;
                     exit;
                  when -1 =>
                     SNode := TNode.all.LL;

                     case SNode.all.Balance is
                        when -1 =>
                           case SNode.all.Link_Kind (Right) is
                              when Child =>
                                 TNode.all.LL := SNode.all.RL;
                                 SNode.all.RL := TNode;
                              when Thread =>
                                 TNode.all.Link_Kind (Left)  := Thread;
                                 SNode.all.Link_Kind (Right) := Child;
                           end case;

                           TNode.all.Balance := 0;
                           SNode.all.Balance := 0;
                        when 0 =>
                           TNode.all.LL := SNode.all.RL;
                           SNode.all.RL := TNode;

                           TNode.all.Balance := -1;
                           SNode.all.Balance := +1;
                        when +1 =>
                           Double_Rotation_Left
                             (NZRT => TNode,
                              Node => SNode);
                     end case;

                     if PNode = null then
                        Head.Root := SNode;

                        if Head.Root.all.Balance = 0 then
                           Head.Height := Head.Height - 1;
                        end if;

                        exit;
                     end if;

                     case T_Dir is
                        when Left =>
                           PNode.all.LL := SNode;
                        when Right =>
                           PNode.all.RL := SNode;
                     end case;

                     exit when SNode.all.Balance /= 0;
               end case;
         end case;

         exit when PNode = null;
      end loop;
   end Balance_Post_Deletion;

   procedure Remove
     (From_The_Tree : in out T;
      The_Key       : in     Key_T)
   is
   begin
      if From_The_Tree.Root /= null then
         if Is_Equal_To (The_Key, From_The_Tree.Root.all.Key) then
            case From_The_Tree.Root.all.Link_Kind (Left) is
               when Child =>
                  case From_The_Tree.Root.all.Link_Kind (Right) is
                     when Child =>
                        if From_The_Tree.Root.all.Balance = -1 then
                           if From_The_Tree.Root.all.LL.all.Link_Kind
                               (Right)
                             = Thread
                           then
                              Remove_Root_LCRC_LCT
                                (Head => From_The_Tree);
                           elsif
                             From_The_Tree.Root.all.RL.all.Link_Kind
                               (Left)
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
                           if From_The_Tree.Root.all.RL.all.Link_Kind
                               (Left)
                             = Thread
                           then
                              Remove_Root_LCRC_RCT
                                (Head => From_The_Tree);
                           elsif
                             From_The_Tree.Root.all.LL.all.Link_Kind
                               (Right)
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
                  case From_The_Tree.Root.all.Link_Kind (Right) is
                     when Child =>
                        Remove_Root_LTRC (Head => From_The_Tree);
                     when Thread =>
                        Unchecked_Deallocate_Node
                          (From_The_Tree.Root);
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
         if Is_Less_Than (Key, DNode.all.Key) then
            if DNode.all.Link_Kind (Left) = Thread then
               raise Key_Not_Found;
            end if;

            PNode := DNode;
            DNode := DNode.all.LL;
         elsif Is_Less_Than (DNode.all.Key, Key) then
            if DNode.all.Link_Kind (Right) = Thread then
               raise Key_Not_Found;
            end if;

            PNode := DNode;
            DNode := DNode.all.RL;
         else
            case DNode.all.Link_Kind (Left) is
               when Child =>
                  case DNode.all.Link_Kind (Right) is
                     when Child =>
                        if DNode.all.Balance = -1 then
                           if DNode.all.LL.all.Link_Kind (Right)
                             = Thread
                           then
                              Remove_Node_LCRC_LCT
                                (PNode => PNode,
                                 DNode => DNode,
                                 Head  => Head);
                           elsif DNode.all.RL.all.Link_Kind (Left)
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
                           if DNode.all.RL.all.Link_Kind (Left)
                             = Thread
                           then
                              Remove_Node_LCRC_RCT
                                (PNode => PNode,
                                 DNode => DNode,
                                 Head  => Head);
                           elsif DNode.all.LL.all.Link_Kind (Right)
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
                  case DNode.all.Link_Kind (Right) is
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
                        if PNode.all.LL = DNode then
                           PNode.all.LL               := DNode.all.LL;
                           PNode.all.Link_Kind (Left) := Thread;

                           Unchecked_Deallocate_Node (DNode);

                           Balance_Post_Deletion
                             (Direction   => Left,
                              Parent_Node => PNode,
                              Head        => Head);
                        else
                           PNode.all.RL                := DNode.all.RL;
                           PNode.all.Link_Kind (Right) := Thread;

                           Unchecked_Deallocate_Node (DNode);

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
      RNode : AVL_Node_A := DNode.all.LL;
   begin
      RNode.all.RL                := DNode.all.RL;
      RNode.all.Link_Kind (Right) := Child;

      RNode := RNode.all.RL;

      while RNode.all.Link_Kind (Left) = Child loop
         RNode := RNode.all.LL;
      end loop;

      RNode.all.LL      := DNode.all.LL;
      RNode             := DNode.all.LL;
      RNode.all.Balance := DNode.all.Balance;

      DNode.all.LL := null;
      DNode.all.RL := null;

      if PNode.all.LL = DNode then
         PNode.all.LL := RNode;
      else
         PNode.all.RL := RNode;
      end if;

      Unchecked_Deallocate_Node (DNode);

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
      RNode : AVL_Node_A := DNode.all.LL;
   begin
      --  Start at the node to the left of the root and keep moving
      --  right to find the ceiling node.
      while RNode.all.Link_Kind (Right) = Child loop
         QNode := RNode;
         RNode := RNode.all.RL;
      end loop;

      RNode := DNode.all.RL;

      while RNode.all.Link_Kind (Left) = Child loop
         RNode := RNode.all.LL;
      end loop;

      RNode.all.LL := QNode.all.RL;
      RNode        := QNode.all.RL;

      case RNode.all.Link_Kind (Left) is
         when Child =>
            QNode.all.RL := RNode.all.LL;
         when Thread =>
            QNode.all.Link_Kind (Right) := Thread;
            RNode.all.Link_Kind (Left)  := Child;
      end case;

      RNode.all.LL                := DNode.all.LL;
      RNode.all.RL                := DNode.all.RL;
      RNode.all.Link_Kind (Right) := Child;

      DNode.all.RL := null;
      DNode.all.LL := null;

      if PNode.all.LL = DNode then
         PNode.all.LL := RNode;
      else
         PNode.all.RL := RNode;
      end if;

      RNode.all.Balance := DNode.all.Balance;

      Unchecked_Deallocate_Node (DNode);

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
      RNode.all.LL               := DNode.all.LL;
      RNode.all.Link_Kind (Left) := Child;

      RNode := RNode.all.LL;

      while RNode.all.Link_Kind (Right) = Child loop
         RNode := RNode.all.RL;
      end loop;

      RNode.all.RL      := DNode.all.RL;
      RNode             := DNode.all.RL;
      RNode.all.Balance := DNode.all.Balance;

      DNode.all.RL := null;
      DNode.all.LL := null;

      if PNode.all.RL = DNode then
         PNode.all.RL := RNode;
      else
         PNode.all.LL := RNode;
      end if;

      Unchecked_Deallocate_Node (DNode);

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
      while RNode.all.Link_Kind (Left) = Child loop
         QNode := RNode;
         RNode := RNode.all.LL;
      end loop;

      RNode := DNode.all.LL;

      while RNode.all.Link_Kind (Right) = Child loop
         RNode := RNode.all.RL;
      end loop;

      RNode.all.RL := QNode.all.LL;
      RNode        := QNode.all.LL;

      case RNode.all.Link_Kind (Right) is
         when Child =>
            QNode.all.LL := RNode.all.RL;
         when Thread =>
            QNode.all.Link_Kind (Left)  := Thread;
            RNode.all.Link_Kind (Right) := Child;
      end case;

      RNode.all.RL               := DNode.all.RL;
      RNode.all.LL               := DNode.all.LL;
      RNode.all.Link_Kind (Left) := Child;

      DNode.all.LL := null;
      DNode.all.RL := null;

      if PNode.all.RL = DNode then
         PNode.all.RL := RNode;
      else
         PNode.all.LL := RNode;
      end if;

      RNode.all.Balance := DNode.all.Balance;

      Unchecked_Deallocate_Node (DNode);

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
      RNode : AVL_Node_A := DNode.all.LL;
   begin
      case RNode.all.Link_Kind (Right) is
         when Child =>
            while RNode.all.Link_Kind (Right) = Child loop
               RNode := RNode.all.RL;
            end loop;

            --  Set the right link of this node, a thread,
            --  to point to the same node as the right link
            --  of the DNode (also a thread).
            RNode.all.RL := DNode.all.RL;
            RNode        := DNode.all.LL;
         when Thread =>
            RNode.all.RL := DNode.all.RL;
      end case;

      DNode.all.RL := null;
      DNode.all.LL := null;

      if PNode.all.RL = DNode then
         --  PNode's left link now points to the left
         --  child of the DNode.
         PNode.all.RL := RNode;
      else
         PNode.all.LL := RNode;
      end if;

      RNode.all.Balance := DNode.all.Balance;

      Unchecked_Deallocate_Node (DNode);

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
      case RNode.all.Link_Kind (Left) is
         when Child =>
            while RNode.all.Link_Kind (Left) = Child loop
               RNode := RNode.all.LL;
            end loop;

            RNode.all.LL := DNode.all.LL;
            RNode        := DNode.all.RL;
         when Thread =>
            RNode.all.LL := DNode.all.LL;
      end case;

      DNode.all.LL := null;
      DNode.all.RL := null;

      if PNode.all.LL = DNode then
         PNode.all.LL := RNode;
      else
         PNode.all.RL := RNode;
      end if;

      RNode.all.Balance := DNode.all.Balance;

      Unchecked_Deallocate_Node (DNode);

      Balance_Post_Deletion
        (Direction   => Right,
         Parent_Node => RNode,
         Head        => Head);
   end Remove_Node_LTRC;

   procedure Remove_Root_LCRC_LCT
     (Head : in out T)
   is
      RNode : AVL_Node_A := Head.Root.all.LL;
   begin
      RNode.all.RL                := Head.Root.all.RL;
      RNode.all.Link_Kind (Right) := Child;

      RNode := RNode.all.RL;

      while RNode.all.Link_Kind (Left) = Child loop
         RNode := RNode.all.LL;
      end loop;

      RNode.all.LL      := Head.Root.all.LL;
      RNode             := Head.Root.all.LL;
      RNode.all.Balance := Head.Root.all.Balance;

      Head.Root.all.LL := null;
      Head.Root.all.RL := null;

      Unchecked_Deallocate_Node (Head.Root);

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
      RNode : AVL_Node_A := Head.Root.all.LL;
   begin
      --  Start at the node to the left of the root and keep moving
      --  right to find the ceiling node.
      while RNode.all.Link_Kind (Right) = Child loop
         PNode := RNode;
         RNode := RNode.all.RL;
      end loop;

      RNode := Head.Root.all.RL;

      while RNode.all.Link_Kind (Left) = Child loop
         RNode := RNode.all.LL;
      end loop;

      RNode.all.LL := PNode.all.RL;
      RNode        := PNode.all.RL;

      case RNode.all.Link_Kind (Left) is
         when Child =>
            PNode.all.RL := RNode.all.LL;
         when Thread =>
            PNode.all.Link_Kind (Right) := Thread;
            RNode.all.Link_Kind (Left)  := Child;
      end case;

      RNode.all.LL                := Head.Root.all.LL;
      RNode.all.RL                := Head.Root.all.RL;
      RNode.all.Link_Kind (Right) := Child;
      RNode.all.Balance           := Head.Root.all.Balance;

      Head.Root.all.RL := null;
      Head.Root.all.LL := null;

      Unchecked_Deallocate_Node (Head.Root);

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
      RNode.all.LL               := Head.Root.all.LL;
      RNode.all.Link_Kind (Left) := Child;

      RNode := RNode.all.LL;

      while RNode.all.Link_Kind (Right) = Child loop
         RNode := RNode.all.RL;
      end loop;

      RNode.all.RL      := Head.Root.all.RL;
      RNode             := Head.Root.all.RL;
      RNode.all.Balance := Head.Root.all.Balance;

      Head.Root.all.RL := null;
      Head.Root.all.LL := null;

      Unchecked_Deallocate_Node (Head.Root);

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
      while RNode.all.Link_Kind (Left) = Child loop
         PNode := RNode;
         RNode := RNode.all.LL;
      end loop;

      RNode := Head.Root.all.LL;

      while RNode.all.Link_Kind (Right) = Child loop
         RNode := RNode.all.RL;
      end loop;

      RNode.all.RL := PNode.all.LL;
      RNode        := PNode.all.LL;

      case RNode.all.Link_Kind (Right) is
         when Child =>
            PNode.all.LL := RNode.all.RL;
         when Thread =>
            PNode.all.Link_Kind (Left)  := Thread;
            RNode.all.Link_Kind (Right) := Child;
      end case;

      RNode.all.RL               := Head.Root.all.RL;
      RNode.all.LL               := Head.Root.all.LL;
      RNode.all.Link_Kind (Left) := Child;

      Head.Root.all.LL := null;
      Head.Root.all.RL := null;

      RNode.all.Balance := Head.Root.all.Balance;

      Unchecked_Deallocate_Node (Head.Root);

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
      DNode : AVL_Node_A := Head.Root.all.LL;
   begin
      case DNode.all.Link_Kind (Right) is
         when Child =>
            while DNode.all.Link_Kind (Right) = Child loop
               DNode := DNode.all.RL;
            end loop;

            DNode.all.RL := null;
            DNode        := Head.Root.all.LL;
         when Thread =>
            DNode.all.RL := null;
      end case;

      Head.Root.all.LL := null;

      Unchecked_Deallocate_Node (Head.Root);

      Head.Root   := DNode;
      Head.Height := Head.Height - 1;
   end Remove_Root_LCRT;

   procedure Remove_Root_LTRC
     (Head : in out T)
   is
      --  First, find the descendant of DNode with a thread on the
      --  left.  This thread points to DNode.  Begin the search at
      --  left child of DNode.
      DNode : AVL_Node_A := Head.Root.all.RL;
   begin
      case DNode.all.Link_Kind (Left) is
         when Child =>
            while DNode.all.Link_Kind (Left) = Child loop
               DNode := DNode.all.LL;
            end loop;

            DNode.all.LL := null;
            DNode        := Head.Root.all.RL;
         when Thread =>
            DNode.all.LL := null;
      end case;

      Head.Root.all.RL := null;

      Unchecked_Deallocate_Node (Head.Root);

      Head.Root   := DNode;
      Head.Height := Head.Height - 1;
   end Remove_Root_LTRC;

   procedure Double_Rotation_Left
     (NZRT : in     AVL_Node_A;
      Node : in out AVL_Node_A)
   is
      MN_C : constant AVL_Node_A := Node;
   begin
      Node := MN_C.all.RL;

      case Node.all.Link_Kind (Left) is
         when Child =>
            MN_C.all.RL := Node.all.LL;
            Node.all.LL := MN_C;
         when Thread =>
            Node.all.Link_Kind (Left)  := Child;
            MN_C.all.Link_Kind (Right) := Thread;
      end case;

      case Node.all.Link_Kind (Right) is
         when Child =>
            NZRT.all.LL := Node.all.RL;
            Node.all.RL := NZRT;
         when Thread =>
            Node.all.Link_Kind (Right) := Child;
            NZRT.all.LL                := Node;
            NZRT.all.Link_Kind (Left)  := Thread;
      end case;

      case Node.all.Balance is
         when -1 =>
            NZRT.all.Balance := +1;
            Node.all.Balance := 0;
            MN_C.all.Balance := 0;
         when 0 =>
            NZRT.all.Balance := 0;
            MN_C.all.Balance := 0;
         when +1 =>
            NZRT.all.Balance := 0;
            Node.all.Balance := 0;
            MN_C.all.Balance := -1;
      end case;
   end Double_Rotation_Left;

   procedure Double_Rotation_Right
     (NZRT : in     AVL_Node_A;
      Node : in out AVL_Node_A)
   is
      MN_C : constant AVL_Node_A := Node;
   begin
      Node := MN_C.all.LL;

      case Node.all.Link_Kind (Right) is
         when Child =>
            MN_C.all.LL := Node.all.RL;
            Node.all.RL := MN_C;
         when Thread =>
            Node.all.Link_Kind (Right) := Child;
            MN_C.all.Link_Kind (Left)  := Thread;
      end case;

      case Node.all.Link_Kind (Left) is
         when Child =>
            NZRT.all.RL := Node.all.LL;
            Node.all.LL := NZRT;
         when Thread =>
            Node.all.Link_Kind (Left)  := Child;
            NZRT.all.RL                := Node;
            NZRT.all.Link_Kind (Right) := Thread;
      end case;

      case Node.all.Balance is
         when +1 =>
            NZRT.all.Balance := -1;
            Node.all.Balance := 0;
            MN_C.all.Balance := 0;
         when 0 =>
            NZRT.all.Balance := 0;
            MN_C.all.Balance := 0;
         when -1 =>
            NZRT.all.Balance := 0;
            Node.all.Balance := 0;
            MN_C.all.Balance := +1;
      end case;
   end Double_Rotation_Right;

   procedure Clear
     (The_Tree : in out T)
   is
      procedure Clear_Tree;
      pragma Inline (Clear_Tree);

      procedure Clear_Tree
      is
         Node : AVL_Node_A := The_Tree.Root;
      begin
         loop
            while Node.all.Link_Kind (Left) = Child loop
               Node := Node.all.LL;
            end loop;

            while Node.all.Link_Kind (Right) = Thread
              and then Node.all.RL /= null
            loop
               Node := Node.all.RL;

               if Node.all.LL.all.Link_Kind (Right) = Child then
                  Node.all.LL.all.LL := Node;
                  Node               := Node.all.LL;

                  while Node.all.Link_Kind (Right) = Child loop
                     Node.all.RL.all.LL := Node;
                     Node               := Node.all.RL;
                  end loop;

                  while Node.all.LL.all.LL /= Node loop
                     Node := Node.all.LL;
                     Unchecked_Deallocate_Node (X => Node.all.RL);
                  end loop;

                  Node := Node.all.LL;
               end if;

               Unchecked_Deallocate_Node (X => Node.all.LL);
            end loop;

            exit when Node.all.RL = null;

            Node := Node.all.RL;
         end loop;

         Node := The_Tree.Root;

         while Node.all.Link_Kind (Right) = Child loop
            Node.all.RL.all.LL := Node;
            Node               := Node.all.RL;
         end loop;

         while Node /= The_Tree.Root loop
            Node := Node.all.LL;
            Unchecked_Deallocate_Node (X => Node.all.RL);
         end loop;
      end Clear_Tree;

   begin
      if The_Tree.Root /= null then
         Clear_Tree;
         --  According to ARM 13.11.2, Tree.Root does not need to be
         --  explicitly set to null after deallocation.
         Unchecked_Deallocate_Node (X => The_Tree.Root);
         The_Tree.Nodes  := 0;
         The_Tree.Height := 0;
      end if;
   end Clear;

   function Greatest_Key
     (Within_The_Tree : in T)
      return Key_T
   is
      Node : AVL_Node_A := Within_The_Tree.Root;
   begin
      if Node = null then
         raise Key_Not_Found;
      end if;

      while Node.all.Link_Kind (Right) = Child loop
         Node := Node.all.RL;
      end loop;

      return Node.all.Key;
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
      procedure Insert_in_Tree
      is
         PNode : AVL_Node_A := Within_The_Tree.Root;
         Node  : AVL_Node_A := Within_The_Tree.Root;
         NZRT  : AVL_Node_A := Within_The_Tree.Root;
         PNZRT : AVL_Node_A := null;
      begin
         loop
            if Is_Less_Than (The_Key, Node.all.Key) then
               case Node.all.Link_Kind (Left) is
                  when Child =>
                     Node := Node.all.LL;

                     if Node.all.Balance /= 0 then
                        PNZRT := PNode;
                        NZRT  := Node;
                     end if;

                     PNode := Node;
                  when Thread =>
                     PNode :=
                       new AVL_Node_T'
                         (Link_Kind =>
                            Links_T'(Left => Thread, Right => Thread),
                          Balance => 0,
                          Key     => The_Key,
                          LL      => Node.all.LL,
                          RL      => Node,
                          Data    => With_Element);

                     Node.all.LL               := PNode;
                     Node.all.Link_Kind (Left) := Child;

                     if Is_Less_Than (The_Key, NZRT.all.Key) then
                        Node := NZRT.all.LL;

                        loop
                           --  While moving down the tree towards the
                           --  new node, adjust the balance factors
                           --  along the way.
                           if Is_Less_Than (The_Key, Node.all.Key) then
                              --  The left subtree of the node has
                              --  become heavy due to the new
                              --  addition.
                              Node.all.Balance := -1;
                              Node             := Node.all.LL;
                           elsif Is_Less_Than (Node.all.Key, The_Key)
                           then
                              --  The right subtree of the node has
                              --  become heavy due to the new
                              --  addition.
                              Node.all.Balance := +1;
                              Node             := Node.all.RL;
                           else
                              --  Node is again equal to the new node.
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
                              Node := NZRT.all.LL;

                              if Node.all.Balance = -1 then
                                 case Node.all.Link_Kind (Right) is
                                    when Child =>
                                       NZRT.all.LL := Node.all.RL;
                                       Node.all.RL := NZRT;
                                    when Thread =>
                                       NZRT.all.Link_Kind (Left) :=
                                         Thread;
                                       Node.all.Link_Kind (Right) :=
                                         Child;
                                 end case;

                                 NZRT.all.Balance := 0;
                                 Node.all.Balance := 0;
                              else
                                 PNode        := Node;
                                 Node         := PNode.all.RL;
                                 PNode.all.RL := Node.all.LL;
                                 Node.all.LL  := PNode;

                                 case Node.all.Link_Kind (Right) is
                                    when Child =>
                                       NZRT.all.LL := Node.all.RL;
                                       Node.all.RL := NZRT;
                                    when Thread =>
                                       Node.all.Link_Kind (Right) :=
                                         Child;
                                       NZRT.all.LL := Node;
                                       NZRT.all.Link_Kind (Left) :=
                                         Thread;
                                 end case;

                                 if Node.all.Balance = -1 then
                                    NZRT.all.Balance  := +1;
                                    Node.all.Balance  := 0;
                                    PNode.all.Balance := 0;
                                 else
                                    NZRT.all.Balance  := 0;
                                    Node.all.Balance  := 0;
                                    PNode.all.Balance := -1;
                                 end if;
                              end if;

                              if PNZRT /= null then
                                 if PNZRT.all.LL = NZRT then
                                    PNZRT.all.LL := Node;
                                 else
                                    PNZRT.all.RL := Node;
                                 end if;
                              else
                                 Within_The_Tree.Root := Node;
                              end if;
                        end case;
                     else
                        Node := NZRT.all.RL;

                        loop
                           --  While moving down the tree towards the
                           --  new node, adjust the balance factors
                           --  along the way.
                           if Is_Less_Than (The_Key, Node.all.Key) then
                              --  The left subtree of the node has
                              --  become heavy due to the new
                              --  addition.
                              Node.all.Balance := -1;
                              Node             := Node.all.LL;
                           elsif Is_Less_Than (Node.all.Key, The_Key)
                           then
                              --  The right subtree of the node has
                              --  become heavy due to the new
                              --  addition.
                              Node.all.Balance := +1;
                              Node             := Node.all.RL;
                           else
                              --  Node is again equal to the new node.
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
                              Node := NZRT.all.RL;

                              if Node.all.Balance = +1 then
                                 NZRT.all.RL      := Node.all.LL;
                                 Node.all.LL      := NZRT;
                                 NZRT.all.Balance := 0;
                                 Node.all.Balance := 0;
                              else
                                 PNode := Node;
                                 Node  := PNode.all.LL;

                                 case Node.all.Link_Kind (Right) is
                                    when Child =>
                                       PNode.all.LL := Node.all.RL;
                                       Node.all.RL  := PNode;
                                    when Thread =>
                                       Node.all.Link_Kind (Right) :=
                                         Child;
                                       PNode.all.Link_Kind (Left) :=
                                         Thread;
                                 end case;

                                 case Node.all.Link_Kind (Left) is
                                    when Child =>
                                       NZRT.all.RL := Node.all.LL;
                                       Node.all.LL := NZRT;
                                    when Thread =>
                                       Node.all.Link_Kind (Left) :=
                                         Child;
                                       NZRT.all.RL := Node;
                                       NZRT.all.Link_Kind (Right) :=
                                         Thread;
                                 end case;

                                 case Node.all.Balance is
                                    when +1 =>
                                       NZRT.all.Balance  := -1;
                                       Node.all.Balance  := 0;
                                       PNode.all.Balance := 0;
                                    when 0 =>
                                       NZRT.all.Balance  := 0;
                                       PNode.all.Balance := 0;
                                    when -1 =>
                                       NZRT.all.Balance  := 0;
                                       Node.all.Balance  := 0;
                                       PNode.all.Balance := +1;
                                 end case;
                              end if;

                              if PNZRT /= null then
                                 if PNZRT.all.LL = NZRT then
                                    PNZRT.all.LL := Node;
                                 else
                                    PNZRT.all.RL := Node;
                                 end if;
                              else
                                 Within_The_Tree.Root := Node;
                              end if;
                        end case;
                     end if;

                     Within_The_Tree.Nodes :=
                       Within_The_Tree.Nodes + 1;
                     exit;
               end case;
            elsif Is_Less_Than (Node.all.Key, The_Key) then
               case Node.all.Link_Kind (Right) is
                  when Child =>
                     Node := Node.all.RL;

                     if Node.all.Balance /= 0 then
                        PNZRT := PNode;
                        NZRT  := Node;
                     end if;

                     PNode := Node;
                  when Thread =>
                     PNode :=
                       new AVL_Node_T'
                         (Link_Kind =>
                            Links_T'(Left => Thread, Right => Thread),
                          Balance => 0,
                          Key     => The_Key,
                          LL      => Node,
                          RL      => Node.all.RL,
                          Data    => With_Element);

                     Node.all.RL                := PNode;
                     Node.all.Link_Kind (Right) := Child;

                     if Is_Less_Than (The_Key, NZRT.all.Key) then
                        Node := NZRT.all.LL;

                        loop
                           --  While moving down the tree towards the
                           --  new node, adjust the balance factors
                           --  along the way.
                           if Is_Less_Than (The_Key, Node.all.Key) then
                              --  The left subtree of the node has
                              --  become heavy due to the new
                              --  addition.
                              Node.all.Balance := -1;
                              Node             := Node.all.LL;
                           elsif Is_Less_Than (Node.all.Key, The_Key)
                           then
                              --  The right subtree of the node has
                              --  become heavy due to the new
                              --  addition.
                              Node.all.Balance := +1;
                              Node             := Node.all.RL;
                           else
                              --  Node is again equal to the new node.
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
                              Node := NZRT.all.LL;

                              if Node.all.Balance = -1 then
                                 NZRT.all.LL      := Node.all.RL;
                                 Node.all.RL      := NZRT;
                                 NZRT.all.Balance := 0;
                                 Node.all.Balance := 0;
                              else
                                 PNode := Node;
                                 Node  := PNode.all.RL;

                                 case Node.all.Link_Kind (Left) is
                                    when Child =>
                                       PNode.all.RL := Node.all.LL;
                                       Node.all.LL  := PNode;
                                    when Thread =>
                                       Node.all.Link_Kind (Left) :=
                                         Child;
                                       PNode.all.Link_Kind (Right) :=
                                         Thread;
                                 end case;

                                 case Node.all.Link_Kind (Right) is
                                    when Child =>
                                       NZRT.all.LL := Node.all.RL;
                                       Node.all.RL := NZRT;
                                    when Thread =>
                                       Node.all.Link_Kind (Right) :=
                                         Child;
                                       NZRT.all.LL := Node;
                                       NZRT.all.Link_Kind (Left) :=
                                         Thread;
                                 end case;

                                 case Node.all.Balance is
                                    when -1 =>
                                       NZRT.all.Balance  := +1;
                                       Node.all.Balance  := 0;
                                       PNode.all.Balance := 0;
                                    when 0 =>
                                       NZRT.all.Balance  := 0;
                                       PNode.all.Balance := 0;
                                    when +1 =>
                                       NZRT.all.Balance  := 0;
                                       Node.all.Balance  := 0;
                                       PNode.all.Balance := -1;
                                 end case;
                              end if;

                              if PNZRT /= null then
                                 if PNZRT.all.LL = NZRT then
                                    PNZRT.all.LL := Node;
                                 else
                                    PNZRT.all.RL := Node;
                                 end if;
                              else
                                 Within_The_Tree.Root := Node;
                              end if;
                        end case;
                     else
                        Node := NZRT.all.RL;

                        loop
                           --  While moving down the tree towards the
                           --  new node, adjust the balance factors
                           --  along the way.
                           if Is_Less_Than (The_Key, Node.all.Key) then
                              --  The left subtree of the node has
                              --  become heavy due to the new
                              --  addition.
                              Node.all.Balance := -1;
                              Node             := Node.all.LL;
                           elsif Is_Less_Than (Node.all.Key, The_Key)
                           then
                              --  The right subtree of the node has
                              --  become heavy due to the new
                              --  addition.
                              Node.all.Balance := +1;
                              Node             := Node.all.RL;
                           else
                              --  Node is again equal to the new node.
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
                              Node := NZRT.all.RL;

                              if Node.all.Balance = +1 then
                                 case Node.all.Link_Kind (Left) is
                                    when Child =>
                                       NZRT.all.RL := Node.all.LL;
                                       Node.all.LL := NZRT;
                                    when Thread =>
                                       NZRT.all.Link_Kind (Right) :=
                                         Thread;
                                       Node.all.Link_Kind (Left) :=
                                         Child;
                                 end case;

                                 NZRT.all.Balance := 0;
                                 Node.all.Balance := 0;
                              else
                                 PNode        := Node;
                                 Node         := PNode.all.LL;
                                 PNode.all.LL := Node.all.RL;
                                 Node.all.RL  := PNode;

                                 case Node.all.Link_Kind (Left) is
                                    when Child =>
                                       NZRT.all.RL := Node.all.LL;
                                       Node.all.LL := NZRT;
                                    when Thread =>
                                       Node.all.Link_Kind (Left) :=
                                         Child;
                                       NZRT.all.RL := Node;
                                       NZRT.all.Link_Kind (Right) :=
                                         Thread;
                                 end case;

                                 if Node.all.Balance = +1 then
                                    NZRT.all.Balance  := -1;
                                    Node.all.Balance  := 0;
                                    PNode.all.Balance := 0;
                                 else
                                    NZRT.all.Balance  := 0;
                                    Node.all.Balance  := 0;
                                    PNode.all.Balance := +1;
                                 end if;
                              end if;

                              if PNZRT /= null then
                                 if PNZRT.all.LL = NZRT then
                                    PNZRT.all.LL := Node;
                                 else
                                    PNZRT.all.RL := Node;
                                 end if;
                              else
                                 Within_The_Tree.Root := Node;
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
      end Insert_in_Tree;

   begin
      if Within_The_Tree.Root = null then
         Within_The_Tree.Root :=
           new AVL_Node_T'
             (Link_Kind => Links_T'(Left => Thread, Right => Thread),
              Balance   => 0,
              Key       => The_Key,
              LL        => null,
              RL        => null,
              Data      => With_Element);

         Within_The_Tree.Height := 1;
         Within_The_Tree.Nodes  := 1;
      else
         Insert_in_Tree;
      end if;
   end Insert;

   procedure Insert_or_Replace
     (Within_The_Tree : in out T;
      The_Key         : in     Key_T;
      With_Element    : in     Element_T)
   is
      procedure Insert_or_Replace_in_Tree
      is
         procedure Adjust_Balance;

         procedure Attach_Tree;

         PNode : AVL_Node_A := Within_The_Tree.Root;
         Node  : AVL_Node_A := Within_The_Tree.Root;
         NZRT  : AVL_Node_A := Within_The_Tree.Root;
         PNZRT : AVL_Node_A := null;

         procedure Adjust_Balance
         is
         begin
            loop
               --  While moving down the tree towards the new node,
               --  adjust the balance factors along the way.
               if Is_Less_Than (The_Key, Node.all.Key) then
                  --  The left subtree of the node has become
                  --  heavy due to the new addition.
                  Node.all.Balance := -1;
                  Node             := Node.all.LL;
               elsif Is_Less_Than (Node.all.Key, The_Key) then
                  --  The right subtree of the node has become
                  --  heavy due to the new addition.
                  Node.all.Balance := +1;
                  Node             := Node.all.RL;
               else
                  --  Node is again equal to the new node.
                  exit;
               end if;
            end loop;
         end Adjust_Balance;

         procedure Attach_Tree
         is
         begin
            if PNZRT /= null then
               if PNZRT.all.LL = NZRT then
                  PNZRT.all.LL := Node;
               else
                  PNZRT.all.RL := Node;
               end if;
            else
               Within_The_Tree.Root := Node;
            end if;
         end Attach_Tree;

      begin
         loop
            if Is_Less_Than (The_Key, Node.all.Key) then
               case Node.all.Link_Kind (Left) is
                  when Child =>
                     Node := Node.all.LL;

                     if Node.all.Balance /= 0 then
                        PNZRT := PNode;
                        NZRT  := Node;
                     end if;

                     PNode := Node;
                  when Thread =>
                     PNode :=
                       new AVL_Node_T'
                         (Link_Kind =>
                            Links_T'(Left => Thread, Right => Thread),
                          Balance => 0,
                          LL      => Node.all.LL,
                          RL      => Node,
                          Key     => The_Key,
                          Data    => With_Element);

                     Node.all.LL               := PNode;
                     Node.all.Link_Kind (Left) := Child;

                     if Is_Less_Than (The_Key, NZRT.all.Key) then
                        Node := NZRT.all.LL;

                        Adjust_Balance;

                        case NZRT.all.Balance is
                           when +1 =>
                              NZRT.all.Balance := 0;
                           when 0 =>
                              NZRT.all.Balance       := -1;
                              Within_The_Tree.Height :=
                                Within_The_Tree.Height + 1;
                           when -1 =>
                              Node := NZRT.all.LL;

                              if Node.all.Balance = -1 then
                                 case Node.all.Link_Kind (Right) is
                                    when Child =>
                                       NZRT.all.LL := Node.all.RL;
                                       Node.all.RL := NZRT;
                                    when Thread =>
                                       NZRT.all.Link_Kind (Left) :=
                                         Thread;
                                       Node.all.Link_Kind (Right) :=
                                         Child;
                                 end case;

                                 NZRT.all.Balance := 0;
                                 Node.all.Balance := 0;
                              else
                                 PNode        := Node;
                                 Node         := PNode.all.RL;
                                 PNode.all.RL := Node.all.LL;
                                 Node.all.LL  := PNode;

                                 case Node.all.Link_Kind (Right) is
                                    when Child =>
                                       NZRT.all.LL := Node.all.RL;
                                       Node.all.RL := NZRT;
                                    when Thread =>
                                       Node.all.Link_Kind (Right) :=
                                         Child;
                                       NZRT.all.LL := Node;
                                       NZRT.all.Link_Kind (Left) :=
                                         Thread;
                                 end case;

                                 if Node.all.Balance = -1 then
                                    NZRT.all.Balance  := +1;
                                    Node.all.Balance  := 0;
                                    PNode.all.Balance := 0;
                                 else
                                    NZRT.all.Balance  := 0;
                                    Node.all.Balance  := 0;
                                    PNode.all.Balance := -1;
                                 end if;
                              end if;

                              Attach_Tree;
                        end case;
                     else
                        Node := NZRT.all.RL;

                        Adjust_Balance;

                        case NZRT.all.Balance is
                           when -1 =>
                              NZRT.all.Balance := 0;
                           when 0 =>
                              NZRT.all.Balance       := +1;
                              Within_The_Tree.Height :=
                                Within_The_Tree.Height + 1;
                           when +1 =>
                              Node := NZRT.all.RL;

                              if Node.all.Balance = +1 then
                                 NZRT.all.RL      := Node.all.LL;
                                 Node.all.LL      := NZRT;
                                 NZRT.all.Balance := 0;
                                 Node.all.Balance := 0;
                              else
                                 PNode := Node;
                                 Node  := PNode.all.LL;

                                 case Node.all.Link_Kind (Right) is
                                    when Child =>
                                       PNode.all.LL := Node.all.RL;
                                       Node.all.RL  := PNode;
                                    when Thread =>
                                       Node.all.Link_Kind (Right) :=
                                         Child;
                                       PNode.all.Link_Kind (Left) :=
                                         Thread;
                                 end case;

                                 case Node.all.Link_Kind (Left) is
                                    when Child =>
                                       NZRT.all.RL := Node.all.LL;
                                       Node.all.LL := NZRT;
                                    when Thread =>
                                       Node.all.Link_Kind (Left) :=
                                         Child;
                                       NZRT.all.RL := Node;
                                       NZRT.all.Link_Kind (Right) :=
                                         Thread;
                                 end case;

                                 case Node.all.Balance is
                                    when +1 =>
                                       NZRT.all.Balance  := -1;
                                       Node.all.Balance  := 0;
                                       PNode.all.Balance := 0;
                                    when 0 =>
                                       NZRT.all.Balance  := 0;
                                       PNode.all.Balance := 0;
                                    when -1 =>
                                       NZRT.all.Balance  := 0;
                                       Node.all.Balance  := 0;
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
            elsif Is_Less_Than (Node.all.Key, The_Key) then
               case Node.all.Link_Kind (Right) is
                  when Child =>
                     Node := Node.all.RL;

                     if Node.all.Balance /= 0 then
                        PNZRT := PNode;
                        NZRT  := Node;
                     end if;

                     PNode := Node;
                  when Thread =>
                     PNode :=
                       new AVL_Node_T'
                         (Link_Kind =>
                            Links_T'(Left => Thread, Right => Thread),
                          Balance => 0,
                          LL      => Node,
                          RL      => Node.all.RL,
                          Key     => The_Key,
                          Data    => With_Element);

                     Node.all.RL                := PNode;
                     Node.all.Link_Kind (Right) := Child;

                     if Is_Less_Than (The_Key, NZRT.all.Key) then
                        Node := NZRT.all.LL;

                        Adjust_Balance;

                        case NZRT.all.Balance is
                           when +1 =>
                              NZRT.all.Balance := 0;
                           when 0 =>
                              NZRT.all.Balance       := -1;
                              Within_The_Tree.Height :=
                                Within_The_Tree.Height + 1;
                           when -1 =>
                              Node := NZRT.all.LL;

                              if Node.all.Balance = -1 then
                                 NZRT.all.LL      := Node.all.RL;
                                 Node.all.RL      := NZRT;
                                 NZRT.all.Balance := 0;
                                 Node.all.Balance := 0;
                              else
                                 PNode := Node;
                                 Node  := PNode.all.RL;

                                 case Node.all.Link_Kind (Left) is
                                    when Child =>
                                       PNode.all.RL := Node.all.LL;
                                       Node.all.LL  := PNode;
                                    when Thread =>
                                       Node.all.Link_Kind (Left) :=
                                         Child;
                                       PNode.all.Link_Kind (Right) :=
                                         Thread;
                                 end case;

                                 case Node.all.Link_Kind (Right) is
                                    when Child =>
                                       NZRT.all.LL := Node.all.RL;
                                       Node.all.RL := NZRT;
                                    when Thread =>
                                       Node.all.Link_Kind (Right) :=
                                         Child;
                                       NZRT.all.LL := Node;
                                       NZRT.all.Link_Kind (Left) :=
                                         Thread;
                                 end case;

                                 case Node.all.Balance is
                                    when -1 =>
                                       NZRT.all.Balance  := +1;
                                       Node.all.Balance  := 0;
                                       PNode.all.Balance := 0;
                                    when 0 =>
                                       NZRT.all.Balance  := 0;
                                       PNode.all.Balance := 0;
                                    when +1 =>
                                       NZRT.all.Balance  := 0;
                                       Node.all.Balance  := 0;
                                       PNode.all.Balance := -1;
                                 end case;
                              end if;

                              Attach_Tree;
                        end case;
                     else
                        Node := NZRT.all.RL;

                        Adjust_Balance;

                        case NZRT.all.Balance is
                           when -1 =>
                              NZRT.all.Balance := 0;
                           when 0 =>
                              NZRT.all.Balance       := +1;
                              Within_The_Tree.Height :=
                                Within_The_Tree.Height + 1;
                           when +1 =>
                              Node := NZRT.all.RL;

                              if Node.all.Balance = +1 then
                                 case Node.all.Link_Kind (Left) is
                                    when Child =>
                                       NZRT.all.RL := Node.all.LL;
                                       Node.all.LL := NZRT;
                                    when Thread =>
                                       NZRT.all.Link_Kind (Right) :=
                                         Thread;
                                       Node.all.Link_Kind (Left) :=
                                         Child;
                                 end case;

                                 NZRT.all.Balance := 0;
                                 Node.all.Balance := 0;
                              else
                                 PNode        := Node;
                                 Node         := PNode.all.LL;
                                 PNode.all.LL := Node.all.RL;
                                 Node.all.RL  := PNode;

                                 case Node.all.Link_Kind (Left) is
                                    when Child =>
                                       NZRT.all.RL := Node.all.LL;
                                       Node.all.LL := NZRT;
                                    when Thread =>
                                       Node.all.Link_Kind (Left) :=
                                         Child;
                                       NZRT.all.RL := Node;
                                       NZRT.all.Link_Kind (Right) :=
                                         Thread;
                                 end case;

                                 if Node.all.Balance = +1 then
                                    NZRT.all.Balance  := -1;
                                    Node.all.Balance  := 0;
                                    PNode.all.Balance := 0;
                                 else
                                    NZRT.all.Balance  := 0;
                                    Node.all.Balance  := 0;
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
               Node.all.Data := With_Element;

               exit;
            end if;
         end loop;
      end Insert_or_Replace_in_Tree;

   begin
      if Within_The_Tree.Root /= null then
         Insert_or_Replace_in_Tree;
      else
         Within_The_Tree.Root :=
           new AVL_Node_T'
             (Link_Kind => Links_T'(Left => Thread, Right => Thread),
              Balance   => 0,
              LL        => null,
              RL        => null,
              Key       => The_Key,
              Data      => With_Element);

         Within_The_Tree.Height := 1;
         Within_The_Tree.Nodes  := 1;
      end if;
   end Insert_or_Replace;

   function Is_Empty
     (The_Tree : in T)
      return Boolean
   is
   begin
      return The_Tree.Nodes = 0;
   end Is_Empty;

   function Is_Present
     (Within_The_Tree : in T;
      The_Key         : in Key_T)
      return Boolean
   is
      Result : Boolean    := False;
      Node   : AVL_Node_A := Within_The_Tree.Root;
   begin
      if Node /= null then
         loop
            if Is_Less_Than (The_Key, Node.all.Key) then
               exit when Node.all.Link_Kind (Left) = Thread;

               Node := Node.all.LL;
            elsif Is_Less_Than (Node.all.Key, The_Key) then
               exit when Node.all.Link_Kind (Right) = Thread;

               Node := Node.all.RL;
            else
               Result := True;
               exit;
            end if;
         end loop;
      end if;

      return Result;
   end Is_Present;

   function Least_Key
     (Within_The_Tree : in T)
      return Key_T
   is
      Node : AVL_Node_A := Within_The_Tree.Root;
   begin
      if Node = null then
         raise Key_Not_Found;
      end if;

      while Node.all.Link_Kind (Left) = Child loop
         Node := Node.all.LL;
      end loop;

      return Node.all.Key;
   end Least_Key;

   function Nodes
     (Within_The_Tree : in T)
      return Natural
   is
   begin
      return Within_The_Tree.Nodes;
   end Nodes;

   function Parent
     (Node : in AVL_Node_A)
      return AVL_Node_A
   is
      --  Place the result in LNode, avoiding the use of another
      --  variable or multiple return statements.
      LNode : AVL_Node_A := Node;
      RNode : AVL_Node_A := Node;
   begin
      --  The chain of children to the left or right may end far
      --  sooner than that of the opposite chain, so stop as soon as
      --  possible to avoid unnecessary effort.
      loop
         if LNode.all.Link_Kind (Left) = Thread then
            LNode := LNode.all.LL;

            if LNode = null or else LNode.all.RL /= Node then
               while RNode.all.Link_Kind (Right) = Child loop
                  RNode := RNode.all.RL;
               end loop;

               LNode := RNode.all.RL;
            end if;

            exit;
         end if;

         if RNode.all.Link_Kind (Right) = Thread then
            RNode := RNode.all.RL;

            if RNode = null or else RNode.all.LL /= Node then
               while LNode.all.Link_Kind (Left) = Child loop
                  LNode := LNode.all.LL;
               end loop;

               --  These two breaks in symmetry are to satisfy the
               --  above demand that the result be placed in LNode.
               LNode := LNode.all.LL;
            else
               LNode := RNode;
            end if;

            exit;
         end if;

         LNode := LNode.all.LL;
         RNode := RNode.all.RL;
      end loop;

      return LNode;
   end Parent;

   function Predecessor_Key
     (Within_The_Tree : in T;
      Of_The_Key      : in Key_T)
      return Key_T
   is
      Node : AVL_Node_A := Within_The_Tree.Root;
   begin
      if Node = null then
         raise Key_Not_Found;
      end if;

      loop
         if Is_Less_Than (Of_The_Key, Node.all.Key) then
            case Node.all.Link_Kind (Left) is
               when Child =>
                  Node := Node.all.LL;
               when Thread =>
                  raise Key_Not_Found;
            end case;
         elsif Is_Less_Than (Node.all.Key, Of_The_Key) then
            case Node.all.Link_Kind (Right) is
               when Child =>
                  Node := Node.all.RL;
               when Thread =>
                  raise Key_Not_Found;
            end case;
         else
            case Node.all.Link_Kind (Left) is
               when Child =>
                  Node := Node.all.LL;

                  while Node.all.Link_Kind (Right) /= Thread loop
                     Node := Node.all.RL;
                  end loop;

                  return Node.all.Key;
               when Thread =>
                  if Node.all.LL = null then
                     raise Key_Not_Found;
                  end if;

                  Node := Node.all.LL;

                  return Node.all.Key;
            end case;
         end if;
      end loop;
   end Predecessor_Key;

   procedure Replace
     (Within_The_Tree : in out T;
      The_Key         : in     Key_T;
      With_Element    : in     Element_T)
   is
      Node : AVL_Node_A := Within_The_Tree.Root;
   begin
      if Node = null then
         raise Key_Not_Found;
      end if;

      loop
         if Is_Less_Than (The_Key, Node.all.Key) then
            if Node.all.Link_Kind (Left) = Thread then
               raise Key_Not_Found;
            end if;

            Node := Node.all.LL;
         elsif Is_Less_Than (Node.all.Key, The_Key) then
            if Node.all.Link_Kind (Right) = Thread then
               raise Key_Not_Found;
            end if;

            Node := Node.all.RL;
         else
            Node.all.Data := With_Element;

            exit;
         end if;
      end loop;
   end Replace;

   procedure Replace_Greatest
     (Within_The_Tree : in out T;
      With_Element    : in     Element_T)
   is
      Node : AVL_Node_A := Within_The_Tree.Root;
   begin
      if Node = null then
         raise Key_Not_Found;
      end if;

      while Node.all.Link_Kind (Right) = Child loop
         Node := Node.all.RL;
      end loop;

      Node.all.Data := With_Element;
   end Replace_Greatest;

   procedure Replace_Least
     (Within_The_Tree : in out T;
      With_Element    : in     Element_T)
   is
      Node : AVL_Node_A := Within_The_Tree.Root;
   begin
      if Node = null then
         raise Key_Not_Found;
      end if;

      while Node.all.Link_Kind (Left) = Child loop
         Node := Node.all.LL;
      end loop;

      Node.all.Data := With_Element;
   end Replace_Least;

   procedure Replace_Predecessor
     (Within_The_Tree : in out T;
      Of_The_Key      : in     Key_T;
      With_Element    : in     Element_T)
   is
      Node : AVL_Node_A := Within_The_Tree.Root;
   begin
      if Node = null then
         raise Key_Not_Found;
      end if;

      loop
         if Is_Less_Than (Of_The_Key, Node.all.Key) then
            if Node.all.Link_Kind (Left) = Thread then
               raise Key_Not_Found;
            end if;

            Node := Node.all.LL;
         elsif Is_Less_Than (Node.all.Key, Of_The_Key) then
            if Node.all.Link_Kind (Right) = Thread then
               raise Key_Not_Found;
            end if;

            Node := Node.all.RL;
         else
            case Node.all.Link_Kind (Left) is
               when Child =>
                  Node := Node.all.LL;

                  while Node.all.Link_Kind (Right) /= Thread loop
                     Node := Node.all.RL;
                  end loop;

                  Node.all.Data := With_Element;
               when Thread =>
                  if Node.all.LL /= null then
                     Node          := Node.all.LL;
                     Node.all.Data := With_Element;
                  end if;
            end case;

            exit;
         end if;
      end loop;
   end Replace_Predecessor;

   procedure Replace_Successor
     (Within_The_Tree : in out T;
      Of_The_Key      : in     Key_T;
      With_Element    : in     Element_T)
   is
      Node : AVL_Node_A := Within_The_Tree.Root;
   begin
      loop
         if Is_Less_Than (Of_The_Key, Node.all.Key) then
            exit when Node.all.Link_Kind (Left) = Thread;

            Node := Node.all.LL;
         elsif Is_Less_Than (Node.all.Key, Of_The_Key) then
            exit when Node.all.Link_Kind (Right) = Thread;

            Node := Node.all.RL;
         else
            case Node.all.Link_Kind (Right) is
               when Child =>
                  Node := Node.all.RL;

                  while Node.all.Link_Kind (Left) /= Thread loop
                     Node := Node.all.LL;
                  end loop;

                  Node.all.Data := With_Element;
               when Thread =>
                  if Node.all.RL /= null then
                     Node          := Node.all.RL;
                     Node.all.Data := With_Element;
                  end if;
            end case;

            exit;
         end if;
      end loop;
   end Replace_Successor;

   procedure Retrieve
     (From_The_Tree    : in     T;
      The_Key          : in     Key_T;
      Into_The_Element :    out Element_T)
   is
      Node : AVL_Node_A := From_The_Tree.Root;
   begin
      if Node = null then
         raise Key_Not_Found;
      end if;

      loop
         if Is_Less_Than (The_Key, Node.all.Key) then
            if Node.all.Link_Kind (Left) = Thread then
               raise Key_Not_Found;
            end if;

            Node := Node.all.LL;
         elsif Is_Less_Than (Node.all.Key, The_Key) then
            if Node.all.Link_Kind (Right) = Thread then
               raise Key_Not_Found;
            end if;

            Node := Node.all.RL;
         else
            Into_The_Element := Node.all.Data;
            exit;
         end if;
      end loop;
   end Retrieve;

   procedure Retrieve_Greatest
     (From_The_Tree    : in     T;
      Into_The_Element :    out Element_T)
   is
      Node : AVL_Node_A := From_The_Tree.Root;
   begin
      if Node = null then
         raise Key_Not_Found;
      end if;

      while Node.all.Link_Kind (Right) = Child loop
         Node := Node.all.RL;
      end loop;

      Into_The_Element := Node.all.Data;
   end Retrieve_Greatest;

   procedure Retrieve_Least
     (From_The_Tree    : in     T;
      Into_The_Element :    out Element_T)
   is
      Node : AVL_Node_A := From_The_Tree.Root;
   begin
      if Node = null then
         raise Key_Not_Found;
      end if;

      while Node.all.Link_Kind (Left) = Child loop
         Node := Node.all.LL;
      end loop;

      Into_The_Element := Node.all.Data;
   end Retrieve_Least;

   procedure Retrieve_Predecessor
     (Within_The_Tree  : in     T;
      Of_The_Key       : in     Key_T;
      Into_The_Element : in out Element_T)
   is
      Node : AVL_Node_A := Within_The_Tree.Root;
   begin
      if Node = null then
         raise Key_Not_Found;
      end if;

      loop
         if Is_Less_Than (Of_The_Key, Node.all.Key) then
            if Node.all.Link_Kind (Left) = Thread then
               raise Key_Not_Found;
            end if;

            Node := Node.all.LL;
         elsif Is_Less_Than (Node.all.Key, Of_The_Key) then
            if Node.all.Link_Kind (Right) = Thread then
               raise Key_Not_Found;
            end if;

            Node := Node.all.RL;
         else
            case Node.all.Link_Kind (Left) is
               when Child =>
                  Node := Node.all.LL;

                  while Node.all.Link_Kind (Right) /= Thread loop
                     Node := Node.all.RL;
                  end loop;

                  Into_The_Element := Node.all.Data;
               when Thread =>
                  if Node.all.LL /= null then
                     Node             := Node.all.LL;
                     Into_The_Element := Node.all.Data;
                  end if;
            end case;

            exit;
         end if;
      end loop;
   end Retrieve_Predecessor;

   procedure Retrieve_Successor
     (Within_The_Tree  : in     T;
      Of_The_Key       : in     Key_T;
      Into_The_Element : in out Element_T)
   is
      Node : AVL_Node_A := Within_The_Tree.Root;
   begin
      if Node = null then
         raise Key_Not_Found;
      end if;

      loop
         if Is_Less_Than (Of_The_Key, Node.all.Key) then
            if Node.all.Link_Kind (Left) = Thread then
               raise Key_Not_Found;
            end if;

            Node := Node.all.LL;
         elsif Is_Less_Than (Node.all.Key, Of_The_Key) then
            if Node.all.Link_Kind (Right) = Thread then
               raise Key_Not_Found;
            end if;

            Node := Node.all.RL;
         else
            case Node.all.Link_Kind (Right) is
               when Child =>
                  Node := Node.all.RL;

                  while Node.all.Link_Kind (Left) /= Thread loop
                     Node := Node.all.LL;
                  end loop;

                  Into_The_Element := Node.all.Data;
               when Thread =>
                  if Node.all.RL /= null then
                     Node             := Node.all.RL;
                     Into_The_Element := Node.all.Data;
                  end if;
            end case;

            exit;
         end if;
      end loop;
   end Retrieve_Successor;

   function Successor_Key
     (Within_The_Tree : in T;
      Of_The_Key      : in Key_T)
      return Key_T
   is
      Node : AVL_Node_A := Within_The_Tree.Root;
   begin
      if Node = null then
         raise Key_Not_Found;
      end if;

      loop
         if Is_Less_Than (Of_The_Key, Node.all.Key) then
            case Node.all.Link_Kind (Left) is
               when Child =>
                  Node := Node.all.LL;
               when Thread =>
                  raise Key_Not_Found;
            end case;
         elsif Is_Less_Than (Node.all.Key, Of_The_Key) then
            case Node.all.Link_Kind (Right) is
               when Child =>
                  Node := Node.all.RL;
               when Thread =>
                  raise Key_Not_Found;
            end case;
         else
            case Node.all.Link_Kind (Right) is
               when Child =>
                  Node := Node.all.RL;

                  while Node.all.Link_Kind (Left) /= Thread loop
                     Node := Node.all.LL;
                  end loop;

                  return Node.all.Key;
               when Thread =>
                  if Node.all.RL = null then
                     raise Key_Not_Found;
                  end if;

                  Node := Node.all.RL;

                  return Node.all.Key;
            end case;
         end if;
      end loop;
   end Successor_Key;

   procedure Swap
     (Within_The_Tree  : in out T;
      The_Key          : in     Key_T;
      With_The_Element : in out Element_T)
   is
      Swap_C : constant Element_T := With_The_Element;
      Node   : AVL_Node_A         := Within_The_Tree.Root;
   begin
      loop
         if Is_Less_Than (The_Key, Node.all.Key) then
            if Node.all.Link_Kind (Left) = Thread then
               raise Key_Not_Found;
            end if;

            Node := Node.all.LL;
         elsif Is_Less_Than (Node.all.Key, The_Key) then
            if Node.all.Link_Kind (Right) = Thread then
               raise Key_Not_Found;
            end if;

            Node := Node.all.RL;
         else
            With_The_Element := Node.all.Data;
            Node.all.Data    := Swap_C;
            exit;
         end if;
      end loop;
   end Swap;

   procedure Swap_Greatest
     (Within_The_Tree  : in out T;
      With_The_Element : in out Element_T)
   is
      Swap_C : constant Element_T := With_The_Element;
      Node   : AVL_Node_A         := Within_The_Tree.Root;
   begin
      if Node = null then
         raise Key_Not_Found;
      end if;

      while Node.all.Link_Kind (Right) = Child loop
         Node := Node.all.RL;
      end loop;

      With_The_Element := Node.all.Data;
      Node.all.Data    := Swap_C;
   end Swap_Greatest;

   procedure Swap_Least
     (Within_The_Tree  : in out T;
      With_The_Element : in out Element_T)
   is
      Swap_C : constant Element_T := With_The_Element;
      Node   : AVL_Node_A         := Within_The_Tree.Root;
   begin
      while Node.all.Link_Kind (Left) = Child loop
         Node := Node.all.LL;
      end loop;

      With_The_Element := Node.all.Data;
      Node.all.Data    := Swap_C;
   end Swap_Least;

   procedure Swap_Predecessor
     (Within_The_Tree  : in out T;
      Of_The_Key       : in     Key_T;
      With_The_Element : in out Element_T)
   is
      Swap_C : constant Element_T := With_The_Element;
      Node   : AVL_Node_A         := Within_The_Tree.Root;
   begin
      if Node = null then
         raise Key_Not_Found;
      end if;

      loop
         if Is_Less_Than (Of_The_Key, (Node.all.Key)) then
            if Node.all.Link_Kind (Left) = Thread then
               raise Key_Not_Found;
            end if;

            Node := Node.all.LL;
         elsif Is_Less_Than (Node.all.Key, Of_The_Key) then
            if Node.all.Link_Kind (Right) = Thread then
               raise Key_Not_Found;
            end if;

            Node := Node.all.RL;
         else
            case Node.all.Link_Kind (Left) is
               when Child =>
                  Node := Node.all.LL;

                  while Node.all.Link_Kind (Right) /= Thread loop
                     Node := Node.all.RL;
                  end loop;

                  With_The_Element := Node.all.Data;
                  Node.all.Data    := Swap_C;
               when Thread =>
                  if Node.all.LL /= null then
                     Node             := Node.all.LL;
                     With_The_Element := Node.all.Data;
                     Node.all.Data    := Swap_C;
                  end if;
            end case;

            exit;
         end if;
      end loop;
   end Swap_Predecessor;

   procedure Swap_Successor
     (Within_The_Tree  : in out T;
      Of_The_Key       : in     Key_T;
      With_The_Element : in out Element_T)
   is
      Swap_C : constant Element_T := With_The_Element;
      Node   : AVL_Node_A         := Within_The_Tree.Root;
   begin
      loop
         if Is_Less_Than (Of_The_Key, Node.all.Key) then
            if Node.all.Link_Kind (Left) = Thread then
               raise Key_Not_Found;
            end if;

            Node := Node.all.LL;
         elsif Is_Less_Than (Node.all.Key, Of_The_Key) then
            if Node.all.Link_Kind (Right) = Thread then
               raise Key_Not_Found;
            end if;

            Node := Node.all.RL;
         else
            case Node.all.Link_Kind (Right) is
               when Child =>
                  Node := Node.all.RL;

                  while Node.all.Link_Kind (Left) /= Thread loop
                     Node := Node.all.LL;
                  end loop;

                  With_The_Element := Node.all.Data;
                  Node.all.Data    := Swap_C;
               when Thread =>
                  if Node.all.RL /= null then
                     Node             := Node.all.RL;
                     With_The_Element := Node.all.Data;
                     Node.all.Data    := Swap_C;
                  end if;
            end case;

            exit;
         end if;
      end loop;
   end Swap_Successor;

end AVL_Trees;
