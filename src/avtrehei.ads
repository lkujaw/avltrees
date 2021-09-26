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
--  File:          avtrehei.ads (Ada Package Specification)
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

package AVL_Tree_Heights is
   pragma Pure;

   --  As the number of nodes in a tree is bounded by the range of a
   --  pointer, so too is the height of the tree. The range provided
   --  by this type should amply suffice for an AVL tree.
   subtype T is Natural range 0 .. 2 * Integer'Size - 1;

end AVL_Tree_Heights;
