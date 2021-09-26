# AVLTREES

AVLTREES is a threaded, self-balancing, binary trees library for Ada
(1987), based upon the algorithms developed by G. M. Adelson-Velsky
and E. M. Landis, often abbreviated AVL in the literature.

## Usage Example

The following specification provides an example of how to instantiate
the generic package AVL_Array_Key_Trees for the implementation of a
symbol table:

```ada
with AVL_Array_Key_Trees;
with Types;

package AVL_Index_Trees is new AVL_Array_Key_Trees
  (Key_Index_T   => Positive,
   Key_Element_T => Character,
   Key_T         => String,
   Element_T     => Types.Symbol_Record_T,
   Is_Less_Than  => "<");
```

## Ada 1987 Compatibility Note

This package utilizes Ada 1995 pragmas within the following package
specifications:

* AVL_Array_Key_Trees (src/avarketr.ads): Preelaborate
* AVL_Trees (src/avltrees.ads): Preelaborate
* AVL_Tree_Heights (src/avtrehei.ads): Pure

Per section 2.8 of the Ada 1987 Language Reference Manual, "[a] pragma
that is not language-defined has no effect if its identifier is not
recognized by the (current) implementation." However, as these pragmas
are merely advisory to the compiler, they may be removed without
adverse effect from the above files should they cause any issues.

## License and Warranty

AVLTREES is [free software][1]: you can redistribute it and/or
modify it under the terms of the GNU Lesser General Public License
as published by the Free Software Foundation, either version 3 of
the License, or (at your option) any later version.

AVLTREES is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Lesser General Public License for more details.

Copies of the [GNU Lesser General Public License][2] and the [GNU
General Public License][3] are provided within the files named
license.txt and gpl3.txt, respectively; the [Free Software
Foundation][4] also publishes [copies online][5].

`SPDX-License-Identifier: LGPL-3.0-or-later`

[1]: https://www.gnu.org/philosophy/free-sw.en.html
[2]: license.txt
[3]: gpl3.txt
[4]: https://www.fsf.org/
[5]: https://www.gnu.org/licenses/
