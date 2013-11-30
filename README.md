Anaphor Prolog
==============

A dialect of Prolog for building interactive content.

The core idea is the introduction of mutable variables (in addition to the existing logical variables). Mutable variables in a query cause the Prolog search tree (or fragments of it) to become a reactive expression graph. Externally changing the values of mutable variables injected via the query, or exposed in any of the solutions, causes the search tree to react, altering the solutions. In this way a query creates something like a mini spreadsheet.

Certain structures in solutions can be interpreted as graphical forms, or DOM-fragments in an HTML context. Thus interactive graphical or web content can be created via Prolog queries.

At least that's the idea...
