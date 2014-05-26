MessagePack for F#
==================

.. image:: https://ci.appveyor.com/api/projects/status/qghqrl7nwq96aval
    :target: https://ci.appveyor.com/project/Gabkm/msgpack-fsharp
    :alt: Build status

What is this?
-------------

MessagePack is a fast and compact binary serialization library.

MessagePack for F# is a MessagePack implementation of F#, by F#, for F#.

Usage
-----

.. code-block:: fsharp

  open MsgPack

  [| 1uy; 2uy; 3uy |]
  |> Array.map (Value.UInt8)
  |> Value.Array
  |> Packer.packOne
  //=> val it : byte [] = [|147uy; 1uy; 2uy; 3uy|]

  Unpacker.unpack [|147uy; 1uy; 2uy; 3uy|]
  //=> [|Value.Array [|Value.UInt8 1uy; Value.UInt8 2uy; Value.UInt8 3uy|]|]

Copyright
---------

``Copyright (c) 2014- Kazuhiro Matsushima``

License
-------

Distributed under the `Apache License, Version 2.0 <http://www.apache.org/licenses/LICENSE-2.0>`_ .
