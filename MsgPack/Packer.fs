namespace MsgPack

module Packer =
    [<CompiledName("PackBool")>]
    let packBool value =
        if value then
            [| Format.True |]
        else
            [| Format.False |]

    [<CompiledName("PackByte")>]
    let packByte value =
        if value < (1uy <<< 7) then
            [| byte value |]
        else
            [| Format.UInt8
               byte value |]

    [<CompiledName("PackUShort")>]
    let packUInt16 value =
        if value < (1us <<< 8) then
            if value < (1us <<< 7) then
                [| byte value |]
            else
                [| Format.UInt8
                   byte value |]
        else
            [| Format.UInt16
               byte (value >>> 8)
               byte (value &&& 0x00FFus) |]

    [<CompiledName("PackUInt")>]
    let packUInt32 value =
        if value < (1u <<< 8) then
            if value < (1u <<< 7) then
                [| byte value |]
            else
                [| Format.UInt8
                   byte value |]
        else
            if value < (1u <<< 16) then
                [| Format.UInt16
                   byte (value >>> 8)
                   byte (value &&& 0x00FFu) |]
            else
                [| Format.UInt32
                   byte (value >>> 24)
                   byte ((value &&& 0x00FF0000u) >>> 16)
                   byte ((value &&& 0x0000FF00u) >>> 8)
                   byte (value &&& 0x000000FFu) |]

    [<CompiledName("PackULong")>]
    let packUInt64 value =
        if value < (1UL <<< 8) then
            if value < (1UL <<< 7)
                then [| byte value |]
            else
                [| Format.UInt8
                   byte value |]
        else
            if value < (1UL <<< 16) then
                [| Format.UInt16
                   byte (value >>> 8)
                   byte (value &&& 0x00FFUL) |]
            elif value < (1UL <<< 32) then
                [| Format.UInt32
                   byte (value >>> 24)
                   byte ((value &&& 0x00FF0000UL) >>> 16)
                   byte ((value &&& 0x0000FF00UL) >>> 8)
                   byte (value &&& 0x000000FFUL) |]
            else
                [| Format.UInt64
                   byte (value >>> 56)
                   byte ((value &&& 0x00FF000000000000UL) >>> 48)
                   byte ((value &&& 0x0000FF0000000000UL) >>> 40)
                   byte ((value &&& 0x000000FF00000000UL) >>> 32)
                   byte ((value &&& 0x00000000FF000000UL) >>> 24)
                   byte ((value &&& 0x0000000000FF0000UL) >>> 16)
                   byte ((value &&& 0x000000000000FF00UL) >>> 8)
                   byte (value &&& 0x00000000000000FFUL) |]

    [<CompiledName("PackSByte")>]
    let packSByte value =
        if value < -(1y <<< 5) then
            [| Format.Int8
               byte value |]
        else
            [| byte value |]

    [<CompiledName("PackShort")>]
    let packInt16 value =
        if value < -(1s <<< 5) then
            if value < -(1s <<< 7) then
                [| Format.Int16
                   byte (value >>> 8)
                   byte (value &&& 0x00FFs) |]
            else
                [| Format.Int8
                   byte value |]
        elif value < (1s <<< 7) then
            // fixnum
            [| byte value |]
        else
            value |> uint16 |> packUInt16

    [<CompiledName("PackInt")>]
    let packInt value =
        if value < -(1 <<< 5) then
            if value < -(1 <<< 15) then
                [| Format.Int32
                   byte (value >>> 24)
                   byte ((value &&& 0x00FF0000) >>> 16)
                   byte ((value &&& 0x0000FF00) >>> 8)
                   byte (value &&& 0x000000FF) |]
            elif value < -(1 <<< 7) then
                [| Format.Int16
                   byte (value >>> 8)
                   byte (value &&& 0x00FF) |]
            else
                [| Format.Int8
                   byte value |]
        elif value < (1 <<< 7) then
            // fixnum
            [| byte value |]
        else
            value |> uint32 |> packUInt32

    [<CompiledName("PackLong")>]
    let packInt64 value =
        if value < -(1L <<< 5) then
            if value < -(1L <<< 15) then
                if value < -(1L <<< 31) then
                    [| Format.Int64
                       byte (value >>> 56)
                       byte ((value &&& 0x00FF000000000000L) >>> 48)
                       byte ((value &&& 0x0000FF0000000000L) >>> 40)
                       byte ((value &&& 0x000000FF00000000L) >>> 32)
                       byte ((value &&& 0x00000000FF000000L) >>> 24)
                       byte ((value &&& 0x0000000000FF0000L) >>> 16)
                       byte ((value &&& 0x000000000000FF00L) >>> 8)
                       byte (value &&& 0x00000000000000FFL) |]
                else
                    value |> int32 |> packInt
            else
                value |> int32 |> packInt
        elif value < (1L <<< 7) then
            // fixnum
            [| byte value |]
        else
            value |> uint64 |> packUInt64

    [<CompiledName("PackSingle")>]
    let packFloat32 (value: float32) =
        Array.append [| Format.Float32 |] (Utility.convertEndianFromFloat32ToBytes value)

    [<CompiledName("PackDouble")>]
    let packFloat (value: float) =
        Array.append [| Format.Float64 |] (Utility.convertEndianFromFloatToBytes value)

    [<CompiledName("PackNil")>]
    let packNil () =
        [| Format.Nil |]

    [<CompiledName("PackString")>]
    let packString (value: string) =
        let bytes = System.Text.Encoding.UTF8.GetBytes(value)
        let length = bytes.Length
        let (|FixStr|_|) (length: int) =
            if length < 32 then Some(length)
            else None
        let (|Str8|_|) (length: int) =
            if length < 0xFF then Some(length)
            else None
        let (|Str16|_|) (length: int) =
            if length < 0xFFFF then Some(length)
            else None
        (* For now, there is no necessity to think about the string whose length is greater than 2^32-1.
        let (|Str32|_|) (length: int) =
            if length < 0xFFFFFFFF then Some(length)
            else None*)
        match length with
        | FixStr length -> Array.append
                                [| byte (160 + length) |]
                                bytes       // string whose length is upto 31.
        | Str8 length   -> Array.append
                                [| Format.Str8
                                   byte length |]
                                bytes       // string whose length is upto 2^8-1.
        | Str16 length  -> Array.append
                                [| Format.Str16
                                   byte (length >>> 8)
                                   byte (length &&& 0x00FF) |]
                                bytes       // string whose length is upto 2^16-1.
        | _             -> Array.append
                                [| Format.Str32
                                   byte (length >>> 24)
                                   byte ((length &&& 0x00FF0000) >>> 16)
                                   byte ((length &&& 0x0000FF00) >>> 8)
                                   byte (length &&& 0x000000FF) |]
                                bytes       // string whose length is greater than 2^16-1.

    [<CompiledName("PackBinary")>]
    let packBin (bs: byte[]) =
        let length = bs.Length
        if length <= 255 then Array.append [| Format.Bin8; byte(length) |] bs
        elif length <= 65535 then Array.append
                                    [| Format.Bin16
                                       byte (length >>> 8)
                                       byte (length &&& 0x00FF) |]
                                    bs
        else Array.append
                [| Format.Bin32
                   byte (length >>> 24)
                   byte ((length &&& 0x00FF0000) >>> 16)
                   byte ((length &&& 0x0000FF00) >>> 8)
                   byte (length &&& 0x000000FF) |]
                bs

    [<CompiledName("PackExtended")>]
    let packExt (t: sbyte) (bs: byte[]) =
        let length = bs.Length
        if length = 1 then Array.append [| Format.FixExt1; byte(t) |] bs
        elif length = 2 then Array.append [| Format.FixExt2; byte(t) |] bs
        elif 3 <= length && length <= 4 then Array.append [| Format.FixExt4; byte(t) |] bs
        elif 5 <= length && length <= 8 then Array.append [| Format.FixExt8; byte(t) |] bs
        elif 9 <= length && length <= 16 then Array.append [| Format.FixExt16; byte(t) |] bs
        elif length <= 255 then Array.append [| Format.Ext8; byte(length); byte(t) |] bs
        elif length <= 65535 then Array.append
                                    [| Format.Ext16
                                       byte (length >>> 8)
                                       byte (length &&& 0x00FF)
                                       byte (t) |]
                                    bs
        else Array.append
                [| Format.Ext32
                   byte (length >>> 24)
                   byte ((length &&& 0x00FF0000) >>> 16)
                   byte ((length &&& 0x0000FF00) >>> 8)
                   byte (length &&& 0x000000FF)
                   byte (t) |]
                bs

    [<CompiledName("PackOne")>]
    let rec packOne = function
        //TODO: change signature to seq<Value> -> byte[]
        | Value.Nil -> packNil()
        | Value.Bool b -> packBool b
        | Value.Float32 f -> packFloat32 f
        | Value.Float64 f -> packFloat f
        | Value.UInt8 u -> packByte u
        | Value.UInt16 u -> packUInt16 u
        | Value.UInt32 u -> packUInt32 u
        | Value.UInt64 u -> packUInt64 u
        | Value.Int8 i -> packSByte i
        | Value.Int16 i -> packInt16 i
        | Value.Int32 i -> packInt i
        | Value.Int64 i -> packInt64 i
        | Value.String s -> packString s
        | Value.Bin b -> packBin b
        | Value.Array arr ->
            let fmapped = Array.collect packOne arr
            let length = arr.Length
            if length <= 15 then Array.append
                                    [| byte (0b10010000 + length) |]
                                    fmapped
            elif length <= 65535 then Array.append
                                        [| Format.Array16
                                           byte (length >>> 8)
                                           byte (length &&& 0x00FF) |]
                                        fmapped
            else Array.append
                    [| Format.Array32
                       byte (length >>> 24)
                       byte ((length &&& 0x00FF0000) >>> 16)
                       byte ((length &&& 0x0000FF00) >>> 8)
                       byte (length &&& 0x000000FF) |]
                    fmapped
        | Value.Map m ->
            let length = m.Count
            let flatten = Map.toArray m |> Array.collect (fun (k, v) -> Array.append (packOne k) (packOne v))
            if length <= 15 then Array.append
                                    [| byte (0b10000000 + length) |]
                                    flatten
            elif length <= 65535 then Array.append
                                        [| Format.Map16
                                           byte (length >>> 8)
                                           byte (length &&& 0x00FF) |]
                                        flatten
            else Array.append
                    [| Format.Map32
                       byte (length >>> 24)
                       byte ((length &&& 0x00FF0000) >>> 16)
                       byte ((length &&& 0x0000FF00) >>> 8)
                       byte (length &&& 0x000000FF) |]
                    flatten
        | Value.Ext (i, b) -> packExt i b

    [<CompiledName("Pack")>]
    let pack (values: Value list) = values |> List.map packOne |> Array.concat