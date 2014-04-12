namespace MsgPack

type Value =
    | Nil
    | Bool of bool
    | Float32 of float32
    | Float64 of float
    | UInt8 of byte
    | UInt16 of uint16
    | UInt32 of uint32
    | UInt64 of uint64
    | Int8 of sbyte
    | Int16 of int16
    | Int32 of int
    | Int64 of int64
    | String of string
    | Bin of byte []
    | Array of Value []
    | Map of Map<Value, Value>
    | Ext of int * byte []

type Format =
    static member Nil = 0xC0uy
    static member False = 0xC2uy
    static member True = 0xC3uy
    static member Float32 = 0xCAuy
    static member Float64 = 0xCBuy
    static member UInt8 = 0xCCuy
    static member UInt16 = 0xCDuy
    static member UInt32 = 0xCEuy
    static member UInt64 = 0xCFuy
    static member Int8 = 0xD0uy
    static member Int16 = 0xD1uy
    static member Int32 = 0xD2uy
    static member Int64 = 0xD3uy
    static member Str8 = 0xD9uy
    static member Str16 = 0xDAuy
    static member Str32 = 0xDBuy
    static member Array16 = 0xDCuy
    static member Array32 = 0xDDuy
    static member Map16 = 0xDEuy
    static member Map32 = 0xDFuy

module internal Utility =
    open System.Runtime.InteropServices
    [<Struct; StructLayout(LayoutKind.Explicit); CompiledName("Single")>]
    type internal Float32 =
        [<FieldOffset(0)>]val mutable Value: float32
        [<FieldOffset(0)>][<DefaultValue>]val mutable Byte0: byte
        [<FieldOffset(1)>][<DefaultValue>]val mutable Byte1: byte
        [<FieldOffset(2)>][<DefaultValue>]val mutable Byte2: byte
        [<FieldOffset(3)>][<DefaultValue>]val mutable Byte3: byte
        member self.ToBytes(isLittleEndian) =
            if isLittleEndian then [| self.Byte3; self.Byte2; self.Byte1; self.Byte0 |]
            else [| self.Byte0; self.Byte1; self.Byte2; self.Byte3 |]

    [<Struct; StructLayout(LayoutKind.Explicit); CompiledName("Double")>]
    type internal Float =
        [<FieldOffset(0)>]val mutable Value: float
        [<FieldOffset(0); DefaultValue>]val mutable Byte0: byte
        [<FieldOffset(1); DefaultValue>]val mutable Byte1: byte
        [<FieldOffset(2); DefaultValue>]val mutable Byte2: byte
        [<FieldOffset(3); DefaultValue>]val mutable Byte3: byte
        [<FieldOffset(4); DefaultValue>]val mutable Byte4: byte
        [<FieldOffset(5); DefaultValue>]val mutable Byte5: byte
        [<FieldOffset(6); DefaultValue>]val mutable Byte6: byte
        [<FieldOffset(7); DefaultValue>]val mutable Byte7: byte
        member self.ToBytes(isLittleEndian) =
            if isLittleEndian then [| self.Byte7; self.Byte6; self.Byte5; self.Byte4; self.Byte3; self.Byte2; self.Byte1; self.Byte0|]
            else [| self.Byte0; self.Byte1; self.Byte2; self.Byte3; self.Byte4; self.Byte5; self.Byte6; self.Byte7 |]

    [<CompiledName("ConvertEndianToSingle")>]
    let convertEndianToFloat32 (value: float32) =
        let f = Float32(Value=value)
        f.ToBytes(System.BitConverter.IsLittleEndian)

    [<CompiledName("ConvertEndianToDouble")>]
    let convertEndianToFloat (value: float) =
        let d = Float(Value=value)
        d.ToBytes(System.BitConverter.IsLittleEndian)

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
               byte ((value <<< 8) >>> 8) |]

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
                   byte ((value <<< 8) >>> 8) |]
            else
                [| Format.UInt32
                   byte (value >>> 24)
                   byte (((value >>> 16) <<< 24) >>> 24)
                   byte (((value >>> 8) <<< 24) >>> 24)
                   byte ((value <<< 24) >>> 24) |]

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
                   byte ((value <<< 8) >>> 8) |]
            elif value < (1UL <<< 32) then
                [| Format.UInt32
                   byte (value >>> 24)
                   byte (((value >>> 16) <<< 24) >>> 24)
                   byte (((value >>> 8) <<< 24) >>> 24)
                   byte ((value <<< 24) >>> 24) |]
            else
                [| Format.UInt64
                   byte (value >>> 56)
                   byte (((value >>> 48) <<< 56) >>> 56)
                   byte (((value >>> 40) <<< 56) >>> 56)
                   byte (((value >>> 32) <<< 56) >>> 56)
                   byte (((value >>> 24) <<< 56) >>> 56)
                   byte (((value >>> 16) <<< 56) >>> 56)
                   byte (((value >>> 8) <<< 56) >>> 56)
                   byte ((value <<< 56) >>> 56) |]

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
                   byte ((value <<< 8) >>> 8) |]
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
                   byte (((value >>> 16) <<< 24) >>> 24)
                   byte (((value >>> 8) <<< 24) >>> 24)
                   byte ((value <<< 24) >>> 24) |]
            elif value < -(1 <<< 7) then
                [| Format.Int16
                   byte (value >>> 8)
                   byte ((value <<< 8) >>> 8) |]
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
                       byte (((value >>> 48) <<< 56) >>> 56)
                       byte (((value >>> 40) <<< 56) >>> 56)
                       byte (((value >>> 32) <<< 56) >>> 56)
                       byte (((value >>> 24) <<< 56) >>> 56)
                       byte (((value >>> 16) <<< 56) >>> 56)
                       byte (((value >>> 8) <<< 56) >>> 56)
                       byte ((value <<< 56) >>> 56) |]
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
        Array.append [| Format.Float32 |] (Utility.convertEndianToFloat32 value)

    [<CompiledName("PackDouble")>]
    let packFloat (value: float) =
        Array.append [| Format.Float64 |] (Utility.convertEndianToFloat value)

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
                                   byte ((length &&& 0xFF00) >>> 8)
                                   byte (length &&& 0x00FF) |]
                                bytes       // string whose length is upto 2^16-1.
        | _             -> Array.append
                                [| Format.Str32
                                   byte ((length &&& 0xFF000000) >>> 24)
                                   byte ((length &&& 0x00FF0000) >>> 16)
                                   byte ((length &&& 0x0000FF00) >>> 8)
                                   byte (length &&& 0x000000FF) |]
                                bytes       // string whose length is greater than 2^16-1.

    let rec pack = function
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
        | Value.Bin b -> [||]
        | Value.Array arr ->
            let fmapped = Array.collect pack arr
            let length = arr.Length
            if length <= 15 then Array.append
                                    [| byte (0b10010000 + length) |]
                                    fmapped
            elif length <= 65535 then Array.append
                                        [| Format.Array16
                                           byte ((length &&& 0xFF00) >>> 8)
                                           byte (length &&& 0x00FF) |]
                                        fmapped
            else Array.append
                    [| Format.Array32
                       byte ((length &&& 0xFF000000) >>> 24)
                       byte ((length &&& 0x00FF0000) >>> 16)
                       byte ((length &&& 0x0000FF00) >>> 8)
                       byte (length &&& 0x000000FF) |]
                    fmapped
        | Value.Map m ->
            let length = m.Count
            let flatten = Map.toArray m |> Array.collect (fun (k, v) -> Array.append (pack k) (pack v))
            if length <= 15 then Array.append
                                    [| byte (0b10000000 + length) |]
                                    flatten
            elif length <= 65535 then Array.append
                                        [| Format.Map16
                                           byte ((length &&& 0xFF00) >>> 8)
                                           byte (length &&& 0x00FF) |]
                                        flatten
            else Array.append
                    [| Format.Map32
                       byte ((length &&& 0xFF000000) >>> 24)
                       byte ((length &&& 0x00FF0000) >>> 16)
                       byte ((length &&& 0x0000FF00) >>> 8)
                       byte (length &&& 0x000000FF) |]
                    flatten
        | Value.Ext (i, b) -> [||]