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
    | Ext of sbyte * byte []
    override self.ToString() =
        match self with
        | Nil -> "Nil"
        | Bool (b) -> "Bool " + (b.ToString())
        | Float32 (f) -> "Float32 " + (f.ToString())
        | Float64 (d) -> "Float64 " + (d.ToString())
        | UInt8 (u) -> "UInt8 " + (u.ToString())
        | UInt16 (u) -> "UInt16 " + (u.ToString())
        | UInt32 (u) -> "UInt32 " + (u.ToString())
        | UInt64 (u) -> "UInt64 " + (u.ToString())
        | Int8 (i) -> "Int8 " + (i.ToString())
        | Int16 (i) -> "Int16 " + (i.ToString())
        | Int32 (i) -> "Int32 " + (i.ToString())
        | Int64 (i) -> "Int64 " + (i.ToString())
        | String (s) -> "String " + s
        | Bin (bs) -> sprintf "Bin of %d-length binary" bs.Length
        | Array (ar) -> //sprintf "Array of %d-length Value array" ar.Length
                        sprintf "%A" ar
        | Map (m) -> "Map " + (m.ToString())
        | Ext (key, value) -> "Ext " + (key, value).ToString()

type Format =
    static member Nil = 0xC0uy
    static member False = 0xC2uy
    static member True = 0xC3uy
    static member Bin8 = 0xC4uy
    static member Bin16 = 0xC5uy
    static member Bin32 = 0xC6uy
    static member Ext8 = 0xC7uy
    static member Ext16 = 0xC8uy
    static member Ext32 = 0xC9uy
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
    static member FixExt1 = 0xD4uy
    static member FixExt2 = 0xD5uy
    static member FixExt4 = 0xD6uy
    static member FixExt8 = 0xD7uy
    static member FixExt16 = 0xD8uy
    static member Str8 = 0xD9uy
    static member Str16 = 0xDAuy
    static member Str32 = 0xDBuy
    static member Array16 = 0xDCuy
    static member Array32 = 0xDDuy
    static member Map16 = 0xDEuy
    static member Map32 = 0xDFuy

type MessagePackException(message : string) =
    inherit System.Exception(message)

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

    [<CompiledName("ConvertEndianFromSingleToBytes")>]
    let convertEndianFromFloat32ToBytes (value: float32) =
        let f = Float32(Value=value)
        f.ToBytes(System.BitConverter.IsLittleEndian)

    [<CompiledName("ConvertEndianFromDoubleToBytes")>]
    let convertEndianFromFloatToBytes (value: float) =
        let d = Float(Value=value)
        d.ToBytes(System.BitConverter.IsLittleEndian)

    [<CompiledName("ConvertEndianFromBytesToSingle")>]
    let convertEndianFromBytesToFloat32 (bs: byte[]) =
        let f =
            if System.BitConverter.IsLittleEndian then
                Float32(
                    Byte0 = (if bs.Length >= 4 then bs.[3] else 0uy),
                    Byte1 = (if bs.Length >= 3 then bs.[2] else 0uy),
                    Byte2 = (if bs.Length >= 2 then bs.[1] else 0uy),
                    Byte3 = (if bs.Length >= 1 then bs.[0] else 0uy))
            else
                Float32(
                    Byte0 = (if bs.Length >= 1 then bs.[0] else 0uy),
                    Byte1 = (if bs.Length >= 2 then bs.[1] else 0uy),
                    Byte2 = (if bs.Length >= 3 then bs.[2] else 0uy),
                    Byte3 = (if bs.Length >= 4 then bs.[3] else 0uy))
        f.Value

    [<CompiledName("ConvertEndianFromBytesToDouble")>]
    let convertEndianFromBytesToFloat (bs: byte[]) =
        let d =
            if System.BitConverter.IsLittleEndian then
                Float(
                    Byte0 = (if bs.Length >= 8 then bs.[7] else 0uy),
                    Byte1 = (if bs.Length >= 7 then bs.[6] else 0uy),
                    Byte2 = (if bs.Length >= 6 then bs.[5] else 0uy),
                    Byte3 = (if bs.Length >= 5 then bs.[4] else 0uy),
                    Byte4 = (if bs.Length >= 4 then bs.[3] else 0uy),
                    Byte5 = (if bs.Length >= 3 then bs.[2] else 0uy),
                    Byte6 = (if bs.Length >= 2 then bs.[1] else 0uy),
                    Byte7 = (if bs.Length >= 1 then bs.[0] else 0uy))
            else
                Float(
                    Byte0 = (if bs.Length >= 1 then bs.[0] else 0uy),
                    Byte1 = (if bs.Length >= 2 then bs.[1] else 0uy),
                    Byte2 = (if bs.Length >= 3 then bs.[2] else 0uy),
                    Byte3 = (if bs.Length >= 4 then bs.[3] else 0uy),
                    Byte4 = (if bs.Length >= 5 then bs.[4] else 0uy),
                    Byte5 = (if bs.Length >= 6 then bs.[5] else 0uy),
                    Byte6 = (if bs.Length >= 7 then bs.[6] else 0uy),
                    Byte7 = (if bs.Length >= 8 then bs.[7] else 0uy))
        d.Value

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

    [<CompiledName("PackBinary")>]
    let packBin (bs: byte[]) =
        let length = bs.Length
        if length <= 255 then Array.append [| Format.Bin8; byte(length) |] bs
        elif length <= 65535 then Array.append
                                    [| Format.Bin16
                                       byte ((length &&& 0xFF00) >>> 8)
                                       byte (length &&& 0x00FF) |]
                                    bs
        else Array.append
                [| Format.Bin32
                   byte ((length &&& 0xFF000000) >>> 24)
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
                                       byte ((length &&& 0xFF00) >>> 8)
                                       byte (length &&& 0x00FF)
                                       byte (t) |]
                                    bs
        else Array.append
                [| Format.Ext32
                   byte ((length &&& 0xFF000000) >>> 24)
                   byte ((length &&& 0x00FF0000) >>> 16)
                   byte ((length &&& 0x0000FF00) >>> 8)
                   byte (length &&& 0x000000FF)
                   byte (t) |]
                bs

    [<CompiledName("Pack")>]
    let rec pack = function
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
        | Value.Ext (i, b) -> packExt i b

module Unpacker =
    type internal Sequencials =
        | ArrayStore of int * Value []
        | MapStore of int * Value * Map<Value, Value>

    [<CompiledName("Unpack")>]
    let unpack (bs: byte[]) =
        let appendValue (newValue: Value) (sequencials: Sequencials list) (values: Value list) =
            let mutable nv, ars, vs, doLoop = newValue, sequencials, values, true
            while doLoop do
                match ars with
                | (ArrayStore (count, arrayValues))::xs ->
                    arrayValues.[arrayValues.Length - count] <- nv
                    let newCount = count - 1
                    if newCount = 0 then
                        ars <- xs
                        nv <- Value.Array arrayValues
                    else
                        ars <- (ArrayStore(newCount, arrayValues))::xs
                        doLoop <- false
                | (MapStore (count, Value.Nil, mapValues))::xs ->
                    ars <- (MapStore(count, newValue, mapValues))::xs
                    doLoop <- false
                | (MapStore (count, key, mapValues))::xs ->
                    let newMap = Map.add key newValue mapValues
                    let newCount = count - 1
                    if newCount = 0 then
                        ars <- xs
                        nv <- Value.Map newMap
                    else
                        ars <- (MapStore(newCount, Value.Nil, newMap))::xs
                        doLoop <- false
                | _ ->
                    vs <- nv::vs
                    doLoop <- false
            ars, vs

        let _unpackPositiveFixint (bytes: byte[]) (sequencials: Sequencials list) (values: Value list) =
            let ars, vs = appendValue (Value.UInt8 bytes.[0]) sequencials values
            bytes.[1..], ars, vs

        let _unpackFixmap (bytes: byte[]) (sequencials: Sequencials list) =
            let length = int(bytes.[0] &&& 0b00001111uy)
            if bytes.Length - 1 >= length then
                bytes.[1..], MapStore(length, Value.Nil, Map.ofList [])::sequencials
            else
                MessagePackException("Attempt to unpack with non-compatible type") |> raise

        let _unpackFixarray (bytes: byte[]) (sequencials: Sequencials list) =
            let length = int(bytes.[0] &&& 0b00001111uy)
            if bytes.Length - 1 >= length then
                bytes.[1..], (ArrayStore(length, Array.init length (fun _ -> Value.Nil)))::sequencials
            else
                MessagePackException("Attempt to unpack with non-compatible type") |> raise

        let _unpackFixstr (bytes: byte[]) (sequencials: Sequencials list) (values: Value list) =
            let length = int(bytes.[0] &&& 0b00011111uy)
            if bytes.Length - 1 >= length then
                let newValue = System.Text.Encoding.UTF8.GetString(bytes.[1..length]) |> Value.String
                let ars, vs = appendValue newValue sequencials values
                bytes.[(length+1)..], ars, vs
            else
                MessagePackException("Attempt to unpack with non-compatible type") |> raise

        let _unpackNil (bytes: byte[]) (sequencials: Sequencials list) (values: Value list) =
            let ars, vs = appendValue Value.Nil sequencials values
            bytes.[1..], ars, vs

        let _unpackFalse (bytes: byte[]) (sequencials: Sequencials list) (values: Value list) =
            let ars, vs = appendValue (Value.Bool false) sequencials values
            bytes.[1..], ars, vs

        let _unpackTrue (bytes: byte[]) (sequencials: Sequencials list) (values: Value list) =
            let ars, vs = appendValue (Value.Bool true) sequencials values
            bytes.[1..], ars, vs

        let _unpackBin8 (bytes: byte[]) (sequencials: Sequencials list) (values: Value list) =
            if bytes.Length >= 2 then
                let length = int(bytes.[1])
                if bytes.Length - 2 >= length then
                    let newValue = Value.Bin bytes.[2..(length+1)]
                    let ars, vs = appendValue newValue sequencials values
                    bytes.[(length+2)..], ars, vs
                else
                    MessagePackException("Attempt to unpack with non-compatible type") |> raise
            else
                MessagePackException("Attempt to unpack with non-compatible type") |> raise

        let _unpackBin16 (bytes: byte[]) (sequencials: Sequencials list) (values: Value list) =
            if bytes.Length >= 3 then
                let length = int(bytes.[1]) * 256 +
                             int(bytes.[2])
                if bytes.Length - 3 >= length then
                    let newValue = Value.Bin bytes.[3..(length+2)]
                    let ars, vs = appendValue newValue sequencials values
                    bytes.[(length+3)..], ars, vs
                else
                    MessagePackException("Attempt to unpack with non-compatible type") |> raise
            else
                MessagePackException("Attempt to unpack with non-compatible type") |> raise

        let _unpackBin32 (bytes: byte[]) (sequencials: Sequencials list) (values: Value list) =
            if bytes.Length >= 5 then
                let length = int(bytes.[1]) * 16777216 +
                             int(bytes.[2]) * 65536 +
                             int(bytes.[3]) * 256 +
                             int(bytes.[4])
                if bytes.Length - 5 >= length then
                    let newValue = Value.Bin bytes.[5..(length+4)]
                    let ars, vs = appendValue newValue sequencials values
                    bytes.[(length+5)..], ars, vs
                else
                    MessagePackException("Attempt to unpack with non-compatible type") |> raise
            else
                MessagePackException("Attempt to unpack with non-compatible type") |> raise

        let _unpackExt8 (bytes: byte[]) (sequencials: Sequencials list) (values: Value list) =
            if bytes.Length >= 3 then
                let length = int(bytes.[1])
                if bytes.Length - 3 >= length then
                    let t = sbyte(bytes.[2])
                    let d = bytes.[3..(length+2)]
                    let ars, vs = appendValue (Value.Ext(t, d)) sequencials values
                    bytes.[(length+3)..], ars, vs
                else
                    MessagePackException("Attempt to unpack with non-compatible type") |> raise
            else
                MessagePackException("Attempt to unpack with non-compatible type") |> raise

        let _unpackExt16 (bytes: byte[]) (sequencials: Sequencials list) (values: Value list) =
            if bytes.Length >= 4 then
                let length = int(bytes.[1]) * 256 +
                             int(bytes.[2])
                if bytes.Length - 4 >= length then
                    let t = sbyte(bytes.[3])
                    let d = bytes.[4..(length+3)]
                    let ars, vs = appendValue (Value.Ext(t, d)) sequencials values
                    bytes.[(length+4)..], ars, vs
                else
                    MessagePackException("Attempt to unpack with non-compatible type") |> raise
            else
                MessagePackException("Attempt to unpack with non-compatible type") |> raise

        let _unpackExt32 (bytes: byte[]) (sequencials: Sequencials list) (values: Value list) =
            if bytes.Length >= 6 then
                let length = int(bytes.[1]) * 16777216 +
                             int(bytes.[2]) * 65536 +
                             int(bytes.[3]) * 256 +
                             int(bytes.[4])
                if bytes.Length - 6 >= length then
                    let t = sbyte(bytes.[5])
                    let d = bytes.[6..(length+5)]
                    let ars, vs = appendValue (Value.Ext(t, d)) sequencials values
                    bytes.[(length+6)..], ars, vs
                else
                    MessagePackException("Attempt to unpack with non-compatible type") |> raise
            else
                MessagePackException("Attempt to unpack with non-compatible type") |> raise

        let _unpackFloat32 (bytes: byte[]) (sequencials: Sequencials list) (values: Value list) =
            if bytes.Length >= 5 then
                let newValue = Utility.convertEndianFromBytesToFloat32(bytes.[1..4]) |> Value.Float32
                let ars, vs = appendValue newValue sequencials values
                bytes.[5..], ars, vs
            else
                MessagePackException("Attempt to unpack with non-compativle type") |> raise

        let _unpackFloat64 (bytes: byte[]) (sequencials: Sequencials list) (values: Value list) =
            if bytes.Length >= 9 then
                let newValue = Utility.convertEndianFromBytesToFloat(bytes.[1..8]) |> Value.Float64
                let ars, vs = appendValue newValue sequencials values
                bytes.[9..], ars, vs
            else
                MessagePackException("Attempt to unpack with non-compativle type") |> raise

        let _unpackUInt8 (bytes: byte[]) (sequencials: Sequencials list) (values: Value list) =
            if bytes.Length >= 2 then
                let newValue = Value.UInt8 bytes.[1]
                let ars, vs = appendValue newValue sequencials values
                bytes.[2..], ars, vs
            else
                MessagePackException("Attempt to unpack with non-compativle type") |> raise

        let _unpackUInt16 (bytes: byte[]) (sequencials: Sequencials list) (values: Value list) =
            if bytes.Length >= 3 then
                let newValue = uint16(bytes.[1]) * 256us +
                               uint16(bytes.[2])
                               |> Value.UInt16
                let ars, vs = appendValue newValue sequencials values
                bytes.[3..], ars, vs
            else
                MessagePackException("Attempt to unpack with non-compativle type") |> raise

        let _unpackUInt32 (bytes: byte[]) (sequencials: Sequencials list) (values: Value list) =
            if bytes.Length >= 5 then
                let newValue = uint32(bytes.[1]) * 16777216u +
                               uint32(bytes.[2]) * 65536u +
                               uint32(bytes.[3]) * 256u +
                               uint32(bytes.[4])
                               |> Value.UInt32
                let ars, vs = appendValue newValue sequencials values
                bytes.[5..], ars, vs
            else
                MessagePackException("Attempt to unpack with non-compativle type") |> raise

        let _unpackUInt64 (bytes: byte[]) (sequencials: Sequencials list) (values: Value list) =
            if bytes.Length >= 9 then
                let newValue =
                    uint64(bytes.[1]) * 72057594037927936UL +
                    uint64(bytes.[2]) * 281474976710656UL +
                    uint64(bytes.[3]) * 1099511627776UL +
                    uint64(bytes.[4]) * 4294967296UL +
                    uint64(bytes.[5]) * 16777216UL +
                    uint64(bytes.[6]) * 65536UL +
                    uint64(bytes.[7]) * 256UL +
                    uint64(bytes.[8])
                    |> Value.UInt64
                let ars, vs = appendValue newValue sequencials values
                bytes.[9..], ars, vs
            else
                MessagePackException("Attempt to unpack with non-compativle type") |> raise

        let _unpackInt8 (bytes: byte[]) (sequencials: Sequencials list) (values: Value list) =
            if bytes.Length >= 2 then
                let newValue = sbyte(bytes.[1]) |> Value.Int8
                let ars, vs = appendValue newValue sequencials values
                bytes.[2..], ars, vs
            else
                MessagePackException("Attempt to unpack with non-compativle type") |> raise

        let _unpackInt16 (bytes: byte[]) (sequencials: Sequencials list) (values: Value list) =
            if bs.Length >= 3 then
                let newValue = uint16(bytes.[1]) * 256us +
                               uint16(bytes.[2])
                               |> int16 |> Value.Int16
                let ars, vs = appendValue newValue sequencials values
                bytes.[3..], ars, vs
            else
                MessagePackException("Attempt to unpack with non-compativle type") |> raise

        let _unpackInt32 (bytes: byte[]) (sequencials: Sequencials list) (values: Value list) =
            if bytes.Length >= 5 then
                let newValue = uint32(bytes.[1]) * 16777216u +
                               uint32(bytes.[2]) * 65536u +
                               uint32(bytes.[3]) * 256u +
                               uint32(bytes.[4])
                               |> int |> Value.Int32
                let ars, vs = appendValue newValue sequencials values
                bytes.[5..], ars, vs
            else
                MessagePackException("Attempt to unpack with non-compativle type") |> raise

        let _unpackInt64 (bytes: byte[]) (sequencials: Sequencials list) (values: Value list) =
            if bytes.Length >= 9 then
                let newValue =
                    uint64(bytes.[1]) * 72057594037927936UL +
                    uint64(bytes.[2]) * 281474976710656UL +
                    uint64(bytes.[3]) * 1099511627776UL +
                    uint64(bytes.[4]) * 4294967296UL +
                    uint64(bytes.[5]) * 16777216UL +
                    uint64(bytes.[6]) * 65536UL +
                    uint64(bytes.[7]) * 256UL +
                    uint64(bytes.[8])
                    |> int64 |> Value.Int64
                let ars, vs = appendValue newValue sequencials values
                bytes.[9..], ars, vs
            else
                MessagePackException("Attempt to unpack with non-compativle type") |> raise

        let _unpackFixExt1 (bytes: byte[]) (sequencials: Sequencials list) (values: Value list) =
            if bytes.Length >= 3 then
                let t = sbyte(bytes.[1])
                let d = [| bytes.[2] |]
                let ars, vs = appendValue (Value.Ext (t, d)) sequencials values
                bytes.[3..], ars, vs
            else
                MessagePackException("Attempt to unpack with non-compativle type") |> raise

        let _unpackFixExt2 (bytes: byte[]) (sequencials: Sequencials list) (values: Value list) =
            if bytes.Length >= 4 then
                let t = sbyte(bytes.[1])
                let d = bytes.[2..3]
                let ars, vs = appendValue (Value.Ext (t, d)) sequencials values
                bytes.[4..], ars, vs
            else
                MessagePackException("Attempt to unpack with non-compativle type") |> raise

        let _unpackFixExt4 (bytes: byte[]) (sequencials: Sequencials list) (values: Value list) =
            if bytes.Length >= 6 then
                let t = sbyte(bytes.[1])
                let d = bytes.[2..5]
                let ars, vs = appendValue (Value.Ext (t, d)) sequencials values
                bytes.[6..], ars, vs
            else
                MessagePackException("Attempt to unpack with non-compativle type") |> raise

        let _unpackFixExt8 (bytes: byte[]) (sequencials: Sequencials list) (values: Value list) =
            if bytes.Length >= 10 then
                let t = sbyte(bytes.[1])
                let d = bytes.[2..9]
                let ars, vs = appendValue (Value.Ext (t, d)) sequencials values
                bytes.[10..], ars, vs
            else
                MessagePackException("Attempt to unpack with non-compativle type") |> raise

        let _unpackFixExt16 (bytes: byte[]) (sequencials: Sequencials list) (values: Value list) =
            if bytes.Length >= 18 then
                let t = sbyte(bytes.[1])
                let d = bytes.[2..17]
                let ars, vs = appendValue (Value.Ext (t, d)) sequencials values
                bytes.[18..], ars, vs
            else
                MessagePackException("Attempt to unpack with non-compativle type") |> raise

        let _unpackStr8 (bytes: byte[]) (sequencials: Sequencials list) (values: Value list) =
            if bytes.Length >= 2 then
                let length = int(bytes.[1])
                if bytes.Length - 2 >= length then
                    let newValue = System.Text.Encoding.UTF8.GetString(bytes.[2..(length+1)]) |> Value.String
                    let ars, vs = appendValue newValue sequencials values
                    bytes.[(length+2)..], ars, vs
                else
                    MessagePackException("Attempt to unpack with non-compatible type") |> raise
            else
                MessagePackException("Attempt to unpack with non-compativle type") |> raise

        let _unpackStr16 (bytes: byte[]) (sequencials: Sequencials list) (values: Value list) =
            if bytes.Length >= 3 then
                let length = int(bytes.[1]) * 256 +
                             int(bytes.[2])
                if bytes.Length - 3 >= length then
                    let newValue = System.Text.Encoding.UTF8.GetString(bytes.[3..(length+2)]) |> Value.String
                    let ars, vs = appendValue newValue sequencials values
                    bytes.[(length+3)..], ars, vs
                else
                    MessagePackException("Attempt to unpack with non-compatible type") |> raise
            else
                MessagePackException("Attempt to unpack with non-compativle type") |> raise

        let _unpackStr32 (bytes: byte[]) (sequencials: Sequencials list) (values: Value list) =
            if bytes.Length >= 5 then
                let length = int(bytes.[1]) * 16777216 +
                             int(bytes.[2]) * 65536 +
                             int(bytes.[3]) * 256 +
                             int(bytes.[4])
                if bytes.Length - 5 >= length then
                    let newValue = System.Text.Encoding.UTF8.GetString(bytes.[5..(length+4)]) |> Value.String
                    let ars, vs = appendValue newValue sequencials values
                    bytes.[(length+5)..], ars, vs
                else
                    MessagePackException("Attempt to unpack with non-compatible type") |> raise
            else
                MessagePackException("Attempt to unpack with non-compativle type") |> raise

        let _unpackArray16 (bytes: byte[]) (sequencials: Sequencials list) =
            if bytes.Length >= 3 then
                let length = int(bytes.[1]) * 256 +
                             int(bytes.[2])
                if bytes.Length - 3 >= length then
                    bytes.[3..], (ArrayStore(length, Array.init length (fun _ -> Value.Nil)))::sequencials
                else
                    MessagePackException("Attempt to unpack with non-compatible type") |> raise
            else
                MessagePackException("Attempt to unpack with non-compativle type") |> raise

        let _unpackArray32 (bytes: byte[]) (sequencials: Sequencials list) =
            if bytes.Length >= 5 then
                let length = int(bytes.[1]) * 16777216 +
                             int(bytes.[2]) * 65536 +
                             int(bytes.[3]) * 256 +
                             int(bytes.[4])
                if bytes.Length - 5 >= length then
                    bytes.[5..], (ArrayStore(length, Array.init length (fun _ -> Value.Nil)))::sequencials
                else
                    MessagePackException("Attempt to unpack with non-compatible type") |> raise
            else
                MessagePackException("Attempt to unpack with non-compativle type") |> raise

        let _unpackMap16 (bytes: byte[]) (sequencials: Sequencials list) =
            if bytes.Length >= 3 then
                let length = int(bytes.[1]) * 256 +
                             int(bytes.[2])
                if bytes.Length - 3 >= length * 2 then
                    bytes.[3..], (MapStore(length, Value.Nil, Map.ofList []))::sequencials
                else
                    MessagePackException("Attempt to unpack with non-compatible type") |> raise
            else
                MessagePackException("Attempt to unpack with non-compativle type") |> raise

        let _unpackMap32 (bytes: byte[]) (sequencials: Sequencials list) =
            if bytes.Length >= 5 then
                let length = int(bytes.[1]) * 16777216 +
                             int(bytes.[2]) * 65536 +
                             int(bytes.[3]) * 256 +
                             int(bytes.[4])
                if bytes.Length - 5 >= length * 2 then
                    bytes.[5..], (MapStore(length, Value.Nil, Map.ofList []))::sequencials
                else
                    MessagePackException("Attempt to unpack with non-compatible type") |> raise
            else
                MessagePackException("Attempt to unpack with non-compativle type") |> raise

        let _unpackNegativeFixint (bytes: byte[]) (sequencials: Sequencials list) (values: Value list) =
            let newValue = sbyte(bytes.[0]) |> Value.Int8
            let ars, vs = appendValue newValue sequencials values
            bytes.[1..], ars, vs

        let rec _unpack (bs: byte[]) (sequencials: Sequencials list) (values: Value list) =
            if bs.Length = 0 then values
            else
                let header = bs.[0]
                if (header &&& 0b10000000uy) = 0uy then
                    _unpackPositiveFixint bs sequencials values
                    |||> _unpack
                elif (header &&& 0b11110000uy) = 0b10000000uy then
                    _unpackFixmap bs sequencials
                    ||> _unpack <| values
                elif (header &&& 0b11110000uy) = 0b10010000uy then
                    _unpackFixarray bs sequencials
                    ||> _unpack <| values
                elif (header &&& 0b11100000uy) = 0b10100000uy then
                    _unpackFixstr bs sequencials values
                    |||> _unpack
                elif (header = Format.Nil) then
                    _unpackNil bs sequencials values
                    |||> _unpack
                elif (header = Format.False) then
                    _unpackFalse bs sequencials values
                    |||> _unpack
                elif (header = Format.True) then
                    _unpackTrue bs sequencials values
                    |||> _unpack
                elif header = Format.Bin8 then
                    _unpackBin8 bs sequencials values
                    |||> _unpack
                elif header = Format.Bin16 then
                    _unpackBin16 bs sequencials values
                    |||> _unpack
                elif header = Format.Bin32 then
                    _unpackBin32 bs sequencials values
                    |||> _unpack
                elif header = Format.Ext8 then
                    _unpackExt8 bs sequencials values
                    |||> _unpack
                elif header = Format.Ext16 then
                    _unpackExt16 bs sequencials values
                    |||> _unpack
                elif header = Format.Ext32 then
                    _unpackExt32 bs sequencials values
                    |||> _unpack
                elif header = Format.Float32 then
                    _unpackFloat32 bs sequencials values
                    |||> _unpack
                elif header = Format.Float64 then
                    _unpackFloat64 bs sequencials values
                    |||> _unpack
                elif header = Format.UInt8 then
                    _unpackUInt8 bs sequencials values
                    |||> _unpack
                elif header = Format.UInt16 then
                    _unpackUInt16 bs sequencials values
                    |||> _unpack
                elif header = Format.UInt32 then
                    _unpackUInt32 bs sequencials values
                    |||> _unpack
                elif header = Format.UInt64 then
                    _unpackUInt64 bs sequencials values
                    |||> _unpack
                elif header = Format.Int8 then
                    _unpackInt8 bs sequencials values
                    |||> _unpack
                elif header = Format.Int16 then
                    _unpackInt16 bs sequencials values
                    |||> _unpack
                elif header = Format.Int32 then
                    _unpackInt32 bs sequencials values
                    |||> _unpack
                elif header = Format.Int64 then
                    _unpackInt64 bs sequencials values
                    |||> _unpack
                elif header = Format.FixExt1 then
                    _unpackFixExt1 bs sequencials values
                    |||> _unpack
                elif header = Format.FixExt2 then
                    _unpackFixExt2 bs sequencials values
                    |||> _unpack
                elif header = Format.FixExt4 then
                    _unpackFixExt4 bs sequencials values
                    |||> _unpack
                elif header = Format.FixExt8 then
                    _unpackFixExt8 bs sequencials values
                    |||> _unpack
                elif header = Format.FixExt16 then
                    _unpackFixExt16 bs sequencials values
                    |||> _unpack
                elif header = Format.Str8 then
                    _unpackStr8 bs sequencials values
                    |||> _unpack
                elif header = Format.Str16 then
                    _unpackStr16 bs sequencials values
                    |||> _unpack
                elif header = Format.Str32 then
                    _unpackStr32 bs sequencials values
                    |||> _unpack
                elif header = Format.Array16 then
                    _unpackArray16 bs sequencials
                    ||> _unpack <| values
                elif header = Format.Array32 then
                    _unpackArray32 bs sequencials
                    ||> _unpack <| values
                elif header = Format.Map16 then
                    _unpackMap16 bs sequencials
                    ||> _unpack <| values
                elif header = Format.Map32 then
                    _unpackMap32 bs sequencials
                    ||> _unpack <| values
                elif (header &&& 0b11100000uy) = 0b11100000uy then
                    _unpackNegativeFixint bs sequencials values
                    |||> _unpack
                else
                    List.rev values
        _unpack bs [] []