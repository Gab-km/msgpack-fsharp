#nowarn "9"

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
