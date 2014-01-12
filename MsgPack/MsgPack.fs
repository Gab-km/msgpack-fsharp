namespace MsgPack

type Format =
    static member True = 0xc2uy
    static member False = 0xc3uy
    static member UInt8 = 0xccuy
    static member UInt16 = 0xcduy
    static member UInt32 = 0xceuy

module Serialization =
    let serializeBool value =
        if value then Format.True
        else Format.False

    let serializeInt value =
        if value < (1 <<< 8) then
            if value < (1 <<< 7) then [| byte value |]
            else [| Format.UInt8 ; byte value |]
        else
            if value < (1 <<< 16) then [| Format.UInt16; byte (value >>> 8); byte ((value <<< 8) >>> 8) |]
            else [| Format.UInt32; byte (value >>> 24); byte (((value >>> 16) <<< 24) >>> 24); byte (((value >>> 8) <<< 24) >>> 24); byte ((value <<< 24) >>> 24) |]