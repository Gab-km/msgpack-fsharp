namespace MsgPack.Test

open NUnit.Framework
open MsgPack

module TestExtensions =

    let assertEqual<'a> expected (actual: 'a) = Assert.That(actual, Is.EqualTo(expected))
    let assertEquivalentTo<'a> expected (actual: 'a) = Assert.That(actual, Is.EquivalentTo(expected))

open TestExtensions

[<TestFixture>]
module SerializeBoolTest =
    [<Test>]
    let ``Given true Then return 0xc2``() =
        true |> Serialization.serializeBool |> assertEqual Format.True

    [<Test>]
    let ``Given false Then return 0xc3``() =
        false |> Serialization.serializeBool |> assertEqual Format.False

[<TestFixture>]
module SerializeByteTest =
    [<Test>]
    let ``Given 127 Then return 0xff``() =
        127uy |> Serialization.serializeByte |> assertEquivalentTo [| 0x7Fuy |]

    [<Test>]
    let ``Given 128 Then return 0xccf0``() =
        128uy |> Serialization.serializeByte |> assertEquivalentTo [| Format.UInt8; 0x80uy |]
        
    [<Test>]
    let ``Given 255 Then return 0xcd0100``() =
        255uy |> Serialization.serializeByte |> assertEquivalentTo [| Format.UInt8; 0xFFuy |]

[<TestFixture>]
module SerializeUShortTest =
    [<Test>]
    let ``Given 127 Then return 0xff``() =
        127us |> Serialization.serializeUShort |> assertEquivalentTo [| 0x7Fuy |]

    [<Test>]
    let ``Given 128 Then return 0xccf0``() =
        128us |> Serialization.serializeUShort |> assertEquivalentTo [| Format.UInt8; 0x80uy |]
        
    [<Test>]
    let ``Given 255 Then return 0xcd0100``() =
        255us |> Serialization.serializeUShort |> assertEquivalentTo [| Format.UInt8; 0xFFuy |]

    [<Test>]
    let ``Given 256 Then return 0xcd0100``() =
        256us |> Serialization.serializeUShort |> assertEquivalentTo [| Format.UInt16; 0x01uy; 0x00uy |]

    [<Test>]
    let ``Given 65535 Then return 0xcdffff``() =
        65535us |> Serialization.serializeUShort |> assertEquivalentTo [| Format.UInt16; 0xFFuy; 0xFFuy |]

[<TestFixture>]
module SerializeUIntTest =
    [<Test>]
    let ``Given 127 Then return 0xff``() =
        127u |> Serialization.serializeUInt |> assertEquivalentTo [| 0x7Fuy |]

    [<Test>]
    let ``Given 128 Then return 0xccf0``() =
        128u |> Serialization.serializeUInt |> assertEquivalentTo [| Format.UInt8; 0x80uy |]
        
    [<Test>]
    let ``Given 255 Then return 0xcd0100``() =
        255u |> Serialization.serializeUInt |> assertEquivalentTo [| Format.UInt8; 0xFFuy |]

    [<Test>]
    let ``Given 256 Then return 0xcd0100``() =
        256u |> Serialization.serializeUInt |> assertEquivalentTo [| Format.UInt16; 0x01uy; 0x00uy |]

    [<Test>]
    let ``Given 65535 Then return 0xcdffff``() =
        65535u |> Serialization.serializeUInt |> assertEquivalentTo [| Format.UInt16; 0xFFuy; 0xFFuy |]

    [<Test>]
    let ``Given 65536 Then return 0xce00010000``() =
        65536u |> Serialization.serializeUInt |> assertEquivalentTo [| Format.UInt32; 0x00uy; 0x01uy; 0x00uy; 0x00uy |]

    [<Test>]
    let ``Given UInt32.MaxValue Then return 0xceffffffff``() =
        System.UInt32.MaxValue |> Serialization.serializeUInt |> assertEquivalentTo [| Format.UInt32; 0xFFuy; 0xFFuy; 0xFFuy; 0xFFuy |]

    [<Test>]
    let ``Given 0x12345678 Then return 0xce12345678``() =
        0x12345678u |> Serialization.serializeUInt |> assertEquivalentTo [| Format.UInt32; 0x12uy; 0x34uy; 0x56uy; 0x78uy |]

[<TestFixture>]
module SerializeULongTest =
    [<Test>]
    let ``Given 128 Then return 0xccf0``() =
        128UL |> Serialization.serializeULong |> assertEquivalentTo [| Format.UInt8; 0x80uy |]
        
    [<Test>]
    let ``Given 255 Then return 0xcd0100``() =
        255UL |> Serialization.serializeULong |> assertEquivalentTo [| Format.UInt8; 0xFFuy |]

    [<Test>]
    let ``Given 256 Then return 0xcd0100``() =
        256UL |> Serialization.serializeULong |> assertEquivalentTo [| Format.UInt16; 0x01uy; 0x00uy |]

    [<Test>]
    let ``Given 65535 Then return 0xcdffff``() =
        65535UL |> Serialization.serializeULong |> assertEquivalentTo [| Format.UInt16; 0xFFuy; 0xFFuy |]

    [<Test>]
    let ``Given 65536 Then return 0xce010000``() =
        65536UL |> Serialization.serializeULong |> assertEquivalentTo [| Format.UInt32; 0x00uy; 0x01uy; 0x00uy; 0x00uy |]

    [<Test>]
    let ``Given 4294967295 Then return 0xceffffffff``() =
        4294967295UL |> Serialization.serializeULong |> assertEquivalentTo [| Format.UInt32; 0xFFuy; 0xFFuy; 0xFFuy; 0xFFuy |]

    [<Test>]
    let ``Given 4294967296 Then return 0xcf0000000100000000``() =
        4294967296UL |> Serialization.serializeULong |> assertEquivalentTo [| Format.UInt64; 0x00uy; 0x00uy; 0x00uy; 0x01uy; 0x00uy; 0x00uy; 0x00uy; 0x00uy |]

    [<Test>]
    let ``Given UInt64.MaxValue Then return 0xcfffffffffffffffff``() =
        System.UInt64.MaxValue |> Serialization.serializeULong |> assertEquivalentTo [| Format.UInt64; 0xFFuy; 0xFFuy; 0xFFuy; 0xFFuy; 0xFFuy; 0xFFuy; 0xFFuy; 0xFFuy |]

    [<Test>]
    let ``Given 0x0123456789abcdef Then return 0xce0123456789abcdef``() =
        0x0123456789ABCDEFUL |> Serialization.serializeULong |> assertEquivalentTo [| Format.UInt64; 0x01uy; 0x23uy; 0x45uy; 0x67uy; 0x89uy; 0xABuy; 0xCDuy; 0xEFuy |]

[<TestFixture>]
module SerializeSByteTest =
    [<Test>]
    let ``Given SByte.MaxValue Then return 0x7f`` () =
        System.SByte.MaxValue |> Serialization.serializeSByte |> assertEquivalentTo [| 0x7Fuy |]

    [<Test>]
    let ``Given -32 Then return 0xe0`` () =
        -32y |> Serialization.serializeSByte |> assertEquivalentTo [| 0xE0uy |]

    [<Test>]
    let ``Given -33 Then return 0xd0df`` () =
        -33y |> Serialization.serializeSByte |> assertEquivalentTo [| Format.Int8; 0xDFuy |]

    [<Test>]
    let ``Given SByte.MinValue Then return 0xd080`` () =
        System.SByte.MinValue |> Serialization.serializeSByte |> assertEquivalentTo [| Format.Int8; 0x80uy |]

[<TestFixture>]
module SerializeShortTest =
    [<Test>]
    let ``Given Int16.MaxValue Then return 0xcd7fff`` () =
        System.Int16.MaxValue |> Serialization.serializeShort |> assertEquivalentTo [| Format.UInt16; 0x7Fuy; 0xFFuy |]

    [<Test>]
    let ``Given 256 Then return 0xcd0100`` () =
        256s |> Serialization.serializeShort |> assertEquivalentTo [| Format.UInt16; 0x01uy; 0x00uy |]

    [<Test>]
    let ``Given Byte.MaxValue Then return 0xccff`` () =
        System.Byte.MaxValue |> int16 |> Serialization.serializeShort |> assertEquivalentTo [| Format.UInt8; 0xFFuy |]

    [<Test>]
    let ``Given 128 Then return 0xcc80`` () =
        128s |> Serialization.serializeShort |> assertEquivalentTo [| Format.UInt8; 0x80uy |]

    [<Test>]
    let ``Given SByte.MaxValue Then return 0x7f`` () =
        System.SByte.MaxValue |> int16 |> Serialization.serializeShort |> assertEquivalentTo [| 0x7Fuy |]

    [<Test>]
    let ``Given -32 Then return 0xe0`` () =
        -32s |> Serialization.serializeShort |> assertEquivalentTo [| 0xE0uy |]

    [<Test>]
    let ``Given -33 Then return 0xd0df`` () =
        -33s |> Serialization.serializeShort |> assertEquivalentTo [| Format.Int8; 0xDFuy |]

    [<Test>]
    let ``Given SByte.MinValue Then return 0xd080`` () =
        System.SByte.MinValue |> int16 |> Serialization.serializeShort |> assertEquivalentTo [| Format.Int8; 0x80uy |]

    [<Test>]
    let ``Given -129 Then return 0xd1ff7f`` () =
        -129s |> Serialization.serializeShort |> assertEquivalentTo [| Format.Int16; 0xFFuy; 0x7Fuy |]

    [<Test>]
    let ``Given Int16.MinValue Then return 0xd18000`` () =
        System.Int16.MinValue |> Serialization.serializeShort |> assertEquivalentTo [| Format.Int16; 0x80uy; 0x00uy |]

[<TestFixture>]
module SerializeIntTest = 
    [<Test>]
    let ``Given Int32.MaxValue Then return 0xce7fffffff``() =
        System.Int32.MaxValue |> Serialization.serializeInt |> assertEquivalentTo [| Format.UInt32; 0x7Fuy; 0xFFuy; 0xFFuy; 0xFFuy |]

    [<Test>]
    let ``Given 65536 Then return 0xce00010000``() =
        65536 |> Serialization.serializeInt |> assertEquivalentTo [| Format.UInt32; 0x00uy; 0x01uy; 0x00uy; 0x00uy |]

    [<Test>]
    let ``Given 65535 Then return 0xcdffff``() =
        65535 |> Serialization.serializeInt |> assertEquivalentTo [| Format.UInt16; 0xFFuy; 0xFFuy |]

    [<Test>]
    let ``Given 256 Then return 0xcd0100``() =
        256 |> Serialization.serializeInt |> assertEquivalentTo [| Format.UInt16; 0x01uy; 0x00uy |]

    [<Test>]
    let ``Given 255 Then return 0xccff``() =
        255 |> Serialization.serializeInt |> assertEquivalentTo [| Format.UInt8; 0xFFuy |]

    [<Test>]
    let ``Given 128 Then return 0xcc80``() =
        128 |> Serialization.serializeInt |> assertEquivalentTo [| Format.UInt8; 0x80uy |]

    [<Test>]
    let ``Given 127 Then return 0x7f``() =
        127 |> Serialization.serializeInt |> assertEquivalentTo [| 0x7Fuy |]

    [<Test>]
    let ``Given -32 Then return 0xe0``() =
        -32 |> Serialization.serializeInt |> assertEquivalentTo [| 0xE0uy |]

    [<Test>]
    let ``Given -33 Then return 0xd0df``() =
        -33 |> Serialization.serializeInt |> assertEquivalentTo [| Format.Int8; 0xDFuy |]

    [<Test>]
    let ``Given -128 Then return 0xd080``() =
        -128 |> Serialization.serializeInt |> assertEquivalentTo [| Format.Int8; 0x80uy |]

    [<Test>]
    let ``Given -129 Then return 0xd1ff7f``() =
        -129 |> Serialization.serializeInt |> assertEquivalentTo [| Format.Int16; 0xFFuy; 0x7Fuy |]

    [<Test>]
    let ``Given -32768 Then return 0xd18000``() =
        -32768 |> Serialization.serializeInt |> assertEquivalentTo [| Format.Int16; 0x80uy; 0x00uy |]

    [<Test>]
    let ``Given -32769 Then return 0xd2ffff7fff``() =
        -32769 |> Serialization.serializeInt |> assertEquivalentTo [| Format.Int32; 0xFFuy; 0xFFuy; 0x7Fuy; 0xFFuy |]

    [<Test>]
    let ``Given Int32.MinValue Then return 0xde80000000``() =
        System.Int32.MinValue |> Serialization.serializeInt |> assertEquivalentTo [| Format.Int32; 0x80uy; 0x00uy; 0x00uy; 0x00uy |]

[<TestFixture>]
module SerializeLongTest =
    [<Test>]
    let ``Given Int64.MaxValue Then return 0xcf7fffffffffffffff``() =
        System.Int64.MaxValue |> Serialization.serializeLong |> assertEquivalentTo [| Format.UInt64; 0x7Fuy; 0xFFuy; 0xFFuy; 0xFFuy; 0xFFuy; 0xFFuy; 0xFFuy; 0xFFuy |]

    [<Test>]
    let ``Given 2147483648 Then return 0xcf0000000100000000``() =
        4294967296L |> Serialization.serializeLong |> assertEquivalentTo [| Format.UInt64; 0x00uy; 0x00uy; 0x00uy; 0x01uy; 0x00uy; 0x00uy; 0x00uy; 0x00uy |]

    [<Test>]
    let ``Given 4294967295 Then return 0xce7fffffff``() =
        4294967295L |> Serialization.serializeLong |> assertEquivalentTo [| Format.UInt32; 0xFFuy; 0xffuy; 0xFFuy; 0xFFuy |]

    [<Test>]
    let ``Given 65536 Then return 0xce00010000``() =
        65536L |> Serialization.serializeLong |> assertEquivalentTo [| Format.UInt32; 0x00uy; 0x01uy; 0x00uy; 0x00uy |]

    [<Test>]
    let ``Given 65535 Then return 0xcdffff``() =
        65535L |> Serialization.serializeLong |> assertEquivalentTo [| Format.UInt16; 0xFFuy; 0xFFuy |]

    [<Test>]
    let ``Given 256 Then return 0xcd0100``() =
        256L |> Serialization.serializeLong |> assertEquivalentTo [| Format.UInt16; 0x01uy; 0x00uy |]

    [<Test>]
    let ``Given 255 Then return 0xccff``() =
        255L |> Serialization.serializeLong |> assertEquivalentTo [| Format.UInt8; 0xFFuy |]

    [<Test>]
    let ``Given 128 Then return 0xcc80``() =
        128L |> Serialization.serializeLong |> assertEquivalentTo [| Format.UInt8; 0x80uy |]

    [<Test>]
    let ``Given 127 Then return 0x7f``() =
        127L |> Serialization.serializeLong |> assertEquivalentTo [| 0x7Fuy |]

    [<Test>]
    let ``Given -32 Then return 0xe0``() =
        -32L |> Serialization.serializeLong |> assertEquivalentTo [| 0xE0uy |]

    [<Test>]
    let ``Given -33 Then return 0xd0df``() =
        -33L |> Serialization.serializeLong |> assertEquivalentTo [| Format.Int8; 0xDFuy |]

    [<Test>]
    let ``Given -128 Then return 0xd080``() =
        -128L |> Serialization.serializeLong |> assertEquivalentTo [| Format.Int8; 0x80uy |]

    [<Test>]
    let ``Given -129 Then return 0xd1ff7f``() =
        -129L |> Serialization.serializeLong |> assertEquivalentTo [| Format.Int16; 0xFFuy; 0x7Fuy |]

    [<Test>]
    let ``Given -32768 Then return 0xd18000``() =
        -32768L |> Serialization.serializeLong |> assertEquivalentTo [| Format.Int16; 0x80uy; 0x00uy |]

    [<Test>]
    let ``Given -32769 Then return 0xd2ffff7fff``() =
        -32769L |> Serialization.serializeLong |> assertEquivalentTo [| Format.Int32; 0xFFuy; 0xFFuy; 0x7Fuy; 0xFFuy |]

    [<Test>]
    let ``Given -2147483648 Then return 0xd380000000``() =
        -2147483648L |> Serialization.serializeLong |> assertEquivalentTo [| Format.Int32; 0x80uy; 0x00uy; 0x00uy; 0x00uy |]

    [<Test>]
    let ``Given -2147483649 Then return 0xd4ffffffff7fffffff``() =
        -2147483649L |> Serialization.serializeLong |> assertEquivalentTo [| Format.Int64; 0xFFuy; 0xFFuy; 0xFFuy; 0xFFuy; 0x7Fuy; 0xFFuy; 0xFFuy; 0xFFuy |]

    [<Test>]
    let ``Given Int64.MinValue Then return 0xd48000000000000000``() =
        System.Int64.MinValue |> Serialization.serializeLong |> assertEquivalentTo [| Format.Int64; 0x80uy; 0x00uy; 0x00uy; 0x00uy; 0x00uy; 0x00uy; 0x00uy; 0x00uy |]

[<TestFixture>]
module SerializeFloatTest =
    [<Test>]
    let ``Given 0.0 Then return 0xca00000000`` () =
        0.0f |> Serialization.serializeFloat |> assertEquivalentTo [| Format.Float; 0x00uy; 0x00uy; 0x00uy; 0x00uy |]

    [<Test>]
    let ``Given 1.00390625 Then reutnr 0xca3f808000`` () =
        1.00390625f |> Serialization.serializeFloat |> assertEquivalentTo [| Format.Float; 0x3Fuy; 0x80uy; 0x80uy; 0x00uy |]

    [<Test>]
    let ``Given -1.0000152587890625 Then return 0xbf800080`` () =
        -1.0000152587890625f |> Serialization.serializeFloat |> assertEquivalentTo [| Format.Float; 0xBFuy; 0x80uy; 0x00uy; 0x80uy |]

    [<Test>]
    let ``Given +infinity Then return 0x7f800000`` () =
        System.Single.PositiveInfinity |> Serialization.serializeFloat |> assertEquivalentTo [| Format.Float; 0x7Fuy; 0x80uy; 0x00uy; 0x00uy |]

    [<Test>]
    let ``Given -infinity Then return 0xff800000`` () =
        System.Single.NegativeInfinity |> Serialization.serializeFloat |> assertEquivalentTo [| Format.Float; 0xFFuy; 0x80uy; 0x00uy; 0x00uy |]

[<TestFixture>]
module SerializeDoubleTest =
    [<Test>]
    let ``Given 0.0 Then return 0xcb0000000000000000`` () =
        0.0 |> Serialization.serializeDouble |> assertEquivalentTo [| Format.Double; 0x00uy; 0x00uy; 0x00uy; 0x00uy; 0x00uy; 0x00uy; 0x00uy; 0x00uy |]

    [<Test>]
    let ``Given 1.03125 Then return 0xcb3ff0800000000000`` () =
        1.03125 |> Serialization.serializeDouble |> assertEquivalentTo [| Format.Double; 0x3Fuy; 0xF0uy; 0x80uy; 0x00uy; 0x00uy; 0x00uy; 0x00uy; 0x00uy |]

    [<Test>]
    let ``Given -1.000001430511474609375 Then return 0xcbbff0000180000000`` () =
        -1.000001430511474609375 |> Serialization.serializeDouble |> assertEquivalentTo [| Format.Double; 0xBFuy; 0xF0uy; 0x00uy; 0x01uy; 0x80uy; 0x00uy; 0x00uy; 0x00uy |]

    [<Test>]
    let ``Given 1.000000000015035084288683719933 Then return 0xcb3ff0000000010880`` () =
        1.000000000015035084288683719933 |> Serialization.serializeDouble |> assertEquivalentTo [| Format.Double; 0x3Fuy; 0xF0uy; 0x00uy; 0x00uy; 0x00uy; 0x01uy; 0x08uy; 0x80uy |]

    [<Test>]
    let ``Given +infinity Then return 0xcb7ff0000000000000`` () =
        System.Double.PositiveInfinity |> Serialization.serializeDouble |> assertEquivalentTo [| Format.Double; 0x7Fuy; 0xF0uy; 0x00uy; 0x00uy; 0x00uy; 0x00uy; 0x00uy; 0x00uy |]

    [<Test>]
    let ``Given -infinity Then return 0xcbfff0000000000000`` () =
        System.Double.NegativeInfinity |> Serialization.serializeDouble |> assertEquivalentTo [| Format.Double; 0xFFuy; 0xF0uy; 0x00uy; 0x00uy; 0x00uy; 0x00uy; 0x00uy; 0x00uy |]
