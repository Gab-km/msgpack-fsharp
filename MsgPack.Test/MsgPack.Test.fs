namespace MsgPack.Test

open NUnit.Framework
open MsgPack

module TestExtensions =

    let assertEqual<'a> expected (actual: 'a) = Assert.That(actual, Is.EqualTo(expected))
    let assertEquivalentTo<'a> expected (actual: 'a) = Assert.That(actual, Is.EquivalentTo(expected))

open TestExtensions

[<TestFixture>]
module PackBoolTest =
    [<Test>]
    let ``Given true Then return 0xc2``() =
        true |> Packer.packBool |> assertEqual Format.True

    [<Test>]
    let ``Given false Then return 0xc3``() =
        false |> Packer.packBool |> assertEqual Format.False

[<TestFixture>]
module PackByteTest =
    [<Test>]
    let ``Given 127 Then return 0xff``() =
        127uy |> Packer.packByte |> assertEquivalentTo [| 0x7Fuy |]

    [<Test>]
    let ``Given 128 Then return 0xccf0``() =
        128uy |> Packer.packByte |> assertEquivalentTo [| Format.UInt8; 0x80uy |]
        
    [<Test>]
    let ``Given 255 Then return 0xcd0100``() =
        255uy |> Packer.packByte |> assertEquivalentTo [| Format.UInt8; 0xFFuy |]

[<TestFixture>]
module PackUInt16Test =
    [<Test>]
    let ``Given 127 Then return 0xff``() =
        127us |> Packer.packUInt16 |> assertEquivalentTo [| 0x7Fuy |]

    [<Test>]
    let ``Given 128 Then return 0xccf0``() =
        128us |> Packer.packUInt16 |> assertEquivalentTo [| Format.UInt8; 0x80uy |]
        
    [<Test>]
    let ``Given 255 Then return 0xcd0100``() =
        255us |> Packer.packUInt16 |> assertEquivalentTo [| Format.UInt8; 0xFFuy |]

    [<Test>]
    let ``Given 256 Then return 0xcd0100``() =
        256us |> Packer.packUInt16 |> assertEquivalentTo [| Format.UInt16; 0x01uy; 0x00uy |]

    [<Test>]
    let ``Given 65535 Then return 0xcdffff``() =
        65535us |> Packer.packUInt16 |> assertEquivalentTo [| Format.UInt16; 0xFFuy; 0xFFuy |]

[<TestFixture>]
module PackUInt32Test =
    [<Test>]
    let ``Given 127 Then return 0xff``() =
        127u |> Packer.packUInt32 |> assertEquivalentTo [| 0x7Fuy |]

    [<Test>]
    let ``Given 128 Then return 0xccf0``() =
        128u |> Packer.packUInt32 |> assertEquivalentTo [| Format.UInt8; 0x80uy |]
        
    [<Test>]
    let ``Given 255 Then return 0xcd0100``() =
        255u |> Packer.packUInt32 |> assertEquivalentTo [| Format.UInt8; 0xFFuy |]

    [<Test>]
    let ``Given 256 Then return 0xcd0100``() =
        256u |> Packer.packUInt32 |> assertEquivalentTo [| Format.UInt16; 0x01uy; 0x00uy |]

    [<Test>]
    let ``Given 65535 Then return 0xcdffff``() =
        65535u |> Packer.packUInt32 |> assertEquivalentTo [| Format.UInt16; 0xFFuy; 0xFFuy |]

    [<Test>]
    let ``Given 65536 Then return 0xce00010000``() =
        65536u |> Packer.packUInt32 |> assertEquivalentTo [| Format.UInt32; 0x00uy; 0x01uy; 0x00uy; 0x00uy |]

    [<Test>]
    let ``Given UInt32.MaxValue Then return 0xceffffffff``() =
        System.UInt32.MaxValue |> Packer.packUInt32 |> assertEquivalentTo [| Format.UInt32; 0xFFuy; 0xFFuy; 0xFFuy; 0xFFuy |]

    [<Test>]
    let ``Given 0x12345678 Then return 0xce12345678``() =
        0x12345678u |> Packer.packUInt32 |> assertEquivalentTo [| Format.UInt32; 0x12uy; 0x34uy; 0x56uy; 0x78uy |]

[<TestFixture>]
module PackUInt64Test =
    [<Test>]
    let ``Given 128 Then return 0xccf0``() =
        128UL |> Packer.packUInt64 |> assertEquivalentTo [| Format.UInt8; 0x80uy |]
        
    [<Test>]
    let ``Given 255 Then return 0xcd0100``() =
        255UL |> Packer.packUInt64 |> assertEquivalentTo [| Format.UInt8; 0xFFuy |]

    [<Test>]
    let ``Given 256 Then return 0xcd0100``() =
        256UL |> Packer.packUInt64 |> assertEquivalentTo [| Format.UInt16; 0x01uy; 0x00uy |]

    [<Test>]
    let ``Given 65535 Then return 0xcdffff``() =
        65535UL |> Packer.packUInt64 |> assertEquivalentTo [| Format.UInt16; 0xFFuy; 0xFFuy |]

    [<Test>]
    let ``Given 65536 Then return 0xce010000``() =
        65536UL |> Packer.packUInt64 |> assertEquivalentTo [| Format.UInt32; 0x00uy; 0x01uy; 0x00uy; 0x00uy |]

    [<Test>]
    let ``Given 4294967295 Then return 0xceffffffff``() =
        4294967295UL |> Packer.packUInt64 |> assertEquivalentTo [| Format.UInt32; 0xFFuy; 0xFFuy; 0xFFuy; 0xFFuy |]

    [<Test>]
    let ``Given 4294967296 Then return 0xcf0000000100000000``() =
        4294967296UL |> Packer.packUInt64 |> assertEquivalentTo [| Format.UInt64; 0x00uy; 0x00uy; 0x00uy; 0x01uy; 0x00uy; 0x00uy; 0x00uy; 0x00uy |]

    [<Test>]
    let ``Given UInt64.MaxValue Then return 0xcfffffffffffffffff``() =
        System.UInt64.MaxValue |> Packer.packUInt64 |> assertEquivalentTo [| Format.UInt64; 0xFFuy; 0xFFuy; 0xFFuy; 0xFFuy; 0xFFuy; 0xFFuy; 0xFFuy; 0xFFuy |]

    [<Test>]
    let ``Given 0x0123456789abcdef Then return 0xce0123456789abcdef``() =
        0x0123456789ABCDEFUL |> Packer.packUInt64 |> assertEquivalentTo [| Format.UInt64; 0x01uy; 0x23uy; 0x45uy; 0x67uy; 0x89uy; 0xABuy; 0xCDuy; 0xEFuy |]

[<TestFixture>]
module PackSByteTest =
    [<Test>]
    let ``Given SByte.MaxValue Then return 0x7f`` () =
        System.SByte.MaxValue |> Packer.packSByte |> assertEquivalentTo [| 0x7Fuy |]

    [<Test>]
    let ``Given -32 Then return 0xe0`` () =
        -32y |> Packer.packSByte |> assertEquivalentTo [| 0xE0uy |]

    [<Test>]
    let ``Given -33 Then return 0xd0df`` () =
        -33y |> Packer.packSByte |> assertEquivalentTo [| Format.Int8; 0xDFuy |]

    [<Test>]
    let ``Given SByte.MinValue Then return 0xd080`` () =
        System.SByte.MinValue |> Packer.packSByte |> assertEquivalentTo [| Format.Int8; 0x80uy |]

[<TestFixture>]
module PackInt16Test =
    [<Test>]
    let ``Given Int16.MaxValue Then return 0xcd7fff`` () =
        System.Int16.MaxValue |> Packer.packInt16 |> assertEquivalentTo [| Format.UInt16; 0x7Fuy; 0xFFuy |]

    [<Test>]
    let ``Given 256 Then return 0xcd0100`` () =
        256s |> Packer.packInt16 |> assertEquivalentTo [| Format.UInt16; 0x01uy; 0x00uy |]

    [<Test>]
    let ``Given Byte.MaxValue Then return 0xccff`` () =
        System.Byte.MaxValue |> int16 |> Packer.packInt16 |> assertEquivalentTo [| Format.UInt8; 0xFFuy |]

    [<Test>]
    let ``Given 128 Then return 0xcc80`` () =
        128s |> Packer.packInt16 |> assertEquivalentTo [| Format.UInt8; 0x80uy |]

    [<Test>]
    let ``Given SByte.MaxValue Then return 0x7f`` () =
        System.SByte.MaxValue |> int16 |> Packer.packInt16 |> assertEquivalentTo [| 0x7Fuy |]

    [<Test>]
    let ``Given -32 Then return 0xe0`` () =
        -32s |> Packer.packInt16 |> assertEquivalentTo [| 0xE0uy |]

    [<Test>]
    let ``Given -33 Then return 0xd0df`` () =
        -33s |> Packer.packInt16 |> assertEquivalentTo [| Format.Int8; 0xDFuy |]

    [<Test>]
    let ``Given SByte.MinValue Then return 0xd080`` () =
        System.SByte.MinValue |> int16 |> Packer.packInt16 |> assertEquivalentTo [| Format.Int8; 0x80uy |]

    [<Test>]
    let ``Given -129 Then return 0xd1ff7f`` () =
        -129s |> Packer.packInt16 |> assertEquivalentTo [| Format.Int16; 0xFFuy; 0x7Fuy |]

    [<Test>]
    let ``Given Int16.MinValue Then return 0xd18000`` () =
        System.Int16.MinValue |> Packer.packInt16 |> assertEquivalentTo [| Format.Int16; 0x80uy; 0x00uy |]

[<TestFixture>]
module PackIntTest = 
    [<Test>]
    let ``Given Int32.MaxValue Then return 0xce7fffffff``() =
        System.Int32.MaxValue |> Packer.packInt |> assertEquivalentTo [| Format.UInt32; 0x7Fuy; 0xFFuy; 0xFFuy; 0xFFuy |]

    [<Test>]
    let ``Given 65536 Then return 0xce00010000``() =
        65536 |> Packer.packInt |> assertEquivalentTo [| Format.UInt32; 0x00uy; 0x01uy; 0x00uy; 0x00uy |]

    [<Test>]
    let ``Given 65535 Then return 0xcdffff``() =
        65535 |> Packer.packInt |> assertEquivalentTo [| Format.UInt16; 0xFFuy; 0xFFuy |]

    [<Test>]
    let ``Given 256 Then return 0xcd0100``() =
        256 |> Packer.packInt |> assertEquivalentTo [| Format.UInt16; 0x01uy; 0x00uy |]

    [<Test>]
    let ``Given 255 Then return 0xccff``() =
        255 |> Packer.packInt |> assertEquivalentTo [| Format.UInt8; 0xFFuy |]

    [<Test>]
    let ``Given 128 Then return 0xcc80``() =
        128 |> Packer.packInt |> assertEquivalentTo [| Format.UInt8; 0x80uy |]

    [<Test>]
    let ``Given 127 Then return 0x7f``() =
        127 |> Packer.packInt |> assertEquivalentTo [| 0x7Fuy |]

    [<Test>]
    let ``Given -32 Then return 0xe0``() =
        -32 |> Packer.packInt |> assertEquivalentTo [| 0xE0uy |]

    [<Test>]
    let ``Given -33 Then return 0xd0df``() =
        -33 |> Packer.packInt |> assertEquivalentTo [| Format.Int8; 0xDFuy |]

    [<Test>]
    let ``Given -128 Then return 0xd080``() =
        -128 |> Packer.packInt |> assertEquivalentTo [| Format.Int8; 0x80uy |]

    [<Test>]
    let ``Given -129 Then return 0xd1ff7f``() =
        -129 |> Packer.packInt |> assertEquivalentTo [| Format.Int16; 0xFFuy; 0x7Fuy |]

    [<Test>]
    let ``Given -32768 Then return 0xd18000``() =
        -32768 |> Packer.packInt |> assertEquivalentTo [| Format.Int16; 0x80uy; 0x00uy |]

    [<Test>]
    let ``Given -32769 Then return 0xd2ffff7fff``() =
        -32769 |> Packer.packInt |> assertEquivalentTo [| Format.Int32; 0xFFuy; 0xFFuy; 0x7Fuy; 0xFFuy |]

    [<Test>]
    let ``Given Int32.MinValue Then return 0xde80000000``() =
        System.Int32.MinValue |> Packer.packInt |> assertEquivalentTo [| Format.Int32; 0x80uy; 0x00uy; 0x00uy; 0x00uy |]

[<TestFixture>]
module PackInt64Test =
    [<Test>]
    let ``Given Int64.MaxValue Then return 0xcf7fffffffffffffff``() =
        System.Int64.MaxValue |> Packer.packInt64 |> assertEquivalentTo [| Format.UInt64; 0x7Fuy; 0xFFuy; 0xFFuy; 0xFFuy; 0xFFuy; 0xFFuy; 0xFFuy; 0xFFuy |]

    [<Test>]
    let ``Given 2147483648 Then return 0xcf0000000100000000``() =
        4294967296L |> Packer.packInt64 |> assertEquivalentTo [| Format.UInt64; 0x00uy; 0x00uy; 0x00uy; 0x01uy; 0x00uy; 0x00uy; 0x00uy; 0x00uy |]

    [<Test>]
    let ``Given 4294967295 Then return 0xce7fffffff``() =
        4294967295L |> Packer.packInt64 |> assertEquivalentTo [| Format.UInt32; 0xFFuy; 0xffuy; 0xFFuy; 0xFFuy |]

    [<Test>]
    let ``Given 65536 Then return 0xce00010000``() =
        65536L |> Packer.packInt64 |> assertEquivalentTo [| Format.UInt32; 0x00uy; 0x01uy; 0x00uy; 0x00uy |]

    [<Test>]
    let ``Given 65535 Then return 0xcdffff``() =
        65535L |> Packer.packInt64 |> assertEquivalentTo [| Format.UInt16; 0xFFuy; 0xFFuy |]

    [<Test>]
    let ``Given 256 Then return 0xcd0100``() =
        256L |> Packer.packInt64 |> assertEquivalentTo [| Format.UInt16; 0x01uy; 0x00uy |]

    [<Test>]
    let ``Given 255 Then return 0xccff``() =
        255L |> Packer.packInt64 |> assertEquivalentTo [| Format.UInt8; 0xFFuy |]

    [<Test>]
    let ``Given 128 Then return 0xcc80``() =
        128L |> Packer.packInt64 |> assertEquivalentTo [| Format.UInt8; 0x80uy |]

    [<Test>]
    let ``Given 127 Then return 0x7f``() =
        127L |> Packer.packInt64 |> assertEquivalentTo [| 0x7Fuy |]

    [<Test>]
    let ``Given -32 Then return 0xe0``() =
        -32L |> Packer.packInt64 |> assertEquivalentTo [| 0xE0uy |]

    [<Test>]
    let ``Given -33 Then return 0xd0df``() =
        -33L |> Packer.packInt64 |> assertEquivalentTo [| Format.Int8; 0xDFuy |]

    [<Test>]
    let ``Given -128 Then return 0xd080``() =
        -128L |> Packer.packInt64 |> assertEquivalentTo [| Format.Int8; 0x80uy |]

    [<Test>]
    let ``Given -129 Then return 0xd1ff7f``() =
        -129L |> Packer.packInt64 |> assertEquivalentTo [| Format.Int16; 0xFFuy; 0x7Fuy |]

    [<Test>]
    let ``Given -32768 Then return 0xd18000``() =
        -32768L |> Packer.packInt64 |> assertEquivalentTo [| Format.Int16; 0x80uy; 0x00uy |]

    [<Test>]
    let ``Given -32769 Then return 0xd2ffff7fff``() =
        -32769L |> Packer.packInt64 |> assertEquivalentTo [| Format.Int32; 0xFFuy; 0xFFuy; 0x7Fuy; 0xFFuy |]

    [<Test>]
    let ``Given -2147483648 Then return 0xd380000000``() =
        -2147483648L |> Packer.packInt64 |> assertEquivalentTo [| Format.Int32; 0x80uy; 0x00uy; 0x00uy; 0x00uy |]

    [<Test>]
    let ``Given -2147483649 Then return 0xd4ffffffff7fffffff``() =
        -2147483649L |> Packer.packInt64 |> assertEquivalentTo [| Format.Int64; 0xFFuy; 0xFFuy; 0xFFuy; 0xFFuy; 0x7Fuy; 0xFFuy; 0xFFuy; 0xFFuy |]

    [<Test>]
    let ``Given Int64.MinValue Then return 0xd48000000000000000``() =
        System.Int64.MinValue |> Packer.packInt64 |> assertEquivalentTo [| Format.Int64; 0x80uy; 0x00uy; 0x00uy; 0x00uy; 0x00uy; 0x00uy; 0x00uy; 0x00uy |]

[<TestFixture>]
module PackFloat32Test =
    [<Test>]
    let ``Given 0.0 Then return 0xca00000000`` () =
        0.0f |> Packer.packFloat32 |> assertEquivalentTo [| Format.Float32; 0x00uy; 0x00uy; 0x00uy; 0x00uy |]

    [<Test>]
    let ``Given 1.00390625 Then reutnr 0xca3f808000`` () =
        1.00390625f |> Packer.packFloat32 |> assertEquivalentTo [| Format.Float32; 0x3Fuy; 0x80uy; 0x80uy; 0x00uy |]

    [<Test>]
    let ``Given -1.0000152587890625 Then return 0xbf800080`` () =
        -1.0000152587890625f |> Packer.packFloat32 |> assertEquivalentTo [| Format.Float32; 0xBFuy; 0x80uy; 0x00uy; 0x80uy |]

    [<Test>]
    let ``Given +infinity Then return 0x7f800000`` () =
        System.Single.PositiveInfinity |> Packer.packFloat32 |> assertEquivalentTo [| Format.Float32; 0x7Fuy; 0x80uy; 0x00uy; 0x00uy |]

    [<Test>]
    let ``Given -infinity Then return 0xff800000`` () =
        System.Single.NegativeInfinity |> Packer.packFloat32 |> assertEquivalentTo [| Format.Float32; 0xFFuy; 0x80uy; 0x00uy; 0x00uy |]

[<TestFixture>]
module PackFloatTest =
    [<Test>]
    let ``Given 0.0 Then return 0xcb0000000000000000`` () =
        0.0 |> Packer.packFloat |> assertEquivalentTo [| Format.Float64; 0x00uy; 0x00uy; 0x00uy; 0x00uy; 0x00uy; 0x00uy; 0x00uy; 0x00uy |]

    [<Test>]
    let ``Given 1.03125 Then return 0xcb3ff0800000000000`` () =
        1.03125 |> Packer.packFloat |> assertEquivalentTo [| Format.Float64; 0x3Fuy; 0xF0uy; 0x80uy; 0x00uy; 0x00uy; 0x00uy; 0x00uy; 0x00uy |]

    [<Test>]
    let ``Given -1.000001430511474609375 Then return 0xcbbff0000180000000`` () =
        -1.000001430511474609375 |> Packer.packFloat |> assertEquivalentTo [| Format.Float64; 0xBFuy; 0xF0uy; 0x00uy; 0x01uy; 0x80uy; 0x00uy; 0x00uy; 0x00uy |]

    [<Test>]
    let ``Given 1.000000000015035084288683719933 Then return 0xcb3ff0000000010880`` () =
        1.000000000015035084288683719933 |> Packer.packFloat |> assertEquivalentTo [| Format.Float64; 0x3Fuy; 0xF0uy; 0x00uy; 0x00uy; 0x00uy; 0x01uy; 0x08uy; 0x80uy |]

    [<Test>]
    let ``Given +infinity Then return 0xcb7ff0000000000000`` () =
        System.Double.PositiveInfinity |> Packer.packFloat |> assertEquivalentTo [| Format.Float64; 0x7Fuy; 0xF0uy; 0x00uy; 0x00uy; 0x00uy; 0x00uy; 0x00uy; 0x00uy |]

    [<Test>]
    let ``Given -infinity Then return 0xcbfff0000000000000`` () =
        System.Double.NegativeInfinity |> Packer.packFloat |> assertEquivalentTo [| Format.Float64; 0xFFuy; 0xF0uy; 0x00uy; 0x00uy; 0x00uy; 0x00uy; 0x00uy; 0x00uy |]
