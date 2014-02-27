namespace MsgPack.Test

open NUnit.Framework
open MsgPack

module TestExtensions =

    let assertEquivalentTo<'a> expected (actual: 'a) = Assert.That(actual, Is.EquivalentTo(expected))

open TestExtensions

[<TestFixture>]
module PackBoolTest =
    [<Test>]
    let ``Given true Then return 0xC2``() =
        true |> Packer.packBool |> assertEquivalentTo [| 0xC2uy |]

    [<Test>]
    let ``Given false Then return 0xC3``() =
        false |> Packer.packBool |> assertEquivalentTo [| 0xC3uy |]

[<TestFixture>]
module PackByteTest =
    [<Test>]
    let ``Given 127 Then return 0x7F``() =
        127uy |> Packer.packByte |> assertEquivalentTo [| 0x7Fuy |]

    [<Test>]
    let ``Given 128 Then return 0xCC80``() =
        128uy |> Packer.packByte |> assertEquivalentTo [| 0xCCuy; 0x80uy |]
        
    [<Test>]
    let ``Given 255 Then return 0xCDFF``() =
        255uy |> Packer.packByte |> assertEquivalentTo [| 0xCCuy; 0xFFuy |]

[<TestFixture>]
module PackUInt16Test =
    [<Test>]
    let ``Given 127 Then return 0x7F``() =
        127us |> Packer.packUInt16 |> assertEquivalentTo [| 0x7Fuy |]

    [<Test>]
    let ``Given 128 Then return 0xCC80``() =
        128us |> Packer.packUInt16 |> assertEquivalentTo [| 0xCCuy; 0x80uy |]
        
    [<Test>]
    let ``Given 255 Then return 0xCCFF``() =
        255us |> Packer.packUInt16 |> assertEquivalentTo [| 0xCCuy; 0xFFuy |]

    [<Test>]
    let ``Given 256 Then return 0xCD0100``() =
        256us |> Packer.packUInt16 |> assertEquivalentTo [| 0xCDuy; 0x01uy; 0x00uy |]

    [<Test>]
    let ``Given 65535 Then return 0xCDFFFF``() =
        65535us |> Packer.packUInt16 |> assertEquivalentTo [| 0xCDuy; 0xFFuy; 0xFFuy |]

[<TestFixture>]
module PackUInt32Test =
    [<Test>]
    let ``Given 127 Then return 0x7F``() =
        127u |> Packer.packUInt32 |> assertEquivalentTo [| 0x7Fuy |]

    [<Test>]
    let ``Given 128 Then return 0xCC80``() =
        128u |> Packer.packUInt32 |> assertEquivalentTo [| 0xCCuy; 0x80uy |]
        
    [<Test>]
    let ``Given 255 Then return 0xCCFF``() =
        255u |> Packer.packUInt32 |> assertEquivalentTo [| 0xCCuy; 0xFFuy |]

    [<Test>]
    let ``Given 256 Then return 0xCD0100``() =
        256u |> Packer.packUInt32 |> assertEquivalentTo [| 0xCDuy; 0x01uy; 0x00uy |]

    [<Test>]
    let ``Given 65535 Then return 0xCDFFFF``() =
        65535u |> Packer.packUInt32 |> assertEquivalentTo [| 0xCDuy; 0xFFuy; 0xFFuy |]

    [<Test>]
    let ``Given 65536 Then return 0xCE00010000``() =
        65536u |> Packer.packUInt32 |> assertEquivalentTo [| 0xCEuy; 0x00uy; 0x01uy; 0x00uy; 0x00uy |]

    [<Test>]
    let ``Given 4294967295 Then return 0xCEFFFFFFFF``() =
        4294967295u |> Packer.packUInt32 |> assertEquivalentTo [| 0xCEuy; 0xFFuy; 0xFFuy; 0xFFuy; 0xFFuy |]

    [<Test>]
    let ``Given 0x12345678 Then return 0xCE12345678``() =
        0x12345678u |> Packer.packUInt32 |> assertEquivalentTo [| 0xCEuy; 0x12uy; 0x34uy; 0x56uy; 0x78uy |]

[<TestFixture>]
module PackUInt64Test =
    [<Test>]
    let ``Given 128 Then return 0xCC80``() =
        128UL |> Packer.packUInt64 |> assertEquivalentTo [| 0xCCuy; 0x80uy |]
        
    [<Test>]
    let ``Given 255 Then return 0xCCFF``() =
        255UL |> Packer.packUInt64 |> assertEquivalentTo [| 0xCCuy; 0xFFuy |]

    [<Test>]
    let ``Given 256 Then return 0xCD0100``() =
        256UL |> Packer.packUInt64 |> assertEquivalentTo [| 0xCDuy; 0x01uy; 0x00uy |]

    [<Test>]
    let ``Given 65535 Then return 0xCDFFFF``() =
        65535UL |> Packer.packUInt64 |> assertEquivalentTo [| 0xCDuy; 0xFFuy; 0xFFuy |]

    [<Test>]
    let ``Given 65536 Then return 0xCE010000``() =
        65536UL |> Packer.packUInt64 |> assertEquivalentTo [| 0xCEuy; 0x00uy; 0x01uy; 0x00uy; 0x00uy |]

    [<Test>]
    let ``Given 4294967295 Then return 0xCEFFFFFFFF``() =
        4294967295UL |> Packer.packUInt64 |> assertEquivalentTo [| 0xCEuy; 0xFFuy; 0xFFuy; 0xFFuy; 0xFFuy |]

    [<Test>]
    let ``Given 4294967296 Then return 0xCF0000000100000000``() =
        4294967296UL |> Packer.packUInt64 |> assertEquivalentTo [| 0xCFuy; 0x00uy; 0x00uy; 0x00uy; 0x01uy; 0x00uy; 0x00uy; 0x00uy; 0x00uy |]

    [<Test>]
    let ``Given 18446744073709551615 Then return 0xCFFFFFFFFFFFFFFFFF``() =
         18446744073709551615UL |> Packer.packUInt64 |> assertEquivalentTo [| 0xCFuy; 0xFFuy; 0xFFuy; 0xFFuy; 0xFFuy; 0xFFuy; 0xFFuy; 0xFFuy; 0xFFuy |]

    [<Test>]
    let ``Given 0x0123456789ABCDEF Then return 0xCE0123456789ABCDEF``() =
        0x0123456789ABCDEFUL |> Packer.packUInt64 |> assertEquivalentTo [| 0xCFuy; 0x01uy; 0x23uy; 0x45uy; 0x67uy; 0x89uy; 0xABuy; 0xCDuy; 0xEFuy |]

[<TestFixture>]
module PackSByteTest =
    [<Test>]
    let ``Given 127 Then return 0x7F`` () =
        127y |> Packer.packSByte |> assertEquivalentTo [| 0x7Fuy |]

    [<Test>]
    let ``Given -32 Then return 0xE0`` () =
        -32y |> Packer.packSByte |> assertEquivalentTo [| 0xE0uy |]

    [<Test>]
    let ``Given -33 Then return 0xD0DF`` () =
        -33y |> Packer.packSByte |> assertEquivalentTo [| 0xD0uy; 0xDFuy |]

    [<Test>]
    let ``Given -128 Then return 0xD080`` () =
        -128y |> Packer.packSByte |> assertEquivalentTo [| 0xD0uy; 0x80uy |]

[<TestFixture>]
module PackInt16Test =
    [<Test>]
    let ``Given 32767 Then return 0xCD7FFF`` () =
        32767s |> Packer.packInt16 |> assertEquivalentTo [| 0xCDuy; 0x7Fuy; 0xFFuy |]

    [<Test>]
    let ``Given 256 Then return 0xCD0100`` () =
        256s |> Packer.packInt16 |> assertEquivalentTo [| 0xCDuy; 0x01uy; 0x00uy |]

    [<Test>]
    let ``Given 255 Then return 0xCCFF`` () =
        255s |> Packer.packInt16 |> assertEquivalentTo [| 0xCCuy; 0xFFuy |]

    [<Test>]
    let ``Given 128 Then return 0xCC80`` () =
        128s |> Packer.packInt16 |> assertEquivalentTo [| 0xCCuy; 0x80uy |]

    [<Test>]
    let ``Given 127 Then return 0x7F`` () =
        127s |> Packer.packInt16 |> assertEquivalentTo [| 0x7Fuy |]

    [<Test>]
    let ``Given -32 Then return 0xE0`` () =
        -32s |> Packer.packInt16 |> assertEquivalentTo [| 0xE0uy |]

    [<Test>]
    let ``Given -33 Then return 0xD0DF`` () =
        -33s |> Packer.packInt16 |> assertEquivalentTo [| 0xD0uy; 0xDFuy |]

    [<Test>]
    let ``Given -128 Then return 0xD080`` () =
        -128s |> Packer.packInt16 |> assertEquivalentTo [| 0xD0uy; 0x80uy |]

    [<Test>]
    let ``Given -129 Then return 0xD1FF7F`` () =
        -129s |> Packer.packInt16 |> assertEquivalentTo [| 0xD1uy; 0xFFuy; 0x7Fuy |]

    [<Test>]
    let ``Given -32768 Then return 0xD18000`` () =
        -32768s |> Packer.packInt16 |> assertEquivalentTo [| 0xD1uy; 0x80uy; 0x00uy |]

[<TestFixture>]
module PackIntTest = 
    [<Test>]
    let ``Given 2147483647 Then return 0xCE7FFFFFFF``() =
        2147483647 |> Packer.packInt |> assertEquivalentTo [| 0xCEuy; 0x7Fuy; 0xFFuy; 0xFFuy; 0xFFuy |]

    [<Test>]
    let ``Given 65536 Then return 0xCE00010000``() =
        65536 |> Packer.packInt |> assertEquivalentTo [| 0xCEuy; 0x00uy; 0x01uy; 0x00uy; 0x00uy |]

    [<Test>]
    let ``Given 65535 Then return 0xCDFFFF``() =
        65535 |> Packer.packInt |> assertEquivalentTo [| 0xCDuy; 0xFFuy; 0xFFuy |]

    [<Test>]
    let ``Given 256 Then return 0xCD0100``() =
        256 |> Packer.packInt |> assertEquivalentTo [| 0xCDuy; 0x01uy; 0x00uy |]

    [<Test>]
    let ``Given 255 Then return 0xCCFF``() =
        255 |> Packer.packInt |> assertEquivalentTo [| 0xCCuy; 0xFFuy |]

    [<Test>]
    let ``Given 128 Then return 0xCC80``() =
        128 |> Packer.packInt |> assertEquivalentTo [| 0xCCuy; 0x80uy |]

    [<Test>]
    let ``Given 127 Then return 0x7F``() =
        127 |> Packer.packInt |> assertEquivalentTo [| 0x7Fuy |]

    [<Test>]
    let ``Given -32 Then return 0xE0``() =
        -32 |> Packer.packInt |> assertEquivalentTo [| 0xE0uy |]

    [<Test>]
    let ``Given -33 Then return 0xD0DF``() =
        -33 |> Packer.packInt |> assertEquivalentTo [| 0xD0uy; 0xDFuy |]

    [<Test>]
    let ``Given -128 Then return 0xD080``() =
        -128 |> Packer.packInt |> assertEquivalentTo [| 0xD0uy; 0x80uy |]

    [<Test>]
    let ``Given -129 Then return 0xD1FF7F``() =
        -129 |> Packer.packInt |> assertEquivalentTo [| 0xD1uy; 0xFFuy; 0x7Fuy |]

    [<Test>]
    let ``Given -32768 Then return 0xD18000``() =
        -32768 |> Packer.packInt |> assertEquivalentTo [| 0xD1uy; 0x80uy; 0x00uy |]

    [<Test>]
    let ``Given -32769 Then return 0xD2FFFF7FFF``() =
        -32769 |> Packer.packInt |> assertEquivalentTo [| 0xD2uy; 0xFFuy; 0xFFuy; 0x7Fuy; 0xFFuy |]

    [<Test>]
    let ``Given -2147483648 Then return 0xD280000000``() =
        -2147483648 |> Packer.packInt |> assertEquivalentTo [| 0xD2uy; 0x80uy; 0x00uy; 0x00uy; 0x00uy |]

[<TestFixture>]
module PackInt64Test =
    [<Test>]
    let ``Given 9223372036854775807 Then return 0xCF7FFFFFFFFFFFFFFF``() =
        9223372036854775807L |> Packer.packInt64 |> assertEquivalentTo [| 0xCFuy; 0x7Fuy; 0xFFuy; 0xFFuy; 0xFFuy; 0xFFuy; 0xFFuy; 0xFFuy; 0xFFuy |]

    [<Test>]
    let ``Given 4294967296L Then return 0xCF0000000100000000``() =
        4294967296L |> Packer.packInt64 |> assertEquivalentTo [| 0xCFuy; 0x00uy; 0x00uy; 0x00uy; 0x01uy; 0x00uy; 0x00uy; 0x00uy; 0x00uy |]

    [<Test>]
    let ``Given 4294967295 Then return 0xCE7FFFFFFF``() =
        4294967295L |> Packer.packInt64 |> assertEquivalentTo [| 0xCEuy; 0xFFuy; 0xFFuy; 0xFFuy; 0xFFuy |]

    [<Test>]
    let ``Given 65536 Then return 0xCE00010000``() =
        65536L |> Packer.packInt64 |> assertEquivalentTo [| 0xCEuy; 0x00uy; 0x01uy; 0x00uy; 0x00uy |]

    [<Test>]
    let ``Given 65535 Then return 0xCDFFFF``() =
        65535L |> Packer.packInt64 |> assertEquivalentTo [| 0xCDuy; 0xFFuy; 0xFFuy |]

    [<Test>]
    let ``Given 256 Then return 0xCD0100``() =
        256L |> Packer.packInt64 |> assertEquivalentTo [| 0xCDuy; 0x01uy; 0x00uy |]

    [<Test>]
    let ``Given 255 Then return 0xCCFF``() =
        255L |> Packer.packInt64 |> assertEquivalentTo [| 0xCCuy; 0xFFuy |]

    [<Test>]
    let ``Given 128 Then return 0xCC80``() =
        128L |> Packer.packInt64 |> assertEquivalentTo [| 0xCCuy; 0x80uy |]

    [<Test>]
    let ``Given 127 Then return 0x7F``() =
        127L |> Packer.packInt64 |> assertEquivalentTo [| 0x7Fuy |]

    [<Test>]
    let ``Given -32 Then return 0xE0``() =
        -32L |> Packer.packInt64 |> assertEquivalentTo [| 0xE0uy |]

    [<Test>]
    let ``Given -33 Then return 0xD0DF``() =
        -33L |> Packer.packInt64 |> assertEquivalentTo [| 0xD0uy; 0xDFuy |]

    [<Test>]
    let ``Given -128 Then return 0xD080``() =
        -128L |> Packer.packInt64 |> assertEquivalentTo [| 0xD0uy; 0x80uy |]

    [<Test>]
    let ``Given -129 Then return 0xD1FF7F``() =
        -129L |> Packer.packInt64 |> assertEquivalentTo [| 0xD1uy; 0xFFuy; 0x7Fuy |]

    [<Test>]
    let ``Given -32768 Then return 0xD18000``() =
        -32768L |> Packer.packInt64 |> assertEquivalentTo [| 0xD1uy; 0x80uy; 0x00uy |]

    [<Test>]
    let ``Given -32769 Then return 0xD2FFFF7FFF``() =
        -32769L |> Packer.packInt64 |> assertEquivalentTo [| 0xD2uy; 0xFFuy; 0xFFuy; 0x7Fuy; 0xFFuy |]

    [<Test>]
    let ``Given -2147483648 Then return 0xD280000000``() =
        -2147483648L |> Packer.packInt64 |> assertEquivalentTo [| 0xD2uy; 0x80uy; 0x00uy; 0x00uy; 0x00uy |]

    [<Test>]
    let ``Given -2147483649 Then return 0xD3FFFFFFFF7FFFFFFF``() =
        -2147483649L |> Packer.packInt64 |> assertEquivalentTo [| 0xD3uy; 0xFFuy; 0xFFuy; 0xFFuy; 0xFFuy; 0x7Fuy; 0xFFuy; 0xFFuy; 0xFFuy |]

    [<Test>]
    let ``Given -9223372036854775808 Then return 0xD38000000000000000``() =
        -9223372036854775808L |> Packer.packInt64 |> assertEquivalentTo [| 0xD3uy; 0x80uy; 0x00uy; 0x00uy; 0x00uy; 0x00uy; 0x00uy; 0x00uy; 0x00uy |]

[<TestFixture>]
module PackFloat32Test =
    [<Test>]
    let ``Given 0.0 Then return 0xCA00000000`` () =
        0.0f |> Packer.packFloat32 |> assertEquivalentTo [| 0xCAuy; 0x00uy; 0x00uy; 0x00uy; 0x00uy |]

    [<Test>]
    let ``Given 1.00390625 Then reutnr 0xCA3F808000`` () =
        1.00390625f |> Packer.packFloat32 |> assertEquivalentTo [| 0xCAuy; 0x3Fuy; 0x80uy; 0x80uy; 0x00uy |]

    [<Test>]
    let ``Given -1.0000152587890625 Then return 0xCABF800080`` () =
        -1.0000152587890625f |> Packer.packFloat32 |> assertEquivalentTo [| 0xCAuy; 0xBFuy; 0x80uy; 0x00uy; 0x80uy |]

    [<Test>]
    let ``Given +infinity Then return 0xCA7F800000`` () =
        System.Single.PositiveInfinity |> Packer.packFloat32 |> assertEquivalentTo [| 0xCAuy; 0x7Fuy; 0x80uy; 0x00uy; 0x00uy |]

    [<Test>]
    let ``Given -infinity Then return 0xCAFF800000`` () =
        System.Single.NegativeInfinity |> Packer.packFloat32 |> assertEquivalentTo [| 0xCAuy; 0xFFuy; 0x80uy; 0x00uy; 0x00uy |]

[<TestFixture>]
module PackFloatTest =
    [<Test>]
    let ``Given 0.0 Then return 0xCB0000000000000000`` () =
        0.0 |> Packer.packFloat |> assertEquivalentTo [| 0xCBuy; 0x00uy; 0x00uy; 0x00uy; 0x00uy; 0x00uy; 0x00uy; 0x00uy; 0x00uy |]

    [<Test>]
    let ``Given 1.03125 Then return 0xCB3FF0800000000000`` () =
        1.03125 |> Packer.packFloat |> assertEquivalentTo [| 0xCBuy; 0x3Fuy; 0xF0uy; 0x80uy; 0x00uy; 0x00uy; 0x00uy; 0x00uy; 0x00uy |]

    [<Test>]
    let ``Given -1.000001430511474609375 Then return 0xCBBFF0000180000000`` () =
        -1.000001430511474609375 |> Packer.packFloat |> assertEquivalentTo [| 0xCBuy; 0xBFuy; 0xF0uy; 0x00uy; 0x01uy; 0x80uy; 0x00uy; 0x00uy; 0x00uy |]

    [<Test>]
    let ``Given 1.000000000015035084288683719933 Then return 0xCB3FF0000000010880`` () =
        1.000000000015035084288683719933 |> Packer.packFloat |> assertEquivalentTo [| 0xCBuy; 0x3Fuy; 0xF0uy; 0x00uy; 0x00uy; 0x00uy; 0x01uy; 0x08uy; 0x80uy |]

    [<Test>]
    let ``Given +infinity Then return 0xCB7FF0000000000000`` () =
        System.Double.PositiveInfinity |> Packer.packFloat |> assertEquivalentTo [| 0xCBuy; 0x7Fuy; 0xF0uy; 0x00uy; 0x00uy; 0x00uy; 0x00uy; 0x00uy; 0x00uy |]

    [<Test>]
    let ``Given -infinity Then return 0xCBFFF0000000000000`` () =
        System.Double.NegativeInfinity |> Packer.packFloat |> assertEquivalentTo [| 0xCBuy; 0xFFuy; 0xF0uy; 0x00uy; 0x00uy; 0x00uy; 0x00uy; 0x00uy; 0x00uy |]

[<TestFixture>]
module PackNilTest =
    [<Test>]
    let ``When packNil Then return 0xC0`` () =
        Packer.packNil() |> assertEquivalentTo [| 0xC0uy |]