namespace MsgPack.Test.Packer

open NUnit.Framework
open MsgPack

open MsgPack.Test.Extensions

[<TestFixture>]
module WhenUsingPackBool =
    [<Test>]
    let ``Should return 0xC3 with passing true``() =
        true |> Packer.packBool |> assertEquivalentTo [| 0xC3uy |]

    [<Test>]
    let ``Should return 0xC2 with passing false``() =
        false |> Packer.packBool |> assertEquivalentTo [| 0xC2uy |]

[<TestFixture>]
module WhenUsingPackByte =
    [<Test>]
    let ``Should return 0x7F with passing 127``() =
        127uy |> Packer.packByte |> assertEquivalentTo [| 0x7Fuy |]

    [<Test>]
    let ``Should return 0xCC80 with passing 128``() =
        128uy |> Packer.packByte |> assertEquivalentTo [| 0xCCuy; 0x80uy |]
        
    [<Test>]
    let ``Should return 0xCDFF with passing 255``() =
        255uy |> Packer.packByte |> assertEquivalentTo [| 0xCCuy; 0xFFuy |]

[<TestFixture>]
module WhenUsingPackUInt16 =
    [<Test>]
    let ``Should return 0x7F with passing 127``() =
        127us |> Packer.packUInt16 |> assertEquivalentTo [| 0x7Fuy |]

    [<Test>]
    let ``Should return 0xCC80 with passing 128``() =
        128us |> Packer.packUInt16 |> assertEquivalentTo [| 0xCCuy; 0x80uy |]
        
    [<Test>]
    let ``Should return 0xCCFF with passing 255``() =
        255us |> Packer.packUInt16 |> assertEquivalentTo [| 0xCCuy; 0xFFuy |]

    [<Test>]
    let ``Should return 0xCD0100 with passing 256``() =
        256us |> Packer.packUInt16 |> assertEquivalentTo [| 0xCDuy; 0x01uy; 0x00uy |]

    [<Test>]
    let ``Should return 0xCDFFFF with passing 65535``() =
        65535us |> Packer.packUInt16 |> assertEquivalentTo [| 0xCDuy; 0xFFuy; 0xFFuy |]

[<TestFixture>]
module WhenUsingPackUInt32 =
    [<Test>]
    let ``Should return 0x7F with passing 127``() =
        127u |> Packer.packUInt32 |> assertEquivalentTo [| 0x7Fuy |]

    [<Test>]
    let ``Should return 0xCC80 with passing 128``() =
        128u |> Packer.packUInt32 |> assertEquivalentTo [| 0xCCuy; 0x80uy |]
        
    [<Test>]
    let ``Should return 0xCCFF with passing 255``() =
        255u |> Packer.packUInt32 |> assertEquivalentTo [| 0xCCuy; 0xFFuy |]

    [<Test>]
    let ``Should return 0xCD0100 with passing 256``() =
        256u |> Packer.packUInt32 |> assertEquivalentTo [| 0xCDuy; 0x01uy; 0x00uy |]

    [<Test>]
    let ``Should return 0xCDFFFF with passing 65535``() =
        65535u |> Packer.packUInt32 |> assertEquivalentTo [| 0xCDuy; 0xFFuy; 0xFFuy |]

    [<Test>]
    let ``Should return 0xCE00010000 with passing 65536``() =
        65536u |> Packer.packUInt32 |> assertEquivalentTo [| 0xCEuy; 0x00uy; 0x01uy; 0x00uy; 0x00uy |]

    [<Test>]
    let ``Should return 0xCEFFFFFFFF with passing 4294967295``() =
        4294967295u |> Packer.packUInt32 |> assertEquivalentTo [| 0xCEuy; 0xFFuy; 0xFFuy; 0xFFuy; 0xFFuy |]

    [<Test>]
    let ``Should return 0xCE12345678 with passing 0x12345678``() =
        0x12345678u |> Packer.packUInt32 |> assertEquivalentTo [| 0xCEuy; 0x12uy; 0x34uy; 0x56uy; 0x78uy |]

[<TestFixture>]
module WhenUsingPackUInt64 =
    [<Test>]
    let ``Should return 0xCC80 with passing 128``() =
        128UL |> Packer.packUInt64 |> assertEquivalentTo [| 0xCCuy; 0x80uy |]
        
    [<Test>]
    let ``Should return 0xCCFF with passing 255``() =
        255UL |> Packer.packUInt64 |> assertEquivalentTo [| 0xCCuy; 0xFFuy |]

    [<Test>]
    let ``Should return 0xCD0100 with passing 256``() =
        256UL |> Packer.packUInt64 |> assertEquivalentTo [| 0xCDuy; 0x01uy; 0x00uy |]

    [<Test>]
    let ``Should return 0xCDFFFF with passing 65535``() =
        65535UL |> Packer.packUInt64 |> assertEquivalentTo [| 0xCDuy; 0xFFuy; 0xFFuy |]

    [<Test>]
    let ``Should return 0xCE010000 with passing 65536``() =
        65536UL |> Packer.packUInt64 |> assertEquivalentTo [| 0xCEuy; 0x00uy; 0x01uy; 0x00uy; 0x00uy |]

    [<Test>]
    let ``Should return 0xCEFFFFFFFF with passing 4294967295``() =
        4294967295UL |> Packer.packUInt64 |> assertEquivalentTo [| 0xCEuy; 0xFFuy; 0xFFuy; 0xFFuy; 0xFFuy |]

    [<Test>]
    let ``Should return 0xCF0000000100000000 with passing 4294967296``() =
        4294967296UL |> Packer.packUInt64 |> assertEquivalentTo [| 0xCFuy; 0x00uy; 0x00uy; 0x00uy; 0x01uy; 0x00uy; 0x00uy; 0x00uy; 0x00uy |]

    [<Test>]
    let ``Should return 0xCFFFFFFFFFFFFFFFFF with passing 18446744073709551615``() =
         18446744073709551615UL |> Packer.packUInt64 |> assertEquivalentTo [| 0xCFuy; 0xFFuy; 0xFFuy; 0xFFuy; 0xFFuy; 0xFFuy; 0xFFuy; 0xFFuy; 0xFFuy |]

    [<Test>]
    let ``Should return 0xCE0123456789ABCDEF with passing 0x0123456789ABCDEF``() =
        0x0123456789ABCDEFUL |> Packer.packUInt64 |> assertEquivalentTo [| 0xCFuy; 0x01uy; 0x23uy; 0x45uy; 0x67uy; 0x89uy; 0xABuy; 0xCDuy; 0xEFuy |]

[<TestFixture>]
module WhenUsingPackSByte =
    [<Test>]
    let ``Should return 0x7F with passing 127`` () =
        127y |> Packer.packSByte |> assertEquivalentTo [| 0x7Fuy |]

    [<Test>]
    let ``Should return 0xE0 with passing -32`` () =
        -32y |> Packer.packSByte |> assertEquivalentTo [| 0xE0uy |]

    [<Test>]
    let ``Should return 0xD0DF with passing -33`` () =
        -33y |> Packer.packSByte |> assertEquivalentTo [| 0xD0uy; 0xDFuy |]

    [<Test>]
    let ``Should return 0xD080 with passing -128`` () =
        -128y |> Packer.packSByte |> assertEquivalentTo [| 0xD0uy; 0x80uy |]

[<TestFixture>]
module WhenUsingPackInt16 =
    [<Test>]
    let ``Should return 0xCD7FFF with passing 32767`` () =
        32767s |> Packer.packInt16 |> assertEquivalentTo [| 0xCDuy; 0x7Fuy; 0xFFuy |]

    [<Test>]
    let ``Should return 0xCD0100 with passing 256`` () =
        256s |> Packer.packInt16 |> assertEquivalentTo [| 0xCDuy; 0x01uy; 0x00uy |]

    [<Test>]
    let ``Should return 0xCCFF with passing 255`` () =
        255s |> Packer.packInt16 |> assertEquivalentTo [| 0xCCuy; 0xFFuy |]

    [<Test>]
    let ``Should return 0xCC80 with passing 128`` () =
        128s |> Packer.packInt16 |> assertEquivalentTo [| 0xCCuy; 0x80uy |]

    [<Test>]
    let ``Should return 0x7F with passing 127`` () =
        127s |> Packer.packInt16 |> assertEquivalentTo [| 0x7Fuy |]

    [<Test>]
    let ``Should return 0xE0 with passing -32`` () =
        -32s |> Packer.packInt16 |> assertEquivalentTo [| 0xE0uy |]

    [<Test>]
    let ``Should return 0xD0DF with passing -33`` () =
        -33s |> Packer.packInt16 |> assertEquivalentTo [| 0xD0uy; 0xDFuy |]

    [<Test>]
    let ``Should return 0xD080 with passing -128`` () =
        -128s |> Packer.packInt16 |> assertEquivalentTo [| 0xD0uy; 0x80uy |]

    [<Test>]
    let ``Should return 0xD1FF7F with passing -129`` () =
        -129s |> Packer.packInt16 |> assertEquivalentTo [| 0xD1uy; 0xFFuy; 0x7Fuy |]

    [<Test>]
    let ``Should return 0xD18000 with passing -32768`` () =
        -32768s |> Packer.packInt16 |> assertEquivalentTo [| 0xD1uy; 0x80uy; 0x00uy |]

[<TestFixture>]
module WhenUsingPackInt = 
    [<Test>]
    let ``Should return 0xCE7FFFFFFF with passing 2147483647``() =
        2147483647 |> Packer.packInt |> assertEquivalentTo [| 0xCEuy; 0x7Fuy; 0xFFuy; 0xFFuy; 0xFFuy |]

    [<Test>]
    let ``Should return 0xCE00010000 with passing 65536``() =
        65536 |> Packer.packInt |> assertEquivalentTo [| 0xCEuy; 0x00uy; 0x01uy; 0x00uy; 0x00uy |]

    [<Test>]
    let ``Should return 0xCDFFFF with passing 65535``() =
        65535 |> Packer.packInt |> assertEquivalentTo [| 0xCDuy; 0xFFuy; 0xFFuy |]

    [<Test>]
    let ``Should return 0xCD0100 with passing 256``() =
        256 |> Packer.packInt |> assertEquivalentTo [| 0xCDuy; 0x01uy; 0x00uy |]

    [<Test>]
    let ``Should return 0xCCFF with passing 255``() =
        255 |> Packer.packInt |> assertEquivalentTo [| 0xCCuy; 0xFFuy |]

    [<Test>]
    let ``Should return 0xCC80 with passing 128``() =
        128 |> Packer.packInt |> assertEquivalentTo [| 0xCCuy; 0x80uy |]

    [<Test>]
    let ``Should return 0x7F with passing 127``() =
        127 |> Packer.packInt |> assertEquivalentTo [| 0x7Fuy |]

    [<Test>]
    let ``Should return 0xE0 with passing -32``() =
        -32 |> Packer.packInt |> assertEquivalentTo [| 0xE0uy |]

    [<Test>]
    let ``Should return 0xD0DF with passing -33``() =
        -33 |> Packer.packInt |> assertEquivalentTo [| 0xD0uy; 0xDFuy |]

    [<Test>]
    let ``Should return 0xD080 with passing -128``() =
        -128 |> Packer.packInt |> assertEquivalentTo [| 0xD0uy; 0x80uy |]

    [<Test>]
    let ``Should return 0xD1FF7F with passing -129``() =
        -129 |> Packer.packInt |> assertEquivalentTo [| 0xD1uy; 0xFFuy; 0x7Fuy |]

    [<Test>]
    let ``Should return 0xD18000 with passing -32768``() =
        -32768 |> Packer.packInt |> assertEquivalentTo [| 0xD1uy; 0x80uy; 0x00uy |]

    [<Test>]
    let ``Should return 0xD2FFFF7FFF with passing -32769``() =
        -32769 |> Packer.packInt |> assertEquivalentTo [| 0xD2uy; 0xFFuy; 0xFFuy; 0x7Fuy; 0xFFuy |]

    [<Test>]
    let ``Should return 0xD280000000 with passing -2147483648``() =
        -2147483648 |> Packer.packInt |> assertEquivalentTo [| 0xD2uy; 0x80uy; 0x00uy; 0x00uy; 0x00uy |]

[<TestFixture>]
module WhenUsingPackInt64 =
    [<Test>]
    let ``Should return 0xCF7FFFFFFFFFFFFFFF with passing 9223372036854775807``() =
        9223372036854775807L |> Packer.packInt64 |> assertEquivalentTo [| 0xCFuy; 0x7Fuy; 0xFFuy; 0xFFuy; 0xFFuy; 0xFFuy; 0xFFuy; 0xFFuy; 0xFFuy |]

    [<Test>]
    let ``Should return 0xCF0000000100000000 with passing 4294967296``() =
        4294967296L |> Packer.packInt64 |> assertEquivalentTo [| 0xCFuy; 0x00uy; 0x00uy; 0x00uy; 0x01uy; 0x00uy; 0x00uy; 0x00uy; 0x00uy |]

    [<Test>]
    let ``Should return 0xCE7FFFFFFF with passing 4294967295``() =
        4294967295L |> Packer.packInt64 |> assertEquivalentTo [| 0xCEuy; 0xFFuy; 0xFFuy; 0xFFuy; 0xFFuy |]

    [<Test>]
    let ``Should return 0xCE00010000 with passing 65536``() =
        65536L |> Packer.packInt64 |> assertEquivalentTo [| 0xCEuy; 0x00uy; 0x01uy; 0x00uy; 0x00uy |]

    [<Test>]
    let ``Should return 0xCDFFFF with passing 65535``() =
        65535L |> Packer.packInt64 |> assertEquivalentTo [| 0xCDuy; 0xFFuy; 0xFFuy |]

    [<Test>]
    let ``Should return 0xCD0100 with passing 256``() =
        256L |> Packer.packInt64 |> assertEquivalentTo [| 0xCDuy; 0x01uy; 0x00uy |]

    [<Test>]
    let ``Should return 0xCCFF with passing 255``() =
        255L |> Packer.packInt64 |> assertEquivalentTo [| 0xCCuy; 0xFFuy |]

    [<Test>]
    let ``Should return 0xCC80 with passing 128``() =
        128L |> Packer.packInt64 |> assertEquivalentTo [| 0xCCuy; 0x80uy |]

    [<Test>]
    let ``Should return 0x7F with passing 127``() =
        127L |> Packer.packInt64 |> assertEquivalentTo [| 0x7Fuy |]

    [<Test>]
    let ``Should return 0xE0 with passing -32``() =
        -32L |> Packer.packInt64 |> assertEquivalentTo [| 0xE0uy |]

    [<Test>]
    let ``Should return 0xD0DF with passing -33``() =
        -33L |> Packer.packInt64 |> assertEquivalentTo [| 0xD0uy; 0xDFuy |]

    [<Test>]
    let ``Should return 0xD080 with passing -128``() =
        -128L |> Packer.packInt64 |> assertEquivalentTo [| 0xD0uy; 0x80uy |]

    [<Test>]
    let ``Should return 0xD1FF7F with passing -129``() =
        -129L |> Packer.packInt64 |> assertEquivalentTo [| 0xD1uy; 0xFFuy; 0x7Fuy |]

    [<Test>]
    let ``Should return 0xD18000 with passing -32768``() =
        -32768L |> Packer.packInt64 |> assertEquivalentTo [| 0xD1uy; 0x80uy; 0x00uy |]

    [<Test>]
    let ``Should return 0xD2FFFF7FFF with passing -32769``() =
        -32769L |> Packer.packInt64 |> assertEquivalentTo [| 0xD2uy; 0xFFuy; 0xFFuy; 0x7Fuy; 0xFFuy |]

    [<Test>]
    let ``Should return 0xD280000000 with passing -2147483648``() =
        -2147483648L |> Packer.packInt64 |> assertEquivalentTo [| 0xD2uy; 0x80uy; 0x00uy; 0x00uy; 0x00uy |]

    [<Test>]
    let ``Should return 0xD3FFFFFFFF7FFFFFFF with passing -2147483649``() =
        -2147483649L |> Packer.packInt64 |> assertEquivalentTo [| 0xD3uy; 0xFFuy; 0xFFuy; 0xFFuy; 0xFFuy; 0x7Fuy; 0xFFuy; 0xFFuy; 0xFFuy |]

    [<Test>]
    let ``Should return 0xD38000000000000000 with passing -9223372036854775808``() =
        -9223372036854775808L |> Packer.packInt64 |> assertEquivalentTo [| 0xD3uy; 0x80uy; 0x00uy; 0x00uy; 0x00uy; 0x00uy; 0x00uy; 0x00uy; 0x00uy |]

[<TestFixture>]
module WhenUsingPackFloat32 =
    [<Test>]
    let ``Should return 0xCA00000000 with passing 0.0`` () =
        0.0f |> Packer.packFloat32 |> assertEquivalentTo [| 0xCAuy; 0x00uy; 0x00uy; 0x00uy; 0x00uy |]

    [<Test>]
    let ``Should reutnr 0xCA3F808000 with passing 1.00390625`` () =
        1.00390625f |> Packer.packFloat32 |> assertEquivalentTo [| 0xCAuy; 0x3Fuy; 0x80uy; 0x80uy; 0x00uy |]

    [<Test>]
    let ``Should return 0xCABF800080 with passing -1.0000152587890625`` () =
        -1.0000152587890625f |> Packer.packFloat32 |> assertEquivalentTo [| 0xCAuy; 0xBFuy; 0x80uy; 0x00uy; 0x80uy |]

    [<Test>]
    let ``Should return 0xCA7F800000 with passing +infinity`` () =
        System.Single.PositiveInfinity |> Packer.packFloat32 |> assertEquivalentTo [| 0xCAuy; 0x7Fuy; 0x80uy; 0x00uy; 0x00uy |]

    [<Test>]
    let ``Should return 0xCAFF800000 with passing -infinity`` () =
        System.Single.NegativeInfinity |> Packer.packFloat32 |> assertEquivalentTo [| 0xCAuy; 0xFFuy; 0x80uy; 0x00uy; 0x00uy |]

[<TestFixture>]
module WhenUsingPackFloat =
    [<Test>]
    let ``Should return 0xCB0000000000000000 with passing 0.0`` () =
        0.0 |> Packer.packFloat |> assertEquivalentTo [| 0xCBuy; 0x00uy; 0x00uy; 0x00uy; 0x00uy; 0x00uy; 0x00uy; 0x00uy; 0x00uy |]

    [<Test>]
    let ``Should return 0xCB3FF0800000000000 with passing 1.03125`` () =
        1.03125 |> Packer.packFloat |> assertEquivalentTo [| 0xCBuy; 0x3Fuy; 0xF0uy; 0x80uy; 0x00uy; 0x00uy; 0x00uy; 0x00uy; 0x00uy |]

    [<Test>]
    let ``Should return 0xCBBFF0000180000000 with passing -1.000001430511474609375`` () =
        -1.000001430511474609375 |> Packer.packFloat |> assertEquivalentTo [| 0xCBuy; 0xBFuy; 0xF0uy; 0x00uy; 0x01uy; 0x80uy; 0x00uy; 0x00uy; 0x00uy |]

    [<Test>]
    let ``Should return 0xCB3FF0000000010880 with passing 1.000000000015035084288683719933`` () =
        1.000000000015035084288683719933 |> Packer.packFloat |> assertEquivalentTo [| 0xCBuy; 0x3Fuy; 0xF0uy; 0x00uy; 0x00uy; 0x00uy; 0x01uy; 0x08uy; 0x80uy |]

    [<Test>]
    let ``Should return 0xCB7FF0000000000000 with passing +infinity`` () =
        System.Double.PositiveInfinity |> Packer.packFloat |> assertEquivalentTo [| 0xCBuy; 0x7Fuy; 0xF0uy; 0x00uy; 0x00uy; 0x00uy; 0x00uy; 0x00uy; 0x00uy |]

    [<Test>]
    let ``Should return 0xCBFFF0000000000000 with passing -infinity`` () =
        System.Double.NegativeInfinity |> Packer.packFloat |> assertEquivalentTo [| 0xCBuy; 0xFFuy; 0xF0uy; 0x00uy; 0x00uy; 0x00uy; 0x00uy; 0x00uy; 0x00uy |]

[<TestFixture>]
module WhenUsingPackNil =
    [<Test>]
    let ``Should return 0xC0`` () =
        Packer.packNil() |> assertEquivalentTo [| 0xC0uy |]

[<TestFixture>]
module WhenUsingPackString =
    [<Test>]
    let ``Should return 0xA7636F6D70616374 with passing "compact"`` () =
        "compact" |> Packer.packString |> assertEquivalentTo [| 0xA7uy; 0x63uy; 0x6Fuy; 0x6Duy; 0x70uy; 0x61uy; 0x63uy; 0x74uy |]

    [<Test>]
    let ``Should return byte[] and its format header is 0xD9 and its length is 45 with passing "The quick brown fox jumps over the lazy dog"(the length is 43)`` () =
        let sut = "The quick brown fox jumps over the lazy dog" |> Packer.packString
        sut.Length |> assertEqualTo 45
        sut.[0..1] |> assertEquivalentTo [| 0xD9uy; 0x2Buy |]

    [<Test>]
    let ``Should return byte[] and its format header is 0xDA and its length is 261 with passing 258-length string`` () =
        let sut = System.String('a', 258) |> Packer.packString
        sut.Length |> assertEqualTo 261
        sut.[0..2] |> assertEquivalentTo [| 0xDAuy; 0x01uy; 0x02uy |]

    [<Test>]
    let ``Should return byte[] and its format header is 0xDB and its length is 16909065 with passing 16909060-length string`` () =
        let sut = System.String('a', 16909060) |> Packer.packString
        sut.Length |> assertEqualTo 16909065
        sut.[0..4] |> assertEquivalentTo [| 0xDBuy; 0x01uy; 0x02uy; 0x03uy; 0x04uy |]

[<TestFixture>]
module WhenUsingPackBin =
    [<Test>]
    let ``Should return byte[] and its format header is 0xC4 and its length is 257 with passing [| 0 .. 254 |]`` () =
        let sut = [| 0uy .. 254uy |] |> Packer.packBin
        sut.Length |> assertEqualTo 257
        sut.[0..1] |> assertEquivalentTo [| 0xC4uy; 0xFFuy |]

    [<Test>]
    let ``Should return byte[] and its format header is 0xC5 and its length is 259 with passing [| 0 .. 255 |]`` () =
        let sut = [| 0uy .. 255uy |] |> Packer.packBin
        sut.Length |> assertEqualTo 259
        sut.[0..2] |> assertEquivalentTo [| 0xC5uy; 0x01uy; 0x00uy |]

    [<Test>]
    let ``Should return byte[] and its format header is 0xC5 and its length is 65538 with passing 65535-length bin array`` () =
        let sut = Array.create 65535 0uy |> Packer.packBin
        sut.Length |> assertEqualTo 65538
        sut.[0..2] |> assertEquivalentTo [| 0xC5uy; 0xFFuy; 0xFFuy |]

    [<Test>]
    let ``Should return byte[] and its format header is 0xC6 and its length is 65541 with passing 65536-length bin array`` () =
        let sut = Array.create 65536 0uy |> Packer.packBin
        sut.Length |> assertEqualTo 65541
        sut.[0..4] |> assertEquivalentTo [| 0xC6uy; 0x00uy; 0x01uy; 0x00uy; 0x00uy |]

[<TestFixture>]
module WhenUsingPackArray =
    [<Test>]
    let ``Should return 0x9400010203 with passing UInt8 array of [| 0 1 2 3 |]`` () =
        let sut = Value.Array [| Value.UInt8(0uy); Value.UInt8(1uy); Value.UInt8(2uy); Value.UInt8(3uy) |] |> Packer.packOne
        sut |> assertEquivalentTo [| 0x94uy; 0x00uy; 0x01uy; 0x02uy; 0x03uy |]

    [<Test>]
    let ``Should return byte[] and its format header is 0xDC and its length is 19 with passing 16-length Bool array of [| true .. true |]`` () =
        let sut = Array.create 16 (Value.Bool true) |> fun arr -> Value.Array arr |> Packer.packOne
        sut.Length |> assertEqualTo 19
        sut.[0..2] |> assertEquivalentTo [| 0xDCuy; 0x00uy; 0x10uy |]
        sut.[3..(sut.Length-1)] |> assertEquivalentTo (Array.create 16 0xC3uy)

    [<Test>]
    let ``Should return byte[] and its format header is 0xDD and its length is 131077 with passing 65536-length String array of [| 'a' .. 'a' |]`` () =
        let sut = Array.create 65536 (Value.String "a") |> fun arr -> Value.Array arr |> Packer.packOne
        sut.Length |> assertEqualTo 131077
        sut.[0..4] |> assertEquivalentTo [| 0xDDuy; 0x00uy; 0x01uy; 0x00uy; 0x00uy |]
        //sut.[5..(sut.Length-1)] |> assertEquivalentTo ((Array.create 65536 [| 0xA1uy; 0x61uy |]) |> Array.collect id)

[<TestFixture>]
module WhenUsingPackMap =
    [<Test>]
    let ``Should return 0x82A7636F6D70616374C3A6736368656D6100 with passing {"compact": true, "schema": 0}`` () =
        let sut = Value.Map (Map.ofList [(Value.String("compact"), Value.Bool(true)); (Value.String("schema"), Value.UInt8(0uy))]) |> Packer.packOne
        sut |> assertEquivalentTo [| 0x82uy; 0xA7uy; 0x63uy; 0x6Fuy; 0x6Duy; 0x70uy; 0x61uy; 0x63uy; 0x74uy; 0xC3uy; 0xA6uy; 0x73uy; 0x63uy; 0x68uy; 0x65uy; 0x6Duy; 0x61uy; 0x00uy |]

    [<Test>]
    let ``Should return byte[] and its format header is 0x8F and its length is 46 with passing 15-length key-value pairs of {1: '1', ..., 15: 'F'}`` () =
        let sut = [| for i in 1uy .. 15uy -> (Value.UInt8(i), Value.String(i.ToString("X"))) |] |> Map.ofArray |> Value.Map |> Packer.packOne
        sut.Length |> assertEqualTo 46
        sut.[0] |> assertEqualTo 0x8Fuy

    [<Test>]
    let ``Should return byte[] and its format header is 0xDE and its length is 35 with passing 16-length key-value pairs of {1: true, ..., 16: false}`` () =
        let sut = [| for i in 1uy .. 16uy -> (Value.UInt8(i), Value.Bool(i % 2uy = 0uy)) |] |> Map.ofArray |> Value.Map |> Packer.packOne
        sut.Length |> assertEqualTo 35
        sut.[0..2] |> assertEquivalentTo [| 0xDEuy; 0x00uy; 0x10uy |]

    [<Test>]
    let ``Should return byte[] and its format header is 0xDE and its length is 261761 with passing 65535-length key-value pairs of {1: true, ..., 65535: true}`` () =
        let sut = [ for i in 1us .. 65535us -> (Value.UInt16(i), Value.Bool(i % 2us = 0us)) ] |> Map.ofList |> Value.Map |> Packer.packOne
        sut.Length |> assertEqualTo 261761
        sut.[0..2] |> assertEquivalentTo [| 0xDEuy; 0xFFuy; 0xFFuy |]

    [<Test>]
    let ``Should return byte[] and its format header is 0xDF and its length 261769 with passing 65536-length key-value pairs of {1: true, ..., 65536: false}`` () =
        let sut = [ for i in 1u .. 65536u -> (Value.UInt32(i), Value.Bool(i % 2u = 0u)) ] |> Map.ofList |> Value.Map |> Packer.packOne
        sut.Length |> assertEqualTo 261769
        sut.[0..4] |> assertEquivalentTo [| 0xDFuy; 0x00uy; 0x01uy; 0x00uy; 0x00uy |]

[<TestFixture>]
module WhenUsingPackExt =
    [<Test>]
    let ``Should return 0xD40100 with passing (1, [| 0 |])`` () =
        (1y, [| 0uy |]) ||> Packer.packExt |> assertEquivalentTo [| 0xD4uy; 0x01uy; 0x00uy |]

    [<Test>]
    let ``Should return 0xD5020001 with passing (2, [| 0; 1 |])`` () =
        (2y, [| 0uy; 1uy |]) ||> Packer.packExt |> assertEquivalentTo [| 0xD5uy; 0x02uy; 0x00uy; 0x01uy |]

    [<Test>]
    let ``Should return 0xD603000102 with passing (3, [| 0; 1; 2 |])`` () =
        (3y, [| 0uy; 1uy; 2uy |]) ||> Packer.packExt |> assertEquivalentTo [| 0xD6uy; 0x03uy; 0x00uy; 0x01uy; 0x02uy |]

    [<Test>]
    let ``Should return 0xD60400010203 with passing (4, [| 0; 1; 2; 3 |])`` () =
        (4y, [| 0uy; 1uy; 2uy; 3uy |]) ||> Packer.packExt |> assertEquivalentTo [| 0xD6uy; 0x04uy; 0x00uy; 0x01uy; 0x02uy; 0x03uy |]

    [<Test>]
    let ``Should return 0xD7050001020304 with passing (5, [| 0 .. 4 |])`` () =
        (5y, [| 0uy .. 4uy |]) ||> Packer.packExt |> assertEquivalentTo [| 0xD7uy; 0x05uy; 0x00uy; 0x01uy; 0x02uy; 0x03uy; 0x04uy |]

    [<Test>]
    let ``Should return 0xD70600010120304050607 with passing (6, [| 0 .. 7 |])`` () =
        (6y, [| 0uy .. 7uy |]) ||> Packer.packExt |> assertEquivalentTo [| 0xD7uy; 0x06uy; 0x00uy; 0x01uy; 0x02uy; 0x03uy; 0x04uy; 0x05uy; 0x06uy; 0x07uy |]

    [<Test>]
    let ``Should return 0xD807000102030405060708 with passing (7, [| 0 .. 8 |])`` () =
        (7y, [| 0uy .. 8uy |]) ||> Packer.packExt |> assertEquivalentTo [| 0xD8uy; 0x07uy; 0x00uy; 0x01uy; 0x02uy; 0x03uy
                                                                           0x04uy; 0x05uy; 0x06uy; 0x07uy; 0x08uy |]

    [<Test>]
    let ``Should return 0xD808000102030405060708090A0B0C0D0E0F with passing (8, [| 0 .. 15 |])`` () =
        (8y, [| 0uy .. 15uy |]) ||> Packer.packExt |> assertEquivalentTo [| 0xD8uy; 0x08uy; 0x00uy; 0x01uy; 0x02uy; 0x03uy; 0x04uy; 0x05uy; 0x06uy
                                                                            0x07uy; 0x08uy; 0x09uy; 0x0Auy; 0x0Buy; 0x0Cuy; 0x0Duy; 0x0Euy; 0x0Fuy |] 

    [<Test>]
    let ``Should return byte[] and its format header is 0xC7 and its length is 20 with passing (9, [| 0 .. 16 |])`` () =
        let sut = (9y, [| 0uy .. 16uy |]) ||> Packer.packExt
        sut.Length |> assertEqualTo 20
        sut.[0..1] |> assertEquivalentTo [| 0xC7uy; 0x11uy |]

    [<Test>]
    let ``Should return byte[] and its format header is 0xC7 and its length is 258 with passing (10, [| 0 .. 254 |])`` () =
        let sut = (10y, [| 0uy .. 254uy |]) ||> Packer.packExt
        sut.Length |> assertEqualTo 258
        sut.[0..1] |> assertEquivalentTo [| 0xC7uy; 0xFFuy |]

    [<Test>]
    let ``Should return byte[] and its format header is 0xC8 and its length is 260 with passing (11, [| 0 .. 255 |])`` () =
        let sut = (11y, [| 0uy .. 255uy |]) ||> Packer.packExt
        sut.Length |> assertEqualTo 260
        sut.[0..2] |> assertEquivalentTo [| 0xC8uy; 0x01uy; 0x00uy |]

    [<Test>]
    let ``Should return byte[] and its format header is 0xC8 and its length is 65539 with passing (12, 65535-length array)`` () =
        let sut = (12y, (Array.create 65535 0uy)) ||> Packer.packExt
        sut.Length |> assertEqualTo 65539
        sut.[0..2] |> assertEquivalentTo [| 0xC8uy; 0xFFuy; 0xFFuy |]

    [<Test>]
    let ``Should return byte[] and its format header is 0xC9 and its length is 65542 with passing (13, 65536-length array)`` () =
        let sut = (13y, (Array.create 65536 0uy)) ||> Packer.packExt
        sut.Length |> assertEqualTo 65542
        sut.[0..4] |> assertEquivalentTo [| 0xC9uy; 0x00uy; 0x01uy; 0x00uy; 0x00uy |]

[<TestFixture>]
module WhenUsingPacker =
    [<Test>]
    let ``Should return 0x01C403000102 with passing [Value.UInt8 1; Value.Bin [0x00; 0x01; 0x02]]`` () =
        [Value.UInt8 1uy; Value.Bin [| 0x00uy; 0x01uy; 0x02uy |]] |> Packer.pack |> assertEquivalentTo [| 0x01uy; 0xC4uy; 0x03uy; 0x00uy; 0x01uy; 0x02uy |]