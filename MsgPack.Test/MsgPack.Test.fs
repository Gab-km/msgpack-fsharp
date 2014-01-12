namespace MsgPack.Test

open NUnit.Framework
open MsgPack

module TestExtensions =

    let assertEqual<'a> expected (actual: 'a) = Assert.That(actual, Is.EqualTo(expected))
    let assertEquivalentTo<'a> expected (actual: 'a) = Assert.That(actual, Is.EquivalentTo(expected))

open TestExtensions

[<TestFixture>]
type MsgPackTest() = 
    [<Test>]
    member self.GivenTrueValueWhenSerializeBoolThenReturn0xc2() =
        true |> Serialization.serializeBool |> assertEqual Format.True

    [<Test>]
    member self.GivenFalseValueWhenSerializeBoolThenReturn0xc3() =
        false |> Serialization.serializeBool |> assertEqual Format.False

    [<Test>]
    member self.Given127WhenSerializeIntThenReturn0xff() =
        127 |> Serialization.serializeInt |> assertEquivalentTo [| 0x7Fuy |]

    [<Test>]
    member self.Given128WhenSerializeIntThenReturn0xccf0() =
        128 |> Serialization.serializeInt |> assertEquivalentTo [| Format.UInt8; 0x80uy |]
        
    [<Test>]
    member self.Given255WhenSerializeIntThenReturn0xcd0100() =
        255 |> Serialization.serializeInt |> assertEquivalentTo [| Format.UInt8; 0xFFuy |]

    [<Test>]
    member self.Given256WhenSerializeIntThenReturn0xcd0100() =
        256 |> Serialization.serializeInt |> assertEquivalentTo [| Format.UInt16; 0x01uy; 0x00uy |]

    [<Test>]
    member self.Given0xffffWhenSerializeIntThenReturn0xcd7fff() =
        0xFFFF |> Serialization.serializeInt |> assertEquivalentTo [| Format.UInt16; 0xFFuy; 0xFFuy |]

    [<Test>]
    member self.Given0x010000WhenSerializeIntThenReturn0xce010000() =
        0x010000 |> Serialization.serializeInt |> assertEquivalentTo [| Format.UInt32; 0x00uy; 0x01uy; 0x00uy; 0x00uy |]

    [<Test>]
    member self.Given0x12345678WhenSerializeIntThenReturn0xce12345678() =
        0x12345678 |> Serialization.serializeInt |> assertEquivalentTo [| Format.UInt32; 0x12uy; 0x34uy; 0x56uy; 0x78uy |]
