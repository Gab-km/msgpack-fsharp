namespace MsgPack.Test.Unpacker

open NUnit.Framework
open MsgPack
open MsgPack.Test.Extensions

[<TestFixture>]
module WhenUsingUnpack =
    [<Test>]
    let ``Should return [| Value.UInt8 0 |] with passing 0x00`` () =
        Unpacker.unpack [| 0x00uy |] |> assertEquivalentTo [| Value.UInt8 0uy |]

    [<Test>]
    let ``Should return [| Value.UInt8 127 |] with passing 0x7F`` () =
        Unpacker.unpack [| 0x7Fuy |] |> assertEquivalentTo [| Value.UInt8 127uy |]

    [<Test>]
    let ``Should return [| Value.Map { Value.UInt8 0: Value.Bool false .. Value.UInt8 14: Value.Bool false } |] with passing 0x8F and byte * bool array of [| (0, false); (1, true) .. (14, false) |]`` () =
        let bs = Array.init 15 (fun i -> if i % 2 = 0 then [| byte(i); 0xC2uy |] else [| byte(i); 0xC3uy |]) |> Array.concat
        let expected = List.init 15 (fun i -> ((i |> byte |> Value.UInt8), if i % 2 = 0 then Value.Bool false else Value.Bool true)) |> Map.ofList
        Array.append [| 0x8Fuy |] bs |> Unpacker.unpack |> assertEquivalentTo [| Value.Map(expected) |]

    [<Test>]
    let ``Should return [| Value.Array [|Value.UInt8 0 ... Value.UInt8 14|] |] with passing 0x9F and byte array of [| 0 .. 14 |]`` () =
        let bs = Array.init 15 (fun i -> byte(i))
        let expected = Array.init 15 (fun i -> i |> byte |> Value.UInt8)
        Array.append [| 0x9Fuy |] bs |> Unpacker.unpack |> assertEquivalentTo [| Value.Array(expected) |]

    [<Test>]
    let ``Should return seq [Value.String "MessagePack"] with passing 0xAB4D6573736167655061636B`` () =
        Unpacker.unpack [| 0xABuy; 0x4Duy; 0x65uy; 0x73uy; 0x73uy; 0x61uy; 0x67uy; 0x65uy; 0x50uy; 0x61uy; 0x63uy; 0x6Buy |]
        |> assertEquivalentTo [| Value.String "MessagePack" |]

    [<Test>]
    let ``Should return [| Value.Nil |] with passing 0xC0`` () =
        Unpacker.unpack [| 0xC0uy |] |> assertEquivalentTo [| Value.Nil |]

    [<Test>]
    let ``Should return [| Value.Bool false |] with passing 0xC2`` () =
        Unpacker.unpack [| 0xC2uy |] |> assertEquivalentTo [| Value.Bool false |]

    [<Test>]
    let ``Should return [| Value.Bool true |] with passing 0xC3`` () =
        Unpacker.unpack [| 0xC3uy |] |> assertEquivalentTo [| Value.Bool true |]

    [<Test>]
    let ``Should return [| Value.Bin (255-length of 0xFF) |] with passing 0xC4FF and 255-length of 0xFF array`` () =
        let bs = Array.init 255 (fun _ -> 0xFFuy)
        Array.append [| 0xC4uy; 0xFFuy |] bs |> Unpacker.unpack |> assertEquivalentTo [| Value.Bin bs |]

    [<Test>]
    let ``Should return seq [Value.Bin (256-length of 0x20)] with passing 0xC50100 and 256-length of 0x20 array`` () =
        let bs = Array.init 256 (fun _ -> 0x20uy)
        Array.append [| 0xC5uy; 0x01uy; 0x00uy |] bs |> Unpacker.unpack |> assertEquivalentTo (seq [Value.Bin bs])

    [<Test>]
    let ``Should return seq [Value.Bin (65535-length of 0x30)] with passing 0xC5FFFF and 65535-length of 0x30 array`` () =
        let bs = Array.init 65535 (fun _ -> 0x30uy)
        Array.append [| 0xC5uy; 0xFFuy; 0xFFuy |] bs |> Unpacker.unpack |> assertEquivalentTo (seq [Value.Bin bs])

    [<Test>]
    let ``Should return seq [Value.Bin (65536-length of 0x41)] with passing 0xC600010000 and 65536-length of 0x41 array`` () =
        let bs = Array.init 65536 (fun _ -> 0x41uy)
        Array.append [| 0xC6uy; 0x00uy; 0x01uy; 0x00uy; 0x00uy |] bs |> Unpacker.unpack |> assertEquivalentTo (seq [Value.Bin bs])

    [<Test>]
    let ``Should return seq [Value.Ext (1, 255-length of 0xFF)] with passing 0xC7FF01 and 255-length of 0xFF array`` () =
        let bs = Array.init 255 (fun _ -> 0xFFuy)
        Array.append [| 0xC7uy; 0xFFuy; 0x01uy |] bs |> Unpacker.unpack |> assertEquivalentTo (seq [Value.Ext(1y, bs)])

    [<Test>]
    let ``Should return seq [Value.Ext (2, 256-length of 0x20)] with passing 0xC8010002 and 256-length of 0x20 array`` () =
        let bs = Array.init 256 (fun _ -> 0x20uy)
        Array.append [| 0xC8uy; 0x01uy; 0x00uy; 0x02uy |] bs |> Unpacker.unpack |> assertEquivalentTo (seq [Value.Ext(2y, bs)])

    [<Test>]
    let ``Should return seq [Value.Ext (3, 65535-length 0f 0x30)] with passing 0xC8FFFF03 and 65535-length of 0x30 array`` () =
        let bs = Array.init 65535 (fun _ -> 0x30uy)
        Array.append [| 0xC8uy; 0xFFuy; 0xFFuy; 0x03uy |] bs |> Unpacker.unpack |> assertEquivalentTo (seq [Value.Ext(3y, bs)])

    [<Test>]
    let ``Should return seq [Value.Ext (4, 65536-length of 0x41)] with passing 0xC90001000004 and 65536-length of 0x41 array`` () =
        let bs = Array.init 65536 (fun _ -> 0x41uy)
        Array.append [| 0xC9uy; 0x00uy; 0x01uy; 0x00uy; 0x00uy; 0x04uy |] bs |> Unpacker.unpack |> assertEquivalentTo (seq [Value.Ext(4y, bs)])

    [<Test>]
    let ``Should return seq [Value.Float32 0.15625] with passing 0xCA3E200000`` () =
        Unpacker.unpack [| 0XCAuy; 0x3Euy; 0x20uy; 0x00uy; 0x00uy |] |> assertEquivalentTo (seq [Value.Float32 0.15625f])

    [<Test>]
    let ``Should return seq [Value.Float32 +infinity] with passing 0xCA7F800000`` () =
        Unpacker.unpack [| 0xCAuy; 0x7Fuy; 0x80uy; 0x00uy; 0x00uy |] |> assertEquivalentTo (seq [Value.Float32 System.Single.PositiveInfinity])

    [<Test>]
    let ``Should return seq [Value.Float64 -1.000001430511474609375] with passing 0xCBBFF0000180000000`` () =
        Unpacker.unpack [| 0xCBuy; 0xBFuy; 0xF0uy; 0x00uy; 0x01uy; 0x80uy; 0x00uy; 0x00uy; 0x00uy |] |> assertEquivalentTo (seq [Value.Float64 (-1.000001430511474609375)])

    [<Test>]
    let ``Should return seq [Value.Float64 -infinity] with passing 0xCBFFF0000000000000`` () =
        Unpacker.unpack [| 0xCBuy; 0xFFuy; 0xF0uy; 0x00uy; 0x00uy; 0x00uy; 0x00uy; 0x00uy; 0x00uy |] |> assertEquivalentTo (seq [Value.Float64 System.Double.NegativeInfinity])

    [<Test>]
    let ``Should return seq [Value.UInt8 128] with passing 0xCC80`` () =
        Unpacker.unpack [| 0xCCuy; 0x80uy |] |> assertEquivalentTo (seq [ Value.UInt8 128uy ])

    [<Test>]
    let ``Should return seq [Value.UInt8 255] with passing 0xCCFF`` () =
        Unpacker.unpack [| 0xCCuy; 0xFFuy |] |> assertEquivalentTo (seq [ Value.UInt8 255uy ])

    [<Test>]
    let ``Should return seq [Value.UInt16 256] with passing 0xCD0100`` () =
        Unpacker.unpack [| 0xCDuy; 0x01uy; 0x00uy |] |> assertEquivalentTo (seq [ Value.UInt16 256us ])

    [<Test>]
    let ``Should return seq [Value.UInt16 65535] with passing 0xCDFFFF`` () =
        Unpacker.unpack [| 0xCDuy; 0xFFuy; 0xFFuy |] |> assertEquivalentTo (seq [ Value.UInt16 65535us ])

    [<Test>]
    let ``Should return seq [Value.UInt32 65536] with passing 0xCE00010000`` () =
        Unpacker.unpack [| 0xCEuy; 0x00uy; 0x01uy; 0x00uy; 0x00uy |] |> assertEquivalentTo (seq [Value.UInt32 65536u ])

    [<Test>]
    let ``Should return seq [Value.UInt32 4294967295] with passing 0xCEFFFFFFFF`` () =
        Unpacker.unpack [| 0xCEuy; 0xFFuy; 0xFFuy; 0xFFuy; 0xFFuy |] |> assertEquivalentTo (seq [Value.UInt32 4294967295u ])

    [<Test>]
    let ``Should return seq [Value.UInt64 4294967296] with passing 0xCF0000000100000000`` () =
        Unpacker.unpack [| 0xCFuy; 0x00uy; 0x00uy; 0x00uy; 0x01uy; 0x00uy; 0x00uy; 0x00uy; 0x00uy |] |> assertEquivalentTo (seq [Value.UInt64 4294967296UL ])

    [<Test>]
    let ``Should return seq [Value.UInt64 18446744073709551615] with passing 0xCFFFFFFFFFFFFFFFFF`` () =
        Unpacker.unpack [| 0xCFuy; 0xFFuy; 0xFFuy; 0xFFuy; 0xFFuy; 0xFFuy; 0xFFuy; 0xFFuy; 0xFFuy |] |> assertEquivalentTo (seq [Value.UInt64 18446744073709551615UL ])

    [<Test>]
    let ``Should return seq [Value.Int8  (-33)] with passing 0xD0DF`` () =
        Unpacker.unpack [| 0xD0uy; 0xDFuy |] |> assertEquivalentTo (seq [Value.Int8 (-33y)])

    [<Test>]
    let ``Should return seq [Value.Int8 (-128)] with passing 0xD080`` () =
        Unpacker.unpack [| 0xD0uy; 0x80uy |] |> assertEquivalentTo (seq [Value.Int8 (-128y)])

    [<Test>]
    let ``Should return seq [Value.Int16 (-129)] with passing 0xD1FF7F`` () =
        Unpacker.unpack [| 0xD1uy; 0xFFuy; 0x7Fuy |] |> assertEquivalentTo (seq [Value.Int16 (-129s)])

    [<Test>]
    let ``Should return seq [Value.Int16 (-32768)] with passing 0xD18000`` () =
        Unpacker.unpack [| 0xD1uy; 0x80uy; 0x00uy |] |> assertEquivalentTo (seq [Value.Int16 (-32768s)])

    [<Test>]
    let ``Should return seq [Value.Int32 (-32769)] with passing 0xD2FFFF7FFF`` () =
        Unpacker.unpack [| 0xD2uy; 0xFFuy; 0xFFuy; 0x7Fuy; 0xFFuy |] |> assertEquivalentTo (seq [Value.Int32 (-32769)])

    [<Test>]
    let ``Should return seq [Value.Int32 (-2147483648)] with passing 0xD280000000`` () =
        Unpacker.unpack [| 0xD2uy; 0x80uy; 0x00uy; 0x00uy; 0x00uy |] |> assertEquivalentTo (seq [Value.Int32 (-2147483648)])

    [<Test>]
    let ``Should return seq [Value.Int64 (-2147483649)] with passing 0xD3FFFFFFFF7FFFFFFF`` () =
        Unpacker.unpack [| 0xD3uy; 0xFFuy; 0xFFuy; 0xFFuy; 0xFFuy; 0x7Fuy; 0xFFuy; 0xFFuy; 0xFFuy |] |> assertEquivalentTo (seq [Value.Int64 (-2147483649L)])

    [<Test>]
    let ``Should return seq [Value.Int64 (-9223372036854775808)] with passing 0xD38000000000000000`` () =
        Unpacker.unpack [| 0xD3uy; 0x80uy; 0x00uy; 0x00uy; 0x00uy; 0x00uy; 0x00uy; 0x00uy; 0x00uy |] |> assertEquivalentTo (seq [Value.Int64 (-9223372036854775808L)])

    [<Test>]
    let ``Should return seq [Value.Ext (5, [| 0xFF |])] with passing 0xD405FF`` () =
        [| 0xD4uy; 0x05uy; 0xFFuy |] |> Unpacker.unpack |> assertEquivalentTo (seq [Value.Ext (5y, [| 0xFFuy |])])

    [<Test>]
    let ``Should return seq [Value.Ext (6, [| 0x20; 0x30 |])] with passing 0xD5062030`` () =
        [| 0xD5uy; 0x06uy; 0x20uy; 0x30uy |] |> Unpacker.unpack |> assertEquivalentTo (seq [Value.Ext (6y, [| 0x20uy; 0x30uy |])])

    [<Test>]
    let ``Should return seq [Value.Ext (7, [| 0xFF; 0x20; 0x30; 0x41 |])] with passing 0xD607FF203041`` () =
        [| 0xD6uy; 0x07uy; 0xFFuy; 0x20uy; 0x30uy; 0x41uy |] |> Unpacker.unpack |> assertEquivalentTo (seq [Value.Ext (7y, [| 0xFFuy; 0x20uy; 0x30uy; 0x41uy |])])

    [<Test>]
    let ``Should return seq [Value.Ext (8, [| 0x20; 0x30; 0x41; 0xFF; 0x20; 0x30; 0x41; 0xFF |])] with passing 0xD708203041FF203041FF`` () =
        [| 0xD7uy; 0x08uy; 0x20uy; 0x30uy; 0x41uy; 0xFFuy; 0x20uy; 0x30uy; 0x41uy; 0xFFuy |] |> Unpacker.unpack |> assertEquivalentTo (seq [Value.Ext (8y, [| 0x20uy; 0x30uy; 0x41uy; 0xFFuy; 0x20uy; 0x30uy; 0x41uy; 0xFFuy |])])

    [<Test>]
    let ``Should return seq [Value.Ext (9, [| 0x20; 0x20; 0x20; 0x20; 0x30; 0x41; 0x30; 0x41; 0x30; 0x41; 0x30; 0x41; 0xFF; 0xFF; 0xFF; 0xFF |])] with passing 0xD809202020203041304130413041FFFFFFFF`` () =
        let bs = [| 0xD8uy; 0x09uy; 0x20uy; 0x20uy; 0x20uy; 0x20uy; 0x30uy; 0x41uy; 0x30uy; 0x41uy; 0x30uy; 0x41uy; 0x30uy; 0x41uy; 0xFFuy; 0xFFuy; 0xFFuy; 0xFFuy |]
        Unpacker.unpack bs |> assertEquivalentTo (seq [Value.Ext (9y, bs.[2..])])

    [<Test>]
    let ``Should return seq [Value.String (32-length of "A")] with passing 0xD920 and 32-length of 0x41 array`` () =
        Array.append [| 0xD9uy; 0x20uy |] (Array.init 32 (fun _ -> 0x41uy)) |> Unpacker.unpack |> assertEquivalentTo (seq [Value.String (System.String('A', 32))])
    
    [<Test>]
    let ``Should return seq [Value.String (255-length of "a")] with passing 0xD9FF and 255-length of 0x61 array`` () =
        Array.append [| 0xD9uy; 0xFFuy |] (Array.init 255 (fun _ -> 0x61uy)) |> Unpacker.unpack |> assertEquivalentTo (seq [Value.String (System.String('a', 255))])

    [<Test>]
    let ``Should return seq [Value.String (256-length of "0")] with passing 0xDA0100 and 256-length of 0x30 array`` () =
        Array.append [| 0xDAuy; 0x01uy; 0x00uy |] (Array.init 256 (fun _ -> 0x30uy)) |> Unpacker.unpack |> assertEquivalentTo (seq [Value.String (System.String('0', 256))])

    [<Test>]
    let ``Should return seq [Value.String (65535-length of "9")] with passing 0xDAFFFF and 65535-length of 0x39 array`` () =
        Array.append [| 0xDAuy; 0xFFuy; 0xFFuy |] (Array.init 65535 (fun _ -> 0x39uy)) |> Unpacker.unpack |> assertEquivalentTo (seq [Value.String (System.String('9', 65535))])

    [<Test>]
    let ``Should return seq [Value.String (65536-length of " ")] with passing 0xDB00010000 and 65536-length of 0x20 array`` () =
        Array.append [| 0xDBuy; 0x00uy; 0x01uy; 0x00uy; 0x00uy |] (Array.init 65536 (fun _ -> 0x20uy)) |> Unpacker.unpack |> assertEquivalentTo (seq [Value.String (System.String(' ', 65536))])

    [<Test>]
    let ``Should return seq [Value.Array (16-length of Value.Nil)] with passing 0xDC0010 and 16-length of 0xC0 array`` () =
        let bs = Array.init 16 (fun _ -> 0xC0uy)
        let expected = Array.init 16 (fun _ -> Value.Nil)
        Array.append [| 0xDCuy; 0x00uy; 0x10uy |] bs |> Unpacker.unpack |> assertEquivalentTo (seq [Value.Array(expected)])

    [<Test; Category("TooSlowTest")>]
    let ``Should return seq [Value.Array (65535-length of Value.Bool false)] with passing 0xDCFFFF and 65535-length of 0xC2 array`` () =
        let bs = Array.init 65535 (fun _ -> 0xC2uy)
        let expected = Array.init 65535 (fun _ -> Value.Bool false)
        Array.append [| 0xDCuy; 0xFFuy; 0xFFuy |] bs |> Unpacker.unpack |> assertEquivalentTo (seq [Value.Array(expected)])

    [<Test; Category("TooSlowTest")>]
    let ``Should return seq [Value.Array (65536-length of Value.Bool true)] with passing 0xDD00010000 and 65536-length of 0xC3 array`` () =
        let bs = Array.init 65536 (fun _ -> 0xC3uy)
        let expected = Array.init 65536 (fun _ -> Value.Bool true)
        Array.append [| 0xDDuy; 0x00uy; 0x01uy; 0x00uy; 0x00uy |] bs |> Unpacker.unpack |> assertEquivalentTo (seq [Value.Array(expected)])

    [<Test>]
    let ``Should return seq [Value.Map (16-length of (int format family, bool format family))] with passing 0xDE0010 and 16-length of key value collections`` () =
        let bs = Array.init 16 (fun i -> if i % 2 = 0 then [| byte(i); 0xC2uy |] else [| byte(i); 0xC3uy |]) |> Array.concat
        let expected = List.init 16 (fun i -> (i |> byte |> Value.UInt8), if i % 2 = 0 then Value.Bool false else Value.Bool true) |> Map.ofList
        Array.append [| 0xDEuy; 0x00uy; 0x10uy |] bs |> Unpacker.unpack |> assertEquivalentTo (seq [Value.Map(expected)])

    [<Test; Category("TooSlowTest")>]
    let ``Should return seq [Value.Map (65535-length of (int format family, bool format family))] with passing 0xDEFFFF and 65535-length of key value collections`` () =
        let bs = Array.init 65535 (fun i -> Array.append (Packer.packInt i) [| (if i % 2 = 0 then 0xC2uy else 0xC3uy) |]) |> Array.concat
        let expected =
            List.init 65535
                (fun i ->
                    if i <= 255 then
                        i |> byte |> Value.UInt8
                    else
                        i |> uint16 |> Value.UInt16
                    , if i % 2 = 0 then Value.Bool false else Value.Bool true)
            |> Map.ofList
        Array.append [| 0xDEuy; 0xFFuy; 0xFFuy |] bs |> Unpacker.unpack |> assertEquivalentTo (seq [Value.Map(expected)])

    [<Test; Category("TooSlowTest")>]
    let ``Should return seq [Value.Map (65536-length of (int format family, bool format family))] with passing 0xDF00010000 and 65536-length of key value collections`` () =
        let bs = Array.init 65536 (fun i -> Array.append (Packer.packInt i) [| 0xC3uy |]) |> Array.concat
        let expected =
            List.init 65536
                (fun i ->
                    if i <= 255 then
                        i |> byte |> Value.UInt8
                    elif i<= 65535 then
                        i |> uint16 |> Value.UInt16
                    else
                        i |> uint32 |> Value.UInt32
                    , Value.Bool true)
            |> Map.ofList
        Array.append [| 0xDFuy; 0x00uy; 0x01uy; 0x00uy; 0x00uy |] bs |> Unpacker.unpack |> assertEquivalentTo (seq [Value.Map(expected)])

    [<Test>]
    let ``Should return seq [Value.Int8 (-1)] with passing 0xFF`` () =
        Unpacker.unpack [| 0xFFuy |] |> assertEquivalentTo (seq [Value.Int8 (-1y)])

    [<Test>]
    let ``Should return seq [Value.Int8 (-32)] with passing 0xE0`` () =
        Unpacker.unpack [| 0xE0uy |] |> assertEquivalentTo (seq [Value.Int8 (-32y)])