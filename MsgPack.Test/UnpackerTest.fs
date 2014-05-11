namespace MsgPack.UnpackerTest

open NUnit.Framework
open MsgPack

open MsgPack.PackerTest.TestExtensions

[<TestFixture>]
module UnpackTest =
    [<Test>]
    let ``Given 0x00 When unpack Then return seq [Value.UInt8 0]`` () =
        Unpacker.unpack [| 0x00uy |] |> assertEquivalentTo (seq [ Value.UInt8 0uy ])

    [<Test>]
    let ``Given 0x7F When unpack Then return seq [Value.UInt8 127]`` () =
        Unpacker.unpack [| 0x7Fuy |] |> assertEquivalentTo (seq [ Value.UInt8 127uy ])

    [<Test>]
    let ``Given 0x8F and byte * bool array of [(0, false); (1, true) .. (14, false)] When unpack Then return seq [Value.Map { Value.UInt8 0: Value.Bool false .. Value.UInt8 14: Value.Bool false }`` () =
        let bs = Array.init 15 (fun i -> if i % 2 = 0 then [| byte(i); 0xC2uy |] else [| byte(i); 0xC3uy |]) |> Array.concat
        let expected = List.init 15 (fun i -> ((i |> byte |> Value.UInt8), if i % 2 = 0 then Value.Bool false else Value.Bool true)) |> Map.ofList
        Array.append [| 0x8Fuy |] bs |> Unpacker.unpack |> assertEquivalentTo (seq [Value.Map(expected)])

    [<Test>]
    let ``Given 0x9F and byte array of [0 .. 14] When unpack Then return seq [Value.Array [|Value.UInt8 0 ... Value.UInt8 14|]]`` () =
        let bs = Array.init 15 (fun i -> byte(i))
        let expected = Array.init 15 (fun i -> i |> byte |> Value.UInt8)
        Array.append [| 0x9Fuy |] bs |> Unpacker.unpack |> assertEquivalentTo (seq [Value.Array(expected)])

    [<Test>]
    let ``Given 0xAB4D6573736167655061636B When unpack Then return seq [Value.String "MessagePack"]`` () =
        Unpacker.unpack [| 0xABuy; 0x4Duy; 0x65uy; 0x73uy; 0x73uy; 0x61uy; 0x67uy; 0x65uy; 0x50uy; 0x61uy; 0x63uy; 0x6Buy |]
        |> assertEquivalentTo (seq [Value.String "MessagePack"])

    [<Test>]
    let ``Given 0xC0 When unpack Then return seq [Value.Nil]`` () =
        Unpacker.unpack [| 0xC0uy |] |> assertEquivalentTo (seq [Value.Nil])

    [<Test>]
    let ``Given 0xC2 When unpack Then return seq [Value.Bool false]`` () =
        Unpacker.unpack [| 0xC2uy |] |> assertEquivalentTo (seq [Value.Bool false])

    [<Test>]
    let ``Given 0xC3 When unpack Then return seq [Value.Bool true]`` () =
        Unpacker.unpack [| 0xC3uy |] |> assertEquivalentTo (seq [Value.Bool true])

    [<Test>]
    let ``Given 0xC4FF and 255-length of 0xFF array When unpack Then return seq [Value.Bin (255-length of 0xFF)]`` () =
        let bs = Array.init 255 (fun _ -> 0xFFuy)
        Array.append [| 0xC4uy; 0xFFuy |] bs |> Unpacker.unpack |> assertEquivalentTo (seq [Value.Bin bs])

    [<Test>]
    let ``Given 0xC50100 and 256-length of 0x20 array When unpack Then return seq [Value.Bin (256-length of 0x20)]`` () =
        let bs = Array.init 256 (fun _ -> 0x20uy)
        Array.append [| 0xC5uy; 0x01uy; 0x00uy |] bs |> Unpacker.unpack |> assertEquivalentTo (seq [Value.Bin bs])

    [<Test>]
    let ``Given 0xC5FFFF and 65535-length of 0x30 array When unpack Then return seq [Value.Bin (65535-length of 0x30)]`` () =
        let bs = Array.init 65535 (fun _ -> 0x30uy)
        Array.append [| 0xC5uy; 0xFFuy; 0xFFuy |] bs |> Unpacker.unpack |> assertEquivalentTo (seq [Value.Bin bs])

    [<Test>]
    let ``Given 0xC600010000 and 65536-length of 0x41 array When unpack Then return seq [Value.Bin (65536-length of 0x41)]`` () =
        let bs = Array.init 65536 (fun _ -> 0x41uy)
        Array.append [| 0xC6uy; 0x00uy; 0x01uy; 0x00uy; 0x00uy |] bs |> Unpacker.unpack |> assertEquivalentTo (seq [Value.Bin bs])

    [<Test>]
    let ``Given 0xC7FF01 and 255-length of 0xFF array When unpack Then return seq [Value.Ext (1, 255-length of 0xFF)]`` () =
        let bs = Array.init 255 (fun _ -> 0xFFuy)
        Array.append [| 0xC7uy; 0xFFuy; 0x01uy |] bs |> Unpacker.unpack |> assertEquivalentTo (seq [Value.Ext(1y, bs)])

    [<Test>]
    let ``Given 0xC8010002 and 256-length of 0x20 array When unpack Then return seq [Value.Ext (2, 256-length of 0x20)]`` () =
        let bs = Array.init 256 (fun _ -> 0x20uy)
        Array.append [| 0xC8uy; 0x01uy; 0x00uy; 0x02uy |] bs |> Unpacker.unpack |> assertEquivalentTo (seq [Value.Ext(2y, bs)])

    [<Test>]
    let ``Given 0xC8FFFF03 and 65535-length of 0x30 array When unpack Then return seq [Value.Ext (3, 65535-length 0f 0x30)]`` () =
        let bs = Array.init 65535 (fun _ -> 0x30uy)
        Array.append [| 0xC8uy; 0xFFuy; 0xFFuy; 0x03uy |] bs |> Unpacker.unpack |> assertEquivalentTo (seq [Value.Ext(3y, bs)])

    [<Test>]
    let ``Given 0xC90001000004 and 65536-length of 0x41 array When unpack Then return seq [Value.Ext (4, 65536-length of 0x41)]`` () =
        let bs = Array.init 65536 (fun _ -> 0x41uy)
        Array.append [| 0xC9uy; 0x00uy; 0x01uy; 0x00uy; 0x00uy; 0x04uy |] bs |> Unpacker.unpack |> assertEquivalentTo (seq [Value.Ext(4y, bs)])

    [<Test>]
    let ``Given 0xCA3E200000 When unpack Then return seq [Value.Float32 0.15625]`` () =
        Unpacker.unpack [| 0XCAuy; 0x3Euy; 0x20uy; 0x00uy; 0x00uy |] |> assertEquivalentTo (seq [Value.Float32 0.15625f])

    [<Test>]
    let ``Given 0xCA7F800000 When unpack Then return seq [Value.Float32 +infinity]`` () =
        Unpacker.unpack [| 0xCAuy; 0x7Fuy; 0x80uy; 0x00uy; 0x00uy |] |> assertEquivalentTo (seq [Value.Float32 System.Single.PositiveInfinity])

    [<Test>]
    let ``Given 0xCBBFF0000180000000 When unpack Then return seq [Value.Float64 -1.000001430511474609375]`` () =
        Unpacker.unpack [| 0xCBuy; 0xBFuy; 0xF0uy; 0x00uy; 0x01uy; 0x80uy; 0x00uy; 0x00uy; 0x00uy |] |> assertEquivalentTo (seq [Value.Float64 (-1.000001430511474609375)])

    [<Test>]
    let ``Given 0xCBFFF0000000000000 When unpack Then return seq [Value.Float64 -infinity]`` () =
        Unpacker.unpack [| 0xCBuy; 0xFFuy; 0xF0uy; 0x00uy; 0x00uy; 0x00uy; 0x00uy; 0x00uy; 0x00uy |] |> assertEquivalentTo (seq [Value.Float64 System.Double.NegativeInfinity])

    [<Test>]
    let ``Given 0xCC80 When unpack Then return seq [Value.UInt8 128]`` () =
        Unpacker.unpack [| 0xCCuy; 0x80uy |] |> assertEquivalentTo (seq [ Value.UInt8 128uy ])

    [<Test>]
    let ``Given 0xCCFF When unpack Then return seq [Value.UInt8 255]`` () =
        Unpacker.unpack [| 0xCCuy; 0xFFuy |] |> assertEquivalentTo (seq [ Value.UInt8 255uy ])

    [<Test>]
    let ``Given 0xCD0100 When unpack Then return seq [Value.UInt16 256]`` () =
        Unpacker.unpack [| 0xCDuy; 0x01uy; 0x00uy |] |> assertEquivalentTo (seq [ Value.UInt16 256us ])

    [<Test>]
    let ``Given 0xCDFFFF When unpack Then return seq [Value.UInt16 65535]`` () =
        Unpacker.unpack [| 0xCDuy; 0xFFuy; 0xFFuy |] |> assertEquivalentTo (seq [ Value.UInt16 65535us ])

    [<Test>]
    let ``Given 0xCE00010000 When unpack Then return seq [Value.UInt32 65536]`` () =
        Unpacker.unpack [| 0xCEuy; 0x00uy; 0x01uy; 0x00uy; 0x00uy |] |> assertEquivalentTo (seq [Value.UInt32 65536u ])

    [<Test>]
    let ``Given 0xCEFFFFFFFF When unpack Then return seq [Value.UInt32 4294967295]`` () =
        Unpacker.unpack [| 0xCEuy; 0xFFuy; 0xFFuy; 0xFFuy; 0xFFuy |] |> assertEquivalentTo (seq [Value.UInt32 4294967295u ])

    [<Test>]
    let ``Given 0xCF0000000100000000 When unpack Then return seq [Value.UInt64 4294967296]`` () =
        Unpacker.unpack [| 0xCFuy; 0x00uy; 0x00uy; 0x00uy; 0x01uy; 0x00uy; 0x00uy; 0x00uy; 0x00uy |] |> assertEquivalentTo (seq [Value.UInt64 4294967296UL ])

    [<Test>]
    let ``Given 0xCFFFFFFFFFFFFFFFFF When unpack Then return seq [Value.UInt64 18446744073709551615]`` () =
        Unpacker.unpack [| 0xCFuy; 0xFFuy; 0xFFuy; 0xFFuy; 0xFFuy; 0xFFuy; 0xFFuy; 0xFFuy; 0xFFuy |] |> assertEquivalentTo (seq [Value.UInt64 18446744073709551615UL ])

    [<Test>]
    let ``Given 0xD0DF When unpack Then return seq [Value.Int8  (-33)]`` () =
        Unpacker.unpack [| 0xD0uy; 0xDFuy |] |> assertEquivalentTo (seq [Value.Int8 (-33y)])

    [<Test>]
    let ``Given 0xD080 When unpack Then return seq [Value.Int8 (-128)]`` () =
        Unpacker.unpack [| 0xD0uy; 0x80uy |] |> assertEquivalentTo (seq [Value.Int8 (-128y)])

    [<Test>]
    let ``Given 0xD1FF7F When unpack Then return seq [Value.Int16 (-129)]`` () =
        Unpacker.unpack [| 0xD1uy; 0xFFuy; 0x7Fuy |] |> assertEquivalentTo (seq [Value.Int16 (-129s)])

    [<Test>]
    let ``Given 0xD18000 When unpack Then return seq [Value.Int16 (-32768)]`` () =
        Unpacker.unpack [| 0xD1uy; 0x80uy; 0x00uy |] |> assertEquivalentTo (seq [Value.Int16 (-32768s)])

    [<Test>]
    let ``Given 0xD2FFFF7FFF When unpack Then return seq [Value.Int32 (-32769)]`` () =
        Unpacker.unpack [| 0xD2uy; 0xFFuy; 0xFFuy; 0x7Fuy; 0xFFuy |] |> assertEquivalentTo (seq [Value.Int32 (-32769)])

    [<Test>]
    let ``Given 0xD280000000 When unpack Then return seq [Value.Int32 (-2147483648)]`` () =
        Unpacker.unpack [| 0xD2uy; 0x80uy; 0x00uy; 0x00uy; 0x00uy |] |> assertEquivalentTo (seq [Value.Int32 (-2147483648)])

    [<Test>]
    let ``Given 0xD3FFFFFFFF7FFFFFFF When unpack Then return seq [Value.Int64 (-2147483649)]`` () =
        Unpacker.unpack [| 0xD3uy; 0xFFuy; 0xFFuy; 0xFFuy; 0xFFuy; 0x7Fuy; 0xFFuy; 0xFFuy; 0xFFuy |] |> assertEquivalentTo (seq [Value.Int64 (-2147483649L)])

    [<Test>]
    let ``Given 0xD38000000000000000 When unpack Then return seq [Value.Int64 (-9223372036854775808)]`` () =
        Unpacker.unpack [| 0xD3uy; 0x80uy; 0x00uy; 0x00uy; 0x00uy; 0x00uy; 0x00uy; 0x00uy; 0x00uy |] |> assertEquivalentTo (seq [Value.Int64 (-9223372036854775808L)])

    [<Test>]
    let ``Given 0xD405FF When unpack Then return seq [Value.Ext (5, [| 0xFF |])]`` () =
        [| 0xD4uy; 0x05uy; 0xFFuy |] |> Unpacker.unpack |> assertEquivalentTo (seq [Value.Ext (5y, [| 0xFFuy |])])

    [<Test>]
    let ``Given 0xD5062030 When unpack Then return seq [Value.Ext (6, [| 0x20; 0x30 |])]`` () =
        [| 0xD5uy; 0x06uy; 0x20uy; 0x30uy |] |> Unpacker.unpack |> assertEquivalentTo (seq [Value.Ext (6y, [| 0x20uy; 0x30uy |])])

    [<Test>]
    let ``Given 0xD607FF203041 When unpack Then return seq [Value.Ext (7, [| 0xFF; 0x20; 0x30; 0x41 |])]`` () =
        [| 0xD6uy; 0x07uy; 0xFFuy; 0x20uy; 0x30uy; 0x41uy |] |> Unpacker.unpack |> assertEquivalentTo (seq [Value.Ext (7y, [| 0xFFuy; 0x20uy; 0x30uy; 0x41uy |])])

    [<Test>]
    let ``Given 0xD708203041FF203041FF When unpack Then return seq [Value.Ext (8, [| 0x20; 0x30; 0x41; 0xFF; 0x20; 0x30; 0x41; 0xFF |])]`` () =
        [| 0xD7uy; 0x08uy; 0x20uy; 0x30uy; 0x41uy; 0xFFuy; 0x20uy; 0x30uy; 0x41uy; 0xFFuy |] |> Unpacker.unpack |> assertEquivalentTo (seq [Value.Ext (8y, [| 0x20uy; 0x30uy; 0x41uy; 0xFFuy; 0x20uy; 0x30uy; 0x41uy; 0xFFuy |])])

    [<Test>]
    let ``Given 0xD809202020203041304130413041FFFFFFFF When unpack Then return seq [Value.Ext (9, [| 0x20; 0x20; 0x20; 0x20; 0x30; 0x41; 0x30; 0x41; 0x30; 0x41; 0x30; 0x41; 0xFF; 0xFF; 0xFF; 0xFF |])]`` () =
        let bs = [| 0xD8uy; 0x09uy; 0x20uy; 0x20uy; 0x20uy; 0x20uy; 0x30uy; 0x41uy; 0x30uy; 0x41uy; 0x30uy; 0x41uy; 0x30uy; 0x41uy; 0xFFuy; 0xFFuy; 0xFFuy; 0xFFuy |]
        Unpacker.unpack bs |> assertEquivalentTo (seq [Value.Ext (9y, bs.[2..])])

    [<Test>]
    let ``Given 0xD920 and 32-length of 0x41 array When unpack Then return seq [Value.String (32-length of "A")]`` () =
        Array.append [| 0xD9uy; 0x20uy |] (Array.init 32 (fun _ -> 0x41uy)) |> Unpacker.unpack |> assertEquivalentTo (seq [Value.String (System.String('A', 32))])
    
    [<Test>]
    let ``Given 0xD9FF and 255-length of 0x61 array When unpack Then return seq [Value.String (255-length of "a")]`` () =
        Array.append [| 0xD9uy; 0xFFuy |] (Array.init 255 (fun _ -> 0x61uy)) |> Unpacker.unpack |> assertEquivalentTo (seq [Value.String (System.String('a', 255))])

    [<Test>]
    let ``Given 0xDA0100 and 256-length of 0x30 array When unpack Then return seq [Value.String (256-length of "0")]`` () =
        Array.append [| 0xDAuy; 0x01uy; 0x00uy |] (Array.init 256 (fun _ -> 0x30uy)) |> Unpacker.unpack |> assertEquivalentTo (seq [Value.String (System.String('0', 256))])

    [<Test>]
    let ``Given 0xDAFFFF and 65535-length of 0x39 array When unpack Then return seq [Value.String (65535-length of "9")]`` () =
        Array.append [| 0xDAuy; 0xFFuy; 0xFFuy |] (Array.init 65535 (fun _ -> 0x39uy)) |> Unpacker.unpack |> assertEquivalentTo (seq [Value.String (System.String('9', 65535))])

    [<Test>]
    let ``Given 0xDB00010000 and 65536-length of 0x20 array When unpack Then return seq [Value.String (65536-length of " ")]`` () =
        Array.append [| 0xDBuy; 0x00uy; 0x01uy; 0x00uy; 0x00uy |] (Array.init 65536 (fun _ -> 0x20uy)) |> Unpacker.unpack |> assertEquivalentTo (seq [Value.String (System.String(' ', 65536))])

    [<Test>]
    let ``Given 0xDC0010 and 16-length of 0xC0 array When unpack Then return seq [Value.Array (16-length of Value.Nil)]`` () =
        let bs = Array.init 16 (fun _ -> 0xC0uy)
        let expected = Array.init 16 (fun _ -> Value.Nil)
        Array.append [| 0xDCuy; 0x00uy; 0x10uy |] bs |> Unpacker.unpack |> assertEquivalentTo (seq [Value.Array(expected)])

    [<Test; Category("TooSlowTest")>]
    let ``Given 0xDCFFFF and 65535-length of 0xC2 array When unpack Then return seq [Value.Array (65535-length of Value.Bool false)]`` () =
        let bs = Array.init 65535 (fun _ -> 0xC2uy)
        let expected = Array.init 65535 (fun _ -> Value.Bool false)
        Array.append [| 0xDCuy; 0xFFuy; 0xFFuy |] bs |> Unpacker.unpack |> assertEquivalentTo (seq [Value.Array(expected)])

    [<Test; Category("TooSlowTest")>]
    let ``Given 0xDD00010000 and 65536-length of 0xC3 array When unpack Then return seq [Value.Array (65536-length of Value.Bool true)]`` () =
        let bs = Array.init 65536 (fun _ -> 0xC3uy)
        let expected = Array.init 65536 (fun _ -> Value.Bool true)
        Array.append [| 0xDDuy; 0x00uy; 0x01uy; 0x00uy; 0x00uy |] bs |> Unpacker.unpack |> assertEquivalentTo (seq [Value.Array(expected)])

    [<Test>]
    let ``Given 0xDE0010 and 16-length of key value collections When unpack Then return seq [Value.Map (16-length of (int format family, bool format family))]`` () =
        let bs = Array.init 16 (fun i -> if i % 2 = 0 then [| byte(i); 0xC2uy |] else [| byte(i); 0xC3uy |]) |> Array.concat
        let expected = List.init 16 (fun i -> (i |> byte |> Value.UInt8), if i % 2 = 0 then Value.Bool false else Value.Bool true) |> Map.ofList
        Array.append [| 0xDEuy; 0x00uy; 0x10uy |] bs |> Unpacker.unpack |> assertEquivalentTo (seq [Value.Map(expected)])

    [<Test; Category("TooSlowTest")>]
    let ``Given 0xDEFFFF and 65535-length of key value collections When unpack Then return seq [Value.Map (65535-length of (int format family, bool format family))]`` () =
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
    let ``Given 0xDF00010000 and 65536-length of key value collections When unpack Then return seq [Value.Map (65536-length of (int format family, bool format family))]`` () =
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
    let ``Given 0xFF When unpack Then return seq [Value.Int8 (-1)]`` () =
        Unpacker.unpack [| 0xFFuy |] |> assertEquivalentTo (seq [Value.Int8 (-1y)])

    [<Test>]
    let ``Given 0xE0 When unpack Then return seq [Value.Int8 (-32)]`` () =
        Unpacker.unpack [| 0xE0uy |] |> assertEquivalentTo (seq [Value.Int8 (-32y)])