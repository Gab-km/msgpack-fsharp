namespace MsgPack

module Unpacker =
    open System.Collections.Generic

    type internal Sequencials =
        | ArrayStore of int * Value []
        | MapStore of int * Value * Map<Value, Value>

    type Bytes(bs:byte[])=
      let mutable _ind=0
      member this.Item with get i=bs.[_ind+i]
      member this.IncInd d= _ind<-_ind+d;this
      member this.Length=bs.Length-_ind
      member this.Dice(startI,endI)=bs.[_ind+startI.._ind+endI]

    [<CompiledName("Unpack")>]
    let unpack (bs:byte[]) =
        let bs=Bytes(bs)
        let sw=System.Diagnostics.Stopwatch.StartNew()
        let raiseMessagePackException () = MessagePackException("Attempt to unpack with non-compatible type") |> raise

        let appendValue (newValue: Value) (sequencials: Stack<Sequencials>) (values: List<Value>) =
            let mutable nv, doLoop = newValue, true
            while doLoop do
                if sequencials.Count > 0 then
                    let elm = sequencials.Pop()
                    match elm with
                    | ArrayStore (count, arrayValues) ->
                        arrayValues.[arrayValues.Length - count] <- nv
                        let newCount = count - 1
                        if newCount = 0 then
                            nv <- Value.Array arrayValues
                        else
                            sequencials.Push(ArrayStore(newCount, arrayValues))
                            doLoop <- false
                    | MapStore (count, Value.Nil, mapValues) ->
                        sequencials.Push(MapStore(count, newValue, mapValues))
                        doLoop <- false
                    | MapStore (count, key, mapValues) ->
                        let newMap = Map.add key newValue mapValues
                        let newCount = count - 1
                        if newCount = 0 then
                            nv <- Value.Map newMap
                        else
                            sequencials.Push(MapStore(newCount, Value.Nil, newMap))
                            doLoop <- false
                else
                    values.Add(nv)
                    doLoop <- false
            sequencials, values

        let _unpackPositiveFixint (bytes: Bytes) (sequencials: Stack<Sequencials>) (values: List<Value>) =
            let ars, vs = appendValue (Value.UInt8 bytes.[0]) sequencials values
            bytes.IncInd 1, ars, vs

        let _unpackFixmap (bytes: Bytes) (sequencials: Stack<Sequencials>) =
            let length = int(bytes.[0] &&& 0b00001111uy)
            if bytes.Length - 1 >= length then
                sequencials.Push(MapStore(length, Value.Nil, Map.ofList []))
                bytes.IncInd 1, sequencials
            else
                raiseMessagePackException ()

        let _unpackFixarray (bytes: Bytes) (sequencials: Stack<Sequencials>) =
            let length = int(bytes.[0] &&& 0b00001111uy)
            if bytes.Length - 1 >= length then
                sequencials.Push(ArrayStore(length, Array.init length (fun _ -> Value.Nil)))
                bytes.IncInd 1, sequencials
            else
                raiseMessagePackException ()

        let _unpackFixstr (bytes: Bytes) (sequencials: Stack<Sequencials>) (values: List<Value>) =
            let length = int(bytes.[0] &&& 0b00011111uy)
            if bytes.Length - 1 >= length then
                let newValue = System.Text.Encoding.UTF8.GetString(bytes.Dice(1,length)) |> Value.String
                let ars, vs = appendValue newValue sequencials values
                bytes.IncInd (length+1), ars, vs
            else
                raiseMessagePackException ()

        let _unpackNil (bytes: Bytes) (sequencials: Stack<Sequencials>) (values: List<Value>) =
            let ars, vs = appendValue Value.Nil sequencials values
            bytes.IncInd 1, ars, vs

        let _unpackFalse (bytes: Bytes) (sequencials: Stack<Sequencials>) (values: List<Value>) =
            let ars, vs = appendValue (Value.Bool false) sequencials values
            bytes.IncInd 1, ars, vs

        let _unpackTrue (bytes: Bytes) (sequencials: Stack<Sequencials>) (values: List<Value>) =
            let ars, vs = appendValue (Value.Bool true) sequencials values
            bytes.IncInd 1, ars, vs

        let _unpackBin8 (bytes: Bytes) (sequencials: Stack<Sequencials>) (values: List<Value>) =
            if bytes.Length >= 2 then
                let length = int(bytes.[1])
                if bytes.Length - 2 >= length then
                    let newValue = Value.Bin <|bytes.Dice(2,(length+1))
                    let ars, vs = appendValue newValue sequencials values
                    bytes.IncInd (length+2), ars, vs
                else
                    raiseMessagePackException ()
            else
                raiseMessagePackException ()

        let _unpackBin16 (bytes: Bytes) (sequencials: Stack<Sequencials>) (values: List<Value>) =
            if bytes.Length >= 3 then
                let length = int(bytes.[1]) * 256 +
                             int(bytes.[2])
                if bytes.Length - 3 >= length then
                    let newValue = Value.Bin <|bytes.Dice(3,length+2)
                    let ars, vs = appendValue newValue sequencials values
                    bytes.IncInd (length+3), ars, vs
                else
                    raiseMessagePackException ()
            else
                raiseMessagePackException ()

        let _unpackBin32 (bytes: Bytes) (sequencials: Stack<Sequencials>) (values: List<Value>) =
            if bytes.Length >= 5 then
                let length = int(bytes.[1]) * 16777216 +
                             int(bytes.[2]) * 65536 +
                             int(bytes.[3]) * 256 +
                             int(bytes.[4])
                if bytes.Length - 5 >= length then
                    let newValue = Value.Bin <|bytes.Dice(5,length+4)
                    let ars, vs = appendValue newValue sequencials values
                    bytes.IncInd (length+5), ars, vs
                else
                    raiseMessagePackException ()
            else
                raiseMessagePackException ()

        let _unpackExt8 (bytes: Bytes) (sequencials: Stack<Sequencials>) (values: List<Value>) =
            if bytes.Length >= 3 then
                let length = int(bytes.[1])
                if bytes.Length - 3 >= length then
                    let t = sbyte(bytes.[2])
                    let d = bytes.Dice(3,length+2)
                    let ars, vs = appendValue (Value.Ext(t, d)) sequencials values
                    bytes.IncInd (length+3), ars, vs
                else
                    raiseMessagePackException ()
            else
                raiseMessagePackException ()

        let _unpackExt16 (bytes: Bytes) (sequencials: Stack<Sequencials>) (values: List<Value>) =
            if bytes.Length >= 4 then
                let length = int(bytes.[1]) * 256 +
                             int(bytes.[2])
                if bytes.Length - 4 >= length then
                    let t = sbyte(bytes.[3])
                    let d = bytes.Dice(4,length+3)
                    let ars, vs = appendValue (Value.Ext(t, d)) sequencials values
                    bytes.IncInd (length+4), ars, vs
                else
                    raiseMessagePackException ()
            else
                raiseMessagePackException ()

        let _unpackExt32 (bytes: Bytes) (sequencials: Stack<Sequencials>) (values: List<Value>) =
            if bytes.Length >= 6 then
                let length = int(bytes.[1]) * 16777216 +
                             int(bytes.[2]) * 65536 +
                             int(bytes.[3]) * 256 +
                             int(bytes.[4])
                if bytes.Length - 6 >= length then
                    let t = sbyte(bytes.[5])
                    let d = bytes.Dice(6,length+5)
                    let ars, vs = appendValue (Value.Ext(t, d)) sequencials values
                    bytes.IncInd (length+6), ars, vs
                else
                    raiseMessagePackException ()
            else
                raiseMessagePackException ()

        let _unpackFloat32 (bytes: Bytes) (sequencials: Stack<Sequencials>) (values: List<Value>) =
            if bytes.Length >= 5 then
                let newValue = Utility.convertEndianFromBytesToFloat32(bytes.Dice(1,4)) |> Value.Float32
                let ars, vs = appendValue newValue sequencials values
                bytes.IncInd 5, ars, vs
            else
                raiseMessagePackException ()

        let _unpackFloat64 (bytes: Bytes) (sequencials: Stack<Sequencials>) (values: List<Value>) =
            if bytes.Length >= 9 then
                let newValue = Utility.convertEndianFromBytesToFloat(bytes.Dice(1,8)) |> Value.Float64
                let ars, vs = appendValue newValue sequencials values
                bytes.IncInd 9, ars, vs
            else
                raiseMessagePackException ()

        let _unpackUInt8 (bytes: Bytes) (sequencials: Stack<Sequencials>) (values: List<Value>) =
            if bytes.Length >= 2 then
                let newValue = Value.UInt8 bytes.[1]
                let ars, vs = appendValue newValue sequencials values
                bytes.IncInd 2, ars, vs
            else
                raiseMessagePackException ()

        let _unpackUInt16 (bytes: Bytes) (sequencials: Stack<Sequencials>) (values: List<Value>) =
            if bytes.Length >= 3 then
                let newValue = uint16(bytes.[1]) * 256us +
                               uint16(bytes.[2])
                               |> Value.UInt16
                let ars, vs = appendValue newValue sequencials values
                bytes.IncInd 3, ars, vs
            else
                raiseMessagePackException ()

        let _unpackUInt32 (bytes: Bytes) (sequencials: Stack<Sequencials>) (values: List<Value>) =
            if bytes.Length >= 5 then
                let newValue = uint32(bytes.[1]) * 16777216u +
                               uint32(bytes.[2]) * 65536u +
                               uint32(bytes.[3]) * 256u +
                               uint32(bytes.[4])
                               |> Value.UInt32
                let ars, vs = appendValue newValue sequencials values
                bytes.IncInd 5, ars, vs
            else
                raiseMessagePackException ()

        let _unpackUInt64 (bytes: Bytes) (sequencials: Stack<Sequencials>) (values: List<Value>) =
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
                bytes.IncInd 9, ars, vs
            else
                raiseMessagePackException ()

        let _unpackInt8 (bytes: Bytes) (sequencials: Stack<Sequencials>) (values: List<Value>) =
            if bytes.Length >= 2 then
                let newValue = sbyte(bytes.[1]) |> Value.Int8
                let ars, vs = appendValue newValue sequencials values
                bytes.IncInd 2, ars, vs
            else
                raiseMessagePackException ()

        let _unpackInt16 (bytes: Bytes) (sequencials: Stack<Sequencials>) (values: List<Value>) =
            if bs.Length >= 3 then
                let newValue = uint16(bytes.[1]) * 256us +
                               uint16(bytes.[2])
                               |> int16 |> Value.Int16
                let ars, vs = appendValue newValue sequencials values
                bytes.IncInd 3, ars, vs
            else
                raiseMessagePackException ()

        let _unpackInt32 (bytes: Bytes) (sequencials: Stack<Sequencials>) (values: List<Value>) =
            if bytes.Length >= 5 then
                let newValue = uint32(bytes.[1]) * 16777216u +
                               uint32(bytes.[2]) * 65536u +
                               uint32(bytes.[3]) * 256u +
                               uint32(bytes.[4])
                               |> int |> Value.Int32
                let ars, vs = appendValue newValue sequencials values
                bytes.IncInd 5, ars, vs
            else
                raiseMessagePackException ()

        let _unpackInt64 (bytes: Bytes) (sequencials: Stack<Sequencials>) (values: List<Value>) =
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
                bytes.IncInd 9, ars, vs
            else
                raiseMessagePackException ()

        let _unpackFixExt1 (bytes: Bytes) (sequencials: Stack<Sequencials>) (values: List<Value>) =
            if bytes.Length >= 3 then
                let t = sbyte(bytes.[1])
                let d = [| bytes.[2] |]
                let ars, vs = appendValue (Value.Ext (t, d)) sequencials values
                bytes.IncInd 3, ars, vs
            else
                raiseMessagePackException ()

        let _unpackFixExt2 (bytes: Bytes) (sequencials: Stack<Sequencials>) (values: List<Value>) =
            if bytes.Length >= 4 then
                let t = sbyte(bytes.[1])
                let d = bytes.Dice(2,3)
                let ars, vs = appendValue (Value.Ext (t, d)) sequencials values
                bytes.IncInd 4, ars, vs
            else
                raiseMessagePackException ()

        let _unpackFixExt4 (bytes: Bytes) (sequencials: Stack<Sequencials>) (values: List<Value>) =
            if bytes.Length >= 6 then
                let t = sbyte(bytes.[1])
                let d = bytes.Dice(2,5)
                let ars, vs = appendValue (Value.Ext (t, d)) sequencials values
                bytes.IncInd 6, ars, vs
            else
                raiseMessagePackException ()

        let _unpackFixExt8 (bytes: Bytes) (sequencials: Stack<Sequencials>) (values: List<Value>) =
            if bytes.Length >= 10 then
                let t = sbyte(bytes.[1])
                let d = bytes.Dice(2,9)
                let ars, vs = appendValue (Value.Ext (t, d)) sequencials values
                bytes.IncInd 10, ars, vs
            else
                raiseMessagePackException ()

        let _unpackFixExt16 (bytes: Bytes) (sequencials: Stack<Sequencials>) (values: List<Value>) =
            if bytes.Length >= 18 then
                let t = sbyte(bytes.[1])
                let d = bytes.Dice(2,17)
                let ars, vs = appendValue (Value.Ext (t, d)) sequencials values
                bytes.IncInd 18, ars, vs
            else
                raiseMessagePackException ()

        let _unpackStr8 (bytes: Bytes) (sequencials: Stack<Sequencials>) (values: List<Value>) =
            if bytes.Length >= 2 then
                let length = int(bytes.[1])
                if bytes.Length - 2 >= length then
                    let newValue = System.Text.Encoding.UTF8.GetString(bytes.Dice(2,length+1)) |> Value.String
                    let ars, vs = appendValue newValue sequencials values
                    bytes.IncInd (length+2), ars, vs
                else
                    raiseMessagePackException ()
            else
                raiseMessagePackException ()

        let _unpackStr16 (bytes: Bytes) (sequencials: Stack<Sequencials>) (values: List<Value>) =
            if bytes.Length >= 3 then
                let length = int(bytes.[1]) * 256 +
                             int(bytes.[2])
                if bytes.Length - 3 >= length then
                    let newValue = System.Text.Encoding.UTF8.GetString(bytes.Dice(3,length+2)) |> Value.String
                    let ars, vs = appendValue newValue sequencials values
                    bytes.IncInd (length+3), ars, vs
                else
                    raiseMessagePackException ()
            else
                raiseMessagePackException ()

        let _unpackStr32 (bytes: Bytes) (sequencials: Stack<Sequencials>) (values: List<Value>) =
            if bytes.Length >= 5 then
                let length = int(bytes.[1]) * 16777216 +
                             int(bytes.[2]) * 65536 +
                             int(bytes.[3]) * 256 +
                             int(bytes.[4])
                if bytes.Length - 5 >= length then
                    let newValue = System.Text.Encoding.UTF8.GetString(bytes.Dice(5,length+4)) |> Value.String
                    let ars, vs = appendValue newValue sequencials values
                    bytes.IncInd (length+5), ars, vs
                else
                    raiseMessagePackException ()
            else
                raiseMessagePackException ()

        let _unpackArray16 (bytes: Bytes) (sequencials: Stack<Sequencials>) =
            if bytes.Length >= 3 then
                let length = int(bytes.[1]) * 256 +
                             int(bytes.[2])
                if bytes.Length - 3 >= length then
                    sequencials.Push(ArrayStore(length, Array.init length (fun _ -> Value.Nil)))
                    bytes.IncInd 3, sequencials
                else
                    raiseMessagePackException ()
            else
                raiseMessagePackException ()

        let _unpackArray32 (bytes: Bytes) (sequencials: Stack<Sequencials>) =
            if bytes.Length >= 5 then
                let length = int(bytes.[1]) * 16777216 +
                             int(bytes.[2]) * 65536 +
                             int(bytes.[3]) * 256 +
                             int(bytes.[4])
                if bytes.Length - 5 >= length then
                    sequencials.Push(ArrayStore(length, Array.init length (fun _ -> Value.Nil)))
                    bytes.IncInd 5, sequencials
                else
                    raiseMessagePackException ()
            else
                raiseMessagePackException ()

        let _unpackMap16 (bytes: Bytes) (sequencials: Stack<Sequencials>) =
            if bytes.Length >= 3 then
                let length = int(bytes.[1]) * 256 +
                             int(bytes.[2])
                if bytes.Length - 3 >= length * 2 then
                    sequencials.Push(MapStore(length, Value.Nil, Map.ofList []))
                    let bs=bytes.IncInd 3
                    bs, sequencials
                else
                    raiseMessagePackException ()
            else
                raiseMessagePackException ()

        let _unpackMap32 (bytes: Bytes) (sequencials: Stack<Sequencials>) =
            if bytes.Length >= 5 then
                let length = int(bytes.[1]) * 16777216 +
                             int(bytes.[2]) * 65536 +
                             int(bytes.[3]) * 256 +
                             int(bytes.[4])
                if bytes.Length - 5 >= length * 2 then
                    sequencials.Push(MapStore(length, Value.Nil, Map.ofList []))
                    bytes.IncInd 5, sequencials
                else
                    raiseMessagePackException ()
            else
                raiseMessagePackException ()

        let _unpackNegativeFixint (bytes: Bytes) (sequencials: Stack<Sequencials>) (values: List<Value>) =
            let newValue = sbyte(bytes.[0]) |> Value.Int8
            let ars, vs = appendValue newValue sequencials values
            bytes.IncInd 1, ars, vs

        let bs,sequencials,values=bs,Stack(),List(bs.Length/2)

        let rec _unpack()=
            if bs.Length = 0 then ()
            else
                let header = bs.[0]
                //個々の戻り値いらなくなったので後で変えるべきだけど手を抜いてスルー
                if (header &&& 0b10000000uy) = 0uy then
                    _unpackPositiveFixint bs sequencials values
                    _unpack()
                elif (header &&& 0b11110000uy) = 0b10000000uy then
                    _unpackFixmap bs sequencials
                    _unpack()
                elif (header &&& 0b11110000uy) = 0b10010000uy then
                    _unpackFixarray bs sequencials
                    _unpack()
                elif (header &&& 0b11100000uy) = 0b10100000uy then
                    _unpackFixstr bs sequencials values
                    _unpack()
                elif (header = Format.Nil) then
                    _unpackNil bs sequencials values
                    _unpack()
                elif (header = Format.False) then
                    _unpackFalse bs sequencials values
                    _unpack()
                elif (header = Format.True) then
                    _unpackTrue bs sequencials values
                    _unpack()
                elif header = Format.Bin8 then
                    _unpackBin8 bs sequencials values
                    _unpack()
                elif header = Format.Bin16 then
                    _unpackBin16 bs sequencials values
                    _unpack()
                elif header = Format.Bin32 then
                    _unpackBin32 bs sequencials values
                    _unpack()
                elif header = Format.Ext8 then
                    _unpackExt8 bs sequencials values
                    _unpack()
                elif header = Format.Ext16 then
                    _unpackExt16 bs sequencials values
                    _unpack()
                elif header = Format.Ext32 then
                    _unpackExt32 bs sequencials values
                    _unpack()
                elif header = Format.Float32 then
                    _unpackFloat32 bs sequencials values
                    _unpack()
                elif header = Format.Float64 then
                    _unpackFloat64 bs sequencials values
                    _unpack()
                elif header = Format.UInt8 then
                    _unpackUInt8 bs sequencials values
                    _unpack()
                elif header = Format.UInt16 then
                    _unpackUInt16 bs sequencials values
                    _unpack()
                elif header = Format.UInt32 then
                    _unpackUInt32 bs sequencials values
                    _unpack()
                elif header = Format.UInt64 then
                    _unpackUInt64 bs sequencials values
                    _unpack()
                elif header = Format.Int8 then
                    _unpackInt8 bs sequencials values
                    _unpack()
                elif header = Format.Int16 then
                    _unpackInt16 bs sequencials values
                    _unpack()
                elif header = Format.Int32 then
                    _unpackInt32 bs sequencials values
                    _unpack()
                elif header = Format.Int64 then
                    _unpackInt64 bs sequencials values
                    _unpack()
                elif header = Format.FixExt1 then
                    _unpackFixExt1 bs sequencials values
                    _unpack()
                elif header = Format.FixExt2 then
                    _unpackFixExt2 bs sequencials values
                    _unpack()
                elif header = Format.FixExt4 then
                    _unpackFixExt4 bs sequencials values
                    _unpack()
                elif header = Format.FixExt8 then
                    _unpackFixExt8 bs sequencials values
                    _unpack()
                elif header = Format.FixExt16 then
                    _unpackFixExt16 bs sequencials values
                    _unpack()
                elif header = Format.Str8 then
                    _unpackStr8 bs sequencials values
                    _unpack()
                elif header = Format.Str16 then
                    _unpackStr16 bs sequencials values
                    _unpack()
                elif header = Format.Str32 then
                    _unpackStr32 bs sequencials values
                    _unpack()
                elif header = Format.Array16 then
                    _unpackArray16 bs sequencials
                    _unpack()
                elif header = Format.Array32 then
                    _unpackArray32 bs sequencials
                    _unpack()
                elif header = Format.Map16 then
                    _unpackMap16 bs sequencials
                    _unpack()
                elif header = Format.Map32 then
                    _unpackMap32 bs sequencials
                    _unpack()
                elif (header &&& 0b11100000uy) = 0b11100000uy then
                    _unpackNegativeFixint bs sequencials values
                    _unpack()
                else
                    ()
        _unpack()
        values.ToArray()