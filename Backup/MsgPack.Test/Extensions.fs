namespace MsgPack.Test

open NUnit.Framework

module Extensions =

    let assertEqualTo<'a> (expected: 'a) (actual: 'a) = Assert.That(actual, Is.EqualTo(expected))
    let assertEquivalentTo<'a> expected (actual: 'a) = Assert.That(actual, Is.EquivalentTo(expected))
