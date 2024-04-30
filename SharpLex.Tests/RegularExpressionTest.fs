namespace MathService.Tests

open System
open NUnit.Framework
open MathService

open RegularExpression

[<TestFixture>]
type TestClass () =

    [<Test>]
    member this.TestCharacter() =
        Assert.AreEqual(process_string "b", Char "b")