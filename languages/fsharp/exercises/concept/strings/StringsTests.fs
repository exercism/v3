module StringsTests

open FsUnit.Xunit
open Xunit

open Strings

[<Fact>]
let ``Error message``() = message "[ERROR]: Stack overflow" |> should equal "Stack overflow"

[<Fact>]
let ``Warning message``() = message "[WARNING]: Disk almost full" |> should equal "Disk almost full"

[<Fact>]
let ``Info message``() = message "[INFO]: File moved" |> should equal "File moved"

[<Fact>]
let ``Message with leading and trailing white space``() =
    message "[WARNING]:   \tTimezone not set  \r\n"
    |> should equal "Timezone not set"

[<Fact>]
let ``Error log level``() = logLevel "[ERROR]: Disk full" |> should equal "error"

[<Fact>]
let ``Warning log level``() = logLevel "[WARNING]: Unsafe password" |> should equal "warning"

[<Fact>]
let ``Info log level``() = logLevel "[INFO]: Timezone changed" |> should equal "info"

[<Fact>]
let ``Error reformat``() = reformat "[ERROR]: Segmentation fault" |> should equal "Segmentation fault (error)"

[<Fact>]
let ``Warning reformat``() =
    reformat "[WARNING]: Decreased performance" |> should equal "Decreased performance (warning)"

[<Fact>]
let ``Info reformat``() = reformat "[INFO]: Disk defragmented" |> should equal "Disk defragmented (info)"

[<Fact>]
let ``Reformat with leading and trailing white space``() =
    reformat "[ERROR]: \t Corrupt disk\t \t \r\n"
    |> should equal "Corrupt disk (error)"
