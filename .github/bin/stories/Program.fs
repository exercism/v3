open System.IO

open FSharp.Markdown
open FSharp.Formatting.Common

type Implementation =
    { Track: string
      Link: string
      Reference: bool }

type Story =
    { Name: string
      Description: string
      Implementations: Implementation list }

let parseStory (fileInfo: FileInfo): Story option =
    let markdown = Markdown.Parse(File.ReadAllText(fileInfo.FullName))
    
    let state = (Option.None, Option.None, Option.None)
    
    let rec parse (name, story, implementations) paragraphs =
        match paragraphs with
        | Heading(size = 1; body = [Literal(text=text)])::xs ->
            parse ((Some text, story, implementations)) xs
        | Heading(size = 2; body = [Literal(text = "Story")])::Paragraph(body = [Literal(text=text)])::xs ->
            parse ((name, Some text, implementations)) xs
        | Heading(size = 2; body = [Literal(text = "Implementations")])::ListBlock(items = items)::xs ->
            
            for item in items do
                match item with
                | [x] ->
                    match x with
                    | Span(body = z) ->
                        match z with
                        | [IndirectLink(body = [Literal(text=body)]); Literal(text=text)] ->
                            printfn "single link %A" body
                        | IndirectLink(body = [Literal(text=body)])::xs ->
                            printfn "z %A" body
                    | _ ->
                        printfn "n"
                    
                    printfn "Match x"
                | Paragraph(body = y)::xs ->
                    printfn "para"
                | Paragraph(body = IndirectLink(body = linkBody)::_)::xs ->
                    printfn "%A" linkBody
                | _ -> ()
            
            printfn "%A" items
//            let impls =
//                items
//                |> List.choose (fun item ->
//                    match item with
//                    | [Paragraph(body = body)] ->
//                        match body with
//                        | IndirectLink(body = [Literal(text=body)]; key = key)::Literal(text=text)::_ ->
//                            { Track = body
//                              Link = ""
//                              Reference = text.Contains("reference") } |> Some
//                        | IndirectLink(body = [Literal(text=body)])::_ ->
//                            { Track = body
//                              Link = ""
//                              Reference = false } |> Some
//                        | _ ->
//                            None
//                    | _ -> None)
            parse ((name, story, Some [])) xs
        | _::xs -> parse (name, story, implementations) xs
        | [] -> (name, story, implementations)
    
    match parse state markdown.Paragraphs with
    | (Some name, Some story, _) ->
        { Name = name
          Description = story
          Implementations = [] } |> Some
    | _ ->
        None

[<EntryPoint>]
let main argv =
    let isStoryFile (fileInfo: FileInfo) = fileInfo.Name <> "README.md" && fileInfo.Name <> "_sidebar.md"
    
    let storiesDirectory = DirectoryInfo(Path.Combine("reference", "stories"))
    let storyFiles =
        storiesDirectory.EnumerateFiles("*.md")
        |> Seq.filter isStoryFile
        |> Seq.filter (fun x -> x.Name.Contains("numbers.car-production-line"))
        |> Seq.toList

    let stories = List.choose parseStory storyFiles
    
    
    printfn "Hello World from F#!"
    0 // return an integer exit code
