open System.IO

open System.Text.RegularExpressions
open FSharp.Markdown
open FSharp.Formatting.Common

type Implementation =
    { Track: string
      Exercise: string }

type Story =
    { Name: string
      Description: string
      Implementations: Implementation list }

let private range (span: MarkdownSpan) =
    match span with
    | Literal(_, range) -> range
    | InlineCode(_, range) -> range
    | Strong(_, range) -> range
    | Emphasis(_, range) -> range
    | AnchorLink(_, range) -> range
    | DirectLink(_, _, _, range) -> range
    | IndirectLink(_, _, _, range) -> range
    | DirectImage(_, _, _, range) -> range
    | IndirectImage(_, _, _, range) -> range
    | HardLineBreak(range) -> range
    | LatexInlineMath(_, range) -> range
    | LatexDisplayMath(_, range) -> range
    | EmbedSpans(_, range) -> range

let private originalMarkdownCode (markdownCode: string) (spans: MarkdownSpans): string =
    let ranges = List.choose range spans 
    let startLine = ranges |> List.map (fun range -> range.StartLine) |> List.min
    let endLine = ranges |> List.map (fun range -> range.EndLine) |> List.max
    
    let code =  markdownCode.Split("\n").[startLine - 1..endLine] |> String.concat "\n"
    code.Trim()
    
let private parseName (markdown: MarkdownDocument): string option =
    let name paragraph =
        match paragraph with
        | Heading(size = 1; body = [Literal(text = name)]) -> Some name
        | _ -> None
    
    Seq.tryPick name markdown.Paragraphs
    
let private parseDescription (markdownCode: string) (markdown: MarkdownDocument): string option =
    let description pair =
        match pair with
        | Heading(size = 2; body = [Literal(text = "Story")]), Paragraph(body = body) -> Some body
        | _ -> None
    
    markdown.Paragraphs
    |> List.pairwise
    |> List.tryPick description
    |> Option.map (originalMarkdownCode markdownCode)

let private parseLink (markdown: MarkdownDocument) (key: string): string option =
    match markdown.DefinedLinks.TryGetValue(key) with
    | true, (link, _) -> Some link
    | _ -> None
    
let private parseExercise (link: string): string option =
    let matched = Regex.Match(link, "exercises/concept/(.+?)/")
    if matched.Success then Some matched.Groups.[1].Value else None 

let private parseImplementation (markdown: MarkdownDocument) (paragraphs: MarkdownParagraphs): Implementation option =
    match paragraphs with
    | [Span(body = IndirectLink(body = [Literal(text=text)]; key = key)::_)] ->
        parseLink markdown key
        |> Option.bind parseExercise
        |> Option.map (fun exercise -> { Track = text; Exercise = exercise })
    | [Span(body = [DirectLink(body = [Literal(text=text)]; link = link)])] ->
        parseExercise link
        |> Option.map (fun exercise -> { Track = text; Exercise = exercise })
    | _ -> None
    
let private parseImplementations (markdown: MarkdownDocument): Implementation list option =
    let description pair =
        match pair with
        | Heading(size = 2; body = [Literal(text = "Implementations")]), ListBlock(items = items) -> Some items
        | _ -> None
    
    markdown.Paragraphs
    |> List.pairwise
    |> List.tryPick description
    |> Option.map (List.choose (parseImplementation markdown))

let private parseStory (fileInfo: FileInfo): Story option =
    let markdownCode = File.ReadAllText(fileInfo.FullName)
    let markdown = Markdown.Parse(markdownCode)

    match parseName markdown, parseDescription markdownCode markdown, parseImplementations markdown with
    | Some name, Some description, Some implementations ->        
        { Name = name
          Description = description
          Implementations = implementations } |> Some
    | _ -> None

let private storyFiles () =
    let isStoryFile (fileInfo: FileInfo) = fileInfo.Name <> "README.md" && fileInfo.Name <> "_sidebar.md"

    let storiesDirectory = DirectoryInfo(Path.Combine("reference", "stories"))    
    storiesDirectory.EnumerateFiles("*.md")
    |> Seq.filter isStoryFile
    |> Seq.toList

[<EntryPoint>]
let main _ =        
    let stories =
        storyFiles()
        |> List.choose parseStory
        
    printfn "%A" stories
    0
