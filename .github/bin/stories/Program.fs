open System.IO

open System.Text
open System.Text.RegularExpressions
open FSharp.Markdown
open FSharp.Formatting.Common

type Implementation =
    { Track: string
      Slug: string
      Exercise: string }

type Story =
    { FileName: string
      Name: string
      Description: string
      Implementations: Implementation list }
    
module Input =
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
        
    let private parseSlugAndExercise (link: string): (string * string) option =
        let matched = Regex.Match(link, "languages/(.+?)/exercises/concept/(.+?)/")
        if matched.Success then Some (matched.Groups.[1].Value, matched.Groups.[2].Value)  else None 

    let private parseImplementation (markdown: MarkdownDocument) (paragraphs: MarkdownParagraphs): Implementation option =
        match paragraphs with
        | [Span(body = IndirectLink(body = [Literal(text=text)]; key = key)::_)] ->
            parseLink markdown key
            |> Option.bind parseSlugAndExercise
            |> Option.map (fun (slug, exercise) -> { Track = text; Slug = slug; Exercise = exercise })
        | [Span(body = [DirectLink(body = [Literal(text=text)]; link = link)])] ->
            parseSlugAndExercise link
            |> Option.map (fun (slug, exercise) -> { Track = text; Slug = slug; Exercise = exercise })
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
            { FileName = fileInfo.Name
              Name = name
              Description = description
              Implementations = implementations } |> Some
        | _ -> None

    let private storyFiles (storiesDirectory: DirectoryInfo): FileInfo list =
        let isStoryFile (fileInfo: FileInfo) = fileInfo.Name <> "README.md" && fileInfo.Name <> "_sidebar.md"
        
        storiesDirectory.EnumerateFiles("*.md")
        |> Seq.filter isStoryFile
        |> Seq.toList

    let parseStories (storiesDirectory: DirectoryInfo): Story list =
        storyFiles storiesDirectory
        |> List.choose parseStory

module Output =
    
    type MarkdownStoryImplementation =
        { Name: string
          Track: string
          Exercise: string }
        
    type MarkdownStory =
        { Name: string
          Concept: string
          Description: string }
        
    let private storyToMarkdownStory (story: Story): MarkdownStory =
        let name = sprintf "[%s](%s)" story.Name story.FileName
        let concept = sprintf "`%s`" (story.FileName.Split('.').[0])
        let description = Regex.Replace(story.Description.Replace("\n", " "), "\[(.+?)\]\[.+?\]", "$1")
        
        { Name = name; Concept = concept; Description = description }
    
    let private storiesToMarkdownStories (stories: Story list): MarkdownStory list =
        stories
        |> List.map storyToMarkdownStory
        |> List.sort

    let private storyToMarkdownStoryImplementation (story: Story): MarkdownStoryImplementation list =
        let implementationToMarkdownStory (implementation: Implementation) =
            let name = sprintf "[%s](%s)" story.Name story.FileName
            let track = sprintf "[%s](../../languages/%s/README.md)" implementation.Track implementation.Slug
            let exercise = sprintf "[%s](../../languages/%s/exercises/concept/%s/.docs/instructions.md)" implementation.Exercise implementation.Slug implementation.Exercise
            
            { Name = name; Track = track; Exercise = exercise }
        
        story.Implementations
        |> List.map implementationToMarkdownStory
    
    let private storiesToMarkdownStoryImplementation (stories: Story list): MarkdownStoryImplementation list =
        stories
        |> List.collect storyToMarkdownStoryImplementation
        |> List.sort
        
    let private appendImplementationsToMarkdown (stories: Story list) (markdown: StringBuilder): StringBuilder =
        let markdownStoryImplementations = storiesToMarkdownStoryImplementation stories
        
        let columnLength column = markdownStoryImplementations |> List.map column |> List.map String.length |> List.max            
        let nameColumnLength = columnLength (fun story -> story.Name)
        let trackColumnLength = columnLength (fun story -> story.Track)
        let exerciseColumnLength = columnLength (fun story -> story.Exercise)
        
        let renderLine (nameColumn: string) (trackColumn: string) (exerciseColumn: string) =
            markdown.AppendFormat(sprintf "| {0,-%d} | {1,-%d} | {2,-%d} |" nameColumnLength trackColumnLength exerciseColumnLength, nameColumn, trackColumn, exerciseColumn) |> ignore
            markdown.AppendLine() |> ignore
        
        markdown.AppendLine("## Implementations") |> ignore
        markdown.AppendLine() |> ignore
        markdown.AppendLine("These are the implementations of the stories in the various tracks.") |> ignore
        markdown.AppendLine() |> ignore
        
        renderLine "Story" "Track" "Exercise"
        renderLine (System.String('-', nameColumnLength)) (System.String('-', trackColumnLength)) (System.String('-', exerciseColumnLength))

        for markdownStoryImplementation in markdownStoryImplementations do
            renderLine markdownStoryImplementation.Name markdownStoryImplementation.Track markdownStoryImplementation.Exercise
            
        markdown
            
    let private appendStoriesMarkdown (stories: Story list) (markdown: StringBuilder): StringBuilder =
        let markdownStories = storiesToMarkdownStories stories
        
        let columnLength column = markdownStories |> List.map column |> List.map String.length |> List.max
        
        let nameColumnLength = columnLength (fun story -> story.Name)
        let conceptColumnLength = columnLength (fun story -> story.Concept)
        let descriptionColumnLength = columnLength (fun story -> story.Description)
        
        markdown.AppendLine("# Stories") |> ignore
        markdown.AppendLine() |> ignore
        markdown.AppendLine("These are the stories currently implemented.") |> ignore
        markdown.AppendLine() |> ignore
        
        let renderLine (nameColumn: string) (conceptColumn: string) (descriptionColumn: string) =
            markdown.AppendFormat(sprintf "| {0,-%d} | {1,-%d} | {2,-%d} |" nameColumnLength conceptColumnLength descriptionColumnLength, nameColumn, conceptColumn, descriptionColumn) |> ignore
            markdown.AppendLine() |> ignore
        
        renderLine "Name" "Concept" "Description"
        renderLine (System.String('-', nameColumnLength)) (System.String('-', conceptColumnLength)) (System.String('-', descriptionColumnLength))

        for markdownStory in markdownStories do
            renderLine markdownStory.Name markdownStory.Concept markdownStory.Description
            
        markdown

    let private renderToMarkdown (stories: Story list): string =
        StringBuilder()
        |> appendStoriesMarkdown stories
        |> appendImplementationsToMarkdown stories
        |> string

    let writeAsMarkdown (storiesDirectory: DirectoryInfo) (stories: Story list): unit =
        let markdown = renderToMarkdown stories
        File.WriteAllText(Path.Combine(storiesDirectory.FullName, "stories.md"), markdown) 

[<EntryPoint>]
let main _ =
    let storiesDirectory = DirectoryInfo(Path.Combine("reference", "stories"))
    
    Input.parseStories storiesDirectory
    |> Output.writeAsMarkdown storiesDirectory

    0
