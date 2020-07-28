open System.IO

open System.Text
open System.Text.Json
open System.Text.RegularExpressions
open FSharp.Markdown
open FSharp.Formatting.Common

type ImplementationStatus =
    | Unimplemented
    | ImplementedWithoutInstructions
    | ImplementedWithInstructions

type Implementation =
    { Track: string
      Slug: string
      Exercise: string
      Status: ImplementationStatus }
    
type Concept =
    { File: FileInfo option
      Name: string }

type Story =
    { File: FileInfo
      Name: string
      Description: string
      Concept: Concept
      Implementations: Implementation list }
    
module Parser =
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
        
    let private parseImplementationStatus (languagesDirectory: DirectoryInfo) (track: string) (exercise: string): ImplementationStatus =
        let exerciseDirectory = DirectoryInfo(Path.Combine(languagesDirectory.FullName, track, "exercises", "concept", exercise))
        let instructionsFile = FileInfo(Path.Combine(languagesDirectory.FullName, track, "exercises", "concept", exercise, ".docs", "instructions.md"))
        
        match instructionsFile.Exists, exerciseDirectory.Exists with
        | true, true  -> ImplementedWithInstructions
        | false, true -> ImplementedWithoutInstructions
        | _           -> Unimplemented

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
        if matched.Success then Some (matched.Groups.[1].Value, matched.Groups.[2].Value) else None

    let private parseTrack (languagesDirectory: DirectoryInfo) (slug: string): string =
        use jsonFile = File.OpenRead(Path.Combine(languagesDirectory.FullName, slug, "config.json")) 
        let jsonDocument = JsonDocument.Parse(jsonFile)
        jsonDocument.RootElement.GetProperty("language").GetString()

    let private parseImplementation (languagesDirectory: DirectoryInfo) (markdown: MarkdownDocument) (paragraphs: MarkdownParagraphs): Implementation option =
        let createImplementation (slug, exercise) =
            { Track = parseTrack languagesDirectory slug
              Slug = slug
              Exercise = exercise
              Status = parseImplementationStatus languagesDirectory slug exercise }
        
        match paragraphs with
        | [Span(body = IndirectLink(body = [Literal]; key = key)::_)] ->
            parseLink markdown key
            |> Option.bind parseSlugAndExercise
            |> Option.map createImplementation
        | [Span(body = [DirectLink(body = [Literal]; link = link)])] ->
            parseSlugAndExercise link
            |> Option.map createImplementation
        | _ -> None
        
    let private parseImplementations (languagesDirectory: DirectoryInfo) (markdown: MarkdownDocument): Implementation list option =
        let description pair =
            match pair with
            | Heading(size = 2; body = [Literal(text = "Implementations")]), ListBlock(items = items) -> Some items
            | _ -> None
        
        markdown.Paragraphs
        |> List.pairwise
        |> List.tryPick description
        |> Option.map (List.choose (parseImplementation languagesDirectory markdown))
        
    let private parseConcept: FileInfo -> Concept =
        let typesDirectory = DirectoryInfo(Path.Combine("reference", "types"))
        let conceptsDirectory = DirectoryInfo(Path.Combine("reference", "concepts"))
        let referenceFiles = Array.append (typesDirectory.GetFiles()) (conceptsDirectory.GetFiles())
        
        fun (fileInfo: FileInfo) ->
            let concept = fileInfo.Name.Split('.').[0]
        
            let potentialFileNames =
                [sprintf "%s.md" concept
                 sprintf "%ss.md" concept
                 sprintf "%s.md" (concept.TrimEnd('s'))
                 sprintf "%s.md" (concept.Replace("-", "_"))
                 sprintf "%ss.md" (concept.Replace("-", "_"))]
            
            let conceptFile =
                referenceFiles
                |> Seq.tryFind (fun fileInfo -> List.contains fileInfo.Name potentialFileNames)
            
            { File = conceptFile
              Name = concept }

    let private parseStory (languagesDirectory: DirectoryInfo) (fileInfo: FileInfo): Story option =
        let markdownCode = File.ReadAllText(fileInfo.FullName)
        let markdown = Markdown.Parse(markdownCode)

        match parseName markdown, parseDescription markdownCode markdown, parseImplementations languagesDirectory markdown with
        | Some name, Some description, Some implementations ->
            { File = fileInfo
              Name = name
              Description = description
              Concept = parseConcept fileInfo
              Implementations = implementations } |> Some
        | _ -> None

    let private storyFiles (storiesDirectory: DirectoryInfo): FileInfo list =
        let isStoryFile (fileInfo: FileInfo) = fileInfo.Name <> "README.md" && fileInfo.Name <> "_sidebar.md"
        
        storiesDirectory.EnumerateFiles("*.md")
        |> Seq.filter isStoryFile
        |> Seq.toList

    let parseStories (storiesDirectory: DirectoryInfo) (languagesDirectory: DirectoryInfo): Story list =
        storyFiles storiesDirectory
        |> List.choose (parseStory languagesDirectory)
    
module Markdown =
    
    type MarkdownStoryImplementation =
        { Name: string
          Track: string
          Exercise: string }
        
    type MarkdownStory =
        { Name: string
          Concept: string
          Description: string }
        
    let private storyToMarkdownStory (story: Story): MarkdownStory =
        let name = sprintf "[%s](%s)" story.Name story.File.Name

        let concept =
            match story.Concept.File with
            | Some conceptFile ->
                let relativePath = Path.GetRelativePath(Path.Combine("reference", "stories"), conceptFile.FullName).Replace(Path.DirectorySeparatorChar, '/')
                sprintf "[`%s`](%s)" story.Concept.Name relativePath
            | None -> sprintf "`%s`" story.Concept.Name

        let description = Regex.Replace(story.Description.Replace("\n", " "), "\[(.+?)\]\[.+?\]", "$1")
        
        { Name = name; Concept = concept; Description = description }
    
    let private storiesToMarkdownStories (stories: Story list): MarkdownStory list =
        stories
        |> List.sortBy (fun story -> (story.Concept.Name, story.Name))
        |> List.map storyToMarkdownStory

    let private storyToMarkdownStoryImplementation (story: Story): MarkdownStoryImplementation list =
        let implementationToMarkdownStory (implementation: Implementation) =
            let name = sprintf "[%s](%s)" story.Name story.File.Name
            let track = sprintf "[%s](../../languages/%s/README.md)" implementation.Track implementation.Slug
            let exercise =
                match implementation.Status with
                | ImplementedWithInstructions -> sprintf "[%s](../../languages/%s/exercises/concept/%s/.docs/instructions.md)" implementation.Exercise implementation.Slug implementation.Exercise
                | _ -> implementation.Exercise
            
            { Name = name; Track = track; Exercise = exercise }
        
        story.Implementations
        |> List.map implementationToMarkdownStory
    
    let private storiesToMarkdownStoryImplementation (stories: Story list): MarkdownStoryImplementation list =
        stories
        |> List.collect storyToMarkdownStoryImplementation
        |> List.sort
        
    let private appendImplementationsToMarkdown (stories: Story list) (markdown: StringBuilder): StringBuilder =
        let markdownStoryImplementations = storiesToMarkdownStoryImplementation stories
        
        let renderLine nameColumn trackColumn exerciseColumn =
            markdown
                .AppendFormat(sprintf "| %s | %s | %s |" nameColumn trackColumn exerciseColumn)
                .AppendLine() |> ignore
        
        markdown
            .AppendLine("## Implementations")
            .AppendLine()
            .AppendLine("These are the implementations of the stories in the various tracks.")
            .AppendLine() |> ignore
        
        renderLine "Story" "Track" "Exercise"
        renderLine "-" "-" "-"

        for markdownStoryImplementation in markdownStoryImplementations do
            renderLine markdownStoryImplementation.Name markdownStoryImplementation.Track markdownStoryImplementation.Exercise
            
        markdown
            
    let private appendStoriesMarkdown (stories: Story list) (markdown: StringBuilder): StringBuilder =
        let markdownStories = storiesToMarkdownStories stories

        markdown
            .AppendLine("# Stories")
            .AppendLine()
            .AppendLine("_This file is auto-generated and should not be modified manually._")
            .AppendLine()
            .AppendLine("A collection of story and narrative ideas that can be used when writing Concept Exercise specs.")
            .AppendLine() |> ignore
        
        let renderLine conceptColumn nameColumn descriptionColumn =
            markdown
                .AppendFormat(sprintf "| %s | %s | %s |" conceptColumn nameColumn descriptionColumn)
                .AppendLine() |> ignore
        
        renderLine "Concept" "Story" "Description"
        renderLine "-" "-" "-"

        for markdownStory in markdownStories do
            renderLine markdownStory.Concept markdownStory.Name markdownStory.Description
            
        markdown

    let private renderToMarkdown (stories: Story list): string =
        StringBuilder()
        |> appendStoriesMarkdown stories
        |> appendImplementationsToMarkdown stories
        |> string

    let writeStories (storiesDirectory: DirectoryInfo) (stories: Story list): unit =
        let markdown = renderToMarkdown stories
        File.WriteAllText(Path.Combine(storiesDirectory.FullName, "README.md"), markdown)

module Json =
    
    [<CLIMutable>]
    type JsonImplementation =
        { Track: string
          Slug: string
          Exercise: string
          Url: string }
    
    [<CLIMutable>]
    type JsonConcept =
        { Url: string
          Name: string }
    
    [<CLIMutable>]
    type JsonStory =
        { Url: string
          Name: string
          Description: string
          Concept: JsonConcept
          Implementations: JsonImplementation list }
        
    let private normalize (str: string) =
        str.Replace("\r\n", "\n")
        
    let private implementationToJsonImplementation (implementation: Implementation) : JsonImplementation =
        let url =
            match implementation.Status with
            | ImplementedWithInstructions -> sprintf "https://github.com/exercism/v3/blob/master/languages/%s/exercises/concept/%s/.docs/instructions.md" implementation.Slug implementation.Exercise
            | ImplementedWithoutInstructions -> sprintf "https://github.com/exercism/v3/blob/master/languages/%s/exercises/concept/%s" implementation.Slug implementation.Exercise
            | _ -> null
        
        { Track = implementation.Track
          Slug = implementation.Slug
          Exercise = implementation.Exercise
          Url = url }

    let private conceptToJsonConcept (concept: Concept): JsonConcept =
        { Name = concept.Name
          Url =
              concept.File
              |> Option.map (fun file ->
                  let relativePath = Path.GetRelativePath("reference", file.FullName)
                  sprintf "https://github.com/exercism/v3/blob/master/reference/%s" (relativePath.Replace("\\", "/")))
              |> Option.toObj }
    
    let private storyToJsonStory (story: Story): JsonStory =
        { Url = sprintf "https://github.com/exercism/v3/blob/master/reference/stories/%s" story.File.Name
          Name = story.Name
          Description = normalize story.Description
          Concept = conceptToJsonConcept story.Concept
          Implementations = List.map implementationToJsonImplementation story.Implementations }
    
    let private renderToJson (stories: Story list): string =
        let jsonStories =
            stories
            |> List.map storyToJsonStory
            |> List.sortBy (fun story -> (story.Name, story.Url))
        
        let options = JsonSerializerOptions()
        options.WriteIndented <- true
        options.PropertyNamingPolicy <- JsonNamingPolicy.CamelCase
        
        JsonSerializer.Serialize(jsonStories, options)
    
    let writeStories (storiesDirectory: DirectoryInfo) (stories: Story list): unit =
        let json = renderToJson stories
        File.WriteAllText(Path.Combine(storiesDirectory.FullName, "stories.json"), json)

[<EntryPoint>]
let main _ =
    let storiesDirectory = DirectoryInfo(Path.Combine("reference", "stories"))
    let languagesDirectory = DirectoryInfo("languages")
    let stories = Parser.parseStories storiesDirectory languagesDirectory
    
    Markdown.writeStories storiesDirectory stories
    Json.writeStories storiesDirectory stories

    0
