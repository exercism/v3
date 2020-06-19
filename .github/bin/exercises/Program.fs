open System.IO

open System.Text
open System.Text.Json
open System.Text.Json.Serialization

type ConceptExercise =
    { Slug: string
      Concepts: string[]
      Prerequisites: string[] }

type Exercises =
    { Concept: ConceptExercise[] }

type Track =
    { Name: string
      Slug: string
      Exercises: Exercises } 
    
module Parser =

    [<CLIMutable>]
    type JsonConceptExercise =
        { [<JsonPropertyName("slug")>] Slug: string
          [<JsonPropertyName("concepts")>] Concepts: string[]
          [<JsonPropertyName("prerequisites")>] Prerequisites: string[] }

    [<CLIMutable>]
    type JsonExercises =
        { [<JsonPropertyName("concept")>] Concept: JsonConceptExercise[] }

    [<CLIMutable>]
    type JsonTrack =
        { [<JsonPropertyName("language")>] Language: string
          [<JsonPropertyName("exercises")>] Exercises: JsonExercises }
    
    let private configJsonPath (languageDirectory: DirectoryInfo) = Path.Combine(languageDirectory.FullName, "config.json")
    
    let private hasConfigJsonFile (languageDirectory: DirectoryInfo) =
        configJsonPath languageDirectory
        |> File.Exists
        
    let private parseConfigJson (languageDirectory: DirectoryInfo) =
        configJsonPath languageDirectory
        |> File.ReadAllText
        |> JsonSerializer.Deserialize<JsonTrack>

    let private parseTrack (languageDirectory: DirectoryInfo): Track =
        let configJson = parseConfigJson languageDirectory
        let toConceptExercise (jsonConceptExercise: JsonConceptExercise) : ConceptExercise =
            let emptyIfNull arr = if arr = null then Array.empty else arr
            
            { Slug = jsonConceptExercise.Slug
              Concepts = jsonConceptExercise.Concepts |> emptyIfNull
              Prerequisites = jsonConceptExercise.Prerequisites |> emptyIfNull }

        { Name = configJson.Language
          Slug = languageDirectory.Name
          Exercises = { Concept = Array.map toConceptExercise configJson.Exercises.Concept } }

    let parseTracks (languagesDirectory: DirectoryInfo): Track list =
        languagesDirectory.EnumerateDirectories()
        |> Seq.filter hasConfigJsonFile
        |> Seq.map parseTrack
        |> Seq.sortBy (fun track -> track.Name)
        |> Seq.toList
    
module Markdown =

    let private appendHeader (markdown: StringBuilder): StringBuilder =
        markdown
            .AppendLine("# Languages")
            .AppendLine()
            .AppendLine("_This file is auto-generated and should not be modified manually._")
            .AppendLine()
    
    let private appendTracks (tracks: Track list) (markdown: StringBuilder): StringBuilder =
        markdown
            .AppendLine("## Contributing")
            .AppendLine()
            .AppendLine("Thanks for wanting to contribute to one of Exercism's language tracks! Contributions are very welcome!")
            .AppendLine()
            .AppendLine("To contribute, please select the language you'd like to contribute to:")
            .AppendLine() |> ignore
        
        let renderLine trackColumn exercisesColumn =
            markdown
                .AppendFormat(sprintf "| %s | %s |" trackColumn exercisesColumn)
                .AppendLine() |> ignore
        
        renderLine "Track" "# Concept Exercises"
        renderLine "-" "-"

        for track in tracks do
            let trackLink = sprintf "[%s](./%s/README.md)" track.Name track.Slug
            let exercisesLink = sprintf "[%d](https://github.com/exercism/v3/tree/master/languages/%s/exercises/concept)" track.Exercises.Concept.Length track.Slug
            renderLine trackLink exercisesLink

        markdown.AppendLine()
    
    let private appendExercises (tracks: Track list) (markdown: StringBuilder): StringBuilder =
        markdown
            .AppendLine("## Implemented Concept Exercises")
            .AppendLine() |> ignore
        
        let renderLine trackColumn exerciseColumn conceptsColumn prerequisitesColumn =
            markdown
                .AppendFormat(sprintf "| %s | %s | %s | %s |" trackColumn exerciseColumn conceptsColumn prerequisitesColumn)
                .AppendLine() |> ignore
        
        renderLine "Track" "Exercise" "Concepts" "Prerequisites"
        renderLine "-" "-" "-" "-"

        for track in tracks do
            let trackLink = sprintf "[%s](./%s/README.md)" track.Name track.Slug
            
            for conceptExercise in track.Exercises.Concept |> Array.sortBy (fun exercise -> exercise.Slug) do
                let renderArray arr =
                    if Array.isEmpty arr then "-" else arr |> Array.sort |> String.concat ", "  
                
                let exerciseLink = sprintf "[%s](https://github.com/exercism/v3/tree/master/languages/%s/exercises/concept/%s/.docs/instructions.md)" conceptExercise.Slug track.Slug conceptExercise.Slug
                let concepts = conceptExercise.Concepts |> renderArray  
                let prerequisites = conceptExercise.Prerequisites |> renderArray
                renderLine trackLink exerciseLink concepts prerequisites

        markdown.AppendLine()
        
    let private appendFooter (markdown: StringBuilder): StringBuilder =
        markdown
            .AppendLine("If you would like to create a new language track for v3, please [open an issue here](https://github.com/exercism/request-new-language-track).")
            .AppendLine()

    let private renderToMarkdown (tracks: Track list): string =
        StringBuilder()
        |> appendHeader
        |> appendTracks tracks
        |> appendExercises tracks
        |> appendFooter
        |> string

    let writeTracks (languagesDirectory: DirectoryInfo) (tracks: Track list): unit =
        let markdown = renderToMarkdown tracks
        File.WriteAllText(Path.Combine(languagesDirectory.FullName, "README.md"), markdown)

module Json =
//    
//    [<CLIMutable>]
//    type JsonImplementation =
//        { Track: string
//          Slug: string
//          Exercise: string
//          Url: string }
//    
//    [<CLIMutable>]
//    type JsonConcept =
//        { Url: string
//          Name: string }
//    
//    [<CLIMutable>]
//    type JsonStory =
//        { Url: string
//          Name: string
//          Description: string
//          Concept: JsonConcept
//          Implementations: JsonImplementation list }
//        
//    let private implementationToJsonImplementation (implementation: Implementation) : JsonImplementation =
//        { Track = implementation.Track
//          Slug = implementation.Slug
//          Exercise = implementation.Exercise
//          Url = sprintf "https://github.com/exercism/v3/blob/master/languages/%s/exercises/concept/%s/.docs/instructions.md" implementation.Slug implementation.Exercise }
//
//    let private conceptToJsonConcept (concept: Concept): JsonConcept =
//        { Name = concept.Name
//          Url =
//              concept.File
//              |> Option.map (fun file ->
//                  let relativePath = Path.GetRelativePath("reference", file.FullName)
//                  sprintf "https://github.com/exercism/v3/blob/master/reference/%s" (relativePath.Replace("\\", "/")))
//              |> Option.toObj }
//    
//    let private storyToJsonStory (story: Story): JsonStory =
//        { Url = sprintf "https://github.com/exercism/v3/blob/master/reference/stories/%s" story.File.Name
//          Name = story.Name
//          Description = story.Description
//          Concept = conceptToJsonConcept story.Concept
//          Implementations = List.map implementationToJsonImplementation story.Implementations }
//    
//    let private renderToJson (stories: Track list): string =
//        let jsonStories = List.map storyToJsonStory stories
//        
//        let options = JsonSerializerOptions()
//        options.WriteIndented <- true
//        options.PropertyNamingPolicy <- JsonNamingPolicy.CamelCase
//        
//        JsonSerializer.Serialize(jsonStories, options)
//    
//    let writeStories (storiesDirectory: DirectoryInfo) (stories: Story list): unit =
//        let json = renderToJson stories
//        File.WriteAllText(Path.Combine(storiesDirectory.FullName, "stories.json"), json)

[<EntryPoint>]
let main _ =
    let languagesDirectory = DirectoryInfo("languages")
    let tracks = Parser.parseTracks languagesDirectory
    
    Markdown.writeTracks languagesDirectory tracks
//    Json.writeStories languagesDirectory tracks

    0
