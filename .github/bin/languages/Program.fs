open System.IO

open System.Text
open System.Text.Json
open System.Text.Json.Serialization

type ConceptExerciseStatus =
    | Unimplemented
    | ImplementedWithoutInstructions
    | ImplementedWithInstructions

type Concept =
    { Name: string
      File: FileInfo option }

type ConceptExercise =
    { Slug: string
      Concepts: Concept[]
      Prerequisites: Concept[]
      Status: ConceptExerciseStatus }

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
        
    let private parseConcept: string -> Concept =
        let typesDirectory = DirectoryInfo(Path.Combine("reference", "types"))
        let conceptsDirectory = DirectoryInfo(Path.Combine("reference", "concepts"))
        let referenceFiles = Array.append (typesDirectory.GetFiles()) (conceptsDirectory.GetFiles())
        
        fun (concept: string) ->
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
    
    let private configJsonPath (languageDirectory: DirectoryInfo) = Path.Combine(languageDirectory.FullName, "config.json")
    
    let private hasConfigJsonFile (languageDirectory: DirectoryInfo) =
        configJsonPath languageDirectory
        |> File.Exists
        
    let private parseConfigJson (languageDirectory: DirectoryInfo) =
        configJsonPath languageDirectory
        |> File.ReadAllText
        |> JsonSerializer.Deserialize<JsonTrack>

    let private parseConceptExerciseStatus (languagesDirectory: DirectoryInfo)  (track: string) (exercise: string): ConceptExerciseStatus =
        let exerciseDirectory = DirectoryInfo(Path.Combine(languagesDirectory.FullName, track, "exercises", "concept", exercise))
        let instructionsFile = FileInfo(Path.Combine(languagesDirectory.FullName, track, "exercises", "concept", exercise, ".docs", "instructions.md"))

        match instructionsFile.Exists, exerciseDirectory.Exists with
        | true, true  -> ImplementedWithInstructions
        | false, true -> ImplementedWithoutInstructions
        | _           -> Unimplemented
        
    let private parseConceptExercise (languagesDirectory: DirectoryInfo) (languageDirectory: DirectoryInfo) (jsonConceptExercise: JsonConceptExercise): ConceptExercise =
        let emptyIfNull arr = if arr = null then Array.empty else arr
        let toConcepts arr = arr |> emptyIfNull |> Array.map parseConcept
        
        { Slug = jsonConceptExercise.Slug
          Concepts = jsonConceptExercise.Concepts |> toConcepts
          Prerequisites = jsonConceptExercise.Prerequisites |> toConcepts
          Status = parseConceptExerciseStatus languagesDirectory languageDirectory.Name jsonConceptExercise.Slug }

    let private parseTrack (languagesDirectory: DirectoryInfo) (languageDirectory: DirectoryInfo): Track =
        let configJson = parseConfigJson languageDirectory

        { Name = configJson.Language
          Slug = languageDirectory.Name
          Exercises = { Concept = Array.map (parseConceptExercise languagesDirectory languageDirectory) configJson.Exercises.Concept } }

    let parseTracks (languagesDirectory: DirectoryInfo): Track list =
        languagesDirectory.EnumerateDirectories()
        |> Seq.filter hasConfigJsonFile
        |> Seq.map (parseTrack languagesDirectory)
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
            let exercisesCount = track.Exercises.Concept |> Array.filter (fun exercise -> exercise.Status <> Unimplemented) |> Array.length
            let exercisesLink = sprintf "[%d](./%s/config.json)" exercisesCount track.Slug
            renderLine trackLink exercisesLink

        markdown.AppendLine()
    
    let private appendExercises (languagesDirectory: DirectoryInfo) (tracks: Track list) (markdown: StringBuilder): StringBuilder =
        markdown
            .AppendLine("## Implemented Concept Exercises")
            .AppendLine()
            .AppendLine("These are the Concept Exercises that have currently been implemented:")
            .AppendLine() |> ignore
        
        let renderLine trackColumn exerciseColumn conceptsColumn prerequisitesColumn =
            markdown
                .AppendFormat(sprintf "| %s | %s | %s | %s |" trackColumn exerciseColumn conceptsColumn prerequisitesColumn)
                .AppendLine() |> ignore
        
        renderLine "Track" "Exercise" "Concepts" "Prerequisites"
        renderLine "-" "-" "-" "-"

        for track in tracks do
            let trackLink = sprintf "[%s](./%s/README.md)" track.Name track.Slug
            
            let exercises =
                track.Exercises.Concept
                |> Array.filter (fun exercise -> exercise.Status <> Unimplemented)
                |> Array.sortBy (fun exercise -> exercise.Slug)
            
            for conceptExercise in exercises do
                let renderConcept (concept: Concept): string =
                    match concept.File with
                    | Some file ->
                        sprintf "[`%s`](%s)" concept.Name (Path.GetRelativePath(languagesDirectory.FullName, file.FullName).Replace(Path.DirectorySeparatorChar, '/'))
                    | None -> sprintf "`%s`" concept.Name
                
                let renderArray (arr: Concept[]) =
                    if Array.isEmpty arr then
                        "-"
                    else
                        arr
                        |> Array.sortBy (fun concept -> concept.Name)
                        |> Array.map renderConcept
                        |> String.concat ", "  
                
                let exerciseLink =
                    match conceptExercise.Status with
                    | ImplementedWithInstructions -> sprintf "[%s](./%s/exercises/concept/%s/.docs/instructions.md)" conceptExercise.Slug track.Slug conceptExercise.Slug
                    | ImplementedWithoutInstructions -> conceptExercise.Slug
                    | Unimplemented -> conceptExercise.Slug

                let concepts = conceptExercise.Concepts |> renderArray  
                let prerequisites = conceptExercise.Prerequisites |> renderArray
                renderLine trackLink exerciseLink concepts prerequisites

        markdown.AppendLine()
        
    let private appendFooter (markdown: StringBuilder): StringBuilder =
        markdown
            .AppendLine("If you would like to create a new language track for v3, please [open an issue here](https://github.com/exercism/request-new-language-track).")
            .AppendLine()

    let private renderToMarkdown (languagesDirectory: DirectoryInfo) (tracks: Track list): string =
        StringBuilder()
        |> appendHeader
        |> appendTracks tracks
        |> appendExercises languagesDirectory tracks
        |> appendFooter
        |> string

    let writeTracks (languagesDirectory: DirectoryInfo) (tracks: Track list): unit =
        let markdown = renderToMarkdown languagesDirectory tracks
        File.WriteAllText(Path.Combine(languagesDirectory.FullName, "README.md"), markdown)

module Json =
    [<CLIMutable>]
    type JsonConcept =
        { Url: string
          Name: string }
    
    [<CLIMutable>]
    type JsonConceptExercise =
        { Url: string
          Slug: string
          Concepts: JsonConcept[]
          Prerequisites: JsonConcept[] }

    [<CLIMutable>]
    type JsonExercises =
        { Concept: JsonConceptExercise[] }

    [<CLIMutable>]
    type JsonTrack =
        { Url: string
          Name: string
          Slug: string
          Exercises: JsonExercises } 
    
    let private conceptToJsonConcept (concept: Concept): JsonConcept =
        { Url =
            concept.File
            |> Option.map (fun file -> sprintf "https://github.com/exercism/v3/tree/master/%s" (Path.GetRelativePath(Directory.GetCurrentDirectory(), file.FullName).Replace(Path.DirectorySeparatorChar, '/')))
            |> Option.toObj
          Name = concept.Name }
    
    let private conceptExerciseToJsonConceptExercise (track: Track) (conceptExercise: ConceptExercise): JsonConceptExercise =
        { Url = sprintf "https://github.com/exercism/v3/tree/master/languages/%s/exercises/concept/%s" track.Slug conceptExercise.Slug
          Slug = conceptExercise.Slug
          Concepts = Array.map conceptToJsonConcept conceptExercise.Concepts
          Prerequisites = Array.map conceptToJsonConcept conceptExercise.Prerequisites }
    
    let private trackToJsonTrack (track: Track): JsonTrack =
        { Url = sprintf "https://github.com/exercism/v3/tree/master/languages/%s" track.Slug
          Name = track.Name
          Slug = track.Slug
          Exercises =
              { Concept =
                  track.Exercises.Concept
                  |> Array.filter (fun exercise -> exercise.Status <> Unimplemented)
                  |> Array.map (conceptExerciseToJsonConceptExercise track)  } }
    
    let private renderToJson (tracks: Track list): string =
        let jsonTracks = List.map trackToJsonTrack tracks
        
        let options = JsonSerializerOptions()
        options.WriteIndented <- true
        options.PropertyNamingPolicy <- JsonNamingPolicy.CamelCase
        
        JsonSerializer.Serialize(jsonTracks, options)
    
    let writeTracks (languagesDirectory: DirectoryInfo) (tracks: Track list): unit =
        let json = renderToJson tracks
        File.WriteAllText(Path.Combine(languagesDirectory.FullName, "languages.json"), json)

[<EntryPoint>]
let main _ =
    let languagesDirectory = DirectoryInfo("languages")
    let tracks = Parser.parseTracks languagesDirectory
    
    Markdown.writeTracks languagesDirectory tracks
    Json.writeTracks languagesDirectory tracks

    0
