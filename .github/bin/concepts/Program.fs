open System.IO

open System.Text
open System.Text.Json
open System.Text.Json.Serialization
    
type Implementation =
    { Exercise: string
      Track: string
      Language: string }
    
type ConceptCategory =
    | Concepts
    | Types

type Concept =
    { Name: string
      Variations: string list
      Category: ConceptCategory
      Implementations: Implementation list }
    
module Parser =
    
    [<CLIMutable>]
    type JsonConceptExercise =
        { [<JsonPropertyName("slug")>] Slug: string
          [<JsonPropertyName("concepts")>] Concepts: string[] }

    [<CLIMutable>]
    type JsonExercises =
        { [<JsonPropertyName("concept")>] Concept: JsonConceptExercise[] }

    [<CLIMutable>]
    type JsonTrack =
        { [<JsonPropertyName("language")>] Language: string
          [<JsonPropertyName("exercises")>] Exercises: JsonExercises }
        
    let private conceptVariations (concept: string): string list =
        let withoutTrailingS = concept.TrimEnd('s')
        let snakeCase = withoutTrailingS.Replace("-", "_")
        let withoutSpaces = withoutTrailingS.Replace(" ", "")
        
        [concept
         sprintf "%s" withoutTrailingS
         sprintf "%s" snakeCase         
         sprintf "%s" withoutSpaces         
         sprintf "%ss" withoutTrailingS
         sprintf "%ss" snakeCase
         sprintf "%ss" withoutSpaces]
        |> List.distinct

    let private isConceptFile (file: FileInfo) =
        file.Name <> "README.md" && file.Name <> "_sidebar.md"
    
    let private conceptsDirectoryForCategory (referenceDirectory: DirectoryInfo) (category: ConceptCategory): DirectoryInfo =
        match category with
        | Concepts -> DirectoryInfo(Path.Combine(referenceDirectory.FullName, "concepts"))
        | Types -> DirectoryInfo(Path.Combine(referenceDirectory.FullName, "types"))
        
    let private conceptFilesForCategory (referenceDirectory: DirectoryInfo) (category: ConceptCategory): FileInfo list =
        let conceptsDirectory = conceptsDirectoryForCategory referenceDirectory category
        conceptsDirectory.EnumerateFiles()
        |> Seq.filter isConceptFile
        |> Seq.toList
    
    let private configJsonPath (languageDirectory: DirectoryInfo) = Path.Combine(languageDirectory.FullName, "config.json")
    
    let private hasConfigJsonFile (languageDirectory: DirectoryInfo) =
        configJsonPath languageDirectory
        |> File.Exists
        
    let private parseConfigJson (languageDirectory: DirectoryInfo) =
        configJsonPath languageDirectory
        |> File.ReadAllText
        |> JsonSerializer.Deserialize<JsonTrack>
    
    let private parseImplementationsOfTrack (languageDirectory: DirectoryInfo): Map<string, Implementation> =
        let jsonTrack = parseConfigJson languageDirectory        
        let toImplementation (exercise: JsonConceptExercise): Implementation =
            { Exercise = exercise.Slug
              Track = languageDirectory.Name
              Language = jsonTrack.Language }
        
        jsonTrack.Exercises.Concept
        |> Array.collect (fun exercise -> exercise.Concepts |> Array.map (fun concept -> (concept, toImplementation exercise)))
        |> Map.ofArray
    
    let private parseImplementations (languagesDirectory: DirectoryInfo) : Map<string, Implementation list> =
        let trackImplementations =
            languagesDirectory.EnumerateDirectories()
            |> Seq.filter hasConfigJsonFile
            |> Seq.map parseImplementationsOfTrack
        
        trackImplementations
        |> Seq.fold (fun acc elem ->
            elem
            |> Map.fold (fun acc key elem ->
                match Map.tryFind key acc with
                | Some implementations -> Map.add key (elem :: implementations) acc 
                | None -> Map.add key [elem] acc) acc
        ) Map.empty
            
    let private conceptName (conceptFile: FileInfo): string = conceptFile.Name.Split('.').[0]
        
    let private parseConcept (implementations: Map<string, Implementation list>) (category: ConceptCategory) (conceptFile: FileInfo): Concept =
        let concept = conceptName conceptFile
        let variations = conceptVariations concept
        let implementations = variations |> Seq.tryPick (fun concept -> Map.tryFind concept implementations) |> Option.defaultValue List.empty
        
        { Name = concept
          Category = category
          Variations = variations
          Implementations = implementations }

    let private parseConceptsOfCategory (referenceDirectory: DirectoryInfo) (languagesDirectory: DirectoryInfo) (category: ConceptCategory): Concept list =
        let implementations = parseImplementations languagesDirectory
        
        conceptFilesForCategory referenceDirectory category
        |> List.map (parseConcept implementations category)
    
    let parseConcepts (referenceDirectory: DirectoryInfo) (languagesDirectory: DirectoryInfo): Concept list =
        [Concepts; Types]
        |> Seq.collect (fun category -> parseConceptsOfCategory referenceDirectory languagesDirectory category)
        |> Seq.sortBy (fun concept -> concept.Name)
        |> Seq.toList
    
module Markdown =

    let private appendHeader (markdown: StringBuilder): StringBuilder =
        markdown
            .AppendLine("# Reference documents")
            .AppendLine()
            .AppendLine("_This file is auto-generated and should not be modified manually._")
            .AppendLine()
            
    let private appendTypes (markdown: StringBuilder): StringBuilder =
        markdown
            .AppendLine("# Reference documents")
            .AppendLine()
            .AppendLine("There are several types of reference documents:")
            .AppendLine()            
            .AppendLine("- [Concepts][concepts]")
            .AppendLine("- [Paradigms][paradigms]")
            .AppendLine("- [Stories][stories]")
            .AppendLine("- [Tooling][tooling]")
            .AppendLine("- [Types][types]")
            .AppendLine()

    let private appendContributing (markdown: StringBuilder): StringBuilder =
        markdown
            .AppendLine("## Contributing")
            .AppendLine()
            .AppendLine("Thanks for wanting to contribute to Exercism's reference documents! Contributions are very welcome!")
            .AppendLine()
            .AppendLine("To contribute, please go to the [concepts][concepts], [paradigms][paradigms], [stories][stories], [tooling][tooling] or [types][types] page to see which documents have been written. You can then contribute by submitting a PR to update an existing document, or to add a missing document.")
            .AppendLine()
    
    let private appendExercises (languagesDirectory: DirectoryInfo) (concepts: Concept list) (markdown: StringBuilder): StringBuilder =
//        markdown
//            .AppendLine("## Implemented Concept Exercises")
//            .AppendLine()
//            .AppendLine("These are the Concept Exercises that have currently been implemented:")
//            .AppendLine() |> ignore
//        
//        let renderLine trackColumn exerciseColumn conceptsColumn prerequisitesColumn =
//            markdown
//                .AppendFormat(sprintf "| %s | %s | %s | %s |" trackColumn exerciseColumn conceptsColumn prerequisitesColumn)
//                .AppendLine() |> ignore
//        
//        renderLine "Track" "Exercise" "Concepts" "Prerequisites"
//        renderLine "-" "-" "-" "-"
//
//        for track in concepts do
//            let trackLink = sprintf "[%s](./%s/README.md)" track.Name track.Slug
//            
//            for conceptExercise in track.Exercises.Concept |> Array.sortBy (fun exercise -> exercise.Slug) do
//                let renderConcept (concept: Concept): string =
//                    match concept.File with
//                    | Some file ->
//                        sprintf "[`%s`](%s)" concept.Name (Path.GetRelativePath(languagesDirectory.FullName, file.FullName).Replace(Path.DirectorySeparatorChar, '/'))
//                    | None -> sprintf "`%s`" concept.Name
//                
//                let renderArray (arr: Concept[]) =
//                    if Array.isEmpty arr then
//                        "-"
//                    else
//                        arr
//                        |> Array.sortBy (fun concept -> concept.Name)
//                        |> Array.map renderConcept
//                        |> String.concat ", "  
//                
//                let exerciseLink = sprintf "[%s](./%s/exercises/concept/%s/.docs/instructions.md)" conceptExercise.Slug track.Slug conceptExercise.Slug
//                let concepts = conceptExercise.Concepts |> renderArray  
//                let prerequisites = conceptExercise.Prerequisites |> renderArray
//                renderLine trackLink exerciseLink concepts prerequisites

        markdown.AppendLine()
        
    let private appendLinks (markdown: StringBuilder): StringBuilder =
        markdown
            .AppendLine("[concepts]: ./concepts/README.md")
            .AppendLine("[paradigms]: ./paradigms/README.md")
            .AppendLine("[stories]: ./stories/README.md")
            .AppendLine("[tooling]: ./tooling/README.md")
            .AppendLine("[types]: ./types/README.md")

    let private renderToMarkdown (languagesDirectory: DirectoryInfo) (concepts: Concept list): string =
        StringBuilder()
        |> appendHeader
        |> appendTypes
        |> appendContributing
//        |> appendExercises languagesDirectory concepts
        |> appendLinks
        |> string

    let writeConcepts (referencesDirectory: DirectoryInfo) (concepts: Concept list): unit =
        let markdown = renderToMarkdown referencesDirectory concepts
        File.WriteAllText(Path.Combine(referencesDirectory.FullName, "README.md"), markdown)
//
//module Json =
//    [<CLIMutable>]
//    type JsonConcept =
//        { Url: string
//          Name: string }
//    
//    [<CLIMutable>]
//    type JsonConceptExercise =
//        { Url: string
//          Slug: string
//          Concepts: JsonConcept[]
//          Prerequisites: JsonConcept[] }
//
//    [<CLIMutable>]
//    type JsonExercises =
//        { Concept: JsonConceptExercise[] }
//
//    [<CLIMutable>]
//    type JsonTrack =
//        { Name: string
//          Slug: string
//          Exercises: JsonExercises } 
//    
//    let private conceptToJsonConcept (concept: Concept): JsonConcept =
//        { Url =
//            concept.File
//            |> Option.map (fun file -> sprintf "https://github.com/exercism/v3/tree/master/%s" (Path.GetRelativePath(Directory.GetCurrentDirectory(), file.FullName).Replace(Path.DirectorySeparatorChar, '/')))
//            |> Option.toObj
//          Name = concept.Name }
//    
//    let private conceptExerciseToJsonConceptExercise (track: Track) (conceptExercise: ConceptExercise): JsonConceptExercise =
//        { Url = sprintf "https://github.com/exercism/v3/tree/master/languages/%s/exercises/concept/%s" track.Slug conceptExercise.Slug
//          Slug = conceptExercise.Slug
//          Concepts = Array.map conceptToJsonConcept conceptExercise.Concepts
//          Prerequisites = Array.map conceptToJsonConcept conceptExercise.Prerequisites }
//    
//    let private trackToJsonTrack (track: Track): JsonTrack =
//        { Name = track.Name
//          Slug = track.Slug
//          Exercises =
//              { Concept = Array.map (conceptExerciseToJsonConceptExercise track) track.Exercises.Concept } }
//    
//    let private renderToJson (concepts: Track list): string =
//        let jsonconcepts = List.map trackToJsonTrack concepts
//        
//        let options = JsonSerializerOptions()
//        options.WriteIndented <- true
//        options.PropertyNamingPolicy <- JsonNamingPolicy.CamelCase
//        
//        JsonSerializer.Serialize(jsonconcepts, options)
//    
//    let writeconcepts (languagesDirectory: DirectoryInfo) (concepts: Track list): unit =
//        let json = renderToJson concepts
//        File.WriteAllText(Path.Combine(languagesDirectory.FullName, "languages.json"), json)

[<EntryPoint>]
let main _ =
    let referenceDirectory = DirectoryInfo("reference")
    let languagesDirectory = DirectoryInfo("languages")
    let concepts = Parser.parseConcepts referenceDirectory languagesDirectory
    
    Markdown.writeConcepts referenceDirectory concepts
//    Json.writeconcepts languagesDirectory concepts

    0
