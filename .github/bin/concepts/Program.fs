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
        let transformations =
            [fun (x: string) -> x.TrimEnd('s')
             fun (x: string) -> x + "s"
             fun (x: string) -> x.Replace("-", "_")
             fun (x: string) -> x.Replace("_", "-")]

        transformations
        |> List.fold (fun variations transformation -> List.map transformation variations |> List.append variations) [concept]
        |> List.distinct

    let private isConceptFile (file: FileInfo) =
        file.Name <> "README.md" && file.Name <> "_sidebar.md"
    
    let private categoryDirectory (category: ConceptCategory) =
        match category with
        | Concepts -> "concepts"
        | Types -> "types"
    
    let private conceptsDirectoryForCategory (referenceDirectory: DirectoryInfo) (category: ConceptCategory): DirectoryInfo =
        DirectoryInfo(Path.Combine(referenceDirectory.FullName, categoryDirectory category))
        
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
    
    let private categoryDirectory (category: ConceptCategory) =
        match category with
        | Concepts -> "concepts"
        | Types -> "types"

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
    
    let private appendImplementedConcepts (concepts: Concept list) (markdown: StringBuilder): StringBuilder =
        markdown
            .AppendLine("## Implemented Concepts")
            .AppendLine()
            .AppendLine("This is a list of Concepts for which an exercise has been implemented:")
            .AppendLine() |> ignore
        
        let renderLine conceptColumn implementationsColumn =
            markdown
                .AppendFormat(sprintf "| %s | %s |" conceptColumn implementationsColumn)
                .AppendLine() |> ignore
        
        renderLine "Concept" "Implementations"
        renderLine "-" "-"

        for concept in concepts do            
            let conceptLink = sprintf "[`%s`](./%s/%s.md)" concept.Name (categoryDirectory concept.Category) concept.Name
            let implementationLink implementation =
                sprintf "[%s](../languages/%s/exercises/concept/%s/.docs/instructions.md)" implementation.Language implementation.Track implementation.Exercise
            let implementationLinks =
                concept.Implementations
                |> List.sortBy (fun implementation -> implementation.Language)
                |> List.map implementationLink
                |> String.concat ", "
                        
            renderLine conceptLink implementationLinks

        markdown.AppendLine()
    
    let private appendUnimplementedConcepts (concepts: Concept list) (markdown: StringBuilder): StringBuilder =
        markdown
            .AppendLine("## Unimplemented Concepts")
            .AppendLine()
            .AppendLine("This is a list of Concepts for which no exercise has yet been implemented:")
            .AppendLine() |> ignore
        
        for concept in concepts do            
            let conceptLink = sprintf "- [`%s`](./%s/%s.md)" concept.Name (categoryDirectory concept.Category) concept.Name
            markdown.AppendLine(conceptLink) |> ignore

        markdown.AppendLine()
        
    let private appendLinks (markdown: StringBuilder): StringBuilder =
        markdown
            .AppendLine("[concepts]: ./concepts/README.md")
            .AppendLine("[paradigms]: ./paradigms/README.md")
            .AppendLine("[stories]: ./stories/README.md")
            .AppendLine("[tooling]: ./tooling/README.md")
            .AppendLine("[types]: ./types/README.md")

    let private renderToMarkdown (concepts: Concept list): string =
        let (unimplementedConcepts, implementedConcepts) = concepts |> List.partition (fun concept -> List.isEmpty concept.Implementations) 
        
        StringBuilder()
        |> appendHeader
        |> appendTypes
        |> appendContributing
        |> appendImplementedConcepts implementedConcepts
        |> appendUnimplementedConcepts unimplementedConcepts
        |> appendLinks
        |> string

    let writeConcepts (referencesDirectory: DirectoryInfo) (concepts: Concept list): unit =
        let markdown = renderToMarkdown concepts
        File.WriteAllText(Path.Combine(referencesDirectory.FullName, "README.md"), markdown)

module Json =
    
    [<CLIMutable>]
    type JsonImplementation =
        { Url: string
          Exercise: string
          Track: string
          Language: string }

    [<CLIMutable>]
    type JsonConcept =
        { Url: string
          Name: string
          Variations: string[]
          Implementations: JsonImplementation[] }
        
    let private categoryDirectory (category: ConceptCategory) =
        match category with
        | Concepts -> "concepts"
        | Types -> "types"
    
    let private implementationToJsonImplementation (implementation: Implementation): JsonImplementation =
        { Url = sprintf "https://github.com/exercism/v3/tree/master/languages/%s/exercises/concept/%s" implementation.Track implementation.Exercise
          Exercise = implementation.Exercise
          Track = implementation.Track
          Language = implementation.Language }
    
    let private conceptToJsonConcept (concept: Concept): JsonConcept =
        { Url = sprintf "https://github.com/exercism/v3/tree/master/reference/%s/%s.md" (categoryDirectory concept.Category) concept.Name
          Name = concept.Name
          Variations = List.toArray concept.Variations
          Implementations = List.map implementationToJsonImplementation concept.Implementations |> List.toArray }
    
    let private renderToJson (concepts: Concept list): string =
        let jsonConcepts = List.map conceptToJsonConcept concepts
        
        let options = JsonSerializerOptions()
        options.WriteIndented <- true
        options.PropertyNamingPolicy <- JsonNamingPolicy.CamelCase
        
        JsonSerializer.Serialize(jsonConcepts, options)
    
    let writeConcepts (referenceDirectory: DirectoryInfo) (concepts: Concept list): unit =
        let json = renderToJson concepts
        File.WriteAllText(Path.Combine(referenceDirectory.FullName, "references.json"), json)

[<EntryPoint>]
let main _ =
    let referenceDirectory = DirectoryInfo("reference")
    let languagesDirectory = DirectoryInfo("languages")
    let concepts = Parser.parseConcepts referenceDirectory languagesDirectory
    
    Markdown.writeConcepts referenceDirectory concepts
    Json.writeConcepts referenceDirectory concepts

    0
