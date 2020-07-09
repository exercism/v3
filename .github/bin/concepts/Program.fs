open System.IO

open System.Text
open System.Text.Json
open System.Text.Json.Serialization

open Humanizer

type ConceptExercise =
    { Exercise: string
      Track: string
      Language: string
      Concepts: string list }
    
type ConceptCategory =
    | Concepts
    | Types
    
type ConceptDocument =
    { Name: string
      Category: ConceptCategory }

type Concept =
    { Name: string
      Variations: string list
      Document: ConceptDocument list
      Implementations: ConceptExercise list }
    
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
        
    let private normalizeConcept (concept: string): string =
        concept.Replace("_", "-")
        
    let private conceptVariations (concept: string): string list =
        seq {
            yield concept.Replace("_", "-")
            yield concept.Replace("_", "-").Pluralize()
            yield concept.Replace("_", "-").Singularize()
            yield concept.Replace("-", "_")
        }
        |> Seq.distinct
        |> Seq.toList

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
    
    let private parseConceptExercisesOfTrack (languageDirectory: DirectoryInfo): ConceptExercise[] =
        let jsonTrack = parseConfigJson languageDirectory        
        let toConceptExercise (exercise: JsonConceptExercise): ConceptExercise =
            { Exercise = exercise.Slug
              Track = languageDirectory.Name
              Language = jsonTrack.Language
              Concepts = Array.toList exercise.Concepts }
        
        jsonTrack.Exercises.Concept
        |> Array.map toConceptExercise
    
    let private parseConceptExercises (languagesDirectory: DirectoryInfo) : Map<string, ConceptExercise list> =
        let exercises =
            languagesDirectory.EnumerateDirectories()
            |> Seq.filter hasConfigJsonFile
            |> Seq.collect parseConceptExercisesOfTrack
        
        exercises        
        |> Seq.collect (fun exercise -> exercise.Concepts |> List.map (fun concept -> (concept, exercise)))
        |> Seq.groupBy fst
        |> Seq.map (fun (concept, exercises) -> (concept, exercises |> Seq.map snd |> Seq.toList))
        |> Map.ofSeq
            
    let private conceptName (conceptFile: FileInfo): string = conceptFile.Name.Split('.').[0]
        
    let private parseConceptDocument (category: ConceptCategory) (conceptFile: FileInfo): ConceptDocument =
        { Name = conceptName conceptFile
          Category = category }

    let private parseConceptDocumentsOfCategory (referenceDirectory: DirectoryInfo) (category: ConceptCategory): ConceptDocument list =
        conceptFilesForCategory referenceDirectory category
        |> List.map (parseConceptDocument category)

    let parseConceptDocuments (referenceDirectory: DirectoryInfo) : Map<string, ConceptDocument list> =
        [Concepts; Types]
        |> Seq.collect (fun category -> parseConceptDocumentsOfCategory referenceDirectory category)
        |> Seq.groupBy (fun concept -> concept.Name)
        |> Seq.map (fun (concept, documents) -> (concept, Seq.toList documents))
        |> Map.ofSeq
    
    let parseConcepts (referenceDirectory: DirectoryInfo) (languagesDirectory: DirectoryInfo): Concept list =
        let conceptExercises = parseConceptExercises languagesDirectory
        let conceptDocuments = parseConceptDocuments referenceDirectory
        
        let mapKeys map = map |> Map.toSeq |> Seq.map fst
        let conceptExerciseConcepts = mapKeys conceptExercises
        let conceptDocumentConcepts = mapKeys conceptDocuments
        let allConcepts =
            Seq.append conceptExerciseConcepts conceptDocumentConcepts
            |> Seq.map normalizeConcept
            |> Seq.distinctBy (fun concept -> concept.Pluralize())
  
        let toConcept (concept: string) =        
            let variations = conceptVariations concept
            let tryFindByConcept map =
                variations
                |> Seq.tryPick (fun variation -> Map.tryFind variation map)
                |> Option.defaultValue []
            
            { Name = concept
              Variations = variations
              Document = tryFindByConcept conceptDocuments
              Implementations = tryFindByConcept conceptExercises }
        
        allConcepts
        |> Seq.map toConcept
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

        for concept in concepts |> List.sortBy (fun concept -> concept.Name) do
            let conceptLink =
                concept.Document
                |> List.tryHead
                |> Option.map (fun conceptDocument -> sprintf "[`%s`](./%s/%s.md)" concept.Name (categoryDirectory conceptDocument.Category) conceptDocument.Name)
                |> Option.defaultValue (sprintf "`%s`" concept.Name)
            
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
            
        let documents =
            concepts
            |> List.collect (fun concept -> concept.Document)
            |> List.sortBy (fun document -> document.Name)
        
        for document in documents do
            let conceptLink = sprintf "- [`%s`](./%s/%s.md)" document.Name (categoryDirectory document.Category) document.Name
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
    type JsonConceptImplementation =
        { Url: string
          Exercise: string
          Track: string
          Language: string }
    
    [<CLIMutable>]
    type JsonConceptDocument =
        { Url: string
          Name: string }

    [<CLIMutable>]
    type JsonConcept =
        { Name: string
          Variations: string[]
          Documents: JsonConceptDocument[]
          Implementations: JsonConceptImplementation[] }
        
    let private categoryDirectory (category: ConceptCategory) =
        match category with
        | Concepts -> "concepts"
        | Types -> "types"
    
    let private conceptExerciseToJsonImplementation (conceptExercise: ConceptExercise): JsonConceptImplementation =
        { Url = sprintf "https://github.com/exercism/v3/tree/master/languages/%s/exercises/concept/%s" conceptExercise.Track conceptExercise.Exercise
          Exercise = conceptExercise.Exercise
          Track = conceptExercise.Track
          Language = conceptExercise.Language }
    
    let private conceptToJsonConceptDocument (conceptDocument: ConceptDocument): JsonConceptDocument =
        { Url = sprintf "https://github.com/exercism/v3/tree/master/reference/%s/%s.md" (categoryDirectory conceptDocument.Category) conceptDocument.Name
          Name = conceptDocument.Name }
    
    let private conceptToJsonConcept (concept: Concept): JsonConcept =
        { Name = concept.Name
          Variations = Array.ofList concept.Variations
          Documents = concept.Document |> List.map conceptToJsonConceptDocument |> Array.ofList
          Implementations = concept.Implementations |> List.map conceptExerciseToJsonImplementation |> Array.ofList }
    
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
