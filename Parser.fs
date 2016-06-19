module Parser

open Microsoft.Build.Framework
open Microsoft.Build.Utilities

open Common
open Report

type public SsrsProjectConfigParser () =
    inherit Task()

    let mutable targetServerUrl = ""
    let mutable targetFolder  = ""
    let mutable targetDataSourceFolder = ""
    let mutable targetDatasetFolder = ""
    let mutable dataSources: ITaskItem[] = null
    let mutable reportsFolder = ""

    let toTaskItem name (fullPath: string, dataSource: DataSource) : ITaskItem =
        let metadata = [
            ("Path", fullPath);
            ("Name", dataSource.Name);
            ("ConnectionString", dataSource.ConnectionString) ]

        let ti = TaskItem ()
        ti.ItemSpec <- name
        
        metadata |> List.iter ti.SetMetadata

        ti :> ITaskItem
        
    let parseDataSources projDir (paths: string seq) : (string * DataSource) seq option = 
        let dataSources =
            paths
            |> List.ofSeq
            |> List.choose (fun path -> 
                opt {
                    let fullPath = path |> pathCombine projDir
                    let! doc = fullPath |> readXml
                    let! dataSource = doc |> getDataSource

                    return fullPath, dataSource
                })

        match dataSources with
        | [] -> None
        | ds -> Some (ds :> (string * DataSource) seq)

    [<Required>] member val ProjectFilePath: ITaskItem = null with get, set
    [<Required>] member val Configuration = "" with get, set

    [<Output>] member __.TargetServerUrl = targetServerUrl
    [<Output>] member __.TargetFolder = targetFolder
    [<Output>] member __.TargetDataSourceFolder = targetDataSourceFolder
    [<Output>] member __.TargetDatasetFolder = targetDatasetFolder
    [<Output>] member __.ReportsFolder = reportsFolder
    [<Output>] member __.DataSources = dataSources

    override this.Execute () =
        // do launchDebugger ()
        match this.ProjectFilePath.ItemSpec, this.Configuration with
        | ValidFilePath path, NotEmpty config ->
            let projDir = pathDir path
            let projXml = readXml path 

            match projXml >>= getConfiguration config with 
            | Some configuration -> 
                targetFolder            <- configuration.TargetFolder
                targetServerUrl         <- configuration.TargetServerUrl.ToString ()
                targetDataSourceFolder  <- configuration.TargetDataSourceFolder
                targetDatasetFolder     <- configuration.TargetDatasetFolder
                reportsFolder           <- projDir

                let dsources =
                    projXml  
                    >>= getDataSourcesPaths 
                    >>= parseDataSources projDir 
                    <!> Seq.map (toTaskItem "DataSources")
                    <!> Array.ofSeq

                dataSources <- 
                    match dsources with
                    | Some ds -> ds
                    | _ -> [||]

                true
            | _ -> 
                this.Log.LogError (sprintf "Configuration '%A' can't be found in '%A'" config path)
                false
        | _ -> 
            this.Log.LogError "ProjectFilePath: path is invalid or file doesn't exist"
            false
