module Report

open System
open System.Xml.Linq

open Common

type DataSource = 
    { Name: string 
      ConnectionString: string }

type Confguration = 
    { Name: string
      TargetServerUrl: Uri
      TargetFolder: string
      TargetDataSourceFolder: string
      TargetDatasetFolder: string }

let private createDataSource (e:XElement) =
    opt {
        let! name   = e |> xAttributeValue  "Name"
        let! props  = e |> xChildElement    "ConnectionProperties"
        let! connectionString   = props |> xChildValue "ConnectString"

        return { Name = name; ConnectionString = connectionString }
    }

let private createConfiguration (e:XElement) = 
    opt {
        let! name = e |> xChildValue "Name"
        let! options = e |> xChildElement "Options"

        let! targetServerUrl         = options |> xChildValue "TargetServerURL" >>= safe Uri
        let! targetFolder            = options |> xChildValue "TargetFolder"
        let! targetDatasetFolder     = options |> xChildValue "TargetDatasetFolder"
        let! targetDataSourceFolder  = options |> xChildValue "TargetDataSourceFolder"

        return { Name = name;
                 TargetServerUrl = targetServerUrl;
                 TargetFolder = targetFolder;
                 TargetDataSourceFolder = targetDataSourceFolder;
                 TargetDatasetFolder = targetDatasetFolder } 
    }

let private findConfigurationElement configName (xml:XDocument) =
    let configurations = 
        xml 
        |> xChildElement "Project"
        >>= xChildElement "Configurations"
        >>= xChildElements "Configuration"
        <!> List.ofSeq
        <!> List.choose (fun config -> 
            match config |> xChildElement "Name" with
            | Some name when name.Value = configName -> Some config
            | _ -> None)

    match configurations with
    | Some [config] -> Some config
    | _ -> None

let private findDataSourceElement (xml:XDocument) =
    xml
    |> xChildElement "RptDataSource"

let getDataSourcesPaths (xml:XDocument) =
    xml 
    |> xChildElement "Project" 
    >>= xChildElement "DataSources" 
    >>= xChildElements "ProjectItem"
    <!> Seq.choose (xChildValue "FullPath")

let getConfiguration configName doc =
    doc |> findConfigurationElement configName >>= createConfiguration

let getDataSource doc =
    doc |> findDataSourceElement >>= createDataSource