namespace Swag4Net.DiffTool

open System
open System.Net
open Swag4Net.DiffTool.Helpers
open Swag4Net.Core.Domain
open Swag4Net.Core.Domain.ApiModel

type DiffLevel = | Info | Warning | Breaking

type Orphan<'a> = {
    Path: string list
    Level: DiffLevel    
    Value: 'a
}

type DiffItem<'a> = {
    Path: string list
    Level: DiffLevel    
    Previous: 'a
    Actual: 'a
}

type DiffStatus = | Added | Removed | Modified

type ComparisonItem =
    | Added of Orphan<string>
    | Modified of DiffItem<string>
    | Removed of Orphan<string>
    with member this.Level =
           match this with
           | Added a -> a.Level
           | Removed r -> r.Level
           | Modified m -> m.Level        

         member this.Status: DiffStatus =
           match this with
           | Added a -> DiffStatus.Added
           | Removed r -> DiffStatus.Removed
           | Modified m -> DiffStatus.Modified        

type MaybeBuilder() =
    member this.Bind(x, f) = 
        match x with
        | Error e -> Error e
        | Ok a -> f a
    member this.Return(x) = 
        Ok x
    member this.ReturnFrom(x) = 
        x
    member this.success(x) = 
        Ok x
    
   
module Comparer =
    let private added path level value =
        Added {Path = path; Value=value; Level = level }

    let private removed path level value =
        Removed {Path = path; Value=value; Level = level }

    let private modified path level previous actual =
        Modified {Path = path; Previous=previous; Actual=actual; Level = level }
           
    let ByLevel (diffStatuses:ComparisonItem seq) =
       diffStatuses |> Seq.groupBy (fun d -> d.Level)
       
    let Warnings (diffStatuses:ComparisonItem seq) =
        diffStatuses |> Seq.filter (fun d -> d.Level = Warning)

    let Breakings (diffStatuses:ComparisonItem seq) =
        diffStatuses |> Seq.filter (fun d -> d.Level = Breaking)
    
    let Infos (diffStatuses:ComparisonItem seq) =
        diffStatuses |> Seq.filter (fun d -> d.Level = Info)

    let inline private compareSimple path level (previous:'a when 'a:comparison) (actual:'a) =
        if (previous <> actual) then
            seq { yield modified path level (string previous) (string actual) }
        else
            Seq.empty        
    
    let private compareString path level previous actual =
        if (not (iEqual previous actual)) then
            seq { yield modified path level previous actual }
        else
            Seq.empty

    let inline private compareOptions comp stringizer path leveler previous actual =
        match previous,actual with
            | Some x, Some y -> comp path x y
            | Some x, None -> seq { yield removed path (leveler DiffStatus.Removed) (stringizer x) }
            | None, Some y -> seq { yield added path (leveler DiffStatus.Added) (stringizer y) }
            | None, None -> Seq.empty

    let compareList order (comparer: string list -> 'a -> 'a -> ComparisonItem seq) stringizer path leveler list1 list2 =
      let matches = ListHelpers.syncZip order list1 list2
      let differ i e1 e2 =
        let index = sprintf "[%02i]" i
        match e1, e2 with
        | None, Some b -> seq { yield added (index :: path) (leveler DiffStatus.Added) (stringizer b) }
        | Some a, None -> seq { yield removed (index :: path) (leveler DiffStatus.Removed) (stringizer a) }
        | Some a, Some b -> comparer (index :: path) a b
        | _ -> Seq.empty
      matches |> Seq.mapi (fun i (e1,e2) -> differ i e1 e2) |> flatten
        
    let compareMap order (comparer: string list -> 'a -> 'a -> ComparisonItem seq) stringizer path leveler map1 map2 =
      let matches = MapHelpers.syncZip order map1 map2
      let differ e1 e2 = 
        match e1, e2 with
        | None, Some (k,_) -> seq { yield added (k :: path) (leveler DiffStatus.Added) (stringizer k) }
        | Some (k,_), None -> seq { yield removed (k :: path) (leveler DiffStatus.Removed) (stringizer k) }
        | Some (k1,v1), Some (_,v2) -> comparer (k1 :: path) v1 v2
        | _ -> Seq.empty
      matches |> Seq.map (fun (e1,e2) -> differ e1 e2) |> flatten
        
    let iCompareStringList path leveler =
        compareList iCompare (fun path p a -> compareString path (leveler DiffStatus.Modified) p a) (fun x -> x) path leveler

    let iCompareStringMap path leveler =
        compareMap iCompare (fun path p a -> compareString path (leveler DiffStatus.Modified) p a) (fun x -> x) path leveler

    ///
    /// resolve comparer level with <see cref='leveler'/>
    /// <param name=leveler>resolve level according to diffStatus</param>
    /// 
    let inline private compareOptionsWithLevel comp stringizer path leveler =
        compareOptions (fun path p a -> comp path (leveler DiffStatus.Modified) p a) stringizer path leveler

    let private compareOString =
        compareOptionsWithLevel compareString (fun x -> x)

    let inline private compareOSimple path leveler =
        compareOptionsWithLevel compareSimple (fun x -> string x) path leveler
    
    let private compareContact path previousContact actualContact =
       match previousContact, actualContact with
       | Email p, Email a -> compareString ("email" :: path) Info p a
        
    let staticLeveler (addedLevel:DiffLevel, removedLevel, modifiedLevel) diffStatus =
        match diffStatus with
        | DiffStatus.Added -> addedLevel
        | DiffStatus.Removed -> removedLevel
        | DiffStatus.Modified -> modifiedLevel
        
    let isoLeveler level =
        staticLeveler (level,level,level)

    let orphanLeveler orphanLevel modifiedLevel =
        staticLeveler (orphanLevel,orphanLevel,modifiedLevel)
    
    type Contact with
        member this.CompareTo =
            compareContact [] this
            
    let private compareLicense path (previousLicense: ApiModel.License) (actualLicense: ApiModel.License) =
        seq {            
               yield! compareString ("name" :: path) Info previousLicense.Name actualLicense.Name
               yield! compareString ("url" :: path) Info previousLicense.Url actualLicense.Url
        }

    type License with
        member this.CompareTo =
            compareLicense [] this
        
    let private compareInfo path (previous: ApiModel.Infos) (actual: ApiModel.Infos) =
        seq {
             yield! compareString ("description" :: path) Info previous.Description actual.Description
             yield! compareString ("title" :: path) Info previous.Title actual.Title
             yield! compareString ("version" :: path) Info previous.Version actual.Version
             yield! compareString ("termOfService" :: path) Info previous.TermsOfService actual.TermsOfService
             yield! compareOptions compareContact (fun c -> string c) ("contact" :: path) (isoLeveler Info) previous.Contact actual.Contact
             yield! compareOptions compareLicense (fun lic -> string lic) ("license" :: path) (isoLeveler Info) previous.License actual.License
        }

    type Infos with
        member this.CompareTo =
            compareInfo [] this
    
    let private sortSchema (schema1: ApiModel.Schema) (schema2: ApiModel.Schema) =
        compareInt schema1.SchemaId schema2.SchemaId
    ///
    ///EBL-TODO handle cycles
    /// 
    let rec private compareSchema path (previous: ApiModel.Schema) (actual: ApiModel.Schema) =
        let printSchema schema = schema.SchemaId
        let compareSchemaList = compareList sortSchema compareSchema printSchema
        let compareOSchemaList property leveler =
            compareOptions (fun path -> compareSchemaList path leveler) (fun _ -> property) (property :: path)
        seq {
             yield! compareSimple ("type" :: path) Breaking previous.Type actual.Type
             yield! compareOString ("title" :: path) (isoLeveler Info) previous.Title actual.Title             
             yield! compareOSchemaList "allOf" (staticLeveler (Warning,Breaking,Breaking)) (isoLeveler Breaking) previous.AllOf actual.AllOf             
             yield! compareOSchemaList "oneOf" (staticLeveler (Warning,Breaking,Breaking)) (isoLeveler Breaking) previous.OneOf actual.OneOf             
             yield! compareOSchemaList "anyOf" (staticLeveler (Warning,Breaking,Breaking)) (isoLeveler Breaking) previous.AnyOf actual.AnyOf             
             yield! compareOptions compareSchema printSchema ("not" :: path) (isoLeveler Breaking) previous.Not actual.Not             
             yield! compareOSimple ("multipleOf" :: path) (isoLeveler Breaking) previous.MultipleOf actual.MultipleOf             
             yield! compareOptions compareSchema printSchema ("items" :: path) (isoLeveler Breaking) previous.Items actual.Items             
             yield! compareOSimple ("maximum" :: path) (isoLeveler Breaking) previous.Maximum actual.Maximum             
             yield! compareOSimple ("exclusiveMaximum" :: path) (isoLeveler Breaking) previous.ExclusiveMaximum actual.ExclusiveMaximum             
             yield! compareOSimple ("minimum" :: path) (isoLeveler Breaking) previous.Minimum actual.Minimum             
             yield! compareOSimple ("exclusiveMinimum" :: path) (isoLeveler Breaking) previous.ExclusiveMinimum actual.ExclusiveMinimum             
             yield! compareOSimple ("maxLength" :: path) (isoLeveler Breaking) previous.MaxLength actual.MaxLength             
             yield! compareOSimple ("minLength" :: path) (isoLeveler Breaking) previous.MinLength actual.MinLength             
             yield! compareOString ("maximum" :: path) (isoLeveler Breaking) previous.Pattern actual.Pattern             
             yield! compareOSimple ("maxItems" :: path) (isoLeveler Breaking) previous.MaxItems actual.MaxItems             
             yield! compareOSimple ("minItems" :: path) (isoLeveler Breaking) previous.MinItems actual.MinItems             
             yield! compareOSimple ("uniqueItems" :: path) (isoLeveler Breaking) previous.UniqueItems actual.UniqueItems             
             yield! compareOSimple ("maxProperties" :: path) (isoLeveler Breaking) previous.MaxProperties actual.MaxProperties             
             yield! compareOSimple ("minProperties" :: path) (isoLeveler Breaking) previous.MinProperties actual.MinProperties             
             yield! compareOptions compareSchemaMap (fun _ -> "properties") ("properties" :: path) (isoLeveler Breaking) previous.Properties actual.Properties
             yield! compareOptions compareAdditionalProperties (fun _ -> "AdditionalProperties") ("AdditionalProperties" :: path) (staticLeveler (Warning, Breaking, Breaking)) previous.AdditionalProperties actual.AdditionalProperties
             yield! compareOSimple ("Required" :: path) (isoLeveler Breaking) previous.Required actual.Required
             yield! compareOSimple ("Nullable" :: path) (isoLeveler Breaking) previous.Nullable actual.Nullable
             yield! compareOptions (fun path -> iCompareStringList path (staticLeveler (Warning, Breaking, Breaking))) (fun i -> string i) ("Enum" :: path) (isoLeveler Breaking) previous.Enum actual.Enum
             yield! compareOSimple ("Format" :: path) (isoLeveler Breaking) previous.Format actual.Format
             yield! compareOptions compareDiscriminator (fun _ -> "Discriminator") ("Discriminator" :: path) (isoLeveler Breaking) previous.Discriminator actual.Discriminator
             yield! compareOSimple ("Readonly" :: path) (isoLeveler Breaking) previous.Readonly actual.Readonly
             yield! compareOSimple ("WriteOnly" :: path) (isoLeveler Breaking) previous.WriteOnly actual.WriteOnly
             yield! compareOSimple ("Example" :: path) (isoLeveler Info) previous.Example actual.Example
             yield! compareOSimple ("Deprecated" :: path) (isoLeveler Breaking) previous.Deprecated actual.Deprecated
        }

    and private compareSchemaMap path =
        compareMap iCompare compareSchema (fun x -> x) path (isoLeveler Breaking)

    and private compareAdditionalProperties path (previous: ApiModel.AdditionalProperties) (actual: ApiModel.AdditionalProperties) =
        match previous, actual with
            | Allowed _, Properties _ -> seq { yield modified path Breaking "allowed" "properties" }
            | Properties _, Allowed _ -> seq { yield modified path Breaking "properties" "allowed" }
            | Properties pp, Properties ap -> compareSchemaMap path pp ap
            | _ -> Seq.empty

    and private compareDiscriminator path (previous: ApiModel.Discriminator) (actual: ApiModel.Discriminator) =
        seq {
             yield! compareSimple ("PropertyName" :: path) Breaking previous.PropertyName actual.PropertyName            
             yield! compareOptions (fun path -> iCompareStringMap path (staticLeveler (Warning, Breaking, Breaking))) (fun x -> "Mapping") ("Mapping" :: path) (isoLeveler Breaking) previous.Mapping actual.Mapping            
        }

    
    type Schema with
        member this.CompareTo =
            compareSchema [] this
            
    let private sortParameter (param1: ApiModel.Parameter) (param2: ApiModel.Parameter) =
        iCompare param1.Name param2.Name

    let private compareParameter path (previous: ApiModel.Parameter) (actual: ApiModel.Parameter) =
        seq {
             yield! compareString ("description" :: path) Info previous.Description actual.Description             
             yield! compareSimple ("deprecated" :: path) Warning previous.Deprecated actual.Deprecated
             yield! compareSimple ("allowEmptyValue" :: path) Info previous.AllowEmptyValue actual.AllowEmptyValue
             yield! compareSchema ("paramType" :: path) previous.ParamType actual.ParamType
             yield! compareSimple ("required" :: path) Warning previous.Required actual.Required
             yield! compareSimple ("location" :: path) Breaking previous.Location actual.Location
        }

    type Parameter with
        member this.CompareTo =
            compareParameter [] this
            
    let private sortResponse (resp1: ApiModel.Response) (resp2: ApiModel.Response) =
        let getRank resp =
            match resp.Code with
            | AnyStatusCode -> 0
            | StatusCode sc -> int sc
        compareInt (getRank resp1) (getRank resp2)

    let private compareResponse path (previous: ApiModel.Response) (actual: ApiModel.Response) =
        seq {
             yield! compareSimple ("code" :: path) Breaking previous.Code actual.Code
             yield! compareString ("description" :: path) Info previous.Description actual.Description             
             yield! compareOptions compareSchema (fun s -> s.SchemaId) ("type" :: path) (staticLeveler (Breaking, Warning, Breaking)) previous.Type actual.Type
        }

    type Response with
        member this.CompareTo =
            compareResponse [] this
            
    let private sortPath (path1: ApiModel.Path) (path2: ApiModel.Path) =
        compareInt path1.OperationId path2.OperationId
        
    let private comparePath path (previous: ApiModel.Path) (actual: ApiModel.Path) =
        seq {
             yield! compareString ("path" :: path) Breaking previous.Path actual.Path
             yield! compareString ("verb" :: path) Breaking previous.Verb actual.Verb
             yield! iCompareStringList ("tags" :: path) (isoLeveler Info) previous.Tags actual.Tags
             yield! compareString ("summary" :: path) Info previous.Summary actual.Summary
             yield! compareString ("description" :: path) Info previous.Description actual.Description
             yield! iCompareStringList ("consumes" :: path) (staticLeveler (Info, Breaking, Breaking)) previous.Consumes actual.Consumes
             yield! iCompareStringList ("produces" :: path) (staticLeveler (Info, Breaking, Breaking)) previous.Produces actual.Produces
             yield! compareList sortParameter compareParameter (fun p -> string p) ("parameters" :: path) (isoLeveler Breaking) previous.Parameters actual.Parameters
             yield! compareList sortResponse compareResponse (fun r -> string r) ("responses" :: path) (staticLeveler (Warning, Breaking, Breaking)) previous.Responses actual.Responses
        }
 
    type Path with
        member this.CompareTo =
            comparePath [] this
            
    let Compare (previous: ApiModel.Api) (actual: ApiModel.Api) =
        seq {
           yield! compareInfo ["info"] previous.Infos actual.Infos
           yield! compareString ["basePath"] Info previous.BasePath actual.BasePath
           yield! compareList sortPath comparePath (fun p -> string p) ["paths"] (staticLeveler (Info, Breaking, Breaking)) previous.Paths actual.Paths
           yield! compareList sortSchema compareSchema (fun d -> string d) ["definitions"] (staticLeveler (Info, Info, Warning)) previous.Definitions actual.Definitions
        }
