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

type DiffStatus =
    | Added of Orphan<string>
    | Modified of DiffItem<string>
    | Removed of Orphan<string>
    with member this.Level =
           match this with
           | Added a -> a.Level
           | Removed r -> r.Level
           | Modified m -> m.Level        

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
           
    let ByLevel (diffStatuses:DiffStatus seq) =
       diffStatuses |> Seq.groupBy (fun d -> d.Level)
       
    let Warnings (diffStatuses:DiffStatus seq) =
        diffStatuses |> Seq.filter (fun d -> d.Level = Warning)

    let Breakings (diffStatuses:DiffStatus seq) =
        diffStatuses |> Seq.filter (fun d -> d.Level = Breaking)
    
    let Infos (diffStatuses:DiffStatus seq) =
        diffStatuses |> Seq.filter (fun d -> d.Level = Info)

    let inline private compareSimple path level previous actual =
        if (previous <> actual) then
            seq { yield modified path level (string previous) (string actual) }
        else
            Seq.empty        
    
    let private compareString path level previous actual =
        if (not (iEqual previous actual)) then
            seq { yield modified path level previous actual }
        else
            Seq.empty

    let inline private compareOptions comp stringizer path level previous actual =
        match previous,actual with
            | Some x, Some y -> comp path level x y
            | Some x, None -> seq { yield removed path level (stringizer x) }
            | None, Some y -> seq { yield added path level (stringizer y) }
            | None, None -> Seq.empty

    let compareList order comparer stringizer path level list1 list2 =
      let matches = ListHelpers.syncZip order list1 list2
      let differ i e1 e2 =
        let index = sprintf "[%02i]" i
        match e1, e2 with
        | None, Some b -> seq { yield added (index :: path) level (stringizer b) }
        | Some a, None -> seq { yield removed (index :: path) level (stringizer a) }
        | Some a, Some b -> comparer (index :: path) level a b
        | _ -> Seq.empty
      matches |> Seq.mapi (fun i (e1,e2) -> differ i e1 e2) |> flatten
        
    let compareMap order comparer stringizer path level map1 map2 =
      let matches = MapHelpers.syncZip order map1 map2
      let differ e1 e2 = 
        match e1, e2 with
        | None, Some (k,_) -> seq { yield added (k :: path) level (stringizer k) }
        | Some (k,_), None -> seq { yield removed (k :: path) level (stringizer k) }
        | Some (k1,v1), Some (_,v2) -> comparer (k1 :: path) level v1 v2
        | _ -> Seq.empty
      matches |> Seq.map (fun (e1,e2) -> differ e1 e2) |> flatten
        
    let iCompareStringList =
        compareList iCompare compareString (fun x -> x)

    let iCompareStringMap =
        compareMap iCompare compareString (fun x -> x)

    let private compareOString =
        compareOptions compareString (fun x -> x)

    let private compareContact path _ previousContact actualContact =
       match previousContact, actualContact with
       | Email p, Email a -> compareString ("email" :: path) Info p a

    type Contact with
        member this.CompareTo =
            compareContact [] this
            
    let private compareLicense path _ (previousLicense: ApiModel.License) (actualLicense: ApiModel.License) =
        seq {            
               yield! compareString ("name" :: path) Info previousLicense.Name actualLicense.Name
               yield! compareString ("url" :: path) Info previousLicense.Url actualLicense.Url
        }

    type License with
        member this.CompareTo =
            compareLicense [] () this
        
    let private compareInfo path (previous: ApiModel.Infos) (actual: ApiModel.Infos) =
        seq {
             yield! compareString ("description" :: path) Info previous.Description actual.Description
             yield! compareString ("title" :: path) Info previous.Title actual.Title
             yield! compareString ("version" :: path) Info previous.Version actual.Version
             yield! compareString ("termOfService" :: path) Info previous.TermsOfService actual.TermsOfService
             yield! compareOptions compareContact (fun c -> string c) ("contact" :: path) Info previous.Contact actual.Contact
             yield! compareOptions compareLicense (fun lic -> string lic) ("license" :: path) Info previous.License actual.License
        }

    type Infos with
        member this.CompareTo =
            compareInfo [] this
    
    let private sortSchema (schema1: ApiModel.Schema) (schema2: ApiModel.Schema) =
        compareInt schema1.SchemaId schema2.SchemaId
    
    ///
    ///EBL-TODO handle cycles
    /// 
    let rec private compareSchema path _ (previous: ApiModel.Schema) (actual: ApiModel.Schema) =
        let printSchema schema = schema.SchemaId
        let compareSchemaList = compareList sortSchema compareSchema printSchema
        let compareOSchemaList property = compareOptions compareSchemaList (fun _ -> property) (property :: path)
        seq {
             yield! compareSimple ("type" :: path) Breaking previous.Type actual.Type
             yield! compareOString ("title" :: path) Info previous.Title actual.Title             
             yield! compareOSchemaList "allOf" Breaking previous.AllOf actual.AllOf             
             yield! compareOSchemaList "oneOf" Breaking previous.OneOf actual.OneOf             
             yield! compareOSchemaList "anyOf" Breaking previous.AnyOf actual.AnyOf             
             yield! compareOptions compareSchema printSchema ("not" :: path) Breaking previous.Not actual.Not             
             yield! compareOptions compareSimple (fun i -> string i) ("multipleOf" :: path) Warning previous.MultipleOf actual.MultipleOf             
             yield! compareOptions compareSchema printSchema ("items" :: path) Breaking previous.Items actual.Items             
             yield! compareOptions compareSimple (fun i -> string i) ("maximum" :: path) Warning previous.Maximum actual.Maximum             
             yield! compareOptions compareSimple (fun i -> string i) ("exclusiveMaximum" :: path) Warning previous.ExclusiveMaximum actual.ExclusiveMaximum             
             yield! compareOptions compareSimple (fun i -> string i) ("minimum" :: path) Warning previous.Minimum actual.Minimum             
             yield! compareOptions compareSimple (fun i -> string i) ("exclusiveMinimum" :: path) Warning previous.ExclusiveMinimum actual.ExclusiveMinimum             
             yield! compareOptions compareSimple (fun i -> string i) ("maxLength" :: path) Warning previous.MaxLength actual.MaxLength             
             yield! compareOptions compareSimple (fun i -> string i) ("minLength" :: path) Warning previous.MinLength actual.MinLength             
             yield! compareOString ("maximum" :: path) Warning previous.Pattern actual.Pattern             
             yield! compareOptions compareSimple (fun i -> string i) ("maxItems" :: path) Warning previous.MaxItems actual.MaxItems             
             yield! compareOptions compareSimple (fun i -> string i) ("minItems" :: path) Warning previous.MinItems actual.MinItems             
             yield! compareOptions compareSimple (fun i -> string i) ("uniqueItems" :: path) Warning previous.UniqueItems actual.UniqueItems             
             yield! compareOptions compareSimple (fun i -> string i) ("maxProperties" :: path) Warning previous.MaxProperties actual.MaxProperties             
             yield! compareOptions compareSimple (fun i -> string i) ("minProperties" :: path) Warning previous.MinProperties actual.MinProperties             
             yield! compareOptions compareSchemaMap (fun _ -> "properties") ("properties" :: path) Warning previous.Properties actual.Properties
             yield! compareOptions compareAdditionalProperties (fun _ -> "AdditionalProperties") ("AdditionalProperties" :: path) Warning previous.AdditionalProperties actual.AdditionalProperties
             yield! compareOptions compareSimple (fun i -> string i) ("Required" :: path) Warning previous.Required actual.Required
             yield! compareOptions compareSimple (fun i -> string i) ("Nullable" :: path) Warning previous.Nullable actual.Nullable
             yield! compareOptions iCompareStringList (fun i -> string i) ("Enum" :: path) Warning previous.Enum actual.Enum
             yield! compareOptions compareSimple (fun i -> string i) ("Format" :: path) Warning previous.Format actual.Format
             yield! compareOptions compareDiscriminator (fun _ -> "Discriminator") ("Discriminator" :: path) Warning previous.Discriminator actual.Discriminator
             yield! compareOptions compareSimple (fun i -> string i) ("Readonly" :: path) Warning previous.Readonly actual.Readonly
             yield! compareOptions compareSimple (fun i -> string i) ("WriteOnly" :: path) Warning previous.WriteOnly actual.WriteOnly
             yield! compareOptions compareSimple (fun i -> string i) ("Example" :: path) Info previous.Example actual.Example
             yield! compareOptions compareSimple (fun i -> string i) ("Deprecated" :: path) Warning previous.Deprecated actual.Deprecated
        }

    and private compareSchemaMap = compareMap iCompare compareSchema (fun x -> x)

    and private compareAdditionalProperties path level (previous: ApiModel.AdditionalProperties) (actual: ApiModel.AdditionalProperties) =
        match previous, actual with
            | Allowed _, Properties _ -> seq { yield modified path level "allowed" "properties" }
            | Properties _, Allowed _ -> seq { yield modified path level "properties" "allowed" }
            | Properties pp, Properties ap -> compareSchemaMap path level pp ap
            | _ -> Seq.empty

    and private compareDiscriminator path _ (previous: ApiModel.Discriminator) (actual: ApiModel.Discriminator) =
        seq {
             yield! compareSimple ("PropertyName" :: path) Breaking previous.PropertyName actual.PropertyName            
             yield! compareOptions iCompareStringMap (fun x -> "Mapping") ("Mapping" :: path) Breaking previous.Mapping actual.Mapping            
        }

    
    type Schema with
        member this.CompareTo =
            compareSchema [] Breaking this
            
    let private sortParameter (param1: ApiModel.Parameter) (param2: ApiModel.Parameter) =
        iCompare param1.Name param2.Name

    let private compareParameter path _ (previous: ApiModel.Parameter) (actual: ApiModel.Parameter) =
        seq {
             yield! compareString ("description" :: path) Info previous.Description actual.Description             
             yield! compareSimple ("deprecated" :: path) Warning previous.Deprecated actual.Deprecated
             yield! compareSimple ("allowEmptyValue" :: path) Info previous.AllowEmptyValue actual.AllowEmptyValue
             yield! compareSchema ("paramType" :: path) Breaking previous.ParamType actual.ParamType
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

    let private compareResponse path _ (previous: ApiModel.Response) (actual: ApiModel.Response) =
        seq {
             yield! compareSimple ("code" :: path) Breaking previous.Code actual.Code
             yield! compareString ("description" :: path) Info previous.Description actual.Description             
             yield! compareOptions compareSchema (fun s -> s.SchemaId) ("type" :: path) Breaking previous.Type actual.Type
        }

    type Response with
        member this.CompareTo =
            compareResponse [] this
            
    let private sortPath (path1: ApiModel.Path) (path2: ApiModel.Path) =
        compareInt path1.OperationId path2.OperationId
        
    let private comparePath path _ (previous: ApiModel.Path) (actual: ApiModel.Path) =
        seq {
             yield! compareString ("path" :: path) Breaking previous.Path actual.Path
             yield! compareString ("verb" :: path) Breaking previous.Verb actual.Verb
             yield! iCompareStringList ("tags" :: path) Info previous.Tags actual.Tags
             yield! compareString ("summary" :: path) Info previous.Summary actual.Summary
             yield! compareString ("description" :: path) Info previous.Description actual.Description
             yield! iCompareStringList ("consumes" :: path) Warning previous.Consumes actual.Consumes
             yield! iCompareStringList ("produces" :: path) Warning previous.Produces actual.Produces
             yield! compareList sortParameter compareParameter (fun p -> string p) ("parameters" :: path) Breaking previous.Parameters actual.Parameters
             yield! compareList sortResponse compareResponse (fun r -> string r) ("responses" :: path) Breaking previous.Responses actual.Responses
        }
 
    type Path with
        member this.CompareTo =
            comparePath [] () this
            
    let Compare (previous: ApiModel.Api) (actual: ApiModel.Api) =
        seq {
           yield! compareInfo ["info"] previous.Infos actual.Infos
           yield! compareString ["basePath"] Info previous.BasePath actual.BasePath
           yield! compareList sortPath comparePath (fun p -> string p) ["paths"] Breaking previous.Paths actual.Paths
           yield! compareList sortSchema compareSchema (fun d -> string d) ["definitions"] Breaking previous.Definitions actual.Definitions
        }
