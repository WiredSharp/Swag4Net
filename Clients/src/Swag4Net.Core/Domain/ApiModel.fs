namespace Swag4Net.Core.Domain

open System.Net

module ApiModel =
  type TypeName = string
  type Any = string
  type RegularExpression = string

  type Api =
    { Infos:Infos
      BasePath:string
      Paths:Path list
      Definitions:Schema list }
  and Infos =
    { Description:string
      Version:string
      Title:string
      TermsOfService:string
      Contact:Contact option
      License:License option }
  and Contact = 
    | Email of string
  and License = 
    { Name:string; Url:string }
  and Schema =
      {
        SchemaId: string
        Type: DataType
        Title: string option
        AllOf: Schema list option
        OneOf: Schema list option
        AnyOf: Schema list option
        Not: Schema option
        MultipleOf: int option
        Items: Schema option
        Maximum: int option
        ExclusiveMaximum: int option
        Minimum: int option
        ExclusiveMinimum: int option
        MaxLength: int option
        MinLength: int option
        Pattern: RegularExpression option
        MaxItems: int option
        MinItems: int option
        UniqueItems: bool option
        MaxProperties: int option
        MinProperties: int option
        Properties: Map<string, Schema> option
        AdditionalProperties: AdditionalProperties option
        Required: bool option
        Nullable: bool option
        Enum: Any list option
        Format: DataTypeFormat option
        Discriminator: Discriminator option
        Readonly: bool option
        WriteOnly: bool option
        Example: Any option
        Deprecated:bool option
      }
  and DataType =
    | Integer
    | String
    | Array
    | Number
    | Object
    | Boolean
  and AdditionalProperties =
    | Allowed of bool
    | Properties of Map<string, Schema>
  and Path = 
    { Path:string
      Verb:string
      Tags:string list
      Summary:string
      Description:string
      OperationId:string
      Consumes:string list
      Produces:string list
      Parameters:Parameter list
      Responses:Response list }
  and ParameterLocation =
    | InQuery
    | InHeader
    | InPath
    | InCookie
    | InBody
    | InFormData
  and Parameter =
    { Location:ParameterLocation
      Name:string
      Description:string
      Deprecated:bool
      AllowEmptyValue:bool
      ParamType:Schema
      Required:bool }
  and Response = 
    { Code:StatusCodeInfo
      Description:string
      Type:Schema option }
  and StatusCodeInfo =
    | AnyStatusCode
    | StatusCode of HttpStatusCode
  and DataTypeFormat = string
  and Discriminator =
    {
      PropertyName: string
      Mapping: Map<string, string> option }

  let IsArrayType (dataType) :bool =
    match dataType with
    | DataType.Array -> true
    | _ -> false

  let IsPrimaryType (dataType) :bool =
    match dataType with
    | DataType.Object -> false
    | DataType.Array -> false
    | _ -> true

  let IsArray (schema:Schema) :bool =
    IsArrayType schema.Type

  let IsPrimary (schema:Schema) :bool =
    IsPrimaryType schema.Type
