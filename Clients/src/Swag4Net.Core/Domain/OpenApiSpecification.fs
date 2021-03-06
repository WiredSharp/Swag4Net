namespace Swag4Net.Core.Domain

open Swag4Net.Core
open SharedKernel

module OpenApiSpecification =

  type TypeName = string
  type HttpStatusCode = int
 
  type RegularExpression = string
  type Any = string
  type Url = string

  type Documentation =
    { Standard: Standard
      Infos:Infos
      Servers:Server list
      Paths:Map<string, Path>
      Components:Components option
      Security:SecurityRequirement list option
      Tags:Tag list option
      ExternalDocs:ExternalDocumentation option }
  and Standard =
    { Name: string
      Version: string }
  and Infos =
    { Description:string option
      Version:string
      Title:string
      TermsOfService:string option
      Contact:Contact option
      License:License option }
  and Request =
    { Description: string option
      Content: Map<string, MediaType>
      Required: bool }
  and Components =
    { Schemas: Map<string, InlinedOrReferenced<Schema>> option
      Responses: Map<string, InlinedOrReferenced<Response>> option
      Parameters: Map<string, InlinedOrReferenced<Parameter>> option
      Examples: Map<string, InlinedOrReferenced<Example>> option
      RequestBodies: Map<string, InlinedOrReferenced<Request>> option
      Headers: Map<string, InlinedOrReferenced<Header>> option
      SecuritySchemes: Map<string, InlinedOrReferenced<SecurityScheme>> option
      Links: Map<string, InlinedOrReferenced<Link>> option
      Callbacks: Map<string, InlinedOrReferenced<Callback>> option }
  and Contact = 
    { Name:string option
      Url:string option
      Email:string option }
  and License = 
    { Name:string; Url:string option }
  and Server = 
    {
      Url:string
      Description:string option
      Variables:Map<string, ServerVariable> option }
  and ServerVariable = 
    {
      Enum: string list option
      Default: string
      Description: string }
  and Path =
    {
      Reference: string
      Summary:string
      Description:string
      Get:Operation option
      Put:Operation option
      Post:Operation option
      Delete:Operation option
      Options:Operation option
      Head:Operation option
      Patch:Operation option
      Trace:Operation option
      Servers:Server list option
      Parameters:Parameter InlinedOrReferenced list
      }
  and Operation =
    {
      Tags: string list
      Summary: string option
      Description: string option
      ExternalDocs: ExternalDocumentation option
      OperationId: string 
      Parameters: Parameter InlinedOrReferenced list
      RequestBody: Request InlinedOrReferenced option
      Responses: Responses
      Callbacks: Map<string, Callback InlinedOrReferenced> option
      Deprecated: bool
      Security: SecurityRequirement list option
      Servers: Server list option }
  and Parameter =
    {
        Name: string
        In: ParameterLocation
        Description: string option
        Required: bool
        Deprecated: bool
        AllowEmptyValue: bool
        Style: string option
        Explode: bool
        AllowReserved: bool
        Schema: Schema InlinedOrReferenced option
        Example: Any option
        Examples: Map<string, Example InlinedOrReferenced> option
        Content: Map<MimeType, MediaType> option }
  and MimeType = string
  and MediaType =
    {
      Schema: Schema InlinedOrReferenced
      Examples: Map<string, Example InlinedOrReferenced>
      Encoding: Map<string, Encoding> }
  and Schema =
    { Name: string
      Title: string
      Type: string
      AllOf: Schema InlinedOrReferenced list option
      OneOf: Schema InlinedOrReferenced list option
      AnyOf: Schema InlinedOrReferenced list option
      Not: Schema InlinedOrReferenced list option
      MultipleOf: Schema InlinedOrReferenced list option
      Items: Schema InlinedOrReferenced option
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
      Properties: Map<string, Schema InlinedOrReferenced> option
      AdditionalProperties: AdditionalProperties option
      Required: string list option
      Nullable: bool
      Enum: Any list option
      Format: DataTypeFormat option
      Discriminator: Discriminator option
      Readonly: bool
      WriteOnly: bool
      Xml: Xml option
      ExternalDocs:ExternalDocumentation option
      Example: Any option
      Deprecated:bool }
  and AdditionalProperties =
    | B of bool
    | M of Map<string, Schema>
  and Response =
    {
      Description: string
      Headers: Map<string, InlinedOrReferenced<Header>>
      Content: Map<MimeType, MediaType>
      Links: Map<string, InlinedOrReferenced<Link>> }
  and Responses = 
    {
       Responses: Map<HttpStatusCode, InlinedOrReferenced<Response>>
       Default: InlinedOrReferenced<Response> option }
  and SecurityScheme =
    {
      Type: string
      Description: string option
      Name: string
      In: string
      Scheme: string
      BearerFormat: string
      Flows: OAuthFlows
      OpenIdConnectUrl: string }
  and OAuthFlows =
    {
      Implicit: OAuthFlow option
      Password: OAuthFlow option
      ClientCredentials: OAuthFlow option
      AuthorizationCode: OAuthFlow option }
  and OAuthFlow =
    {
      AuthorizationUrl: string
      TokenUrl: string
      RefreshUrl: string
      Scopes: Map<string, string> }
  and SecurityRequirement = Map<string, string list>
  and Header = 
    {
        Description: string
        Required: bool
        Deprecated: bool
        AllowEmptyValue: bool
        Style: string
        Explode: bool
        AllowReserved: bool
        Schema: InlinedOrReferenced<Schema>
        Example: Any
        Examples: Map<string, InlinedOrReferenced<Example>>
        Content: Map<string, MediaType> }
  and Tag =
    {
      Name: string
      Description: string option
      ExternalDocs: ExternalDocumentation option }
  and ExternalDocumentation =
    {
      Description: string option
      Url: Url }
  and Encoding =
    {
      ContentType: string
      Headers: Map<string, InlinedOrReferenced<Header>>
      Style: string
      Explode: bool
      AllowReserved: bool }
  and Link =
    {
      OperationRef: string option
      OperationId: string option
      Parameters: Map<string, Any> option
      RequestBody: Any option
      Descritpion: string option
      Server: Server option }
  and Callback = Map<string, Path>
  and Example =
    {
      Summary: string option
      Description: string option
      Value: Any option
      ExternalValue: string option }
  and DataTypeFormat = string
  and Discriminator =
    {
      PropertyName: string
      Mapping: Map<string, string> option }
  and Xml =
    {
      Name: string option
      Namespace: string option
      Prefix: string option
      Attribute: bool option
      Wrapped: bool option }

  [<RequireQualifiedAccess>]
  module Schema =
    
    let Empty =
      {
        Name=System.String.Empty
        Title=System.String.Empty
        Type=System.String.Empty
        AllOf=None
        OneOf=None
        AnyOf=None
        Not=None
        MultipleOf=None
        Items=None
        Maximum=None
        ExclusiveMaximum=None
        Minimum=None
        ExclusiveMinimum=None
        MaxLength=None
        MinLength=None
        Pattern=None
        MaxItems=None
        MinItems=None
        UniqueItems=None
        MaxProperties=None
        MinProperties=None
        Properties=None
        AdditionalProperties=None
        Required=None
        Nullable=false
        Enum=None
        Format=None
        Discriminator=None
        Readonly=false
        WriteOnly=false
        Xml=None
        ExternalDocs=None
        Example=None
        Deprecated=false }
    
    let named name (s:Schema) =
      if System.String.IsNullOrWhiteSpace s.Name
      then { s with Name = name }
      else s
