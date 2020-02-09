#if !INTERACTIVE

module private Swag4Net.DiffTool.Helpers

#else

module Swag4Net.DiffTool.Helpers

#endif

open System

let compare s1 s2 =
  String.Compare(s1, s2, StringComparison.Ordinal)
  
let equal s1 s2 =
  String.Equals(s1, s2, StringComparison.Ordinal)
  
let iCompare s1 s2 =
  String.Compare(s1, s2, StringComparison.OrdinalIgnoreCase)
  
let iEqual s1 s2 =
  String.Equals(s1, s2, StringComparison.OrdinalIgnoreCase)

let compareInt i1 i2 =
  if i1 > i2 then
    1
  else if i1 = i2 then
    0
  else
    -1  
  
let flatten sequences =
   Seq.collect (fun x -> x) sequences

module ListHelpers=   
  ///
  /// enumerate through two lists and identify orphans and mismatches
  /// 
  let rec syncZip comp (list1: 'a list) (list2: 'a list) =
      match list1, list2 with
      | [], [] -> Seq.empty
      | [], e2::tail2 -> seq {
                                yield (None,Some e2)
                                yield! syncZip comp [] tail2
                          }
      | e1::tail1, [] -> seq {
                                yield (Some e1,None)
                                yield! syncZip comp tail1 []
                              }
      | e1::tail1, e2::tail2 -> 
                              seq {
                                  match comp e1 e2 with
                                  | c when c > 0 -> 
                                      yield (None, Some e2) 
                                      yield! syncZip comp list1 tail2 
                                  | 0 -> 
                                      yield (Some e1, Some e2) 
                                      yield! syncZip comp tail1 tail2 
                                  | c when c < 0 -> 
                                      yield (Some e1, None) 
                                      yield! syncZip comp tail1 list2
                              }
                            
module MapHelpers =
  ///
  /// enumerate through two maps and identify orphans and mismatches
  /// 
  let rec syncZip comp (map1:Map<'a,'b>) (map2:Map<'a,'b>) : seq<('a * 'b) option * ('a * 'b) option> =
    let compKV (k1,_) (k2,_) =
      comp k1 k2
    ListHelpers.syncZip compKV (Map.toList map1) (Map.toList map2)
