exception Wrong_exp_type

open Number
        
type sobject =
  Null
| Number  of Number.t
| Boolean of bool
| Symbol  of string
| String  of string
| Cons    of sobject * sobject
          
val car   : sobject -> sobject                      
val cdr   : sobject -> sobject
val atomp : sobject -> bool
val listp : sobject -> bool                    
val pairp : sobject -> bool
