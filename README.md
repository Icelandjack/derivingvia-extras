# derivingvia-extras

Miscellaneous via types, for use with `DerivingVia`.

## `On`

The type `On T "field"` derives non-structural instances for `T` by
comparing an evaluting it only at the record field `"field"`.

```haskell
{-# Language DataKinds     #-}                                          
{-# Language DerivingVia   #-}                                          
{-# Language TypeOperators #-}                                          

import Deriving.On
import Data.Hashable                                                    
                                                                         
data User = User                                                        
  { name   :: String                                                    
  , age    :: Int                                                       
  , userID :: Integer                                                   
  }                                                                     
  deriving (Eq, Ord, Hashable)                                          
  via User `On` "userID"                                                
```

such that all other fields are ignored

```haskell
>> alice = User "Alice" 50 0xDEADBEAF                                   
>> bob   = User "Bob"   20 0xDEADBEAF                                   
>>                                                                      
>> alice == bob                                                         
True                                                                    
>> alice <= bob                                                         
True                                                                    
>> hash alice == hash bob                                               
True                                                                    
```
