
abstype error = Error of (string * (int * int)) with
  
end


datatype Result = Ok of Parser | Ng of Parser * Error

